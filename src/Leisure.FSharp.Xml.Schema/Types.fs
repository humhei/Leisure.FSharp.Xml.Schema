// Learn more about F# at http://fsharp.org
namespace Leisure.FSharp.Xml.Schema
#nowarn "0104"
open System.Runtime.Serialization

#nowarn "3535"
#nowarn "3536"

open System
open System.Globalization

open System.Collections
open System.Collections.Generic

open System.Reflection

open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Xml
open System.Xml.Schema

type FsIXmlSerializableTypeMapping = 
    abstract member WrapOldName: bool

type FsIXmlSerializableTypeMapping<'T, 'XmlT> =
    inherit FsIXmlSerializableTypeMapping
    /// NOTE: static abstract for compitiple to .net standard 2.0
    abstract member OfXml: Type * 'XmlT -> 'T
    abstract member ToXml: unit -> 'XmlT


[<AutoOpen>]
module private _Type_Extensions =
    type Type with 
        member x.GetInterface_Last(name: string) =
            x.GetInterfaces()
            |> Array.tryFindBack(fun m ->
                m.Name.ToLower() = name.ToLower()
            )

type FsXmlSerializerTypeMapping =
    { ToXmlSerializable: obj -> obj 
      OfXmlSerializable: obj -> obj
      TargetType: Type
      WrapOldName: bool }


//[<AutoOpen>]
//module private _FsXmlSerializerConfigurationUtils = 
//    let fsIXmlSerializableTypeMappingCache = ConcurrentDictionary()

type IFsSchemaType = interface end

type FsXmlSerializerConfiguration =
    internal
        { TypeMapping: Dictionary<Type, FsXmlSerializerTypeMapping>
          FsIXmlSerializableTypeMappingCache:  ConcurrentDictionary<Type, FsXmlSerializerTypeMapping option>
          FsSchemaTypeCache: ConcurrentDictionary<Type, IFsSchemaType>}
with 
    member x.AddTypeMapping<'Origin, 'Target>(toXml: 'Origin -> 'Target, ofXml: 'Target -> 'Origin, ?wrapOldName) =
        let typeMapping = 
            let toXml (v: obj) =
                toXml (v :?> 'Origin)
                |> box

            let ofXml(v: obj) =
                ofXml(v :?> 'Target)
                |> box

            { ToXmlSerializable = toXml 
              OfXmlSerializable = ofXml
              TargetType = typeof<'Target>
              WrapOldName = defaultArg wrapOldName false }

        let originTp = typeof<'Origin>

        let originTpCode = Type.GetTypeCode originTp

        match originTpCode with 
        | TypeCode.Object -> ()
        | _ -> failwithf "Invalid origin type %A for typeMapping" originTp


        match typeMapping.WrapOldName with 
        | true ->
            match originTp.IsGenericType with 
            | true -> failwithf "originTp %A cannot be generic when wrapOldName is true" originTp
            | false -> ()

        | false -> ()


        x.TypeMapping.Add(typeof<'Origin>, typeMapping)
        x



    static member DefaultValue =
        { TypeMapping = Dictionary()
          FsIXmlSerializableTypeMappingCache = ConcurrentDictionary() 
          FsSchemaTypeCache = ConcurrentDictionary()
        }

    member internal x.UpdateTypeAndValue_ToXml(tp: Type, value: obj) =
        match x.TypeMapping.TryGetValue tp with 
        | false, _ -> None
        | true, typeMapping ->
            let newValue = 
                typeMapping.ToXmlSerializable(value)

            (typeMapping.TargetType, newValue)
            |> Some

    //member internal x.OF_XML_UpdateType(tp: Type) =
    //    match x.InverseTypeMapping.TryGetValue tp with 
    //    | false, _ -> None
    //    | true, typeMapping ->
    //        Some typeMapping.TargetType



[<Interface>]
type FsIXmlSerializable = 
    abstract member WriteXml: writer: XmlWriter * config: FsXmlSerializerConfiguration -> unit
    /// NOTE: static abstract for compitiple to .net standard 2.0
    abstract member ReadXmlObj: tp: Type * reader: XmlReader * config: FsXmlSerializerConfiguration -> obj
    
type FsIXmlSerializable<'T> =
    inherit FsIXmlSerializable
    /// NOTE: static abstract for compitiple to .net standard 2.0
    abstract member ReadXml: tp: Type * reader:XmlReader * config: FsXmlSerializerConfiguration -> 'T


//type FsIXmlSerializableSchema =
//    inherit FsIXmlSerializable
    
//    static abstract member SchemaType: unit -> Type

//type FsIXmlSerializableSchema<'T> =
//    inherit FsIXmlSerializableSchema
//    static abstract member ReadXml: tp: Type * reader:XmlReader * config: FsXmlSerializerConfiguration -> 'T




[<AutoOpen>]
module internal _Utils =
    

    let itemText (i) =
        "Item" + (i+1).ToString()

    let advanceReader(reader: XmlReader) =
        let rec loop () =
            match reader.Read() with 
            | true -> 
                match reader.NodeType with 
                | XmlNodeType.EndElement -> loop ()
                | XmlNodeType.XmlDeclaration -> loop ()
                | XmlNodeType.Whitespace -> loop ()
                | _ -> true

            | false -> false

        loop()
       
    let advanceReaderFullElement(reader: XmlReader, fElement) =
        let name = reader.Name

        let rec loop stack =
            match reader.Read() with 
            | true -> 
                let goEnd(stack) =
                    let newStack = (stack-1)
                    assert(newStack >= 0)
                    match newStack with 
                    | 0 -> 
                        match reader.Name = name with 
                        | true -> true
                        | false -> failwithf "Invalid token, reader.Name %s should be %s here" reader.Name name 

                    | _ -> false

                match reader.NodeType with 
                | XmlNodeType.Element -> 
                    let newStack = stack+1

                    assert(newStack >= 1)
                    match newStack with 
                    | 0 -> ()
                    | _ -> fElement(newStack)

                    match reader.NodeType with 
                    | XmlNodeType.EndElement ->
                        match goEnd(newStack) with 
                        | true -> true
                        | false -> loop (newStack-1)

                    | _ -> loop (newStack)


                | XmlNodeType.EndElement -> 
                    match goEnd(stack) with 
                    | true -> true
                    | false -> loop (stack - 1)
                | _ -> 
                    loop stack

            | false -> false

        loop 1

    let advanceReaderFullElement_GetElement(reader: XmlReader, fElement) =
        let mutable value = None

        advanceReaderFullElement(reader, fun stack ->
            let r = fElement stack
            value <- Some r
        )
        |> ignore

        match value with 
        | None -> failwithf "failed Get Nested singleton element from reader"
        | Some v -> v


    let [<Literal>] W3XMLSchema = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] W3XMLSchemaInstance = "http://www.w3.org/2001/XMLSchema-instance"

    let private textInfo = (new CultureInfo("en-US", false)).TextInfo
    let toTitleCase (text: string) = textInfo.ToTitleCase(text)

    [<RequireQualifiedAccess>]
    type FsObjectTypeCode =
        | Record 
        | Union of UnionCaseInfo []
        | SingletonCaseUnion of UnionCaseInfo
        | FsXmlSerializableTypeMapping

    [<RequireQualifiedAccess>]
    type FsTypeCode =
        | ValueType of TypeCode
        | Enum
        | Object of FsObjectTypeCode



    let private typeCodeCache = ConcurrentDictionary()



    let getFsTpCode (tp: Type) =
        typeCodeCache.GetOrAdd(tp, valueFactory = fun _ ->
            let tpCode = Type.GetTypeCode tp
            match tpCode with 
            | TypeCode.Object ->
                match FSharpType.IsRecord tp with 
                | true -> FsTypeCode.Object(FsObjectTypeCode.Record)
                | false ->
                    match FSharpType.IsUnion tp with 
                    | true -> 
                        match FSharpType.GetUnionCases(tp, true) with 
                        | [||] -> failwithf "[Xml serializable] Un supported type %A" tp
                        | [|case|] ->
                            let fields = case.GetFields()
                            
                            match fields with 
                            | [||] ->  failwithf "[Xml serializable] Un supported type %A" tp
                            | _ ->
                                FsTypeCode.Object(FsObjectTypeCode.SingletonCaseUnion case)
                          
                        | cases -> FsTypeCode.Object(FsObjectTypeCode.Union cases)

                            
                    | false -> 
                        match FSharpType.IsUnion(tp, true) with 
                        | true ->
                            match FSharpType.GetUnionCases(tp, true) with 
                            | [|case|] ->
                                FsTypeCode.Object(FsObjectTypeCode.SingletonCaseUnion case)

                            | _ -> failwithf "[Xml serializable] Un supported type %A" tp

                        | false -> 
                            match tp.GetInterface_Last(nameof FsIXmlSerializableTypeMapping) with 
                            | None -> 
                                failwithf "[Xml serializable] Un supported type %A, using FsXmlSerializableSchema instead" tp

                            | Some itp ->
                                FsTypeCode.Object(FsObjectTypeCode.FsXmlSerializableTypeMapping)
                
            | _ -> 
                match tp.IsEnum with
                | true -> FsTypeCode.Enum 
                | false -> FsTypeCode.ValueType tpCode
        )

    [<AutoOpen>]
    module _CollectionTypeDelegate =

        let private listModule_ofArray_MethodInfo = 
            lazy
                let list = []
                let fsharpCoreAssembly = list.GetType().Assembly
                let listModule = fsharpCoreAssembly.GetType("Microsoft.FSharp.Collections.ListModule")
                let ofArray = listModule.GetMethod("OfArray")
                ofArray

        let private setModule_ofArray_MethodInfo = 
            lazy
                let list = []
                let fsharpCoreAssembly = list.GetType().Assembly
                let listModule = fsharpCoreAssembly.GetType("Microsoft.FSharp.Collections.SetModule")
                let ofArray = listModule.GetMethod("OfArray")
                ofArray

        let private mapModule_ofArray_MethodInfo = 
            lazy
                let list = []
                let fsharpCoreAssembly = list.GetType().Assembly
                let listModule = fsharpCoreAssembly.GetType("Microsoft.FSharp.Collections.MapModule")
                let ofArray = listModule.GetMethod("OfArray")
                ofArray

        let private listModule_ofArray_delegateCache = ConcurrentDictionary()
        let private setModule_ofArray_delegateCache = ConcurrentDictionary()
        let private mapModule_ofArray_delegateCache = ConcurrentDictionary()

        let listModule_ofArray(array: Array, elementType: Type) =
            //let delegate_ = 
            //    listModule_ofArray_delegateCache.GetOrAdd(elementType, valueFactory = fun _ ->
            //        let outputTp = typedefof<_ list>.MakeGenericType(elementType)
            //        let inputTp = array.GetType()
            //        let methodInfo = listModule_ofArray_MethodInfo.Value.MakeGenericMethod(elementType)

            //        let delegateType = 
            //            Expression.GetDelegateType([|inputTp; outputTp|]);
            //        //let tp = typedefof<_ []>.MakeGenericType(elementType)
            //        let delegate_ = 
            //            let m: Func<_, _> = List.ofArray 
            //            let p = m.GetType()
            //            methodInfo.CreateDelegate(delegateType, null)
            //        delegate_
            //    )
            let methodInfo = 
                listModule_ofArray_delegateCache.GetOrAdd(elementType, valueFactory = fun _ ->
                    listModule_ofArray_MethodInfo.Value.MakeGenericMethod(elementType)
                )

            methodInfo.Invoke(null, [|array|])

        let setModule_ofArray(array: Array, elementType: Type) =
            //let delegate_ = 
            //    listModule_ofArray_delegateCache.GetOrAdd(elementType, valueFactory = fun _ ->
            //        let outputTp = typedefof<_ list>.MakeGenericType(elementType)
            //        let inputTp = array.GetType()
            //        let methodInfo = listModule_ofArray_MethodInfo.Value.MakeGenericMethod(elementType)

            //        let delegateType = 
            //            Expression.GetDelegateType([|inputTp; outputTp|]);
            //        //let tp = typedefof<_ []>.MakeGenericType(elementType)
            //        let delegate_ = 
            //            let m: Func<_, _> = List.ofArray 
            //            let p = m.GetType()
            //            methodInfo.CreateDelegate(delegateType, null)
            //        delegate_
            //    )
            let methodInfo = 
                setModule_ofArray_delegateCache.GetOrAdd(elementType, valueFactory = fun _ ->
                    setModule_ofArray_MethodInfo.Value.MakeGenericMethod(elementType)
                )

            methodInfo.Invoke(null, [|array|])

        let private mapModule_ofList_cache = ConcurrentDictionary()

        let mapModule_ofList(dict: Dictionary<obj, obj>, keyTp, valueTp) =
            let methodInfo = 
                mapModule_ofArray_delegateCache.GetOrAdd((keyTp, valueTp), valueFactory = fun _ ->
                    mapModule_ofArray_MethodInfo.Value.MakeGenericMethod([|keyTp; valueTp|])
                )

            let tupleArray =    

                let tupleType, array = 
                    mapModule_ofList_cache.GetOrAdd((keyTp, valueTp), valueFactory = fun _ ->
                        let tupleType =     
                            typedefof<Tuple<_, _>>.MakeGenericType([|keyTp; valueTp|])

                        tupleType, Array.CreateInstance(tupleType, dict.Count)
                    )

                let array = array.Clone() :?> Array

                let mutable i = 0 
                for pair in dict do 
                    let tuple = FSharpValue.MakeTuple([|pair.Key; pair.Value|], tupleType)
                    array.SetValue(tuple, i)
                    i <- i + 1

                array

            methodInfo.Invoke(null, [|tupleArray|])

        let arrayToCSharpList(array: seq<_>, elementType: Type) =
            let tp = typedefof<ResizeArray<_>>.MakeGenericType(elementType)
            let list = Activator.CreateInstance(tp) :?> IList
            for item in array do 
                list.Add(item)
                |> ignore

            list

        let arrayToCSharpHashSet(array: seq<_>, elementType: Type) =
            let tp = typedefof<HashSet<_>>.MakeGenericType(elementType)
            let addMethod = tp.GetMethod("Add")
            let list = Activator.CreateInstance(tp) 
            for item in array do 
                addMethod.Invoke(list, [|item|])
                |> ignore

            list

        let makeOption(elementTp, elementValue) =   
            let optionTp = typedefof<option<_>>.MakeGenericType([|elementTp|])
            let cases = FSharpType.GetUnionCases optionTp
            match elementValue with 
            | null -> 
                FSharpValue.MakeUnion(cases.[0], [||])
            | _ ->
                FSharpValue.MakeUnion(cases.[1], [|elementValue|])


    [<RequireQualifiedAccess>]
    type CollectionType =
        | FSharpList of elementTp: Type
        | Array of elementTp: Type
        | List of elementTp: Type
        | Seq of elementTp: Type
        | Set of elementTp: Type
        | HashSet of elementTp: Type
    with 
        member x.ElementType =
            match x with 
            | FSharpList  tp -> tp
            | Array       tp -> tp
            | List        tp -> tp
            | Seq         tp -> tp
            | Set         tp -> tp
            | HashSet     tp -> tp


        member internal x.MakeObject(elements: ResizeArray<obj>) =
            let elementTp = x.ElementType
            let array() =
                let array = 
                    Array.CreateInstance(elementTp, elements.Count)

                for i = 0 to array.Length-1 do 
                    array.SetValue(elements.[i], i)

                array

            match x with 
            | Array _ -> box (array())
            | FSharpList _ ->
                listModule_ofArray(array(), elementTp)
            | List _ ->
                arrayToCSharpList(elements, elementTp)
            | Seq _ -> box (array())
            | Set _ -> 
                setModule_ofArray(array(), elementTp)
            | HashSet _ ->
                arrayToCSharpHashSet(elements, elementTp)
                
    [<RequireQualifiedAccess>]
    module CollectionType =
        let private collectionTypeNames1 = 
            ["FSharpList`1", CollectionType.FSharpList
             "FSharpSet`1", CollectionType.Set
             "IEnumerable`1", CollectionType.Seq
             "HashSet`1", CollectionType.HashSet
             "List`1", CollectionType.List]

        let getCollectionType (collectionType: Type) =

            let typeName = collectionType.Name

            let r1 =
                collectionTypeNames1
                |> List.tryFind(fun (m, _) -> m = typeName)

            match r1 with 
            | Some (_, collectionTypeFactory) ->
                collectionTypeFactory(collectionType.GetGenericArguments().[0])
                |> Result.Ok
                
            | None -> 
                if collectionType.IsArray then
                    collectionType.GetElementType()
                    |> CollectionType.Array
                    |> Result.Ok
                //else if FSharpType.IsTuple collectionType then
                //    collectionType.GetGenericArguments()
                //    |> CollectionType.Tuple
            
                else 
                    sprintf "Could not extract element type from collection of type %s"  collectionType.FullName    
                    |> Result.Error



    let private dictionaryTpCache = 
        ConcurrentDictionary()



    [<RequireQualifiedAccess>]
    type DictionaryType =
        | FSharpMap of keyTp: Type * valueTp: Type
        | Dictionary of keyTp: Type * valueTp: Type
        | ConcurrrentDictionary of keyTp: Type * valueTp: Type
    
    with
        member x.KeyType =
            match x with 
            | FSharpMap              (keyTp, valueTp) -> keyTp
            | Dictionary             (keyTp, valueTp) -> keyTp
            | ConcurrrentDictionary  (keyTp, valueTp) -> keyTp

        member x.ValueType =
            match x with 
            | FSharpMap              (keyTp, valueTp) -> valueTp
            | Dictionary             (keyTp, valueTp) -> valueTp
            | ConcurrrentDictionary  (keyTp, valueTp) -> valueTp


        member internal x.SetType(keyTp, valueTp) =
            match x with 
            | FSharpMap              (_, _) -> FSharpMap            (keyTp, valueTp)
            | Dictionary             (_, _) -> Dictionary           (keyTp, valueTp)
            | ConcurrrentDictionary  (_, _) -> ConcurrrentDictionary(keyTp, valueTp)


        //member x.EntryTypeName =
        //    //$"巜{x.KeyType.Name}_{x.ValueType.Name}〢"
        //    ("Entry__" + x.KeyType.Name + "_" + x.ValueType.Name).ToLower()
                


        member internal x.MakeObject(elements: Dictionary<obj, obj>) =
            match x with 
            | Dictionary (keyTp, valueTp) ->
                let tp =    
                    dictionaryTpCache.GetOrAdd(x, valueFactory = fun _ ->
                        typedefof<Dictionary<_, _>>.MakeGenericType([|keyTp; valueTp|])
                    )

                let dict = Activator.CreateInstance(tp) :?> IDictionary

                for element in elements do
                    dict.Add(element.Key, element.Value)

                box dict

            | ConcurrrentDictionary (keyTp, valueTp) ->
                let tp =    
                    dictionaryTpCache.GetOrAdd(x, valueFactory = fun _ ->
                        typedefof<ConcurrentDictionary<_, _>>.MakeGenericType([|keyTp; valueTp|])
                    )

                let dict = Activator.CreateInstance(tp) :?> IDictionary

                for element in elements do
                    dict.Add(element.Key, element.Value)

                dict

            | FSharpMap (keyTp, valueTp) ->
                let map = mapModule_ofList(elements, keyTp, valueTp)
                map 


                
    [<RequireQualifiedAccess>]
    module DictionaryType =
        
        let private dictionaryTypeNames1 = 
            [
              "FSharpMap`2", DictionaryType.FSharpMap
              "Dictionary`2", DictionaryType.Dictionary
              "ConcurrentDictionary`2", DictionaryType.ConcurrrentDictionary
            ]


        let getDictionaryType (dictionaryType: Type) =

            let typeName = dictionaryType.Name

            let r1 =
                dictionaryTypeNames1
                |> List.tryFind(fun (m, _) -> m = typeName)

            match r1 with 
            | Some (_, collectionTypeFactory) ->
                let genericArguments = dictionaryType.GetGenericArguments()
                collectionTypeFactory(genericArguments.[0], genericArguments.[1])
                |> Result.Ok
                
            | None -> 
                sprintf "Could not extract element type from dictionary of type %s"  dictionaryType.FullName    
                |> Result.Error



    [<RequireQualifiedAccess>]
    type FsTypeCodeEx =
        | CollectionType of CollectionType
        | DictionaryType of DictionaryType
        | FsTypeCode of FsTypeCode
        | Option     of Type 
        | Tuple      of array<Type>

    let private entityTypeCodeExCache = ConcurrentDictionary()
    //let private typeCodeExCache = ConcurrentDictionary()
    let private tupleCodeExCache = ConcurrentDictionary()




    let getFsTpCodeEx (tp: Type) =
        let getEntiryType(tp: Type) = 
            entityTypeCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
                match CollectionType.getCollectionType tp with 
                | Result.Error _ -> 
                    match DictionaryType.getDictionaryType tp with 
                    | Result.Ok dictionaryType ->
                        dictionaryType
                        |> FsTypeCodeEx.DictionaryType
                    | Result.Error _ ->
                        getFsTpCode tp
                        |> FsTypeCodeEx.FsTypeCode
                | Result.Ok collectionType ->
                    collectionType
                    |> FsTypeCodeEx.CollectionType
            )


        //let getOptionType(tp: Type) =
        //    typeCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
        //        match tp.Name = "FSharpOption`1" with 
        //        | true -> 
        //            let subType = tp.GetGenericArguments().[0]
        //            FsTypeCodeEx.Option(getEntiryType subType, subType)
        //        | false -> getEntiryType(tp)
        //    )

        tupleCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
            let rec loop (tp: Type) =
                
                match FSharpType.IsTuple tp with 
                | false -> 
                    match tp.Name = "FSharpOption`1" with 
                    | true -> 
                        let subType = tp.GetGenericArguments().[0]
                        FsTypeCodeEx.Option(subType)
                    | false -> getEntiryType(tp)

                | true -> 
                    let subTypes = tp.GetGenericArguments()
                    subTypes
                    |> Array.map(fun subType -> 
                        subType
                    )
                    |> FsTypeCodeEx.Tuple
            loop tp
        )


    type FsTypeCodeEx with 
        static member ValueTypeCodeToXmlTypeName(tpCode: TypeCode) =
            let tpName = 
                match tpCode with 
                | TypeCode.Char 
                | TypeCode.DBNull 
                | TypeCode.String -> "string"
                | TypeCode.Single 
                | TypeCode.Double 
                | TypeCode.Decimal -> "decimal"
                | TypeCode.Boolean -> "boolean"
                | TypeCode.DateTime -> "date"
                | TypeCode.Byte
                | TypeCode.SByte 
                | TypeCode.UInt16 
                | TypeCode.UInt32
                | TypeCode.UInt64
                | TypeCode.Int16
                | TypeCode.Int32
                | TypeCode.Int64 -> "int"
                | TypeCode.Empty -> "string"
                | TypeCode.Object -> failwith "Invalid token"

            tpName

        //member x.GetXmlQualifiedName(tp: Type): XmlQualifiedName =

        //    let getName_InLoop(tp: Type) =
        //        (getFsTpCodeEx(tp)).GetXmlQualifiedName(tp).Name

        //    match x with 
        //    | FsTypeCodeEx.Option (elementTpCode, elementType) ->
        //        elementTpCode.GetXmlQualifiedName(elementType)
                
        //    | FsTypeCodeEx.CollectionType collectionType -> 
        //        let name = "ArrayOf" + getName_InLoop collectionType.ElementType
        //        XmlQualifiedName(name)

        //    | FsTypeCodeEx.DictionaryType dictType -> 
        //        let name = "DictOf__" + getName_InLoop dictType.KeyType + "_" + getName_InLoop dictType.ValueType
                    
        //        XmlQualifiedName(name)

        //    | FsTypeCodeEx.FsTypeCode fsTypeCode ->
        //        match fsTypeCode with
        //        | FsTypeCode.Object _
        //        | FsTypeCode.Enum -> XmlQualifiedName(tp.Name)
        //        | FsTypeCode.ValueType tpCode ->
        //            let tpName = FsTypeCodeEx.ValueTypeCodeToXmlTypeName(tpCode)
        //            XmlQualifiedName(tpName, W3XMLSchema)

        //    | FsTypeCodeEx.Tuple tupleTypes ->
        //        let name = 
        //            tupleTypes
        //            |> Array.map(fun (elementTpCode, elementType) ->
        //                elementTpCode.GetXmlQualifiedName(elementType).Name
        //            )
        //            |> String.concat "_"

        //        "tuple" + (tupleTypes.Length.ToString()) + "__" + name
        //        |> XmlQualifiedName

        //member x.GetElementName(tp: Type): string =

        //    let getName_InLoop(tp: Type) =
        //        (getFsTpCodeEx(tp)).GetElementName(tp)

        //    match x with 
        //    | FsTypeCodeEx.Option (elementTpCode, elementType) ->
        //        elementTpCode.GetElementName(elementType)
                
        //    | FsTypeCodeEx.CollectionType collectionType -> 
        //        "ArrayOf" + getName_InLoop collectionType.ElementType

        //    | FsTypeCodeEx.DictionaryType dictType -> 
        //        "DictOf__" + getName_InLoop dictType.KeyType + "_" + getName_InLoop dictType.ValueType
                    

        //    | FsTypeCodeEx.FsTypeCode fsTypeCode ->
        //        match fsTypeCode with
        //        | FsTypeCode.Object _
        //        | FsTypeCode.Enum -> tp.Name
        //        | FsTypeCode.ValueType tpCode -> 
        //            FsTypeCodeEx.ValueTypeCodeToXmlTypeName(tpCode)
        //            |> toTitleCase


        //    | FsTypeCodeEx.Tuple tupleTypes ->
        //        "Tuple" + tupleTypes.Length.ToString()
              

    //type CollectionType with 
    //    member x.TypeName =
    //        FsTypeCodeEx.CollectionType(x).GetXmlQualifiedName(x.ElementType).Name

    //type DictionaryType with 
    //    member x.TypeName =
    //        FsTypeCodeEx.DictionaryType(x).GetXmlQualifiedName(x.KeyType).Name

    //    member x.EntryTypeName =
    //        x.TypeName.Replace("Dictionary__", "Entry__")
                





    //type Type with 
    //    member tp.GetXmlQualifiedName() =
    //        (getFsTpCodeEx tp).GetXmlQualifiedName(tp)
          
    //    member tp.GetElementName() =
    //        (getFsTpCodeEx tp).GetElementName(tp)
          
    
    //[<RequireQualifiedAccess>]
    //type SCasablePropertyType =
    //    | OneFieldSCase of Type
    //    | PropertyInfo of PropertyInfo
    //    | Type of Type
    //    | NamedType of string * Type
    //    | NillableNamedType of string * Type
    //with 

    //    member x.Name =
    //        match x with 
    //        | OneFieldSCase v -> "SCase"
    //        | PropertyInfo v -> v.Name
    //        | Type v -> v.GetElementName()
    //        | NamedType (name, _) -> name
    //        | NillableNamedType (name, _) -> name

    //    member x.Nillable =
    //        match x with 
    //        | OneFieldSCase _
    //        | PropertyInfo _
    //        | Type _
    //        | NamedType _ -> false
    //        | NillableNamedType _ -> true
    
    //    //member private x.Value =
    //    //    match x with 
    //    //    | OneFieldSCase v 
    //    //    | PropertyInfo v -> v
    
    //    member x.PropertyType = 
    //        match x with 
    //        | PropertyInfo v -> v.PropertyType
    //        | OneFieldSCase tp 
    //        | Type tp 
    //        | NamedType (_, tp) 
    //        | NillableNamedType (_, tp) -> tp
            
    //    member x.ToNamedType() =
    //        match x with 
    //        | Type(_) -> x
    //        | NamedType _
    //        | NillableNamedType _ -> x
    //        | _ ->
    //            let tp = x.PropertyType
    //            let tpCode = getFsTpCodeEx(tp)
    //            match tpCode with 
    //            | FsTypeCodeEx.Option _ -> 
    //                NillableNamedType(x.Name, tp)
    //            | _ -> x

    //    member x.ToNamedTypeWith(tp: Type) =
    //        match x with 
    //        | Type (_) -> Type(tp)
    //        | NamedType (name, _) -> NamedType(name, tp)
    //        | NillableNamedType (name, _) -> NillableNamedType(name, tp)
    //        | _ ->
    //            let tpCode = getFsTpCodeEx(tp)
    //            match tpCode with 
    //            | FsTypeCodeEx.Option _ -> 
    //                NillableNamedType(x.Name, tp)
    //            | _ -> NamedType(x.Name, tp)

    //    static member CreateNamedType(name, tp: Type) =
    //        let tpCode = getFsTpCodeEx(tp)
    //        match tpCode with 
    //        | FsTypeCodeEx.Option _ -> 
    //            NillableNamedType(name, tp)
    //        | _ -> NamedType(name, tp)

    //type SCasablePropertyType with 
    //    member x.GenerateElement() =
    //        let name = x.Name
    //        let propTP = x.PropertyType

    //        let x = x.ToNamedType()
    //        match x with 
    //        | SCasablePropertyType.NillableNamedType _ ->
    //            XmlSchemaElement(
    //                Name = name,
    //                SchemaTypeName = propTP.GetXmlQualifiedName(),
    //                IsNillable = true
    //            )

    //        | _ ->

    //            XmlSchemaElement(
    //                Name = name,
    //                SchemaTypeName = propTP.GetXmlQualifiedName()
    //            )



[<AutoOpen>]
module private _DefaultObject =
    let private defaultObjectCache = ConcurrentDictionary()
    let createDefaultObject(tp: Type) =
        defaultObjectCache.GetOrAdd(tp, valueFactory = fun _ ->
            let code = getFsTpCodeEx tp 
            match code with 
            | FsTypeCodeEx.FsTypeCode tpCode ->
                match tpCode with 
                | FsTypeCode.Object tpCode ->
                    match tpCode with 
                    | FsObjectTypeCode.Union cases ->
                        match tp.IsAbstract with 
                        | false -> FormatterServices.GetUninitializedObject(tp)
                        | true ->
                            let nestTp = 
                                tp.GetNestedTypes()
                                |> Array.find(fun m -> m.BaseType = tp)

                            FormatterServices.GetUninitializedObject(nestTp)

                    | _ -> FormatterServices.GetUninitializedObject(tp)

                | _ -> FormatterServices.GetUninitializedObject(tp)

            | _ -> FormatterServices.GetUninitializedObject(tp)
            
        )

[<RequireQualifiedAccess>]
type LinkableFsXmlSerializerTypeMapping =
    | EndPoint of FsXmlSerializerTypeMapping
    | Linked of FsXmlSerializerTypeMapping * LinkableFsXmlSerializerTypeMappingPair
with 
    member x.TargetType =
        match x with 
        | EndPoint v -> v.TargetType
        | Linked (_, v) -> v.TargetType

    member x.WrapOldName =
        match x with 
        | EndPoint v -> v.WrapOldName
        | Linked (m, v) -> m.WrapOldName || v.WrapOldName


    member x.OfXmlSerializable(value: obj) =
        match x with 
        | EndPoint v -> v.OfXmlSerializable value
        | Linked (m, v) -> 
            v.OfXmlSerializable(value)
            |> m.OfXmlSerializable
        
    member x.ToXmlSerializable(value: obj) =
        match x with 
        | EndPoint v -> v.ToXmlSerializable value
        | Linked (m, v) -> 
            m.ToXmlSerializable value
            |> v.ToXmlSerializable

and LinkableFsXmlSerializerTypeMappingPair =
    { OriginType: Type 
      LinkableTypeMapping: LinkableFsXmlSerializerTypeMapping }
with 
    member internal x.FirstNamedType() = 
        match x.LinkableTypeMapping with 
        | LinkableFsXmlSerializerTypeMapping.EndPoint v ->
            match v.WrapOldName with 
            | true -> Some x.OriginType
            | false -> None

        | LinkableFsXmlSerializerTypeMapping.Linked (m, v) ->
            match m.WrapOldName with 
            | true -> Some x.OriginType
            | false -> v.FirstNamedType()


    member x.OfXmlSerializable(value: obj) = x.LinkableTypeMapping.OfXmlSerializable value

    member x.ToXmlSerializable(value: obj) = x.LinkableTypeMapping.ToXmlSerializable value

    member x.TargetType = x.LinkableTypeMapping.TargetType

    member x.WrapOldName = x.LinkableTypeMapping.WrapOldName

        

type FsXmlSerializerTypeMappingPair =
    { OriginType: Type 
      TypeMapping: FsXmlSerializerTypeMapping }


[<AutoOpen>]
module private _Util2 =

    let private getReadXmlObjMethodCache = ConcurrentDictionary()

    let getReadXmlObjMethod(tp: Type) =
        getReadXmlObjMethodCache.GetOrAdd(tp, valueFactory = fun _ ->
            let fullName = typeof<FsIXmlSerializable>.FullName
            let instance = Unchecked.defaultof<FsIXmlSerializable>
            let fsIXmlSerializable = tp.GetInterface_Last(nameof FsIXmlSerializable)
            match fsIXmlSerializable with 
            | None -> None
            | Some itp ->
                let itpMap = tp.GetInterfaceMap(itp)

                let method = 
                    itpMap.TargetMethods
                    |> Array.find(fun m -> m.Name = fullName + "." + nameof instance.ReadXmlObj)

                let defaultObj = createDefaultObject tp

                Some (defaultObj, method)

        )

    //let private getReadSchemaTypeMethodCache = ConcurrentDictionary()

    //let getSchemaType(tp: Type) =
    //    getReadSchemaTypeMethodCache.GetOrAdd(tp, valueFactory = fun _ ->
    //        let fullName = typeof<FsIXmlSerializable>.FullName
    //        let fsIXmlSerializable = tp.GetInterface(nameof FsIXmlSerializable)
    //        match fsIXmlSerializable with 
    //        | null -> None
    //        | itp ->
    //            let itpMap = tp.GetInterfaceMap(itp)

    //            let method = 
    //                itpMap.TargetMethods
    //                |> Array.find(fun m -> m.Name = fullName + "." + nameof FsIXmlSerializableSchema.SchemaType)

    //            let property = method.Invoke(null, [||])

    //            Some property

    //    )

    //let private updateSCasablePropertyType_Cache = ConcurrentDictionary()
    //let private updateSCasablePropertyType_Cache__NoUpdateForWrappedTypeName = ConcurrentDictionary()




    type FsXmlSerializerConfiguration with 


        //member private x.UpdateType_ToXml_Op(tp: Type) =
        //    match x.TypeMapping.TryGetValue tp with 
        //    | false, _ -> None
        //    | true, typeMapping ->
        //        Some typeMapping.TargetType

        //member private x.UpdateType_ToXml(tp: Type) =
        //    match x.TypeMapping.TryGetValue tp with 
        //    | false, _ -> tp
        //    | true, typeMapping ->
        //        typeMapping.TargetType



        //member private x.UpdateType_ToXml_Op_Ex_Private(tp: Type) =
        //    match x.UpdateType_ToXml_Op (tp) with 
        //    | None ->   
        //        match tp.IsGenericType with 
        //        | true -> 
        //            let genericArguments = tp.GetGenericArguments()
        //            let genericTypeDefinition = tp.GetGenericTypeDefinition()

        //            let mutable genericArgumentsChanged = false

        //            let newGenericArguments =
        //                genericArguments
        //                |> Array.map(fun m ->
        //                    match x.UpdateType_ToXml_Op_Ex_Private (m) with 
        //                    | None -> m
        //                    | Some targetType -> 
        //                        genericArgumentsChanged <- true
        //                        targetType
        //                )

        //            match genericArgumentsChanged with 
        //            | true -> 
        //                let newType = genericTypeDefinition.MakeGenericType(newGenericArguments)
        //                Some newType
        //            | false -> None
                        

        //        | false -> None

        //    | Some newTp ->
        //        Some newTp

        //member private x.UpdateType_ToXml_Op__NoUpdateForWrappedTypeName(tp: Type) =
        //    match x.TypeMapping.TryGetValue tp with 
        //    | false, _ -> None
        //    | true, typeMapping -> Some typeMapping
                

        //member private x.UpdateType_ToXml_Op_Ex_Private__NoUpdateForWrappedTypeName(tp: Type) =
        //    match x.UpdateType_ToXml_Op__NoUpdateForWrappedTypeName (tp) with 
        //    | None ->   
        //        match tp.IsGenericType with 
        //        | true -> 
        //            let genericArguments = tp.GetGenericArguments()
        //            let genericTypeDefinition = tp.GetGenericTypeDefinition()

        //            let mutable genericArgumentsChanged = false

        //            let newGenericArguments =
        //                genericArguments
        //                |> Array.map(fun m ->
        //                    match x.UpdateType_ToXml_Op_Ex_Private__NoUpdateForWrappedTypeName (m) with 
        //                    | None -> m
        //                    | Some targetType -> 
        //                        genericArgumentsChanged <- true
        //                        targetType
        //                )

        //            match genericArgumentsChanged with 
        //            | true -> 
        //                let newType = genericTypeDefinition.MakeGenericType(newGenericArguments)
        //                Some newType
        //            | false -> None
                        

        //        | false -> None

        //    | Some tpMapping ->
        //        match tpMapping.WrapOldName with 
        //        | true -> None
        //        | false -> Some tpMapping.TargetType

        //member internal x.UpdateType_ToXml_Op_Ex(tp: Type) =
        //    updateSCasablePropertyType_Cache.GetOrAdd(tp, valueFactory = fun _ ->
        //        x.UpdateType_ToXml_Op_Ex_Private(tp)
        //    )

        //member private x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName_Choice(tp: Type) =
        //    updateSCasablePropertyType_Cache__NoUpdateForWrappedTypeName.GetOrAdd(tp, valueFactory = fun _ ->
        //        let newTp = x.UpdateType_ToXml_Op_Ex_Private__NoUpdateForWrappedTypeName(tp)
        //        match newTp with 
        //        | None -> None, tp, tp.GetXmlQualifiedName()
        //        | Some newTp -> Some 0, newTp, newTp.GetXmlQualifiedName()
        //    )

        //member private x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName(tp: Type) =
        //    let (_, a, b) = x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName_Choice(tp)
        //    a, b

        //member private x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName_Op(tp: Type) =
        //    let (code, a, b) = x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName_Choice(tp)
        //    match code with 
        //    | None -> None
        //    | Some _ -> Some (a, b) 

        //member private x.CreateNamedTp(tp: SCasablePropertyType, newTp) =
        //    let tp = 
        //        match tp with 
        //        | SCasablePropertyType.Type _ -> SCasablePropertyType.Type newTp
        //        | _ -> tp

        //    tp.ToNamedTypeWith(newTp)
                

        //member private x.UpdateSCasablePropertyType_ToXml_Ex(tp: SCasablePropertyType) =
        //    match x.UpdateType_ToXml_Op_Ex(tp.PropertyType) with 
        //    | None -> tp
        //    | Some newTp ->
        //        x.CreateNamedTp(tp, newTp)
              

        //member internal x.UpdateSCasablePropertyTypeAndValue_ToXml(tp: SCasablePropertyType, value: obj) =
        //    match x.UpdateTypeAndValue_ToXml(tp.PropertyType, value) with 
        //    | None -> x.UpdateSCasablePropertyType_ToXml_Ex tp, value
        //    | Some (newTp, newValue) ->
        //        x.CreateNamedTp(tp, newTp), newValue



        //member internal x.UpdateType_ToXml_Ex(tp: Type) =
        //    match x.UpdateType_ToXml_Op_Ex(tp) with 
        //    | None -> tp
        //    | Some tp -> tp

        //member internal x.UpdateType_ToXml_Ex__NoUpdateForWrappedTypeName(tp: Type) =
        //    x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName(tp) 

        //member internal x.UpdateSCasablePropertyType_ToXml(tp: SCasablePropertyType) =
        //    match x.UpdateType_ToXml_Op_Ex (tp.PropertyType) with 
        //    | None -> tp
        //    | Some newTp ->
        //        x.CreateNamedTp(tp, newTp)

        //member internal x.UpdateSCasablePropertyType_ToXml__NoUpdateForWrappedTypeName(tp: SCasablePropertyType) =
        //    match x.UpdateType_ToXml_Op_Ex__NoUpdateForWrappedTypeName_Op (tp.PropertyType) with 
        //    | None -> tp
        //    | Some (newTp, _) ->
        //        x.CreateNamedTp(tp, newTp)
                
        member private x.AddTypeMappingByTypeEntity(tp: Type) =
            
            match x.FsIXmlSerializableTypeMappingCache.TryGetValue(tp) with 
            | true, v -> v
            | false, _ ->
                let r: FsXmlSerializerTypeMapping option =
                    match tp.GetInterface_Last(nameof FsIXmlSerializableTypeMapping + "`2") with 
                    | None -> None
                    | Some itp ->
                        match x.TypeMapping.TryGetValue(tp) with 
                        | true, v -> Some v
                        | false, _ ->
                            let fullName = typeof<FsIXmlSerializableTypeMapping>.FullName
                            let genericArguments = itp.GetGenericArguments()
                            let itpMap = tp.GetInterfaceMap(itp)
                            let method_toXml = 
                                itpMap.TargetMethods
                                |> Array.find(fun m -> 
                                    m.Name.StartsWith(fullName) && m.Name.EndsWith (".ToXml")
                                )

                            let method_ofXml = 
                                itpMap.TargetMethods
                                |> Array.find(fun m -> 
                                    m.Name.StartsWith(fullName) && m.Name.EndsWith (".OfXml")
                                )

                            let defaultObj = createDefaultObject tp :?> FsIXmlSerializableTypeMapping

                            let tpMapping =
                                { TargetType = genericArguments.[1]
                                  ToXmlSerializable = (fun tpObj ->
                                    method_toXml.Invoke(tpObj, [||])
                              
                                  )
                                  OfXmlSerializable = (fun xml ->
                                    method_ofXml.Invoke(defaultObj, [|tp; xml|])
                                  )
                                  WrapOldName = defaultObj.WrapOldName
                                }

                            x.TypeMapping.Add(tp, tpMapping)
                            Some tpMapping


                x.FsIXmlSerializableTypeMappingCache.TryAdd(tp, r)
                |> ignore

                r

        member x.AddTypeMappingByType(tp: Type) =
            match tp.IsGenericType with
            | true -> 
                x.AddTypeMappingByTypeEntity(tp)
                |> ignore

                tp.GetGenericArguments()
                |> Array.iter(fun tp -> x.AddTypeMappingByType(tp))


            | false -> 
                x.AddTypeMappingByTypeEntity(tp)
                |> ignore


        member internal x.TryGetTypeMapping(tp: Type): LinkableFsXmlSerializerTypeMappingPair option =
            match x.TypeMapping.TryGetValue (tp) with 
            | false, _ -> None
            | true, typeMapping ->
                //match x.AddTypeMappingByTypeEntity(tpMapping.TargetType) with 
                //| None -> 

                //| Some targetType ->
                //    failwithf
                //        "Not implemented when target type %A is also FsIXmlSerializableTypeMapping" 
                //        tpMapping.TargetType
                x.AddTypeMappingByType(typeMapping.TargetType)
                
                match x.TryGetTypeMapping(typeMapping.TargetType) with 
                | None -> 
                    { OriginType = tp
                      LinkableTypeMapping = LinkableFsXmlSerializerTypeMapping.EndPoint typeMapping }
                    |> Some

                | Some tpMappingPair ->
                    { OriginType = tp
                      LinkableTypeMapping = LinkableFsXmlSerializerTypeMapping.Linked (typeMapping, tpMappingPair) }
                    |> Some




        //member internal x.UpdateSCasablePropertyType_ToXml_Op(tp: SCasablePropertyType) =
        //    match x.TypeMapping.TryGetValue (tp.PropertyType) with 
        //    | false, _ -> None
        //    | true, typeMapping ->
        //        let newTp = 
        //            x.CreateNamedTp(tp, typeMapping.TargetType)

        //        {|
        //            PropertyType = newTp
        //            TypeMapping = typeMapping
        //        |}
                
        //        |> Some


    //type SCasablePropertyTypeWithTypeMapping =
    //    { SCasablePropertyType : SCasablePropertyType 
    //      TypeMapping: option<FsXmlSerializerTypeMappingPair>
    //      Type__NoUpdateForWrappedType: Type
    //      Type__NoUpdateForWrappedTypeName: XmlQualifiedName }

    //with 
    //    member x.GenerateElement() =
    //        let element = x.SCasablePropertyType.GenerateElement()

    //        element.SchemaTypeName <- x.Type__NoUpdateForWrappedTypeName
    //        element


    //    static member CreateNamedType(name: string, tp: Type, configuration: FsXmlSerializerConfiguration) =
    //        let newTp = configuration.UpdateType_ToXml_Ex tp
    //        let tpMapping = 
    //            configuration.TryGetTypeMapping tp

    //        let newTp_NoUpdateForWrappedTypeName, newTpName = 
    //            configuration.UpdateType_ToXml_Ex__NoUpdateForWrappedTypeName tp

    //        { SCasablePropertyType = SCasablePropertyType.CreateNamedType(name, newTp)
    //          TypeMapping = tpMapping
    //          Type__NoUpdateForWrappedType = newTp_NoUpdateForWrappedTypeName
    //          Type__NoUpdateForWrappedTypeName = newTpName
    //          }



        //member internal x.OF_XML_UpdateSCasablePropertyType(tp: SCasablePropertyType) =
        //    match x.OF_XML_UpdateType(tp.PropertyType) with 
        //    | None -> tp
        //    | Some (newTp) ->
        //        SCasablePropertyType.CreateNamedType(tp.Name, newTp)

