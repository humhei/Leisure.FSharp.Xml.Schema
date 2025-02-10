// Learn more about F# at http://fsharp.org
namespace Leisure.FSharp.Xml.Schema
#nowarn "0104"
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

type FsIXmlSerializableTypeMapping = interface end

type FsIXmlSerializableTypeMapping<'T, 'XmlT> =
    inherit FsIXmlSerializableTypeMapping
    static abstract member OfXml: Type * 'XmlT -> 'T
    abstract member ToXml: unit -> 'XmlT



type FsXmlSerializerTypeMapping =
    { ToXmlSerializable: obj -> obj 
      OfXmlSerializable: obj -> obj
      TargetType: Type}


[<AutoOpen>]
module private _FsXmlSerializerConfigurationUtils = 
    let fsIXmlSerializableTypeMappingCache = ConcurrentDictionary()

type FsXmlSerializerConfiguration =
    { TypeMapping: Dictionary<Type, FsXmlSerializerTypeMapping> }
with 
    member x.AddTypeMapping<'Origin, 'Target>(toXml: 'Origin -> 'Target, ofXml: 'Target -> 'Origin) =
        let typeMapping = 
            let toXml (v: obj) =
                toXml (v :?> 'Origin)
                |> box

            let ofXml(v: obj) =
                ofXml(v :?> 'Target)
                |> box

            { ToXmlSerializable = toXml 
              OfXmlSerializable = ofXml
              TargetType = typeof<'Target>}

        x.TypeMapping.Add(typeof<'Origin>, typeMapping)
        x

    member private x.AddTypeMappingByTypeEntity(tp: Type) =
        
        fsIXmlSerializableTypeMappingCache.GetOrAdd(tp, valueFactory = fun _ ->
            match tp.GetInterface(nameof FsIXmlSerializableTypeMapping + "`2") with 
            | null -> None
            | itp ->
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

                    let tpMapping =
                        { TargetType = genericArguments.[1]
                          ToXmlSerializable = (fun tpObj ->
                            method_toXml.Invoke(tpObj, [||])
                        
                          )
                          OfXmlSerializable = (fun xml ->
                            method_ofXml.Invoke(null, [|tp; xml|])
                          )
                        }

                    x.TypeMapping.Add(tp, tpMapping)
                    Some tpMapping
        )
        |> ignore

    member x.AddTypeMappingByType(tp: Type) =
        match tp.IsGenericType with 
        | true -> 
            tp.GetGenericArguments()
            |> Array.iter(fun tp -> x.AddTypeMappingByType(tp))

        | false -> 
            x.AddTypeMappingByTypeEntity(tp)

    static member DefaultValue =
        { TypeMapping = Dictionary() }

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
    static abstract ReadXmlObj: tp: Type * reader: XmlReader * config: FsXmlSerializerConfiguration -> obj
    
type FsIXmlSerializable<'T> =
    inherit FsIXmlSerializable
    static abstract member ReadXml: tp: Type * reader:XmlReader * config: FsXmlSerializerConfiguration -> 'T


//type FsIXmlSerializableSchema =
//    inherit FsIXmlSerializable
    
//    static abstract member SchemaType: unit -> Type

//type FsIXmlSerializableSchema<'T> =
//    inherit FsIXmlSerializableSchema
//    static abstract member ReadXml: tp: Type * reader:XmlReader * config: FsXmlSerializerConfiguration -> 'T




[<AutoOpen>]
module private _Utils =
    

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

    let [<Literal>] W3XMLSchema = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] W3XMLSchemaInstance = "http://www.w3.org/2001/XMLSchema-instance"

    let private textInfo = (new CultureInfo("en-US", false)).TextInfo
    let toTitleCase (text: string) = textInfo.ToTitleCase(text)

    [<RequireQualifiedAccess>]
    type FsObjectTypeCode =
        | Record 
        | Union of UnionCaseInfo []
        | SingletonCaseUnion of UnionCaseInfo
        | FsXmlSerializable

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
                            match tp.GetInterface(nameof FsIXmlSerializable) with 
                            | null -> failwithf "[Xml serializable] Un supported type %A, using FsXmlSerializableSchema instead" tp

                            | itp ->
                                FsTypeCode.Object(FsObjectTypeCode.FsXmlSerializable)
                
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

        let makeOption(tp, elementType: Type, elementValue) =
            let cases = FSharpType.GetUnionCases tp
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
        | Option     of FsTypeCodeEx * Type 
        | Tuple      of array<FsTypeCodeEx * Type>

    let private entityTypeCodeExCache = ConcurrentDictionary()
    let private typeCodeExCache = ConcurrentDictionary()
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


        let getOptionType(tp: Type) =
            typeCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
                match tp.Name = "FSharpOption`1" with 
                | true -> 
                    let subType = tp.GetGenericArguments().[0]
                    FsTypeCodeEx.Option(getEntiryType subType, subType)
                | false -> getEntiryType(tp)
            )

        tupleCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
            match FSharpType.IsTuple tp with 
            | false -> getOptionType(tp)
            | true -> 
                let subTypes = tp.GetGenericArguments()
                subTypes
                |> Array.map(fun subType -> 
                    getOptionType subType, subType
                )
                |> FsTypeCodeEx.Tuple
        )


    type FsTypeCodeEx with 
        member private x.ValueTypeCodeToXmlTypeName(tpCode: TypeCode) =
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

        member x.GetXmlQualifiedName(tp: Type): XmlQualifiedName =

            let getName_InLoop(tp: Type) =
                (getFsTpCodeEx(tp)).GetXmlQualifiedName(tp).Name

            match x with 
            | FsTypeCodeEx.Option (elementTpCode, elementType) ->
                elementTpCode.GetXmlQualifiedName(elementType)
                
            | FsTypeCodeEx.CollectionType collectionType -> 
                let name = "ArrayOf" + getName_InLoop collectionType.ElementType
                XmlQualifiedName(name)

            | FsTypeCodeEx.DictionaryType dictType -> 
                let name = "DictOf__" + getName_InLoop dictType.KeyType + "_" + getName_InLoop dictType.ValueType
                    
                XmlQualifiedName(name)

            | FsTypeCodeEx.FsTypeCode fsTypeCode ->
                match fsTypeCode with
                | FsTypeCode.Object _
                | FsTypeCode.Enum -> XmlQualifiedName(tp.Name)
                | FsTypeCode.ValueType tpCode ->
                    let tpName = x.ValueTypeCodeToXmlTypeName(tpCode)
                    XmlQualifiedName(tpName, W3XMLSchema)

            | FsTypeCodeEx.Tuple tupleTypes ->
                let name = 
                    tupleTypes
                    |> Array.map(fun (elementTpCode, elementType) ->
                        elementTpCode.GetXmlQualifiedName(elementType).Name
                    )
                    |> String.concat "_"

                "tuple" + (tupleTypes.Length.ToString()) + "__" + name
                |> XmlQualifiedName

        member x.GetElementName(tp: Type): string =

            let getName_InLoop(tp: Type) =
                (getFsTpCodeEx(tp)).GetElementName(tp)

            match x with 
            | FsTypeCodeEx.Option (elementTpCode, elementType) ->
                elementTpCode.GetElementName(elementType)
                
            | FsTypeCodeEx.CollectionType collectionType -> 
                "ArrayOf" + getName_InLoop collectionType.ElementType

            | FsTypeCodeEx.DictionaryType dictType -> 
                "DictOf__" + getName_InLoop dictType.KeyType + "_" + getName_InLoop dictType.ValueType
                    

            | FsTypeCodeEx.FsTypeCode fsTypeCode ->
                match fsTypeCode with
                | FsTypeCode.Object _
                | FsTypeCode.Enum -> tp.Name
                | FsTypeCode.ValueType tpCode -> 
                    x.ValueTypeCodeToXmlTypeName(tpCode)
                    |> toTitleCase


            | FsTypeCodeEx.Tuple tupleTypes ->
                "Tuple" + tupleTypes.Length.ToString()
              

    type CollectionType with 
        member x.TypeName =
            FsTypeCodeEx.CollectionType(x).GetXmlQualifiedName(x.ElementType).Name

    type DictionaryType with 
        member x.TypeName =
            FsTypeCodeEx.DictionaryType(x).GetXmlQualifiedName(x.KeyType).Name

        member x.EntryTypeName =
            x.TypeName.Replace("Dictionary__", "Entry__")
                

    //let private defaultObjectCache = ConcurrentDictionary()
    //let createDefaultObject(tp: Type) =
    //    defaultObjectCache.GetOrAdd(tp, valueFactory = fun _ ->
    //        let code = getFsTpCodeEx tp 
    //        match code with 
    //        | FsTypeCodeEx.FsTypeCode tpCode ->
    //            match tpCode with 
    //            | FsTypeCode.Object tpCode ->
    //                match tpCode with 
    //                | FsObjectTypeCode.Union _ ->FormatterServices.GetUninitializedObject(tp)
    //                | _ -> FormatterServices.GetUninitializedObject(tp)

    //            | _ -> FormatterServices.GetUninitializedObject(tp)

    //        | _ -> FormatterServices.GetUninitializedObject(tp)
            
    //    )




    type Type with 
        member tp.GetXmlQualifiedName() =
            (getFsTpCodeEx tp).GetXmlQualifiedName(tp)
          
        member tp.GetElementName() =
            (getFsTpCodeEx tp).GetElementName(tp)
          
    
    [<RequireQualifiedAccess>]
    type SCasablePropertyType =
        | OneFieldSCase of PropertyInfo
        | PropertyInfo of PropertyInfo
        | Type of Type
        | NamedType of string * Type
        | NillableNamedType of string * Type
    with 

        member x.Name =
            match x with 
            | OneFieldSCase v -> "SCase"
            | PropertyInfo v -> v.Name
            | Type v -> v.GetElementName()
            | NamedType (name, _) -> name
            | NillableNamedType (name, _) -> name

        member x.Nillable =
            match x with 
            | OneFieldSCase _
            | PropertyInfo _
            | Type _
            | NamedType _ -> false
            | NillableNamedType _ -> true
    
        //member private x.Value =
        //    match x with 
        //    | OneFieldSCase v 
        //    | PropertyInfo v -> v
    
        member x.PropertyType = 
            match x with 
            | OneFieldSCase v 
            | PropertyInfo v -> v.PropertyType
            | Type tp -> tp
            | NamedType (_, tp) 
            | NillableNamedType (_, tp) -> tp
            
        member x.ToNamedType() =
            match x with 
            | NamedType _
            | NillableNamedType _ -> x
            | _ ->
                let tp = x.PropertyType
                let tpCode = getFsTpCodeEx(tp)
                match tpCode with 
                | FsTypeCodeEx.Option _ -> 
                    NillableNamedType(x.Name, tp)
                | _ -> x

        member x.ToNamedTypeWith(tp: Type) =
            match x with 
            | NamedType (name, _) -> NamedType(name, tp)
            | NillableNamedType (name, _) -> NillableNamedType(name, tp)
            | _ ->
                let tpCode = getFsTpCodeEx(tp)
                match tpCode with 
                | FsTypeCodeEx.Option _ -> 
                    NillableNamedType(x.Name, tp)
                | _ -> NamedType(x.Name, tp)

        static member CreateNamedType(name, tp: Type) =
            let tpCode = getFsTpCodeEx(tp)
            match tpCode with 
            | FsTypeCodeEx.Option _ -> 
                NillableNamedType(name, tp)
            | _ -> NamedType(name, tp)

    type SCasablePropertyType with 
        member x.GenerateElement() =
            let name = x.Name
            let propTP = x.PropertyType

            let x = x.ToNamedType()
            match x with 
            | SCasablePropertyType.NillableNamedType _ ->
                XmlSchemaElement(
                    Name = name,
                    SchemaTypeName = propTP.GetXmlQualifiedName(),
                    IsNillable = true
                )

            | _ ->

                XmlSchemaElement(
                    Name = name,
                    SchemaTypeName = propTP.GetXmlQualifiedName()
                )




[<AutoOpen>]
module private _Util2 =

    let private getReadXmlObjMethodCache = ConcurrentDictionary()

    let getReadXmlObjMethod(tp: Type) =
        getReadXmlObjMethodCache.GetOrAdd(tp, valueFactory = fun _ ->
            let fullName = typeof<FsIXmlSerializable>.FullName
            let fsIXmlSerializable = tp.GetInterface(nameof FsIXmlSerializable)
            match fsIXmlSerializable with 
            | null -> None
            | itp ->
                let itpMap = tp.GetInterfaceMap(itp)

                let method = 
                    itpMap.TargetMethods
                    |> Array.find(fun m -> m.Name = fullName + "." + nameof FsIXmlSerializable.ReadXmlObj)

                Some method

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

    let private updateSCasablePropertyType_Cache = ConcurrentDictionary()

    type FsXmlSerializerConfiguration with 


        member private x.UpdateType_ToXml_Op(tp: Type) =
            match x.TypeMapping.TryGetValue tp with 
            | false, _ -> None
            | true, typeMapping ->
                Some typeMapping.TargetType

        member private x.UpdateType_ToXml(tp: Type) =
            match x.TypeMapping.TryGetValue tp with 
            | false, _ -> tp
            | true, typeMapping ->
                typeMapping.TargetType



        member private x.UpdateType_ToXml_Op_Ex_Private(tp: Type) =
            match x.UpdateType_ToXml_Op (tp) with 
            | None ->   
                match tp.IsGenericType with 
                | true -> 
                    let genericArguments = tp.GetGenericArguments()
                    let genericTypeDefinition = tp.GetGenericTypeDefinition()

                    let mutable genericArgumentsChanged = false

                    let newGenericArguments =
                        genericArguments
                        |> Array.map(fun m ->
                            match x.UpdateType_ToXml_Op_Ex_Private (m) with 
                            | None -> m
                            | Some targetType -> 
                                genericArgumentsChanged <- true
                                targetType
                        )

                    match genericArgumentsChanged with 
                    | true -> 
                        let newType = genericTypeDefinition.MakeGenericType(newGenericArguments)
                        Some newType
                    | false -> None
                        

                | false -> None

            | Some newTp ->
                Some newTp

        member internal x.UpdateType_ToXml_Op_Ex(tp: Type) =
            updateSCasablePropertyType_Cache.GetOrAdd(tp, valueFactory = fun _ ->
                x.UpdateType_ToXml_Op_Ex_Private(tp)
            )

        member private x.CreateNamedTp(tp: SCasablePropertyType, newTp) =
            let tp = 
                match tp with 
                | SCasablePropertyType.Type _ -> SCasablePropertyType.Type newTp
                | _ -> tp

            tp.ToNamedTypeWith(newTp)
                

        member private x.UpdateSCasablePropertyType_ToXml_Ex(tp: SCasablePropertyType) =
            match x.UpdateType_ToXml_Op_Ex(tp.PropertyType) with 
            | None -> tp
            | Some newTp ->
                x.CreateNamedTp(tp, newTp)
              

        member internal x.UpdateSCasablePropertyTypeAndValue_ToXml(tp: SCasablePropertyType, value: obj) =
            match x.UpdateTypeAndValue_ToXml(tp.PropertyType, value) with 
            | None -> x.UpdateSCasablePropertyType_ToXml_Ex tp, value
            | Some (newTp, newValue) ->
                x.CreateNamedTp(tp, newTp), newValue



        member internal x.UpdateType_ToXml_Ex(tp: Type) =
            match x.UpdateType_ToXml_Op_Ex(tp) with 
            | None -> tp
            | Some tp -> tp

        member internal x.UpdateSCasablePropertyType_ToXml(tp: SCasablePropertyType) =
            match x.UpdateType_ToXml_Op_Ex (tp.PropertyType) with 
            | None -> tp
            | Some newTp ->
                x.CreateNamedTp(tp, newTp)
                

        member internal x.UpdateSCasablePropertyType_ToXml_Op(tp: SCasablePropertyType) =
            match x.TypeMapping.TryGetValue (tp.PropertyType) with 
            | false, _ -> None
            | true, typeMapping ->
                let newTp = 
                    x.CreateNamedTp(tp, typeMapping.TargetType)

                {|
                    PropertyType = newTp
                    TypeMapping = typeMapping
                |}
                
                |> Some



        //member internal x.OF_XML_UpdateSCasablePropertyType(tp: SCasablePropertyType) =
        //    match x.OF_XML_UpdateType(tp.PropertyType) with 
        //    | None -> tp
        //    | Some (newTp) ->
        //        SCasablePropertyType.CreateNamedType(tp.Name, newTp)