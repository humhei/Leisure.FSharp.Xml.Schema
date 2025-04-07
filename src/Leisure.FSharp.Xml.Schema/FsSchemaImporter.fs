// Learn more about F# at http://fsharp.org
namespace Leisure.FSharp.Xml.Schema
#nowarn "0104"
#nowarn "3535"
#nowarn "3536"

open System

open System.Collections.Generic


open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Xml
open System.Xml.Schema


open FsSchemaTypesAST



type private IndexedXmlSchemaType =
    { Index: int 
      XmlSchemaType: FsSchemaType }

type private TypeElementsCache() =
    let tpNames = HashSet()
    let mutable cache = ConcurrentDictionary<Type, IndexedXmlSchemaType>()

    member x.TryGet(tp: Type) =
        match cache.TryGetValue(tp) with 
        | true, v -> Some v
        | false, _ -> None

    member x.Add(tp, schemaType) =
        let index = cache.Count
        let indexedType =
            { Index = index 
              XmlSchemaType = schemaType }

        cache.TryAdd(tp, indexedType)
        |> ignore

    member x.GetOrAdd(tp, valueFactory) =
        let index = cache.Count
        cache.GetOrAdd(tp, valueFactory = fun tp ->
            match tpNames.Contains tp with 
            | true -> failwithf "Duplicate type Name %A is not supported" tp
            | false -> 
                tpNames.Add(tp)
                |> ignore
            { Index = index 
              XmlSchemaType = valueFactory tp }
        )

    //member private x.UpdateRecusiveTypesOneFace(level: int) =  
    //    let newCache = ConcurrentDictionary()
    //    for pair in cache do    
    //        let newValue = 
    //            { pair.Value with 
    //                XmlSchemaType = 
    //                    pair.Value.XmlSchemaType.MapRecursive (level = level, f = fun tp ->
    //                        match x.TryGet(tp) with 
    //                        | Some schemaType ->
    //                            schemaType.XmlSchemaType
    //                        | None -> failwithf "Invalid token, schema type %A should be cached in previous" tp
    //                    )
    //            }

    //        newCache.TryAdd(pair.Key, newValue)
    //        |> ignore

    //    cache <- newCache

    /// allow only at most 2 Recusive stack, otherwise please using CustomMapping instead to avoid complex type structure
    member x.UpdateRecusiveTypes() =  
        //let rec loop () =
        //    x.UpdateRecusiveTypesOneFace()
        //    let mutable exists_RecusiveType_NotUpdated = false
        //    for pair in cache do    
        //        pair.Value.XmlSchemaType.MapSubSchemaType_TillRecursive(fun m ->
        //            match m with 
        //            | FsSchemaType.Recursive (_, None) -> 
        //                exists_RecusiveType_NotUpdated <- true
        //                m
        //            | _ -> m
        //        )
        //        |> ignore

        //    match exists_RecusiveType_NotUpdated with 
        //    | true -> loop ()
        //    | false -> ()

        //loop ()
        //x.UpdateRecusiveTypesOneFace()
        let texts() = 
            cache
            |> List.ofSeq
            |> List.map(fun m -> m.ToString())
            |> String.concat "\n\n\n"

        //x.UpdateRecusiveTypesOneFace(level = 3)
        //[1..2]
        //|> List.iter(fun level ->
        //    let texts = texts()
        //    x.UpdateRecusiveTypesOneFace(level = level)
        //)

        //x.UpdateRecusiveTypesOneFace(level = 1)
        //x.UpdateRecusiveTypesOneFace(level = 10)
        let texts = texts()
        ()
        //[1..5]
        //|> List.iter(fun i -> 
        //    let texts = 
        //        cache
        //        |> List.ofSeq
        //        |> List.map(fun m -> m.ToString())
        //        |> String.concat "\n\n\n"

        //    x.UpdateRecusiveTypesOneFace()
        //)


    member x.Count = cache.Count

    member x.ContainsKey(k) = cache.ContainsKey k

    member x.Values = cache.Values

    member x.Keys = cache.Keys

    member x.Pairs = List.ofSeq cache



module _Entry = 

    type Entry<'Key, 'Value> =
        { Key: 'Key 
          Value: 'Value }

open _Entry

[<RequireQualifiedAccess>]
module private Entry =
    let private cache = ConcurrentDictionary()

    let makeEntryGenericType(dictionaryType: DictionaryType) =
        let keyType = dictionaryType.KeyType
        let valueType = dictionaryType.ValueType

        cache.GetOrAdd((keyType, valueType), valueFactory = fun _ ->
            typedefof<Entry<_, _>>.MakeGenericType(keyType, valueType)
        )

    let createByObjects(key: obj, value: obj) (dictionaryType: DictionaryType) =
        let genericTp = makeEntryGenericType(dictionaryType)
        FSharpValue.MakeRecord(genericTp, [|key; value|])

type SCase<'T> = SCase of 'T


type private WrapOldNameEnum =
    | WrapOldName = 0
    | UsingCurrentName = 1



[<RequireQualifiedAccess>]
module private XmlSchemaElement =
    let create_isOption isOption name schemaType =
        match isOption with 
        | true -> 
            XmlSchemaElement(
                Name = name,
                SchemaType = schemaType,
                IsNillable = true
            )

        | false -> 
            XmlSchemaElement(
                Name = name,
                SchemaType = schemaType
            )




type FsSchemaImporter(configuration: FsXmlSerializerConfiguration) =
    
    member private x.TpToXmlSchemaObject(tp: Type): XmlSchemaObject list =
        let typeElements = TypeElementsCache()
        let rec loop1 accumTps (tp: Type) =
            let loop tp =
                //loop1 (tp :: accumTps) tp
                try 
                    let recusiveType = List.contains tp accumTps
                    match recusiveType with 
                    | false -> 
                        loop1 (tp :: accumTps) tp

                    | true -> 
                        FsSchemaType.Recursive (tp, None)

                    //match typeElements.TryGet tp with 
                    //| Some v ->  v.XmlSchemaType 
                    //| None ->  

                with :? InvlaidXmlSchemaTypeException as exp ->
                    let accumTps = 
                        tp :: accumTps
                        |> List.rev
                        |> List.map(fun m -> m.Name)

                    failwithf "TypeStacks: %A\n%A" accumTps exp

            match typeElements.TryGet tp with 
            | None ->
                let rec r(tp: Type) = 
                    match getListRestriction tp with 
                    | None ->

                        let tpCode = getFsTpCodeEx (tp)

                        let r = 
                            match tpCode with 
                            | FsTypeCodeEx.Tuple (tps) ->
                                let children = 
                                    tps
                                    |> List.ofArray
                                    |> List.map(fun (tp) ->
                                        loop tp
                                    )

                                //let tpCodes: FsSchemaType list =
                                //    tps
                                //    |> Array.map loop
                                //    |> List.ofArray

                                FsSchemaComplexType_Tuple.Create(tp, children)
                                |> FsSchemaComplexGenericType.Tuple
                                |> FsSchemaComplexType.Generic
                                |> FsSchemaType.ComplexType


                            | FsTypeCodeEx.Option (elementTp) ->
                                loop elementTp
                                |> FsSchemaType.Option

                            | FsTypeCodeEx.DictionaryType dictionaryType ->
                                let keySchemaType = loop dictionaryType.KeyType
                                let valueSchemaType = loop dictionaryType.ValueType

                                FsSchemaComplexType_Dictionary.Create(dictionaryType, keySchemaType, valueSchemaType)
                                |> FsSchemaComplexGenericType.Dictionary
                                |> FsSchemaComplexType.Generic
                                |> FsSchemaType.ComplexType

                
                            | FsTypeCodeEx.CollectionType collectionType -> 
                                let elementSchemaType = loop collectionType.ElementType
                                FsSchemaComplexType_Collection.Create(collectionType, elementSchemaType)
                                |> FsSchemaComplexGenericType.Collection
                                |> FsSchemaComplexType.Generic
                                |> FsSchemaType.ComplexType

                            | FsTypeCodeEx.FsTypeCode tpCode ->

                                match tpCode with 
                                | FsTypeCode.Enum ->
                                    FsSchemaSimpleType_Enum tp
                                    |> FsSchemaSimpleType.EnumType 
                                    |> FsSchemaType.SimpleType

                
                                | FsTypeCode.ValueType tpCode -> 
                                    tpCode
                                    |> FsSchemaSimpleType_Value
                                    |> FsSchemaSimpleType.ValueType
                                    |> FsSchemaType.SimpleType
                  
                                | FsTypeCode.Object fsObjectTypeCode ->   
                    
                                    match fsObjectTypeCode with 
                                    | FsObjectTypeCode.FsXmlSerializableTypeMapping ->
                                        failwithf "Invalid token, using FsXmlSerializableTypeMapping to instead %A" tp
                      

                                    | FsObjectTypeCode.JsonPOCO jsonPoco ->
                                        let props = Array.toList jsonPoco.Members

                                        let elements, schemaTypes = 
                                            props
                                            |> List.map(fun prop ->
                                                let propTp = prop.PropertyType 
                                                let schemaType = loop propTp
                                                let element = 
                                                    schemaType.GenerateFsElement(Some prop.Name)

                                                (element, schemaType)
                                            )
                                            |> List.unzip

                                        { FsSchemaComplexType_Record.Elements = elements 
                                          ElementSchemaTypes = schemaTypes
                                          PropertyInfos = props
                                          JsonPOCO = Some jsonPoco
                                          Type = tp }
                                        |> FsSchemaComplexType.Record
                                        |> FsSchemaType.ComplexType

                                    | FsObjectTypeCode.Record ->

                                        let props = 
                                            FSharpType.GetRecordFields tp
                                            |> List.ofArray

                                        let elements, schemaTypes = 
                                            props
                                            |> List.map(fun prop ->
                                                let propTp = prop.PropertyType 
                                                let schemaType = loop propTp
                                                let element = 
                                                    schemaType.GenerateFsElement(Some prop.Name)

                                                (element, schemaType)
                                            )
                                            |> List.unzip

                                        { FsSchemaComplexType_Record.Elements = elements 
                                          ElementSchemaTypes = schemaTypes
                                          PropertyInfos = props
                                          JsonPOCO = None
                                          Type = tp }
                                        |> FsSchemaComplexType.Record
                                        |> FsSchemaType.ComplexType

                                    | FsObjectTypeCode.SingletonCaseUnion ucase ->
                                        let fields = ucase.GetFields()

                                        match fields with 
                                        | [||] -> 
                                            FsSchemaComplexType_SingleCaseUnion_NoField.Create(
                                                ucase, tp
                                            )
                                            |> FsSchemaComplexType_SingleCaseUnion.NoField
                                            |> FsSchemaComplexType.SinglecaseUnion
                                            |> FsSchemaType.ComplexType


                                        | [|field|] ->  
                                            let fieldSchemaType = loop field.PropertyType
                                            FsSchemaComplexType_SingleCaseUnion_OneField.Create(ucase, tp, field, fieldSchemaType)
                                            |> FsSchemaComplexType_SingleCaseUnion.OneField
                                            |> FsSchemaComplexType.SinglecaseUnion
                                            |> FsSchemaType.ComplexType

                                        | fields -> 
                                            let fields = List.ofSeq fields

                                            let fieldSchemaTypes =
                                                fields
                                                |> List.map(fun field -> loop field.PropertyType)


                                            FsSchemaComplexType_SingleCaseUnion_MultipleFields.Create(ucase, tp, fields, fieldSchemaTypes)
                                            |> FsSchemaComplexType_SingleCaseUnion.MultipleFields
                                            |> FsSchemaComplexType.SinglecaseUnion
                                            |> FsSchemaType.ComplexType

                                    | FsObjectTypeCode.Union cases ->

                                        let elements = 
                                            cases
                                            |> List.ofArray
                                            |> List.map(fun case ->
                                                let fields = case.GetFields()
                                                let caseName = case.Name
                                                let tpName = tp.Name + "_" + case.Name.ToLower() + "Case"
                                                for field in fields do
                                                    configuration.AddTypeMappingByType field.PropertyType

                                                match fields with 
                                                | [||] -> 
                                                    FsSchemaComplexType_Union_NamedCase.Create(case)
                                                    |> FsSchemaComplexType_UnionCase.NamedCase
                                                | [|field|] -> 
                                                    let fieldSchemaType = loop field.PropertyType
                                                    FsSchemaComplexType_Union_OneFieldCase.Create(
                                                        case,
                                                        field,
                                                        fieldSchemaType
                                                    )
                                                    |> FsSchemaComplexType_UnionCase.OneFieldCase

                                                | fields ->
                                                    let fields = List.ofSeq fields

                                                    let fieldSchemaTypes = 
                                                        fields
                                                        |> List.map(fun field -> loop field.PropertyType)
                                        
                                                    FsSchemaComplexType_Union_MultipleFieldsCase.Create(
                                                        case,
                                                        fields,
                                                        fieldSchemaTypes
                                                    )
                                                    |> FsSchemaComplexType_UnionCase.MultipleFieldsCase

                                            )

                                        { UnionCases = elements 
                                          Type = tp
                                          TagOptions = configuration.UnionTagOptions }
                                        |> FsSchemaComplexType.Union
                                        |> FsSchemaType.ComplexType

                        typeElements.Add(tp, r)
                
                        r

                    | Some listRestriction ->
                        let targetType = listRestriction.TargetType
                        let r2 = r targetType
                        let r2 = 
                            match r2.GetAsCollection() with 
                            | Some collectionType ->
                                { collectionType with ListRestriction = Some listRestriction.Restriction }
                                |> FsSchemaComplexGenericType.Collection
                                |> FsSchemaComplexType.Generic
                                |> FsSchemaType.ComplexType

                            | None -> failwithf "Invalid token, schema type %A should be collection type here" r2

                        typeElements.Add(tp, r2)
                        r2

                let configuration = configuration
                configuration.AddTypeMappingByType(tp)
                let tpMapping = configuration.TryGetTypeMapping(tp)
                match tpMapping with 
                | None ->
                    let tpCode = getFsTpCodeEx tp
                    r(tp)
                | Some tpMapping -> 
                    let schemaType = loop(tpMapping.TargetType)
                    let schemaType =
                        { 
                            TypeMappingPair = tpMapping
                            FsSchemaType = schemaType
                        }
                        |> FsSchemaType.MappedType

                    typeElements.Add(tp, schemaType)
                    schemaType


            | Some v -> v.XmlSchemaType

        loop1 [tp] tp
        |> ignore

        //let __update_recusive_types =
        //    typeElements.UpdateRecusiveTypes()

        let __updateConfiguration_schemaTypesCache =
            let cache = configuration.FsSchemaTypeCache
            cache.Clear()
            for pair in typeElements.Pairs do
                cache.TryAdd(pair.Key, pair.Value.XmlSchemaType)
                |> ignore

        let rootElement = 
            let tp = configuration.GetFsXmlSchemaType(tp)
            XmlSchemaElement(
                Name = tp.GetElementName(),
                SchemaTypeName = new XmlQualifiedName(tp.GetElementName())
            )

        //let entryType =
        //    XmlSchemaComplexType(
        //        Name = "Entry"
        //    )

        let scaseType =
            XmlSchemaComplexType(
                Name = "SCase"
            )

        let typeElements = 
            typeElements.Values
            |> List.ofSeq
            |> List.sortBy(fun m -> m.Index)
            |> List.collect(fun m -> 
                match m.XmlSchemaType with 
                | FsSchemaType.Option _ -> []
                | _ ->
                    match m.XmlSchemaType.GetAsGeneric() with 
                    | Some _ -> []
                    | _ ->
                        let r = m.XmlSchemaType.ToSchemas()
                        r
            )
            |> List.distinctBy(fun m -> m.Name)
            |> List.map(fun m -> m :> XmlSchemaObject)
        

        rootElement :: typeElements @ [(*entryType; *)scaseType]

    member x.ImportTp(tp: Type) =
        let xmlSchema = XmlSchema(ElementFormDefault = XmlSchemaForm.Qualified)
        let elements = x.TpToXmlSchemaObject(tp)
        for element in elements do
            xmlSchema.Items.Add(element)
            |> ignore

        xmlSchema
