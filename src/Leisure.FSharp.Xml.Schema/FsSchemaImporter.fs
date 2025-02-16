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



type private IndexedXmlSchemaType =
    { Index: int 
      XmlSchemaType: XmlSchemaType }

type private TypeElementsCache() =
    let tpNames = HashSet()
    let cache = ConcurrentDictionary<Type, IndexedXmlSchemaType option>()
    //let unionCasesCache = ConcurrentDictionary<Type * string, XmlSchemaComplexType>()

    member x.TryGet(tp: Type) =
        match cache.TryGetValue(tp) with 
        | true, v -> v
        | false, _ -> None

    member x.GetOrAdd(tp, valueFactory) =
        let index = cache.Count
        cache.GetOrAdd(tp, valueFactory = fun tp ->
            let tpName_titleCase = tp.GetXmlQualifiedName().Name |> toTitleCase
            match tpNames.Contains tpName_titleCase with 
            | true -> failwithf "Duplicate type Name %s is not supported" tpName_titleCase
            | false -> 
                tpNames.Add(tpName_titleCase)
                |> ignore
            { Index = index 
              XmlSchemaType = valueFactory tp }
            |> Some
        )
        |> Option.get

    member x.GetOrAddOp(tp, valueFactory) =
        let index = cache.Count
        cache.GetOrAdd(tp, valueFactory = fun tp ->
            let tpName_titleCase = tp.GetXmlQualifiedName().Name |> toTitleCase
            match tpNames.Contains tpName_titleCase with 
            | true -> failwithf "Duplicate type Name %s is not supported" tpName_titleCase
            | false -> 
                tpNames.Add(tpName_titleCase)
                |> ignore

            let xmlSchemaType = valueFactory tp
            match xmlSchemaType with 
            | None -> None
            | Some xmlSchemaType ->
                { Index = index 
                  XmlSchemaType = xmlSchemaType }
                |> Some
        )

    //member x.GetOrAddUci(uci: UnionCaseInfo, valueFactory) =
    //    unionCasesCache.GetOrAdd((uci.DeclaringType, uci.Name), valueFactory = valueFactory)

    member x.Count = cache.Count

    member x.ContainsKey(k) = cache.ContainsKey k

    member x.Values = cache.Values

    member x.Keys = cache.Keys



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


module rec FsSchemaTypes =
    [<RequireQualifiedAccess>]
    type FsSchemaTypeOrSchemaTypeName =
        | SchemaType of FsSchemaType
        | SchemaTypeName of XmlQualifiedName

    type FsXmlSchemaElement =
        { IsOption: bool 
          Name: string 
          SchemaTypeOrSchemaTypeName: FsSchemaTypeOrSchemaTypeName
          }
    with 
        member x.ToSchema() =
            let name = x.Name
            let element = 
                match x.IsOption with 
                | true ->
                    XmlSchemaElement(
                        Name = name,
                        IsNillable = true
                    )

                | _ ->
                    XmlSchemaElement(
                        Name = name
                    )

            match x.SchemaTypeOrSchemaTypeName with 
            | FsSchemaTypeOrSchemaTypeName.SchemaTypeName tpName -> 
                element.SchemaTypeName <- tpName
                element
            | FsSchemaTypeOrSchemaTypeName.SchemaType tp ->
                element.SchemaType <- tp.ToSchema()
                element


    type FsSchemaComplexType_Tuple =
        { Elements: FsXmlSchemaElement list
          TypeCode: array<FsTypeCodeEx * Type>
        }
    with 
        member x.Name =
            "Tuple" + x.Elements.Length.ToString()
 
        member x.ToSchema() =
            let tuplePropSequence =
                let propSequence = 
                    XmlSchemaSequence()

                for element in x.Elements do
                    let element = element.ToSchema()
                    propSequence.Items.Add(element)
                    |> ignore
                
                propSequence


            let innerType = 
                XmlSchemaComplexType(
                    Particle = tuplePropSequence
                )
            
            innerType

            //let element = 
            //    let name = x.Name
            //    match enumValue with 
            //    | WrapOldNameEnum.WrapOldName ->
            //        XmlSchemaElement(
            //            Name = name,
            //            SchemaType = innerType
            //        )

            //    | WrapOldNameEnum.UsingCurrentName ->
            //        XmlSchemaElement.create_isOption isOption name innerType 

            //let propSequence = 
            //    let sequence = XmlSchemaSequence()
            //    sequence.Items.Add(element) |> ignore
            //    sequence

            //XmlSchemaComplexType(
            //    Name = tp.GetXmlQualifiedName().Name,
            //    Particle = propSequence
            //)

    type FsSchemaComplexType_Entry =
        { KeyElement: XmlSchemaElement
          ValueElement: XmlSchemaElement
          TypeCode: DictionaryType
        }
    with 
        member x.EntryName = x.TypeCode.EntryTypeName
    
    type FsSchemaComplexType_Dictionary =
        { Entry: FsSchemaComplexType_Entry
          TypeCode: DictionaryType
        }
    with
        member x.TypeName = x.TypeCode.TypeName



    type FsSchemaComplexType_Collection =
        { Entry: FsSchemaComplexType_Entry
          TypeCode: CollectionType
          ElementTypeOrTypeName: FsSchemaTypeOrSchemaTypeName
        }
    with
        member x.TypeName = x.TypeCode.TypeName
            

    [<RequireQualifiedAccess>]
    type FsSchemaComplexType =
        | Tuple of FsSchemaComplexType_Tuple
        | Dictionary of DictionaryType
        | Collection of CollectionType
        | Union of UnionCaseInfo list
        | SinglecaseUnion of UnionCaseInfo
    


    [<RequireQualifiedAccess>]
    type FsSchemaSimpleType =
        | EnumType of Type
        | ValueType of Type

    [<RequireQualifiedAccess>]
    type FsSchemaType =
        | FsSchemaComplexType of FsSchemaComplexType
        | Option of FsSchemaType
        | SimpleType of FsSchemaSimpleType
        | WrapOldType of oldType: Type * FsSchemaType
    with 
        member x.ToSchema() =
            failwith ""

type FsSchemaImporter(configuration: FsXmlSerializerConfiguration) =
    
    member private x.TpToXmlSchemaObject(tp: Type): XmlSchemaObject list =
        let createXmlSchemaSequence(props: SCasablePropertyType []) =
            let propSequence = 
                XmlSchemaSequence()

            for prop in props do
                let element = prop.GenerateElement()
                propSequence.Items.Add(element)
                |> ignore
            
            propSequence

        let createXmlSchemaSequence_WithTypeMapping(props: SCasablePropertyTypeWithTypeMapping []) =
            let propSequence = 
                XmlSchemaSequence()

            for prop in props do
                let element = prop.GenerateElement()
                propSequence.Items.Add(element)
                |> ignore
            
            propSequence


        let typeElements = TypeElementsCache()
        let rec loop isOption (tp0: Type) =
            configuration.AddTypeMappingByType(tp0)
            let configuration = configuration
            let tpMapping = configuration.TryGetTypeMapping tp0
            match tpMapping with 
            | None -> ()
            | Some tpMapping ->
                configuration.AddTypeMappingByType(tpMapping.TypeMapping.TargetType)


            let tryWrapOldName_TypeMapping(f) =
                match tpMapping with 
                | None -> f WrapOldNameEnum.UsingCurrentName
                | Some tpMapping ->
                    match tpMapping.TypeMapping.WrapOldName with 
                    | false -> f WrapOldNameEnum.UsingCurrentName
                    | true ->
                        let schemaType: XmlSchemaComplexType = f WrapOldNameEnum.WrapOldName
                        schemaType.Name <- null
                        let element = 
                            XmlSchemaElement.create_isOption isOption tpMapping.OriginType.Name schemaType

                        let propSequence = 
                            let sequence = XmlSchemaSequence()
                            sequence.Items.Add(element) |> ignore
                            sequence

                        XmlSchemaComplexType(
                            Name = tpMapping.OriginType.Name,
                            Particle = propSequence
                        )

            let tp0 = 
                configuration.UpdateType_ToXml_Ex__NoUpdateForWrappedTypeName(tp0)
                |> fst

            let tp = 
                match tpMapping with 
                | None -> tp0

                | Some tpMapping ->
                    tpMapping.TypeMapping.TargetType

            let tpCode = getFsTpCodeEx (tp)
            let getTuplePropSequence(tpCodes: array<FsTypeCodeEx * Type>) =
                let propTypes = 
                    tpCodes
                    |> Array.mapi(fun i (_, tp) -> 
                        SCasablePropertyType.CreateNamedType(itemText i, tp)
                    )
                createXmlSchemaSequence propTypes

            match tpCode with 

            | FsTypeCodeEx.Tuple (tpCodes) ->
                let r = 
                    typeElements.GetOrAdd(tp0, valueFactory = fun _ -> 
                        tryWrapOldName_TypeMapping(fun enumValue ->
                            let innerType = 
                                let propSequence = getTuplePropSequence(tpCodes)

                                XmlSchemaComplexType(
                                    Particle = propSequence
                                )
                        
                            let element = 
                                let name = "Tuple" + tpCodes.Length.ToString()
                                match enumValue with 
                                | WrapOldNameEnum.WrapOldName ->
                                    XmlSchemaElement(
                                        Name = name,
                                        SchemaType = innerType
                                    )

                                | WrapOldNameEnum.UsingCurrentName ->
                                    XmlSchemaElement.create_isOption isOption name innerType 

                            let propSequence = 
                                let sequence = XmlSchemaSequence()
                                sequence.Items.Add(element) |> ignore
                                sequence

                            XmlSchemaComplexType(
                                Name = tp.GetXmlQualifiedName().Name,
                                Particle = propSequence
                            )
                        )

                    )


                for (tpCode, tp) in tpCodes do
                    loop false tp
                    |> ignore
                r

            | FsTypeCodeEx.Option (elementTpCode, elementTp) ->
                loop true elementTp

            | FsTypeCodeEx.DictionaryType dictionaryType ->
                let keyType = dictionaryType.KeyType
                let valueType = dictionaryType.ValueType

                let arrayTpName = dictionaryType.TypeName
                let entryTpName = dictionaryType.EntryTypeName
                typeElements.GetOrAdd(tp, valueFactory = fun _ ->
                    let propSequence = 
                        XmlSchemaSequence()

                    propSequence.Items.Add(
                        XmlSchemaElement(
                            MinOccurs = 0,
                            MaxOccursString = "unbounded",
                            Name = "Entry",
                            SchemaTypeName = XmlQualifiedName(entryTpName)
                        )
                    )   
                    |> ignore

                    XmlSchemaComplexType(
                        Name = arrayTpName,
                        Particle = propSequence
                    )

                )
                |> ignore

                let entryType = Entry.makeEntryGenericType dictionaryType
                typeElements.GetOrAdd(entryType, fun _ ->
                    let propSequence = 
                        createXmlSchemaSequence     
                            [|SCasablePropertyType.CreateNamedType ("Key", keyType);
                              SCasablePropertyType.CreateNamedType ("Value", valueType) |]

                    let content = 
                        XmlSchemaComplexContent(
                            Content = 
                                XmlSchemaComplexContentExtension(
                                    BaseTypeName = XmlQualifiedName("Entry"),
                                    Particle = propSequence
                                )
                        )

                    XmlSchemaComplexType(
                        Name = entryTpName,
                        ContentModel = content
                    )
                )
                |> ignore

                loop false keyType
                loop false valueType
                
            | FsTypeCodeEx.CollectionType collectionType -> 
                let elementType = collectionType.ElementType


                typeElements.GetOrAdd(tp, valueFactory = fun _ ->
                    let arrayTpName = collectionType.TypeName
                    let propSequence = 
                        XmlSchemaSequence()

                    let elementTypeName = 


                        match getFsTpCodeEx elementType with 
                        | FsTypeCodeEx.Tuple tpCodes ->
                            let propSequence = getTuplePropSequence(tpCodes)
                                
                            XmlSchemaComplexType(
                                Particle = propSequence
                            )
                            |> Choice1Of2

                        | _ -> Choice2Of2 (elementType.GetXmlQualifiedName())

                    let element = 

                        match elementTypeName with 
                        | Choice1Of2 tupleType ->
                            XmlSchemaElement(
                                MinOccurs = 0,
                                MaxOccursString = "unbounded",
                                Name = elementType.GetElementName(),
                                SchemaType = tupleType
                            )
                            
                        | Choice2Of2 elementTypeName -> 
                            XmlSchemaElement(
                                MinOccurs = 0,
                                MaxOccursString = "unbounded",
                                Name = elementType.GetElementName(),
                                SchemaTypeName = elementTypeName
                            )



                    propSequence.Items.Add(
                        element
                    )   
                    |> ignore

                    XmlSchemaComplexType(
                        Name = arrayTpName,
                        Particle = propSequence
                    )

                )
                |> ignore

                loop false elementType

            | FsTypeCodeEx.FsTypeCode tpCode ->

                match tpCode with 
                | FsTypeCode.Enum ->
                    let enumTp = 
                        typeElements.GetOrAdd(tp, valueFactory = fun _ -> 
                            let content = 
                                XmlSchemaSimpleTypeRestriction(
                                    BaseTypeName = new XmlQualifiedName("string", W3XMLSchema)
                                )

                            for enumName in System.Enum.GetNames(tp) do
                                content.Facets.Add(
                                    XmlSchemaEnumerationFacet(
                                        Value = enumName
                                    )
                                )
                                |> ignore

                            XmlSchemaSimpleType(
                                Name = tp.Name,
                                Content = content
                            )
                        )

                    match tpMapping with 
                    | None -> ()
                    | Some tpMapping -> 
                        match tpMapping.TypeMapping.WrapOldName with 
                        | true -> 
                            let content = 
                                XmlSchemaSimpleTypeRestriction(
                                    BaseTypeName = new XmlQualifiedName(enumTp.XmlSchemaType.Name)
                                )
                           
                            typeElements.GetOrAdd(tp0, valueFactory = fun _ ->
                                XmlSchemaSimpleType(
                                    Name = tpMapping.OriginType.GetXmlQualifiedName().Name,
                                    Content = content
                                )
                            )
                            |> ignore


                        | false -> ()

                    ()

                
                | FsTypeCode.ValueType tpCode -> 
                    match tpMapping with 
                    | None -> 
                        typeElements.GetOrAddOp(tp0, valueFactory = fun _ -> None)
                        |> ignore

                    | Some tpMapping -> 
                        match tpMapping.TypeMapping.WrapOldName with 
                        | true -> 
                           
                            typeElements.GetOrAdd(tp0, valueFactory = fun _ ->
                                let content = 
                                    XmlSchemaSimpleTypeRestriction(
                                        BaseTypeName = tp.GetXmlQualifiedName()
                                    )
                                XmlSchemaSimpleType(
                                    Name = tpMapping.OriginType.GetXmlQualifiedName().Name,
                                    Content = content
                                )
                            )
                            |> ignore


                        | false -> ()

                | FsTypeCode.Object fsObjectTypeCode ->   

                    
                    match fsObjectTypeCode with 
                    | FsObjectTypeCode.FsXmlSerializableTypeMapping ->
                        match tpMapping with 
                        | None -> 
                            failwithf "Invalid token, using FsXmlSerializableTypeMapping instead"
                            ()


                        | Some tpMapping ->
                            let rec loop2 (tpMapping: FsXmlSerializerTypeMappingPair) tp = 
                                loop false tp
                                let element = typeElements.TryGet(tp)
                                match element with 
                                | None -> 
                                    let targetType = tpMapping.TypeMapping.TargetType
                                    match configuration.TryGetTypeMapping tpMapping.TypeMapping.TargetType with 
                                    | None -> tpMapping.TypeMapping.TargetType
                                    | Some tpMapping2 ->
                                        loop2 tpMapping2 targetType
                                | Some _ -> failwithf ""

                            let finalTargetType = loop2 tpMapping tp
                            let r = 
                                typeElements.GetOrAdd(tp0, valueFactory = fun _ ->
                                    let content = 
                                        XmlSchemaSimpleTypeRestriction(
                                            BaseTypeName = tp.GetXmlQualifiedName()
                                        )

                                    XmlSchemaSimpleType(
                                        Name = finalTargetType.GetXmlQualifiedName().Name,
                                        Content = content
                                    )
                                )

                            ()


                    | FsObjectTypeCode.Record ->

                        let props = 
                            FSharpType.GetRecordFields tp
                            |> Array.map(fun prop ->
                                let propTp = prop.PropertyType |> configuration.UpdateType_ToXml_Ex 
                                SCasablePropertyType.CreateNamedType(prop.Name, propTp)
                            )

                        typeElements.GetOrAdd(tp, valueFactory = fun _ ->
                            let propSequence = createXmlSchemaSequence props

                            XmlSchemaComplexType(
                                Name = tp.Name,
                                Particle = propSequence
                            )

                        )
                        |> ignore

                        for prop in props do 
                            let propTp = prop.PropertyType
                            match typeElements.ContainsKey propTp with 
                            | true -> ()
                            | false -> loop false propTp

                    | FsObjectTypeCode.SingletonCaseUnion ucase ->
                        let fields = ucase.GetFields()



                        typeElements.GetOrAdd(tp, valueFactory = fun _ ->

                            match fields with 
                            | [||] -> failwithf "Not implemented"
                            | [|field|] ->  
                                let propSequence = createXmlSchemaSequence [|SCasablePropertyType.OneFieldSCase field|]
                                XmlSchemaComplexType(
                                    Name = tp.Name,
                                    Particle = propSequence
                                )

                            | fields -> 
                                let innerType = 
                                    let propSequence = 
                                        let propTypes = 
                                            fields
                                            |> Array.map(fun m -> 
                                                SCasablePropertyType.PropertyInfo m
                                            )
                                        createXmlSchemaSequence propTypes

                                    XmlSchemaComplexType(
                                        Particle = propSequence
                                    )


                                let element = 
                                    XmlSchemaElement(
                                        Name = "SCase",
                                        SchemaType = innerType
                                    )

                                let propSequence = 
                                    let sequence = XmlSchemaSequence()
                                    sequence.Items.Add(element) |> ignore
                                    sequence

                                XmlSchemaComplexType(
                                    Name = tp.Name,
                                    Particle = propSequence
                                )

                        )
                        |> ignore

                        for field in fields do 
                            let propTp = field.PropertyType
                            match typeElements.ContainsKey propTp with 
                            | true -> ()
                            | false -> loop false propTp


                    | FsObjectTypeCode.Union cases ->
                        typeElements.GetOrAdd(tp, valueFactory = fun _ ->   

                            let propChoice = 
                                let choice = XmlSchemaChoice()
                                let elements = 
                                    cases
                                    |> Array.map(fun case ->
                                        let innerType = 
                                            let fields = case.GetFields()
                                            let caseName = case.Name
                                            let tpName = tp.Name + "_" + case.Name.ToLower() + "Case"
                                            for field in fields do
                                                configuration.AddTypeMappingByType field.PropertyType

                                            match fields with 
                                            | [||] -> 
                                                XmlQualifiedName("string", W3XMLSchema)
                                                |> Choice1Of3
                                            | [|field|] -> 
                                                let propType =
                                                    SCasablePropertyTypeWithTypeMapping.CreateNamedType(
                                                        case.Name,
                                                        field.PropertyType,
                                                        configuration
                                                    )

                                                (propType)
                                                |> Choice2Of3

                                            | fields ->
                                                let propSequence = 
                                                    let propTypes = 
                                                        fields
                                                        |> Array.map(fun m -> 
                                                            SCasablePropertyTypeWithTypeMapping.CreateNamedType(
                                                                m.Name,
                                                                m.PropertyType,
                                                                configuration
                                                            )
                                                        )
                                                    createXmlSchemaSequence_WithTypeMapping propTypes
                                            
                                                XmlSchemaComplexType(
                                                    Particle = propSequence
                                                )
                                                |> Choice3Of3

                                        let tpName = tp.Name + "_" + case.Name.ToLower() + "Case"
                                        match innerType with 
                                        | Choice1Of3 name ->
                                            XmlSchemaElement(
                                                Name = case.Name,
                                                SchemaTypeName = name
                                            )

                                        | Choice2Of3 (propInfo) ->
                                            let element = propInfo.GenerateElement()
                                            element

                                        | Choice3Of3 innerType ->
                                            XmlSchemaElement(
                                                Name = case.Name,
                                                SchemaType = innerType
                                            )
                                    )

                                for element in elements do
                                    choice.Items.Add(element) |> ignore

                                choice

                            let union = 
                                let sequence = XmlSchemaSequence()
                                let element = XmlSchemaElement(
                                    Name = "Choice" + cases.Length.ToString(),
                                    SchemaType = XmlSchemaComplexType(Particle = propChoice)
                                )
                                sequence.Items.Add element |> ignore
                                sequence

                            XmlSchemaComplexType(
                                Name = tp.Name,
                                Particle = union
                            )
                        )
                        |> ignore


                        for case in cases do 
                            let fields = case.GetFields()
                            for field in fields do 
                                let propTp = field.PropertyType
                                match typeElements.ContainsKey propTp with 
                                | true -> ()
                                | false -> loop false propTp



        loop false tp
        |> ignore

        let rootElement = 
            
            XmlSchemaElement(
                Name = tp.Name,
                SchemaTypeName = new XmlQualifiedName(tp.Name)
            )

        let entryType =
            XmlSchemaComplexType(
                Name = "Entry"
            )

        let scaseType =
            XmlSchemaComplexType(
                Name = "SCase"
            )

        let typeElements = 
            typeElements.Values
            |> List.ofSeq
            |> List.choose id
            |> List.sortBy(fun m -> m.Index)
            |> List.map(fun m -> m.XmlSchemaType :> XmlSchemaObject)

        rootElement :: typeElements @ [entryType; scaseType]

    member x.ImportTp(tp: Type) =
        let xmlSchema = XmlSchema(ElementFormDefault = XmlSchemaForm.Qualified)
        let elements = x.TpToXmlSchemaObject(tp)
        for element in elements do
            xmlSchema.Items.Add(element)
            |> ignore

        xmlSchema
