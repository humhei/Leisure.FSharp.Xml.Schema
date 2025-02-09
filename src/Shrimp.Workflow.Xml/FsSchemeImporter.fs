// Learn more about F# at http://fsharp.org
namespace Shrimp.Workflow.Xml
#nowarn "0104"
#nowarn "3535"
#nowarn "3536"
open System.Linq.Expressions

open System.Runtime.Serialization
open System
open System.Globalization

open System.Collections
open System.Collections.Generic

open System.Reflection

open MBrace.FsPickler
open System.Xml.Serialization
open System
open System.IO
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Xml
open System.Xml.Schema
open Fake.IO


type private IndexedXmlSchemaType =
    { Index: int 
      XmlSchemaType: XmlSchemaType }

type private TypeElementsCache() =
    let tpNames = HashSet()
    let cache = ConcurrentDictionary<Type, IndexedXmlSchemaType>()
    let unionCasesCache = ConcurrentDictionary<Type * string, XmlSchemaComplexType>()

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
        )

    member x.GetOrAddUci(uci: UnionCaseInfo, valueFactory) =
        unionCasesCache.GetOrAdd((uci.DeclaringType, uci.Name), valueFactory = valueFactory)

    member x.Count = cache.Count

    member x.ContainsKey(k) = cache.ContainsKey k

    member x.Values = cache.Values

    member x.Keys = cache.Keys

type Entry<'Key, 'Value> =
    { Key: 'Key 
      Value: 'Value }


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


type FsSchemeImporter(configuration: FsXmlSerializerConfiguration) =
    
    member private x.TpToXmlSchemaObject(tp: Type): XmlSchemaObject list =
        let createXmlSchemaSequence(props: SCasablePropertyType []) =
            let propSequence = 
                XmlSchemaSequence()

            for prop in props do
                let element = prop.GenerateElement()
                propSequence.Items.Add(element)
                |> ignore
            
            propSequence


        let typeElements = TypeElementsCache()
        let rec loop isOption (tp: Type) =
            let tp = configuration.UpdateType_ToXml_Ex(tp) 

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
                typeElements.GetOrAdd(tp, valueFactory = fun _ -> 
                    let innerType = 
                        let propSequence = getTuplePropSequence(tpCodes)

                        XmlSchemaComplexType(
                            Particle = propSequence
                        )
                        
                    
                    let element = 
                        XmlSchemaElement(
                            Name = "Tuple" + tpCodes.Length.ToString(),
                            SchemaType = innerType
                        )

                    let propSequence = 
                        let sequence = XmlSchemaSequence()
                        sequence.Items.Add(element) |> ignore
                        sequence

                    XmlSchemaComplexType(
                        Name = tp.GetXmlQualifiedName().Name,
                        Particle = propSequence
                    )
                )
                |> ignore

                for (tpCode, tp) in tpCodes do
                    loop false tp

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
                    |> ignore

                
                | FsTypeCode.ValueType tpCode -> ()

                | FsTypeCode.Object fsObjectTypeCode ->   

                    
                    match fsObjectTypeCode with 
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
                                            let tpName = tp.Name + "_" + case.Name.ToLower() + "Case"
                                            match fields with 
                                            | [||] -> 
                                                XmlQualifiedName("string", W3XMLSchema)
                                                |> Choice1Of3
                                            | [|field|] -> 
                                                let propType =
                                                    SCasablePropertyType.CreateNamedType(
                                                        case.Name,
                                                        configuration.UpdateType_ToXml_Ex field.PropertyType
                                                    )
                                                
                                                propType
                                                |> Choice2Of3

                                            | fields ->
                                                let propSequence = 
                                                    let propTypes = 
                                                        fields
                                                        |> Array.map(fun m -> 
                                                            SCasablePropertyType.PropertyInfo m
                                                        )
                                                        |> Array.map configuration.UpdateSCasablePropertyType_ToXml
                                                    createXmlSchemaSequence propTypes
                                            
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

                                        | Choice2Of3 propInfo ->
                                            propInfo.GenerateElement()

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


                    | _ -> failwith "Not implemented"

        loop false tp

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
