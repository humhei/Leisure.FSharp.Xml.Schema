// Learn more about F# at http://fsharp.org
namespace Shrimp.Workflow.Xml
#nowarn "0104"
open System.Runtime.Serialization

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

type FsIXmlSerializable<'T> =
    abstract member ReadXml: XmlReader -> obj
    abstract member WriteXml: XmlWriter -> unit

type FsIXmlSerializable =
    abstract member ReadXml: XmlReader -> obj

    abstract member WriteXml: XmlWriter -> unit



[<AutoOpen>]
module private _Utils =
    
    let [<Literal>] W3XMLSchema = "http://www.w3.org/2001/XMLSchema"

    let private textInfo = (new CultureInfo("en-US", false)).TextInfo
    let toTitleCase (text: string) = textInfo.ToTitleCase(text)

    [<RequireQualifiedAccess>]
    type FsObjectTypeCode =
        | Record 
        | Union
        | SingletonCaseUnion of UnionCaseInfo

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
                    | true -> FsTypeCode.Object(FsObjectTypeCode.Union)
                    | false -> 
                        match FSharpType.IsUnion(tp, true) with 
                        | true ->
                            match FSharpType.GetUnionCases(tp, true) with 
                            | [|case|] ->
                                FsTypeCode.Object(FsObjectTypeCode.SingletonCaseUnion case)

                            | _ -> failwithf "[Xml serializable] Un supported type %A" tp

                        | false -> failwithf "[Xml serializable] Un supported type %A" tp
                
            | _ -> 
                match tp.IsEnum with
                | true -> FsTypeCode.Enum 
                | false -> FsTypeCode.ValueType tpCode
        )

    [<RequireQualifiedAccess>]
    type CollectionType =
        | FSharpList of elementTp: Type
        | Array of elementTp: Type
        | List of elementTp: Type
        | Seq of elementTp: Type
    with 
        member x.ElementType =
            match x with 
            | FSharpList  tp -> tp
            | Array       tp -> tp
            | List        tp -> tp
            | Seq         tp -> tp

    let private collectionTypeNames1 = 
        ["FSharpList`1", CollectionType.FSharpList
         "IEnumerable`1", CollectionType.Seq
         "List`1", CollectionType.List]


    [<RequireQualifiedAccess>]
    type FsTypeCodeEx =
        | CollectionType of CollectionType
        | FsTypeCode of FsTypeCode


    let private typeCodeExCache = ConcurrentDictionary()

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


    let getFsTpCodeEx (tp: Type) =
        typeCodeExCache.GetOrAdd(tp, valueFactory = fun _ ->
            match getCollectionType tp with 
            | Result.Error _ ->
                getFsTpCode tp
                |> FsTypeCodeEx.FsTypeCode
            | Result.Ok collectionType ->
                collectionType
                |> FsTypeCodeEx.CollectionType
        )




    type Type with 
        member tp.GetXmlQualifiedName() =
            match getFsTpCodeEx tp with 
            | FsTypeCodeEx.CollectionType collectionType -> 
                XmlQualifiedName("ArrayOf" + collectionType.ElementType.Name)

            | FsTypeCodeEx.FsTypeCode fsTypeCode ->
                match fsTypeCode with
                | FsTypeCode.Object _
                | FsTypeCode.Enum -> XmlQualifiedName(tp.Name)
                | FsTypeCode.ValueType tpCode ->
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

                    XmlQualifiedName(tpName, W3XMLSchema)
    
    [<RequireQualifiedAccess>]
    type SCasablePropertyType =
        | OneFieldSCase of PropertyInfo
        | PropertyInfo of PropertyInfo
        | Type of Type
    with 
        member x.Name =
            match x with 
            | OneFieldSCase v -> "SCase"
            | PropertyInfo v -> v.Name
            | Type v -> v.Name
    
        //member private x.Value =
        //    match x with 
        //    | OneFieldSCase v 
        //    | PropertyInfo v -> v
    
        member x.PropertyType = 
            match x with 
            | OneFieldSCase v 
            | PropertyInfo v -> v.PropertyType
            | Type tp -> tp
            
            

    type SCasablePropertyType with 
        member x.GenerateElement() =
            let name = x.Name
            let propTP = x.PropertyType
            XmlSchemaElement(
                Name = name,
                SchemaTypeName = propTP.GetXmlQualifiedName()
            )

type private IndexedXmlSchemaType =
    { Index: int 
      XmlSchemaType: XmlSchemaType }

type private TypeElementsCache() =
    let tpNames = HashSet()
    let cache = ConcurrentDictionary<Type, IndexedXmlSchemaType>()

    member x.GetOrAdd(tp, valueFactory) =
        let index = cache.Count
        cache.GetOrAdd(tp, valueFactory = fun tp ->
            let tpName_titleCase = toTitleCase tp.Name
            match tpNames.Contains tpName_titleCase with 
            | true -> failwithf "Duplicate type Name %s is not supported" tpName_titleCase
            | false -> 
                tpNames.Add(tpName_titleCase)
                |> ignore
            { Index = index 
              XmlSchemaType = valueFactory tp }
        )

    member x.Count = cache.Count

    member x.ContainsKey(k) = cache.ContainsKey k

    member x.Values = cache.Values

    member x.Keys = cache.Keys

type FsSchemeImporter() =
    
    member private x.TpToXmlSchemaObject(tp: Type): XmlSchemaObject list =
        let contentElements = ResizeArray<XmlSchemaObject>()
        let typeElements = TypeElementsCache()
        let rec loop (tp: Type) =
            let tpCode = getFsTpCodeEx (tp)
            match tpCode with 
            | FsTypeCodeEx.CollectionType collectionType -> 
                let elementType = collectionType.ElementType
                typeElements.GetOrAdd(tp, valueFactory = fun _ ->
                    let arrayTpName = "ArrayOf" + elementType.Name
                    let propSequence = 
                        XmlSchemaSequence()

                    propSequence.Items.Add(
                        XmlSchemaElement(
                            MinOccurs = 0,
                            MaxOccursString = "unbounded",
                            Name = elementType.Name,
                            SchemaTypeName = XmlQualifiedName(elementType.Name)
                        )
                    )   
                    |> ignore

                    XmlSchemaComplexType(
                        Name = arrayTpName,
                        Particle = propSequence
                    )

                )
                |> ignore

                loop elementType

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
                    let createXmlSchemaSequence(props: SCasablePropertyType []) =
                        let propSequence = 
                            XmlSchemaSequence()

                        for prop in props do
                            let element = prop.GenerateElement()
                            propSequence.Items.Add(element)
                            |> ignore
                        
                        propSequence
                    
                    match fsObjectTypeCode with 
                    | FsObjectTypeCode.Record ->

                        let props = 
                            FSharpType.GetRecordFields tp
                            |> Array.map SCasablePropertyType.PropertyInfo

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
                            | false -> loop propTp

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

                            | fields -> failwithf "Not implemented"
                        )
                        |> ignore

                        for field in fields do 
                            let propTp = field.PropertyType
                            match typeElements.ContainsKey propTp with 
                            | true -> ()
                            | false -> loop propTp

                    | _ -> failwith "Not implemented"

        loop tp

        let rootElement = 
            
            XmlSchemaElement(
                Name = tp.Name,
                SchemaTypeName = new XmlQualifiedName(tp.Name)
            )


        let typeElements = 
            typeElements.Values
            |> List.ofSeq
            |> List.sortBy(fun m -> m.Index)
            |> List.map(fun m -> m.XmlSchemaType :> XmlSchemaObject)

        rootElement :: typeElements

    member x.ImportTp(tp: Type) =
        let xmlSchema = XmlSchema(ElementFormDefault = XmlSchemaForm.Qualified)
        let elements = x.TpToXmlSchemaObject(tp)
        for element in elements do
            xmlSchema.Items.Add(element)
            |> ignore

        xmlSchema


[<AutoOpen>]
module private _FsXmlSerializerUtils =
    let makeSelfFsXmlSerializerCache =
        ConcurrentDictionary()

type FsXmlSerializer<'T>() =
    let encoding = System.Text.Encoding.UTF8
    let tp = typeof<'T>
    let __CheckTypeValid =
        match FSharpType.IsRecord tp with 
        | true -> ()
        | false -> failwithf "Root type should be fsharp record"

    let props = FSharpType.GetRecordFields tp


    member private x.File_WriteCsXsd(xsdPath: string) =
        use tw = new StreamWriter(xsdPath)
        let importer = new XmlReflectionImporter();
        let schemas = new XmlSchemas();
        let exporter = new XmlSchemaExporter(schemas);
        let props = tp.GetProperties()
        let tp = 
            props
            |> Array.tryFind(fun m -> m.Name = "XMLScheme")
            |> function
                | None -> tp

                | Some scheme ->
                    scheme.PropertyType
        
        
        let map = importer.ImportTypeMapping(tp);
        exporter.ExportTypeMapping(map);
        let schema = schemas[0]
        let items = schema.Items
        schema.Write(tw);

    member private x.File_WriteFsXsd(xsdPath: string) =
        use tw = new StreamWriter(xsdPath)
        let importer = new FsSchemeImporter();
        let props = tp.GetProperties()
        let tp = 
            props
            |> Array.tryFind(fun m -> m.Name = "XMLScheme")
            |> function
                | None -> tp

                | Some scheme ->
                    scheme.PropertyType
        
        
        let schemas = importer.ImportTp(tp)
        schemas.Write(tw)


    member private x.File_WriteXml(xmlPath: string, value: 'T) =
        use sww = new StreamWriter(xmlPath)
        use writer = 
            XmlWriter.Create(
                sww,
                XmlWriterSettings(Indent = true, Encoding = encoding)
            )

        match box value with 
        | :? FsIXmlSerializable as v -> 
            writer.WriteStartElement(tp.Name)
            v.WriteXml(writer)
            writer.WriteEndElement()
        | _->
            let xsSubmit = new XmlSerializer(tp);
            xsSubmit.Serialize(writer, value)


    //member private x.WriteXml_FsPickler(xmlPath: string, value: 'T) =
    //    let sww = new StreamWriter(xmlPath)
    //    let xsSubmit = FsPickler.CreateXmlSerializer(indent = true)

    //    xsSubmit.Serialize(sww, value)

    member private x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath) =
        let xsdFileName = Path.GetFileName xsdPath
        let lines = 
            File.ReadAllLines(xmlPath)
            |> Array.mapi(fun i line ->
                match i with 
                | 1 -> line.Replace("xmlns:xsi", $"xsi:noNamespaceSchemaLocation=\"{xsdFileName}\" xmlns:xsi")
                | _ -> line
            )
        File.WriteAllLines(xmlPath, lines)
     

    member private x.File_TrimXsdSchemeEnd(xsdPath) =
        let lines = 
            File.ReadAllLines(xsdPath)
            |> Array.mapi(fun i line ->
                line.Replace("_XMLScheme\"", $"\"")
            )
        File.WriteAllLines(xsdPath, lines)
     
    //static member private MakeSelf(propTp: Type, methodName) =
    //    let x = Unchecked.defaultof<FsXmlSerializer<_>>
    //    let subPropSerializer, subPropSerializerTP = 
    //        makeSelfFsXmlSerializerCache.GetOrAdd(propTp, valueFactory = fun _ ->
    //            let subPropSerializerTP = 
    //                let subPropSerializerType = 
    //                    typedefof<FsXmlSerializer<_>>

    //                subPropSerializerType.MakeGenericType([|propTp|])

    //            let subPropSerializer = Activator.CreateInstance(subPropSerializerTP)

    //            subPropSerializer, subPropSerializerTP
    //        )

    //    let method =
    //        subPropSerializerTP.GetMethod(methodName)
        
    //    fun parameters ->
    //        method.Invoke(subPropSerializer, parameters)

    static member private SerializeValueProp(prop: SCasablePropertyType, propValue: obj, writer: XmlWriter) =        

        let propTp = prop.PropertyType
        let tpCode = getFsTpCodeEx (propTp)
        match tpCode with 
        | FsTypeCodeEx.CollectionType propCollectionTp -> 
            writer.WriteStartElement(prop.Name)
            let elementTp = propCollectionTp.ElementType

            match propValue with 
            | :? System.Collections.IEnumerable as items ->
                for item in items do 
                    FsXmlSerializer<_>.SerializeValueProp(SCasablePropertyType.Type elementTp, item, writer)

            | _ -> failwithf "%s should be IEnumerable type" propTp.Name

            writer.WriteEndElement()

        | FsTypeCodeEx.FsTypeCode tpCode ->
            
            match tpCode with 
            | FsTypeCode.ValueType _
            | FsTypeCode.Enum ->
                writer.WriteElementString(prop.Name, propValue.ToString())

            | FsTypeCode.Object objectTpCode ->
                match propValue with 
                | :? IXmlSerializable as xmlSerilizable -> xmlSerilizable.WriteXml(writer)
                | _ -> 
                    match objectTpCode with 
                    | FsObjectTypeCode.Record -> 
                        writer.WriteStartElement(propTp.Name)
                        FsXmlSerializer<_>.SerializeValueStatic(writer, propValue)
                        writer.WriteEndElement()


                    | FsObjectTypeCode.SingletonCaseUnion case ->
                        writer.WriteStartElement(propTp.Name)
                        let fields = case.GetFields()
                        match fields with 
                        | [||] -> failwithf "Not implemented"
                        | [|field|] ->
                            let fieldValue = field.GetValue(propValue)
                            FsXmlSerializer<_>.SerializeValueProp(SCasablePropertyType.OneFieldSCase field, fieldValue, writer)

                        | fields ->
                        
                            writer.WriteStartElement("SCase")

                            for field in fields do
                                let fieldValue = field.GetValue(propValue)
                                FsXmlSerializer<_>.SerializeValueProp(SCasablePropertyType.PropertyInfo field, fieldValue, writer)
                                ()

                            writer.WriteEndElement()

                        writer.WriteEndElement()

                    | FsObjectTypeCode.Union -> failwithf "Not implemented"

    static member private SerializeValueStatic(writer: XmlWriter, value: obj) =
        let tp = value.GetType()

        match FSharpType.IsRecord tp with 
        | true ->
            let props = FSharpType.GetRecordFields(tp)
            for prop in props do 
                let propValue = prop.GetValue(value)
                FsXmlSerializer<_>.SerializeValueProp(SCasablePropertyType.PropertyInfo prop, propValue, writer)

        | false -> failwithf "Not implemented"


    member x.SerializeValue(writer: XmlWriter, value: 'T) =
        match FSharpType.IsRecord tp with 
        | true ->
            for prop in props do 
                let propValue = prop.GetValue(value)
                FsXmlSerializer<_>.SerializeValueProp(SCasablePropertyType.PropertyInfo prop, propValue, writer)
                

        | false -> failwithf "Not implemented"

    member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T) =
        x.File_WriteXml(xmlPath, value)
        //x.File_WriteCsXsd(xsdPath)
        x.File_WriteFsXsd(xsdPath)
        x.File_TrimXsdSchemeEnd(xsdPath)
        x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath)


    member x.DeserializeFromFile(fileName: string) =
        
        let serializer = new XmlSerializer(tp)
        use reader = new FileStream(fileName, FileMode.Open)
        let value = FormatterServices.GetUninitializedObject(tp)
        match value with 
        | :? FsIXmlSerializable as value ->
            let reader = XmlReader.Create(reader)
            value.ReadXml(reader)
            |> unbox<_>

        | _ ->
            let r = serializer.Deserialize(reader);
            r :?> 'T

    static member private DeserializeToProp(reader: XmlReader, value: obj, prop: SCasablePropertyType) =

        while (reader.Read()) do

            let nodeType = reader.NodeType
            match nodeType with 
            | XmlNodeType.Element -> 
                let propTp = (prop.PropertyType)
                let tpCode = getFsTpCode(propTp)
                match tpCode with 
                | FsTypeCode.Enum 
                | FsTypeCode.ValueType _ -> reader.Read() |> ignore 
                | _ -> ()

                let propText = reader.Value

                match tpCode with 
                | FsTypeCode.ValueType _ ->
                    let propValue = Convert.ChangeType(propText, propTp)
                    
                    prop.SetValue(value, propValue)
                    
                | FsTypeCode.Enum ->  
                    let propValue = 
                        let enumValue = System.Enum.Parse(propTp, propText)
                        enumValue

                    prop.SetValue(value, propValue)


                | FsTypeCode.Object _ ->
                    match reader.NodeType with 
                    | XmlNodeType.Element ->
                        let propTpCode = getFsTpCodeEx propTp


                        let inputPropValue = 
                            match prop.GetValue(value) with 
                            | null ->  FormatterServices.GetUninitializedObject(propTp)
                            | value -> value


                        match inputPropValue with 
                        | :? IXmlSerializable as inputPropValue ->
                            inputPropValue.ReadXml(reader)

                        | _ ->
                            let goNext() =
                                let method = 
                                    FsXmlSerializer<_>.MakeSelf(propTp, nameof x.DeserializeToValue)


                                let r = method([|reader; inputPropValue|])
                                ()
                                
                            match propTpCode with 
                            | FsTypeCodeEx.CollectionType propTpCode -> failwithf "Not implemented"
                            | FsTypeCodeEx.FsTypeCode propTpCode ->
                                match propTpCode with 
                                | FsTypeCode.Enum
                                | FsTypeCode.ValueType _ -> goNext()


                        prop.SetValue(value, inputPropValue)

                    | _ -> failwithf "Not implemented"
                    
                    

            | XmlNodeType.EndElement -> ()

    
    member x.DeserializeToValue(reader: XmlReader): 'T =
        failwithf ""
        //while (reader.Read()) do
        //    let nodeType = reader.NodeType
        //    match nodeType with 
        //    | XmlNodeType.Element -> 
        //        let prop = 
        //            props
        //            |> Array.find(fun m -> m.Name = reader.Name)

        //        FsXmlSerializer<_>.DeserializeToProp()
                
                    
                    

        //    | XmlNodeType.EndElement -> ()




    member x.WriteAttributeString_xsi_xsd(writer: XmlWriter) =
        writer.WriteAttributeString("xmlns", "xsi", null, "http://www.w3.org/2001/XMLSchema-instance")
        writer.WriteAttributeString("xmlns", "xsd", null, "http://www.w3.org/2001/XMLSchema")