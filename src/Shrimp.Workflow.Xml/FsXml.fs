// Learn more about F# at http://fsharp.org
namespace Shrimp.Workflow.Xml
#nowarn "0104"
#nowarn "3535"
#nowarn "3536"

open System

open System.Collections.Generic

open System.Reflection

open System.Xml.Serialization
open System.IO
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Xml


type private MapSerializer<'k,'v when 'k : comparison>() =
    static member Deserialize(t:DictionaryType, dict: Dictionary<obj, obj>) =
        match t with 
        | DictionaryType.Dictionary _ ->    
            let newDict = Dictionary<'k, 'v>()
            for pair in dict do 
                newDict.Add(pair.Key :?> 'k, pair.Value :?> 'v)

            box newDict

        | DictionaryType.ConcurrrentDictionary _ ->
            let newDict = ConcurrentDictionary<'k, 'v>()
            for pair in dict do 
                newDict.TryAdd(pair.Key :?> 'k, pair.Value :?> 'v)
                |> ignore

            box newDict

        | DictionaryType.FSharpMap _ ->
            let tupleLists = 
                dict
                |> Seq.map(fun pair ->
                    pair.Key :?> 'k, pair.Value :?> 'v
                )

            let newDict = Map.ofSeq tupleLists
            box newDict


    static member Serialize(t: DictionaryType, value: obj, writer:XmlWriter, configuration: FsXmlSerializerConfiguration, serializeRecordStatic) =
        let tupleList =
            match t with 
            | DictionaryType.FSharpMap _ -> 
                value :?> Map<'k, 'v>
                |> Map.toList

            | DictionaryType.Dictionary _ ->
                value :?> Dictionary<'k, 'v>
                |> Seq.toList
                |> List.map(fun m -> m.Key, m.Value)

            | DictionaryType.ConcurrrentDictionary _ ->
                value :?> ConcurrentDictionary<'k, 'v>
                |> Seq.toList
                |> List.map(fun m -> m.Key, m.Value)


        for (key, value) in tupleList do
            let entry = Entry.createByObjects(key, value) t
            writer.WriteStartElement("Entry")
            writer.WriteAttributeString("type", W3XMLSchemaInstance, t.EntryTypeName)
            serializeRecordStatic(writer, entry, false, configuration)
            writer.WriteEndElement()




type FsXmlSerializer<'T>(configuration) =
    let configuration = configuration
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
        let importer = new FsSchemeImporter(configuration);
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
            v.WriteXml(writer, configuration)
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

    static member private SerializeValue(writer: XmlWriter, prop: SCasablePropertyType, propValue: obj,  configuration: FsXmlSerializerConfiguration, ?inCollection) =        
        let prop, propValue = 
            configuration.UpdateSCasablePropertyTypeAndValue_ToXml(prop, propValue)

        let propTp = prop.PropertyType

        let tpCode = getFsTpCodeEx (propTp)
        match tpCode with 
        | FsTypeCodeEx.Tuple (tpCodes) ->
            let inCollection = defaultArg inCollection false
            let ignorePropInCollection(f) =
                match inCollection with 
                | false -> 
                    writer.WriteStartElement(prop.Name)
                    f()
                    writer.WriteFullEndElement()

                | true -> f()

            ignorePropInCollection(fun () ->
                writer.WriteStartElement("Tuple" + tpCodes.Length.ToString())

                let tupleElements =
                    FSharpValue.GetTupleFields(propValue)

                (tpCodes, tupleElements)
                ||> Array.iteri2(fun i (tpCode, tp) tupleElement ->
                    let name = itemText i
                    FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NamedType(name, tp), tupleElement, configuration)
                )

                writer.WriteFullEndElement()
            )


        | FsTypeCodeEx.Option (tpCode, tp) ->
            match propValue with 
            | null -> 
                writer.WriteStartElement(prop.Name)
                writer.WriteAttributeString("nil", W3XMLSchemaInstance, "true")
                writer.WriteFullEndElement()

            | propValue ->
                let valueProp = propValue.GetType().GetProperty("Value")
                let propValue = valueProp.GetValue(propValue)
                FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NillableNamedType(prop.Name, tp), propValue, configuration)

        | FsTypeCodeEx.DictionaryType propDictTp -> 
            writer.WriteStartElement(prop.Name)

            let mapSerializeMethod = 
                let mapSerializer = typedefof<MapSerializer<_,_>>.MakeGenericType([|propDictTp.KeyType; propDictTp.ValueType|])
                let bindingFlags =
                    BindingFlags.NonPublic ||| BindingFlags.Static 
                mapSerializer.GetMethod("Serialize", bindingFlags)

            mapSerializeMethod.Invoke(null, [| propDictTp; propValue; writer; configuration; FsXmlSerializer<_>.SerializeRecordStatic |]) |> ignore
           
            writer.WriteEndElement()


        | FsTypeCodeEx.CollectionType propCollectionTp -> 
            writer.WriteStartElement(prop.Name)
            let elementTp = propCollectionTp.ElementType

            match propValue with 
            | :? System.Collections.IEnumerable as items ->
                for item in items do 
                    FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.Type elementTp, item, configuration, inCollection = true)

            | _ -> failwithf "%s should be IEnumerable type" propTp.Name

            writer.WriteEndElement()

        | FsTypeCodeEx.FsTypeCode tpCode ->
            
            let writeProp(f) =
                match prop with 
                | SCasablePropertyType.NillableNamedType _ -> 
                    writer.WriteStartElement(prop.Name)
                    writer.WriteAttributeString("nil", W3XMLSchemaInstance, "false")
                    f()
                | _ -> 
                    writer.WriteStartElement(prop.Name)
                    f()

                writer.WriteFullEndElement()

            match tpCode with 
            | FsTypeCode.ValueType _
            | FsTypeCode.Enum ->
                writeProp(fun () ->
                    writer.WriteValue(propValue.ToString())
                )
                    

            | FsTypeCode.Object objectTpCode ->
                match propValue with 
                | :? FsIXmlSerializable as xmlSerilizable -> xmlSerilizable.WriteXml(writer, configuration)
                | _ -> 
                    writeProp(fun () ->
                        match objectTpCode with 
                        | FsObjectTypeCode.FsXmlSerializable  -> 
                            /// already predicate by previous line (| :? FsIXmlSerializable as xmlSerilizable)
                            failwithf "Invalid token"

                        | FsObjectTypeCode.Record -> 
                            FsXmlSerializer<_>.SerializeRecordStatic(writer, propValue, prop.Nillable, configuration)

                        | FsObjectTypeCode.SingletonCaseUnion case ->
                            let fields = case.GetFields()
                            match fields with 
                            | [||] -> failwithf "Not implemented"
                            | [|field|] ->
                                let fieldValue = field.GetValue(propValue)
                                FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.OneFieldSCase field, fieldValue, configuration)

                            | fields ->
                            
                                writer.WriteStartElement("SCase")

                                for field in fields do
                                    let fieldValue = field.GetValue(propValue)
                                    FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.PropertyInfo field, fieldValue, configuration)
                                    ()

                                writer.WriteEndElement()


                        | FsObjectTypeCode.Union cases -> 
                            let uci, fields = FSharpValue.GetUnionFields(propValue, propTp)
                            let zippedFields =
                                uci.GetFields()
                                |> Array.zip fields


                            writer.WriteStartElement("Choice" + cases.Length.ToString())
                            match zippedFields with 
                            | [||] -> 
                                writer.WriteStartElement(uci.Name)
                                writer.WriteFullEndElement()

                            | [|field, fieldTp|] ->
                                FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NamedType(uci.Name, fieldTp.PropertyType), field, configuration)
                                
                            | zippedFields ->
                                writer.WriteStartElement(uci.Name)

                                for (field, fieldTp) in zippedFields do
                                    FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.PropertyInfo(fieldTp), field, configuration)

                                writer.WriteFullEndElement()

                                
                            writer.WriteEndElement()

                                
                    )


    static member private SerializeRecordStatic(writer: XmlWriter, value: obj, nillable, configuration) =
        let tp = value.GetType()

        match FSharpType.IsRecord tp with 
        | true ->
            let props = FSharpType.GetRecordFields(tp)
            for prop in props do 
                let propValue = prop.GetValue(value)
                let propType =
                    match nillable with 
                    | false -> SCasablePropertyType.PropertyInfo prop
                    | true -> SCasablePropertyType.NillableNamedType (prop.Name, prop.PropertyType)

                FsXmlSerializer<_>.SerializeValue(writer, propType, propValue, configuration)

        | false -> failwithf "Not implemented"

    static member SerializeXmlNodeValue(writer: XmlWriter, value: obj, configuration, ?tpName) =
        let tp = value.GetType()
        let tpName = defaultArg tpName tp.Name
        let tp = SCasablePropertyType.CreateNamedType(tpName, tp)

        FsXmlSerializer<_>.SerializeValue(writer, tp, value, configuration)
    
    member private x.WriteAttributeString_xsi_xsd(writer: XmlWriter) =
        writer.WriteAttributeString("xmlns", "xsi", null, W3XMLSchemaInstance)
        writer.WriteAttributeString("xmlns", "xsd", null, W3XMLSchema)

    member x.SerializeRecord(writer: XmlWriter, value: 'T) =
        match FSharpType.IsRecord tp with 
        | true ->
            x.WriteAttributeString_xsi_xsd(writer)
            for prop in props do 
                let propValue = prop.GetValue(value)
                FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.PropertyInfo prop, propValue, configuration)
                

        | false -> failwithf "Not implemented"



    member x.DeserializeFromFile(fileName: string) =
        
        use reader = new FileStream(fileName, FileMode.Open)
        match getReadXmlObjMethod tp with 
        | None ->
            let serializer = new XmlSerializer(tp)
            let r = serializer.Deserialize(reader);
            r :?> 'T

        | Some method ->
            let reader = XmlReader.Create(reader)
            let r = method.Invoke(null, [|tp; reader; configuration|])
            r :?> 'T



    static member private DeserializeToProp(reader: XmlReader, prop: SCasablePropertyType, configuration: FsXmlSerializerConfiguration, ?inCollection: bool) =
        let nodeType = reader.NodeType
        let propMappingOp = configuration.UpdateSCasablePropertyType_ToXml_Op prop

        let prop0 =
            match propMappingOp with 
            | None -> prop
            | Some prop -> prop.PropertyType

        let prop =
            match propMappingOp with 
            | None -> configuration.UpdateSCasablePropertyType_ToXml prop
            | Some prop -> prop.PropertyType

        let value = 
            match nodeType with 
            | XmlNodeType.Element -> 
                let tpCode = 
                    let propTp =  (prop.PropertyType) 
                    getFsTpCodeEx(propTp)

                let propTp = prop0.PropertyType

                match tpCode with 
                | FsTypeCodeEx.Tuple (_) ->   
                    let tpCodes = FSharpType.GetTupleElements propTp

                    let inCollection = defaultArg inCollection false
                    let advanceReader_IgnorePropInCollection(reader, f) =
                        match inCollection with 
                        | false -> 
                            advanceReaderFullElement(reader, fun _ ->
                                advanceReaderFullElement(reader, fun _ ->
                                    f()
                                )
                                |> ignore
                            )
                            |> ignore

                        | true ->
                            advanceReaderFullElement(reader, fun _ ->
                                f()
                            )
                            |> ignore

                    let tupleElements = ResizeArray()

                    advanceReader_IgnorePropInCollection(reader, fun () ->
                        let i = tupleElements.Count
                        let (tp) = tpCodes.[i]
                        let name = itemText i
                        let element = FsXmlSerializer<_>.DeserializeToProp(reader, SCasablePropertyType.NamedType(name, tp), configuration)
                        tupleElements.Add(element)
                    )
                
                    let tuple = FSharpValue.MakeTuple(Array.ofSeq tupleElements, propTp)
                    tuple
                
                | FsTypeCodeEx.Option (_, _) ->
                    let elementType = propTp.GetGenericArguments().[0]
                    let elementValue = 
                        FsXmlSerializer<_>.DeserializeToProp(
                            reader,
                            SCasablePropertyType.NillableNamedType(elementType.Name, elementType),
                            configuration)
                    let r = makeOption(propTp, elementType, elementValue)
                    r

                | FsTypeCodeEx.DictionaryType tpCode ->
                    let genericArguments = propTp.GetGenericArguments()
                    let keyTp = genericArguments.[0]
                    let valueTp = genericArguments.[1]

                    let tpCode = 
                        tpCode.SetType(keyTp, valueTp)

                    let dict = Dictionary<_, _>()
                    advanceReaderFullElement(reader, fun stack ->
                        advanceReaderFullElement(reader, fun stack ->
                            let key = 
                                FsXmlSerializer<_>.DeserializeToProp(
                                    reader,
                                    SCasablePropertyType.NamedType("Key", keyTp),
                                    configuration
                                )
                            advanceReader(reader) |> ignore
                            let value = 
                                FsXmlSerializer<_>.DeserializeToProp(
                                    reader,
                                    SCasablePropertyType.NamedType("Value", valueTp),
                                    configuration
                                )
                            dict.Add(key, value)
                        )
                        |> ignore
                    ) |> ignore
                
                    let r = tpCode.MakeObject(dict)
                    r

                | FsTypeCodeEx.CollectionType _ -> 
                    let tpCode = 
                        match getFsTpCodeEx propTp with 
                        | FsTypeCodeEx.CollectionType tpCode -> tpCode
                        | _ -> failwithf "Invalid token"

                    let elementTp = tpCode.ElementType
                    let elements = ResizeArray()
                    advanceReaderFullElement(reader, fun stack ->
                        let prop = 
                            FsXmlSerializer<_>.DeserializeToProp(
                                reader,
                                SCasablePropertyType.Type elementTp,
                                configuration,
                                inCollection = true)
                        elements.Add(prop)
                        ()
                    ) |> ignore
                
                    let elements = tpCode.MakeObject(elements)
                    elements


                | FsTypeCodeEx.FsTypeCode tpCode ->
                    let createNullable(text: string, f) =
                        match text, prop.Nillable with 
                        | "", true -> null
                        | _ -> f()
                        
                
                    match tpCode with 
                    | FsTypeCode.Enum ->
                        reader.Read() |> ignore 
                        let propText = reader.Value
                        createNullable(propText, fun _ ->
                            System.Enum.Parse(propTp, propText)
                        )

                    | FsTypeCode.ValueType _ -> 
                        reader.Read() |> ignore 
                        let propText = reader.Value
                        createNullable(propText, fun _ ->
                            Convert.ChangeType(propText, propTp)
                        )


                    | FsTypeCode.Object tpCode -> 
                        match getReadXmlObjMethod propTp with 
                        | Some methodInfo ->
                            let r = methodInfo.Invoke(null, [|propTp; reader; configuration|])
                            r

                        | None -> 
                            match tpCode with 
                            | FsObjectTypeCode.FsXmlSerializable  -> 
                                /// already predicate by previous line (match getReadXmlObjMethod propTp with)
                                failwithf "Invalid token"

                            | FsObjectTypeCode.Record ->
                                FsXmlSerializer<_>.DeserializeToRecordStatic(reader, propTp, configuration)
                            
                            | FsObjectTypeCode.SingletonCaseUnion uci ->  
                                let elements = ResizeArray()

                                advanceReaderFullElement(reader, fun _ ->
                                    let fields = uci.GetFields() 
                                    match fields with 
                                    | [||] -> failwithf "Not implemented"
                                    | [|field|] ->
                                        let uciValue = 
                                            FsXmlSerializer<_>.DeserializeToProp(
                                                reader,
                                                SCasablePropertyType.OneFieldSCase field,
                                                configuration)
                                        elements.Add(uciValue)

                                    | fields ->
                                        let uciValues = 
                                            fields
                                            |> Array.map(fun field ->
                                                advanceReader(reader) |> ignore
                                                let r = 
                                                    FsXmlSerializer<_>.DeserializeToProp(
                                                        reader,
                                                        SCasablePropertyType.OneFieldSCase field,
                                                        configuration
                                                    )
                                                r
                                            )
                                        advanceReader(reader) |> ignore
                                        elements.AddRange(uciValues)
                                )
                                |> ignore

                                let fields = Array.ofSeq elements
                                let r = FSharpValue.MakeUnion(uci, fields, allowAccessToPrivateRepresentation = true)
                                r

                            | FsObjectTypeCode.Union cases -> 
                                let mutable caseValue = None
                                advanceReaderFullElement(reader, fun _ ->
                                    advanceReaderFullElement(reader, fun _ ->
                                        let name = reader.Name
                                        let uci = 
                                            cases
                                            |> Array.find(fun m -> m.Name = name)

                                        let fields = uci.GetFields()
                                        let value = 
                                            match fields with 
                                            | [||] -> FSharpValue.MakeUnion(uci, [||])
                                            | [|field|] ->
                                                let value = 
                                                    FsXmlSerializer<_>.DeserializeToProp(   
                                                        reader,
                                                        SCasablePropertyType.NamedType(uci.Name, field.PropertyType),
                                                        configuration
                                                    )

                                                FSharpValue.MakeUnion(uci, [|value|])

                                            | fields ->
                                                let elements = ResizeArray()
                                                advanceReaderFullElement(reader, fun _ ->
                                                    let name = reader.Name
                                                    let field = 
                                                        fields
                                                        |> Array.find(fun m -> m.Name = name)

                                                    let element = 
                                                        FsXmlSerializer<_>.DeserializeToProp(
                                                            reader,
                                                            SCasablePropertyType.PropertyInfo(field),
                                                            configuration
                                                        )

                                                    elements.Add(element)
                                                )
                                                |> ignore
                                                //let value = 
                                                //    FsXmlSerializer<_>.DeserializeToProp(reader, SCasablePropertyType.NamedType(uci.Name, field.PropertyType))
                                                FSharpValue.MakeUnion(uci, Array.ofSeq elements)

                                        caseValue <- Some value
                                    )
                                    |> ignore
                                )
                                |> ignore

                                caseValue.Value


            | _ -> failwithf "Not implemented"
                    

        match propMappingOp with 
        | None -> value
        | Some propMappingOp -> 
            match prop.Nillable, value with 
            | true, null -> null
            | _ -> propMappingOp.TypeMapping.OfXmlSerializable value
                    

    static member DeserializeXmlNodeValueTo(reader: XmlReader, tp: Type, configuration, ?tpName) =
        let tpName = defaultArg tpName tp.Name
        let tp = SCasablePropertyType.CreateNamedType(tpName, tp)

        FsXmlSerializer<_>.DeserializeToProp(reader, tp, configuration)

    static member DeserializeToRecordStatic(reader: XmlReader, tp: Type, configuration) =
            
        let props = FSharpType.GetRecordFields tp

        let rec loop accum =
            match advanceReader(reader) with 
            | true ->
                match reader.NodeType with 
                | XmlNodeType.Element -> 
                    let prop = 
                        props
                        |> Array.find(fun m -> m.Name = reader.Name)

                    let propValue = 
                        FsXmlSerializer<_>.DeserializeToProp(
                            reader,
                            SCasablePropertyType.PropertyInfo prop,
                            configuration
                        )
                    loop (propValue :: accum) 

                | _ -> failwithf "Not implemented"

            | false -> List.rev accum

        let props = loop []
        FSharpValue.MakeRecord(tp, List.toArray props)
    
    member x.DeserializeToRecord(reader: XmlReader): 'T =
        let props = props
        let rec loop accum =
            match advanceReader(reader) with 
            | true -> 
                match reader.NodeType with 
                | XmlNodeType.Element -> 
                    let prop = 
                        props
                        |> Array.find(fun m -> m.Name = reader.Name)

                    let propValue = FsXmlSerializer<_>.DeserializeToProp(reader, SCasablePropertyType.PropertyInfo prop, configuration)
                    loop (propValue :: accum) 

                | _ -> failwithf "Not implemented"

            | false -> List.rev accum

        advanceReader(reader) |> ignore
        match reader.Name = tp.Name || reader.Name + "_XMLScheme" = tp.Name with 
        | true -> 
            reader.Read() |> ignore
            |> ignore

        | false -> failwithf "Invalid token, reader.Name %s should be record type name %s here" reader.Name tp.Name


        let props = loop []
        FSharpValue.MakeRecord(tp, List.toArray props)
        |> unbox<'T>


    member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T) =
        x.File_WriteFsXsd(xsdPath)
        x.File_WriteXml(xmlPath, value)
        //x.File_WriteCsXsd(xsdPath)
        x.File_TrimXsdSchemeEnd(xsdPath)
        x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath)
