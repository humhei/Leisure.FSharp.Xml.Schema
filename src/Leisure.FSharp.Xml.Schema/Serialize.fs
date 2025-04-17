// Learn more about F# at http://fsharp.org
namespace Leisure.FSharp.Xml.Schema
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
open FsSchemaTypesAST

[<AutoOpen>]
module internal rec _SerializePart = 
    type ZippedXmlWriter =
        { Writer: XmlWriter
          RecursiveResolver: RecursiveResolver
          }
    with 
        member x.WriteStartElement(localName) = x.Writer.WriteStartElement(localName)

        member x.WriteStartElement(localName, ns) = x.Writer.WriteStartElement(localName, ns)

        member x.WriteEndElement() = x.Writer.WriteEndElement()

        member x.WriteValue(value: string) = x.Writer.WriteValue(value)

        member x.WriteFullEndElement() = x.Writer.WriteFullEndElement()

        member x.WriteString(text) = x.Writer.WriteString(text)

        member x.WriteAttributeString(localName, ns, value) = x.Writer.WriteAttributeString(localName, ns, value)
        member x.WriteAttributeString(prefix, localName, ns, value) = x.Writer.WriteAttributeString(prefix, localName, ns, value)


    type private MapSerializer<'k,'v when 'k : comparison>() =
        //static member Deserialize(t:DictionaryType, dict: Dictionary<obj, obj>) =
        //    match t with 
        //    | DictionaryType.Dictionary _ ->    
        //        let newDict = Dictionary<'k, 'v>()
        //        for pair in dict do 
        //            newDict.Add(pair.Key :?> 'k, pair.Value :?> 'v)

        //        box newDict

        //    | DictionaryType.ConcurrrentDictionary _ ->
        //        let newDict = ConcurrentDictionary<'k, 'v>()
        //        for pair in dict do 
        //            newDict.TryAdd(pair.Key :?> 'k, pair.Value :?> 'v)
        //            |> ignore

        //        box newDict

        //    | DictionaryType.FSharpMap _ ->
        //        let tupleLists = 
        //            dict
        //            |> Seq.map(fun pair ->
        //                pair.Key :?> 'k, pair.Value :?> 'v
        //            )

        //        let newDict = Map.ofSeq tupleLists
        //        box newDict


        static member Serialize(t: FsSchemaComplexType_Dictionary, value: obj, writer: ZippedXmlWriter) =
            let tupleList =
                match t.TypeCode with 
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
                writer.WriteStartElement("Entry")
                //writer.WriteAttributeString("type", W3XMLSchemaInstance, t.EntryTypeName)
                let __writeKey =
                    let propTp =
                        { Name = Some "Key" 
                          FsSchemaType = t.Entry.KeySchemaType }
                    propTp.WriteValue(writer, key)

                let __writeValue =
                    let propTp =
                        { Name = Some "Value" 
                          FsSchemaType = t.Entry.ValueSchemaType }

                    propTp.WriteValue(writer, value)

                writer.WriteEndElement()


    type FsSchemaSimpleType_Enum with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            writer.WriteValue(value.ToString())

    type FsSchemaSimpleType_Value with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            let value = 
                match value with 
                | :? bool as b -> b.ToString().ToLower()
                | _ -> value.ToString()
            writer.Writer.WriteRaw(value)
            //writer.WriteString(value)

    type FsSchemaSimpleType with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x with 
            | FsSchemaSimpleType.EnumType v -> v.WriteValue(writer, value)
            | FsSchemaSimpleType.ValueType v -> v.WriteValue(writer, value)

    type FsSchemaComplexType_Collection with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =   
            match x.ListRestriction with 
            | None -> 
                match value with 
                | :? System.Collections.IEnumerable as items ->
                    for item in items do    
                        let itemTp = 
                            { Name = None 
                              FsSchemaType = x.ElementSchemaType
                            }
                        
                        itemTp.WriteValue(writer, item)


                | _ -> failwithf "%A should be IEnumerable type" (value.GetType())

            | Some listRestriction ->
                let value = value :?> IFsXmlSchemaListWithRestriction
                let value = value.BoxedItems
                { x with ListRestriction = None}.WriteValue(writer, box value)

    type FsSchemaComplexType_Dictionary with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =  
            let propDictTp = x.TypeCode
            let mapSerializeMethod = 
                let mapSerializer = typedefof<MapSerializer<_,_>>.MakeGenericType([|propDictTp.KeyType; propDictTp.ValueType|])
                let bindingFlags =
                    BindingFlags.NonPublic ||| BindingFlags.Static 
                mapSerializer.GetMethod("Serialize", bindingFlags)

            mapSerializeMethod.Invoke(null, [| x; value; writer |]) |> ignore
           
    type FsSchemaComplexType_Tuple with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =  
            writer.WriteStartElement("Tuple" + x.Elements.Length.ToString())

            let tupleElements =
                FSharpValue.GetTupleFields(value)
                |> List.ofArray

            (x.ElementSchemaTypes, tupleElements)
            ||> List.iteri2(fun i schemaType tupleElement -> 
                writer.WriteStartElement("Item" + (i+1).ToString())
                schemaType.WriteValue(writer, tupleElement)
                writer.WriteFullEndElement()
            )

            writer.WriteFullEndElement()

    type FsSchemaComplexGenericType with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexGenericType.Collection v -> v.WriteValue(writer, value)
            | FsSchemaComplexGenericType.Dictionary v -> v.WriteValue(writer, value)
            | FsSchemaComplexGenericType.Tuple v -> v.WriteValue(writer, value)
            | _ -> failwithf "Not implemented"

    type FsSchemaComplexType_Record with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            x.Zip3()
            |> List.iter(fun (element, schemaType, propertyInfo) ->
                let propValue = propertyInfo.GetValue(value)
                let propTp =
                    { Name = Some element.Name.Text
                      FsSchemaType = schemaType }

                match schemaType.GetFSharpOptionRevealWay() with 
                | Some fsharpOptionRevealWay ->
                    match propValue with 
                    | null ->
                        match fsharpOptionRevealWay with 
                        | FSharpOptionRevealWay.Reveal -> propTp.WriteValue(writer, propValue)
                        | FSharpOptionRevealWay.AlwaysHideInXml ->
                            ()

                    | _ -> propTp.WriteValue(writer, propValue)
                | None ->
                    propTp.WriteValue(writer, propValue)
            )

    type FsSchemaComplexType_SingleCaseUnion with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexType_SingleCaseUnion.NoField noField ->
                //writer.WriteStartElement(noField.Type.Name)
                //writer.WriteEndElement()
                writer.WriteString(noField.Type.Name)
                //writer.WriteElementString(noField.Type.Name, "")
                //|> ignore

            | FsSchemaComplexType_SingleCaseUnion.OneField field ->
                let fieldValue = 
                    let field = field.Field
                    field.GetValue(value)

                match field.Element.SimpleTypeInfo with 
                | None -> 

                    let namedSchemaType =
                        { Name = Some field.Element.Name.Text
                          FsSchemaType = field.ElementSchemaType }

                    namedSchemaType.WriteValue(writer, fieldValue)

                | Some simpleTypeInfo ->
                    field.ElementSchemaType.WriteValue(writer, fieldValue)


           
            | FsSchemaComplexType_SingleCaseUnion.MultipleFields fields -> 
                writer.WriteStartElement("SCase")

                for (element, schemaTp, propInfo) in fields.Zip3() do
                    let fieldValue = propInfo.GetValue(value)
                    let namedSchemaType =
                        { Name = Some element.Name.Text
                          FsSchemaType = schemaTp }

                    namedSchemaType.WriteValue(writer, fieldValue)

                writer.WriteEndElement()

    type FsSchemaComplexType_Union with 
        member x.WriteValue(writer: ZippedXmlWriter, propValue: obj) =
            let propTp = x.Type
            let uci, fields = FSharpValue.GetUnionFields(propValue, propTp)
            //let zippedFields =
            //    uci.GetFields()
            //    |> Array.zip fields

            let getUciSchema() =
                let uci = 
                    x.UnionCases
                    |> List.find(fun m -> 
                        m.UnionCase = uci
                    )

                uci

            let cases = x.UnionCases
            match x.TagOptions with
            | UnionTagOptions.Independent ->
                writer.WriteStartElement("Choice" + cases.Length.ToString())

            | UnionTagOptions.SuffixToRootType -> ()

            let uciName = 
                PropNameOrElementName.FixPropName uci.Name

            match fields with 
            | [||] -> 
                writer.WriteStartElement(uciName)
                writer.WriteFullEndElement()

            | [|field|] ->
                let uciSchema = 
                    match getUciSchema() with 
                    | FsSchemaComplexType_UnionCase.OneFieldCase v -> v
                    | uci -> failwithf "Invalid token, uciSchema %A should be OneFieldCase here" uci

                let schemaType =
                    { Name = Some uciName 
                      FsSchemaType = uciSchema.ElementSchemaType }

                schemaType.WriteValue(writer, field)

            | fields ->
                writer.WriteStartElement(uciName)

                let uciSchema = 
                    match getUciSchema() with 
                    | FsSchemaComplexType_UnionCase.MultipleFieldsCase v -> v
                    | uci -> failwithf "Invalid token, uci %A should be OneFieldCase here" uci

                let zippedFields =
                    fields
                    |> List.ofArray
                    |> List.zip (uciSchema.Zip3())
                    
                for ((_, fieldTp, propInfo), field) in zippedFields do
                    let fieldTp =
                        { Name = Some propInfo.Name 
                          FsSchemaType = fieldTp 
                        }

                    fieldTp.WriteValue(writer, field)

                writer.WriteFullEndElement()

            match x.TagOptions with
            | UnionTagOptions.Independent ->
                writer.WriteEndElement()

            | UnionTagOptions.SuffixToRootType -> ()       

    type FsSchemaComplexType with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexType.Generic v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.Record v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.SinglecaseUnion v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.Union v -> v.WriteValue(writer, value)


    type MappedFsSchemaType with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x.WrapOldName with 
            | false -> 
                let value = x.TypeMappingPair.ToXmlSerializable value
                x.FsSchemaType.WriteValue(writer, value)

            | true -> 
                let elementName = x.GetElementName()
                writer.WriteStartElement(elementName)
                let value = x.TypeMappingPair.ToXmlSerializable value
                x.FsSchemaType.WriteValue(writer, value)
                writer.WriteFullEndElement()
                

    type FsSchemaType with 
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) =
            match x with 
            | FsSchemaType.Ignore ignoreInfo ->
                match ignoreInfo.IgnoreInfo_MinOccurs_Zero__OR__DeleteAll with 
                | true -> ()
                | false -> writer.WriteAttributeString("nil", W3XMLSchemaInstance, "true")
                //()

            | FsSchemaType.SimpleType v -> v.WriteValue(writer, value)
            | FsSchemaType.ComplexType v -> v.WriteValue(writer, value)
            | FsSchemaType.Option (_, v) ->
                match value with 
                | null -> 
                    writer.WriteAttributeString("nil", W3XMLSchemaInstance, "true")

                | propValue ->
                    writer.WriteAttributeString("nil", W3XMLSchemaInstance, "false")
                    let valueProp = propValue.GetType().GetProperty("Value")
                    let propValue = valueProp.GetValue(propValue)
                    v.WriteValue(writer, propValue)

            | FsSchemaType.MappedType(v) ->
                v.WriteValue(writer, value)

            | FsSchemaType.Recursive (tp, schemaType) ->
                match schemaType with 
                | None -> 
                    let schemaType = writer.RecursiveResolver.Invoke tp
                    let schemaType = schemaType :?> FsSchemaType
                    schemaType.WriteValue(writer, value)

                | Some schemaType ->
                    schemaType.WriteValue(writer, value)



    type NamedFsSchemaType with
        member x.WriteValue(writer: ZippedXmlWriter, value: obj) = 
            match x with 
            | NamedFsSchemaType.ElementName_AND_Tuple2(tupleTp, name) ->
                x.FsSchemaType.WriteValue(writer, value)

            | _ ->
                let schemaTypeOrSchemaTypeName = x.FsSchemaType.GetSchemaTypeOrSchemaTypeName()
                let name = 
                    match x.Name with 
                    | None -> 
                        schemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name
                        |> PropNameOrElementName.ElementName
                    | Some name -> PropNameOrElementName.PropName name

                //writer.WriteStartElement(name.Text)
                //x.FsSchemaType.WriteValue(writer, value)
                //writer.WriteFullEndElement()

                match x.FsSchemaType with 
                | FsSchemaType.Ignore (ignoreInfo) when ignoreInfo.IgnoreInfo_MinOccurs_Zero__OR__DeleteAll -> ()
                | _ -> 
                    writer.WriteStartElement(name.Text)
                    x.FsSchemaType.WriteValue(writer, value)
                    writer.WriteFullEndElement()






    type internal FsXmlSerializer_SerializePart<'T>(configuration: FsXmlSerializerConfiguration) =
        let configuration = configuration
        let encoding = System.Text.Encoding.UTF8
        let tp = typeof<'T>

    
        static member internal SerializeValue(writer: ZippedXmlWriter, propTp: NamedFsSchemaType, propValue: obj) =        
            propTp.WriteValue(writer, propValue)
    

        member x.Serialize(writer: ZippedXmlWriter, value: 'T) =
            let tp = typeof<'T>
            x.WriteW3CAttributeString_xsi_xsd(writer)
            match FSharpType.IsRecord tp with 
            | true ->
                let tp = configuration.GetFsXmlSchemaType(tp)
                tp.WriteValue(writer, value)
                //let props = FSharpType.GetRecordFields tp
                //for prop in props do 
                //    let propValue = prop.GetValue(value)
                //    let propTp =
                //        let fsSchemaType = configuration.GetFsXmlSchemaType(prop.PropertyType)
                //        { Name = Some prop.Name 
                //          FsSchemaType = fsSchemaType }

                //    FsXmlSerializer_SerializePart<_>.SerializeValue(writer, propTp, propValue)
                

            | false -> 
                let tp = configuration.GetFsXmlSchemaType(tp)
                tp.WriteValue(writer, value)


        //member x.SerializeValue(writer: ZippedXmlWriter, value: 'T) =
        //    let tp = typeof<'T>
            

        static member SerializeXmlNodeValue(writer: ZippedXmlWriter, value: obj, configuration: FsXmlSerializerConfiguration, name: string option) =

            let tp = 
                let fsSchemaType = configuration.GetFsXmlSchemaType(value.GetType())
                { FsSchemaType = fsSchemaType  
                  Name = name }

            FsXmlSerializer_SerializePart<_>.SerializeValue(writer, tp, value)
    
        member private x.WriteW3CAttributeString_xsi_xsd(writer: ZippedXmlWriter) =
            writer.WriteAttributeString("xmlns", "xsi", null, W3XMLSchemaInstance)
            writer.WriteAttributeString("xmlns", "xsd", null, W3XMLSchema)


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
                let schemaType = configuration.GetFsXmlSchemaType(tp)

                let zippedWriter =
                    { RecursiveResolver = configuration.RecursiveResolver() 
                      Writer = writer}

                writer.WriteStartElement(schemaType.GetElementName())
                x.Serialize(zippedWriter, value)
                writer.WriteEndElement()


        member private x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath: string, ?xsdSubDirLocation: string) =
            let xsdFileName = Path.GetFileName xsdPath
            let xsdFileName =
                match xsdSubDirLocation with 
                | None -> xsdFileName
                | Some subDir -> subDir.Replace('\\', '/').TrimEnd('/') + "/" + xsdFileName

            let lines = 
                File.ReadAllLines(xmlPath)
                |> Array.mapi(fun i line ->
                    match i with 
                    | 1 -> line.Replace("xmlns:xsi", $"xsi:noNamespaceSchemaLocation=\"{xsdFileName}\" xmlns:xsi")
                    | _ -> line
                )
            File.WriteAllLines(xmlPath, lines)



        member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T, ?xsdSubDirLocation) =
            x.File_WriteXml(xmlPath, value)
            x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath, ?xsdSubDirLocation = xsdSubDirLocation)
