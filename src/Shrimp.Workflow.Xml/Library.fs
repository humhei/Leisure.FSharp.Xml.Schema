// Learn more about F# at http://fsharp.org
namespace Shrimp.Workflow.Xml
#nowarn "0104"
open MBrace.FsPickler
open System.Xml.Serialization
open System
open System.IO
open Microsoft.FSharp.Reflection
open System.Xml
open Fake.IO


type FsXmlSerializer<'T>() =
    let encoding = System.Text.Encoding.UTF8
    let tp = typeof<'T>
    let __CheckTypeValid =
        match FSharpType.IsRecord tp with 
        | true -> ()
        | false -> failwithf "Root type should be fsharp record"

    let props = FSharpType.GetRecordFields tp


    member private x.WriteXsd(xsdPath: string) =
        use tw = new StreamWriter(xsdPath)
        let importer = new XmlReflectionImporter();
        let schemas = new XmlSchemas();
        let exporter = new XmlSchemaExporter(schemas);
        let map = importer.ImportTypeMapping(tp);
        exporter.ExportTypeMapping(map);
        
        schemas[0].Write(tw);

    member private x.WriteXml(xmlPath: string, value: 'T) =
        use sww = new StreamWriter(xmlPath)
        let xsSubmit = new XmlSerializer(typeof<'T>);

        use writer = 
            XmlWriter.Create(
                sww,
                XmlWriterSettings(Indent = true, Encoding = encoding)
            )

        xsSubmit.Serialize(writer, value);


    //member private x.WriteXml_FsPickler(xmlPath: string, value: 'T) =
    //    let sww = new StreamWriter(xmlPath)
    //    let xsSubmit = FsPickler.CreateXmlSerializer(indent = true)

    //    xsSubmit.Serialize(sww, value)

    member private x.WriteXmlNamespace(xmlPath, xsdPath) =
        let xsdFileName = Path.GetFileName xsdPath
        let lines = 
            File.ReadAllLines(xmlPath)
            |> Array.mapi(fun i line ->
                match i with 
                | 1 -> line.Replace("xmlns:xsi", $"xsi:noNamespaceSchemaLocation=\"{xsdFileName}\" xmlns:xsi")
                | _ -> line
            )
        File.WriteAllLines(xmlPath, lines)
     

    member x.SerializeValue(writer: XmlWriter, value: 'T) =
        match FSharpType.IsRecord tp with 
        | true ->
            for prop in props do 
                let propTp = prop.PropertyType
                let propValue = prop.GetValue(value)
                match Type.GetTypeCode(propTp) with 
                | TypeCode.String 
                | TypeCode.Single
                | TypeCode.Boolean
                | TypeCode.Char
                | TypeCode.DateTime
                | TypeCode.DBNull
                | TypeCode.Decimal
                | TypeCode.Double
                | TypeCode.UInt16
                | TypeCode.UInt32
                | TypeCode.UInt64
                | TypeCode.Int16
                | TypeCode.Int32
                | TypeCode.Int64
                | TypeCode.Empty
                | TypeCode.Byte
                | TypeCode.SByte ->
                    writer.WriteElementString(prop.Name, propValue.ToString())

                | TypeCode.Object ->
                    match propValue with 
                    | :? IXmlSerializable as xmlSerilizable -> xmlSerilizable.WriteXml(writer)
                    | _ -> 
                        failwithf "[Xml serializable] Un supported type %A" tp
                

        | false -> failwithf "Not implemented"

    member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T) =
        x.WriteXml(xmlPath, value)
        x.WriteXsd(xsdPath)
        x.WriteXmlNamespace(xmlPath, xsdPath)


    member x.Deserialize(fileName: string) =
        let serializer = new XmlSerializer(typeof<'T>);
        use reader = new FileStream(fileName, FileMode.Open)
        let r = serializer.Deserialize(reader);
        r :?> 'T

    
    member x.DeserializeToValue(reader: XmlReader, value: 'T) =
        while (reader.Read()) do
            let nodeType = reader.NodeType
            match nodeType with 
            | XmlNodeType.Element -> 
                let prop = 
                    props
                    |> Array.find(fun m -> m.Name = reader.Name)

                let propTp = (prop.PropertyType)
                reader.Read() |> ignore
                let propText = reader.Value

                match Type.GetTypeCode(propTp) with 
                | TypeCode.String 
                | TypeCode.Single 
                | TypeCode.Boolean
                | TypeCode.Char
                | TypeCode.DateTime
                | TypeCode.DBNull
                | TypeCode.Decimal
                | TypeCode.Double
                | TypeCode.UInt16
                | TypeCode.UInt32
                | TypeCode.UInt64
                | TypeCode.Int16
                | TypeCode.Int32
                | TypeCode.Int64
                | TypeCode.Empty
                | TypeCode.Byte
                | TypeCode.SByte ->  
                    let propValue = Convert.ChangeType(propText, propTp)
                    prop.SetValue(value, propValue)

                | TypeCode.Object ->
                    failwithf "Not implemented"

            | XmlNodeType.EndElement -> ()


    member x.DeserialzeToValue_WithRoot(reader: XmlReader, value: 'T, valueToRootValue: 'T -> 'Root, root: 'Root) =
        x.DeserializeToValue(reader, value)
        let rootValue = valueToRootValue value
        let rootTp = typeof<'Root>
        match FSharpType.IsRecord rootTp with 
        | true ->
            for prop in FSharpType.GetRecordFields rootTp do
                prop.SetValue(root, prop.GetValue(rootValue))


        | false -> failwithf "Not implemented"