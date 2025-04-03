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


//type private MapSerializer<'k,'v when 'k : comparison>() =
//    static member Deserialize(t:DictionaryType, dict: Dictionary<obj, obj>) =
//        match t with 
//        | DictionaryType.Dictionary _ ->    
//            let newDict = Dictionary<'k, 'v>()
//            for pair in dict do 
//                newDict.Add(pair.Key :?> 'k, pair.Value :?> 'v)

//            box newDict

//        | DictionaryType.ConcurrrentDictionary _ ->
//            let newDict = ConcurrentDictionary<'k, 'v>()
//            for pair in dict do 
//                newDict.TryAdd(pair.Key :?> 'k, pair.Value :?> 'v)
//                |> ignore

//            box newDict

//        | DictionaryType.FSharpMap _ ->
//            let tupleLists = 
//                dict
//                |> Seq.map(fun pair ->
//                    pair.Key :?> 'k, pair.Value :?> 'v
//                )

//            let newDict = Map.ofSeq tupleLists
//            box newDict


//    static member Serialize(t: DictionaryType, value: obj, writer:XmlWriter, configuration: FsXmlSerializerConfiguration, serializeRecordStatic) =
//        let tupleList =
//            match t with 
//            | DictionaryType.FSharpMap _ -> 
//                value :?> Map<'k, 'v>
//                |> Map.toList

//            | DictionaryType.Dictionary _ ->
//                value :?> Dictionary<'k, 'v>
//                |> Seq.toList
//                |> List.map(fun m -> m.Key, m.Value)

//            | DictionaryType.ConcurrrentDictionary _ ->
//                value :?> ConcurrentDictionary<'k, 'v>
//                |> Seq.toList
//                |> List.map(fun m -> m.Key, m.Value)


//        for (key, value) in tupleList do
//            let entry = Entry.createByObjects(key, value) t
//            writer.WriteStartElement("Entry")
//            writer.WriteAttributeString("type", W3XMLSchemaInstance, t.EntryTypeName)
//            serializeRecordStatic(writer, entry, false, configuration)
//            writer.WriteEndElement()




type FsXmlSerializer<'T>(configuration: FsXmlSerializerConfiguration) =
    let configuration = configuration
    do configuration.FsIXmlSerializableTypeMappingCache.Clear()
    let encoding = System.Text.Encoding.UTF8
    let tp = typeof<'T>
    //let __CheckTypeValid =
    //    match FSharpType.IsRecord tp with 
    //    | true -> ()
    //    | false -> failwithf "Root type should be fsharp record"

    let serializer_part = FsXmlSerializer_SerializePart<'T>(configuration)
    let deserializer_part = FsXmlSerializer_DeserializePart<'T>(configuration)

    //let props = FSharpType.GetRecordFields tp



     
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








    //static member DeserializeToRecordStatic(reader: XmlReader, tp: Type, configuration) =
            
    //    let props = FSharpType.GetRecordFields tp

    //    let rec loop accum =
    //        match advanceReader(reader) with 
    //        | true ->
    //            match reader.NodeType with 
    //            | XmlNodeType.Element -> 
    //                let prop = 
    //                    props
    //                    |> Array.find(fun m -> m.Name = reader.Name)

    //                let propValue = 
    //                    FsXmlSerializer<_>.DeserializeToProp(
    //                        reader,
    //                        SCasablePropertyType.PropertyInfo prop,
    //                        configuration
    //                    )
    //                loop (propValue :: accum) 

    //            | _ -> failwithf "Not implemented"

    //        | false -> List.rev accum

    //    let props = loop []
    //    FSharpValue.MakeRecord(tp, List.toArray props)
    

    static member DeserializeXmlNodeValueTo(reader: XmlReader, tp: Type, configuration: FsXmlSerializerConfiguration, name) =
        let zippedReader = 
            { RecursiveResolver = configuration.RecursiveResolver() 
              Reader = reader }

        FsXmlSerializer_DeserializePart<'T>.DeserializeXmlNodeValueTo(zippedReader, tp, configuration, name)

    member x.DeserializeFromFile(fileName: string) =
        deserializer_part.DeserializeFromFile(fileName)

    member x.Deserialize(reader: XmlReader): 'T =
        let zippedReader = 
            { RecursiveResolver = configuration.RecursiveResolver() 
              Reader = reader }

        deserializer_part.Deserialize(zippedReader)

    member x.Serialize(writer: XmlWriter, value: 'T) =
        let zippedWriter =
            { RecursiveResolver = configuration.RecursiveResolver() 
              Writer = writer}

        serializer_part.Serialize(zippedWriter, value)

    member private x.File_TrimXsdSchemaEnd(xsdPath) =
        let lines = 
            File.ReadAllLines(xsdPath)
            |> Array.mapi(fun i line ->
                line.Replace("_XMLSchema\"", $"\"")
            )
        File.WriteAllLines(xsdPath, lines)

    member private x.File_WriteFsXsd(xsdPath: string) =
        use tw = new StreamWriter(xsdPath)
        let importer = new FsSchemaImporter(configuration);
        let props = tp.GetProperties()
        let tp = 
            props
            |> Array.tryFind(fun m -> m.Name = "XMLSchema")
            |> function
                | None -> tp
                | Some schema ->
                    schema.PropertyType
        
        let schemas = importer.ImportTp(tp)
        schemas.Write(tw)


    member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T) =
        x.File_WriteFsXsd(xsdPath)
        x.File_TrimXsdSchemaEnd(xsdPath)
        serializer_part.SerializeToFile(xmlPath, xsdPath, value)
