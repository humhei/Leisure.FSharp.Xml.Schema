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
module internal rec _DeserializePart = 
    type ZippedXmlReader =
        { Reader: XmlReader
          RecursiveResolver: RecursiveResolver}
    with 
        member x.Read() = x.Reader.Read()
        member x.Value = x.Reader.Value
        member x.Name = x.Reader.Name
        member x.GetAttribute(name, namespaceURI) = x.Reader.GetAttribute(name, namespaceURI)
        member x.NodeType = x.Reader.NodeType

    let advanceReader(reader: ZippedXmlReader) =
        let reader = reader.Reader
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
       
    let advanceReaderFullElement(reader: ZippedXmlReader, fElement) =
        let reader = reader.Reader
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

    let advanceReaderFullElement_GetElement(reader: ZippedXmlReader, fElement) =
        let mutable value = None

        advanceReaderFullElement(reader, fun stack ->
            let r = fElement stack
            value <- Some r
        )
        |> ignore

        match value with 
        | None -> failwithf "failed Get Nested singleton element from reader"
        | Some v -> v

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


    //    static member Serialize(t: DictionaryType, value: obj, reader:XmlReader, configuration: FsXmlSerializerConfiguration, serializeRecordStatic) =
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
    //            reader.WriteStartElement("Entry")
    //            reader.WriteAttributeString("type", W3XMLSchemaInstance, t.EntryTypeName)
    //            serializeRecordStatic(reader, entry, false, configuration)
    //            reader.WriteEndElement()


    type FsSchemaSimpleType_Enum with 
        member x.ReadValue(reader: ZippedXmlReader) =
            reader.Read() |> ignore 
            let propText = reader.Value
            System.Enum.Parse(x.EnumType, propText)
           

    type FsSchemaSimpleType_Value with 
        member x.ReadValue(reader: ZippedXmlReader) =
            reader.Read() |> ignore 
            let propText = reader.Value
            Convert.ChangeType(propText, x.TypeCode)
      

    type FsSchemaSimpleType with 
        member x.ReadValue(reader: ZippedXmlReader) =
            match x with 
            | FsSchemaSimpleType.EnumType v -> v.ReadValue(reader)
            | FsSchemaSimpleType.ValueType v -> v.ReadValue(reader)

    type FsSchemaComplexType_Collection with 
        member x.ReadValue(reader: ZippedXmlReader) =   
            let tpCode = x.TypeCode

            let elementTp = 
                { Name = None 
                  FsSchemaType = x.ElementSchemaType }

            let elements = ResizeArray()
            advanceReaderFullElement(reader, fun stack ->
                let prop = 
                    FsXmlSerializer_DeserializePart<_>.DeserializeToProp(
                        reader,
                        elementTp
                    )
                elements.Add(prop)
            ) |> ignore
                        
            let elements = tpCode.MakeObject(elements, ?listRestriction = x.ListRestriction)
            elements


    type FsSchemaComplexType_Dictionary with 
        member x.ReadValue(reader: ZippedXmlReader) =     

            let tpCode = x.TypeCode
            let dict = Dictionary<_, _>()

            advanceReaderFullElement(reader, fun stack ->
                advanceReaderFullElement(reader, fun stack ->
                    let key = 
                        let keyTp =
                            { Name = Some "Key"
                              FsSchemaType = x.Entry.KeySchemaType }

                        FsXmlSerializer_DeserializePart<_>.DeserializeToProp(
                            reader,
                            keyTp
                        )

                    advanceReader(reader) |> ignore
                    let value = 
                        let valueTp =
                            { Name = Some "Value"
                              FsSchemaType = x.Entry.ValueSchemaType }

                        FsXmlSerializer_DeserializePart<_>.DeserializeToProp(
                            reader,
                            valueTp
                        )

                    dict.Add(key, value)
                )
                |> ignore
            ) |> ignore
                        
            let r = tpCode.MakeObject(dict)
            r

    type FsSchemaComplexType_Tuple with 
        member x.ReadValue_InElement(reader: ZippedXmlReader) =     
            let tupleElements = ResizeArray()

            advanceReaderFullElement(reader, fun _ ->
                let i = tupleElements.Count
                let (tp) = x.ElementSchemaTypes.[i]
                let value = tp.ReadValue(reader)
                tupleElements.Add(value)
            )
            |> ignore
            //advanceReaderFullElement(reader, fun _ ->

            //)
            //|> ignore

            FSharpValue.MakeTuple(Array.ofSeq tupleElements, x.Tp)

        member x.ReadValue(reader: ZippedXmlReader) =     
            let tupleElements = ResizeArray()

            advanceReaderFullElement(reader, fun _ ->
                advanceReaderFullElement(reader, fun _ ->
                    let i = tupleElements.Count
                    let (tp) = x.ElementSchemaTypes.[i]
                    let value = tp.ReadValue(reader)
                    tupleElements.Add(value)
                )
                |> ignore
            )   
            |> ignore

            FSharpValue.MakeTuple(Array.ofSeq tupleElements, x.Tp)

    type FsSchemaComplexGenericType with 
        member x.ReadValue(reader: ZippedXmlReader) =
            match x with 
            | FsSchemaComplexGenericType.Collection v -> v.ReadValue(reader)
            | FsSchemaComplexGenericType.Dictionary v -> v.ReadValue(reader)
            | FsSchemaComplexGenericType.Tuple v -> v.ReadValue(reader)
                

    type FsSchemaComplexType_Record with 
        member x.ReadValue(reader: ZippedXmlReader) =
            let props = ResizeArray()

            advanceReaderFullElement(reader, fun stack ->
                let zipped = x.Zip3()
                zipped
                |> List.iteri(fun i (element, schemaType, propInfo) ->
                    let namedSchemaType =
                        { Name = Some (propInfo.Name)
                          FsSchemaType = schemaType }

                    let value = namedSchemaType.ReadValue(reader)
                    match i = zipped.Length - 1 with 
                    | true -> ()
                    | false -> 
                        advanceReader(reader)
                        |> ignore
                    //let value = schemaType.ReadValue(reader)
                    props.Add(value)
                )
            )
            |> ignore

            match x.JsonPOCO with 
            | None -> FSharpValue.MakeRecord(x.Type, Array.ofSeq props)
            | Some jsonPOCO ->
                match jsonPOCO.ZippedParameters.Length = props.Count with 
                | true -> jsonPOCO.Constructor.Invoke(Array.ofSeq props)
                | false ->
                    failwithf 
                        "[%A] paramters count mismatch %A" 
                        jsonPOCO.Constructor.DeclaringType 
                        (jsonPOCO.ZippedParameters.Length, props.Count)

    type FsSchemaComplexType_SingleCaseUnion with 
        member x.ReadValue(reader: ZippedXmlReader) =
            let fields_resizeArray = ResizeArray()
            
            match x with 
            | FsSchemaComplexType_SingleCaseUnion.NoField noField ->
                reader.Read() |> ignore 
                let propText = reader.Value

                match propText = noField.Type.Name with 
                | true -> ()
                | false -> failwithf "Invalid token"


            | FsSchemaComplexType_SingleCaseUnion.OneField field ->
                match field.Element.SimpleTypeInfo with 
                | None -> 
                    advanceReaderFullElement(reader, fun stack ->
                        let uciValue = 
                            field.ElementSchemaType.ReadValue(reader)
                        
                        fields_resizeArray.Add(uciValue)
                    )
                    |> ignore

                | Some simpleTypeInfo ->
                    let uciValue = 
                        field.ElementSchemaType.ReadValue(reader)

                    fields_resizeArray.Add(uciValue)
                    |> ignore

            | FsSchemaComplexType_SingleCaseUnion.MultipleFields fields ->
                advanceReaderFullElement(reader, fun stack ->
                    let uciValues = 
                        fields.Zip3()
                        |> List.map(fun (element, schemaType, propInfo) ->
                            advanceReader(reader) |> ignore
                            let r = 
                                schemaType.ReadValue(reader)
                            r
                        )

                    advanceReader(reader) |> ignore
                    fields_resizeArray.AddRange(uciValues)
                )
                |> ignore
                

            let fields = Array.ofSeq fields_resizeArray
            let r = FSharpValue.MakeUnion(x.Uci(), fields, allowAccessToPrivateRepresentation = true)
            r

    type FsSchemaComplexType_Union with 
        member x.ReadValue(reader: ZippedXmlReader) =
            let mutable caseValue = None
            let advanceReaderFullElement_concated(reader, valueFactory) =
                match x.TagOptions with 
                | UnionTagOptions.Independent ->
                    advanceReaderFullElement(reader, fun _ ->
                        advanceReaderFullElement(reader, fun _ ->
                            valueFactory()
                        )
                        |> ignore
                    )
                    |> ignore

                | UnionTagOptions.SuffixToRootType ->
                    advanceReaderFullElement(reader, fun _ ->
                        valueFactory()
                    )
                    |> ignore


            advanceReaderFullElement_concated(reader, fun _ ->
                let name = reader.Name
                let uci = 
                    x.UnionCases
                    |> List.find(fun m -> m.UnionCase.Name = name)


                let value = 
                    match uci with 
                    | FsSchemaComplexType_UnionCase.NamedCase uci -> FSharpValue.MakeUnion(uci.UnionCase, [||])
                    | FsSchemaComplexType_UnionCase.OneFieldCase uci ->
                        let value = 
                            uci.ElementSchemaType.ReadValue(reader)

                        FSharpValue.MakeUnion(uci.UnionCase, [|value|])

                    | FsSchemaComplexType_UnionCase.MultipleFieldsCase fields ->

                        let elements = ResizeArray()
                        advanceReaderFullElement(reader, fun _ ->
                            let name = reader.Name
                            let _, schemaTp, _ = 
                                fields.Zip3()
                                |> List.find(fun (_, _, m) -> m.Name = name)

                            let element = 
                                schemaTp.ReadValue(reader)
                            elements.Add(element)
                        )
                        |> ignore

                        FSharpValue.MakeUnion(uci.UnionCase, Array.ofSeq elements)

                caseValue <- Some value
            )
            |> ignore

            caseValue.Value

    type FsSchemaComplexType with 
        member x.ReadValue(reader: ZippedXmlReader) =
            match x with 
            | FsSchemaComplexType.Generic v -> v.ReadValue(reader)
            | FsSchemaComplexType.Record v -> v.ReadValue(reader)
            | FsSchemaComplexType.SinglecaseUnion v -> v.ReadValue(reader)
            | FsSchemaComplexType.Union v -> v.ReadValue(reader)

    type MappedFsSchemaType with 
        member x.ReadValue(reader: ZippedXmlReader) =
            match x.WrapOldName with 
            | false -> 
                let value = x.FsSchemaType.ReadValue(reader)
                x.TypeMappingPair.OfXmlSerializable value

            | true -> 
                //let namedSchemaType = 
                //    { Name = Some x.OriginType.Name 
                //      FsSchemaType = x.FsSchemaType }

                //let value = namedSchemaType.ReadValue(reader)

                let value =
                    advanceReaderFullElement_GetElement(reader, fun _ ->
                        x.FsSchemaType.ReadValue(reader)
                    )


                let r = 
                    x.TypeMappingPair.OfXmlSerializable value

                r


    type FsSchemaType with 
        member x.ReadValue(reader: ZippedXmlReader) =
            match x with 
            | FsSchemaType.SimpleType v -> v.ReadValue(reader)
            | FsSchemaType.ComplexType v -> v.ReadValue(reader)
            | FsSchemaType.Option v -> 
                let isNull =
                    let attr = reader.GetAttribute("nil", W3XMLSchemaInstance)
                    match attr with 
                    | null -> false
                    | attr -> System.Boolean.Parse attr

                match isNull with
                | true ->
                    reader.Read() |> ignore
                    null

                | false -> 
                    let elementValue = v.ReadValue(reader)
                    let elementType = elementValue.GetType()
                    makeOption(elementType, elementValue)
                //failwithf ""
                //let elementType = 
                //    { Name = None }
                //    v
                //let elementValue = 
                //    FsXmlSerializer_DeserializePart<_>.DeserializeToProp(
                //        reader,
                //        SCasablePropertyType.NillableNamedType(elementType.Name, elementType),
                //        configuration)
                //let r = makeOption(propTp, elementType, elementValue)
                //r

            | FsSchemaType.MappedType (v) ->
                let value = v.ReadValue(reader)
                value

            | FsSchemaType.Recursive (tp, schemaType) ->
                match schemaType with 
                | None -> 
                    let schemaType = reader.RecursiveResolver.Invoke tp
                    let schemaType = schemaType :?> FsSchemaType
                    schemaType.ReadValue(reader)

                | Some schemaType ->
                    schemaType.ReadValue(reader)

            | FsSchemaType.Ignore ignoreInfo ->
                reader.Read() |> ignore
                null
                //let r = createDefaultObject(ignoreInfo.PropertyType)
                //failwith ""
            //| _ -> failwithf "Not implemented"


    type NamedFsSchemaType with
        member x.ReadValue(reader: ZippedXmlReader) = 
            let nodeType = reader.NodeType
            match nodeType with 
            | XmlNodeType.Element ->
                match x with 
                | NamedFsSchemaType.ElementName_AND_Tuple2(tupleTp, name) ->
                    tupleTp.ReadValue_InElement(reader)

                | _ ->
                    let r = x.FsSchemaType.ReadValue(reader)
                    r

            | _ -> failwithf "Not implemented"




    type internal FsXmlSerializer_DeserializePart<'T>(configuration: FsXmlSerializerConfiguration) =
        let configuration = configuration
        let encoding = System.Text.Encoding.UTF8
        let tp = typeof<'T>
        //let __CheckTypeValid =
        //    match FSharpType.IsRecord tp with 
        //    | true -> ()
        //    | false -> failwithf "Root type should be fsharp record"

        //let props = FSharpType.GetRecordFields tp

        static member internal DeserializeToProp(reader: ZippedXmlReader, propTp: NamedFsSchemaType) =
            propTp.ReadValue(reader)

        static member DeserializeXmlNodeValueTo(reader: ZippedXmlReader, tp: Type, configuration: FsXmlSerializerConfiguration, name: string option) =

            let tp = 
                let fsSchemaType = configuration.GetFsXmlSchemaType(tp)
                { FsSchemaType = fsSchemaType 
                  Name = name }

            FsXmlSerializer_DeserializePart<_>.DeserializeToProp(reader, tp)

        member x.Deserialize(reader: ZippedXmlReader): 'T =
            let tp = typeof<'T>
            //let __CheckTypeValid =
            //    match FSharpType.IsRecord tp with 
            //    | true -> ()
            //    | false -> failwithf "Root type should be fsharp record"


            let schemaTp = configuration.GetFsXmlSchemaType(tp)
            match schemaTp.IsRecordEx() with 
            | true -> advanceReader(reader) |> ignore
            | false -> ()

            schemaTp.ReadValue(reader)
            |> unbox<'T>

            //match FSharpType.IsRecord tp with 
            //| true ->
            //    let props = FSharpType.GetRecordFields tp

            //    let props = props
            //    let rec loop accum =
            //        match advanceReader(reader) with 
            //        | true -> 
            //            match reader.NodeType with 
            //            | XmlNodeType.Element -> 
            //                let prop = 
            //                    props
            //                    |> Array.find(fun m -> m.Name = reader.Name)

            //                let propTp = 
            //                    { Name = Some prop.Name 
            //                      FsSchemaType = configuration.GetFsXmlSchemaType(prop.PropertyType) }

            //                let propValue = FsXmlSerializer_DeserializePart<_>.DeserializeToProp(reader, propTp)
            //                loop (propValue :: accum) 

            //            | _ -> failwithf "Not implemented"

            //        | false -> List.rev accum

            //    advanceReader(reader) |> ignore
            //    match reader.Name = tp.Name || reader.Name + "_XMLSchema" = tp.Name with 
            //    | true -> 
            //        reader.Read() |> ignore
            //        |> ignore

            //    | false -> failwithf "Invalid token, reader.Name %s should be record type name %s here" reader.Name tp.Name


            //    let props = loop []
            //    FSharpValue.MakeRecord(tp, List.toArray props)
            //    |> unbox<'T>

            //| false ->
            //    let schemaTp = configuration.GetFsXmlSchemaType(tp)
            //    match schemaTp.IsRecordEx() with 
            //    | true -> advanceReader(reader) |> ignore
            //    | false -> ()

            //    schemaTp.ReadValue(reader)
            //    |> unbox<'T>

        member x.DeserializeFromFile(fileName: string) =
            
            use reader = new FileStream(fileName, FileMode.Open)
            match getReadXmlObjMethod tp with 
            | None ->
                let reader = XmlReader.Create(reader)
                let zippedReader = 
                    { RecursiveResolver = configuration.RecursiveResolver() 
                      Reader = reader }
                x.Deserialize(zippedReader)
                //let serializer = new XmlSerializer(tp)
                //let r = serializer.Deserialize(reader);
                //r :?> 'T

            | Some (defaultObj, method) ->
                let reader = XmlReader.Create(reader)
                let r = method.Invoke(defaultObj, [|tp; reader; configuration|])
                r :?> 'T
        




