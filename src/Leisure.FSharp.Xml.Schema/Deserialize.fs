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
        member x.ReadValue(reader: XmlReader) =
            reader.Read() |> ignore 
            let propText = reader.Value
            System.Enum.Parse(x.EnumType, propText)
           

    type FsSchemaSimpleType_Value with 
        member x.ReadValue(reader: XmlReader) =
            reader.Read() |> ignore 
            let propText = reader.Value
            Convert.ChangeType(propText, x.TypeCode)
      

    type FsSchemaSimpleType with 
        member x.ReadValue(reader: XmlReader) =
            match x with 
            | FsSchemaSimpleType.EnumType v -> v.ReadValue(reader)
            | FsSchemaSimpleType.ValueType v -> v.ReadValue(reader)

    type FsSchemaComplexType_Collection with 
        member x.ReadValue(reader: XmlReader) =   
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
                        
            let elements = tpCode.MakeObject(elements)
            elements


    type FsSchemaComplexType_Dictionary with 
        member x.ReadValue(reader: XmlReader) =     

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
        member x.ReadValue_InElement(reader: XmlReader) =     
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

        member x.ReadValue(reader: XmlReader) =     
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
        member x.ReadValue(reader: XmlReader) =
            match x with 
            | FsSchemaComplexGenericType.Collection v -> v.ReadValue(reader)
            | FsSchemaComplexGenericType.Dictionary v -> v.ReadValue(reader)
            | FsSchemaComplexGenericType.Tuple v -> v.ReadValue(reader)
                
                
            | _ -> failwithf "Not implemented"

    type FsSchemaComplexType_Record with 
        member x.ReadValue(reader: XmlReader) =
            let props = ResizeArray()

            advanceReaderFullElement(reader, fun stack ->
                x.Zip3()
                |> List.iter(fun (element, schemaType, propInfo) ->
                    let namedSchemaType =
                        { Name = Some (propInfo.Name)
                          FsSchemaType = schemaType }

                    let value = namedSchemaType.ReadValue(reader)

                    //let value = schemaType.ReadValue(reader)
                    props.Add(value)
                )
            )
            |> ignore

            FSharpValue.MakeRecord(x.Type, Array.ofSeq props)

    type FsSchemaComplexType_SingleCaseUnion with 
        member x.ReadValue(reader: XmlReader) =
            let elements = ResizeArray()
            
            match x with 
            | FsSchemaComplexType_SingleCaseUnion.OneField field ->
                advanceReaderFullElement(reader, fun stack ->
                    let uciValue = 
                        field.ElementSchemaType.ReadValue(reader)
                        
                    elements.Add(uciValue)
                )
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
                    elements.AddRange(uciValues)
                )
                |> ignore
                

            let fields = Array.ofSeq elements
            let r = FSharpValue.MakeUnion(x.Uci(), fields, allowAccessToPrivateRepresentation = true)
            r

    type FsSchemaComplexType_Union with 
        member x.ReadValue(reader: XmlReader) =
            let mutable caseValue = None
            advanceReaderFullElement(reader, fun _ ->
                advanceReaderFullElement(reader, fun _ ->
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
            )
            |> ignore

            caseValue.Value

    type FsSchemaComplexType with 
        member x.ReadValue(reader: XmlReader) =
            match x with 
            | FsSchemaComplexType.Generic v -> v.ReadValue(reader)
            | FsSchemaComplexType.Record v -> v.ReadValue(reader)
            | FsSchemaComplexType.SinglecaseUnion v -> v.ReadValue(reader)
            | FsSchemaComplexType.Union v -> v.ReadValue(reader)

            | _ -> failwithf "Not implemented"

    type MappedFsSchemaType with 
        member x.ReadValue(reader: XmlReader) =
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
        member x.ReadValue(reader: XmlReader) =
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

            | _ -> failwithf "Not implemented"


    type NamedFsSchemaType with
        member x.ReadValue(reader: XmlReader) = 
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
        let __CheckTypeValid =
            match FSharpType.IsRecord tp with 
            | true -> ()
            | false -> failwithf "Root type should be fsharp record"

        let props = FSharpType.GetRecordFields tp

        static member internal DeserializeToProp(reader: XmlReader, propTp: NamedFsSchemaType) =
            propTp.ReadValue(reader)
            //    let nodeType = reader.NodeType
            //    let propMappingOp = configuration.UpdateSCasablePropertyType_ToXml_Op prop

            //    let prop0 =
            //        match propMappingOp with 
            //        | None -> prop
            //        | Some propMapping ->
            //            match propMapping.TypeMapping.WrapOldName with
            //            | true -> prop
            //            | false -> propMapping.PropertyType

            //    let prop =
            //        match propMappingOp with 
            //        | None -> configuration.UpdateSCasablePropertyType_ToXml__NoUpdateForWrappedTypeName prop
            //        | Some propTypeMapping -> prop0

            //    let value = 
            //        match nodeType with 
            //        | XmlNodeType.Element -> 
            //            let tpCode = 
            //                let propTp =  (prop.PropertyType) 
            //                getFsTpCodeEx(propTp)

            //            let propTp = prop0.PropertyType

            //            let createNullable(text: string, fNull, f) =
            //                match text, prop.Nillable with 
            //                | "", true -> 
            //                    fNull()
            //                    null
            //                | _ -> f()

            //            match tpCode with 
            //            | FsTypeCodeEx.Tuple (_) -> 
            //                let tpCodes = FSharpType.GetTupleElements propTp

            //                let inCollection = defaultArg inCollection false
            //                let advanceReader_IgnorePropInCollection(reader, f) =
            //                    match inCollection with 
            //                    | false -> 
            //                        advanceReaderFullElement(reader, fun _ ->
            //                            advanceReaderFullElement(reader, fun _ ->
            //                                f()
            //                            )
            //                            |> ignore
            //                        )
            //                        |> ignore

            //                    | true ->
            //                        advanceReaderFullElement(reader, fun _ ->
            //                            f()
            //                        )
            //                        |> ignore

            //                let tupleElements = ResizeArray()

            //                advanceReader_IgnorePropInCollection(reader, fun () ->
            //                    let i = tupleElements.Count
            //                    let (tp) = tpCodes.[i]
            //                    let name = itemText i
            //                    let element = FsXmlSerializer<_>.DeserializeToProp(reader, SCasablePropertyType.NamedType(name, tp), configuration)
            //                    tupleElements.Add(element)
            //                )
                            
            //                match prop.Nillable, tupleElements.Count with 
            //                | true, 0 -> 
            //                    null
            //                | _ ->
            //                    let tuple = FSharpValue.MakeTuple(Array.ofSeq tupleElements, propTp)
            //                    tuple

                        
            //            | FsTypeCodeEx.Option (_, _) ->
            //                let elementType = propTp.GetGenericArguments().[0]
            //                let elementValue = 
            //                    FsXmlSerializer<_>.DeserializeToProp(
            //                        reader,
            //                        SCasablePropertyType.NillableNamedType(elementType.Name, elementType),
            //                        configuration)
            //                let r = makeOption(propTp, elementType, elementValue)
            //                r

            //            | FsTypeCodeEx.DictionaryType tpCode ->


            //            | FsTypeCodeEx.CollectionType _ -> 
            //                let tpCode = 
            //                    match getFsTpCodeEx propTp with 
            //                    | FsTypeCodeEx.CollectionType tpCode -> tpCode
            //                    | _ -> failwithf "Invalid token"

            //                let elementTp = tpCode.ElementType
            //                let elements = ResizeArray()
            //                advanceReaderFullElement(reader, fun stack ->
            //                    let prop = 
            //                        FsXmlSerializer<_>.DeserializeToProp(
            //                            reader,
            //                            SCasablePropertyType.Type elementTp,
            //                            configuration,
            //                            inCollection = true)
            //                    elements.Add(prop)
            //                    ()
            //                ) |> ignore
                        
            //                let elements = tpCode.MakeObject(elements)
            //                elements


            //            | FsTypeCodeEx.FsTypeCode _ ->
            //                let tpCode = getFsTpCode propTp


                                
                        
            //                match tpCode with 
            //                | FsTypeCode.Enum ->
            //                    reader.Read() |> ignore 
            //                    let propText = reader.Value
            //                    createNullable(propText, ignore, fun _ ->
            //                        System.Enum.Parse(propTp, propText)
            //                    )

            //                | FsTypeCode.ValueType _ -> 
            //                    reader.Read() |> ignore 
            //                    let propText = reader.Value
            //                    createNullable(propText, ignore, fun _ ->
            //                        Convert.ChangeType(propText, propTp)
            //                    )


            //                | FsTypeCode.Object tpCode -> 
            //                    match getReadXmlObjMethod propTp with 
            //                    | Some (defaultObj, methodInfo) ->
            //                        let r = methodInfo.Invoke(defaultObj, [|propTp; reader; configuration|])
            //                        r

            //                    | None -> 
            //                        match tpCode with 
            //                        | FsObjectTypeCode.FsXmlSerializableTypeMapping  -> 
            //                            /// already predicate by previous line (match getReadXmlObjMethod propTp with)
            //                            //let r = FsXmlSerializer<_>.DeserializeToProp(reader, prop0, configuration)
            //                            match propMappingOp with 
            //                            | None ->  failwith "Invalid token"
            //                            | Some typeMapping ->
            //                                match typeMapping.TypeMapping.WrapOldName with 
            //                                | true -> 
            //                                    let targetTypeCode = getFsTpCodeEx typeMapping.TypeMapping.TargetType
            //                                    let advanceReader_wrapOldName(reader, f) =
            //                                        match targetTypeCode with 
            //                                        | FsTypeCodeEx.FsTypeCode (FsTypeCode.Enum)
            //                                        | FsTypeCodeEx.FsTypeCode (FsTypeCode.ValueType _) -> f 0
            //                                        | _ -> 
            //                                            advanceReaderFullElement(reader, f)
            //                                            |> ignore

            //                                    let readInnerValue() =
            //                                        let mutable valueMutable = null
            //                                        advanceReader_wrapOldName(reader, fun _ ->
            //                                            let tpName = reader.Name
            //                                            let prop = 
            //                                                SCasablePropertyType.NamedType(tpName, typeMapping.TypeMapping.TargetType)

            //                                                //match prop.Nillable with 
            //                                                //| true -> 
            //                                                //    SCasablePropertyType.NillableNamedType(tpName, typeMapping.TypeMapping.TargetType)

            //                                                //| false -> SCasablePropertyType.NamedType(tpName, typeMapping.TypeMapping.TargetType)

            //                                            let value = 
            //                                                FsXmlSerializer<_>.DeserializeToProp(
            //                                                    reader,
            //                                                    prop,
            //                                                    configuration
            //                                                )

            //                                            valueMutable <- value
            //                                        )

            //                                        |> ignore

            //                                        valueMutable
                                                
            //                                    match prop.Nillable with 
            //                                    | true -> 
            //                                        let isNull = 
            //                                            let attr = reader.GetAttribute("nil", W3XMLSchemaInstance)
            //                                            match attr with 
            //                                            | null -> false
            //                                            | attr -> System.Boolean.Parse attr

            //                                        match isNull with 
            //                                        | true -> 
            //                                            reader.Read() |> ignore
            //                                            null
                                                    
            //                                        | false -> readInnerValue()

            //                                    | false -> readInnerValue()

            //                                | false -> failwithf "Invalid token"

            //                        | FsObjectTypeCode.Record ->
            //                            FsXmlSerializer<_>.DeserializeToRecordStatic(reader, propTp, configuration)
                                    
            //                        | FsObjectTypeCode.SingletonCaseUnion uci ->  
            //                            let elements = ResizeArray()

            //                            advanceReaderFullElement(reader, fun _ ->
            //                                let fields = uci.GetFields() 
            //                                match fields with 
            //                                | [||] -> failwithf "Not implemented"
            //                                | [|field|] ->
            //                                    let uciValue = 
            //                                        FsXmlSerializer<_>.DeserializeToProp(
            //                                            reader,
            //                                            SCasablePropertyType.OneFieldSCase field.PropertyType,
            //                                            configuration)
            //                                    elements.Add(uciValue)

            //                                | fields ->
            //                                    let uciValues = 
            //                                        fields
            //                                        |> Array.map(fun field ->
            //                                            advanceReader(reader) |> ignore
            //                                            let r = 
            //                                                FsXmlSerializer<_>.DeserializeToProp(
            //                                                    reader,
            //                                                    SCasablePropertyType.OneFieldSCase field.PropertyType,
            //                                                    configuration
            //                                                )
            //                                            r
            //                                        )
            //                                    advanceReader(reader) |> ignore
            //                                    elements.AddRange(uciValues)
            //                            )
            //                            |> ignore

            //                            let fields = Array.ofSeq elements
            //                            let r = FSharpValue.MakeUnion(uci, fields, allowAccessToPrivateRepresentation = true)
            //                            r

            //                        | FsObjectTypeCode.Union cases -> 
            //                            let mutable caseValue = None
            //                            advanceReaderFullElement(reader, fun _ ->
            //                                advanceReaderFullElement(reader, fun _ ->
            //                                    let name = reader.Name
            //                                    let uci = 
            //                                        cases
            //                                        |> Array.find(fun m -> m.Name = name)

            //                                    let fields = uci.GetFields()
            //                                    let value = 
            //                                        match fields with 
            //                                        | [||] -> FSharpValue.MakeUnion(uci, [||])
            //                                        | [|field|] ->
            //                                            let value = 
            //                                                FsXmlSerializer<_>.DeserializeToProp(   
            //                                                    reader,
            //                                                    SCasablePropertyType.NamedType(uci.Name, field.PropertyType),
            //                                                    configuration
            //                                                )

            //                                            FSharpValue.MakeUnion(uci, [|value|])

            //                                        | fields ->
            //                                            let elements = ResizeArray()
            //                                            advanceReaderFullElement(reader, fun _ ->
            //                                                let name = reader.Name
            //                                                let field = 
            //                                                    fields
            //                                                    |> Array.find(fun m -> m.Name = name)

            //                                                let element = 
            //                                                    FsXmlSerializer<_>.DeserializeToProp(
            //                                                        reader,
            //                                                        SCasablePropertyType.PropertyInfo(field),
            //                                                        configuration
            //                                                    )

            //                                                elements.Add(element)
            //                                            )
            //                                            |> ignore
            //                                            //let value = 
            //                                            //    FsXmlSerializer<_>.DeserializeToProp(reader, SCasablePropertyType.NamedType(uci.Name, field.PropertyType))
            //                                            FSharpValue.MakeUnion(uci, Array.ofSeq elements)

            //                                    caseValue <- Some value
            //                                )
            //                                |> ignore
            //                            )
            //                            |> ignore

            //                            caseValue.Value


            //        | _ -> failwithf "Not implemented"
                            

            //    match propMappingOp with 
            //    | None -> value
            //    | Some propMappingOp -> 

            //        match prop.Nillable, value with 
            //        | true, null -> null
            //        | false, null -> failwithf "%A Value cannot be null" prop
            //        | _ -> propMappingOp.TypeMapping.OfXmlSerializable value

        static member DeserializeXmlNodeValueTo(reader: XmlReader, tp: Type, configuration: FsXmlSerializerConfiguration, name: string option) =
            let tp = 
                let fsSchemaType = configuration.GetFsXmlSchemaType(tp)
                { FsSchemaType = fsSchemaType 
                  Name = name }

            FsXmlSerializer_DeserializePart<_>.DeserializeToProp(reader, tp)

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

                        let propTp = 
                            { Name = Some prop.Name 
                              FsSchemaType = configuration.GetFsXmlSchemaType(prop.PropertyType) }

                        let propValue = FsXmlSerializer_DeserializePart<_>.DeserializeToProp(reader, propTp)
                        loop (propValue :: accum) 

                    | _ -> failwithf "Not implemented"

                | false -> List.rev accum

            advanceReader(reader) |> ignore
            match reader.Name = tp.Name || reader.Name + "_XMLSchema" = tp.Name with 
            | true -> 
                reader.Read() |> ignore
                |> ignore

            | false -> failwithf "Invalid token, reader.Name %s should be record type name %s here" reader.Name tp.Name


            let props = loop []
            FSharpValue.MakeRecord(tp, List.toArray props)
            |> unbox<'T>

        member x.DeserializeFromFile(fileName: string) =
            
            use reader = new FileStream(fileName, FileMode.Open)
            match getReadXmlObjMethod tp with 
            | None ->
                let reader = XmlReader.Create(reader)
                x.DeserializeToRecord(reader)
                //let serializer = new XmlSerializer(tp)
                //let r = serializer.Deserialize(reader);
                //r :?> 'T

            | Some (defaultObj, method) ->
                let reader = XmlReader.Create(reader)
                let r = method.Invoke(defaultObj, [|tp; reader; configuration|])
                r :?> 'T
        




