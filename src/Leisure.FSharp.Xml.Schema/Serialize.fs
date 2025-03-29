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


        static member Serialize(t: FsSchemaComplexType_Dictionary, value: obj, writer:XmlWriter) =
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
        member x.WriteValue(writer: XmlWriter, value: obj) =
            writer.WriteValue(value.ToString())

    type FsSchemaSimpleType_Value with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
            writer.WriteValue(value.ToString())

    type FsSchemaSimpleType with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
            match x with 
            | FsSchemaSimpleType.EnumType v -> v.WriteValue(writer, value)
            | FsSchemaSimpleType.ValueType v -> v.WriteValue(writer, value)

    type FsSchemaComplexType_Collection with 
        member x.WriteValue(writer: XmlWriter, value: obj) =   
            match value with 
            | :? System.Collections.IEnumerable as items ->
                for item in items do    
                    let itemTp = 
                        { Name = None 
                          FsSchemaType = x.ElementSchemaType
                        }
                        
                    itemTp.WriteValue(writer, item)


            | _ -> failwithf "%A should be IEnumerable type" (value.GetType())

    type FsSchemaComplexType_Dictionary with 
        member x.WriteValue(writer: XmlWriter, value: obj) =  
            let propDictTp = x.TypeCode
            let mapSerializeMethod = 
                let mapSerializer = typedefof<MapSerializer<_,_>>.MakeGenericType([|propDictTp.KeyType; propDictTp.ValueType|])
                let bindingFlags =
                    BindingFlags.NonPublic ||| BindingFlags.Static 
                mapSerializer.GetMethod("Serialize", bindingFlags)

            mapSerializeMethod.Invoke(null, [| x; value; writer |]) |> ignore
           
    type FsSchemaComplexType_Tuple with 
        member x.WriteValue(writer: XmlWriter, value: obj) =  
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
        member x.WriteValue(writer: XmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexGenericType.Collection v -> v.WriteValue(writer, value)
            | FsSchemaComplexGenericType.Dictionary v -> v.WriteValue(writer, value)
            | FsSchemaComplexGenericType.Tuple v -> v.WriteValue(writer, value)
            | _ -> failwithf "Not implemented"

    type FsSchemaComplexType_Record with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
            x.Zip3()
            |> List.iter(fun (element, schemaType, propertyInfo) ->
                let propValue = propertyInfo.GetValue(value)
                let propTp =
                    { Name = Some element.Name.Text
                      FsSchemaType = schemaType }

                propTp.WriteValue(writer, propValue)
            )

    type FsSchemaComplexType_SingleCaseUnion with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexType_SingleCaseUnion.OneField field ->
                let fieldValue = 
                    let field = field.Field
                    field.GetValue(value)

                let namedSchemaType =
                    { Name = Some field.Element.Name.Text
                      FsSchemaType = field.ElementSchemaType }

                namedSchemaType.WriteValue(writer, fieldValue)


           
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
        member x.WriteValue(writer: XmlWriter, propValue: obj) =
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

            writer.WriteStartElement("Choice" + cases.Length.ToString())

            match fields with 
            | [||] -> 
                writer.WriteStartElement(uci.Name)
                writer.WriteFullEndElement()

            | [|field|] ->
                let uciSchema = 
                    match getUciSchema() with 
                    | FsSchemaComplexType_UnionCase.OneFieldCase v -> v
                    | uci -> failwithf "Invalid token, uciSchema %A should be OneFieldCase here" uci

                let schemaType =
                    { Name = Some uciSchema.UnionCase.Name 
                      FsSchemaType = uciSchema.ElementSchemaType }

                schemaType.WriteValue(writer, field)

            | fields ->
                writer.WriteStartElement(uci.Name)

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

                                
            writer.WriteEndElement()

    type FsSchemaComplexType with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
            match x with 
            | FsSchemaComplexType.Generic v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.Record v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.SinglecaseUnion v -> v.WriteValue(writer, value)
            | FsSchemaComplexType.Union v -> v.WriteValue(writer, value)
            | _ -> failwithf "Not implemented"


    type MappedFsSchemaType with 
        member x.WriteValue(writer: XmlWriter, value: obj) =
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
        member x.WriteValue(writer: XmlWriter, value: obj) =
            match x with 
            | FsSchemaType.SimpleType v -> v.WriteValue(writer, value)
            | FsSchemaType.ComplexType v -> v.WriteValue(writer, value)
            | FsSchemaType.Option v ->
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

            | _ -> failwithf "Not implemented"


    type NamedFsSchemaType with
        member x.WriteValue(writer: XmlWriter, value: obj) = 
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

                writer.WriteStartElement(name.Text)
                x.FsSchemaType.WriteValue(writer, value)
                writer.WriteFullEndElement()






    type internal FsXmlSerializer_SerializePart<'T>(configuration: FsXmlSerializerConfiguration) =
        let configuration = configuration
        let encoding = System.Text.Encoding.UTF8
        let tp = typeof<'T>
        let __CheckTypeValid =
            match FSharpType.IsRecord tp with 
            | true -> ()
            | false -> failwithf "Root type should be fsharp record"

        let props = FSharpType.GetRecordFields tp


     


     
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

        //static member private SerializeValue(writer: XmlWriter, prop: SCasablePropertyType, propValue: obj,  configuration: FsXmlSerializerConfiguration, ?inCollection) =        
            //let propValueType = 
            //    match propValue with 
            //    | null -> None
            //    | _ ->  
            //        propValue.GetType()
            //        |> Some

            //let wrapOldName = 
            //    match propValueType with 
            //    | None -> None
            //    | Some propValueType ->
            //        match configuration.TryGetTypeMapping(propValueType) with 
            //        | None -> None
            //        | Some tpMapping -> 
            //            match tpMapping.TypeMapping.WrapOldName with 
            //            | true -> Some tpMapping.OriginType.Name
            //            | false -> None


            //let tryWrapOldName_TypeMapping(f) =
            //        //match propValue with 
            //        //| :? FsIXmlSerializableTypeMapping as v -> 
            //        //    match v.WrapOldName with 
            //        //    | false -> None
            //        //    | true -> 
            //        //        propValue.GetType().Name
            //        //        |> Some
            //        //| _ -> None

            //    match wrapOldName with 
            //    | Some oldName -> 
            //        writer.WriteStartElement(oldName)
            //        f()
            //        writer.WriteFullEndElement()

            //    | None -> f()


            //let prop, propValue = 
            //    let prop = 
            //        match propValue with 
            //        | null -> prop
            //        | _ -> prop.ToNamedTypeWith(propValue.GetType())

            //    configuration.UpdateSCasablePropertyTypeAndValue_ToXml(prop, propValue)


            //let propTp = prop.PropertyType

            //let tpCode = getFsTpCodeEx (propTp)
            //match tpCode with 
            //| FsTypeCodeEx.Tuple (tpCodes) ->
            //    let inCollection = defaultArg inCollection false
            //    let ignorePropInCollection(f) =
            //        let f() =   
            //            tryWrapOldName_TypeMapping(f)

            //        match inCollection with 
            //        | false -> 
            //            writer.WriteStartElement(prop.Name)
            //            f()
            //            writer.WriteFullEndElement()

            //        | true -> f()

            //    ignorePropInCollection(fun () ->
            //        writer.WriteStartElement("Tuple" + tpCodes.Length.ToString())

            //        let tupleElements =
            //            FSharpValue.GetTupleFields(propValue)

            //        (tpCodes, tupleElements)
            //        ||> Array.iteri2(fun i (tpCode, tp) tupleElement ->
            //            let name = itemText i
            //            FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NamedType(name, tp), tupleElement, configuration)
            //        )

            //        writer.WriteFullEndElement()
            //    )


            //| FsTypeCodeEx.Option (tpCode, tp) ->
            //    match propValue with 
            //    | null -> 
            //        writer.WriteStartElement(prop.Name)
            //        writer.WriteAttributeString("nil", W3XMLSchemaInstance, "true")
            //        writer.WriteFullEndElement()

            //    | propValue ->
            //        let valueProp = propValue.GetType().GetProperty("Value")
            //        let propValue = valueProp.GetValue(propValue)
            //        FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NillableNamedType(prop.Name, tp), propValue, configuration)

            //| FsTypeCodeEx.DictionaryType propDictTp -> 
            //    writer.WriteStartElement(prop.Name)

            //    let mapSerializeMethod = 
            //        let mapSerializer = typedefof<MapSerializer<_,_>>.MakeGenericType([|propDictTp.KeyType; propDictTp.ValueType|])
            //        let bindingFlags =
            //            BindingFlags.NonPublic ||| BindingFlags.Static 
            //        mapSerializer.GetMethod("Serialize", bindingFlags)

            //    mapSerializeMethod.Invoke(null, [| propDictTp; propValue; writer; configuration; FsXmlSerializer<_>.SerializeRecordStatic |]) |> ignore
           
            //    writer.WriteEndElement()


            //| FsTypeCodeEx.CollectionType propCollectionTp -> 
            //    writer.WriteStartElement(prop.Name)
            //    let elementTp = propCollectionTp.ElementType

            //    match propValue with 
            //    | :? System.Collections.IEnumerable as items ->
            //        for item in items do 
            //            FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.Type elementTp, item, configuration, inCollection = true)

            //    | _ -> failwithf "%s should be IEnumerable type" propTp.Name

            //    writer.WriteEndElement()

            //| FsTypeCodeEx.FsTypeCode tpCode ->
            
            //    let writeProp(f) =
            //        let f() =
            //            tryWrapOldName_TypeMapping(f)

            //        match prop with 
            //        | SCasablePropertyType.NillableNamedType _ -> 
            //            writer.WriteStartElement(prop.Name)
            //            writer.WriteAttributeString("nil", W3XMLSchemaInstance, "false")
            //            f()
            //        | _ -> 
            //            writer.WriteStartElement(prop.Name)
            //            f()

            //        writer.WriteFullEndElement()

            //    let writePropValueType(f) =
            //        match wrapOldName with 
            //        | None -> 
            //            match prop with 
            //            | SCasablePropertyType.NillableNamedType _ -> 
            //                writer.WriteStartElement(prop.Name)
            //                writer.WriteAttributeString("nil", W3XMLSchemaInstance, "false")
            //                f()
            //            | _ -> 
            //                writer.WriteStartElement(prop.Name)
            //                f()

            //            writer.WriteFullEndElement()


            //        | Some oldName ->
            //            writer.WriteStartElement(oldName)
            //            match prop with 
            //            | SCasablePropertyType.NillableNamedType _ -> 
            //                writer.WriteAttributeString("nil", W3XMLSchemaInstance, "false")
            //                f()
            //            | _ -> 
            //                f()

            //            writer.WriteFullEndElement()



            //    match tpCode with 
            //    | FsTypeCode.ValueType _
            //    | FsTypeCode.Enum ->
            //        writePropValueType(fun () ->
            //            writer.WriteValue(propValue.ToString())
            //        )
                    

            //    | FsTypeCode.Object objectTpCode ->
            //        match propValue with 
            //        | :? FsIXmlSerializable as xmlSerilizable -> xmlSerilizable.WriteXml(writer, configuration)
            //        | _ -> 
            //            writeProp(fun () ->
            //                match objectTpCode with 
            //                | FsObjectTypeCode.FsXmlSerializableTypeMapping  -> 
            //                    /// already predicate by previous line (| :? FsIXmlSerializable as xmlSerilizable)
            //                    failwithf "Invalid token"

            //                | FsObjectTypeCode.Record -> 
            //                    FsXmlSerializer<_>.SerializeRecordStatic(writer, propValue, prop.Nillable, configuration)

            //                | FsObjectTypeCode.SingletonCaseUnion case ->
            //                    let fields = case.GetFields()
            //                    match fields with 
            //                    | [||] -> failwithf "Not implemented"
            //                    | [|field|] ->
            //                        let fieldValue = field.GetValue(propValue)
            //                        FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.OneFieldSCase field.PropertyType, fieldValue, configuration)

            //                    | fields ->
                            
            //                        writer.WriteStartElement("SCase")

            //                        for field in fields do
            //                            let fieldValue = field.GetValue(propValue)
            //                            FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.PropertyInfo field, fieldValue, configuration)
            //                            ()

            //                        writer.WriteEndElement()


            //                | FsObjectTypeCode.Union cases -> 
            //                    let uci, fields = FSharpValue.GetUnionFields(propValue, propTp)
            //                    let zippedFields =
            //                        uci.GetFields()
            //                        |> Array.zip fields



            //                    writer.WriteStartElement("Choice" + cases.Length.ToString())
            //                    match zippedFields with 
            //                    | [||] -> 
            //                        writer.WriteStartElement(uci.Name)
            //                        writer.WriteFullEndElement()

            //                    | [|field, fieldTp|] ->
            //                        FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.NamedType(uci.Name, fieldTp.PropertyType), field, configuration)
                                
            //                    | zippedFields ->
            //                        writer.WriteStartElement(uci.Name)

            //                        for (field, fieldTp) in zippedFields do
            //                            FsXmlSerializer<_>.SerializeValue(writer, SCasablePropertyType.PropertyInfo(fieldTp), field, configuration)

            //                        writer.WriteFullEndElement()

                                
            //                    writer.WriteEndElement()

                                
            //            )




    
        static member internal SerializeValue(writer: XmlWriter, propTp: NamedFsSchemaType, propValue: obj) =        
            propTp.WriteValue(writer, propValue)
    
        //static member private SerializeRecordStatic(writer: XmlWriter, value: obj, configuration: FsXmlSerializerConfiguration) =
        //    let tp = value.GetType()

        //    match FSharpType.IsRecord tp with 
        //    | true ->
        //        let props = FSharpType.GetRecordFields(tp)
        //        for prop in props do 
        //            let propValue = prop.GetValue(value)
        //            let propTp =
        //                let fsSchemaType = configuration.GetFsXmlSchemaType(prop.PropertyType)
        //                { Name = Some prop.Name 
        //                  FsSchemaType = fsSchemaType }

        //            FsXmlSerializer_SerializePart<_>.SerializeValue(writer, propTp, propValue)

        //    | false -> failwithf "Not implemented"


        member x.SerializeRecord(writer: XmlWriter, value: 'T) =
            match FSharpType.IsRecord tp with 
            | true ->
                x.WriteW3CAttributeString_xsi_xsd(writer)
                for prop in props do 
                    let propValue = prop.GetValue(value)
                    let propTp =
                        let fsSchemaType = configuration.GetFsXmlSchemaType(prop.PropertyType)
                        { Name = Some prop.Name 
                          FsSchemaType = fsSchemaType }

                    FsXmlSerializer_SerializePart<_>.SerializeValue(writer, propTp, propValue)
                

            | false -> failwithf "Not implemented"


        static member SerializeXmlNodeValue(writer: XmlWriter, value: obj, configuration: FsXmlSerializerConfiguration, name: string option) =

            let tp = 
                let fsSchemaType = configuration.GetFsXmlSchemaType(value.GetType())
                { FsSchemaType = fsSchemaType  
                  Name = name }

            FsXmlSerializer_SerializePart<_>.SerializeValue(writer, tp, value)
    
        member private x.WriteW3CAttributeString_xsi_xsd(writer: XmlWriter) =
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
                writer.WriteStartElement(tp.Name)
                x.SerializeRecord(writer, value)
                writer.WriteEndElement()


        member private x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath: string) =
            let xsdFileName = Path.GetFileName xsdPath
            let lines = 
                File.ReadAllLines(xmlPath)
                |> Array.mapi(fun i line ->
                    match i with 
                    | 1 -> line.Replace("xmlns:xsi", $"xsi:noNamespaceSchemaLocation=\"{xsdFileName}\" xmlns:xsi")
                    | _ -> line
                )
            File.WriteAllLines(xmlPath, lines)



        member x.SerializeToFile(xmlPath: string, xsdPath: string, value: 'T) =
            x.File_WriteXml(xmlPath, value)
            x.File_WriteXml_NamespaceSchemaLocation(xmlPath, xsdPath)
