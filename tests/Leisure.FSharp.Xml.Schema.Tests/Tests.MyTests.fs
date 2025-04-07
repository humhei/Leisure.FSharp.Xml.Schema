module Tests.MyTests
open Expecto
open Leisure.FSharp.Xml.Schema
open System.Collections.Concurrent
open System
open System.Drawing
open Newtonsoft.Json

module DefaultSerializer = 

    type WenZhou =
        | LuCheng = 0
        | AoJiang = 1

    type City =
        | HangZhou = 0
        | WenZhou  = 1



    [<CLIMutable>]
    type Record = 
        { Name: string 
          Age: int 
          Address: string
          City: City []
          }
    with 

        static member SampleData =
            { Name = "Jia"
              Age = 15
              Address = "HangZhou"
              City = [| City.HangZhou; City.WenZhou |] }

[<RequireQualifiedAccess>]
module GeneralRecord =
    type Card =
        | Card1 = 0
        | Card2 = 1


    type ColorMapping =
        { TargetKnownColor: KnownColor
          OriginKnownColor: Set<KnownColor>
          IndexedCard: Map<int, Card>
          ID: int option
        }
    with 
        static member SampleData =
            { OriginKnownColor = Set.ofList [KnownColor.Black; KnownColor.Silver] 
              TargetKnownColor = KnownColor.Red
              IndexedCard = 
                let dict = ConcurrentDictionary()  
                dict.TryAdd(0, Card.Card1)
                dict.TryAdd(1, Card.Card2)
                dict
                |> Seq.map(fun m -> m.Key, m.Value)
                |> Map.ofSeq
              ID = Some 1

              }
        

[<RequireQualifiedAccess>]
module GeneralRecordWithNestedRecord =


    type ShapeEnum =
        | Circle = 0
        | Rectangle = 1

    [<CLIMutable>]
    type InnerProps =
        { 
            ShapeEnum: ShapeEnum
            Name: string
        }

    [<CLIMutable>]
    type ColorMapping_XMLSchema =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          InnerProps: InnerProps }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          InnerProps: InnerProps }
    with 
        member x.XMLSchema = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              InnerProps  = x.InnerProps }

        static member OfSchema(schema: ColorMapping_XMLSchema) =
            {
                OriginKnownColor = schema.OriginColor
                TargetKnownColor = schema.TargetColor
                InnerProps        = schema.InnerProps
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
            xsSubmit.Deserialize(reader)
            |> ColorMapping.OfSchema
            

        interface FsIXmlSerializable<ColorMapping> with

            member __.ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member __.ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
                xsSubmit.Serialize(writer, x.XMLSchema)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              InnerProps =  
                {Name = "Circle1"
                 ShapeEnum = ShapeEnum.Rectangle} }

[<RequireQualifiedAccess>]
module GeneralRecordWithSingletonCase =


    type ShapeEnum =
        | Circle = 0
        | Rectangle = 1

    [<CLIMutable>]
    type InnerProps =
        { 
            Shape: ShapeEnum
            Name: string
        }

    /// Color value Tolerance for comparison
    type Tolerance =  ByValue 
   

    [<CLIMutable>]
    type ColorMapping_XMLSchema =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          ToleranceProp: Tolerance }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          ToleranceProp: Tolerance }
    with 
        member x.XMLSchema = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              ToleranceProp  = x.ToleranceProp }

        static member OfSchema(schema: ColorMapping_XMLSchema) =
            {
                OriginKnownColor = schema.OriginColor
                TargetKnownColor = schema.TargetColor
                ToleranceProp        = schema.ToleranceProp
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
            xsSubmit.Deserialize(reader)
            |> ColorMapping.OfSchema
            

        interface FsIXmlSerializable<ColorMapping> with

            member __.ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member __.ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
                xsSubmit.Serialize(writer, x.XMLSchema)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              ToleranceProp = Tolerance.ByValue  }


[<RequireQualifiedAccess>]
module GeneralRecordWithTuple =


    type ShapeEnum =
        | Circle = 0
        | Rectangle = 1

    [<CLIMutable>]
    type InnerProps =
        { 
            Shape: ShapeEnum
            Name: string
        }

    /// Color value Tolerance for comparison
    type Tolerance =  private ByValue of float * float
   

    [<CLIMutable>]
    type ColorMapping_XMLSchema =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: float option * float
          AddtionalTolerances: list<float option * float> }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: float option * float
          AddtionalTolerances: list<float option * float>}
    with 
        member x.XMLSchema = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance
              AddtionalTolerances = x.AddtionalTolerances }

        static member OfSchema(schema: ColorMapping_XMLSchema) =
            {
                OriginKnownColor    = schema.OriginColor
                TargetKnownColor    = schema.TargetColor
                Tolerance           = schema.Tolerance
                AddtionalTolerances = schema.AddtionalTolerances
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
            xsSubmit.Deserialize(reader)
            |> ColorMapping.OfSchema
            

        interface FsIXmlSerializable<ColorMapping> with

            member __.ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member __.ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
                xsSubmit.Serialize(writer, x.XMLSchema)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = (Some 5, 3)
              AddtionalTolerances = 
                [(Some 7, 2); (Some 3, 6)]  
            }


[<RequireQualifiedAccess>]
module GeneralRecordWithUnion =


    type ShapeEnum =
        | Circle = 0
        | Rectangle = 1

    [<CLIMutable>]
    type InnerProps =
        { 
            Shape: ShapeEnum
            Name: string
        }

    /// Color value Tolerance for comparison
    [<RequireQualifiedAccess>]
    type Tolerance =    
        | ByValue of float
        | Precise
        | ByValues of float * float
        | ByValueList of float list
        | ByValueOption of float option
        | ByValuesOption of float option * float option
        | ByTupleList of list<float option * float>
   

    [<CLIMutable>]
    type ColorMapping_XMLSchema =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: Tolerance }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: Tolerance }
    with 
        member x.XMLSchema = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance }

        static member OfSchema(schema: ColorMapping_XMLSchema) =
            {
                OriginKnownColor = schema.OriginColor
                TargetKnownColor = schema.TargetColor
                Tolerance        = schema.Tolerance
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
            xsSubmit.Deserialize(reader)
            |> ColorMapping.OfSchema
            

        interface FsIXmlSerializable<ColorMapping> with

            member __.ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member __.ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
                xsSubmit.Serialize(writer, x.XMLSchema)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = Tolerance.ByValue 10 }




[<RequireQualifiedAccess>]
module GeneralRecordWithCustomMapping =

   type ToleranceValue private (v) =
        inherit POCOBaseV<float * float>(v)
        
        member x.Value = v

        static member Create(v) =
            ToleranceValue(v)


   
   [<RequireQualifiedAccess>]
    type Tolerance =    
       | ByValue of ToleranceValue
       | Precise
       | ByValues of ToleranceValue * ToleranceValue
       | ByValueList of ToleranceValue list
       | ByValueOption of ToleranceValue option
       | ByValuesOption of ToleranceValue option * ToleranceValue option
       | ByTupleList of list<ToleranceValue option * ToleranceValue>

    type ColorSpaceEnum =
        | CMYK = 0
        | RGB = 1

    type ColorSpace private () =
        inherit AutoSerializationPOCOBase<ColorSpaceEnum>()
    
        new (colorSpaceValue: ColorSpaceEnum) as this =
            new ColorSpace()
            then this.SetPOCOKey(colorSpaceValue)


    type InnerTolerance =
        { ToleranceValueProp: Tolerance }

    type ColorMapping_XMLSchema =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: Tolerance
          //Tolerance: ToleranceValue
          ColorSpace: ColorSpace
        }

    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor


          Tolerance: Tolerance
          //Tolerance: ToleranceValue
          ColorSpace: ColorSpace
        }
    with 
        member x.XMLSchema = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance
              ColorSpace = x.ColorSpace 
            }

        static member OfSchema(schema: ColorMapping_XMLSchema) =
            {
                OriginKnownColor = schema.OriginColor
                TargetKnownColor = schema.TargetColor
                Tolerance        = schema.Tolerance
                ColorSpace       = schema.ColorSpace
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
            xsSubmit.Deserialize(reader)
            |> ColorMapping.OfSchema
            

        interface FsIXmlSerializable<ColorMapping> with

            member __.ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member __.ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLSchema>(config)
                xsSubmit.Serialize(writer, x.XMLSchema)



        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = 
                //Some (
                //    (ToleranceValue.Create (6, 6))
                //)
                //[
                //    {
                //        ToleranceValueProp = ToleranceValue.Create(5, 6)
                //    }
                //]
                //Tolerance.ByValuesOption (None, Some (ToleranceValue.Create (6, 6)))
                Tolerance.ByValue  (ToleranceValue.Create (6, 6))
                //ToleranceValue.Create(5, 6)
                //[
                //    { ToleranceValueProp =
                //        Tolerance.ByValuesOption (None, Some (ToleranceValue.Create (6, 6)))
                //    }
                //]
              ColorSpace = ColorSpace(ColorSpaceEnum.RGB)
            }



module GeneralRecordWithSkipComparasion =
    [<RequireQualifiedAccess>]
    type DecimalSelector =
        | BetweenCase of float * float 
        | EqualTo of float
        | BiggerOrEqual of float
        | SmallerOrEqual of float
        | True

    type Record =
        { DecimalSelector: SkipComparation_Serializable<ProductName>
          //ProductNameProp: ProductName
          }
    with 
        static member SampleData =
            let decimalSelector = 
                DecimalSelector.BiggerOrEqual(100.)
                |> SkipComparation_Serializable

            { 
              DecimalSelector = 
                //decimalSelector
                SkipComparation_Serializable(ProductName("NestProductName"))

              //ProductNameProp = ProductName("MyProductName")
            }

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let config = FsXmlSerializerConfiguration.DefaultValue
System.IO.Directory.CreateDirectory(@"xml")


type RecordWithInt_BE_0 =
    { Name: string 
      Number: ``Int>=0`` }

type RecordWithAtleastOneList =
    { Name: string 
      Numbers: AtLeastOneList<int> }

type RecordWithAtleastOneMap =
    { Name: string 
      NumberPairs: AtLeastOneMap<int, int> }

[<RequireQualifiedAccess>]
type DirectoryOrFileName =
    | FileName of string
    | Directory of current: string * subDirOrFileNames: DirectoryOrFileName list
    | And of DirectoryOrFileName * DirectoryOrFileName


type RecordWithRecursiveType =
    { Name: string 
      DirectoryOrFileName: DirectoryOrFileName }

type RecordWithInt64 =
    { Name: string 
      Int64: int64 }

type RecordWithSkipComparation =
    { Name: string 
      Age_SkipComparation: SkipComparation_Serializable<int option> }

type SQLStatementVersion =
    | Old = 0 
    | New = 1

let  mutable internal SQLStatementVersionMutable = SQLStatementVersion.Old 

type SheetName [<JsonConstructor>] internal (sheetName: string, ?version: SQLStatementVersion) =
    inherit POCOBase<StringIC * SQLStatementVersion option>(StringIC sheetName, version)

    let sheetName = sheetName.TrimEnd('$')

    member x.NoJsonProp = "<Null>"

    [<JsonProperty>]
    member x.SheetName: string = sheetName

    [<JsonProperty>]
    member x.Version = version

    member x.LongSheetName =
        match defaultArg version SQLStatementVersionMutable with 
        | SQLStatementVersion.Old -> 
            sprintf "%s$" sheetName
    
        | SQLStatementVersion.New ->
            sprintf "Excel.%s" sheetName

    override x.ToString() = 
        sprintf "`%s`" x.LongSheetName
        
    new  (sheetName) = SheetName(sheetName, ?version = None)

type Props =
    { A: int 
      B: int }

type RecordWithSheetName =
    { Name: string 
      SheetName: SheetName }

type RecordWithIgnoreProp =
    { Name: string 
      [<FsXmlSchemaIgnore>]
      IgnoreProp: SheetName }


let MyTests =
    
  testList "MyTests" [
    testCase "default Serializer test" <| fun _ ->
      let fileID = 1
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = DefaultSerializer.Record.SampleData

      let serializer = new FsXmlSerializer<DefaultSerializer.Record>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()
     

    testCase "IXmlSerializable general Record" <| fun _ ->
      let fileID = 2
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = GeneralRecord.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecord.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "IXmlSerializable general Record with nest record" <| fun _ ->
      let fileID = 3
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = GeneralRecordWithNestedRecord.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithNestedRecord.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()


    testCase "IXmlSerializable general Record with singtonCase union" <| fun _ ->
      let fileID = 4
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = GeneralRecordWithSingletonCase.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithSingletonCase.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    
    testCase "IXmlSerializable general Record with tuple" <| fun _ ->
      let fileID = 5
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = GeneralRecordWithTuple.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithTuple.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "IXmlSerializable general Record with union" <| fun _ ->
      let fileID = 6
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 

      let data = GeneralRecordWithUnion.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithUnion.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "IXmlSerializable general Record with custom mapping" <| fun _ ->
      let fileID = 7
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 
      
      let config = 
        FsXmlSerializerConfiguration.DefaultValue.AddTypeMapping<GeneralRecordWithCustomMapping.ToleranceValue, float * float>(
            toXml = (fun m -> m.Value),
            ofXml = (fun (a, b) -> GeneralRecordWithCustomMapping.ToleranceValue.Create(a, b)),
            wrapOldName = true
        )

      let data = GeneralRecordWithCustomMapping.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "IXmlSerializable general Record with POCOBase FsIXmlSerializableTypeMapping" <| fun _ ->
      let fileID = 8
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = GeneralRecordWithCustomMapping.ColorMapping.SampleData
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "IXmlSerializable general Record with SkipComparasion" <| fun _ ->
      let fileID = 9
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = GeneralRecordWithSkipComparasion.Record.SampleData
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<GeneralRecordWithSkipComparasion.Record>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    //testCase "allow non root record type" <| fun _ ->
    //  let fileID = 10
    //  let xmlFile = sprintf @"xml\%d.xml" fileID
    //  let xsdFile = sprintf @"xml\%d.xsd" fileID
    //  let data = GeneralRecordWithSkipComparasion.Record.SampleData.ProductNameProp
    //  let config = FsXmlSerializerConfiguration.DefaultValue

    //  let serializer = new FsXmlSerializer<ProductName>(config)
    //  serializer.SerializeToFile(xmlFile, xsdFile, data)
    //  let data2 = serializer.DeserializeFromFile(xmlFile)
    //  match data = data2 with 
    //  | true -> pass()
    //  | false -> fail()

    testCase "simple type for singleton union case" <| fun _ ->
      let fileID = 11
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          Number = ``Int>=0``.Create 1 }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithInt_BE_0>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "AtLeastOneList min minoccurs support" <| fun _ ->
      let fileID = 12
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          Numbers = AtLeastOneList.Create [1; 2; 3] }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithAtleastOneList>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "AtLeastOneMap support" <| fun _ ->
      let fileID = 13
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          NumberPairs = 
            AtLeastOneMap.Create [
                (1, 2)
                (3, 4)
            ]
          }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithAtleastOneMap>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "recursive type support" <| fun _ ->
      let fileID = 14
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          DirectoryOrFileName = 
            DirectoryOrFileName.Directory(
                "currentDir",
                [DirectoryOrFileName.Directory (
                    "subDir",
                    [DirectoryOrFileName.FileName "MyFileName"]
                )]
            )
            //DirectoryOrFileName.FileName "fileName"
          }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithRecursiveType>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "int64 support" <| fun _ ->
      let fileID = 15
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          Int64 = 1000000000000000L
            //DirectoryOrFileName.FileName "fileName"
          }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithInt64>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "SkipComparation supported" <| fun _ ->
      let fileID = 16
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          Age_SkipComparation = SkipComparation_Serializable (Some 16)
            //DirectoryOrFileName.FileName "fileName"
          }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithSkipComparation>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    testCase "POCOBase with two parameters constructor" <| fun _ ->
      let fileID = 17
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID
      let data = 
        { Name = "MyName"
          SheetName = SheetName("Name")
        }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithSheetName>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

    ftestCase "Record with IgnoreAttribute" <| fun _ ->
      let fileID = 18
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID

      let data = 
        { Name = "MyName"
          IgnoreProp = SheetName("Name")
        }
        
      let config = FsXmlSerializerConfiguration.DefaultValue

      let serializer = new FsXmlSerializer<RecordWithIgnoreProp>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match { data with IgnoreProp = Unchecked.defaultof<_> } = data2 with 
      | true -> pass()
      | false -> fail()

  ]