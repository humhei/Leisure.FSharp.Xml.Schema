module Tests.MyTests
open Expecto
open Leisure.FSharp.Xml.Schema
open System.Collections.Concurrent
open System.Drawing
open System.Collections
open System.Collections.Generic

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
        { OriginKnownColor: Set<KnownColor>
          TargetKnownColor: KnownColor
          IndexedCard: Map<int, Card>
          ID: int option  }
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
            Name: string
        }

    [<CLIMutable>]
    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          InnerProps: InnerProps }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          InnerProps: InnerProps }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              InnerProps  = x.InnerProps }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                InnerProps        = scheme.InnerProps
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
            xsSubmit.DeserializeToRecord(reader)
            |> ColorMapping.OfScheme
            

        interface FsIXmlSerializable<ColorMapping> with

            static member ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            static member ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
                xsSubmit.SerializeRecord(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              InnerProps = {Name = "Circle1"} }

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
    type Tolerance =  private ByValue of float * float
   

    [<CLIMutable>]
    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: Tolerance }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: Tolerance }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                Tolerance        = scheme.Tolerance
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
            xsSubmit.DeserializeToRecord(reader)
            |> ColorMapping.OfScheme
            

        interface FsIXmlSerializable<ColorMapping> with

            static member ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            static member ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
                xsSubmit.SerializeRecord(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = Tolerance.ByValue (5, 3) }


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
    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: float option * float }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: float option * float }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                Tolerance        = scheme.Tolerance
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
            xsSubmit.DeserializeToRecord(reader)
            |> ColorMapping.OfScheme
            

        interface FsIXmlSerializable<ColorMapping> with

            static member ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            static member ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
                xsSubmit.SerializeRecord(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = (Some 5, 3) }


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
    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: Tolerance }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: Tolerance }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                Tolerance        = scheme.Tolerance
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
            xsSubmit.DeserializeToRecord(reader)
            |> ColorMapping.OfScheme
            

        interface FsIXmlSerializable<ColorMapping> with

            static member ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            static member ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
                xsSubmit.SerializeRecord(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = Tolerance.ByValueList[5; 6] }




[<RequireQualifiedAccess>]
module GeneralRecordWithCustomMapping =

   type ToleranceValue(v) =
        inherit POCOBaseV<float>(v)
        
        member x.Value = v


   
   [<RequireQualifiedAccess>]
    type Tolerance =    
       | ByValue of ToleranceValue
       | Precise
       | ByValues of ToleranceValue * ToleranceValue
       | ByValueList of ToleranceValue list
       | ByValueOption of ToleranceValue option
       | ByValuesOption of ToleranceValue option * ToleranceValue option
       | ByTupleList of list<ToleranceValue option * ToleranceValue>

    
    type InnerTolerance =
        { Tolerance: Tolerance }

    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor
          TargetColor: KnownColor
          Tolerance: InnerTolerance list }

    type ColorMapping =
        { OriginKnownColor: KnownColor
          TargetKnownColor: KnownColor
          Tolerance: InnerTolerance list }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              Tolerance  = x.Tolerance }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                Tolerance        = scheme.Tolerance
            }

        static member ReadXml(reader, config) =
            let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
            xsSubmit.DeserializeToRecord(reader)
            |> ColorMapping.OfScheme
            

        interface FsIXmlSerializable<ColorMapping> with

            static member ReadXml(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            static member ReadXmlObj(tp, reader, config) = ColorMapping.ReadXml(reader, config)

            member x.WriteXml(writer, config) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>(config)
                xsSubmit.SerializeRecord(writer, x.XMLScheme)



        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = 
                [
                    { Tolerance =
                        Tolerance.ByValuesOption (None, Some (ToleranceValue 6))
                    }
                ]
            }


let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let config = FsXmlSerializerConfiguration.DefaultValue
System.IO.Directory.CreateDirectory(@"xml")

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

    ftestCase "IXmlSerializable general Record with custom mapping" <| fun _ ->
      let fileID = 7
      let xmlFile = sprintf @"xml\%d.xml" fileID
      let xsdFile = sprintf @"xml\%d.xsd" fileID 
      
      let config = 
        FsXmlSerializerConfiguration.DefaultValue.AddTypeMapping<GeneralRecordWithCustomMapping.ToleranceValue, float>(
            toXml = (fun m -> m.Value),
            ofXml = (fun m -> GeneralRecordWithCustomMapping.ToleranceValue m)
        )

      let data = GeneralRecordWithCustomMapping.ColorMapping.SampleData

      let serializer = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      serializer.SerializeToFile(xmlFile, xsdFile, data)
      let data2 = serializer.DeserializeFromFile(xmlFile)
      match data = data2 with 
      | true -> pass()
      | false -> fail()

      let m = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      let cc = m.SerializeToFile(@"xml\7.xml", @"xml\7.xsd", GeneralRecordWithCustomMapping.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"xml\7.xml")
      pass()

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
  ]