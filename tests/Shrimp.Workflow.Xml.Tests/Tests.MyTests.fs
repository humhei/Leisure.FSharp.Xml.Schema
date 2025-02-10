module Tests.MyTests
open Expecto
open Shrimp.Workflow.Xml
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

    [<CLIMutable>]
    type ColorMapping_XMLScheme =
        { OriginColor: HashSet<KnownColor>
          TargetColor: KnownColor
          IndexedCard: Map<int, Card>
          ID: int option
          }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: HashSet<KnownColor>
          TargetKnownColor: KnownColor
          IndexedCard: Map<int, Card>
          ID: int option  }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor
              IndexedCard = x.IndexedCard
              ID          = x.ID
            }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
                IndexedCard      = scheme.IndexedCard
                ID               = scheme.ID
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
            { OriginKnownColor = HashSet [KnownColor.Black; KnownColor.Silver] 
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

let MyTests =
  testList "MyTests" [
    testCase "default Serializer test" <| fun _ ->
      let m = new FsXmlSerializer<DefaultSerializer.Record>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\1.xml", @"C:\Users\Administrator\Desktop\1.xsd", DefaultSerializer.Record.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\1.xml")
      pass()

    testCase "IXmlSerializable general Record" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecord.ColorMapping>(config)
      let p = GeneralRecord.ColorMapping.SampleData.IndexedCard :> IEnumerable
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\2.xml", @"C:\Users\Administrator\Desktop\2.xsd", GeneralRecord.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\2.xml")
      pass()

    testCase "IXmlSerializable general Record with nest record" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithNestedRecord.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\3.xml", @"C:\Users\Administrator\Desktop\3.xsd", GeneralRecordWithNestedRecord.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\3.xml")
      pass()

    testCase "IXmlSerializable general Record with singtonCase union" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithSingletonCase.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\4.xml", @"C:\Users\Administrator\Desktop\4.xsd", GeneralRecordWithSingletonCase.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\4.xml")
      pass()

    
    testCase "IXmlSerializable general Record with tuple" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithTuple.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\5.xml", @"C:\Users\Administrator\Desktop\5.xsd", GeneralRecordWithTuple.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\5.xml")
      pass()

    testCase "IXmlSerializable general Record with union" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithUnion.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\6.xml", @"C:\Users\Administrator\Desktop\6.xsd", GeneralRecordWithUnion.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\6.xml")
      pass()

    testCase "IXmlSerializable general Record with custom mapping" <| fun _ ->
      let config = 
        FsXmlSerializerConfiguration.DefaultValue.AddTypeMapping<GeneralRecordWithCustomMapping.ToleranceValue, float>(
            toXml = (fun m -> m.Value),
            ofXml = (fun m -> GeneralRecordWithCustomMapping.ToleranceValue m)
        )

      let m = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\7.xml", @"C:\Users\Administrator\Desktop\7.xsd", GeneralRecordWithCustomMapping.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\7.xml")
      pass()

    ftestCase "IXmlSerializable general Record with POCOBase FsIXmlSerializable" <| fun _ ->
      let config = FsXmlSerializerConfiguration.DefaultValue

      let m = new FsXmlSerializer<GeneralRecordWithCustomMapping.ColorMapping>(config)
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\8.xml", @"C:\Users\Administrator\Desktop\8.xsd", GeneralRecordWithCustomMapping.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\8.xml")
      pass()
  ]