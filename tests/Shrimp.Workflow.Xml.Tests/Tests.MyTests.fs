module Tests.MyTests
open Expecto
open Shrimp.Workflow.Xml
open Shrimp.Workflow.Xml
open System.Xml
open System.Xml.Schema
open System.Drawing
open System.Xml.Serialization

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
          City: City [] }
    with 

        static member SampleData =
            { Name = "Jia"
              Age = 15
              Address = "HangZhou"
              City = [| City.HangZhou; City.WenZhou |] }

[<RequireQualifiedAccess>]
module GeneralRecord =

    [<CLIMutable>]
    type ColorMapping_XMLScheme =
        { OriginColor: KnownColor []
          TargetColor: KnownColor }

    [<CLIMutable>]
    type ColorMapping =
        { OriginKnownColor: KnownColor []
          TargetKnownColor: KnownColor }
    with 
        member x.XMLScheme = 
            { OriginColor = x.OriginKnownColor 
              TargetColor = x.TargetKnownColor }

        static member OfScheme(scheme: ColorMapping_XMLScheme) =
            {
                OriginKnownColor = scheme.OriginColor
                TargetKnownColor = scheme.TargetColor
            }

        interface FsIXmlSerializable<ColorMapping> with

            member x.ReadXml(reader) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                xsSubmit.DeserializeToValue(reader)
                |> ColorMapping.OfScheme

            member x.WriteXml(writer) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                xsSubmit.WriteAttributeString_xsi_xsd(writer)
                xsSubmit.SerializeValue(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = [|KnownColor.Black|] 
              TargetKnownColor = KnownColor.Red }
        

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

        interface IXmlSerializable with
            member x.GetSchema() = 
                null

            member x.ReadXml(reader) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                let scheme = x.XMLScheme
                xsSubmit.DeserialzeToValue_WithRoot(reader, scheme, ColorMapping.OfScheme, x)

            member x.WriteXml(writer) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                xsSubmit.WriteAttributeString_xsi_xsd(writer)
                xsSubmit.SerializeValue(writer, x.XMLScheme)


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
    type Tolerance =  private ByValue of float
   

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

        interface FsIXmlSerializable with

            member __.ReadXml(reader) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                xsSubmit.DeserializeToValue(reader, scheme, ColorMapping.OfScheme, x)

            member x.WriteXml(writer) = 
                let xsSubmit = FsXmlSerializer<ColorMapping_XMLScheme>()
                xsSubmit.WriteAttributeString_xsi_xsd(writer)
                xsSubmit.SerializeValue(writer, x.XMLScheme)


        static member SampleData =
            { OriginKnownColor = KnownColor.Black 
              TargetKnownColor = KnownColor.Red
              Tolerance = Tolerance.ByValue 5 }

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let MyTests =
  testList "MyTests" [
    testCase "default Serializer test" <| fun _ ->
      let m = new FsXmlSerializer<DefaultSerializer.Record>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\1.xml", @"C:\Users\Administrator\Desktop\1.xsd", DefaultSerializer.Record.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\1.xml")
      pass()

    ftestCase "IXmlSerializable general Record" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecord.ColorMapping>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\2.xml", @"C:\Users\Administrator\Desktop\2.xsd", GeneralRecord.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\2.xml")
      pass()

    testCase "IXmlSerializable general Record with nest record" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithNestedRecord.ColorMapping>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\3.xml", @"C:\Users\Administrator\Desktop\3.xsd", GeneralRecordWithNestedRecord.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\3.xml")
      pass()

    testCase "IXmlSerializable general Record with singtonCase union" <| fun _ ->
      let m = new FsXmlSerializer<GeneralRecordWithSingletonCase.ColorMapping>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\4.xml", @"C:\Users\Administrator\Desktop\4.xsd", GeneralRecordWithSingletonCase.ColorMapping.SampleData)
      let p = m.DeserializeFromFile(@"C:\Users\Administrator\Desktop\4.xml")
      pass()
  ]