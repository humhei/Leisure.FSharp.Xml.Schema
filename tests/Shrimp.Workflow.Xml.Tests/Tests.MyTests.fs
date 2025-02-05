module Tests.MyTests
open Expecto
open Shrimp.Workflow.Xml
open Shrimp.Workflow.Xml
open System.Xml
open System.Xml.Schema
open System.Drawing
open System.Xml.Serialization

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


[<CLIMutable>]
type ColorMappingXMLScheme =
    { OriginColor: string 
      TargetColor: string }

[<CLIMutable>]
type ColorMapping =
    { OriginKnownColor: KnownColor
      TargetKnownColor: KnownColor }
with 
    member x.Scheme = 
        { OriginColor = x.OriginKnownColor.ToString() 
          TargetColor = x.TargetKnownColor.ToString() }

    static member OfScheme(scheme: ColorMappingXMLScheme) =
        {
            OriginKnownColor = System.Enum.Parse<_> scheme.OriginColor
            TargetKnownColor = System.Enum.Parse<_> scheme.TargetColor
        }

    interface IXmlSerializable with
        member x.GetSchema() = 
            null

        member x.ReadXml(reader) = 
            let xsSubmit = FsXmlSerializer<ColorMappingXMLScheme>()
            let scheme = x.Scheme
            xsSubmit.DeserialzeToValue_WithRoot(reader, scheme, ColorMapping.OfScheme, x)

        member x.WriteXml(writer) = 
            let xsSubmit = FsXmlSerializer<ColorMappingXMLScheme>()
            xsSubmit.SerializeValue(writer, x.Scheme)

    static member SampleData =
        { OriginKnownColor = KnownColor.Black 
          TargetKnownColor = KnownColor.Red }
        

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let MyTests =
  testList "MyTests" [
    testCase "MyTest Record" <| fun _ ->
      let m = new FsXmlSerializer<Record>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\1.xml", @"C:\Users\Administrator\Desktop\1.xsd", Record.SampleData)
      let p = m.Deserialize(@"C:\Users\Administrator\Desktop\1.xml")
      pass()

    ftestCase "MyTest IXmlSerializable" <| fun _ ->
      let m = new FsXmlSerializer<ColorMapping>()
      let cc = m.SerializeToFile(@"C:\Users\Administrator\Desktop\2.xml", @"C:\Users\Administrator\Desktop\2.xsd", ColorMapping.SampleData)
      let p = m.Deserialize(@"C:\Users\Administrator\Desktop\2.xml")
      pass()
  ]