# Leisure.FSharp.Xml.Schema
serialize xml and xsd schema file for fsharp types toghther
so your can edit your xml structure in editor in type safe mode(e.g by vs code xmlExtension)


## Notes
### fsharp types
    1. most fsharp type supported
    2. root type must be fsharp record

### csharp types
    1. CSharp List and Dictionary are supported
    2. other POCO classes are supported by TypeMapping

### Xml Attributes
    currently all xml attributes are ignored

unsupported type will raise error

## Usage
### sample code
```fsharp
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

```
### typeMapping

#### 1. by FsXmlSerializerConfiguration
```fsharp
        FsXmlSerializerConfiguration.DefaultValue.AddTypeMapping<GeneralRecordWithCustomMapping.ToleranceValue, float>(
            toXml = (fun m -> m.Value),
            ofXml = (fun m -> GeneralRecordWithCustomMapping.ToleranceValue m)
        )
```
#### 2. implement FsIXmlSerializableTypeMapping interface
see Leisure.FSharp.Xml.Schema.Tests.POCOBase.fs for detail



Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/Leisure.FSharp.Xml.Schema)](https://www.nuget.org/packages/Leisure.FSharp.Xml.Schema/) | [![NuGet Badge](https://buildstats.info/nuget/Leisure.FSharp.Xml.Schema?includePreReleases=true)](https://www.nuget.org/packages/Leisure.FSharp.Xml.Schema/)


MacOS/Linux | Windows
--- | ---
[![CircleCI](https://circleci.com/gh/myName/Leisure.FSharp.Xml.Schema.svg?style=svg)](https://circleci.com/gh/myName/Leisure.FSharp.Xml.Schema) | [![Build status](https://ci.appveyor.com/api/projects/status/0qnls95ohaytucsi?svg=true)](https://ci.appveyor.com/project/myName/Leisure.FSharp.Xml.Schema)
[![Build History](https://buildstats.info/circleci/chart/myName/Leisure.FSharp.Xml.Schema)](https://circleci.com/gh/myName/Leisure.FSharp.Xml.Schema) | [![Build History](https://buildstats.info/appveyor/chart/myName/Leisure.FSharp.Xml.Schema)](https://ci.appveyor.com/project/myName/Leisure.FSharp.Xml.Schema)

---