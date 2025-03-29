// Learn more about F# at http://fsharp.org
namespace Leisure.FSharp.Xml.Schema
#nowarn "0104"
open System.Reflection

#nowarn "3535"
#nowarn "3536"

open System

open System.Collections.Generic


open System.Collections.Concurrent
open Microsoft.FSharp.Reflection
open System.Xml
open System.Xml.Schema

module internal rec FsSchemaTypesAST =
    [<RequireQualifiedAccess>]
    type FsSchemaTypeOrSchemaTypeName =
        | SchemaType of FsSchemaType
        | SchemaTypeName of XmlQualifiedName
    with 
        member x.GetAsXmlQualifiedName() =
            match x with 
            | SchemaTypeName v -> v
            | SchemaType v -> 
                v.GetElementName()
                |> XmlQualifiedName

    [<RequireQualifiedAccess>]
    type PropNameOrElementName =
        | PropName of string
        | ElementName of string
    with 
        member x.Text =
            match x with 
            | PropName v
            | ElementName v -> v

    let internal (|ElementName_AND_Tuple|_|) (name: PropNameOrElementName, tpOrTypeName: FsSchemaTypeOrSchemaTypeName) =
        match name, tpOrTypeName with 
        | PropNameOrElementName.ElementName name, FsSchemaTypeOrSchemaTypeName.SchemaType schemaType ->
            match schemaType.GetAsGeneric() with 
            | Some (FsSchemaComplexGenericType.Tuple tp) ->
                Some (tp, name)

            | _ -> None

        | _ -> None

    type FsXmlSchemaElement =
        { IsOption: bool 
          Name: PropNameOrElementName
          SchemaTypeOrSchemaTypeName: FsSchemaTypeOrSchemaTypeName
        }

    with 
        member x.ToSchema(): XmlSchemaElement =
            match (x.Name, x.SchemaTypeOrSchemaTypeName) with 
            | ElementName_AND_Tuple(tupleTp: FsSchemaComplexType_Tuple, name) ->
                let schemaType: XmlSchemaComplexType = tupleTp.ToSchema()
                let m = schemaType.Particle :?> XmlSchemaSequence
                match m.Items.Count with 
                | 1 ->
                    let item = m.Items.[0] :?> XmlSchemaElement
                    item
                | _ ->  failwithf "Not implement"

            | _ ->
                
                let name = x.Name.Text
                let element = 
                    match x.IsOption with 
                    | true ->
                        XmlSchemaElement(
                            Name = name,
                            IsNillable = true
                        )

                    | _ ->
                        XmlSchemaElement(
                            Name = name
                        )

                match x.SchemaTypeOrSchemaTypeName with 
                | FsSchemaTypeOrSchemaTypeName.SchemaTypeName tpName -> 
                    element.SchemaTypeName <- tpName
                    element

                | FsSchemaTypeOrSchemaTypeName.SchemaType tp ->
                    let schemas =  tp.ToSchemas()
                    match schemas with 
                    | [schemaType] -> 
                        element.SchemaType <- schemaType
                        element
                       
                    | _ -> failwithf "Not implemented, schemaType %A should be singleton here" tp

                //match List.tryLast schemas with 
                //| None -> failwithf "Invalid token, schemaType %A should be value type here" tp
                //| Some schemaType -> element.SchemaType <- schemaType


    let private createPropSequence (elements: FsXmlSchemaElement list) =
        let propSequence = 
            XmlSchemaSequence()

        for element in elements do
            let element = element.ToSchema()
            propSequence.Items.Add(element)
            |> ignore
        
        propSequence

    type FsSchemaComplexType_Tuple =
        { Elements: FsXmlSchemaElement list
          ElementSchemaTypes: FsSchemaType list
          //TypeCode: array<FsTypeCodeEx * Type>
          Tp: Type
        }
    with 
        member x.InnerElementName =
            "Tuple" + x.Elements.Length.ToString()

        member x.ToSchema() =
            let innerType = 
                let tuplePropSequence =
                    createPropSequence x.Elements

                XmlSchemaComplexType(
                    Particle = tuplePropSequence
                )

            //innerType

            let element =   
                XmlSchemaElement(
                    IsNillable = false,
                    Name = x.InnerElementName,
                    SchemaType = innerType
                )

            let propSequence = 
                XmlSchemaSequence()

            propSequence.Items.Add(element)
            |> ignore
            
            XmlSchemaComplexType(
                Particle = propSequence
            )


        static member Create(tp: Type, elementSchemaTypes) =
            let elements = 
                elementSchemaTypes
                |> List.mapi(fun i (fsSchemaType: FsSchemaType) -> 
                    fsSchemaType.GenerateFsElement(Some (itemText i))
                    //SCasablePropertyType.CreateNamedType_Schema(itemText i, fsSchemaType)
                )
                //|> List.map(fun m -> m.GenerateFsElement())

            { Elements = elements
              ElementSchemaTypes = elementSchemaTypes
              //TypeCode = tpCodes
              Tp = tp}

        member x.GetSchemaTypeOrSchemaTypeName(): FsSchemaTypeOrSchemaTypeName =
            x
            |> FsSchemaComplexGenericType.Tuple
            |> FsSchemaComplexType.Generic
            |> FsSchemaType.ComplexType
            |> FsSchemaTypeOrSchemaTypeName.SchemaType 
            //failwithf ""
                

    type FsSchemaComplexType_Entry =
        { KeyElement: FsXmlSchemaElement
          ValueElement: FsXmlSchemaElement
          KeySchemaType: FsSchemaType
          ValueSchemaType: FsSchemaType
          TypeCode: DictionaryType
        }
    with 
        //member x.EntryTypeName = 
        //    "Entry__" 
        //    + x.KeyElement.SchemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name 
        //    + "_" + x.ValueElement.SchemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name
    
        member x.ToSchema() =
            let propSequence = 
                createPropSequence [x.KeyElement; x.ValueElement]

            //let content = 
            //    XmlSchemaComplexContent(
            //        Content = 
            //            XmlSchemaComplexContentExtension(
            //                BaseTypeName = XmlQualifiedName("Entry"),
            //                Particle = propSequence
            //            )
            //    )

            XmlSchemaComplexType(
                //Name = x.EntryTypeName,
                Particle = propSequence
            )

        static member Create(tpCode: DictionaryType, keySchemaType: FsSchemaType, valueSchemaType: FsSchemaType) =

            {
                KeyElement = 
                    keySchemaType.GenerateFsElement(Some "Key")

                ValueElement = 
                    valueSchemaType.GenerateFsElement(Some "Value")

                KeySchemaType = keySchemaType
                ValueSchemaType = valueSchemaType
                TypeCode = tpCode
            }
        

    type FsSchemaComplexType_Dictionary =
        { Entry: FsSchemaComplexType_Entry
          TypeCode: DictionaryType
        }
    with
        member x.KeyElement = x.Entry.KeyElement
        member x.ValueElement = x.Entry.ValueElement

        //member x.DictionaryTypeName = 
        //    "Dict__" 
        //    + x.KeyElement.SchemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name 
        //    + "_" + x.ValueElement.SchemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name

        //member x.EntryTypeName = x.Entry.EntryTypeName

        member private x.ToSchema() =
            //let arrayTpName = x.DictionaryTypeName
            //let entryTypeName = x.EntryTypeName
            let propSequence = XmlSchemaSequence()

            propSequence.Items.Add(
                XmlSchemaElement(
                    MinOccurs = 0,
                    MaxOccursString = "unbounded",
                    Name = "Entry",
                    SchemaType = x.Entry.ToSchema()
                )
            )   
            |> ignore

            XmlSchemaComplexType(
                //Name = arrayTpName,
                Particle = propSequence
            )

        member x.ToSchemas() =
            [
                //x.Entry.ToSchema()
                x.ToSchema()
            ]

        //member x.GetXmlQualifiedName() =
        //    x.DictionaryTypeName
        //    |> XmlQualifiedName

        member x.GetSchemaTypeOrSchemaTypeName() =
            x
            |> FsSchemaComplexGenericType.Dictionary
            |> FsSchemaComplexType.Generic
            |> FsSchemaType.ComplexType
            |> FsSchemaTypeOrSchemaTypeName.SchemaType


        static member Create(tpCode: DictionaryType, keySchemaType, valueSchemaType) =
            {
                Entry = FsSchemaComplexType_Entry.Create(tpCode, keySchemaType, valueSchemaType)
                TypeCode = tpCode
            }

    type FsSchemaComplexType_Collection =
        { TypeCode: CollectionType
          ElementSchemaType: FsSchemaType
          Element: FsXmlSchemaElement
        }
    with
        
        //member x.ArrayTypeName = 
        //    "ArrayOf" + x.Element.Name

        member x.ToSchema() =
            let element = x.Element.ToSchema()

            do
                element.MinOccurs <- 0
                element.MaxOccursString <- "unbounded"

            //let arrayTpName = x.ArrayTypeName

            let propSequence = 
                XmlSchemaSequence()

            do
                propSequence.Items.Add(
                    element
                )   
                |> ignore

            XmlSchemaComplexType(
                //Name = arrayTpName,
                Particle = propSequence
            )

        //member x.GetXmlQualifiedName() =
        //    x.ArrayTypeName
        //    |> XmlQualifiedName

        member x.GetSchemaTypeOrSchemaTypeName() =
            x
            |> FsSchemaComplexGenericType.Collection
            |> FsSchemaComplexType.Generic
            |> FsSchemaType.ComplexType
            |> FsSchemaTypeOrSchemaTypeName.SchemaType




        static member Create(tpCode, elementSchemaType: FsSchemaType) =
            let element =
                elementSchemaType.GenerateFsElement(propName = None)
                //let prop = SCasablePropertyType.CreateNamedType_Schema(elementSchemaType.GetElementName(), elementSchemaType)
                //prop.GenerateFsElement()

            let schemaTypeOrSchemaTypeName =
                elementSchemaType.GetSchemaTypeOrSchemaTypeName()
                
            let element =
                { element with SchemaTypeOrSchemaTypeName = schemaTypeOrSchemaTypeName }


            { Element = element 
              ElementSchemaType = elementSchemaType 
              TypeCode = tpCode }



    [<RequireQualifiedAccess>]
    type FsSchemaComplexGenericType =
        | Tuple of FsSchemaComplexType_Tuple
        | Dictionary of FsSchemaComplexType_Dictionary
        | Collection of FsSchemaComplexType_Collection
    with 
        member x.GetElementName() =
            match x with 
            | Tuple v -> v.InnerElementName
            | Dictionary _ -> failwithf "Not implemented"
            | Collection _ -> failwithf "Not implemented"

        member x.GetSchemaTypeOrSchemaTypeName() =
            match x with 
            | Collection v -> v.GetSchemaTypeOrSchemaTypeName()
            | Dictionary v -> v.GetSchemaTypeOrSchemaTypeName()
            | Tuple v -> v.GetSchemaTypeOrSchemaTypeName()

        member x.ToSchemas() =
            match x with 
            | Tuple v      -> [v.ToSchema()]
            | Dictionary v -> v.ToSchemas()
            | Collection v -> [v.ToSchema()]


    type FsSchemaComplexType_Union_OneFieldCase =
        { Field: PropertyInfo
          Element: FsXmlSchemaElement 
          ElementSchemaType: FsSchemaType
          UnionCase: UnionCaseInfo }
    with 
        static member Create(uci: UnionCaseInfo, field, elementSchemaType: FsSchemaType) =
            let element =   
                elementSchemaType.GenerateFsElement(Some uci.Name)

            { Field = field 
              ElementSchemaType = elementSchemaType 
              Element = element
              UnionCase = uci
            }

        member x.ToSchema() = x.Element.ToSchema()
        

    type FsSchemaComplexType_Union_NamedCase =
        { 
            Element: FsXmlSchemaElement
            UnionCase: UnionCaseInfo
        }
    with    
        static member Create(uci: UnionCaseInfo) =
            let tpName = XmlQualifiedName("string", W3XMLSchema)
            let element = 
                { IsOption = false 
                  SchemaTypeOrSchemaTypeName = FsSchemaTypeOrSchemaTypeName.SchemaTypeName tpName
                  Name = PropNameOrElementName.PropName uci.Name }


            { UnionCase = uci 
              Element = element}

        member x.ToSchema() = x.Element.ToSchema()
            

    type FsSchemaComplexType_Union_MultipleFieldsCase =
        { Fields: PropertyInfo list
          Elements: FsXmlSchemaElement list
          ElementSchemaTypes: FsSchemaType list
          UnionCase: UnionCaseInfo }
    with 
        member x.Zip3() =
            List.zip3 x.Elements x.ElementSchemaTypes x.Fields

        static member Create(uci, fields: PropertyInfo list, elementSchemaTypes: FsSchemaType list) =
            let elements = 
                (fields, elementSchemaTypes)
                ||> List.map2(fun field elementSchemaType ->
                    elementSchemaType.GenerateFsElement(Some field.Name)
                )

            { Fields = fields 
              ElementSchemaTypes = elementSchemaTypes 
              Elements = elements
              UnionCase = uci
            }

        member x.ToSchema() =
            let propSequence = 
                createPropSequence x.Elements

            let innerType = 
                XmlSchemaComplexType(
                    Particle = propSequence
                )

            XmlSchemaElement(
                Name = x.UnionCase.Name,
                SchemaType = innerType
            )

    [<RequireQualifiedAccess>]
    type FsSchemaComplexType_UnionCase =
        | NamedCase of FsSchemaComplexType_Union_NamedCase
        | OneFieldCase of FsSchemaComplexType_Union_OneFieldCase
        | MultipleFieldsCase of FsSchemaComplexType_Union_MultipleFieldsCase
    with 
        member x.UnionCase =
            match x with 
            | NamedCase v -> v.UnionCase
            | OneFieldCase v -> v.UnionCase
            | MultipleFieldsCase v ->v.UnionCase

        member x.ToSchema() =
            match x with 
            | NamedCase           v -> v.ToSchema()
            | OneFieldCase        v -> v.ToSchema()
            | MultipleFieldsCase  v -> v.ToSchema()

    type FsSchemaComplexType_Union =
        { UnionCases: FsSchemaComplexType_UnionCase list
          Type: Type }
    with 
        member x.GetSchemaTypeOrSchemaTypeName() =
            x.Type.Name
            |> XmlQualifiedName
            |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName

        member x.ToSchema() =
            let propChoice = XmlSchemaChoice()
            for unionCase in x.UnionCases do
                let element = unionCase.ToSchema()
                propChoice.Items.Add(element)
                |> ignore

            let union = 
                let sequence = XmlSchemaSequence()
                let element = XmlSchemaElement(
                    Name = "Choice" + x.UnionCases.Length.ToString(),
                    SchemaType = XmlSchemaComplexType(Particle = propChoice)
                )
                sequence.Items.Add element |> ignore
                sequence
            
            XmlSchemaComplexType(
                Name = x.Type.Name,
                Particle = union
            )
                

    type FsSchemaComplexType_SingleCaseUnion_OneField =
        { Field: PropertyInfo
          Uci: UnionCaseInfo
          Element: FsXmlSchemaElement 
          ElementSchemaType: FsSchemaType
          Type: Type }
    with 
        static member Create(uci, tp, field, elementSchemaType: FsSchemaType) =
            let element = 
                elementSchemaType.GenerateFsElement(propName = Some "SCase")

            { Field = field 
              Uci = uci
              ElementSchemaType = elementSchemaType 
              Element = element
              Type = tp
            }

        member x.ToSchema() =
            let propSequence = createPropSequence [x.Element]
            XmlSchemaComplexType(
                Name = x.Type.Name,
                Particle = propSequence
            )

    type FsSchemaComplexType_SingleCaseUnion_MultipleFields =
        { Fields: PropertyInfo list
          Elements: FsXmlSchemaElement list
          Uci: UnionCaseInfo
          ElementSchemaTypes: FsSchemaType list
          Type: Type }
    with 
        member x.Zip3() =
            List.zip3 x.Elements x.ElementSchemaTypes x.Fields

        static member Create(uci, tp, fields: PropertyInfo list, elementSchemaTypes: FsSchemaType list) =
            let elements = 
                (fields, elementSchemaTypes)
                ||> List.map2(fun field elementSchemaType ->
                    elementSchemaType.GenerateFsElement(Some field.Name)
                )

            { Fields = fields
              Uci = uci
              ElementSchemaTypes = elementSchemaTypes 
              Elements = elements
              Type = tp
            }

        member x.ToSchema() =
            let innerType = 
                let propSequence = 
                    createPropSequence x.Elements

                XmlSchemaComplexType(
                    Particle = propSequence
                )

            let element = 
                XmlSchemaElement(
                    Name = "SCase",
                    SchemaType = innerType
                )

            let propSequence = 
                let sequence = XmlSchemaSequence()
                sequence.Items.Add(element) |> ignore
                sequence

            XmlSchemaComplexType(
                Name = x.Type.Name,
                Particle = propSequence
            )
            

    [<RequireQualifiedAccess>]
    type FsSchemaComplexType_SingleCaseUnion =
        | OneField of FsSchemaComplexType_SingleCaseUnion_OneField
        | MultipleFields of FsSchemaComplexType_SingleCaseUnion_MultipleFields
    with 
        member internal x.Uci() =   
            match x with 
            | OneField v -> v.Uci
            | MultipleFields v -> v.Uci

        member x.ToSchema() =
            match x with 
            | OneField       v -> v.ToSchema()
            | MultipleFields v -> v.ToSchema()

        member private x.Tp() =
            match x with 
            | OneField v -> v.Type
            | MultipleFields v -> v.Type

        member x.GetElementName() =
            x.Tp().Name

        member x.GetSchemaTypeOrSchemaTypeName() =
            x.Tp().Name
            |> XmlQualifiedName
            |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName

    type FsSchemaComplexType_Record =
        { Elements: FsXmlSchemaElement list
          ElementSchemaTypes: FsSchemaType list
          PropertyInfos: PropertyInfo list
          Type: Type }
    with 
        member x.Zip3() =
            List.zip3 x.Elements x.ElementSchemaTypes x.PropertyInfos

        member x.ToSchema() =
            let propSequence = createPropSequence x.Elements

            XmlSchemaComplexType(
                Name = x.Type.Name,
                Particle = propSequence
            )
            
        member x.GetSchemaTypeOrSchemaTypeName() =
            x.Type.Name
            |> XmlQualifiedName
            |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName

    [<RequireQualifiedAccess>]
    type FsSchemaComplexType =
        | Generic of FsSchemaComplexGenericType
        | Union of FsSchemaComplexType_Union
        | SinglecaseUnion of FsSchemaComplexType_SingleCaseUnion
        | Record of FsSchemaComplexType_Record
    with 
        member x.GetAsGeneric() =
            match x with 
            | SinglecaseUnion _ 
            | Record _
            | Union _ -> None
            | Generic v -> Some v
                

        member x.ToSchemas() =
            match x with 
            | Generic             v  -> v.ToSchemas()
            | Union               v  -> v.ToSchema()  |> List.singleton
            | SinglecaseUnion     v  -> v.ToSchema()  |> List.singleton
            | Record              v  -> v.ToSchema()  |> List.singleton

        member x.GetSchemaTypeOrSchemaTypeName() =  
            match x with 
            | Generic v -> v.GetSchemaTypeOrSchemaTypeName()
            | Record v ->  v.GetSchemaTypeOrSchemaTypeName()
            | SinglecaseUnion v -> v.GetSchemaTypeOrSchemaTypeName()
            | Union v -> v.GetSchemaTypeOrSchemaTypeName()

        member x.GetElementName() =
            match x with 
            | Generic v -> v.GetElementName()
            | Record v ->  v.Type.Name
            | SinglecaseUnion v -> v.GetElementName()
            | Union v -> v.Type.Name

    type FsSchemaSimpleType_Enum = FsSchemaSimpleType_Enum of Type
    with 


        member x.EnumType =
            let (FsSchemaSimpleType_Enum v) = x
            v

        member x.ToSchema() =
            let (FsSchemaSimpleType_Enum tp) = x
            let content = 
                XmlSchemaSimpleTypeRestriction(
                    BaseTypeName = new XmlQualifiedName("string", W3XMLSchema)
                )

            for enumName in System.Enum.GetNames(tp) do
                content.Facets.Add(
                    XmlSchemaEnumerationFacet(
                        Value = enumName
                    )
                )
                |> ignore

            XmlSchemaSimpleType(
                Name = tp.Name,
                Content = content
            )

        member enumType.GetSchemaTypeOrSchemaTypeName() =
            XmlQualifiedName(enumType.EnumType.Name)
            |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName

        member enumType.GetElementName(): string = 
            enumType.EnumType.Name


    type FsSchemaSimpleType_Value = FsSchemaSimpleType_Value of TypeCode
    with 
        member x.ToSchema() =
            None

        member x.TypeCode =
            let (FsSchemaSimpleType_Value v) = x
            v

        member tpCode.GetSchemaTypeOrSchemaTypeName() =
            let tpCode =    
                FsTypeCodeEx.ValueTypeCodeToXmlTypeName tpCode.TypeCode
               
            XmlQualifiedName(tpCode, W3XMLSchema)
            |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName


        member tpCode.GetElementName(): string = 
            let tpCode =    
                FsTypeCodeEx.ValueTypeCodeToXmlTypeName tpCode.TypeCode
               
            tpCode
            //toTitleCase tpCode


    [<RequireQualifiedAccess>]
    type FsSchemaSimpleType =
        | EnumType of FsSchemaSimpleType_Enum
        | ValueType of FsSchemaSimpleType_Value
    with 
        member x.GetSchemaTypeOrSchemaTypeName() =
            match x with 
            | FsSchemaSimpleType.ValueType tpCode -> tpCode.GetSchemaTypeOrSchemaTypeName()
            | FsSchemaSimpleType.EnumType enumType -> enumType.GetSchemaTypeOrSchemaTypeName()


        member x.GetElementName(): string = 
            match x with 
            | FsSchemaSimpleType.ValueType tpCode -> tpCode.GetElementName()
            | FsSchemaSimpleType.EnumType enumType -> enumType.GetElementName()

        member x.ToSchema(): XmlSchemaType option =
            match x with 
            | EnumType v -> 
                (v.ToSchema() :> XmlSchemaType)
                |> Some

            | ValueType _ -> None


    type MappedFsSchemaType =
        { FsSchemaType: FsSchemaType
          TypeMappingPair: LinkableFsXmlSerializerTypeMappingPair }
    with 
        //member x.TypeMapping = x.TypeMappingPair.TypeMapping

        member x.WrapOldName = x.TypeMappingPair.WrapOldName

        member x.OriginType = x.TypeMappingPair.OriginType


        member x.GetElementName() = 
            match x.WrapOldName with 
            | false -> x.FsSchemaType.GetElementName()
            | true -> x.OriginType.Name

        member x.GetSchemaTypeOrSchemaTypeName() =
            match x.WrapOldName with 
            | false -> x.FsSchemaType.GetSchemaTypeOrSchemaTypeName()
            | true -> 
                x.OriginType.Name
                |> XmlQualifiedName
                |> FsSchemaTypeOrSchemaTypeName.SchemaTypeName


        member x.ToSchemas() =
            match x.WrapOldName with 
            | false -> x.FsSchemaType.ToSchemas()
            | true -> 
                let innerSchemas = x.FsSchemaType.GetSchemaTypeOrSchemaTypeName()
                let originTpName = x.GetElementName()

                let fsSchemaElement = 
                    {
                        Name = PropNameOrElementName.PropName (originTpName)
                        IsOption = false
                        SchemaTypeOrSchemaTypeName = innerSchemas
                    }

                let element = fsSchemaElement.ToSchema()
                let propSequence = 
                    XmlSchemaSequence()

                propSequence.Items.Add(element)
                |> ignore

                let schemaType = 
                    XmlSchemaComplexType(
                        Name = originTpName,
                        Particle = propSequence
                    ) :> XmlSchemaType

                schemaType
                |> List.singleton
                //failwithf "Not implemented"

    [<RequireQualifiedAccess>]
    type FsSchemaType =
        | ComplexType of FsSchemaComplexType
        | Option of FsSchemaType
        | SimpleType of FsSchemaSimpleType
        | MappedType of MappedFsSchemaType
    with 
        member x.GetAsGeneric() =
            match x with 
            | SimpleType _ -> None
            | Option v -> v.GetAsGeneric()
            | ComplexType v -> v.GetAsGeneric()
            | MappedType (v) -> 
                match v.WrapOldName with 
                | true -> None
                | false -> v.FsSchemaType.GetAsGeneric() 

        member x.ToSchemas(): XmlSchemaType list =
            match x with 
            | SimpleType v -> v.ToSchema() |> Option.toList
            | Option v -> v.ToSchemas()
            | ComplexType v ->  
                (v.ToSchemas())
                |> List.map(fun m ->
                    m :> XmlSchemaType
                )

            | MappedType (v) -> v.ToSchemas() 

        member x.GetElementName(): string = 
            match x with 
            | SimpleType tp -> tp.GetElementName()
            | ComplexType tp -> tp.GetElementName()
            | Option tp -> tp.GetElementName()
            | MappedType (tp) -> tp.GetElementName()


        member x.GetSchemaTypeOrSchemaTypeName(): FsSchemaTypeOrSchemaTypeName = 
            match x with 
            | SimpleType tp -> tp.GetSchemaTypeOrSchemaTypeName()
            | ComplexType tp -> tp.GetSchemaTypeOrSchemaTypeName()
            | Option tp -> tp.GetSchemaTypeOrSchemaTypeName()
            | MappedType (tp) -> tp.GetSchemaTypeOrSchemaTypeName()


        member x.GenerateFsElement(propName: string option): FsXmlSchemaElement =   
            let isOption = x.IsOption
            let name =
                match propName with 
                | None -> x.GetElementName() |> PropNameOrElementName.ElementName
                | Some propName -> propName  |> PropNameOrElementName.PropName


            let r = 
                { IsOption = isOption 
                  Name = name 
                  SchemaTypeOrSchemaTypeName = x.GetSchemaTypeOrSchemaTypeName() }


            r
            //r
            //match propName with  
            //| Some _ -> r
            //| None ->
            //    match x.GetSchemaTypeOrSchemaTypeName() with 
            //    | FsSchemaTypeOrSchemaTypeName.SchemaTypeName _ -> r

            //    | FsSchemaTypeOrSchemaTypeName.SchemaType x ->
            //        match x.GetAsGeneric() with 
            //        | None -> r
            //        | Some generic ->
            //            match generic with 
            //            | FsSchemaComplexGenericType.Tuple _ ->
            //                match x.ToSchemas() with 
            //                | [schemaType] ->
            //                    { r with UsingSubSchemaTypeElement = true }

            //                | _ -> failwithf "Not implemented"
                            
            //            | _ -> r



        interface IFsSchemaType



    type FsXmlSerializerConfiguration with 
        member x.GetFsXmlSchemaType(tp: Type) =
            match x.FsSchemaTypeCache.TryGetValue tp with 
            | true, v -> v :?> FsSchemaType
            | false, _ -> failwithf "Cannot get FsXmlSchemaType by %A" tp


    type NamedFsSchemaType =
        { FsSchemaType: FsSchemaType 
          Name: string option }

    [<RequireQualifiedAccess>]
    module NamedFsSchemaType =
        let internal (|ElementName_AND_Tuple2|Others|) (x: NamedFsSchemaType) =
            let schemaTypeOrSchemaTypeName = x.FsSchemaType.GetSchemaTypeOrSchemaTypeName()
            let name = 
                match x.Name with 
                | None -> 
                    schemaTypeOrSchemaTypeName.GetAsXmlQualifiedName().Name
                    |> PropNameOrElementName.ElementName
                | Some name -> PropNameOrElementName.PropName name

            match (name, schemaTypeOrSchemaTypeName) with 
            | ElementName_AND_Tuple (tpType, name) -> ElementName_AND_Tuple2(tpType, name)
            | _ -> 
                Others
                //match schemaTypeOrSchemaTypeName with 
                //| FsSchemaTypeOrSchemaTypeName.SchemaType schemaTp ->
                //    match schemaTp.GetAsGeneric() with 
                //    | Some (FsSchemaComplexGenericType.Tuple v) ->
                //        Tuple(v)

                //    | _ -> Others

                //| _ -> Others
