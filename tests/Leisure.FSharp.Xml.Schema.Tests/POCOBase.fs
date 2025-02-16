
[<AutoOpenAttribute>]
module Tests.POCOBaseTypes


open Newtonsoft.Json
open System
open System.Reflection

open System.Diagnostics
open Leisure.FSharp.Xml.Schema
open System.Collections.Concurrent

[<AutoOpen>]
module private _POCOBaseUtils = 
    let pocoBaseCtrCache = ConcurrentDictionary()
    let autoSerializationPOCOBaseCtrCache = ConcurrentDictionary()

[<AbstractClass; JsonObject(MemberSerialization.OptIn)>]
type ParameterlessPOCOBase<'T when 'T : equality and 'T : comparison>() =
    abstract member POCOKey : 'T with get

    override x.ToString() = x.POCOKey.ToString()

    override x.GetHashCode() = 
        let num = 0
        let arg_34_0 = -1640531527
        arg_34_0 + 
            (hash  x.POCOKey) + ((num <<< 6) + (num >>> 2))

    override x.Equals(y: obj) =
        match x.GetType() = y.GetType() with 
        | true ->
            let y = y :?> ParameterlessPOCOBase<'T>
            y.POCOKey = x.POCOKey

        | false -> false

    
    interface System.IEquatable<ParameterlessPOCOBase<'T>> with 
        member x.Equals(y) = x.POCOKey = (y.POCOKey) 

    interface System.Collections.IStructuralEquatable with 
        member x.Equals(y, comparer) = x.Equals(y)
           
        member x.GetHashCode(comparer) = x.GetHashCode()

    interface System.IComparable with 
        member x.CompareTo(y: obj) =
            match x.GetType() = y.GetType() with 
            | true ->
                let y = y :?> ParameterlessPOCOBase<'T>
                compare x.POCOKey y.POCOKey

            | false ->  failwithf "Cannot compare different types %s %s" (x.GetType().FullName) (y.GetType().FullName)


    interface System.IComparable<ParameterlessPOCOBase<'T>> with 
        member x.CompareTo(y) = compare x.POCOKey y.POCOKey


    interface System.Collections.IStructuralComparable with 
        member x.CompareTo(y, comparer) = (x :> System.IComparable).CompareTo(y)



    

[<AbstractClass; JsonObject(MemberSerialization.OptIn)>]
type POCOBase<'T when 'T : equality and 'T : comparison> (pocoKey: 'T) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal x.POCOKey = pocoKey

    override x.ToString() = x.POCOKey.ToString()

    override x.Equals(y: obj) =
        match x.GetType() = y.GetType() with 
        | true ->
            let y = y :?> POCOBase<'T>
            compare x.POCOKey y.POCOKey  = 0

        | false -> false

    static member private GetCtr(tp: Type) =
        pocoBaseCtrCache.GetOrAdd(tp, valueFactory = fun _ ->
            match tp.GetConstructor([|typeof<'T>|]) with 
            | null ->   
                match tp.GetConstructors(BindingFlags.Instance ||| BindingFlags.NonPublic) with 
                | null -> failwithf "[FsXMLSerialization] Constructor with param tp %s not exists" (typeof<'T>.FullName)
                | ctrs -> 
                    ctrs
                    |> Array.tryFind(fun m -> 
                        match m.GetParameters() with 
                        | [|parameter|] -> 
                            parameter.ParameterType = typeof<'T>
                        | _ -> false
                    )
                    |> function
                        | None -> failwithf "[FsXMLSerialization] Constructor with param tp %s not exists" (typeof<'T>.FullName)
                        | Some ctr -> ctr

            | ctr -> ctr
        )
        

    static member ReadXml(tp: Type, reader: System.Xml.XmlReader, config: FsXmlSerializerConfiguration): POCOBase<'T> = 
        let ctr = POCOBase<'T>.GetCtr(tp)
        
        let pocoKey = FsXmlSerializer<_>.DeserializeXmlNodeValueTo(reader, typeof<'T>, config)

        ctr.Invoke([|pocoKey|])
        |> unbox<_>
      
    //interface FsIXmlSerializableSchema<POCOBase<'T>> with 
    //    member x.WriteXml (writer: System.Xml.XmlWriter, config: FsXmlSerializerConfiguration): unit = 
    //        FsXmlSerializer<_>.SerializeXmlNodeValue(writer, pocoKey, config)
        
    //    static member ReadXml (tp, arg: System.Xml.XmlReader, config: FsXmlSerializerConfiguration): POCOBase<'T> = 
    //        POCOBase<'T>.ReadXml(tp, arg, config)

    //    static member ReadXmlObj (tp, arg: System.Xml.XmlReader, config: FsXmlSerializerConfiguration): obj = 
    //        POCOBase<'T>.ReadXml(tp, arg, config)

    //    static member SchemaType() = typeof<'T>


    interface FsIXmlSerializableTypeMapping<POCOBase<'T>, 'T> with
        member __.OfXml(tp: Type, v): POCOBase<'T> = 
            let ctr = POCOBase<'T>.GetCtr(tp)
            ctr.Invoke([|v|])
            |> unbox<_>

        member x.ToXml() = pocoKey
            
        member x.WrapOldName = true

    interface System.IComparable with 
        member x.CompareTo(y: obj) =
            match x.GetType() = y.GetType() with 
            | true ->
                let y = y :?> POCOBase<'T>
                compare x.POCOKey y.POCOKey

            | false ->  failwithf "Cannot compare different types %s %s" (x.GetType().FullName) (y.GetType().FullName)


    interface System.IComparable<POCOBase<'T>> with 
        member x.CompareTo(y) = compare x.POCOKey y.POCOKey

    interface System.IEquatable<POCOBase<'T>> with 
        member x.Equals(y) = x.POCOKey = (y.POCOKey) 

    interface System.Collections.IStructuralEquatable with 
        member x.Equals(y, comparer) = x.Equals(y)
        member x.GetHashCode(comparer) = x.GetHashCode()

    interface System.Collections.IStructuralComparable with 
        member x.CompareTo(y, comparer) = (x :> System.IComparable).CompareTo(y)


    override x.GetHashCode() = 
        let num = 0
        let arg_34_0 = -1640531527
        arg_34_0 + 
            (hash  x.POCOKey) + ((num <<< 6) + (num >>> 2))

module POCOBaseV =
    
    type V<'T> = V of 'T
    with 
        member x.Value =
            let (V v) = x
            v

[<AbstractClass; JsonObject(MemberSerialization.OptIn)>]
type POCOBaseV<'T when 'T : comparison> (v: 'T) =
    inherit POCOBase<'T>(v)

    let vv = POCOBaseV.V(v)

    [<JsonProperty>]
    member private x.V = v

    member x.VV = vv



/// must be invoke SetPOCOKey after constructor then expression
[<AbstractClass>]
type AutoSerializationPOCOBase<'T when 'T : comparison>() =
    inherit ParameterlessPOCOBase<'T>()
    let mutable pocoKeyMutableVV: POCOBaseV.V<_> option = None

    member x.VV = 
        match pocoKeyMutableVV with 
        | Some v -> v
        | None -> failwithf "%A POCOKey was not setted yet, please set it in constructor then expression" (x.GetType())


    [<JsonProperty; DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member private x.POCOKeyMutable 
        with get() = 
            match pocoKeyMutableVV with 
            | None -> Unchecked.defaultof<'T>
            | Some v -> v.Value

        and
            set(v) = 
                match pocoKeyMutableVV with 
                | None -> pocoKeyMutableVV <- Some (POCOBaseV.V v)
                | Some _ -> failwithf "%A POCOKey can only be setted only once in constructor then expression" (x.GetType())

    /// must be invoked after constructor then expression
    member x.SetPOCOKey(key) = x.POCOKeyMutable <- key

    override x.POCOKey = x.POCOKeyMutable

    static member private GetCtr(tp: Type) =
        autoSerializationPOCOBaseCtrCache.GetOrAdd(tp, valueFactory = fun _ ->
            match tp.GetConstructor([||]) with 
            | null ->   
                match tp.GetConstructors(BindingFlags.Instance ||| BindingFlags.NonPublic) with 
                | null -> failwithf "[FsXMLSerialization] Constructor with param tp %s not exists" (typeof<'T>.FullName)
                | ctrs -> 
                    ctrs
                    |> Array.tryFind(fun m -> 
                        match m.GetParameters() with 
                        | [||] -> true
                        | _ -> false
                    )
                    |> function
                        | None -> failwithf "[FsXMLSerialization] Constructor with param tp %s not exists" (typeof<'T>.FullName)
                        | Some ctr -> ctr

            | ctr -> ctr
        )

    interface FsIXmlSerializableTypeMapping<AutoSerializationPOCOBase<'T>, 'T> with
        member __.OfXml(tp: Type, v): AutoSerializationPOCOBase<'T> = 
            let ctr = AutoSerializationPOCOBase<'T>.GetCtr(tp)
            let instance = ctr.Invoke([||]) :?> AutoSerializationPOCOBase<'T>
            instance.SetPOCOKey(v)
            instance

        member x.ToXml() = x.POCOKey

        member x.WrapOldName = true
            
[<Sealed>]
type SkipComparation_Serializable<'T> [<JsonConstructor>] (value: 'T) =
    inherit POCOBase<int>(0)
    
    [<JsonProperty>]
    member x.Value = value

    override x.ToString() = value.ToString()

    interface FsIXmlSerializableTypeMapping<SkipComparation_Serializable<'T>, 'T> with
        member __.OfXml(tp: Type, v) = 
            SkipComparation_Serializable(v)

        member x.ToXml() = value

        member x.WrapOldName = false


/// Ignore case
[<Sealed>]
type StringIC (value: string)  =
    inherit POCOBase<string>(value.ToLowerInvariant())
    [<JsonProperty>]
    member x.Value: string = value

    member x.Text = value

    member x.LowerInvariantValue = x.Value.ToLowerInvariant()

    member x.Contains(stringIC: StringIC) =
        x.LowerInvariantValue.Contains(stringIC.LowerInvariantValue)

    member x.StartsWith(stringIC: StringIC) =
        x.LowerInvariantValue.StartsWith(stringIC.LowerInvariantValue)

    member x.EndsWith(stringIC: StringIC) =
        x.LowerInvariantValue.EndsWith(stringIC.LowerInvariantValue)

    member x.Length = x.Value.Length

    override x.ToString() = x.Value

    member x.Trim() = x.Value.Trim() |> StringIC

    /// Only trim once
    member x.TrimEnd(ends: StringIC) =
        match x.LowerInvariantValue.EndsWith (ends.LowerInvariantValue) with 
        | true -> 
            let text = x.Value
            text.Substring(0, text.Length - ends.Value.Length)
            |> StringIC

        | false -> ends


    interface FsIXmlSerializableTypeMapping<StringIC, string> with
        member __.OfXml(tp: Type, v) = 
            StringIC(v)

        member x.ToXml() = value

        member x.WrapOldName = false

[<RequireQualifiedAccess>]
module String =
    let asOption (value: string) =
        match value with 
        | null -> None
        | "" -> None
        | _ -> Some value

    /// "" -> Empty
    ///
    /// _ -> NotEmpty
    let (|Empty|NotEmpty|) (value: string) =
        match value with 
        | "" -> Empty
        | _ -> NotEmpty



    let (|EqualIC|_|) (a: string) (b: string) =
        if a.ToLowerInvariant() = b.ToLowerInvariant()
        then Some ()
        else None

    let (|IncludedInIC|_|) (a: string list) (b: string) =
        List.tryFind (fun (m:string) -> m.ToLowerInvariant() = b.ToLowerInvariant()) a

    let (|Contains|_|) (a: string) (b: string) =
        if b.Contains a
        then Some ()
        else None

    let (|NOT_Contains|_|) (a: string) (b: string) =
        if b.Contains a
        then None
        else Some ()



    let (|ContainsAny|_|) (a: string list) (b: string) =
        a
        |> List.exists(fun a -> b.Contains a)
        |> function
            | true -> Some ()
            | false -> None

    let (|ContainsAll|_|) (a: string list) (b: string) =
        a
        |> List.forall(fun a -> b.Contains a)
        |> function
            | true -> Some ()
            | false -> None


    let (|ContainsIC|_|) (a: string) (b: string) =
        if b.ToLowerInvariant().Contains (a.ToLowerInvariant())
        then Some ()
        else None


    let (|EndsWith|_|) (a: string) (b: string) =
        if b.EndsWith (a)
        then Some ()
        else None

    let (|EndsWithAny|_|) (a: string list) (b: string) =
        let r = 
            a
            |> List.exists(fun a ->
                b.EndsWith (a)
            )

        if r
        then Some ()
        else None


    let (|EndsWithIC|_|) (a: string) (b: string) =
        if b.ToLowerInvariant().EndsWith (a.ToLowerInvariant())
        then Some ()
        else None

    let (|StartsWith|_|) (a: string) (b: string) =
        if b.StartsWith (a)
        then Some ()
        else None

    let (|StartsWithIC|_|) (a: string) (b: string) =
        if b.ToLowerInvariant().StartsWith (a.ToLowerInvariant())
        then Some ()
        else None





[<Sealed>]
type ProductName [<JsonConstructor>] private (originStringIC, stringIC, masterProductName: ProductName option) =
    inherit POCOBase<StringIC>(stringIC)
    let v = stringIC

    [<JsonProperty>]
    member private x.OriginStringIC = originStringIC

    [<JsonProperty>]
    member x.StringIC = stringIC

    member x.Text = x.StringIC.Value

    [<JsonProperty>]
    member x.MasterProductName = masterProductName

    member x.SetMasterProductName(xlProductName) =
        match x.MasterProductName with 
        | None -> ProductName(originStringIC, stringIC, Some xlProductName)
        | Some _ -> 
            failwithf "Master product name was setted already, it can only be set once"


    new (name: string) =
        let name = name.Trim()
        let normalize(text: string) =
            text.Replace("×", "x")

        let normalizedName = normalize name

        ProductName(StringIC name, StringIC normalizedName, None)

    new (name: StringIC) =
        ProductName(name.Value)