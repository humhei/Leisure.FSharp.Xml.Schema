
[<AutoOpenAttribute>]
module Tests.OtherTypes


open Newtonsoft.Json
open System.Diagnostics

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
