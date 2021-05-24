#if INTERACTIVE
namespace global
#else
namespace FSharp.Collections.Immutable
#endif

// The FlatList name comes from a similar construct seen in the official F# source code
type FlatList<'T> = System.Collections.Immutable.ImmutableArray<'T>

// based on the F# Array module source
[<RequireQualifiedAccess; CompiledName((nameof System.Collections.Immutable.ImmutableArray) + "Module")>]
module FlatList =

    type internal FlatListFactory = System.Collections.Immutable.ImmutableArray

    let inline internal checkNotDefault argName (list : FlatList<'T>) =
        if list.IsDefault then invalidArg argName "Uninstantiated ImmutableArray/FlatList"
    let inline internal check (list : FlatList<'T>) = checkNotDefault (nameof list) list
    let inline internal checkEmpty (list : FlatList<_>) = check list; if list.Length = 0 then invalidArg (nameof list) "Source is empty" else ()
    let inline internal raiseOrReturn list = check list; list

    ////////// Creating //////////

    let inline empty<'T> : FlatList<_> = FlatListFactory.Create<'T>()
    let inline singleton<'T> (item : 'T) : FlatList<'T> = FlatListFactory.Create<'T> (item)
    let copy (list:FlatList<_>) = FlatListFactory.CreateRange list

    let inline ofSeq source = FlatListFactory.CreateRange source
    let inline ofArray (source : _ array) = FlatListFactory.CreateRange source
    let inline ofList (source: _ list) = FlatListFactory.CreateRange source

    let inline toSeq (flatList: FlatList<_>) = check flatList; flatList :> seq<_>
    let inline toArray (list : FlatList<_>) = check list; Seq.toArray list
    let inline toList list = check list; Seq.toList list

    ////////// Building //////////

    let moveFromBuilder (builder : FlatList<_>.Builder) : FlatList<_> =
        checkNotNull (nameof builder) builder
        builder.MoveToImmutable()
    let ofBuilder (builder : FlatList<_>.Builder) : FlatList<_> =
        checkNotNull (nameof builder) builder
        builder.ToImmutable()

    let inline builder () : FlatList<'T>.Builder = FlatListFactory.CreateBuilder()
    let inline builderWith capacity : FlatList<'T>.Builder = FlatListFactory.CreateBuilder(capacity)

    let toBuilder list: FlatList<_>.Builder = check list; list.ToBuilder()

    module Builder =
        let inline private check (builder: FlatList<'T>.Builder) = checkNotNull (nameof builder) builder

        let add item builder = check builder; builder.Add(item)

    let inline internal indexNotFound() = raise <| System.Collections.Generic.KeyNotFoundException()

    let isEmpty (list: FlatList<_>) = list.IsEmpty
    let isDefault (list: FlatList<_>) = list.IsDefault
    let isDefaultOrEmpty (list: FlatList<_>) = list.IsDefaultOrEmpty

    ////////// IReadOnly* //////////

    let length list = check list; list.Length

    let item index list = check list; list.[index]

    let append list1 list2 : FlatList<'T> =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        list1.AddRange(list2 : FlatList<_>)

    /// Searches for the specified object and returns the zero-based index of the first occurrence within the range
    /// of elements in the list that starts at the specified index and
    /// contains the specified number of elements.
    let indexRangeWith comparer index count item list =
        check list
        list.IndexOf(item, index, count, comparer)
    let indexRange index count item list =
        indexRangeWith HashIdentity.Structural index count item list
    let indexFromWith comparer index item list =
        indexRangeWith comparer index (length list - index) item
    let indexFrom index item list =
        indexFromWith HashIdentity.Structural index item list
    let indexWith comparer item list =
        indexFromWith comparer 0 item list
    let index item list = indexWith HashIdentity.Structural item list

    /// Searches for the specified object and returns the zero-based index of the last occurrence within the
    /// range of elements in the list that contains the specified number
    /// of elements and ends at the specified index.
    let lastIndexRangeWith comparer index count item list =
        check list
        list.LastIndexOf(item, index, count, comparer)
    let lastIndexRange index count item list =
        lastIndexRangeWith HashIdentity.Structural index count item list
    let lastIndexFromWith comparer index item list =
        lastIndexRangeWith comparer index (index + 1) item list
    let lastIndexFrom index item list =
        lastIndexFromWith HashIdentity.Structural index item list
    let lastIndexWith comparer item list =
        lastIndexFromWith comparer (length list - 1) item list
    let lastIndex item list = lastIndexWith HashIdentity.Structural item list

    /// Removes the specified objects from the list with the given comparer.
    let removeAllWith (comparer: System.Collections.Generic.IEqualityComparer<_>) items list: FlatList<_> =
        check list
        list.RemoveRange(items, comparer)

    /// Removes the specified objects from the list.
    let removeAll items list = removeAllWith HashIdentity.Structural items list

    /// Removes all the elements that do not match the conditions defined by the specified predicate.
    let filter predicate list: FlatList<_> =
        check list
        System.Predicate(not << predicate)
        |> list.RemoveAll

    /// Removes all the elements that do not match the conditions defined by the specified predicate.
    let where predicate list = filter predicate list

    /// Removes a range of elements from the list.
    let removeRange index (count: int) list: FlatList<_> = check list; list.RemoveRange(index, count)

    let blit source sourceIndex (destination: 'T[]) destinationIndex count =
        checkNotDefault (nameof source) source
        try source.CopyTo(sourceIndex, destination, destinationIndex, count)
        with exn -> raise exn // throw same exception with the correct stack trace. Update exception code

    let sortRangeWithComparer comparer index count list =
        check list
        list.Sort(index, count, comparer)
    let sortRangeWith comparer index count list =
        sortRangeWithComparer (ComparisonIdentity.FromFunction comparer) index count list
    let sortRange index count list = sortRangeWithComparer ComparisonIdentity.Structural index count list
    let sortWithComparer (comparer : System.Collections.Generic.IComparer<_>) list = check list; list.Sort(comparer)
    let sortWith comparer list = sortWithComparer (ComparisonIdentity.FromFunction comparer) list
    let sort list = check list; list.Sort()

    ////////// Loop-based //////////

    let inline private builderWithLengthOf list = builderWith <| length list

    let init count initializer =
        if count < 0 then invalidArg (nameof count) ErrorStrings.InputMustBeNonNegative
        let builder = builderWith count
        for i = 0 to count - 1 do
            builder.Add <| initializer i
        moveFromBuilder builder

    let initWithValue count value =
        if count < 0 then invalidArg (nameof count) ErrorStrings.InputMustBeNonNegative
        let builder = builderWith count
        for i = 0 to count - 1 do
            builder.Add value
        ofBuilder builder

    let rec private concatAddLengths (arrs: FlatList<FlatList<_>>) i acc =
        if i >= length arrs then acc
        else concatAddLengths arrs (i+1) (acc + arrs.[i].Length)

    let concat (seqs:'a seq seq) = seqs |> Seq.concat |> ofSeq

    let map mapping = raiseOrReturn >> Seq.map mapping >> ofSeq

    let countBy projection = raiseOrReturn >> Seq.countBy projection >> ofSeq

    let indexed list = list |> raiseOrReturn |> Seq.indexed |> ofSeq

    let iter action = raiseOrReturn >> Seq.iter action

    let iter2 action list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.iter2 action list1 list2

    let distinct (list: FlatList<'T>) = list |> Seq.distinct |> ofSeq

    let distinctBy projection = raiseOrReturn >> Seq.distinctBy projection >> ofSeq

    let map2 mapping list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.map2 mapping list1 list2 |> ofSeq

    let map3 mapping list1 list2 list3 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        checkNotDefault (nameof list3) list3
        Seq.map3 mapping list1 list2 list3 |> ofSeq

    let mapi2 mapping list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.mapi2 mapping list1 list2 |> ofSeq

    let iteri action = raiseOrReturn >> Seq.iteri action

    let iteri2 action list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.iteri2 action list1 list2

    let mapi mapping = raiseOrReturn >> Seq.mapi mapping >> ofSeq

    let exists predicate = raiseOrReturn >> Seq.exists predicate

    let contains e = raiseOrReturn >> Seq.contains e

    let exists2 predicate list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.exists2 predicate list1 list2

    let forall predicate = raiseOrReturn >> Seq.forall predicate

    let forall2 predicate list1 list2 =
        checkNotDefault (nameof list1) list1
        checkNotDefault (nameof list2) list2
        Seq.forall2 predicate list1 list2

    let groupBy projection = raiseOrReturn >> Seq.groupBy projection >> Seq.map (fun (k, i) -> k, ofSeq i) >> ofSeq

    let pick chooser = raiseOrReturn >> Seq.pick chooser

    let tryPick chooser = raiseOrReturn >> Seq.tryPick chooser

    let choose chooser = raiseOrReturn >> Seq.choose chooser >> ofSeq

    let partition predicate list =
        check list
        let res1 = builderWith list.Length
        let res2 = builderWith list.Length
        for i = 0 to list.Length - 1 do
            let x = list.[i]
            if predicate x then res1.Add(x) else res2.Add(x)
        ofBuilder res1, ofBuilder res2

    let find predicate = raiseOrReturn >> Seq.find predicate
    let tryFind predicate = raiseOrReturn >> Seq.tryFind predicate
    let findBack predicate = raiseOrReturn >> Seq.findBack predicate
    let tryFindBack predicate = raiseOrReturn >> Seq.tryFindBack predicate
    let findIndex predicate = raiseOrReturn >> Seq.findIndex predicate
    let findIndexBack predicate = raiseOrReturn >> Seq.findIndexBack predicate
    let tryFindIndex predicate = raiseOrReturn >> Seq.tryFindIndex predicate
    let tryFindIndexBack predicate = raiseOrReturn >> Seq.tryFindIndexBack predicate

    let fold folder (state: 'state) = raiseOrReturn >> Seq.fold folder state

    let scan folder (state: 'state) = raiseOrReturn >> Seq.scan folder state >> ofSeq

    let fold2 folder (state: 'state) (left:FlatList<'a>) (right:FlatList<'b>) =
        check left; check right
        Seq.fold2 folder state left right

    let foldBack2 folder (left:FlatList<'a>) (right:FlatList<'b>) (state:'state) =
        check left; check right
        Seq.foldBack2 folder left right state

    let foldBack folder (list:FlatList<'a>) (state: 'state) =
        check list
        Seq.foldBack folder list state

    let scanBack folder (list:FlatList<'a>) (state:'state) =
        check list
        Seq.scanBack folder list state |> ofSeq

    let unfold (generator: 'state -> ('a * 'state) option) state =
        Seq.unfold generator state |> ofSeq

    let reduce reduction = raiseOrReturn >> Seq.reduce reduction

    let reduceBack reduction = raiseOrReturn >> Seq.reduceBack reduction

    let mapFold mapping (state:'State) (list:FlatList<'T>) =
        check list
        let (items, s) = Seq.mapFold mapping state list
        ofSeq items, s

    let mapFoldBack mapping (list:FlatList<'T>) (state:'State) =
        check list
        let (i, s) = Seq.mapFoldBack mapping list state
        ofSeq i, s

    let zip (left:FlatList<_>) (right:FlatList<_>) =
        check left; check right
        Seq.zip left right |> ofSeq

    let zip3 (left:FlatList<_>) (middle:FlatList<_>) (right:FlatList<_>) =
        check left; check middle; check right
        Seq.zip3 left middle right |> ofSeq

    let unzip list =
        let left = builderWithLengthOf list
        let right = builderWithLengthOf list
        for item in list do
            left.Add <| fst item
            right.Add <| snd item
        ofBuilder left, ofBuilder right

    let unzip3 list =
        let left = builderWithLengthOf list
        let right = builderWithLengthOf list
        let middle = builderWithLengthOf list
        for item in list do
            left.Add <| fst3 item
            middle.Add <| snd3 item
            right.Add <| thd3 item
        ofBuilder left, ofBuilder middle, ofBuilder right

    let windowed windowSize = raiseOrReturn >> Seq.windowed windowSize >> Seq.map ofSeq >> ofSeq

    let fill target targetIndex count value =
        mapi (fun i a -> if targetIndex <= i && i < targetIndex + count then value else a) target
        |> ofSeq

    ////////// Based on other operations //////////

    let take count list = removeRange count (length list - count) list

    let inline private lengthWhile predicate list =
        check list
        let mutable count = 0
        while count < list.Length && predicate list.[count] do
            count <- count + 1
        count
    let takeWhile predicate list = take (lengthWhile predicate list) list

    let skip index list = removeRange 0 index list

    let skipWhile predicate list = skip (lengthWhile predicate list) list

    let sub start stop list = skip start list |> take (stop - start - 1)

    let truncate count list = if count < length list then take count list else list

    let splitAt index list = take index list, skip index list

    let head list = item 0 list

    let tryItem index list =
        if index >= length list || index < 0 then None
        else Some(list.[index])

    let tryHead list = tryItem 0 list

    let last (list : FlatList<_>) = list.[length list - 1]

    let tryLast list = tryItem (length list - 1) list

    let tail list = skip 1 list

    let tryTail list = if isEmpty list then None else Some <| tail list

    let create = initWithValue

    let replicate item = item |> flip initWithValue

    let collect mapping list = concat <| map mapping list

    let inline build f =
        let builder = builder()
        f builder
        moveFromBuilder builder

    let inline update f list =
        let builder = toBuilder list
        f builder
        moveFromBuilder builder

    let inline sum ( list:FlatList< ^T > when ^T : (static member (+) : ^T * ^T -> ^T) and ^T : (static member Zero : ^T) ) =
        list |> raiseOrReturn |> reduce (+)

    let inline sumBy projection ( list:FlatList< ^T > when ^T : (static member (+) : ^T * ^T -> ^T) and ^T : (static member Zero : ^T) ) =
        list |> raiseOrReturn |> map projection |> reduce (+)

    let inline average ( list:FlatList< ^T > when ^T : (static member (+) : ^T * ^T -> ^T) and ^T : (static member DivideByInt : ^T*int -> ^T) and ^T : (static member Zero : ^T) ) =
        list |> raiseOrReturn |> applyOverFuncs LanguagePrimitives.DivideByInt sum length

    let inline averageBy projection ( list:FlatList< ^T > when ^T : (static member (+) : ^T * ^T -> ^T) and ^T : (static member DivideByInt : ^T*int -> ^T) and ^T : (static member Zero : ^T) ) =
        list |> raiseOrReturn |> applyOverFuncs LanguagePrimitives.DivideByInt ((map projection) >> sum) length

    let maxBy projection (list:FlatList<'a> when 'a : comparison) = list |> raiseOrReturn |> Seq.map projection |> Seq.reduce max
    let minBy projection (list:FlatList<'a> when 'a : comparison) = list |> raiseOrReturn |> Seq.map projection |> Seq.reduce min
    let max (list:FlatList<'a> when 'a : comparison) = list |> raiseOrReturn |> reduce max
    let min (list:FlatList<'a> when 'a : comparison) = list |> raiseOrReturn |> reduce min

    let sortBy projection = sortWith (applyOverArgs LanguagePrimitives.GenericComparison projection)
    let sortDescending (list:FlatList<'a>) = sortWith (flip LanguagePrimitives.GenericComparison) list
    let sortByDescending projection = sortWith (flip (applyOverArgs LanguagePrimitives.GenericComparison projection))

    let compareWith comparer (left:FlatList<'a>) (right:FlatList<'b>) = zip left right |> Seq.skipWhile ((uncurry comparer) >> ((=) 0)) |> Seq.head |> (uncurry comparer)

    let tryExactlyOne (list:FlatList<_>) = Seq.tryExactlyOne list
    let exactlyOne (list:FlatList<_>) = Seq.exactlyOne list

    let rev (list:FlatList<_>) = list |> raiseOrReturn |> Seq.rev |> ofSeq
    let transpose (list:FlatList<_>) = list |> raiseOrReturn |> Seq.transpose |> Seq.map ofSeq |> ofSeq
    let permute indexMap (list:FlatList<_>) = list |> raiseOrReturn |> Seq.permute indexMap |> ofSeq
    let pairwise (list:FlatList<_>) = list |> raiseOrReturn |> Seq.pairwise |> ofSeq
    let except itemsToExclude (list:FlatList<_>) = list |> raiseOrReturn |> Seq.except itemsToExclude |> ofSeq
    let splitInto count (list:FlatList<_>) = list |> raiseOrReturn |> Seq.splitInto count |> Seq.map ofSeq |> ofSeq
    let chunkBySize chunkSize (list:FlatList<_>) = list |> raiseOrReturn |> Seq.chunkBySize chunkSize |> Seq.map ofSeq |> ofSeq
    let allPairs (left:FlatList<'a>) (right:FlatList<'b>) = Seq.allPairs (raiseOrReturn left) (raiseOrReturn right) |> ofSeq

    //////////

module ImmutableArray = FlatList
