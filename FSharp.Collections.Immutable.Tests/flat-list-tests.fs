namespace FSharp.Collections.Immutable.Tests

open NUnit.Framework
open FSharp.Collections.Immutable

[<AutoOpen>]
module FlatListTestsUtils =
    let nullList<'a> () = new FlatList<'a>()
    let emptyList<'a> () = FlatList<'a>.Empty
    let list0to9 () = FlatList.init 10 id
    let throwsOnlyOnNullLists category func = [
        (func |> appliedTo (nullList<int> ()) |> throws) |> testCase "throws on null list" category
        (func |> appliedTo (emptyList<int> ()) |> doesNotThrow) |> testCase "does not throw on empty list" category
        (func |> appliedTo (list0to9 ()) |> doesNotThrow) |> testCase "does not throw on non-empty list" category
    ]
    let doesNotThrowOnlyOnFilledList category func = [
        (func |> appliedTo (nullList<int> ()) |> throws) |> testCase "throws on null list" category
        (func |> appliedTo (emptyList<int> ()) |> throws) |> testCase "throws on empty list" category
        (func |> appliedTo (list0to9 ()) |> doesNotThrow) |> testCase "does not throw on non-empty list" category
    ]

type FlatListFixture () =
    static member validationCases = Seq.concat [
        FlatList.toSeq |> throwsOnlyOnNullLists (nameof(FlatList.toSeq))
        FlatList.toArray |> throwsOnlyOnNullLists (nameof(FlatList.toArray))
        FlatList.toList |> throwsOnlyOnNullLists (nameof(FlatList.toList))
        FlatList.length |> throwsOnlyOnNullLists (nameof(FlatList.length))
        (flip FlatList.append <| emptyList<int> ()) |> throwsOnlyOnNullLists (nameof(FlatList.append) + " (first arg)")
        (FlatList.append <| emptyList<int> ()) |> throwsOnlyOnNullLists (nameof(FlatList.append) + " (second arg)")
        (FlatList.indexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> throwsOnlyOnNullLists (nameof(FlatList.indexFromWith))
        (FlatList.indexFrom 0 1) |> throwsOnlyOnNullLists (nameof(FlatList.indexFrom))
        (FlatList.indexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> throwsOnlyOnNullLists (nameof(FlatList.indexWith))
        (FlatList.index 1) |> throwsOnlyOnNullLists (nameof(FlatList.index))
        (FlatList.removeAllWith LanguagePrimitives.FastGenericEqualityComparer <| emptyList ()) |> throwsOnlyOnNullLists (nameof(FlatList.removeAllWith))
        (FlatList.removeAll <| emptyList ()) |> throwsOnlyOnNullLists (nameof(FlatList.removeAll))
        (FlatList.filter noopPredicate) |> throwsOnlyOnNullLists (nameof(FlatList.filter))
        (FlatList.where noopPredicate) |> throwsOnlyOnNullLists (nameof(FlatList.where))
        (FlatList.sortWithComparer LanguagePrimitives.FastGenericComparer) |> throwsOnlyOnNullLists (nameof(FlatList.sortWithComparer))
        (FlatList.sortWith LanguagePrimitives.GenericComparison) |> throwsOnlyOnNullLists (nameof(FlatList.sortWith))
        FlatList.sort |> throwsOnlyOnNullLists (nameof(FlatList.sort))
        FlatList.map noopPredicate |> throwsOnlyOnNullLists (nameof(FlatList.map))
        FlatList.countBy noopPredicate |> throwsOnlyOnNullLists (nameof(FlatList.countBy))
        FlatList.indexed |> throwsOnlyOnNullLists (nameof(FlatList.indexed))
        FlatList.iter ignore |> throwsOnlyOnNullLists (nameof(FlatList.iter))
        (flip (FlatList.iter2 ignore2) <| (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.iter2) + " (first arg)")
        (FlatList.iter2 ignore2 (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.iter2) + " (second arg)")
        FlatList.distinct |> throwsOnlyOnNullLists (nameof(FlatList.distinct))
        FlatList.distinctBy noopPredicate |> throwsOnlyOnNullLists (nameof(FlatList.distinctBy))
        (flip (FlatList.map2 ignore2) <| (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.map2) + " (first arg)")
        (FlatList.map2 ignore2 (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.map2) + " (second arg)")
        (fun a -> FlatList.map3 ignore3 a (emptyList ()) (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.map3) + " (first arg)")
        (fun a -> FlatList.map3 ignore3 (emptyList ()) a (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.map3) + " (second arg)")
        (FlatList.map3 ignore3 (emptyList ()) (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.map3) + " (third arg)")
        (flip (FlatList.mapi2 ignore3) <| (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.mapi2) + " (first arg)")
        (FlatList.mapi2 ignore3 (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.mapi2) + " (second arg)")
        (FlatList.iteri ignore2) |> throwsOnlyOnNullLists (nameof(FlatList.iteri))
        (flip (FlatList.iteri2 ignore3) <| (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.iteri2) + " (first arg)")
        (FlatList.iteri2 ignore3 (emptyList ())) |> throwsOnlyOnNullLists (nameof(FlatList.iteri2) + " (second arg)")
        (FlatList.mapi ignore2) |> throwsOnlyOnNullLists (nameof(FlatList.mapi))

        (FlatList.item 0) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.item))
        (FlatList.indexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.indexRangeWith))
        (FlatList.indexRange 0 1 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.indexRange))
        (FlatList.lastIndexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndexRangeWith))
        (FlatList.lastIndexRange 0 1 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndexRange))
        (FlatList.lastIndexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndexFromWith))
        (FlatList.lastIndexFrom 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndexFrom))
        (FlatList.lastIndexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndexWith))
        (FlatList.lastIndex 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.lastIndex))
        (FlatList.removeRange 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.removeRange))
        (fun a -> FlatList.blit a 0 [|10;11;12|] 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.blit))
        (FlatList.sortRangeWithComparer LanguagePrimitives.FastGenericComparer 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.sortRangeWithComparer))
        (FlatList.sortRangeWith LanguagePrimitives.GenericComparison 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.sortRangeWith))
        (FlatList.sortRange 0 1) |> doesNotThrowOnlyOnFilledList (nameof(FlatList.sortRange))
    ]

    static member operationCases = [
        (FlatList.init 5 |> appliedTo (id) |> producesEquivalentOf [0..4]) |> testCase "for 5 elements yield valid list" (nameof(FlatList.init))
        (FlatList.init 0 |> appliedTo (id) |> producesEquivalentOf []) |> testCase "for 0 elements yields empty list" (nameof(FlatList.init))
        (FlatList.isEmpty |> appliedTo (emptyList ()) |> produces true) |> testCase "empty list is" (nameof(FlatList.isEmpty))
        (FlatList.isEmpty |> appliedTo (list0to9 ()) |> produces false) |> testCase "non-empty list is not" (nameof(FlatList.isEmpty))
        (FlatList.isDefault |> appliedTo (nullList ()) |> produces true) |> testCase "null list is" (nameof(FlatList.isDefault))
        (FlatList.isDefault |> appliedTo (emptyList ()) |> produces false) |> testCase "empty list is not" (nameof(FlatList.isDefault))
        (FlatList.isDefault |> appliedTo (list0to9 ()) |> produces false) |> testCase "non-empty list is not" (nameof(FlatList.isDefault))
        (FlatList.isDefaultOrEmpty |> appliedTo (nullList ()) |> produces true) |> testCase "null list is" (nameof(FlatList.isDefaultOrEmpty))
        (FlatList.isDefaultOrEmpty |> appliedTo (emptyList ()) |> produces true) |> testCase "empty list is" (nameof(FlatList.isDefaultOrEmpty))
        (FlatList.isDefaultOrEmpty |> appliedTo (list0to9 ()) |> produces false) |> testCase "non-empty list is not" (nameof(FlatList.isDefaultOrEmpty))
        (FlatList.length |> appliedTo (emptyList ()) |> produces 0) |> testCase "for empty list is 0" (nameof(FlatList.length))
        (FlatList.length |> appliedTo (list0to9 ()) |> produces 10) |> testCase "for non-empty list is .length" (nameof(FlatList.length))
        (FlatList.item 0 |> appliedTo (emptyList ()) |> throws) |> testCase "throws for empty list" (nameof(FlatList.item))
        (FlatList.item 0 |> appliedTo (list0to9 ()) |> produces 0) |> testCase "[0] for non-empty list equals to [0]" (nameof(FlatList.item))
        (FlatList.item 5 |> appliedTo (list0to9 ()) |> produces 5) |> testCase "[5] for non-empty list equals to [5]" (nameof(FlatList.item))
        (FlatList.item -1 |> appliedTo (list0to9 ()) |> throws) |> testCase "[-1] for non-empty list throws" (nameof(FlatList.item))
        (FlatList.item 25 |> appliedTo (list0to9 ()) |> throws) |> testCase "[out of bounds] for non-empty list throws" (nameof(FlatList.item))
        (FlatList.append (emptyList ()) |> appliedTo (list0to9 ()) |> producesEquivalentOf [0..9]) |> testCase "empty to non-empty" (nameof(FlatList.append))
        (FlatList.append (list0to9 ()) |> appliedTo (emptyList ()) |> producesEquivalentOf [0..9]) |> testCase "non-empty to empty" (nameof(FlatList.append))
        (FlatList.append (list0to9 ()) |> appliedTo (list0to9 ()) |> producesEquivalentOf (List.append [0..9] [0..9])) |> testCase "non-empty to non-empty" (nameof(FlatList.append))
        (FlatList.indexRangeWith HashIdentity.Structural 3 5 6 |> appliedTo (list0to9 ()) |> produces 6) |> testCase "returns index for valid args" (nameof(FlatList.indexRangeWith))
        (FlatList.indexRangeWith HashIdentity.Structural 3 15 6 |> appliedTo (list0to9 ()) |> throws) |> testCase "throws for invalid args (count)" (nameof(FlatList.indexRangeWith))
        (FlatList.indexRangeWith HashIdentity.Structural -2 5 6 |> appliedTo (list0to9 ()) |> throws) |> testCase "throws for invalid args (index)" (nameof(FlatList.indexRangeWith))
        (FlatList.indexRangeWith HashIdentity.Structural 3 5 0 |> appliedTo (list0to9 ()) |> produces -1) |> testCase "returns -1 for non-present item" (nameof(FlatList.indexRangeWith))
        (FlatList.removeAll ([] |> FlatList.ofList) |> appliedTo (list0to9 ()) |> producesEquivalentOf [0..9]) |> testCase "with empty list - returns source" (nameof(FlatList.removeAll))
        (FlatList.removeAll ([] |> FlatList.ofList) |> appliedTo (emptyList ()) |> producesEquivalentOf []) |> testCase "with empty list - returns source (even if empty)" (nameof(FlatList.removeAll))
        (FlatList.removeAll (list0to9 ()) |> appliedTo (emptyList ()) |> producesEquivalentOf []) |> testCase "takes no action for not present values" (nameof(FlatList.removeAll))
        (FlatList.removeAll (list0to9 ()) |> appliedTo (list0to9 ()) |> producesEquivalentOf []) |> testCase "removes all present items" (nameof(FlatList.removeAll))
        (FlatList.filter (fun i -> i < 4) |> appliedTo (list0to9 ()) |> producesEquivalentOf [0..3]) |> testCase "yields only valid items" (nameof(FlatList.filter))
        (FlatList.filter (fun i -> i > 100) |> appliedTo (list0to9 ()) |> producesEquivalentOf []) |> testCase "returns empty list if predicate matches no items" (nameof(FlatList.filter))
    ]

    [<TestCaseSource(nameof(FlatListFixture.validationCases))>]
    member this.testParameterValidation (code:unit->unit) = code ()

    [<TestCaseSource(nameof(FlatListFixture.operationCases))>]
    member this.testOperationResult (code:unit->unit) = code ()
