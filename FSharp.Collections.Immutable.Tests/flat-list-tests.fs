module FSharp.Collections.Immutable.Tests

open NUnit.Framework
open FSharp.Collections.Immutable

let nullList<'a> = new FlatList<'a>()
let emptyList<'a> = FlatList<'a>.Empty
let list0to9 = FlatList.init 10 id
let noopPredicate _ = true
let ignore2 _ _ = ()
let ignore3 _ _ _ = ()

let throwsOn item func =
    fun () ->
        Assert.Catch (fun () -> func item)
        |> ignore

let doesNotThrowOn item func = fun () -> Assert.DoesNotThrow (fun () -> func item)

let testCase name category (code:unit->unit) = ((TestCaseData code).SetName (category + " - " + name)).SetCategory category

let acceptsAllValues func category = [
    testCase "throws on null list" category ((func >> ignore) |> doesNotThrowOn nullList<int>)
    testCase "does not throw on empty list" category ((func >> ignore) |> doesNotThrowOn emptyList<int>)
    testCase "does not throw on non-empty list" category ((func >> ignore) |> doesNotThrowOn list0to9)
]

let throwsOnlyOnNullLists func category = [
    testCase "throws on null list" category ((func >> ignore) |> throwsOn nullList<int>)
    testCase "does not throw on empty list" category ((func >> ignore) |> doesNotThrowOn emptyList<int>)
    testCase "does not throw on non-empty list" category ((func >> ignore) |> doesNotThrowOn list0to9)
]

let doesNotThrowOnlyOnFilledList func category = [
    testCase "throws on null list" category ((func >> ignore) |> throwsOn nullList<int>)
    testCase "throws on empty list" category ((func >> ignore) |> throwsOn emptyList<int>)
    testCase "does not throw on non-empty list" category ((func >> ignore) |> doesNotThrowOn list0to9)
]

let expectType<'a> value = Assert.IsInstanceOf<'a> value
let expectValue expected actual = Assert.AreEqual (expected, actual)
let expectToBeEquivalentTo expected actual = CollectionAssert.AreEquivalent (expected, actual)

let appliedTo value func = fun () -> func value
let produces value func =
    fun () ->
        let a = func ()
        expectValue value a

let producesEquivalentOf value func =
    fun () ->
        () |> func |> expectToBeEquivalentTo value

let throws func = fun () -> Assert.Catch (func >> ignore) |> ignore

type FlatListFixture () =
    static member validationCases = Seq.concat [
        "toSeq" |> (FlatList.toSeq |> throwsOnlyOnNullLists)
        "toArray" |> (FlatList.toArray |> throwsOnlyOnNullLists)
        "toList" |> (FlatList.toList |> throwsOnlyOnNullLists)
        "length" |> (FlatList.length |> throwsOnlyOnNullLists)
        "append (first arg)" |> ((fun a -> FlatList.append a emptyList<int> |> ignore) |> throwsOnlyOnNullLists)
        "append (second arg)" |> ((FlatList.append emptyList<int>) |> throwsOnlyOnNullLists)
        "indexFromWith" |> ((FlatList.indexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> throwsOnlyOnNullLists)
        "indexFrom" |> ((FlatList.indexFrom 0 1) |> throwsOnlyOnNullLists)
        "indexWith" |> ((FlatList.indexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> throwsOnlyOnNullLists)
        "index" |> ((FlatList.index 1) |> throwsOnlyOnNullLists)
        "removeAllWith" |> ((FlatList.removeAllWith LanguagePrimitives.FastGenericEqualityComparer emptyList) |> throwsOnlyOnNullLists)
        "removeAll" |> ((FlatList.removeAll emptyList) |> throwsOnlyOnNullLists)
        "filter" |> ((FlatList.filter noopPredicate) |> throwsOnlyOnNullLists)
        "where" |> ((FlatList.where noopPredicate) |> throwsOnlyOnNullLists)
        "sortWithComparer" |> ((FlatList.sortWithComparer LanguagePrimitives.FastGenericComparer) |> throwsOnlyOnNullLists)
        "sortWith" |> ((FlatList.sortWith LanguagePrimitives.GenericComparison) |> throwsOnlyOnNullLists)
        "sort" |> (FlatList.sort |> throwsOnlyOnNullLists)
        "map" |> (FlatList.map noopPredicate |> throwsOnlyOnNullLists)
        "countBy" |> (FlatList.countBy noopPredicate |> throwsOnlyOnNullLists)
        "indexed" |> (FlatList.indexed |> throwsOnlyOnNullLists)
        "iter" |> (FlatList.iter ignore |> throwsOnlyOnNullLists)
        "iter2 (first arg)" |> ((fun a -> FlatList.iter2 ignore2 a emptyList) |> throwsOnlyOnNullLists)
        "iter2 (second arg)" |> ((FlatList.iter2 ignore2 emptyList) |> throwsOnlyOnNullLists)
        "distinct" |> (FlatList.distinct |> throwsOnlyOnNullLists)
        "distinctBy" |> (FlatList.distinctBy noopPredicate |> throwsOnlyOnNullLists)
        "map2 (first arg)" |> ((fun a -> FlatList.map2 ignore2 a emptyList) |> throwsOnlyOnNullLists)
        "map2 (second arg)" |> ((FlatList.map2 ignore2 emptyList) |> throwsOnlyOnNullLists)
        "map3 (first arg)" |> ((fun a -> FlatList.map3 ignore3 a emptyList emptyList) |> throwsOnlyOnNullLists)
        "map3 (second arg)" |> ((fun a -> FlatList.map3 ignore3 a emptyList emptyList) |> throwsOnlyOnNullLists)
        "map3 (third arg)" |> ((FlatList.map3 ignore3 emptyList emptyList) |> throwsOnlyOnNullLists)
        "mapi2 (first arg)" |> ((fun a -> FlatList.mapi2 ignore3 a emptyList) |> throwsOnlyOnNullLists)
        "mapi2 (second arg)" |> ((FlatList.mapi2 ignore3 emptyList) |> throwsOnlyOnNullLists)
        "iteri" |> ((FlatList.iteri ignore2) |> throwsOnlyOnNullLists)
        "iteri2 (first arg)" |> ((fun a -> FlatList.iteri2 ignore3 a emptyList) |> throwsOnlyOnNullLists)
        "iteri2 (second arg)" |> ((FlatList.iteri2 ignore3 emptyList) |> throwsOnlyOnNullLists)
        "mapi" |> ((FlatList.mapi ignore2) |> throwsOnlyOnNullLists)

        "item" |> ((FlatList.item 0) |> doesNotThrowOnlyOnFilledList)
        "indexRangeWith" |> ((FlatList.indexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList)
        "indexRange" |> ((FlatList.indexRange 0 1 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndexRangeWith" |> ((FlatList.lastIndexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndexRange" |> ((FlatList.lastIndexRange 0 1 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndexFromWith" |> ((FlatList.lastIndexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndexFrom" |> ((FlatList.lastIndexFrom 0 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndexWith" |> ((FlatList.lastIndexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> doesNotThrowOnlyOnFilledList)
        "lastIndex" |> ((FlatList.lastIndex 1) |> doesNotThrowOnlyOnFilledList)
        "removeRange" |> ((FlatList.removeRange 0 1) |> doesNotThrowOnlyOnFilledList)
        "blit" |> ((fun a -> FlatList.blit a 0 [|10;11;12|] 0 1) |> doesNotThrowOnlyOnFilledList)
        "sortRangeWithComparer" |> ((FlatList.sortRangeWithComparer LanguagePrimitives.FastGenericComparer 0 1) |> doesNotThrowOnlyOnFilledList)
        "sortRangeWith" |> ((FlatList.sortRangeWith LanguagePrimitives.GenericComparison 0 1) |> doesNotThrowOnlyOnFilledList)
        "sortRange" |> ((FlatList.sortRange 0 1) |> doesNotThrowOnlyOnFilledList)
    ]

    static member operationCases = [
        (FlatList.isEmpty |> appliedTo emptyList |> produces true) |> testCase "empty list is" "isEmpty"
        (FlatList.isEmpty |> appliedTo list0to9 |> produces false) |> testCase "non-empty list is not" "isEmpty"
        (FlatList.isDefault |> appliedTo nullList |> produces true) |> testCase "null list is" "isDefault"
        (FlatList.isDefault |> appliedTo emptyList |> produces false) |> testCase "empty list is not" "isDefault"
        (FlatList.isDefault |> appliedTo list0to9 |> produces false) |> testCase "non-empty list is not" "isDefault"
        (FlatList.isDefaultOrEmpty |> appliedTo nullList |> produces true) |> testCase "null list is" "isDefaultOrEmpty"
        (FlatList.isDefaultOrEmpty |> appliedTo emptyList |> produces true) |> testCase "empty list is" "isDefaultOrEmpty"
        (FlatList.isDefaultOrEmpty |> appliedTo list0to9 |> produces false) |> testCase "non-empty list is not" "isDefaultOrEmpty"
        (FlatList.length |> appliedTo emptyList |> produces 0) |> testCase "for empty list is 0" "length"
        (FlatList.length |> appliedTo list0to9 |> produces 10) |> testCase "for non-empty list is .length" "length"
        (FlatList.item 0 |> appliedTo emptyList |> throws) |> testCase "throws for empty list" "item"
        (FlatList.item 0 |> appliedTo list0to9 |> produces 0) |> testCase "[0] for non-empty list equals to [0]" "item"
        (FlatList.item 5 |> appliedTo list0to9 |> produces 5) |> testCase "[5] for non-empty list equals to [5]" "item"
        (FlatList.item -1 |> appliedTo list0to9 |> throws) |> testCase "[-1] for non-empty list throws" "item"
        (FlatList.item 25 |> appliedTo list0to9 |> throws) |> testCase "[out of bounds] for non-empty list throws" "item"
        (FlatList.append emptyList |> appliedTo list0to9 |> producesEquivalentOf [0..9]) |> testCase "empty to non-empty" "append"
        (FlatList.append list0to9 |> appliedTo emptyList |> producesEquivalentOf [0..9]) |> testCase "non-empty to empty" "append"
        (FlatList.append list0to9 |> appliedTo list0to9 |> producesEquivalentOf (List.append [0..9] [0..9])) |> testCase "non-empty to non-empty" "append"
    ]

    [<TestCaseSource(nameof(FlatListFixture.validationCases))>]
    member this.testParameterValidation (code:unit->unit) = code ()

    [<TestCaseSource(nameof(FlatListFixture.operationCases))>]
    member this.testOperationResult (code:unit->unit) = code ()
