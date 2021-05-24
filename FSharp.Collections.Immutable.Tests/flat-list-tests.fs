module FSharp.Collections.Immutable.Tests

open NUnit.Framework
open Fuchu
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

let throwsOnlyOnNullLists func = [
    testCase "throws on null list" ((func >> ignore) |> throwsOn nullList<int>)
    testCase "does not throw on empty list" ((func >> ignore) |> doesNotThrowOn emptyList<int>)
    testCase "does not throw on non-empty list" ((func >> ignore) |> doesNotThrowOn list0to9)
]

let doesNotThrowOnlyOnFilledList func = [
    testCase "throws on null list" ((func >> ignore) |> throwsOn nullList<int>)
    testCase "throws on empty list" ((func >> ignore) |> throwsOn emptyList<int>)
    testCase "does not throw on non-empty list" ((func >> ignore) |> doesNotThrowOn list0to9)
]

[<Tests>]
let flatListTests =
    testList "FlatList" [
        testList "toArray" (FlatList.toArray |> throwsOnlyOnNullLists)
        testList "toList" (FlatList.toList |> throwsOnlyOnNullLists)
        testList "toSeq" (FlatList.toSeq |> throwsOnlyOnNullLists)
        testList "length" (FlatList.length |> throwsOnlyOnNullLists)
        testList "append (first arg)" ((fun a -> FlatList.append a emptyList<int> |> ignore) |> throwsOnlyOnNullLists)
        testList "append (second arg)" ((FlatList.append emptyList<int>) |> throwsOnlyOnNullLists)
        testList "indexFromWith" ((FlatList.indexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> throwsOnlyOnNullLists)
        testList "indexFrom" ((FlatList.indexFrom 0 1) |> throwsOnlyOnNullLists)
        testList "indexWith" ((FlatList.indexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> throwsOnlyOnNullLists)
        testList "index" ((FlatList.index 1) |> throwsOnlyOnNullLists)
        testList "removeAllWith" ((FlatList.removeAllWith LanguagePrimitives.FastGenericEqualityComparer emptyList) |> throwsOnlyOnNullLists)
        testList "removeAll" ((FlatList.removeAll emptyList) |> throwsOnlyOnNullLists)
        testList "filter" ((FlatList.filter noopPredicate) |> throwsOnlyOnNullLists)
        testList "where" ((FlatList.where noopPredicate) |> throwsOnlyOnNullLists)
        testList "sortWithComparer" ((FlatList.sortWithComparer LanguagePrimitives.FastGenericComparer) |> throwsOnlyOnNullLists)
        testList "sortWith" ((FlatList.sortWith LanguagePrimitives.GenericComparison) |> throwsOnlyOnNullLists)
        testList "sort" (FlatList.sort |> throwsOnlyOnNullLists)
        testList "map" (FlatList.map noopPredicate |> throwsOnlyOnNullLists)
        testList "countBy" (FlatList.countBy noopPredicate |> throwsOnlyOnNullLists)
        testList "indexed" (FlatList.indexed |> throwsOnlyOnNullLists)
        testList "iter" (FlatList.iter ignore |> throwsOnlyOnNullLists)
        testList "iter2 (first arg)" ((fun a -> FlatList.iter2 ignore2 a emptyList) |> throwsOnlyOnNullLists)
        testList "iter2 (second arg)" ((FlatList.iter2 ignore2 emptyList) |> throwsOnlyOnNullLists)
        testList "distinct" (FlatList.distinct |> throwsOnlyOnNullLists)
        testList "distinctBy" (FlatList.distinctBy noopPredicate |> throwsOnlyOnNullLists)
        testList "map2 (first arg)" ((fun a -> FlatList.map2 ignore2 a emptyList) |> throwsOnlyOnNullLists)
        testList "map2 (second arg)" ((FlatList.map2 ignore2 emptyList) |> throwsOnlyOnNullLists)
        testList "map3 (first arg)" ((fun a -> FlatList.map3 ignore3 a emptyList emptyList) |> throwsOnlyOnNullLists)
        testList "map3 (second arg)" ((fun a -> FlatList.map3 ignore3 a emptyList emptyList) |> throwsOnlyOnNullLists)
        testList "map3 (third arg)" ((FlatList.map3 ignore3 emptyList emptyList) |> throwsOnlyOnNullLists)
        testList "mapi2 (first arg)" ((fun a -> FlatList.mapi2 ignore3 a emptyList) |> throwsOnlyOnNullLists)
        testList "mapi2 (second arg)" ((FlatList.mapi2 ignore3 emptyList) |> throwsOnlyOnNullLists)
        testList "iteri" ((FlatList.iteri ignore2) |> throwsOnlyOnNullLists)
        testList "iteri2 (first arg)" ((fun a -> FlatList.iteri2 ignore3 a emptyList) |> throwsOnlyOnNullLists)
        testList "iteri2 (second arg)" ((FlatList.iteri2 ignore3 emptyList) |> throwsOnlyOnNullLists)
        testList "mapi" ((FlatList.mapi ignore2) |> throwsOnlyOnNullLists)

        testList "item" ((FlatList.item 0) |> doesNotThrowOnlyOnFilledList)
        testList "indexRangeWith" ((FlatList.indexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList)
        testList "indexRange" ((FlatList.indexRange 0 1 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndexRangeWith" ((FlatList.lastIndexRangeWith LanguagePrimitives.FastGenericEqualityComparer 0 1 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndexRange" ((FlatList.lastIndexRange 0 1 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndexFromWith" ((FlatList.lastIndexFromWith LanguagePrimitives.FastGenericEqualityComparer 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndexFrom" ((FlatList.lastIndexFrom 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndexWith" ((FlatList.lastIndexWith LanguagePrimitives.FastGenericEqualityComparer 1) |> doesNotThrowOnlyOnFilledList)
        testList "lastIndex" ((FlatList.lastIndex 1) |> doesNotThrowOnlyOnFilledList)
        testList "removeRange" ((FlatList.removeRange 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "blit" ((fun a -> FlatList.blit a 0 [|10;11;12|] 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "sortRangeWithComparer" ((FlatList.sortRangeWithComparer LanguagePrimitives.FastGenericComparer 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "sortRangeWith" ((FlatList.sortRangeWith LanguagePrimitives.GenericComparison 0 1) |> doesNotThrowOnlyOnFilledList)
        testList "sortRange" ((FlatList.sortRange 0 1) |> doesNotThrowOnlyOnFilledList)

    ]
