// F# の詳細 (http://fsharp.net)

module LruCacheTest

open FsUnit
open NUnit.Framework
open LruCache

[<TestFixture>]
type キャッシュテスト() =
    
    let empty_cache = LruCache<_, _>()
    
    [<Test>]
    member this.必ず成功するスタブ() = true |> should be True

    [<Test>]
    member this.空のデータが登録できる() = 
        empty_cache.List |> should equal []

    member private this.IsSameElementList (xs: 'a list) (ys: 'a list) = 
        Set xs |> should equal (Set ys)
    
    // listの内容を持つLRUキャッシュをputの連続で作成する
    member private this.DataSet xs = 
        List.fold (fun cache (k, v) -> cache |> put k v) empty_cache xs

    [<Test>]
    member this.複数のデータが登録できる() = 
        let ok xs ys =
            this.IsSameElementList (xs |> this.DataSet |> to_list) ys
        ok [("key", "value")] [("key", "value")] 
//        this.IsSameElementList ((this.DataSet [("a","b"); ("c","d")]).List) [("a", "b"); ("c", "d")]
//        ((this.DataSet ["a","b"; "c","d"]).List, [("a", "b"); ("c", "d")])
//        ||> this.IsSameElementList

    [<Test>]
    member this.登録されているデータが取り出せる() =
        this.DataSet [] |> get "key" |> should equal None
        this.DataSet [("key", "value")] |> get "key" |> should equal (Some "value")

    [<Test>]
    member this.Size以上の要素を追加した場合に最初の要素が切り捨てられる() = 
        [("b", "B"); ("c", "C"); ("d", "D")] 
        |> this.IsSameElementList ((this.DataSet [("a", "A"); ("b", "B"); ("c", "C"); ("d", "D")]).List)
