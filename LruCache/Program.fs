module LruCache

type LruCache<'a, 'b>( lst: ('a*'b) list, size: int) =
    member this.Size = size
    member this.List : ('a*'b) list = lst
    new () = new LruCache<'a, 'b>( [], 3 )
    
let put k v (lst:LruCache<_, _>) =
    
    new LruCache<_, _>( (k, v)::lst.List, lst.Size)

let get k (lst: LruCache<_, _>) = 
    match List.tryFind (fun (key,_) -> k = key) lst.List with
     | Some(_,v) -> Some(v)
     | _ -> None 

let to_list (x : LruCache<_, _>) = x.List