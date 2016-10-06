open Core.Std


module Storage = struct
    type t =
    | Stack of int list
    | Queue of int list

    let peek = function
    | Stack (hd :: _) -> hd
    | Queue (hd :: _) -> hd
    | _ -> 0

    let pop = function
    | Stack (_ :: tl) -> Stack tl
    | Queue (_ :: tl) -> Queue tl
    | _ -> raise (Failure "pop")

    let push x = function
    | Stack xs -> Stack (x :: xs)
    | Queue xs -> Queue (List.append xs [x])

    let swap = function
    | Stack (x :: y :: ys) -> Stack (y :: x :: ys)
    | Queue (x :: y :: ys) -> Queue (y :: x :: ys)
    | _ -> raise (Failure "swap")

    let dup = function
    | Stack (hd :: tl) -> Stack (hd :: hd :: tl)
    | Queue xs -> Queue (List.append xs [List.hd_exn xs])
    | _ -> raise (Failure "dup")

    let size = function
    | Stack xs -> List.length xs
    | Queue xs -> List.length xs
end


type t = {
    storages : Storage.t array;
    mutable current_storage_idx : int;
}

let init : t =
    let new_storage idx = if idx = 21 then Storage.Queue [] else Storage.Stack [] in
    { storages = Array.init 27 ~f:new_storage;
      current_storage_idx = 0 }

let current mem = mem.storages.(mem.current_storage_idx)

let switch idx mem = mem.current_storage_idx <- idx

let modify_at idx f mem =
    mem.storages.(idx) <- f mem.storages.(idx)

let modify f mem =
    modify_at mem.current_storage_idx f mem

let peek mem = current mem |> Storage.peek

let pop mem =
    let x = peek mem in
    modify Storage.pop mem;
    x

let push_to idx x mem = modify_at idx (Storage.push x) mem

let push x mem = modify (Storage.push x) mem

let swap mem = modify Storage.swap mem

let dup mem = modify Storage.dup mem

let size mem = current mem |> Storage.size
