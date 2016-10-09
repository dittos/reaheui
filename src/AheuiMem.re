open Core.Std;

let module Storage = {
  type t =
    | Stack (list int)
    | Queue (list int)
  [@@deriving sexp];
  let unwrap (Stack xs | Queue xs) => xs;
  let map f s =>
    switch s {
    | Stack xs => Stack (f xs)
    | Queue xs => Queue (f xs)
    };
  let peek s =>
    switch (unwrap s) {
    | [hd, ..._] => hd
    | _ => 0
    };
  let pop = map List.tl_exn;
  let push x s =>
    switch s {
    | Stack xs => Stack [x, ...xs]
    | Queue xs => Queue (List.append xs [x])
    };
  let swap = map (
    fun s =>
      switch s {
      | [x, y, ...ys] => [y, x, ...ys]
      | _ => raise (Failure "swap")
      }
  );
  let dup = map (
    fun s =>
      switch s {
      | [hd, ...tl] => [hd, hd, ...tl]
      | _ => raise (Failure "dup")
      }
  );
  let size s => List.length (unwrap s);
};

type t = {storages: array Storage.t, mutable currentStorageIndex: int};

let init: t = {
  let new_storage idx => idx == 21 ? Storage.Queue [] : Storage.Stack [];
  {storages: Array.init 27 f::new_storage, currentStorageIndex: 0}
};

let current mem => mem.storages.(mem.currentStorageIndex);

let switch_to idx mem => mem.currentStorageIndex = idx;

let modify_at idx f mem => mem.storages.(idx) = f mem.storages.(idx);

let modify f mem => modify_at mem.currentStorageIndex f mem;

let peek mem => current mem |> Storage.peek;

let pop mem => {
  let x = peek mem;
  modify Storage.pop mem;
  x
};

let push_to idx x mem => modify_at idx (Storage.push x) mem;

let push x mem => modify (Storage.push x) mem;

let swap mem => modify Storage.swap mem;

let dup mem => modify Storage.dup mem;

let size mem => current mem |> Storage.size;
