/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let module Storage = {
  type t =
    | Stack (Stack.t int)
    | Queue (Dequeue.t int)
    ;
  let peek s =>
    switch s {
    | Stack x => Stack.top x |> Option.value default::0
    | Queue x => Dequeue.peek_front x |> Option.value default::0
    };
  let pop s =>
    switch s {
    | Stack x => Stack.pop_exn x
    | Queue x => Dequeue.dequeue_front_exn x
    };
  let push value s =>
    switch s {
    | Stack x => Stack.push x value
    | Queue x => Dequeue.enqueue_back x value
    };
  let swap s =>
    switch s {
    | Stack x => {
        let a = Stack.pop_exn x;
        let b = Stack.pop_exn x;
        Stack.push x a;
        Stack.push x b
      }
    | Queue x => {
        let a = Dequeue.dequeue_front_exn x;
        let b = Dequeue.dequeue_front_exn x;
        Dequeue.enqueue_front x a;
        Dequeue.enqueue_front x b
      }
    };
  let dup s =>
    switch s {
    | Stack x => {
        let a = Stack.top_exn x;
        Stack.push x a
      }
    | Queue x => {
        let a = Dequeue.peek_front_exn x;
        Dequeue.enqueue_front x a
      }
    };
  let size s => switch s {
  | Stack x => Stack.length x
  | Queue x => Dequeue.length x
  };
};

type t = {storages: array Storage.t, mutable currentStorage: Storage.t};

let init: t = {
  let new_storage idx => switch idx {
  | 21 => Storage.Queue (Dequeue.create ())
  | _ => Storage.Stack ({
      let s = Stack.create ();
      Stack.set_capacity s 100;
      s
    })
  };
  let storages = Array.init 27 f::new_storage;
  {storages, currentStorage: storages.(0)}
};

let switch_to idx mem => mem.currentStorage = mem.storages.(idx);

let peek mem => Storage.peek mem.currentStorage;

let pop mem => Storage.pop mem.currentStorage;

let push_to idx x mem => Storage.push x mem.storages.(idx);

let push x mem => Storage.push x mem.currentStorage;

let swap mem => Storage.swap mem.currentStorage;

let dup mem => Storage.dup mem.currentStorage;

let size mem => Storage.size mem.currentStorage;
