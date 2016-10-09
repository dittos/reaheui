open Core.Std

type dir =
    | SetDir of int * int
    | FlipX
    | FlipY
    | FlipXY
    | KeepDir
    [@@deriving sexp]

type op =
    | DivOp | AddOp | MulOp | ModOp | DupOp | SwitchOp of int | MoveOp of int
    | CmpOp | BranchOp | SubOp | SwapOp | ExitOp | PrintNumOp
    | PrintCharOp | PopOp | InputNumOp | InputCharOp | PushOp of int | NoOp
    [@@deriving sexp]

type t = Cell of op * dir
[@@deriving sexp]

let empty = Cell(NoOp, KeepDir)

let make op dir = Cell(op, dir)
