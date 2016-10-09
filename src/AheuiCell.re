/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type dir =
  | SetDir (int, int)
  | FlipX
  | FlipY
  | FlipXY
  | KeepDir;

type op =
  | DivOp
  | AddOp
  | MulOp
  | ModOp
  | DupOp
  | SwitchOp int
  | MoveOp int
  | CmpOp
  | BranchOp
  | SubOp
  | SwapOp
  | ExitOp
  | PrintNumOp
  | PrintCharOp
  | PopOp
  | InputNumOp
  | InputCharOp
  | PushOp int
  | NoOp;

type t = {op: op, dir: dir};

let empty = {op: NoOp, dir: KeepDir};

let make op dir => {op, dir};

let op cell => cell.op;

let dir cell => cell.dir;
