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

type t = Cell (op, dir);

let empty = Cell (NoOp, KeepDir);

let make op dir => Cell (op, dir);
