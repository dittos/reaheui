open Core.Std;

let module C = AheuiCell;

type delta = (int, int);

type pos = (int, int);

type ptr = {space: AheuiSpace.t, pos: pos, delta: delta};

let currentCell {space, pos} => AheuiSpace.cellAt pos space;

let requiredElems op =>
  switch op {
  | C.DivOp => 2
  | C.AddOp => 2
  | C.MulOp => 2
  | C.ModOp => 2
  | C.DupOp => 1
  | C.MoveOp _ => 1
  | C.CmpOp => 2
  | C.BranchOp => 1
  | C.SubOp => 2
  | C.SwapOp => 2
  | C.PrintNumOp => 1
  | C.PrintCharOp => 1
  | C.PopOp => 1
  | _ => 0
  };

let wrap pos size =>
  if (pos < 0) {
    size - 1
  } else if (pos >= size) {
    0
  } else {
    pos
  };

let executeOp op mem => {
  let module M = AheuiMem;
  let stack2 f mem => {
    let x = M.pop mem;
    let y = M.pop mem;
    M.push (f y x) mem
  };
  switch op {
  | C.InputNumOp => M.push (int_of_string (read_line ())) mem
  | C.InputCharOp => M.push (Uchar.to_int (Utf8.read_uchar stdin)) mem
  | C.PrintNumOp => print_string (string_of_int (M.pop mem))
  | C.PrintCharOp => Utf8.print_uchar (Uchar.of_int (M.pop mem))
  | C.ExitOp => exit (M.peek mem)
  | C.DivOp => stack2 (/) mem
  | C.AddOp => stack2 (+) mem
  | C.MulOp => stack2 (*) mem
  | C.ModOp => stack2 (%) mem
  | C.DupOp => M.dup mem
  | C.SwitchOp i => M.switch_to i mem
  | C.MoveOp i => M.push_to i (M.pop mem) mem
  | C.CmpOp => stack2 (fun y x => y >= x ? 1 : 0) mem
  | C.SubOp => stack2 (-) mem
  | C.SwapOp => M.swap mem
  | C.PopOp => ignore (M.pop mem)
  | C.PushOp x => M.push x mem
  | C.BranchOp => ignore (M.pop mem)
  | C.NoOp => ()
  }
};

let execute space => {
  let mem = AheuiMem.init;
  let advancePtr {space, pos: (x, y)} (dx, dy) => {
    let x' = wrap (x + dx) (AheuiSpace.width space);
    let y' = wrap (y + dy) (AheuiSpace.height space);
    {space, pos: (x', y'), delta: (dx, dy)}
  };
  let applyDir (dx, dy) dir =>
    switch dir {
    | C.SetDir (sx, sy) => (sx, sy)
    | C.FlipX => (-dx, dy)
    | C.FlipY => (dx, -dy)
    | C.FlipXY => (-dx, -dy)
    | C.KeepDir => (dx, dy)
    };
  let nextDelta ({delta} as ptr) branch underflow => {
    let dir = C.dir (currentCell ptr);
    let flip (x, y) => (-x, -y);
    let flipIf pred x => pred ? flip x : x;
    applyDir delta dir |> flipIf underflow |> flipIf branch
  };
  let rec loop ptr => {
    let op = C.op (currentCell ptr);
    let branch = switch op {
    | C.BranchOp => AheuiMem.peek mem == 0
    | _ => false
    };
    let underflow = AheuiMem.size mem < requiredElems op;
    if (not underflow) {
      executeOp op mem
    };
    let delta = nextDelta ptr branch underflow;
    loop (advancePtr ptr delta)
  };
  loop {space, pos: (0, 0), delta: (0, 1)}
};
