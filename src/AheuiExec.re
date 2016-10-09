open Core.Std;

type delta = (int, int);

type pos = (int, int);

type ptr = {space: AheuiSpace.t, pos: pos, delta: delta};

let current_cell {space, pos} => AheuiSpace.cellAt pos space;

let should_branch op mem =>
  switch op {
  | AheuiCell.BranchOp => AheuiMem.peek mem == 0
  | _ => false
  };

let required_elems op => {
  let module C = AheuiCell;
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
  }
};

let will_underflow op mem => AheuiMem.size mem < required_elems op;

let flip_dir (x, y) => (- x, - y);

let apply_dir (dx, dy) dir => {
  let module C = AheuiCell;
  switch dir {
  | C.SetDir (sx, sy) => (sx, sy)
  | C.FlipX => (- dx, dy)
  | C.FlipY => (dx, - dy)
  | C.FlipXY => flip_dir (dx, dy)
  | C.KeepDir => (dx, dy)
  }
};

let next_delta ({delta} as ptr) branch underflow => {
  let AheuiCell.Cell (_, dir) = current_cell ptr;
  let flip_if pred x => pred ? flip_dir x : x;
  apply_dir delta dir |> flip_if underflow |> flip_if branch
};

let wrap pos size =>
  if (pos < 0) {
    size - 1
  } else if (pos >= size) {
    0
  } else {
    pos
  };

let advance_ptr {space, pos: (x, y)} (dx, dy) => {
  let x' = wrap (x + dx) (AheuiSpace.width space);
  let y' = wrap (y + dy) (AheuiSpace.height space);
  {space, pos: (x', y'), delta: (dx, dy)}
};

let stack2 f mem => {
  let x = AheuiMem.pop mem;
  let y = AheuiMem.pop mem;
  AheuiMem.push (f y x) mem
};

let executeOp op mem => {
  let module C = AheuiCell;
  let module M = AheuiMem;
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

let execute (space: AheuiSpace.t) => {
  let rec loop ptr mem => {
    let AheuiCell.Cell (op, dir) = current_cell ptr;
    let branch = should_branch op mem;
    let underflow = will_underflow op mem;
    let delta = next_delta ptr branch underflow;
    let ptr' = advance_ptr ptr delta;
    executeOp (underflow ? AheuiCell.NoOp : op) mem;
    loop ptr' mem
  };
  loop {space, pos: (0, 0), delta: (0, 1)} AheuiMem.init
};
