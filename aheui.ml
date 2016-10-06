open Core.Std

type delta = int * int
type pos = int * int
type ptr = { space: Aheui_space.t; pos: pos; delta: delta; }

let current_cell { space; pos; _ } = Aheui_space.cell_at pos space

let should_branch op mem =
    match op with
    | Aheui_cell.BranchOp -> Aheui_mem.peek mem = 0
    | _ -> false

let required_elems = let module C = Aheui_cell in function
| C.DivOp -> 2
| C.AddOp -> 2
| C.MulOp -> 2
| C.ModOp -> 2
| C.DupOp -> 1
| C.MoveOp _ -> 1
| C.CmpOp -> 2
| C.BranchOp -> 1
| C.SubOp -> 2
| C.SwapOp -> 2
| C.PrintNumOp -> 1
| C.PrintCharOp -> 1
| C.PopOp -> 1
| _ -> 0

let will_underflow op mem = Aheui_mem.size mem < required_elems op

let flip_dir (x, y) = (-x, -y)

let apply_dir (dx, dy) dir =
    let module C = Aheui_cell in
    match dir with
    | C.SetDir (sx, sy) -> (sx, sy)
    | C.FlipX -> (-dx, dy)
    | C.FlipY -> (dx, -dy)
    | C.FlipXY -> flip_dir (dx, dy)
    | C.KeepDir -> (dx, dy)

let next_delta ({ delta; _ } as ptr) branch underflow =
    let Aheui_cell.Cell (_, dir) = current_cell ptr in
    let flip_if pred x = if pred then flip_dir x else x in
    apply_dir delta dir |> flip_if underflow |> flip_if branch

let wrap pos size =
    if pos < 0 then size - 1
    else if pos >= size then 0
    else pos

let advance_ptr { space; pos = (x, y); _ } (dx, dy) =
    let x' = wrap (x + dx) (Aheui_space.width space) in
    let y' = wrap (y + dy) (Aheui_space.height space) in
    { space; pos = (x', y'); delta = (dx, dy) }


let read_uchar () =
    read_line ()
    |> Utf8.chars
    |> List.hd_exn
    |> Uchar.to_int

let stack2 f mem =
    let x = Aheui_mem.pop mem in
    let y = Aheui_mem.pop mem in
    Aheui_mem.push (f y x) mem

let execute_op op mem =
    let module C = Aheui_cell in
    let module M = Aheui_mem in
    match op with
    | C.InputNumOp -> M.push (int_of_string (read_line ())) mem
    | C.InputCharOp -> M.push (read_uchar ()) mem
    | C.PrintNumOp -> print_string (string_of_int (M.pop mem))
    | C.PrintCharOp -> print_string (Utf8.to_string (Uchar.of_int (M.pop mem)))
    | C.ExitOp -> raise Exit
    | C.DivOp -> stack2 (/) mem
    | C.AddOp -> stack2 (+) mem
    | C.MulOp -> stack2 ( * ) mem
    | C.ModOp -> stack2 (%) mem
    | C.DupOp -> M.dup mem
    | C.SwitchOp i -> M.switch i mem
    | C.MoveOp i -> M.push_to i (M.pop mem) mem
    | C.CmpOp -> stack2 (fun y x -> if y >= x then 1 else 0) mem
    | C.SubOp -> stack2 (-) mem
    | C.SwapOp -> M.swap mem
    | C.PopOp -> ignore (M.pop mem)
    | C.PushOp x -> M.push x mem
    | C.BranchOp -> ignore (M.pop mem)
    | C.NoOp -> ()

let rec run_loop ptr mem =
    let Aheui_cell.Cell (op, dir) = current_cell ptr in
    let branch = should_branch op mem in
    let underflow = will_underflow op mem in
    let delta = next_delta ptr branch underflow in
    let ptr' = advance_ptr ptr delta in
    execute_op (if underflow then Aheui_cell.NoOp else op) mem;
    run_loop ptr' mem



let execute (space:Aheui_space.t) =
    run_loop { space; pos = (0, 0); delta = (1, 0) } Aheui_mem.init


let () =
    let code = In_channel.read_all Sys.argv.(1) in
    let space = Aheui_parser.parse code in
    (*fprintf stderr "%s\n" (Sexp.to_string_hum (Aheui_space.sexp_of_t space));*)
    try
        execute space
    with Exit -> ()
