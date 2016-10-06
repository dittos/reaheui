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
    let delta' = apply_dir delta dir in
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


let execute_op op mem =
    let module C = Aheui_cell in
    match op with
    | C.InputCharOp -> Aheui_mem.push (read_uchar ()) mem
    | C.PrintNumOp -> printf "%d" (Aheui_mem.pop mem)
    | C.ExitOp -> raise Exit
    | _ -> ()

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
    let space = Aheui_parser.parse "밯망희\n아희" in
    printf "%s\n" (Sexp.to_string_hum (Aheui_space.sexp_of_t space));
    execute space
