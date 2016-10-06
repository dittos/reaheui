open Core.Std


module Hangul = struct
    let ga = 0xAC00
    let hih = 0xD7A3

    let test (ch:Uchar.t) : bool =
        let c = Uchar.to_int ch in
        ga <= c && c <= hih

    let decompose (ch:Uchar.t) : int * int * int =
        let c = (Uchar.to_int ch) - ga in
        (c / 28 / 21, c / 28 % 21, c % 28)
end


let valueTable = [|
    0; 2; 4; 4; 2; 5; 5; 3; 5; 7; 9; 9; 7; 9;
    9; 8; 4; 4; 6; 2; 4; 1; 3; 4; 3; 4; 4; 3
    |]


let parseOp (op:int) (value:int) =
    let module C = Aheui_cell in
    match (op, value) with
        | (2, _) -> C.DivOp
        | (3, _) -> C.AddOp
        | (4, _) -> C.MulOp
        | (5, _) -> C.ModOp
        | (8, _) -> C.DupOp
        | (9, x) -> C.SwitchOp(x)
        | (10, x) -> C.MoveOp(x)
        | (12, _) -> C.CmpOp
        | (14, _) -> C.BranchOp
        | (16, _) -> C.SubOp
        | (17, _) -> C.SwapOp
        | (18, _) -> C.ExitOp
        | (6, 21) -> C.PrintNumOp
        | (6, 27) -> C.PrintCharOp
        | (6, _) -> C.PopOp
        | (7, 21) -> C.InputNumOp
        | (7, 27) -> C.InputCharOp
        | (7, x) -> C.PushOp(valueTable.(x))
        | _ -> C.NoOp
        

let parseDir (x:int) =
    let module C = Aheui_cell in
    match x with
        | 0 -> C.SetDir(1, 0)
        | 2 -> C.SetDir(2, 0)
        | 4 -> C.SetDir(-1, 0)
        | 6 -> C.SetDir(-2, 0)
        | 8 -> C.SetDir(0, -1)
        | 12 -> C.SetDir(0, -2)
        | 13 -> C.SetDir(0, 1)
        | 17 -> C.SetDir(0, 2)
        | 18 -> C.FlipX
        | 19 -> C.FlipXY
        | 20 -> C.FlipY
        | _-> C.KeepDir


let parseChar (ch:Uchar.t) : Aheui_cell.t =
    if Hangul.test ch then
        let (first, middle, last) = Hangul.decompose ch in
        Aheui_cell.make (parseOp first last) (parseDir middle)
    else
        Aheui_cell.empty


let parseLine (line:string) : Aheui_cell.t array =
    Utf8.chars line
    |> List.map ~f:parseChar
    |> Array.of_list


let parse (code:string) : Aheui_space.t =
    let space = String.split_lines code |> List.map ~f:parseLine |> Array.of_list in
    Aheui_space.fill_blanks (Aheui_space.width space) space
