open Core.Std

type t = Aheui_cell.t array array
[@@deriving sexp]

let width (space:t) =
    match Array.max_elt ~cmp:(fun a b -> compare (Array.length a) (Array.length b)) space with
    | Some x -> Array.length x
    | None   -> 0

let height (space:t) = Array.length space

let fill_blanks (width:int) (space:t) : t =
    let fill row =
        let blank = Array.init (width - Array.length row) ~f:(fun _ -> Aheui_cell.empty) in
        Array.append row blank in
    Array.map ~f:fill space

let cell_at (x, y) (space:t) =
    space.(y).(x)
