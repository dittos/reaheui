/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type t = {
  width: int,
  height: int,
  cells: array (array AheuiCell.t),
};

let width {width} => width;

let height {height} => height;

let create cells => {
  let width = {
    let folder maxWidth row => max maxWidth (Array.length row);
    Array.fold_left folder 0 cells
  };
  let height = Array.length cells;
  let fill row => {
    let blank = Array.init (width - Array.length row) (fun _ => AheuiCell.empty);
    Array.append row blank
  };
  {
    width,
    height,
    cells: Array.map fill cells,
  }
};

let cellAt (x, y) {cells} => cells.(y).(x);
