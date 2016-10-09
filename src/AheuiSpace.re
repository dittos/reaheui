/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type t = array (array AheuiCell.t);

let width space => {
  let folder maxWidth row => max maxWidth (Array.length row);
  Array.fold_left folder 0 space
};

let height space => Array.length space;

let fillBlanks width space => {
  let fill row => {
    let blank = Array.init (width - Array.length row) (fun _ => AheuiCell.empty);
    Array.append row blank
  };
  Array.map fill space
};

let cellAt (x, y) space => space.(y).(x);
