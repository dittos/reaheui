/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

let () = {
  let code = In_channel.read_all Sys.argv.(1);
  let space = AheuiSpace.create (AheuiParser.parse code);
  AheuiExec.execute space
};
