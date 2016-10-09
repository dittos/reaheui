open Core.Std;

let () = {
  let code = In_channel.read_all Sys.argv.(1);
  let space = AheuiParser.parse code;
  AheuiExec.execute space
};
