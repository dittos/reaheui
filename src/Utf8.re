/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

type readState = {
  expectedSize: int,
  code: int,
  buf: list Uchar.t,
};

let initialReadState: readState = {
  expectedSize: 0,
  code: 0,
  buf: [],
};

let resetThenAppend {buf} ch =>
  { ...initialReadState, buf: buf @ [Uchar.of_int ch] };

let folder (state: readState) (ch: char): readState => {
  let c = int_of_char ch;
  switch state.expectedSize {
  | 0 when c < 0x80 => resetThenAppend state c
  | 0 when (c land 0xf0) == 0xf0 =>
    { ...state, code: c land 0x07 }
  | 0 when (c land 0xe0) == 0xe0 =>
    { ...state, expectedSize: 2, code: c land 0x0f }
  | 0 when (c land 0xc0) == 0xc0 =>
    { ...state, expectedSize: 1, code: c land 0x1f }
  | size =>
    let nextCode = (state.code lsl 6) + (c land 0x3f);
    if (size == 1) {
      resetThenAppend state nextCode
    } else {
      { ...state, expectedSize: size - 1, code: nextCode }
    }
  }
};

let chars s => {
  let finalState = String.fold init::initialReadState f::folder s;
  finalState.buf
};

let read_uchar chan => {
  let rec fold state => switch (In_channel.input_char chan) {
  | Some ch =>
    let nextState = folder state ch;
    switch nextState.buf {
    | [result] => result
    | _ => fold nextState
    }
  | _ => Uchar.of_int 0
  };
  fold initialReadState
};

let print_uchar c => {
  let print ch => print_char (char_of_int ch);
  let ch = Uchar.to_int c;
  if (ch < 0x80) {
    print ch
  } else if (ch < 0x0800) {
    print (0xc0 lor (ch lsr 6));
    print (0x80 lor ((ch lsr 0) land 0x3f))
  } else if (ch < 0x10000) {
    print (0xe0 lor (ch lsr 12));
    print (0x80 lor ((ch lsr 6) land 0x3f));
    print (0x80 lor ((ch lsr 0) land 0x3f))
  } else if (ch < 0x110000) {
    print (0xf0 lor (ch lsr 18));
    print (0x80 lor ((ch lsr 12) land 0x3f));
    print (0x80 lor ((ch lsr 6) land 0x3f));
    print (0x80 lor ((ch lsr 0) land 0x3f))
  } else {
    failwith (sprintf "Print error. U+%x is not a unicode code point." ch)
  }
};
