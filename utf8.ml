open Core.Std

type read_state = {
    expected_len : int;
    code : int;
    buf : Uchar.t list;
}

let initial_read_state : read_state =
    {
        expected_len = 0;
        code = 0;
        buf = [];
    }

let reset_then_append ({buf; _} : read_state) (ch : int) : read_state =
    { initial_read_state with buf = buf @ [Uchar.of_int ch] }

let folder (state : read_state) (ch : char) : read_state =
    let c = int_of_char ch in
    match state.expected_len with
    | 0 when c < 0x80 -> reset_then_append state c
    | 0 when (c land 0xf0) = 0xf0 ->
        { state with expected_len = 3; code = c land 0x07 }
    | 0 when (c land 0xe0) = 0xe0 ->
        { state with expected_len = 2; code = c land 0x0f }
    | 0 when (c land 0xc0) = 0xc0 ->
        { state with expected_len = 1; code = c land 0x1f }
    | len ->
        let next_code = (state.code lsl 6) + (c land 0x3f) in
        if len = 1 then
            reset_then_append state next_code
        else
            { state with expected_len = len - 1; code = next_code }

let chars (s : string) =
    let final_state = String.fold ~init:initial_read_state ~f:folder s in
    final_state.buf
