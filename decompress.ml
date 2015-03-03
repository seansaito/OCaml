open Io

type 'a compress_tree = 
    Br of int * 'a compress_tree * 'a compress_tree
    | Lf of char * int;;

exception SomethingisWrong
(* Function for reading the tree. Takes the input channel as input and returns
 * the head pointer of the tree *)
let rec make_tree ic = 
    match input_line ic with
    "L" -> 
        let ch = input_char ic in
        let freq = int_of_string (input_line ic) in
        Lf (ch, freq)
    | "B" ->
        let size = int_of_string (input_line ic) in
        let l = make_tree ic in
        let r = make_tree ic in
            Br (size, l, r)
    | _ -> raise SomethingisWrong;;

(*
let rec decode_from_tree node head input_bits out_buffer size = 
    if size = 0 then () else
        match node with
        Lf (ch, _) -> Buffer.add_char out_buffer ch;
                  decode_from_tree head head input_bits out_buffer (size - 1);
        | Br (_, l, r) ->
            try
                if (getbit input_bits) then
                decode_from_tree l head input_bits out_buffer size else
                decode_from_tree r head input_bits out_buffer size
            with End_of_file -> ();;
*)

let rec decode_from_tree node head input_bits sl size = 
    if size = 0 then String.concat "" (List.rev sl) else
        match node with
        Lf (ch, _) -> decode_from_tree head head input_bits 
            ((Char.escaped ch) :: sl) (size - 1);
        | Br (_, l, r) ->
            try if (getbit input_bits) then
                decode_from_tree l head input_bits sl size else
                    decode_from_tree r head input_bits sl size
            with End_of_file -> String.concat "" (List.rev sl);;

let decompress source destination = 
    let in_chan = open_in_bin source in
    let i = input_of_channel in_chan in
    let input_bits = input_bits_of_input i in
    let out_chan = open_out destination in
    let size = int_of_string (input_line in_chan)  in
    let out_buffer = Buffer.create size in
    let head = make_tree in_chan in
    let res = decode_from_tree head head input_bits [] size in
    Buffer.add_string out_buffer res;
    Buffer.output_buffer out_chan out_buffer;
    close_out out_chan;
    close_in in_chan;;
   (* decode_from_tree head head input_bits out_buffer size;
    Buffer.output_buffer out_chan out_buffer;
    close_out out_chan;
    close_in in_chan;; *)
