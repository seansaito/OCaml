open Printf
open Io

(* Array for storing the frequency of each ASCII character *)
let freq_array = ref (Array.make 256 0);;

(* For reseting freq_array *)
let reset () = 
    freq_array := Array.make 256 0;;

(* Read each line from the input file, and count the frequency of 
 * each character *)
let count_freq line = 
    String.iter (fun x -> !freq_array.((int_of_char x)) <- 
        !freq_array.((int_of_char x)) + 1) line;;

let read_file filename = 
    let getline ch = try input_line ch with End_of_file -> "" in
    let ic = open_in filename in
    try
        let rec load_line ic = 
            let line = getline ic in
            if line = "" then () else
                begin
                    count_freq line;
                    load_line ic
                end in
        load_line ic;
        close_in ic;
     with e -> close_in ic; raise e;;

(* Create a dictionary with frequency information of each character that appears in the file *)
let rec create_dict_inner arr dict pos = 
    if pos = 256 then dict else
    let ch = char_of_int pos in
    let num = arr.(pos) in
    (* If frequency is zero, skip to next character *)
    match num with 0 -> create_dict_inner arr dict (pos + 1)
    (* If nonzero, then add to the dictionary *)
    | _ -> create_dict_inner arr ((ch, num) :: dict) (pos + 1);;

let create_dict arr = 
    let res = create_dict_inner arr [] 0 in res;;

(* Helper function for sorting the list *)
let descending x y = 
    match x, y with
    (k, v), (k', v') ->
        if v > v' then -1 else if v = v' then 0 else 1;;

(* Sort the dictionary or array *)
let sort f dict = 
    List.stable_sort f dict;;

(* Type declaration of the tree nodes *)
type 'a compress_tree = 
    Br of int * 'a compress_tree * 'a compress_tree
    | Lf of char * int;;

(* Converting the dictionary to a tree *)
(* First, converting the dictionary to an array, which will be used to make the 
 * tree, with each entry converted into a Leaf *)
let rec convert_inner dict arr pos = 
    match dict with
    [] -> arr
    | (ch, freq) :: t -> 
            arr.(pos) <- Lf(ch, freq);
            convert_inner t arr (pos + 1);;

let convert dict = 
    let arr = Array.make (List.length dict) (Lf(' ', 0)) in
    convert_inner dict arr 0;;

(* Helper function for sorting the list of leaves/branches in 
* descending order*)
let descending_tree x y = 
    match x, y with
    Lf (_, freq), Lf(_, freq') ->
        if freq > freq' then -1 else if freq = freq' then 0 else 1
    | Lf(_, freq), Br(size, _, _) ->
            if freq > size then -1 else if freq = size then 0 else 1
    | Br(size, _, _), Lf(_, freq) ->
            if size > freq then -1 else if size = freq then 0 else 1
    | Br(size, _, _), Br(size', _, _) ->
            if size > size' then -1 else if size = size' then 0 else 1;;

(* Shrinks the array by one place after combine two leaves in the 
 * original array. Used as helper function for create_tree_inner *)
let shrink arr = 
    let new_arr = Array.make (Array.length arr - 1) (Lf(' ', 0)) in
    for i = 0 to Array.length new_arr - 1 do
        new_arr.(i) <- arr.(i)
    done;
    new_arr;;

(* Combines two nodes, creating a Br. Used as helper function for
 * create_tree_inner*)
let combine x y = 
    match x, y with
    Lf(_, freq), Lf(_, freq') ->
        Br((freq + freq'), x, y);
    | Lf(_, freq), Br(size, _, _) ->
            Br((freq + size), y, x)
    | Br(size, _, _), Lf(_, freq) ->
            Br((size + freq), x, y)
    | Br(size, _, _), Br(size', _, _) ->
            Br((size + size'), x, y);;

(* Create the tree from the array of frequency information. Takes the array and
 * two indices (the last and second last positions in the array) as inputs.
 * Returns the top node of the tree *)
let rec create_tree_inner arr second_last last = 
    if Array.length arr = 1 then arr.(0)
    else
    begin
        let right = arr.(last) in
        let left = arr.(second_last) in
        let new_node = combine left right in
            arr.(second_last) <- new_node;
            let new_array = shrink arr in
            Array.stable_sort descending_tree new_array;
            create_tree_inner new_array (second_last - 1) (last - 1)
    end;;

let create_tree arr =
    let array_of_leaves = convert (sort descending (create_dict arr)) in
    let last = Array.length array_of_leaves - 1 in
    let second_last = Array.length array_of_leaves - 2 in
    let head = create_tree_inner array_of_leaves second_last last in
    head;;

(* This function gets the binary sequence of the character in the sequence*)
let rec get_binary_seq_inner ch str node = 
    match node with
    Lf (c, _) -> (String.concat "" (List.rev str), c)
    | Br (_, l, r) ->
        match l, r with
        Br (_, l', r'), Lf (ch', _) -> if ch = ch' then 
                    get_binary_seq_inner ch ("0" :: str) r
        else
            get_binary_seq_inner ch ("1" :: str) l
        | Lf(a, _), Lf(b, _) -> if a = ch then
                    get_binary_seq_inner ch ("1" :: str) l
        else
            get_binary_seq_inner ch ("0" :: str) r
        | Br (_, _, _), Br(_, _, _) ->
                match get_binary_seq_inner ch ("1" :: str) l with
                (bin_seq, res) -> if res = ch then (bin_seq, res) else
                    get_binary_seq_inner ch ("0" :: str) r;;

let push_to_o_bit o_bits ch = 
    if (int_of_string (Char.escaped ch)) = 1 then
        putbit o_bits true else putbit o_bits false;;

(* This function takes the head pointer, the output channel, and a character as
 * input. After getting the binary sequence of the character, it writes the 
 * sequence to the output channel. *)
let get_binary_seq head o_bits ch = 
    match get_binary_seq_inner ch [] head with
    (bin_seq, ch') -> String.iter (push_to_o_bit o_bits) bin_seq;;

(* The compress function for the main text. It takes the name of the input file,
 * the output channel, and the head pointer of the tree as inputs.*)
let compress_text filename o_bits head = 
    let getline ich = try input_line ich with End_of_file -> "" in
    let ich = open_in filename in
    try
        let rec read_line ich = 
            let line = getline ich in
            if line = "" then () else
                begin
                    String.iter (get_binary_seq head o_bits) line;
                    flush o_bits;
                    read_line ich
                end in
        read_line ich;
        close_in ich;
    with e -> close_in ich; raise e;;

(* The compress function for the tree. It takes the output channel and the
 * head pointer as inputs. In the file itself, a leaf is denoted as 'L', while
 * a branch is denoted as 'B' *)
let rec compress_tree och node = 
    match node with
          Lf(ch, freq) -> output_string och "L\n";
                          output_char och ch;
                          output_string och (string_of_int freq);
                          output_string och "\n";
                          true
        | Br (size, l, r) -> output_string och "B\n";
                          output_string och (string_of_int size);
                          output_string och "\n";
                          compress_tree och l;
                          compress_tree och r;;

(* This compresses EVERYTHING into the file. It takes the file
 * name as input *)
let compress_EVERYTHING source destination = 
    read_file source;
    let head = create_tree !freq_array in
    let och = open_out_bin destination in
    let o = output_of_channel och in
    let o_bits = output_bits_of_output o in
    match head with Br (size, _, _) ->
        output_string och (string_of_int size);
        output_char och '\n';
        if compress_tree och head then
            compress_text source o_bits head else ();
        close_out och;;

(* This function sees whether the input file is empty or not *)
let not_empty filename = 
    let getline ich = try input_line ich with End_of_file -> "" in
    let ich = open_in filename in
        let il = getline ich in
        let res = (il = "") in
        close_in ich; res;;

(* Main function. Takes a two file names as inputs from the command line *)
try
    match Sys.argv with
    | [|_; source; destination |] ->
            if not_empty source then begin
                print_string "Compressing...\n";
                compress_EVERYTHING source destination;
                print_string "Compressed!\n" end
            else begin
                let och = open_out_bin destination in
                print_string "Empty file, but still compressed.\n";
                close_out och end
    | _ -> printf "Usage: compress.ml <source> <destination>\n"
with
    e -> printf "Error: %s\n" (Printexc.to_string e);
    exit 1;;

