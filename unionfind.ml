type uf_t = (((int option) * int ref) option) array * int ref;;

let make (size : int) : uf_t = 
    (Array.make size None, ref(size));;

exception NotValid
exception AlreadyThere
exception NotFound

let singleton uf_t pos = 
    match uf_t with
    (a, c) -> if Array.length a <= pos || pos < 0 then raise NotValid
    else match a.(pos) with
    None -> a.(pos) <- Some (None, ref(1))
    | _ -> raise AlreadyThere;;

let make_full (size : int) : uf_t = 
    (Array.make size (Some(None, ref(1))), ref(size));;

let rec find_inner tree pos rank_minus = 
    match tree with
    (a, c) -> match a.(pos) with
    | None -> raise NotFound
    | Some (None, _) -> pos
    | Some (Some(parent), rank) ->
            let top = find_inner tree parent (rank_minus + !rank) in
            a.(pos) <- Some (Some top, ref(!rank - rank_minus));
            top;;

let find tree pos = find_inner tree pos 0;;

let combine tree first second = 
    match tree with
    (a, c) -> match a.(second) with Some (None, rank_second) ->
        a.(second) <- Some (Some first, rank_second);
        match a.(first) with
        Some (None, rank_first) -> a.(first) <- Some (None, 
            ref(!rank_first + !rank_second));
            c := !c - 1;;

let union tree first second = 
    let first_top = find tree first in
        let second_top = find tree second in
        if first_top == second_top then () else
        match tree with (a, c) -> match a.(first_top) with
            Some (None, rank_first) -> match a.(second_top) with
            Some (None, rank_second) ->
               if !rank_first >= !rank_second then combine tree 
               first_top second_top else combine tree second_top first_top;;

let count tree = 
    match tree with (a, c) -> !c;;
