(* Analysis of unionfind_unbound
 * Unlike the union function in unionfind.ml, this one has type unit -> uf_t.
 * This allows for the user to add singletons beyond the initial size of the
 * array. When this happens, the array increases at a constant rate until the
 * array size exceeds the position of the new singleton. For example, with an
 * initial array size of 100, if the user calls singleton 700, the array will
 * double in size 3 times (100 * 2, 200 * 2, 400 * 2). Observe that by having
 * the array increase at a constant rate, this process does not depend upon the 
 * user input. Just like in unionfind.ml, the part that does depend upon the
 * size of the array is copying the elements from the old array to the new one.
 * Essentially, the migration from the old array to the new one has the
 * cost of O(n), with every singleton call leading to migration costing O(1).
 * Therefore, by analysis we can observe that unionfind in unionfind_unbound
 * still enjoys amortized O(1). 
 * However, also note that this depends on the density of the array, i.e.
 * how many slots of the array are filled. Having a very sparse array (e.g.
 * 100 elements in a size 1000000 array) will cause the algorithm above
 * the above timebound. Thus this analysis assumes that the density of the array 
 * is decided beforehand and is large enough.*)


(* Type uf_t now also has a pointer to the next available slot *)
type uf_t_copy = (((int option) * int ref) option) array * int ref * int ref;;

type uf_t = uf_t_copy ref;;

let make () = 
    ref (Array.make 100 None, ref 100, ref 0);;

exception AlreadyThere;;
exception NotFound;;
exception NotValid;;

(* This function doubles the array size until it exceeds the position *)
let rec get_newarr arr pos = 
    if Array.length arr > pos then arr
    else
        let new_arr = Array.make((Array.length arr) * 2) None in
        get_newarr new_arr pos;;

(* Migrate doubles the size of the array until it exceeds the parameter of the
 * singleton call. The array thus increases at a constant rate.*)
let migrate (tree : uf_t) (pos : int) : uf_t_copy = 
    match !tree with (arr, comp, ptr) ->
        let new_arr = get_newarr arr pos in
        for i = 0 to Array.length arr - 1 do
            new_arr.(i) <- arr.(i)
        done;
        new_arr.(pos) <- Some (None, ref(1));
        let new_ptr = pos + 1 in
        (new_arr, ref(!comp + 1), ref(new_ptr));;
        
let singleton (tree : uf_t) pos = 
    if pos < 0 then raise NotValid;
    match !tree with
    (arr, comp, ptr) -> if pos >= Array.length arr then
        tree := migrate tree pos
        else match arr.(pos) with
        Some(_, _) -> raise AlreadyThere;
        | None -> arr.(pos) <- Some (None, ref(1));
            if pos == !ptr then tree := (arr, ref(!comp + 1), ref(!ptr + 1))
            else tree := (arr, ref(!comp + 1), ptr);;

let new_singleton tree = 
    match !tree with
    (arr, comp, ptr) -> if !ptr == Array.length arr then
        tree := migrate tree (!ptr) else
        arr.(!ptr) <- Some(None, ref(1));
        tree := (arr, ref(!comp + 1), ref(!ptr + 1));
        !ptr + 1;;

let make_full (size : int) : uf_t = 
    ref (Array.make size (Some (None, ref(1))), ref(size), ref(size));;

let rec find_inner (tree : uf_t) (pos : int) (rank_minus : int) = 
    match !tree with
    (arr, comp, ptr) -> match arr.(pos) with
    None -> raise NotFound
    | Some (None, _) -> pos
    | Some (Some(parent), rank) ->
            let top = find_inner tree parent (rank_minus + !rank) in
            arr.(pos) <- Some (Some(top), ref(!rank - rank_minus));
            top;;

let find (tree : uf_t) (pos : int) = find_inner tree pos 0;;

let combine tree first second = 
    match !tree with
    (arr, comp, ptr) -> match arr.(second) with Some (None, rank_second) ->
        arr.(second) <- Some (Some first, rank_second);
        match arr.(first) with
        Some (None, rank_first) -> arr.(first) <- Some (None,
            ref (!rank_first + !rank_second));
        comp := !comp - 1;;

let union tree first second = 
    let first_top = find tree first in
    let second_top = find tree second in
    if first_top == second_top then () else
    match !tree with (arr, comp, ptr) -> match arr.(first_top) with
        Some (None, rank_first) -> match arr.(second_top) with
        Some (None, rank_second) -> 
            if !rank_first >= !rank_second then 
                combine tree first_top second_top else
                combine tree second_top first_top;;

let count (tree : uf_t) = 
    match !tree with (_, comp, _) -> !comp;;

let rec components_inner_inner arr ls pos = 
    match arr.(pos) with
    Some (None, _) -> pos :: ls
    | Some (Some(parent), rank) -> components_inner_inner arr (pos :: ls) (parent);;

let rec components_inner lsls arr pos = 
    if pos >= Array.length arr then lsls else
        match arr.(pos) with
        Some (Some(parent), rank) ->
            if !rank == 1 then
               components_inner ((components_inner_inner arr [] pos) :: lsls) arr
            (pos + 1) else
               components_inner lsls arr (pos + 1) 
        | _ -> components_inner lsls arr (pos + 1)

let components tree = 
    match !tree with (arr, comp, ptr) -> components_inner [[]] arr 0;;
