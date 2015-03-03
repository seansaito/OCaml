(* Implementation of parray *)

type 'a pa_cell = 
        Prime of 'a array
        | Modification of int * 'a * 'a pa_cell ref;;

type 'a pa = ('a pa_cell) ref;;

let make_pa size i = 
        ref (Prime(Array.make size i));;

(* Lookup and Update without reroot *)
let rec raw_lookup pa n = 
        match !pa with
                Prime(a) -> a.(n)
                | Modification(p, v, pa') ->
                                if p=n then v else raw_lookup pa' n;;

let raw_update pa n v = 
        ref (Modification(n, v, pa));;

(* Lookup and Update with reroot including helper functions *)
let change modi prime = 
        match !modi with
                Modification(p, v, pa') ->
                        match !prime with
                        Prime(a) -> 
                                prime := Modification(p, a.(p), modi);
                                a.(p) <- v;
                                modi := Prime(a);
                                modi;;

let rec reroot pa =
        match !pa with
        Prime(a) -> pa;
        | Modification(p, v, pa') ->
                        change (pa) (reroot pa');;

let lookup pa n = 
        raw_lookup reroot(pa) n;;

let update pa n v =
        let new_pa = raw_update pa n v in
                reroot new_pa;;
