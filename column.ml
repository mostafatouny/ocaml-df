(* type definition for a column *)
type 'a t = {
    name : string;
    data : 'a array;
}

(* create column from raw *)
let make name data = {name; data}

(* create column from list *)
let of_list name lst = {
    name = name;
    data = Array.of_list lst;
}

(* get column name *)
let name (col : 'a t) = col.name

(* get column data *)
let data (col : 'a t) = col.data

(* get column length *)
let length (col : 'a t) = Array.length col.data

(* get ith value *)
let ithVal i (col : 'a t) = col.data.(i)

let to_list (col : 'a t) = Array.to_list col.data

(* printer *)
let print (col : 'a t) =
    Printf.printf "| %s |\n--------\n" col.name;
    Array.iter (fun x ->
        Printf.printf "| %s |\n" x;
    ) col.data;
    print_newline ()
