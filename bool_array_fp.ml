(* commented code is unused *)

(* Module signature for types that can be compared *)
module type COMPARABLE = sig
  type t
  val equal : t -> t -> bool
  val to_string : t -> string
end

(* Boolean implementation of COMPARABLE *)
module BoolComparable : COMPARABLE with type t = bool = struct
  type t = bool
  let equal = Bool.equal
  let to_string = Bool.to_string
end

(* Generic array operations signature *)
module type ARRAY_OPS = sig
  type 'a t
  type index = int
  
  val create : 'a -> len:int -> 'a t
  val length : 'a t -> int
  val get : 'a t -> index -> 'a
  val iteri_ : 'a t -> f:(index -> 'a -> unit) -> length:int -> unit
  val mapi : 'a t -> f:(index -> 'a -> 'b) -> 'b t
end

(* Standard array implementation *)
module ArrayOps : ARRAY_OPS with type 'a t = 'a array = struct
  type 'a t = 'a array
  type index = int
  
  let create value ~len = Array.make len value
  let length = Array.length
  let get = Array.get
  
  let iteri_ data ~f ~length =
    let actual_length = min length (Array.length data) in
    for i = 0 to actual_length - 1 do
      f i (Array.get data i)
    done
  
  let mapi t ~f = Array.mapi f t
end

(* Functor for creating search operations on arrays *)
module MakeSearchable (A : ARRAY_OPS) (C : COMPARABLE) = struct
  (* type t = C.t A.t *)
  
  let indexes arr ~value =
    let indices = ref [] in
    let add_if_match i x =
      if C.equal x value then
        indices := i :: !indices
    in
    A.iteri_ arr ~f:add_if_match ~length:(A.length arr);
    List.rev !indices
  
  let find_first arr ~value =
    let rec search i =
      if i >= A.length arr then None
      else if C.equal (A.get arr i) value then Some i
      else search (i + 1)
    in
    search 0
  
  let count arr ~value =
    let counter = ref 0 in
    A.iteri_ arr ~f:(fun _ x -> 
      if C.equal x value then incr counter
    ) ~length:(A.length arr);
    !counter
end

(* Boolean-specific array module *)
module BoolArray = struct
  include ArrayOps
  
  (* Boolean array type *)
  (* type bool_array = bool t *)
  
  (* Create boolean-specific operations *)
  module Search = MakeSearchable(ArrayOps)(BoolComparable)
  
  (* Convenience functions for boolean arrays *)
  let create_true ~len = create true ~len
  let create_false ~len = create false ~len
  
  let indexes = Search.indexes
  let find_first = Search.find_first
  let count = Search.count
  
  (* Boolean-specific operations *)
  let negate arr = mapi arr ~f:(fun _ x -> Bool.not x)
  
  let all_true arr =
    let rec check i =
      if i >= length arr then true
      else if get arr i then check (i + 1)
      else false
    in
    check 0
  
  let any_true arr =
    let rec check i =
      if i >= length arr then false
      else if get arr i then true
      else check (i + 1)
    in
    check 0
  
  let logical_and arr1 arr2 =
    if length arr1 <> length arr2 then
      invalid_arg "Arrays must have same length"
    else
      mapi arr1 ~f:(fun i x -> Bool.( && ) x (get arr2 i))
  
  let logical_or arr1 arr2 =
    if length arr1 <> length arr2 then
      invalid_arg "Arrays must have same length"
    else
      mapi arr1 ~f:(fun i x -> Bool.( || ) x (get arr2 i))
end

(* Module for pretty printing *)
module Printer = struct
  let print_bool_array arr =
    Printf.printf "[|";
    for i = 0 to (BoolArray.length arr) - 1 do
      Printf.printf "%s%s" 
        (Bool.to_string (BoolArray.get arr i))
        (if i < (BoolArray.length arr) - 1 then "; " else "")
    done;
    Printf.printf "|]"
  
  let print_indices indices =
    Printf.printf "[";
    List.iteri (fun i idx ->
      Printf.printf "%d%s" idx (if i < List.length indices - 1 then "; " else "")
    ) indices;
    Printf.printf "]"
end

(* (* Monadic operations for array processing *) *)
(* module ArrayMonad = struct *)
(*   let return x ~len = BoolArray.create x ~len *)
(*    *)
(*   let bind arr ~f = *)
(*     let results = ref [] in *)
(*     BoolArray.iteri_ arr ~f:(fun i x -> *)
(*       let result_arr = f x i in *)
(*       for j = 0 to (BoolArray.length result_arr) - 1 do *)
(*         results := (BoolArray.get result_arr j) :: !results *)
(*       done *)
(*     ) ~length:(BoolArray.length arr); *)
(*     let final_results = List.rev !results in *)
(*     Array.of_list final_results *)
(* end *)

(* Example usage and tests *)
let () =
  Printf.printf "=== OCaml Functional Programming Demo ===\n\n";
  
  (* Test basic operations *)
  let arr1 = BoolArray.create_true ~len:5 in
  let arr2 = BoolArray.create_false ~len:3 in
  
  Printf.printf "Created true array: ";
  Printer.print_bool_array arr1;
  Printf.printf " (length: %d)\n" (BoolArray.length arr1);
  
  Printf.printf "Created false array: ";
  Printer.print_bool_array arr2;
  Printf.printf " (length: %d)\n\n" (BoolArray.length arr2);
  
  (* Test get operations *)
  Printf.printf "arr1[0] = %s\n" (Bool.to_string (BoolArray.get arr1 0));
  Printf.printf "arr2[1] = %s\n\n" (Bool.to_string (BoolArray.get arr2 1));
  
  (* Test iteration *)
  Printf.printf "Iterating over arr1:\n";
  BoolArray.iteri_ arr1 ~f:(fun i x -> 
    Printf.printf "  [%d] = %s\n" i (Bool.to_string x)
  ) ~length:(BoolArray.length arr1);
  Printf.printf "\n";
  
  (* Test mapping *)
  let test_array = Array.of_list [true; false; true; false] in
  let negated = BoolArray.negate test_array in
  Printf.printf "Original: ";
  Printer.print_bool_array test_array;
  Printf.printf "\nNegated:  ";
  Printer.print_bool_array negated;
  Printf.printf "\n\n";
  
  (* Test search operations *)
  let mixed_array = Array.of_list [true; false; true; true; false] in
  let true_indices = BoolArray.indexes mixed_array ~value:true in
  let false_indices = BoolArray.indexes mixed_array ~value:false in
  
  Printf.printf "Array: ";
  Printer.print_bool_array mixed_array;
  Printf.printf "\nIndexes of true:  ";
  Printer.print_indices true_indices;
  Printf.printf "\nIndexes of false: ";
  Printer.print_indices false_indices;
  Printf.printf "\n\n";
  
  (* Test advanced boolean operations *)
  let arr_a = Array.of_list [true; false; true; false] in
  let arr_b = Array.of_list [false; false; true; true] in
  
  Printf.printf "Array A: ";
  Printer.print_bool_array arr_a;
  Printf.printf "\nArray B: ";
  Printer.print_bool_array arr_b;
  
  let and_result = BoolArray.logical_and arr_a arr_b in
  let or_result = BoolArray.logical_or arr_a arr_b in
  
  Printf.printf "\nA AND B: ";
  Printer.print_bool_array and_result;
  Printf.printf "\nA OR B:  ";
  Printer.print_bool_array or_result;
  Printf.printf "\n\n";
  
  (* Test predicates *)
  Printf.printf "All true in arr1: %s\n" (Bool.to_string (BoolArray.all_true arr1));
  Printf.printf "Any true in arr2: %s\n" (Bool.to_string (BoolArray.any_true arr2));
  Printf.printf "Count of true in mixed_array: %d\n" (BoolArray.count mixed_array ~value:true);
  
  (* Test search *)
  match BoolArray.find_first mixed_array ~value:false with
  | None -> Printf.printf "No false found\n"
  | Some idx -> Printf.printf "First false at index: %d\n" idx
