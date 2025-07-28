(* to-do: remove the functor *)

open ArrayOps

(* Boolean implementation of COMPARABLE *)
module BoolComparable : COMPARABLE with type t = bool = struct
  type t = bool
  let equal = Bool.equal
end

(* Boolean-specific array module *)
module BoolArray = struct
  include ArrayOps

  (* Boolean array type *)
  (* type bool_array = bool t *)
  
  (* Create boolean-specific operations *)
  (* module Search = MakeSearchable(ArrayOps)(BoolComparable) *)
  include MakeSearchable(ArrayOps)(BoolComparable)
  
  (* Convenience functions for boolean arrays *)
  let make_true len = make len true
  let make_false len = make len false

  let indexes = indexes
  let find_first = find_first
  let count = count

  (* Boolean-specific operations *)
  let negate arr = Array.map Bool.not arr
  let all_true arr = Array.for_all Fun.id arr
  let any_true arr = Array.exists Fun.id arr
  let array_and = Array.map2 Bool.(&&)
  let array_or = Array.map2 Bool.(||)
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
  let arr1 = BoolArray.make_true 5
  and arr2 = BoolArray.make_false 3 in
  
  Printf.printf "Created true array: ";
  print_endline @@ [%show: bool array] arr1;
  Printf.printf " (length: %d)\n" (BoolArray.length arr1);
  
  Printf.printf "Created false array: ";
  print_endline @@ [%show: bool array] arr2;
  Printf.printf " (length: %d)\n\n" (BoolArray.length arr2);
  
  (* Test get operations *)
  Printf.printf "arr1[0] = %s\n" (Bool.to_string (BoolArray.get arr1 0));
  Printf.printf "arr2[1] = %s\n\n" (Bool.to_string (BoolArray.get arr2 1));
  
  (* Test iteration *)
  Printf.printf "Iterating over arr1:\n";
  BoolArray.iteri (fun i x -> 
    Printf.printf "  [%d] = %s\n" i (Bool.to_string x)
  ) arr1;
  Printf.printf "\n";
  
  (* Test mapping *)
  let test_array = Array.of_list [true; false; true; false] in
  let negated = BoolArray.negate test_array in
  Printf.printf "Original: ";
  print_endline @@ [%show: bool array] test_array;
  Printf.printf "Negated:  ";
  print_endline @@ [%show: bool array] negated;
  Printf.printf "\n";
  
  (* Test search operations *)
  let mixed_array = Array.of_list [true; false; true; true; false] in
  let true_indices = BoolArray.indexes mixed_array ~value:true in
  let false_indices = BoolArray.indexes mixed_array ~value:false in
  
  Printf.printf "Array: ";
  print_endline @@ [%show: bool array] mixed_array;
  Printf.printf "\nIndexes of true:  ";
  print_endline @@ [%show: int list] true_indices;
  Printf.printf "Indexes of false: ";
  print_endline @@ [%show: int list] false_indices;
  Printf.printf "\n";
  
  (* Test advanced boolean operations *)
  let arr_a = Array.of_list [true; false; true; false] in
  let arr_b = Array.of_list [false; false; true; true] in
  
  Printf.printf "Array A: ";
  print_endline @@ [%show: bool array] arr_a;
  Printf.printf "Array B: ";
  print_endline @@ [%show: bool array] arr_b;
  
  let and_result = BoolArray.array_and arr_a arr_b in
  let or_result = BoolArray.array_or arr_a arr_b in
  
  Printf.printf "\nA AND B: ";
  print_endline @@ [%show: bool array] and_result;
  Printf.printf "A OR B:  ";
  print_endline @@ [%show: bool array] or_result;
  Printf.printf "\n";
  
  (* Test predicates *)
  Printf.printf "All true in arr1: %s\n" (Bool.to_string (BoolArray.all_true arr1));
  Printf.printf "Any true in arr2: %s\n" (Bool.to_string (BoolArray.any_true arr2));
  Printf.printf "Count of true in mixed_array: %d\n" (BoolArray.count mixed_array ~value:true);
  
  (* Test search *)
  match BoolArray.find_first mixed_array ~value:false with
  | None -> Printf.printf "No false found\n"
  | Some idx -> Printf.printf "First false at index: %d\n" idx
