(* Create a boolean array with specified length, initialized to false *)
let create b ~len:length = Array.make length b

(* Get the length of an array *)
let length t = Array.length t

(* Get element at index i *)
let get t i = Array.get t i

(* Iterate over array with index, applying function f to each element up to specified length *)
let iteri_ data ~f ~length =
  for i = 0 to (min length (Array.length data)) - 1 do
    f i (Array.get data i)
  done

(* Map over array with index, creating new array *)
let mapi t ~f = Array.mapi f t

(* Find all indexes where the array contains the specified value *)
let indexes t ~value =
  let result = ref [] in
  Array.iteri (fun i x -> 
    if Bool.equal x value then 
      result := i :: !result
  ) t;
  List.rev !result

(* Example usage and tests *)
let () =
  (* Test create *)
  let arr1 = create true ~len:5 in
  let arr2 = create false ~len:3 in
  
  Printf.printf "Created array of length %d with true values\n" (length arr1);
  Printf.printf "Created array of length %d with false values\n" (length arr2);
  
  (* Test get *)
  Printf.printf "arr1[0] = %b\n" (get arr1 0);
  Printf.printf "arr2[1] = %b\n" (get arr2 1);
  
  (* Test iteri_ *)
  Printf.printf "Elements in arr1:\n";
  iteri_ arr1 ~f:(fun i x -> Printf.printf "  [%d] = %b\n" i x) ~length:(length arr1);
  
  (* Test mapi *)
  let arr3 = [|true; false; true; false|] in
  let negated = mapi arr3 ~f:(fun _ x -> Bool.not x) in
  Printf.printf "Original: ";
  Array.iter (Printf.printf "%b ") arr3;
  Printf.printf "\nNegated:  ";
  Array.iter (Printf.printf "%b ") negated;
  Printf.printf "\n";
  
  (* Test indexes *)
  let test_array = [|true; false; true; true; false|] in
  let true_indexes = indexes test_array ~value:true in
  let false_indexes = indexes test_array ~value:false in
  
  Printf.printf "Array: ";
  Array.iter (Printf.printf "%b ") test_array;
  Printf.printf "\nIndexes of true: ";
  List.iter (Printf.printf "%d ") true_indexes;
  Printf.printf "\nIndexes of false: ";
  List.iter (Printf.printf "%d ") false_indexes;
  Printf.printf "\n"
