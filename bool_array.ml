(* This is immutable so we can store num_set here. *)
type t =
  { data : bytes
  ; length : int
  ; num_set : int
  }

let create b ~len:length =
  if length < 0
  then failwith "negative length"
  else if length > Sys.max_string_length * 8
  then failwith @@ Printf.sprintf "%d is above max-string-length" length
  else (
    let num_set = if b then length else 0 in
    let fill_value = Char.chr (if b then 255 else 0) in
    let data_length = (length + 7) / 8 in
    let data = Bytes.make data_length fill_value in
    { data; length; num_set })

let length t = t.length

let get_ data i ~length =
  if i < 0
  then failwith @@ Printf.sprintf "negative index %d" i
  else if i >= length
  then failwith @@ Printf.sprintf "index above length %d >= %d" i length
  else (
    let byte = Bytes.get data (i lsr 3) |> Char.code in
    let index_in_byte = i land 7 in
    byte land (1 lsl index_in_byte) <> 0)

let get t i = get_ t.data i ~length:t.length
let num_set t = t.num_set

(* let iteri_ data ~f ~length = *)
(*   let bytes_length = Bytes.length data in *)
(*   for byte_index = 0 to bytes_length - 1 do *)
(*     let byte = Bytes.unsafe_get data byte_index |> Char.code in *)
(*     let bit_offset = 8 * byte_index in *)
(*     let bits_used = if byte_index <> bytes_length - 1 then 8 else length land 7 in *)
(*     let bits_used = if bits_used = 0 then 8 else bits_used in *)
(*     for i = 0 to bits_used - 1 do *)
(*       let bit_set = 1 lsl i in *)
(*       f (bit_offset + i) (byte land bit_set <> 0) *)
(*     done *)
(*   done *)
(**)
(* let iteri t ~f = iteri_ t.data ~f ~length:t.length *)

(* let mapi t ~f = *)
(*   let bytes_length = Bytes.length t.data in *)
(*   let num_set = ref 0 in *)
(*   let data = Bytes.make bytes_length (Char.chr 0) in *)
(*   for byte_index = 0 to bytes_length - 1 do *)
(*     let byte = Bytes.unsafe_get t.data byte_index |> Char.code in *)
(*     let bit_offset = 8 * byte_index in *)
(*     let bits_used = if byte_index <> bytes_length - 1 then 8 else t.length land 7 in *)
(*     let bits_used = if bits_used = 0 then 8 else bits_used in *)
(*     let v = ref 0 in *)
(*     for i = 0 to bits_used - 1 do *)
(*       let bit_set = 1 lsl i in *)
(*       if f (bit_offset + i) (byte land bit_set <> 0) *)
(*       then ( *)
(*         v := !v lor bit_set; *)
(*         incr num_set) *)
(*     done; *)
(*     if !v <> 0 then Bytes.unsafe_set data byte_index (Char.chr !v) *)
(*   done; *)
(*   { data; length = t.length; num_set = !num_set } *)

(* let indexes t ~value = *)
(*   let indexes_len = if value then t.num_set else t.length - t.num_set in *)
(*   let indexes = Array.make indexes_len (-1) in *)
(*   let indexes_i = ref 0 in *)
(*   iteri t ~f:(fun i b -> *)
(*       if b = value *)
(*       then ( *)
(*         indexes.(!indexes_i) <- i; *)
(*         incr indexes_i)); *)
(*   indexes *)



(* (* We could also use a phantom type rather than a separate module. *) *)
(* module Mutable = struct *)
(*   type immutable = t *)
(**)
(*   type t = *)
(*     { data : bytes *)
(*     ; length : int *)
(*     } *)
(**)
(*   let create v ~len = *)
(*     let t = create v ~len in *)
(*     { data = t.data; length = t.length } *)
(**)
(*   let get t i = get_ t.data i ~length:t.length *)
(**)
(*   let set t i value = *)
(*     if i < 0 *)
(*     then Printf.failwithf "negative index %d" i () *)
(*     else if i >= t.length *)
(*     then Printf.failwithf "index above length %d >= %d" i t.length () *)
(*     else ( *)
(*       let byte_index = i lsr 3 in *)
(*       let byte = Bytes.get t.data byte_index |> Char.code in *)
(*       let index_in_byte = i land 7 in *)
(*       let bit_set = 1 lsl index_in_byte in *)
(*       let byte = if value then byte lor bit_set else byte land lnot bit_set in *)
(*       Bytes.unsafe_set t.data byte_index (Char.chr byte)) *)
(**)
(*   let length t = t.length *)
(**)
(*   let finish : t -> immutable = *)
(*    fun t -> *)
(*     let num_set = ref 0 in *)
(*     iteri_ t.data ~length:t.length ~f:(fun _ b -> if b then incr num_set); *)
(*     { data = Bytes.copy t.data; length = t.length; num_set = !num_set } *)
(* end *)



(* (* Examples of using the Boolean Array module *) *)
(**)
(* (* First, include the boolean array implementation *) *)
(* (* ... (the previous bool_array code would go here) ... *) *)

(* Example 1: Basic creation and access *)
let example1 () =
  Printf.printf "=== Example 1: Basic Operations ===\n";
  
  (* Create a boolean array of length 10, all initially false *)
  let arr = create false ~len:10 in
  Printf.printf "Created array of length %d\n" (length arr);
  Printf.printf "Number of true values: %d\n" (num_set arr);
  
  (* Access some elements *)
  Printf.printf "arr[0] = %b\n" (get arr 0);
  Printf.printf "arr[5] = %b\n" (get arr 5);
  
  (* Create another array with all true values *)
  let arr_true = create true ~len:5 in
  Printf.printf "All-true array has %d set bits out of %d\n" 
    (num_set arr_true) (length arr_true);
  print_newline ()

(* (* Example 2: Using the mutable version *) *)
(* let example2 () = *)
(*   Printf.printf "=== Example 2: Mutable Operations ===\n"; *)
(*    *)
(*   (* Create a mutable boolean array *) *)
(*   let mut_arr = Mutable.create false ~len:8 in *)
(*   Printf.printf "Created mutable array of length %d\n" (Mutable.length mut_arr); *)
(*    *)
(*   (* Set some bits *) *)
(*   Mutable.set mut_arr 1 true; *)
(*   Mutable.set mut_arr 3 true; *)
(*   Mutable.set mut_arr 7 true; *)
(*    *)
(*   (* Check the values *) *)
(*   for i = 0 to 7 do *)
(*     Printf.printf "mut_arr[%d] = %b\n" i (Mutable.get mut_arr i) *)
(*   done; *)
(*    *)
(*   (* Convert to immutable *) *)
(*   let immutable_arr = Mutable.finish mut_arr in *)
(*   Printf.printf "After finishing: %d bits are set\n" (num_set immutable_arr); *)
(*   print_newline () *)
(**)
(* (* Example 3: Iteration *) *)
(* let example3 () = *)
(*   Printf.printf "=== Example 3: Iteration ===\n"; *)
(*    *)
(*   (* Create an array with some pattern *) *)
(*   let mut_arr = Mutable.create false ~len:6 in *)
(*   Mutable.set mut_arr 0 true; *)
(*   Mutable.set mut_arr 2 true; *)
(*   Mutable.set mut_arr 4 true; *)
(*   let arr = Mutable.finish mut_arr in *)
(*    *)
(*   Printf.printf "Array contents:\n"; *)
(*   iteri arr ~f:(fun i value -> *)
(*     Printf.printf "  [%d] = %b\n" i value); *)
(*   print_newline () *)
(**)
(* (* Example 4: Mapping *) *)
(* let example4 () = *)
(*   Printf.printf "=== Example 4: Mapping ===\n"; *)
(*    *)
(*   (* Create an array *) *)
(*   let mut_arr = Mutable.create false ~len:5 in *)
(*   Mutable.set mut_arr 1 true; *)
(*   Mutable.set mut_arr 3 true; *)
(*   let arr = Mutable.finish mut_arr in *)
(*    *)
(*   Printf.printf "Original array:\n"; *)
(*   iteri arr ~f:(fun i value -> *)
(*     Printf.printf "  [%d] = %b\n" i value); *)
(*    *)
(*   (* Flip all bits *) *)
(*   let flipped = mapi arr ~f:(fun i value -> not value) in *)
(*    *)
(*   Printf.printf "Flipped array:\n"; *)
(*   iteri flipped ~f:(fun i value -> *)
(*     Printf.printf "  [%d] = %b\n" i value); *)
(*    *)
(*   Printf.printf "Original had %d set, flipped has %d set\n" *)
(*     (num_set arr) (num_set flipped); *)
(*   print_newline () *)
(**)
(* (* Example 5: Finding indexes *) *)
(* let example5 () = *)
(*   Printf.printf "=== Example 5: Finding Indexes ===\n"; *)
(*    *)
(*   (* Create a sparse array *) *)
(*   let mut_arr = Mutable.create false ~len:10 in *)
(*   Mutable.set mut_arr 2 true; *)
(*   Mutable.set mut_arr 5 true; *)
(*   Mutable.set mut_arr 8 true; *)
(*   let arr = Mutable.finish mut_arr in *)
(*    *)
(*   (* Get indexes of true values *) *)
(*   let true_indexes = indexes arr ~value:true in *)
(*   Printf.printf "Indexes with true values: "; *)
(*   Array.iter (Printf.printf "%d ") true_indexes; *)
(*   print_newline (); *)
(*    *)
(*   (* Get indexes of false values *) *)
(*   let false_indexes = indexes arr ~value:false in *)
(*   Printf.printf "Indexes with false values: "; *)
(*   Array.iter (Printf.printf "%d ") false_indexes; *)
(*   print_newline (); *)
(*   print_newline () *)
(**)
(* (* Example 6: Practical use case - sieve of Eratosthenes *) *)
(* let example6 () = *)
(*   Printf.printf "=== Example 6: Sieve of Eratosthenes ===\n"; *)
(*    *)
(*   let find_primes_up_to n = *)
(*     (* Create array, initially all true (assume all are prime) *) *)
(*     let sieve = Mutable.create true ~len:(n + 1) in *)
(*      *)
(*     (* 0 and 1 are not prime *) *)
(*     if n >= 0 then Mutable.set sieve 0 false; *)
(*     if n >= 1 then Mutable.set sieve 1 false; *)
(*      *)
(*     (* Sieve algorithm *) *)
(*     for i = 2 to int_of_float (sqrt (float_of_int n)) do *)
(*       if Mutable.get sieve i then ( *)
(*         (* Mark multiples of i as not prime *) *)
(*         let j = ref (i * i) in *)
(*         while !j <= n do *)
(*           Mutable.set sieve !j false; *)
(*           j := !j + i *)
(*         done *)
(*       ) *)
(*     done; *)
(*      *)
(*     Mutable.finish sieve *)
(*   in *)
(*    *)
(*   let primes = find_primes_up_to 30 in *)
(*   let prime_indexes = indexes primes ~value:true in *)
(*    *)
(*   Printf.printf "Prime numbers up to 30: "; *)
(*   Array.iter (Printf.printf "%d ") prime_indexes; *)
(*   print_newline (); *)
(*   Printf.printf "Found %d primes\n" (Array.length prime_indexes); *)
(*   print_newline () *)

(* execute examples *)
let () =
  example1 ();
