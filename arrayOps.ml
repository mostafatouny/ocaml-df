(* Module signature for types that can be compared *)
module type COMPARABLE = sig
  type t
  val equal : t -> t -> bool
end

(* Generic array operations signature *)
module type ARRAY_OPS = sig
  type 'a t
  type index = int
  
  val make : int -> 'a -> 'a t
  val length : 'a t -> int
  val get : 'a t -> index -> 'a
  val iteri: (int -> 'a -> unit) -> 'a array -> unit
  val mapi : (index -> 'a -> 'b) -> 'a t -> 'b t
end

(* Standard array implementation *)
module ArrayOps : ARRAY_OPS with type 'a t = 'a array = struct
  type 'a t = 'a array
  type index = int

  let make = Array.make
  let length = Array.length
  let get = Array.get

  let iteri = Array.iteri
  let mapi = Array.mapi
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
    A.iteri add_if_match arr;
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
    A.iteri (fun _ x -> if C.equal x value then incr counter) arr;
    !counter
end
