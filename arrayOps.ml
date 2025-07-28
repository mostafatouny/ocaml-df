module ArrayOps = struct
    type 'a t = 'a array
    type index = int

    let make = Array.make
    let length = Array.length
    let get = Array.get

    let iteri = Array.iteri
    let mapi = Array.mapi

    let find_el_index x = Array.find_index ((==) x)

    let count el arr =
        let counter = ref 0 in
            iteri (fun _ x -> if (==) x el then incr counter) arr;
        !counter

    let indexes el arr =
        let indices = ref [] in
        let add_if_match i x =
            if (==) x el then indices := i :: !indices
        in iteri add_if_match arr;
        List.rev !indices
end
