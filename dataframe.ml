(* type definition for a DataFrame *)
type 'a t = {
    columns : ('a Column.t) list ;
}

(* create DataFrame from a list of columns *)
let create (column_list : ('a Column.t) list) : 'a t = {
    columns = column_list
}

(* get number of rows *)
let numRows df = df.columns |> List.hd |> Column.length

(* get number of columns *)
let numCols df = List.length df.columns

(* add a column to the dataframe *)
let addColumn (df : 'a t) (col : 'a Column.t) : 'a t = {
    columns = List.append df.columns [col]
}

(* select specific columns *)
let select df column_names = {
    columns = List.filter (fun col -> List.exists ( (==) (Column.name col) ) column_names) df.columns
}


(* transpose a list of lists *)
let transposeList (lst : 'a list list) =
    let getIthRow i = List.map (fun l -> List.nth l i) lst in
    List.init (List.hd lst |> List.length) getIthRow

(* construct a list of rows, transposed from a list of columns *)
let transpose df =
    (* convert to a list of lists *)
    let lst = List.map Column.to_list df.columns in
    (* transpose *)
    transposeList lst


(* filter rows based on a predicate *)
let filter df predicate =
    let names = List.map Column.name df.columns
    and lst = transpose df |> List.filter predicate |> transposeList in {
        columns = List.map2 (fun name l -> Column.make name (Array.of_list l)) names lst
    }


(* print dataframe *)
let print (df : 'a t) =
    (* number of rows and columns *)
    Printf.printf "DataFrame (%d rows, %d columns)\n" (numRows df) (numCols df);

    (* print columns' names *)
    let names = List.map Column.name df.columns in
    List.iter ( Printf.printf "| %s " ) names;
    Printf.printf "|\n";

    (* print dashes *)
    List.iter ( fun _ -> Printf.printf "|------"  ) names;
    Printf.printf "|\n";

    (* transpose to rows *)
    let rows = transpose df in
    List.iter ( fun row ->
        List.iter ( Printf.printf "| %s " ) row;
        Printf.printf "|\n";
    ) rows;
    Printf.printf "\n";
