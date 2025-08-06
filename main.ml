let () =

    (* create columns *)
    let names = Column.of_list "name" ["Alice"; "Bob"; "Charlie"]
    and ages = Column.of_list "town" ["Cairo"; "Greece"; "London"] in

    (* print names column *)
    Column.print names;

    (* create dataframe *)
    let df = Dataframe.create [names; ages] in

    (* print *)
    Dataframe.print df;

    (* add column then print *)
    Column.of_list "adult" ["True"; "True"; "False"] |> Dataframe.addColumn df |> Dataframe.print;

    (* select column then print *)
    Dataframe.select df ["name"] |> Dataframe.print;

    (* Filter data *)
    Dataframe.filter df (fun row -> List.hd row == "Alice") |> Dataframe.print
