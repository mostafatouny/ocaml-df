let () =

    (* create columns *)
    let names = Column.of_list "name" ["Alice"; "Bob"; "Charlie"]
    and ages = Column.of_list "town" ["Cairo"; "Greece"; "London"] in

    (* print names column *)
    print_endline "print names column";
    Column.print names;

    (* create dataframe *)
    let df = Dataframe.create [names; ages] in

    (* print *)
    print_endline "print dataframe";
    Dataframe.print df;

    (* add column then print *)
    print_endline "add column adult";
    Column.of_list "adult" ["True"; "True"; "False"] |> Dataframe.addColumn df |> Dataframe.print;

    (* select column then print *)
    print_endline "select column name";
    Dataframe.select df ["name"] |> Dataframe.print;

    (* Filter data *)
    print_endline "filter dataframe";
    Dataframe.filter df (fun row -> List.hd row == "Alice") |> Dataframe.print
