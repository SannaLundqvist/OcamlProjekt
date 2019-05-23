module Graph = struct
  type 'a edge = 'a * 'a
  type 'a graph = 'a edge list

  let edge from til = (from, til)

  let string_of_edge (a,b) =
    String.((trim a) ^" - "^ (trim b))

  let rec print_all_edges list =
    match list with
    | [] -> print_newline
    | edge :: rest -> print_endline (string_of_edge edge) ; print_all_edges rest

  let create_stream_of_name name =
    let file = open_in name in
    let line_to_edge str =
      let split_list c = String.split_on_char c str
      in
      match split_list '-' with
      | a :: b :: rest -> String.(Some(edge (trim a) (trim b)))
      | _ -> None
    in 
    let process_line n = try 
        line_to_edge (input_line file)
      with e ->
        None
    in
    Stream.from(process_line)

  let rec stream_to_edges str =
    match Stream.peek str with
    | None -> []
    | Some a -> let d = Stream.next str in
      d :: stream_to_edges str

end
let _ = Graph.(print_all_edges (stream_to_edges (create_stream_of_name "test.txt")))
