let create_stream_of_name name =
  let file = open_in name in
  let stream = Stream.from(fun nr -> try Some (input_line file) with e -> None) in
  stream

let _ =
  let str = create_stream_of_name "test.txt" in
  while (Stream.peek str) <> None do
    Printf.printf "Line read: %s\n" (Stream.next str)
  done
