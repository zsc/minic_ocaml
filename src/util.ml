exception Error of Loc.t * string

let error loc fmt = Printf.ksprintf (fun msg -> raise (Error (loc, msg))) fmt

let read_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

