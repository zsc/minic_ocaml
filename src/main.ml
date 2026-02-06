let usage () =
  prerr_endline "usage: minic <input.c> [-o output.ll]";
  exit 2

let () =
  try
    let input = ref None in
    let output = ref None in
    let rec loop i =
      if i >= Array.length Sys.argv then ()
      else
        match Sys.argv.(i) with
        | "-o" ->
            if i + 1 >= Array.length Sys.argv then usage ();
            output := Some Sys.argv.(i + 1);
            loop (i + 2)
        | s when String.length s > 0 && s.[0] = '-' -> usage ()
        | path ->
            input := Some path;
            loop (i + 1)
    in
    loop 1;
    let input_path =
      match !input with
      | None -> usage ()
      | Some p -> p
    in
    let src = Util.read_file input_path in
    let ll = Driver.compile_to_llvm src in
    match !output with
    | None -> print_string ll
    | Some out ->
        let oc = open_out_bin out in
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () -> output_string oc ll)
  with
  | Util.Error (loc, msg) ->
      prerr_endline (Printf.sprintf "%s: error: %s" (Loc.pp loc) msg);
      exit 1

