type history =
  { mutable items_rev : string list
  }

let history_create () : history = { items_rev = [] }

let history_add (h : history) (line : string) : unit =
  if String.trim line = "" then ()
  else
    match h.items_rev with
    | last :: _ when String.equal last line -> ()
    | _ -> h.items_rev <- line :: h.items_rev

let history_snapshot (h : history) : string array = Array.of_list (List.rev h.items_rev)

type repl_key =
  | KChar of char
  | KEnter
  | KBackspace
  | KLeft
  | KRight
  | KUp
  | KDown
  | KEof
  | KOther

let read_char_opt () : char option = try Some (input_char stdin) with End_of_file -> None

let read_key () : repl_key =
  match read_char_opt () with
  | None -> KEof
  | Some ('\r' | '\n') -> KEnter
  | Some '\127' -> KBackspace
  | Some '\008' -> KBackspace
  | Some '\004' -> KEof
  | Some '\027' -> (
      match (read_char_opt (), read_char_opt ()) with
      | Some '[', Some 'A' -> KUp
      | Some '[', Some 'B' -> KDown
      | Some '[', Some 'C' -> KRight
      | Some '[', Some 'D' -> KLeft
      | _ -> KOther)
  | Some c -> KChar c

let redraw_line ~(prompt : string) ~(buf : string) ~(cursor : int) : unit =
  print_string "\r";
  print_string prompt;
  print_string buf;
  print_string "\027[K";
  let tail = String.length buf - cursor in
  if tail > 0 then print_string (Printf.sprintf "\027[%dD" tail);
  flush stdout

let line_insert (s : string) (cursor : int) (c : char) : string * int =
  let left = String.sub s 0 cursor in
  let right = String.sub s cursor (String.length s - cursor) in
  (left ^ String.make 1 c ^ right, cursor + 1)

let line_backspace (s : string) (cursor : int) : string * int =
  if cursor <= 0 then (s, cursor)
  else
    let left = String.sub s 0 (cursor - 1) in
    let right = String.sub s cursor (String.length s - cursor) in
    (left ^ right, cursor - 1)

let read_line_edited ~(prompt : string) (hist : history) : string option =
  let fd = Unix.descr_of_in_channel stdin in
  let old_term = Unix.tcgetattr fd in
  let raw_term = { old_term with Unix.c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 } in
  Unix.tcsetattr fd Unix.TCSANOW raw_term;
  Fun.protect
    ~finally:(fun () ->
      Unix.tcsetattr fd Unix.TCSANOW old_term;
      flush stdout)
    (fun () ->
      let history = history_snapshot hist in
      let buf = ref "" in
      let cursor = ref 0 in
      let hist_pos = ref None in
      let draft = ref None in
      redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
      let rec loop () =
        match read_key () with
        | KEnter ->
            print_newline ();
            Some !buf
        | KEof ->
            if String.length !buf = 0 then (
              print_newline ();
              None)
            else loop ()
        | KBackspace ->
            let b, c = line_backspace !buf !cursor in
            buf := b;
            cursor := c;
            hist_pos := None;
            draft := None;
            redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
            loop ()
        | KLeft ->
            if !cursor > 0 then decr cursor;
            redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
            loop ()
        | KRight ->
            if !cursor < String.length !buf then incr cursor;
            redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
            loop ()
        | KUp ->
            if Array.length history = 0 then loop ()
            else (
              let next_pos =
                match !hist_pos with
                | None ->
                    draft := Some !buf;
                    Array.length history - 1
                | Some i -> max 0 (i - 1)
              in
              hist_pos := Some next_pos;
              buf := history.(next_pos);
              cursor := String.length !buf;
              redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
              loop ())
        | KDown -> (
            match !hist_pos with
            | None -> loop ()
            | Some i ->
                if i < Array.length history - 1 then (
                  let next = i + 1 in
                  hist_pos := Some next;
                  buf := history.(next);
                  cursor := String.length !buf;
                  redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
                  loop ())
                else (
                  hist_pos := None;
                  buf := Option.value ~default:"" !draft;
                  cursor := String.length !buf;
                  redraw_line ~prompt ~buf:!buf ~cursor:!cursor;
                  loop ()))
        | KChar c ->
            if Char.code c >= 32 && Char.code c <> 127 then (
              let b, cpos = line_insert !buf !cursor c in
              buf := b;
              cursor := cpos;
              hist_pos := None;
              draft := None;
              redraw_line ~prompt ~buf:!buf ~cursor:!cursor);
            loop ()
        | KOther -> loop ()
      in
      loop ())

let read_line_prompt ~(prompt : string) ~(interactive : bool) (hist : history) : string option =
  if interactive then read_line_edited ~prompt hist
  else
    try Some (read_line ()) with
    | End_of_file -> None

let usage () =
  prerr_endline "usage: minic <input.c> [-o output.ll]";
  prerr_endline "       minic --toplevel|-i";
  exit 2

let print_error (loc : Loc.t) (msg : string) : unit =
  prerr_endline (Printf.sprintf "%s: error: %s" (Loc.pp loc) msg)

let run_lli (ll : string) : int * string =
  let lli =
    match Sys.getenv_opt "MINIC_LLI" with
    | Some p -> p
    | None ->
        if Sys.file_exists "/opt/homebrew/opt/llvm/bin/lli" then "/opt/homebrew/opt/llvm/bin/lli"
        else "lli"
  in
  let ll_path = Filename.temp_file "minic_toplevel_" ".ll" in
  let out_path = Filename.temp_file "minic_toplevel_" ".out" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists ll_path then Sys.remove ll_path;
      if Sys.file_exists out_path then Sys.remove out_path)
    (fun () ->
      let oc = open_out_bin ll_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc ll);
      let cmd =
        Printf.sprintf "%s %s > %s 2>&1" (Filename.quote lli) (Filename.quote ll_path)
          (Filename.quote out_path)
      in
      let code = Sys.command cmd in
      let output = if Sys.file_exists out_path then Util.read_file out_path else "" in
      (code, output))

type read_result =
  | Repl_quit
  | Repl_help
  | Repl_empty
  | Repl_source of string

let rec read_source_block ~(interactive : bool) (hist : history) (acc_rev : string list) : read_result
    =
  let prompt = if acc_rev = [] then "minic> " else "....> " in
  match read_line_prompt ~prompt ~interactive hist with
  | None ->
      if acc_rev = [] then Repl_quit else Repl_source (String.concat "\n" (List.rev acc_rev) ^ "\n")
  | Some line ->
      history_add hist line;
      let t = String.trim line in
      if acc_rev = [] && (t = ":quit" || t = ":q") then Repl_quit
      else if acc_rev = [] && (t = ":help" || t = ":h") then Repl_help
      else if t = ";;" then
        if acc_rev = [] then Repl_empty else Repl_source (String.concat "\n" (List.rev acc_rev) ^ "\n")
      else read_source_block ~interactive hist (line :: acc_rev)

let run_toplevel () =
  let interactive =
    try Unix.isatty (Unix.descr_of_in_channel stdin) && Unix.isatty (Unix.descr_of_out_channel stdout)
    with
    | Unix.Unix_error _ -> false
  in
  print_endline "MiniC toplevel";
  print_endline "Enter a full MiniC program, end input with a line containing only ';;'.";
  print_endline "Commands: :help, :quit";
  if interactive then
    print_endline "Line editing: up/down history, left/right cursor, backspace edit.";
  let hist = history_create () in
  let rec loop () =
    match read_source_block ~interactive hist [] with
    | Repl_quit -> ()
    | Repl_help ->
        print_endline "Input a complete program then ';;'.";
        print_endline "Example:";
        print_endline "  int main() { putchar(65); return 0; }";
        print_endline "  ;;";
        loop ()
    | Repl_empty -> loop ()
    | Repl_source src -> (
        try
          let ll = Driver.compile_to_llvm src in
          print_endline "[LLVM IR]";
          print_string ll;
          if ll = "" || ll.[String.length ll - 1] <> '\n' then print_newline ();
          let code, output = run_lli ll in
          print_endline "[Execution Output]";
          if output = "" then print_endline "(no output)" else print_string output;
          if output <> "" && output.[String.length output - 1] <> '\n' then print_newline ();
          Printf.printf "[Exit Code] %d\n" code;
          loop ()
        with
        | Util.Error (loc, msg) ->
            print_error loc msg;
            loop ())
  in
  loop ()

let run_file_mode (input_path : string) (output_path : string option) : unit =
  let src = Util.read_file input_path in
  let ll = Driver.compile_to_llvm src in
  match output_path with
  | None -> print_string ll
  | Some out ->
      let oc = open_out_bin out in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc ll)

let () =
  try
    let input = ref None in
    let output = ref None in
    let toplevel = ref false in
    let rec loop i =
      if i >= Array.length Sys.argv then ()
      else
        match Sys.argv.(i) with
        | "-o" ->
            if i + 1 >= Array.length Sys.argv then usage ();
            output := Some Sys.argv.(i + 1);
            loop (i + 2)
        | "--toplevel" | "-i" ->
            toplevel := true;
            loop (i + 1)
        | s when String.length s > 0 && s.[0] = '-' -> usage ()
        | path ->
            input := Some path;
            loop (i + 1)
    in
    loop 1;
    if !toplevel then (
      if Option.is_some !input || Option.is_some !output then usage ();
      run_toplevel ())
    else
      let input_path =
        match !input with
        | None -> usage ()
        | Some p -> p
      in
      run_file_mode input_path !output
  with
  | Util.Error (loc, msg) ->
      print_error loc msg;
      exit 1
