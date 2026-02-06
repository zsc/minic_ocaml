type position =
  { offset : int
  ; line : int
  ; col : int
  }

type t =
  { start : position
  ; end_ : position
  }

let none =
  let p = { offset = 0; line = 1; col = 1 } in
  { start = p; end_ = p }

let pp_position (p : position) : string = Printf.sprintf "%d:%d" p.line p.col

let pp (loc : t) : string = pp_position loc.start

