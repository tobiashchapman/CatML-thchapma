open Lang

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec string_of_val = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Pair (e1, e2) -> "(" ^ string_of_val e1 ^ ", " ^ string_of_val e2 ^ ")"
  | Fun (x, _) -> "<fun " ^ x ^ ">"
  | Fix (f, x, _) -> "<fix " ^ f ^ "." ^ x ^ ">"
  | _ -> "non-value"

let run_catML_file (filename : string) : unit =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let ast_opt =
    try Some (Parser.prog Lexer.read lexbuf) with
    | Failure msg ->
        prerr_endline ("Lexer error: " ^ msg);
        None
    | Parser.Error ->
        prerr_endline ("Parse error in " ^ filename);
        None
  in
  close_in ch;
  match ast_opt with
  | None -> ()
  | Some ast ->
      match type_of_expr (empty_context, ast) with
      | None -> prerr_endline ("Type error in " ^ filename)
      | Some _ -> print_endline (string_of_val (multistep ast))
