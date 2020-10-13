open! Core_kernel

let variable_name_of_file_name s =
  String.to_list s
  |> List.mapi ~f:(fun i c ->
    match c with
    | '0' .. '9' -> if i = 0 then sprintf "_%c" c else String.of_char c
    | 'A' .. 'Z' -> String.of_char (Char.lowercase c)
    | 'a' .. 'z' | '_' -> String.of_char c
    | '.' -> "_dot_"
    | '-' -> "_"
    | _ -> sprintf "_0x%x_" (Char.to_int c))
  |> String.concat
;;

let chunk_len = 80

let chunks str =
  let n = String.length str in
  let q = n / chunk_len in
  let r = n % chunk_len in
  let chunk i ~len = String.sub str ~pos:(chunk_len * i) ~len in
  List.concat
    [ List.init q ~f:(fun i -> chunk i ~len:chunk_len)
    ; (if r = 0 then [] else [ chunk q ~len:r ])
    ]
;;

let replace_CRs : string -> string =
  (* [replace_CRs input] replaces all occurrences in [input] of "CR" with "C\082". *)
  let search_pattern = lazy (String.Search_pattern.create "CR") in
  let replacement = lazy (sprintf "C\\%03d" (Char.to_int 'R')) in
  fun input ->
    String.Search_pattern.replace_all
      (force search_pattern)
      ~in_:input
      ~with_:(force replacement)
;;

let write_ml w ~var ~contents =
  Stdio.Out_channel.fprintf w "let %s =\n  \"" var;
  List.iteri (chunks contents) ~f:(fun i chunk ->
    let escaped_chunk = replace_CRs (String.escaped chunk) in
    if i = 0
    then Stdio.Out_channel.fprintf w "%s" escaped_chunk
    else if String.length chunk > 0 && Char.( = ) chunk.[0] ' '
    then Stdio.Out_channel.fprintf w "\\\n  \\%s" escaped_chunk
    else Stdio.Out_channel.fprintf w "\\\n   %s" escaped_chunk);
  Stdio.Out_channel.output_string w "\"\n;;\n"
;;

let write_alist_ml w ~files =
  Stdio.Out_channel.output_lines w ["\nlet by_filename ="];
  let line_start = ref "[" in
  Map.iteri files ~f:(fun ~key:file ~data:var ->
    Stdio.Out_channel.fprintf w "  %s \"%s\", %s\n" !line_start file var;
    line_start := ";");
  Stdio.Out_channel.output_lines w ["  ]"];
  Stdio.Out_channel.output_lines w [";;"]
;;

let write_mli w ~var = Stdio.Out_channel.fprintf w "val %s : string\n" var

let write_alist_mli w =
  Stdio.Out_channel.output_lines
    w
    ["\n\
      (** an association list mapping the embedded file basenames to their string values \
      *)";
     "val by_filename : (string * string) list"];
;;

let command =
  Command.basic
    ~summary:"embed text files as ocaml strings"
    ~readme:(fun () ->
      {|
Generates ocaml code defining string constants containing the contents of the provided
files.
|})
    (let open Command.Let_syntax in
     let%map_open module_name =
       flag "output" (required string) ~doc:"NAME name of the generated module"
     and output_directory =
       flag
         "output-dir"
         (optional_with_default "." string)
         ~doc:"PATH where to put the generated module (default = cwd)"
     and with_alist =
       flag "with-alist" no_arg ~doc:"include an alist of file basename -> file contents"
     and paths = anon (non_empty_sequence_as_list ("FILE" %: string)) in
     fun () ->
       (* normalize module name *)
       let module_name =
         module_name
         |> String.tr ~target:'-' ~replacement:'_'
         |> String.lowercase
         |> String.capitalize
       in
       let filename ext = output_directory ^/ String.lowercase module_name ^ "." ^ ext in
       let write ext ~write_file_line ~write_alist =
         Stdio.Out_channel.with_file (filename ext) ~f:(fun w ->
           let first_time = ref true in
           let files =
             List.map paths ~f:(fun path ->
               if !first_time then first_time := false else Stdio.Out_channel.newline w;
               let basename = Filename.basename path in
               let var = variable_name_of_file_name basename in
               let () = write_file_line w ~var ~path in
               basename, var)
             |> String.Map.of_alist_exn
           in
           if with_alist then write_alist w ~files)
       in
       let () =
         write
           "ml"
           ~write_file_line:(fun w ~var ~path ->
             let contents = Stdio.In_channel.read_all path in
             write_ml w ~var ~contents)
           ~write_alist:(fun w ~files -> write_alist_ml w ~files)
       in
       write
         "mli"
         ~write_file_line:(fun w ~var ~path:_ ->
           write_mli w ~var)
         ~write_alist:(fun w ~files:_ -> write_alist_mli w))
;;

module Private = struct
  let variable_name_of_file_name = variable_name_of_file_name
end
