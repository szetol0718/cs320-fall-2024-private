open Utils

let lex (s : string) : tok list option =
  let words = split s in
  let rec convert_to_tokens word_list acc =
    match word_list with
    | [] -> Some (List.rev acc)
    | w :: rest ->
        match tok_of_string_opt w with
        | Some t -> convert_to_tokens rest (t :: acc)
        | None -> None
  in
  convert_to_tokens words []
