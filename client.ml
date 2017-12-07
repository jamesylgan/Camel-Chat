type state = {
  userid : int;
  curr_chat : string * int;
  chats : (string * int) list;
  print: string list;
}

let init_state () =
  {
    userid = -1;
    curr_chat = ("not initialized", -1);
    chats = [];
    print = [];
  }

(* Pigment values for string coloring. *)
let b = "\027[0m"
let red = "\027[31m"
let blue = "\027[34m"
let green = "\027[32m"
let purp = "\027[35m"
let cyan = "\027[36m"

(* A helper function that colors a list of strings in the same color. *)
let color c lst = List.map (fun x -> c ^ x) lst


let get_userid st = st.userid

let get_curr_chat st = st.curr_chat |> fst

let get_print st = st.print

let get_chats st = st.chats |> List.map fst

let change_chat s st =
  let chat_n = s |> String.lowercase_ascii in
  let chats = st.chats |> List.map
                (fun (n, i) -> (String.lowercase_ascii n, i)) in
  let chatname = List.fold_left
      (fun acc (n, i) -> if String.lowercase_ascii n = chat_n then n else acc)
      "" st.chats in
  if not (List.mem_assoc chat_n chats) then
    {st with print = [red ^ "Error: You are not in chat " ^ purp ^ s ^ red ^ "."]}
  else if (st.curr_chat |> fst |> String.lowercase_ascii) = chat_n then
    {st with print = [red ^ "Error: You are already in the chat."]}
  else {
    userid = st.userid;
    curr_chat = (chatname, (List.assoc chat_n chats));
    chats = st.chats;
    print = [red ^ "Entering chat " ^ purp ^ chatname ^ red ^ "..." ^ b];
  }

let check_chat s st =
  let open String in
  let join_chat = Str.regexp "#join \\(.+\\)" in
  let priv_chat = Str.regexp "#chatwith \\(.+\\)" in
  let pub_chat = Str.regexp "#makechat \\(.+\\)" in
  let leave_chat = Str.regexp "#leave \\(.+\\)" in
  let chats_low = st.chats |> List.map
                (fun (n, i) -> (String.lowercase_ascii n, i)) in
  if (Str.string_match priv_chat s 0) || (Str.string_match pub_chat s 0)
  then let name = sub s 10 ((length s) - 10) |> String.lowercase_ascii in
    if (contains name ' ') then
      {st with print = [red ^ "Error: Please use a chat name without spaces!"]}
    else if (List.mem_assoc name chats_low
             && not (Str.string_match pub_chat s 0)) then
      {st with print = [red ^ "Error: You are already in the chat!"]}
    else {st with print = []}
  else if (Str.string_match join_chat s 0)
  then let name = sub s 6 ((length s) - 6) |> String.lowercase_ascii in
    if not (List.mem_assoc name chats_low) then {st with print = []}
    else {st with print = [red ^ "Error: You are already in the chat!"]}
  else if (Str.string_match leave_chat s 0)
  then let name = sub s 7 ((length s) - 7) |> String.lowercase_ascii in
    if name = "lobby" then {st with print = [red ^ "Error: You can't leave the lobby!"]}
    else if (List.mem_assoc name chats_low) then {st with print = []}
    else {st with print = [red ^ "Error: You are not in chat " ^ purp ^ name]}
  else {st with print = []}

let parse_create_user s =
  "f, " ^ (String.length s |> string_of_int) ^ ":" ^ s

let parse_send s st =
  let open String in
  let uid = ", " ^ (st.userid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (st.userid |> string_of_int) in
  let chatid = ", " ^ (snd st.curr_chat |> string_of_int |> length |>
                       string_of_int)
               ^ ":" ^ (snd st.curr_chat |> string_of_int) in
  match s with
  (* Does not include [Help], [Create_user], and [Quit] which are
     managed in [View.ml]. *)
  | "#history" -> "b" ^ uid ^ chatid
  | "#users" -> "c" ^ uid
  | "#pubchats" -> "i" ^ uid
  | s -> begin
      let priv_chat = Str.regexp "#chatwith \\(.+\\)" in
      let pub_chat = Str.regexp "#makechat \\(.+\\)" in
      let join_chat = Str.regexp "#join \\(.+\\)" in
      let leave_chat = Str.regexp "#leave \\(.+\\)" in
      if Str.string_match priv_chat s 0
      then "d" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if Str.string_match pub_chat s 0
      then "e" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if Str.string_match join_chat s 0
      then "g" ^ uid ^ ", " ^ ((length s)-6 |> string_of_int) ^
           ":" ^ ((length s)-6 |> sub s 6)
      else if Str.string_match leave_chat s 0
      then "h" ^ uid ^ ", " ^ ((length s)-7 |> string_of_int) ^
           ":" ^ ((length s)-7 |> sub s 7)
      else "a" ^ uid ^ ", " ^ (s |> length |> string_of_int) ^
           ":" ^ s ^ chatid
    end

(* A helper function that checks whether a chat name is in a given list in
 * a case-insensitive manner. *)
let rec check_lower lst cname acc =
  match lst with
  | [] -> List.rev acc
  | (chatname, chatid)::t ->
    if String.lowercase_ascii chatname = String.lowercase_ascii cname
    then List.rev_append acc t else check_lower t cname ((chatname, chatid)::acc)

(* A helper function for [parse_receive] that extracts infromation from
 * string of the format "<len>:<name>". *)
let rec extract s acc =
  let open String in
  match s with
  | "" -> acc
  | s -> begin
    let c_loc = index_from s 0 ':' in
    let len = (sub s 0 c_loc) |> int_of_string in
    let m = (sub s (c_loc+1) len) in
    let new_s = sub s (c_loc + len + 1) ((length s) - c_loc - len - 1) in
    extract new_s (acc @ [m])
  end

let parse_receive s st =
  let open String in
  let c_id = get s 3 in
  if (String.get s 0) == 'f' then
    let snd_c = index_from s 2 ':' in
    let mes = sub s (snd_c + 1) ((length s) - snd_c - 1) in
    {st with print = [red ^ mes]}
  else let len_of_uid =
        sub s 6 ((index_from s 6 ':')-6)
        |> int_of_string in
    match (c_id) with
    | 'a' -> {st with print = []}
    | 'b' -> begin
        let len = length s in
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let his = sub s (trd_c + 1) (len - trd_c - 1) in
        {st with print = color cyan (extract his [])}
      end
    | 'c' -> begin
        let snd_c = index_from s 2 ':' in
        let user_num = sub s 6 (snd_c - 6) |> int_of_string in
        if user_num == 0 then
          {st with print = [red ^ "No users online currently."]}
        else let users = sub s (snd_c + 1) ((length s) - snd_c -1) in
          {st with print = color green (extract users [])}
      end
    | 'f' -> begin
        let uid =
          sub s ((index_from s 6 ':') + 1) len_of_uid
          |> int_of_string in
        {
          userid = uid;
          curr_chat = ("Lobby", 0);
          chats = [("Lobby", 0)];
          print = []
        }
        end
    | 'i' -> begin
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let pub_chats = sub s (trd_c + 1) ((length s) - trd_c - 1) in
        if (extract pub_chats []) <> [] then
          {st with print = color purp (extract pub_chats [])}
        else {st with print = [red ^ "No public chats available currently."]}
      end
    (*Response strings involving <len of chatid>:<chatid>*)
    | others ->
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let trd_comma = index_from s (trd_c + 1) ',' in
        let fth_c = index_from s (trd_c + 1) ':' in
        let chatid = sub s (trd_c + 1) (trd_comma - trd_c - 1)
                     |> int_of_string in
        let info = sub s (fth_c + 1) ((length s) - fth_c - 1) in
        match others with
        | 'h' -> {
          userid = st.userid;
          curr_chat = ("Lobby", 0);
          chats = check_lower st.chats info [];
          print = [red ^ "Returning to " ^ purp ^ "Lobby" ^ red ^ "..."]
        }
        | 'j' -> begin
          if ((snd st.curr_chat) <> chatid) then
            {st with print = []}
          else {st with print = color blue [info]}
        end
        | 'k' -> begin
            let chat_len = (sub s (trd_comma + 2) (fth_c - trd_comma - 2))
                           |> int_of_string in
            let chat_n = sub s (fth_c + 1) chat_len in
            let fifth_c = index_from s (fth_c + chat_len + 1) ':' in
            let msg = sub s (fifth_c + 1) ((length s) - fifth_c - 1) in
            if msg = " has started a chat with you." then
          {
            userid = st.userid;
            curr_chat = st.curr_chat;
            chats = (chat_n, chatid) :: st.chats;
            print = [green ^ chat_n ^ red ^ msg];
          } else
              if chatid <> (st.curr_chat |> snd) then {st with print = []}
              else
            {
              userid = st.userid;
              curr_chat = st.curr_chat;
              chats = st.chats;
              print = [green ^ chat_n ^ red ^ msg];
            }
        end
        (* Response strings d, e, g all return the same update. *)
        | same -> {
            userid = st.userid;
            curr_chat = (info, chatid);
            chats = (info, chatid) :: st.chats;
            print = [red ^ "Entering chat " ^ purp ^ info ^ red ^ "..."];
        }
