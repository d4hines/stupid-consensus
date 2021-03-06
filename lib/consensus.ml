type node = { value : bool; reliable : bool; id : int }

type message = { value : bool; id : int }

let majority_value : message list -> bool =
 fun l ->
  if (List.length l) mod 2 = 0 then failwith "Requires odd number";
  let sum =
    List.fold_left (fun acc m -> if m.value then acc + 1 else acc) 0 l
  in
  let avg = Float.of_int sum /. (List.length l |> Float.of_int) in
  Float.round avg = 1.0

let count_values : message list -> int * int =
 fun l ->
  List.fold_left
    (fun (t, f) m -> if m.value then (t + 1, f) else (t, f + 1))
    (0, 0) l

let message_sent_in_phase_2 messages_phase_1 = majority_value messages_phase_1

let value_after_round_k s k messages_phase_1 messages_phase_2 =
  match count_values messages_phase_1 with
  | t, _ when t >= s -> true
  | _, f when f >= s -> false
  | _ -> (List.find (fun m -> m.id = k) messages_phase_2).value
