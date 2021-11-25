open Consensus

let node_list_gen : node list QCheck.Gen.t =
  let open QCheck.Gen in
  int_range 1 10 >>= fun t ->
  float_range 4.0 6.0 >>= fun t_scale ->
  list_size (return t) (pair bool (return false)) >>= fun t_list ->
  let s = Float.of_int t *. t_scale |> Int.of_float in
  let s = if (s + t) mod 2 = 0 then s + 1 else s in
  list_size (return s) (pair bool (return true)) >|= fun s_list ->
  s_list @ t_list
  |> List.mapi (fun id (value, reliable) -> { value; reliable; id })

let node_list_print =
  let open QCheck.Print in
  list
  @@ comap
       (fun { value; reliable; id } -> (id, reliable, value))
       (triple int bool bool)

let node_list_arb =
  QCheck.make ~print:node_list_print ~shrink:QCheck.Shrink.list node_list_gen

let simulate nodes =
  let read_value node = if node.reliable then node.value else Random.bool () in
  let s = List.filter (fun x -> x.reliable) nodes |> List.length in
  let t = List.length nodes - s in
  let rec go k (nodes : node list) =
    if k = 0 then nodes
    else
      let message_phase_1 =
        List.map
          (fun node ->
            let value = read_value node in
            { value; id = node.id })
          nodes
      in
      let messages_phase_2 =
        List.map
          (fun (node : node) ->
            let value =
              if node.reliable then message_sent_in_phase_2 message_phase_1
              else Random.bool ()
            in
            { id = node.id; value })
          nodes
      in
      let new_nodes =
        List.map
          (fun (node : node) ->
            {
              node with
              value = value_after_round_k s k message_phase_1 messages_phase_2;
            })
          nodes
      in
      go (k - 1) new_nodes
  in
  go (t + 1) nodes

let enough_nodes l =
  let s, t = List.partition (fun x -> x.reliable) l in
  let s, t = (List.length s, List.length t) in
  s > 3 * t

let all_reliable_have_same_value =
  QCheck.Test.make ~count:10000
    ~name:"all reliable nodes have same value at t + 1" node_list_arb
    (fun node_list ->
      QCheck.assume (enough_nodes node_list);
      let final = simulate node_list |> List.filter (fun x -> x.reliable) in
      let first_value = (List.hd final).value in
      List.for_all (fun (x : node) -> x.value = first_value) final)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest [ all_reliable_have_same_value ]
  in
  Alcotest.run "my test" [ ("suite", suite) ]
