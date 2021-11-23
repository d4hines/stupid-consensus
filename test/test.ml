let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing; ]
  in
  Alcotest.run "my test" [
    "suite", suite
  ]
