open OUnit2
open A6

(* is_empty empty = true*)
let test_eq1 _ =
  assert_equal true (Set.is_empty Set.empty)

(* is_empty (insert x s) = false*)
let test_eq2 _ =
  assert_equal false (Set.is_empty (Set.insert 1 Set.empty));
  assert_equal false (Set.is_empty (Set.insert 2 Set.empty));
  assert_equal false (Set.is_empty (Set.insert 2 (Set.insert 1 Set.empty)))

(* mem x empty = false*)
let test_eq3 _ =
  assert_equal false (Set.mem 1 Set.empty);
  assert_equal false (Set.mem (-1) Set.empty);
  assert_equal false (Set.mem 0 Set.empty)

(* mem y (insert x s) = true if x = y*)
let test_eq4 _ =
  assert_equal true (Set.mem 1 (Set.insert 1 Set.empty));
  assert_equal true (Set.mem 2 (Set.insert 2 Set.empty));
  let s = Set.insert 3 (Set.insert 2 (Set.insert 1 Set.empty)) in
  assert_equal true (Set.mem 1 s);
  assert_equal true (Set.mem 2 s);
  assert_equal true (Set.mem 3 s)

(* mem y (insert x s) = mem y s if x <> y*)
let test_eq5 _ =
  let s1 = Set.insert 1 Set.empty in
  assert_equal (Set.mem 2 s1) (Set.mem 2 (Set.insert 3 s1));
  let s2 = Set.insert 2 s1 in
  assert_equal (Set.mem 1 s2) (Set.mem 1 (Set.insert 3 s2));
  assert_equal (Set.mem 2 s2) (Set.mem 2 (Set.insert 4 s2))

let random_unique_list n =
  let rec add_unique acc count =
    if count = 0 then acc
    else 
      let x = Random.int 1000 in
      if List.mem x acc then add_unique acc count
      else add_unique (x::acc) (count-1)
  in
  List.sort compare (add_unique [] n)

let test_random_100 _ =
  let nums = random_unique_list 100 in
  let set = List.fold_left (fun s x -> Set.insert x s) Set.empty nums in
  List.iter (fun x -> assert_equal true (Set.mem x set)) nums;
  List.iter (fun x -> 
    if not (List.mem x nums) then 
      assert_equal false (Set.mem x set)
  ) [-10; -1; 1001; 1002]

let suite = "set_test_suite" >::: [
  "test_eq1" >:: test_eq1;
  "test_eq2" >:: test_eq2;
  "test_eq3" >:: test_eq3;
  "test_eq4" >:: test_eq4;
  "test_eq5" >:: test_eq5;
  "test_random_100" >:: test_random_100;
]

let _ = run_test_tt_main suite