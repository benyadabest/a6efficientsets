open A6

let time f x =
  let start = Unix.gettimeofday () in
  let _ = f x in
  let stop = Unix.gettimeofday () in
  stop -. start

let random_list n =
  let rec aux acc remaining =
    if remaining = 0 then acc
    else
      let x = Random.int (n * 2) in
      if List.mem x acc then aux acc remaining
      else aux (x :: acc) (remaining - 1)
  in
  aux [] n

let measure_insert_time n =
  Printf.printf "[Measuring n=%d...]\n%!" n;
  let nums = random_list n in
  let samples = [1;2;3] in
  let times = List.map (fun _ -> 
    time (fun nums -> 
      List.fold_left (fun s x -> Set.insert x s) Set.empty nums
    ) nums
  ) samples in
  let sorted = List.sort Float.compare times in
  List.nth sorted 1

let () =
  Random.self_init ();
  Printf.printf "N,N log N,Time\n%!";
  
  let ns = [
    50_000;
    100_000;
    200_000;
    500_000;
    1_000_000;
    2_000_000;
    3_000_000;
    4_000_000;
    5_000_000;
    6_000_000;
    7_000_000;
    8_000_000;
    9_000_000;
    10_000_000;
  ] in
  
  List.iter (fun n ->
    try
      let t = measure_insert_time n in
      Printf.printf "%d,%g,%g\n%!" 
        n 
        (float_of_int n *. log10 (float_of_int n)) 
        t
    with e -> 
      Printf.printf "[Error at n=%d: %s]\n%!" n (Printexc.to_string e)
  ) ns