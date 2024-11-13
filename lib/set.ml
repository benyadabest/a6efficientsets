(** AF: A tree node represents a set containing all elements stored in the node 
    and its children. Leaf represents the empty set. Nodes2(l, x, r) represents a binary node with element x and children l. 
    Nodes3(l, x, m, y, r) represents a ternary node with elements x,y and children l,m,r.
    RI: All elements in left subtree of x are less than x and all elements in right subtree of x are greater than x.
    In Nodes3, x < y and all elements in middle subtree are between x and y. All paths from root to leaf have
     same length (perfect height balance). Tree is either empty or root is a 2-node.*)
type 'a t =
  | Leaf
  | Nodes2 of 'a t * 'a * 'a t
  | Nodes3 of 'a t * 'a * 'a t * 'a * 'a t

type 'a insert_result = {
  tree: 'a t;
  grew: bool;
}

let empty = Leaf

let is_empty = function
  | Leaf -> true
  | _ -> false

let compare x y = if x < y then -1 else if x > y then 1 else 0

let rec insert_inner x = function
  | Leaf -> 
    { tree = Nodes2(Leaf, x, Leaf); grew = true }
  | Nodes2(l, y, r) as t ->
    (match compare x y with
      | 0 -> { tree = t; grew = false }
      | -1 -> 
        let res = insert_inner x l in
        if not res.grew then
          { tree = Nodes2(res.tree, y, r); grew = false }
        else
          (match res.tree with
          | Nodes2(l', y', r') -> 
            { tree = Nodes2(Nodes2(l', y', r'), y, r); grew = false }
          | Nodes3(l', y', m', z', r') -> 
            { tree = Nodes3(l', y', m', z', Nodes2(r', y, r)); grew = false }
          | Leaf -> failwith "impossible: insert_inner into leaf returns Nodes2")
      | _ -> 
        let res = insert_inner x r in
        if not res.grew then
          { tree = Nodes2(l, y, res.tree); grew = false }
        else
          (match res.tree with
          | Nodes2(l', y', r') -> 
            { tree = Nodes2(l, y, Nodes2(l', y', r')); grew = false }
          | Nodes3(l', y', m', z', r') ->
            { tree = Nodes3(Nodes2(l, y, l'), y', m', z', r'); grew = false }
          | Leaf -> failwith "impossible: insert_inner into leaf returns Nodes2"))
  | Nodes3(l, y, m, z, r) as t ->
    match compare x y, compare x z with
    | 0, _ -> { tree = t; grew = false }
    | _, 0 -> { tree = t; grew = false }
    | -1, _ ->
      let res = insert_inner x l in
      if not res.grew then
        { tree = Nodes3(res.tree, y, m, z, r); grew = false }
      else
        (match res.tree with
        | Nodes2(a, v, b) ->
          { tree = Nodes2(Nodes2(a, v, b), y, Nodes2(m, z, r)); grew = true }
        | Nodes3(a, v1, b, v2, c) ->
          { tree = Nodes2(Nodes2(a, v1, b), v2, Nodes2(c, y, Nodes2(m, z, r))); grew = true }
        | Leaf -> failwith "impossible: insert_inner into leaf returns Nodes2")
    | _, 1 ->
      let res = insert_inner x r in
      if not res.grew then
        { tree = Nodes3(l, y, m, z, res.tree); grew = false }
      else
        (match res.tree with
        | Nodes2(a, v, b) ->
          { tree = Nodes2(Nodes2(l, y, m), z, Nodes2(a, v, b)); grew = true }
        | Nodes3(a, v1, b, v2, c) ->
          { tree = Nodes2(Nodes2(l, y, m), z, Nodes2(a, v1, Nodes2(b, v2, c))); grew = true }
        | Leaf -> failwith "impossible: insert_inner into leaf returns Nodes2")
    | _ ->
      let res = insert_inner x m in
      if not res.grew then
        { tree = Nodes3(l, y, res.tree, z, r); grew = false }
      else
        (match res.tree with
        | Nodes2(a, v, b) ->
          { tree = Nodes2(Nodes2(l, y, a), v, Nodes2(b, z, r)); grew = true }
        | Nodes3(a, v1, b, v2, c) ->
          { tree = Nodes2(Nodes2(l, y, a), v1, Nodes2(b, v2, Nodes2(c, z, r))); grew = true }
        | Leaf -> failwith "impossible: insert_inner into leaf returns Nodes2")

let insert x t =
  (insert_inner x t).tree

let rec mem x = function
  | Leaf -> false
  | Nodes2(l, y, r) ->
    (match compare x y with
      | 0 -> true
      | -1 -> mem x l
      | _ -> mem x r)
  | Nodes3(l, y, m, z, r) ->
    match compare x y, compare x z with
    | 0, _ | _, 0 -> true
    | -1, _ -> mem x l
    | _, 1 -> mem x r
    | _ -> mem x m

let rec to_string f = function [@coverage off]
  | Leaf -> "Leaf"
  | Nodes2(l, x, r) ->
    Printf.sprintf "Nodes2(%s, %s, %s)" 
      (to_string f l) (f x) (to_string f r)
  | Nodes3(l, x, m, y, r) ->
    Printf.sprintf "Nodes3(%s, %s, %s, %s, %s)"
      (to_string f l) (f x) (to_string f m) (f y) (to_string f r)

let rec check_height = function [@coverage off]
  | Leaf -> 0
  | Nodes2(l, _, r) ->
    let hl = check_height l in
    let hr = check_height r in
    if hl = hr then 1 + hl
    else failwith "unbalanced heights"
  | Nodes3(l, _, m, _, r) ->
    let hl = check_height l in
    let hm = check_height m in
    let hr = check_height r in
    if hl = hm && hm = hr then 1 + hl
    else failwith "unbalanced heights"

let rec check_order = function [@coverage off]
  | Leaf -> ()
  | Nodes2(l, x, r) ->
    check_order l;
    check_order r;
    let check_all_less t =
      let rec check = function
        | Leaf -> ()
        | Nodes2(l, y, r) ->
          if y >= x then failwith "ordering violation" else (check l; check r)
        | Nodes3(l, y, m, z, r) ->
          if y >= x || z >= x then failwith "ordering violation"
          else (check l; check m; check r)
      in check t
    in
    let check_all_greater t =
      let rec check = function
        | Leaf -> ()
        | Nodes2(l, y, r) ->
          if y <= x then failwith "ordering violation" else (check l; check r)
        | Nodes3(l, y, m, z, r) ->
          if y <= x || z <= x then failwith "ordering violation"
          else (check l; check m; check r)
      in check t
    in
    check_all_less l;
    check_all_greater r
  | Nodes3(l, x, m, y, r) ->
    if x >= y then failwith "ordering violation";
    check_order l;
    check_order m;
    check_order r;
    let check_all_less v t =
      let rec check = function
        | Leaf -> ()
        | Nodes2(l, z, r) ->
          if z >= v then failwith "ordering violation" else (check l; check r)
        | Nodes3(l, z, m, w, r) ->
          if z >= v || w >= v then failwith "ordering violation"
          else (check l; check m; check r)
      in check t
    in
    let check_all_between v1 v2 t =
      let rec check = function
        | Leaf -> ()
        | Nodes2(l, z, r) ->
          if z <= v1 || z >= v2 then failwith "ordering violation"
          else (check l; check r)
        | Nodes3(l, z, m, w, r) ->
          if z <= v1 || z >= v2 || w <= v1 || w >= v2
          then failwith "ordering violation"
          else (check l; check m; check r)
      in check t
    in
    let check_all_greater v t =
      let rec check = function
        | Leaf -> ()
        | Nodes2(l, z, r) ->
          if z <= v then failwith "ordering violation" else (check l; check r)
        | Nodes3(l, z, m, w, r) ->
          if z <= v || w <= v then failwith "ordering violation"
          else (check l; check m; check r)
      in check t
    in
    check_all_less x l;
    check_all_between x y m;
    check_all_greater y r

[@@@coverage off]
let rep_ok t =
  check_height t |> ignore;
  check_order t;
  t