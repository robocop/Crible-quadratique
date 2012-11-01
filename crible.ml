let l n =  let n = float_of_int n in exp ((log n *. log (log n)) ** (0.5));;

let array_find i0 f arr =
  let t = Array.length arr in
  let rec loop i = 
    if i >= t then raise Not_found
    else if f (arr.(i)) then i
    else loop (i+1)
  in
  loop i0
;;


let crible n = 
  let tbl = Array.make (n+1) true in
  let i = ref 2 in
  while !i <= n do
    if tbl.(!i) = true then
      begin
	for j = 1 to (n-(!i))/(!i) do
	tbl.(!i + !i * j) <- false;
	done
      end;
    incr i
  done;
  let r = ref [] in
  for i = 2 to n do
    if tbl.(i) then r := i::!r;
  done;
  List.rev (!r) ;;


let base n = 
  Array.of_list (-1 :: crible (int_of_float ((l n)**0.5))) ;;

let factor_base base n = 
  let r = Array.make (Array.length base) 0 in
  if n < 0 then r.(0) <- 1;
  let rec factor i n = 
    if n = 1 then Some r
    else if i >= Array.length base then None
    else 
      if (n mod base.(i)) = 0 then (r.(i) <- r.(i) +1; factor i (n/(base.(i))) )
      else factor (i+1) n
  in
  factor 1 (abs n)
  ;;

let cherche_xi base nbr n = 
  let s = int_of_float (sqrt (float_of_int n)) in
  let rec cherche nb_a_trouver i b =
    if nb_a_trouver <= 0 then []
    else 
      let xi = s + (if b then i else -i) in
      Printf.printf "xi = %d\n" xi;
      let yi = xi*xi - n in
      (match factor_base base yi with
	  None -> cherche nb_a_trouver (if b then i else (i+1)) (not b)
	| Some d -> (xi, d) :: cherche (nb_a_trouver -1)  (if b then i else (i+1)) (not b)
      )
  in
  cherche nbr 0 false
;;
  
let matrix_id n = let m = Array.create_matrix n n 0 in for i = 0 to n-1 do m.(i).(i) <- 1 done; m;;

let add_ligne m i j = 
  for k = 0 to Array.length m.(0) -1 do
    m.(i).(k) <- (m.(i).(k) + m.(j).(k)) mod 2;
  done
;;

let ligne_nulle l = 
  let t = Array.length l in
  let rec test i = 
    if i >= t then true
    else if l.(i) <> 0 then false
    else test (i+1)
  in
  test 0
;;

let swap m i j = 
  let r = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- r
;;
(* Trouve une combinaison lineaire *)
let rec pivot m t (i0, j0) = 
  Printf.printf "i0 = %d, j0 = %d\n" i0 j0;
  if i0 >= Array.length m  || j0 >= Array.length (m.(0))  then failwith "impossible de trouver une combinaison linéaire"
  else
  (* On cherche la première ligne i (i >= i0) de M telle que l.(j0) <> 0 *)
  try
    let i = array_find i0 (fun l -> l.(j0) <> 0) m in
    (* Si elle existe, on échange la ligne i et la ligne i0 *)
    swap m i i0;
    Printf.printf "echange de la ligne %d et %d\n" i i0;
    for i = i0 + 1 to Array.length m -1 do
      if m.(i).(j0) <> 0 then
	begin
	  Printf.printf "L%d <- L%d + L%d\n" i i i0;
	  add_ligne m i i0;
	  add_ligne t i i0
	end
    done;
    pivot m t (i0+1, j0+1)

  with _ -> 
    print_endline "colonne nulle";
    (* On regarde si une ligne de M n'est pas nulle *)
    (try (array_find 0 ligne_nulle m, m, t)
     with Not_found -> pivot m t (i0+1, j0+1) 
    )

;;
let m = [|[|0;1;0;1;1|]; 
	  [|1;1;0;0;0|]; 
	  [|1;1;1;1;1|]; 
	  [|0;1;1;0;0|]
	|]
;;
let t = matrix_id 4;;

pivot m t (0,0);;
add_ligne m 0 0;;

let n = 11351541513;;
let b = base n;;
let nb = Array.length b +1;;
cherche_xi b nb n;;
