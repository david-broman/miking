open Printf
open Intrinsics
open Ustring.Op

let utest str l r =
  if l = r then printf "."
  else (
    printf "\nERROR: test '%s' failed\n" str ;
    exit 1 )

let check_seq s seq =
    let rec work k = function
      | x :: xs ->
          if Mseq.get s k = x then work (k + 1) xs else false
      | [] ->
         true
    in
    work 0 seq

let print_seq s = Mseq.iter (printf "%d, ") s; printf "\n\n"
let print_list s = List.iter (printf "%d, ") s; printf "\n\n"
let my_sublist lst a n =
  let (_, lst') =
      List.fold_left
        (fun (i,acc) x -> if i < a || i >= a + n
                          then (i+1, acc) else (i+1, x::acc)) (0, []) lst
    in
    List.rev lst'



let test_mseq () =
  printf "Testing MSeq: " ;
  (* create, length and is_length_at_least *)
  let s1 = Mseq.create 10 (fun k -> k) in
  let s1_real = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  utest "Mseq.create" (Mseq.length s1) 10 ;
  utest "Mseq.is_length_at_least" (Mseq.is_length_at_least s1 10) true ;
  utest "Mseq.is_length_at_least" (Mseq.is_length_at_least s1 11) false ;
  (* concat *)
  let s2 = Mseq.concat s1 (Mseq.create 2 (fun k -> k)) in
  let s2_real = s1_real @ [0; 1] in
  utest "Mseq.concat #1" (Mseq.length s2) 12 ;
  utest "Mseq.concat #2" true (check_seq s2 s2_real) ;
  (* empty *)
  let s3 = Mseq.concat s2 Mseq.empty in
  let s3_real = s2_real in
  utest "Mseq.empty" (Mseq.length s3) 12 ;
  (* get *)
  utest "Mseq.get #1" (Mseq.get s1 1) 1 ;
  utest "Mseq.get #2" (Mseq.get s1 5) 5 ;
  utest "Mseq.get #3" (Mseq.get s2 10) 0 ;
  (* set *)
  let s4 = Mseq.concat s2 (Mseq.create 10 (fun k -> k + 100)) in
  let s4_real = s2_real @ List.map (fun x -> x + 100) s1_real in
  utest "Mseq.concat #3" true (check_seq s4 s4_real) ;
  utest "Mseq.length" (Mseq.length (Mseq.set s4 11 99)) 22 ;
  utest "Mseq.length" (Mseq.length s4) 22 ;
  utest "Mseq.set #1" (Mseq.get (Mseq.set s1 2 99) 0) 0 ;
  utest "Mseq.set #2" (Mseq.get (Mseq.set s1 2 99) 2) 99 ;
  utest "Mseq.set #2" (Mseq.get (Mseq.set s4 4 99) 4) 99 ;
  utest "Mseq.set #2" (Mseq.get (Mseq.set s4 11 99) 11) 99 ;
  utest "Mseq.set #2" (Mseq.get (Mseq.set s4 13 99) 13) 99 ;
  (* cons *)
  utest "Mseq.cons" true (check_seq (Mseq.cons 77 s4) (77 :: s4_real)) ;
  (* snoc *)
  utest "Mseq.snoc" true
    (check_seq (Mseq.snoc s2 77) (List.rev (77 :: List.rev s2_real))) ;
  utest "Mseq.snoc" true
    (check_seq (Mseq.snoc s4 77) (List.rev (77 :: List.rev s4_real))) ;
  (* reverse *)
  utest "Mseq.reverse" true
    (check_seq (Mseq.reverse s4) (List.rev s4_real));
  (* head *)
  utest "Mseq.head" (Mseq.head s1) (List.hd s1_real);
  utest "Mseq.head" (Mseq.head s2) (List.hd s2_real);
  utest "Mseq.head" (Mseq.head s3) (List.hd s3_real);
  utest "Mseq.head" (Mseq.head s4) (List.hd s4_real);
  (* tail *)
  utest "Mseq.tail" true
    (check_seq (Mseq.tail s1) (List.tl s1_real));
  utest "Mseq.tail" true
    (check_seq (Mseq.tail s2) (List.tl s2_real));
  utest "Mseq.tail" true
    (check_seq (Mseq.tail s3) (List.tl s3_real));
  utest "Mseq.tail" true
    (check_seq (Mseq.tail s4) (List.tl s4_real));
  (* null *)
  utest "Mseq.null #1" (Mseq.null s4) false;
  utest "Mseq.null #2" (Mseq.null Mseq.empty) true;
  (* iter *)
  let l = ref [] in
  Mseq.iter (fun x -> l := x::!l) s4;
  utest "Mseq.iter" ((List.rev !l) = s4_real) true;
  (* iteri *)
  l := [];
  let k = ref [] in
  Mseq.iteri (fun i x -> l := x::!l; k := i::!k) s4;
  utest "Mseq.iteri #1" ((List.rev !l) = s4_real) true;
  let k_real = List.init (List.length s4_real) (fun x -> x) in
  utest "Mseq.iteri #2" ((List.rev !k) = k_real) true;
  (* split_at *)
  let x_split = 14 in
  let s4_real_a = my_sublist s4_real 0 x_split in
  let s4_real_b = my_sublist s4_real x_split (List.length s4_real - x_split) in
  let (s4_a, s4_b) = Mseq.split_at s4 x_split in
  utest "Mseq.split_at #1" true (check_seq s4_a s4_real_a);
  utest "Mseq.split_at #2" true (check_seq s4_b s4_real_b);
  (* subsequence *)
  let s4_sub_real = my_sublist s4_real 7 9 in
  let s4_sub = Mseq.subsequence s4 7 9 in
  utest "Mseq.subsequence" true (check_seq s4_sub s4_sub_real);
  (* map *)
  let adding x = x + 1000 in
  let (s, s_real) = (Mseq.map adding s4, List.map adding s4_real) in
  utest "Mseq.map" true (check_seq s s_real);
  (* mapi *)
  let adding_i i x = x + 100 * i in
  let (s, s_real) = (Mseq.mapi adding_i s4, List.mapi adding_i s4_real) in
  utest "Mseq.mapi" true (check_seq s s_real);
  (* The end *)
  (* to_list, to_seq, of_list, of_seq *)
  utest "Mseq.to_list" true ((Mseq.Helpers.to_list s4) = s4_real);
  let s4_list2 = (Mseq.Helpers.to_seq s4) |> List.of_seq in
  utest "Mseq.to_seq" true (s4_list2 = s4_real);
  utest "Mseq.of_list" true (check_seq (Mseq.Helpers.of_list s4_real) s4_real);
  let s4_l = Mseq.Helpers.of_seq (List.to_seq s4_real) in
  utest "Mseq.of_seq" true (check_seq s4_l s4_real);
  (* to_array, of_array *)
  let s4_l = Mseq.Helpers.to_array s4 |> Array.to_list in
  utest "Mseq.to_array" true (s4_l = s4_real);
  let s4_l = Mseq.Helpers.of_array (Array.of_list s4_real) in
  utest "Mseq.of_array" true (check_seq s4_l s4_real);
  (* ustring *)
  let str = "This is a string" in
  let ustr = us str in
  let ustr2 = ustr |> Mseq.Helpers.of_ustring |> Mseq.Helpers.to_ustring in
  utest "Mseq.of_ustring and Mseq.to_ustring" true (ustr2 =. ustr);
  let str2 = str |> Mseq.Helpers.of_utf8 |> Mseq.Helpers.to_utf8 in
  utest "Mseq.of_utf8 and Mseq.to_utf8" true (str2 = str);
  (* equal *)
  let f x y = (x = y) in
  utest "Mseq.equal #1" true (Mseq.Helpers.equal f s4 s4);
  utest "Mseq.equal #2" false (Mseq.Helpers.equal f s4 s3);
  (* fold_left *)
  let f1 a x = a + x in
  let s4_real_fl = List.fold_left f1 0 s4_real in
  let s4_fl = Mseq.Helpers.fold_left f1 0 s4 in
  utest "Mseq.fold_left #1" true (s4_fl = s4_real_fl);
  let f2 a x = x::a in
  let s4_real_fl = List.fold_left f2 [] s4_real in
  let s4_fl = Mseq.Helpers.fold_left f2 [] s4 in
  utest "Mseq.fold_left #1" true (s4_fl = s4_real_fl);
  (* fold_right *)
  let f x a = x::a in
  let s4_real_fr = List.fold_right f s4_real [] in
  let s4_fr = Mseq.Helpers.fold_right f [] s4 in
  utest "Mseq.fold_right" true (s4_fr = s4_real_fr);
  (* combine *)
  let s4rev_real = List.rev s4_real in
  let s4rev = Mseq.reverse s4 in
  let comb_real = List.combine s4rev_real s4_real in
  let comb = Mseq.Helpers.combine s4rev s4 in
  utest "Mseq.combine" true (comb_real = (Mseq.Helpers.to_list comb));
()
(*  List.iter (fun (x,y) -> printf "(%d,%d) " x y) comb_real; *)

let run_tests () =
  test_mseq () ;
  printf "\nAll tests were successful\n"
