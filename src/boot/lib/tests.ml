open Printf
open Intrinsics

let utest str l r =
  if l = r then printf "."
  else (
    printf "\nERROR: test '%s' failed\n" str ;
    exit 1 )

let check_seq s seq =
    let rec work k = function
      | x :: xs ->
          if MyMseq.get s k = x then work (k + 1) xs else false
      | [] ->
         true
    in
    work 0 seq

let print_seq s = MyMseq.iter (printf "%d, ") s; printf "\n\n"
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
  let s1 = MyMseq.create 10 (fun k -> k) in
  let s1_real = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  utest "MyMseq.create" (MyMseq.length s1) 10 ;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s1 10) true ;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s1 11) false ;
  (* concat *)
  let s2 = MyMseq.concat s1 (MyMseq.create 2 (fun k -> k)) in
  let s2_real = s1_real @ [0; 1] in
  utest "MyMseq.concat #1" (MyMseq.length s2) 12 ;
  utest "MyMseq.concat #2" true (check_seq s2 s2_real) ;
  (* empty *)
  let s3 = MyMseq.concat s2 MyMseq.empty in
  let s3_real = s2_real in
  utest "MyMseq.empty" (MyMseq.length s3) 12 ;
  (* get *)
  utest "MyMseq.get #1" (MyMseq.get s1 1) 1 ;
  utest "MyMseq.get #2" (MyMseq.get s1 5) 5 ;
  utest "MyMseq.get #3" (MyMseq.get s2 10) 0 ;
  (* set *)
  let s4 = MyMseq.concat s2 (MyMseq.create 10 (fun k -> k + 100)) in
  let s4_real = s2_real @ List.map (fun x -> x + 100) s1_real in
  utest "MyMseq.concat #3" true (check_seq s4 s4_real) ;
  utest "MyMseq.length" (MyMseq.length (MyMseq.set s4 11 99)) 22 ;
  utest "MyMseq.length" (MyMseq.length s4) 22 ;
  utest "MyMseq.set #1" (MyMseq.get (MyMseq.set s1 2 99) 0) 0 ;
  utest "MyMseq.set #2" (MyMseq.get (MyMseq.set s1 2 99) 2) 99 ;
  utest "MyMseq.set #2" (MyMseq.get (MyMseq.set s4 4 99) 4) 99 ;
  utest "MyMseq.set #2" (MyMseq.get (MyMseq.set s4 11 99) 11) 99 ;
  utest "MyMseq.set #2" (MyMseq.get (MyMseq.set s4 13 99) 13) 99 ;
  (* cons *)
  utest "MyMseq.cons" true (check_seq (MyMseq.cons 77 s4) (77 :: s4_real)) ;
  (* snoc *)
  utest "MyMseq.snoc" true
    (check_seq (MyMseq.snoc s2 77) (List.rev (77 :: List.rev s2_real))) ;
  utest "MyMseq.snoc" true
    (check_seq (MyMseq.snoc s4 77) (List.rev (77 :: List.rev s4_real))) ;
  (* reverse *)
  utest "MyMseq.reverse" true
    (check_seq (MyMseq.reverse s4) (List.rev s4_real));
  (* head *)
  utest "MyMseq.head" (MyMseq.head s1) (List.hd s1_real);
  utest "MyMseq.head" (MyMseq.head s2) (List.hd s2_real);
  utest "MyMseq.head" (MyMseq.head s3) (List.hd s3_real);
  utest "MyMseq.head" (MyMseq.head s4) (List.hd s4_real);
  (* tail *)
  utest "MyMseq.tail" true
    (check_seq (MyMseq.tail s1) (List.tl s1_real));
  utest "MyMseq.tail" true
    (check_seq (MyMseq.tail s2) (List.tl s2_real));
  utest "MyMseq.tail" true
    (check_seq (MyMseq.tail s3) (List.tl s3_real));
  utest "MyMseq.tail" true
    (check_seq (MyMseq.tail s4) (List.tl s4_real));
  (* null *)
  utest "MyMseq.null #1" (MyMseq.null s4) false;
  utest "MyMseq.null #2" (MyMseq.null MyMseq.empty) true;
  (* iter *)
  let l = ref [] in
  MyMseq.iter (fun x -> l := x::!l) s4;
  utest "MyMseq.iter" ((List.rev !l) = s4_real) true;
  (* iteri *)
  l := [];
  let k = ref [] in
  MyMseq.iteri (fun i x -> l := x::!l; k := i::!k) s4;
  utest "MyMseq.iteri #1" ((List.rev !l) = s4_real) true;
  let k_real = List.init (List.length s4_real) (fun x -> x) in
  utest "MyMseq.iteri #2" ((List.rev !k) = k_real) true;
  (* split_at *)
  let x_split = 14 in
  let s4_real_a = my_sublist s4_real 0 x_split in
  let s4_real_b = my_sublist s4_real x_split (List.length s4_real - x_split) in
  let (s4_a, s4_b) = MyMseq.split_at s4 x_split in
  utest "MyMseq.split_at #1" true (check_seq s4_a s4_real_a);
  utest "MyMseq.split_at #2" true (check_seq s4_b s4_real_b);
  (* subsequence *)
  let s4_sub_real = my_sublist s4_real 7 9 in
  let s4_sub = MyMseq.subsequence s4 7 9 in
  utest "MyMseq.subsequence" true (check_seq s4_sub s4_sub_real);
  (* map *)
  let adding x = x + 1000 in
  let (s, s_real) = (MyMseq.map adding s4, List.map adding s4_real) in
  utest "MyMseq.map" true (check_seq s s_real);
  (* mapi *)
  let adding_i i x = x + 100 * i in
  let (s, s_real) = (MyMseq.mapi adding_i s4, List.mapi adding_i s4_real) in
  utest "MyMseq.mapi" true (check_seq s s_real);
  (* The end *)
  (* to_list, to_seq, of_list, of_seq *)
  utest "MyMseq.to_list" true ((MyMseq.Helpers.to_list s4) = s4_real);
  let s4_list2 = (MyMseq.Helpers.to_seq s4) |> List.of_seq in
  utest "MyMseq.to_seq" true (s4_list2 = s4_real);
  utest "MyMseq.of_list" true (check_seq (MyMseq.Helpers.of_list s4_real) s4_real);
  let s4_l = MyMseq.Helpers.of_seq (List.to_seq s4_real) in
  utest "MyMseq.of_seq" true (check_seq s4_l s4_real);
  (* to_array, of_array *)
  let s4_l = MyMseq.Helpers.to_array s4 |> Array.to_list in
  utest "MyMseq.to_array" true (s4_l = s4_real);
  let s4_l = MyMseq.Helpers.of_array (Array.of_list s4_real) in
  utest "MyMseq.of_array" true (check_seq s4_l s4_real);
  ()

let run_tests () =
  test_mseq () ;
  printf "\nAll tests were successful\n"
