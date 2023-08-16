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
  utest "MyMSeq.length" (MyMseq.length (MyMseq.set s4 11 99)) 22 ;
  utest "MyMSeq.length" (MyMseq.length s4) 22 ;
  utest "MyMSeq.set #1" (MyMseq.get (MyMseq.set s1 2 99) 0) 0 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s1 2 99) 2) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 4 99) 4) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 11 99) 11) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 13 99) 13) 99 ;
  (* cons *)
  utest "MyMSeq.cons" true (check_seq (MyMseq.cons 77 s4) (77 :: s4_real)) ;
  (* snoc *)
  utest "MyMSeq.snoc" true
    (check_seq (MyMseq.snoc s2 77) (List.rev (77 :: List.rev s2_real))) ;
  utest "MyMSeq.snoc" true
    (check_seq (MyMseq.snoc s4 77) (List.rev (77 :: List.rev s4_real))) ;
  (* reverse *)
  utest "MyMSeq.reverse" true
    (check_seq (MyMseq.reverse s4) (List.rev s4_real));
  (* head *)
  utest "MyMSeq.head" (MyMseq.head s1) (List.hd s1_real);
  utest "MyMSeq.head" (MyMseq.head s2) (List.hd s2_real);
  utest "MyMSeq.head" (MyMseq.head s3) (List.hd s3_real);
  utest "MyMSeq.head" (MyMseq.head s4) (List.hd s4_real);
  (* tail *)
  utest "MyMSeq.tail" true
    (check_seq (MyMseq.tail s1) (List.tl s1_real));
  utest "MyMSeq.tail" true
    (check_seq (MyMseq.tail s2) (List.tl s2_real));
  utest "MyMSeq.tail" true
    (check_seq (MyMseq.tail s3) (List.tl s3_real));
  utest "MyMSeq.tail" true
    (check_seq (MyMseq.tail s4) (List.tl s4_real));
  (* null *)
  utest "MyMSeq.null #1" (MyMseq.null s4) false;
  utest "MyMSeq.null #2" (MyMseq.null MyMseq.empty) true;
  (* iter *)
  let l = ref [] in
  MyMseq.iter (fun x -> l := x::!l) s4;
  utest "MyMSeq.iter" ((List.rev !l) = s4_real) true;
  (* iteri *)
  l := [];
  let k = ref [] in
  MyMseq.iteri (fun i x -> l := x::!l; k := i::!k) s4;
  utest "MyMSeq.iteri #1" ((List.rev !l) = s4_real) true;
  let k_real = List.init (List.length s4_real) (fun x -> x) in
  utest "MyMSeq.iteri #2" ((List.rev !k) = k_real) true;
  (* split_at *)
  let x_split = 14 in
  let s4_real_a = my_sublist s4_real 0 x_split in
  let s4_real_b = my_sublist s4_real x_split (List.length s4_real - x_split) in
  let (s4_a, s4_b) = MyMseq.split_at s4 x_split in
  utest "MyMSeq.split_at #1" true (check_seq s4_a s4_real_a);
  utest "MyMSeq.split_at #2" true (check_seq s4_b s4_real_b);

  (* The end *)
  ()

let run_tests () =
  test_mseq () ;
  printf "\nAll tests were successful\n"
