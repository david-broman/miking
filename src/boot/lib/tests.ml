open Printf
open Intrinsics

let utest str l r =
  if l = r then printf "."
  else (
    printf "\nERROR: test '%s' failed\n" str ;
    exit 1 )

let test_mseq () =
  printf "Testing MSeq: " ;
  (* create, length and is_length_at_least *)
  let s1 = MyMseq.create 10 (fun k -> k) in
  utest "MyMseq.create" (MyMseq.length s1) 10 ;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s1 10) true ;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s1 11) false ;
  (* concat *)
  let s2 = MyMseq.concat s1 (MyMseq.create 2 (fun k -> k)) in
  utest "MyMseq.concat" (MyMseq.length s2) 12 ;
  (* empty *)
  let s3 = MyMseq.concat s2 MyMseq.empty in
  utest "MyMseq.empty" (MyMseq.length s3) 12 ;
  (* get *)
  utest "MyMseq.get #1" (MyMseq.get s1 1) 1 ;
  utest "MyMseq.get #2" (MyMseq.get s1 5) 5 ;
  utest "MyMseq.get #3" (MyMseq.get s2 10) 0 ;
  (* set *)
  let s4 = MyMseq.concat s2 (MyMseq.create 10 (fun k -> k + 100)) in
  utest "MyMSeq.length" (MyMseq.length (MyMseq.set s4 11 99)) 22 ;
  utest "MyMSeq.length" (MyMseq.length s4) 22 ;
  utest "MyMSeq.set #1" (MyMseq.get (MyMseq.set s1 2 99) 0) 0 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s1 2 99) 2) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 4 99) 4) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 11 99) 11) 99 ;
  utest "MyMSeq.set #2" (MyMseq.get (MyMseq.set s4 13 99) 13) 99

let run_tests () =
  test_mseq () ;
  printf "\nAll tests were successful\n"
