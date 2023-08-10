

open Printf
open Intrinsics

let utest str l r =
  if l = r then printf "." else (printf "\nERROR: test '%s' failed\n" str; exit 1)


let test_mseq () =
  printf "Testing MSeq: ";

  let s = MyMseq.create 10 (fun k -> k) in
  utest "MyMseq.create" (MyMseq.length s) 10;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s 10) true;
  utest "MyMseq.is_length_at_least" (MyMseq.is_length_at_least s 11) false;

  let s2 = MyMseq.concat s (MyMseq.create 2 (fun k -> k)) in
  utest "MyMseq.concat" (MyMseq.length s2) 12;

  let s3 = MyMseq.concat s2 MyMseq.empty in
  utest "MyMseq.empty" (MyMseq.length s3) 12;

  utest "MyMseq.get #1" (MyMseq.get s 1) 1;
  utest "MyMseq.get #2" (MyMseq.get s 5) 5;
  utest "MyMseq.get #3" (MyMseq.get s2 10) 0



let run_tests () =
  test_mseq();
  printf "\nAll tests were successful\n"
