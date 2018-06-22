(* nymphabet
 * prelude.ml
 * Keith Waclena <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 John Jung & Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

(* A miniaturized version of KW's Prelude; for doc, see:
   https://www2.lib.uchicago.edu/keith/software/prelude/Prelude.html *)
let ($.) f g x = f (g x)
include List
let len = length
include Printf
let uncurry f = fun (x,y) -> f  x y
let foldl = List.fold_left
let product = foldl ( * ) 1
let splitat n list =
  let rec loop acc n = function
    | x::xs when n > 0    -> loop (x::acc) (n-1) xs
    | list (* otherwise*) -> List.rev acc, list
  in
  loop [] n list
let take n xs = splitat n xs |> fst
let drop n xs = splitat n xs |> snd
module Pair = struct let swap (a,b) = b,a end
module Option = struct let default d = function Some x -> x | None -> d end
module String = struct
  include String
  let alphabet = "abcdefghijklmnopqrstuvwxyz"
  let foldl f a str =
    let rec fold i acc = if i = String.length str then acc else fold (i+1) (f acc str.[i]) in
    fold 0 a
  let explode str = foldl (fun xs x -> x::xs) [] str |> List.rev
end
let print = print_endline
let rec whilst p f x = if p x then whilst p f (f x) else x
