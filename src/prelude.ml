(* nymphabet
 * prelude.ml
 * Keith Waclena <http://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2017 John Jung & Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

(* A miniaturized version of KW's Prelude; for doc, see:
   https://www2.lib.uchicago.edu/keith/software/prelude/Prelude.html *)
let id x = x
let flip f x y = f y x
let default d f x = try f x with _ -> d
let ($.) f g x = f (g x)
include List
let cons x xs = x::xs
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
module Pair = struct let swap (a,b) = b,a let cross f g (a,b) = f a, g b  end
let ( &&& ) = Pair.cross
module Option = struct let some x = Some x let default d = function Some x -> x | None -> d end
let some = Option.some
let withbuf2 size f = Buffer.(let buf = create size in let r = f buf in contents buf, r)
let withbuf size f = withbuf2 size f |> fst
module String = struct
  include String
  let alphabet = "abcdefghijklmnopqrstuvwxyz"
  let foldl f a str =
    let rec fold i acc = if i = String.length str then acc else fold (i+1) (f acc str.[i]) in
    fold 0 a
  let maps f str =
    withbuf (String.length str) (fun buf -> String.iter (fun c -> Buffer.add_string buf (f c)) str)
  let explode str = foldl (fun xs x -> x::xs) [] str |> List.rev
end
let print = print_endline
let rec whilst p f x = if p x then whilst p f (f x) else x
type ('a,'b) result = Ok of 'a | Error of 'b
module Result = struct
  let bind r f = match r with Ok x -> f x | Error _ as err -> err
  let (>>=) = bind              (* unit test: see Op.(>>=) *)
  let map r f = match r with Ok x -> Ok (f x) | Error _ as err -> err
  let (>>|) = map              (* unit test: see Op.(>>=) *)
end
module Map = struct        (*$< Map *)
  module Make (Ord : Map.OrderedType) = struct
    module M = Map.Make(Ord)
    include M
    let adjoin vadd empty k v m = default empty (find k) m |> vadd v |> flip (add k) m
    let keys m = fold (fun k _ acc -> k::acc) m [] |> rev
  end
end
module Random = struct
  include Random
  let choose ?state ?n m lst =
    let state = match state with
      | Some s -> s
      | None   -> Random.State.make_self_init () in
    let rec rc acc m n = function
      |   []   -> acc
      | h :: t ->
        if Random.State.int state n < m
        then rc (h :: acc) (m - 1) (n - 1) t
        else rc acc m (n - 1) t
    in
    let n = match n with Some n -> n | None -> List.length lst in
    rc [] m n lst
end
let choose = Random.choose

