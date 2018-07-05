(* nymphabet
 * game.ml
 * Keith Waclena <http://www.lib.uchicago.edu/keith/>
 *
 * Implementation of David Parlett's game NYMPHABET:
 *   http://www.parlettgames.uk/wgames/nymph.html
 *
 * Copyright 2017 John Jung & Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

open Prelude

let jam = String.concat ""

module Kind = struct
  type t = Human | Computer
  let to_string = function Human -> "human" | Computer -> "computer"
end

module Player = struct
  type t = { kind : Kind.t; name : string option; score : int list }
  let make ?name kind = { kind; name; score = [] }
  let update t s = { t with score = s :: t.score }
  let name t = match t with { name=None } -> Kind.to_string t.kind | { name=Some n } -> n
  let score t = match t.score with [] -> 0 | _ -> product t.score
  let to_string t = match t.name with
    | None   -> sprintf {|{ %s %d }|}      (Kind.to_string t.kind)   (score t)
    | Some n -> sprintf {|{ %s "%s" %d }|} (Kind.to_string t.kind) n (score t)
end

module Alphabet = struct
  type t = string list
  let to_string = jam
  let rotate n t = splitat n t |> Pair.swap |> uncurry append
  let from_string str = String.explode str |> map (String.make 1)
  let default () = let az = from_string String.alphabet in rotate (Random.int (len az)) az
  let use n t = drop n t
end

module State = struct
  type t = {
    players : Player.t * Player.t;
    turn : int;
    alphabet : Alphabet.t;
  }
  let to_string t =
    sprintf "{ (%s, %s) %d %s }"
      (fst t.players |> Player.name) (snd t.players |> Player.name)
      t.turn (jam t.alphabet)
  let start ?alphabet p1 p2 =
    let alphabet = Option.default (Alphabet.default ()) alphabet in
    { players = p1, p2; turn = 0; alphabet; }
  let over t = t.alphabet = []
  let next used t =
    let players =
      if t.turn mod 2 = 0
      then Player.update (fst t.players) used, (snd t.players)
      else (fst t.players), Player.update (snd t.players) used
    in
    if over t then failwith "game over" else
    { turn = t.turn + 1; players; alphabet = Alphabet.use used t.alphabet }
  let whom t = if t.turn mod 2 = 0 then fst t.players else snd t.players
  let name t = Player.name (whom t)
end

let norm str = Js.String.(normalize str |> toLowerCase)

let score alphabet word =
  let open Result in
  let rec loop scores n = function
  |    [], []               -> assert false
  |    [], _                -> Ok (   [],     scores)
  | a::bz, []               -> Ok (a::bz, (n::scores))
  | a::bz, l::ls when l = a -> loop scores (succ n) (   bz, ls)
  | a::bz, l::ls (* else *) -> loop scores       n  (a::bz, ls)
  in
  match alphabet, word with
  | a :: bz, l :: ls when l = a -> loop [] 0 (alphabet, word) >>| (id &&& product)
  |      [], _                  -> Error "empty alphabet"
  |       _, _                  -> Error "word doesn't start with alphabet"

module Dict = struct
  exception Empty
  module M = Map.Make (String)
  type t = string list M.t
  let empty = M.empty
  let mem w t =
    if w = ""
    then false
    else match M.find (Js.String.slice ~from:0 ~to_:1 w) t  with
    | None    -> false
    | Some ws -> List.mem w ws
  let cardinal t = M.fold (fun _ ws n -> len ws + n) t 0
  let add t w = match Js.String.(normalize w |> slice ~from:0 ~to_:1) with
  | "" -> t
  | s  -> M.adjoin cons [] s (Js.String.toLowerCase w) t

  let tojson t =
    let each k v acc =
      let vs = List.map (fun v -> Js.Json.string v) v in
      (k, vs)::acc
    in
    M.fold each t [] |> Js_dict.fromList

  let random t =
    match M.keys t |> choose 1 with
    | []   -> None
    | c::_ -> match flip M.find t c with
    | []   -> None
    | ws   -> choose 1 ws |> List.hd |> some

  let search az c t =
    let eachword best w = match best with
    | None   -> Some w
    | Some b ->
        if score az w > score az b
        then Some w
        else if score az w = score az b
        then choose ~n:2 1 [w;b] |> List.hd |> some
        else best
    in
    match M.find c t with
    | exception Not_found -> None
    | ws                  -> foldl eachword None ws

end
