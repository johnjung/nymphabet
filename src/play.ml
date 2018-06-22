(* nymphabet
 * play.ml
 * Keith Waclena <http://www.lib.uchicago.edu/keith/>
 *
 * Implementation of David Parlett's game NYMPHABET:
 *   http://www.parlettgames.uk/wgames/nymph.html
 *
 * Copyright 2017 John Jung & Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

open Prelude

let play () =
  let open Game in
  let state = State.start Player.(make Kind.Human) Player.(make Kind.Computer) in
  let turn t =
    let used = max 1 (min 4 (Random.int (len t.State.alphabet))) in
    let word = take used t.State.alphabet |> jam in
    let score = Player.score (State.whom t) in
    jam t.State.alphabet |> print;
    if score = 0
    then printf "%s says: %s (%d points)\n\n"
        (State.name t |> String.capitalize) word used
    else printf "%s says: %s (%d points * %d = %d)\n\n"
        (State.name t |> String.capitalize) word used score (used * score);
    State.next used t
  in
  let result t =
    let p1,p2 = fst t.State.players, snd t.State.players in
    let s1,s2 = Player.score p1, Player.score p2 in
    if s1 = s2
    then printf "It's a draw after %d turns, %d = %d!\n" t.State.turn s1 s2
    else if s1 > s2
    then printf "%s wins in %d turns, %d to %d!\n" (Player.name p1) t.State.turn s1 s2
    else printf "%s wins in %d turns, %d to %d!\n" (Player.name p2) t.State.turn s2 s2
  in
  whilst (not $. State.over) turn state |> result

let _ = play ()
