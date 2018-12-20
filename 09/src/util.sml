(* The SML standard library lacks many of the conveniences found in e.g. Haskell or Clojure,
   but it is simple to define our own. *)

structure Fn :> sig
    (* Given a value of type 'a, return a function that always returns that value. *)
    val const: 'a -> 'b -> 'a
end = struct
    fun const c _ = c
end

(* Like the Data.Bifunctor of Haskell, not to be confused with ML module functors. *)
signature BIFUNCTOR = sig
    type ('a, 'b) bifunctor

    (* Map over the first component. *)
    val first: ('a -> 'c) -> ('a, 'b) bifunctor -> ('c, 'b) bifunctor
    (* Map over the second component. *)
    val second: ('b -> 'c) -> ('a, 'b) bifunctor -> ('a, 'c) bifunctor
end

(* Utilities for pairs (two-element tuples). *)
structure Pair :> BIFUNCTOR where type ('a, 'b) bifunctor = 'a * 'b = struct
    type ('a, 'b) bifunctor = 'a * 'b

    fun first f (l, r) = (f l, r)
    fun second f (l, r) = (l, f r)
end

(* A type whose values can be RANGEd over. *)
signature RANGEABLE = sig
    type index

    val compare: index * index -> order

    val zero: index
    val increment: index -> index
end

signature RANGE = sig
    type index
    (* A range from start inclusice to stop exclusive. *)
    type range = {start: index, stop: index}

    (* A range from the index zero upto the given index. *)
    val to: index -> range

    val foldl: (index * 'a -> 'a) -> 'a -> range -> 'a
end

(* Generic RANGE module parameterized over a RANGEABLE module. *)
functor Range (Index: RANGEABLE) :> RANGE where type index = Index.index = struct
    type index = Index.index
    type range = { start: index
                 , stop: index }

    fun to stop = {start = Index.zero, stop}

    fun foldl f v {start, stop} =
        let fun loop i v =
                case Index.compare (i, stop)
                of LESS => loop (Index.increment i) (f (i, v))
                 | _ => v
        in loop start v
        end
end

(* A RANGE over int. *)
structure IntRange = Range(struct
    type index = Int.int

    val compare = Int.compare

    val zero = 0
    val increment = fn n => n + 1
end)

structure Util :> sig
    (* Split a list at the given index. Essentially `(take n xs, drop n xs)` (but faster). *)
    val split: int * 'a list -> 'a list * 'a list

    (* Find the maximum value and its index (if unambiguous) of a vector (if nonempty). *)
    val maxi: ('a * 'a -> order) -> 'a vector -> (int option * 'a) option
end = struct
    fun split (n, xs) =
        let val move =
                fn (front, x :: back') => (x :: front, back')
                 | _ => raise Subscript
            val (front, back) = IntRange.foldl (move o #2) ([], xs) (IntRange.to n)
        in (List.rev front, back)
        end

    fun maxi compare coll =
        let val rec step =
                fn (i, v, acc as SOME (_, oldMax)) =>
                    (case compare (v, oldMax)
                     of LESS => acc
                      | EQUAL => SOME (NONE, oldMax)
                      | GREATER => SOME (SOME i, v))
                | (i, v, NONE) => SOME (SOME i, v)
        in Vector.foldli step NONE coll
        end
end
