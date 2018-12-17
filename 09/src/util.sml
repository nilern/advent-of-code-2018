structure Fn :> sig
    val const: 'a -> 'b -> 'a
end = struct
    fun const c _ = c
end

signature BIFUNCTOR = sig
    type ('a, 'b) t

    val first: ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
    val second: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end

structure Pair :> BIFUNCTOR where type ('a, 'b) t = 'a * 'b = struct
    type ('a, 'b) t = 'a * 'b

    fun first f (l, r) = (f l, r)
    fun second f (l, r) = (l, f r)
end

signature RANGEABLE = sig
    type index

    val compare: index * index -> order

    val zero: index
    val inc: index -> index
end

signature RANGE = sig
    type index
    type range = {start: index, stop: index}

    val to: index -> range

    val foldl: (index * 'a -> 'a) -> 'a -> range -> 'a
end

functor Range (Index: RANGEABLE) :> RANGE where type index = Index.index = struct
    type index = Index.index
    type range = { start: index
                 , stop: index }

    fun to stop = {start = Index.zero, stop}

    fun foldl f v {start, stop} =
        let fun loop i v =
                case Index.compare (i, stop)
                of LESS => loop (Index.inc i) (f (i, v))
                 | _ => v
        in loop start v
        end
end

structure IntRange = Range(struct
    type index = Int.int

    val compare = Int.compare

    val zero = 0
    val inc = fn n => n + 1
end)

structure Util :> sig
    val split: int * 'a list -> 'a list * 'a list

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

signature LENGTHY_LIST = sig
    type 'a list = { list: 'a List.list
                   , length: int }

    val empty: 'a list
    val pushFront: 'a list * 'a -> 'a list
    val popFront: 'a list -> ('a list * 'a) option

    val append: 'a list * 'a list -> 'a list
    val split: int * 'a list -> 'a list * 'a list
    val rev: 'a list -> 'a list
end

structure LengthyConsList :> LENGTHY_LIST = struct
    type 'a list = { list: 'a List.list
                   , length: int }

    val empty = {list = [], length = 0}

    fun pushFront ({list, length}, x) = {list = x :: list, length = length + 1}

    val popFront =
        fn {list = [], ...} => NONE
         | {list = x :: xs, length} => SOME ({list = xs, length = length - 1}, x)

    fun append (xs: 'a list, ys: 'a list) =
        { list = #list xs @ #list ys
        , length = #length xs + #length ys }

    fun split (n, {list, length}) =
        let val (front, back) = Util.split (n, list)
        in ( {list = front, length = n}
           , {list = back, length = length - n} )
        end

    fun rev {list, length} = {list = List.rev list, length}
end
