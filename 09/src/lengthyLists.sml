(* A list whose length can be computed in O(1). *)
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

(* A LENGTHY_LIST that wraps a standard cons list. *)
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
