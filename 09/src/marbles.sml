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

functor Range(Index: RANGEABLE) :> RANGE where type index = Index.index = struct
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
        let val step =
                fn (_, (front, x :: back')) => (x :: front, back')
                 | _ => raise Subscript
            val (front, back) = IntRange.foldl step ([], xs) (IntRange.to n)
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

signature MONO_QUEUE = sig
    type queue
    type item

    val empty: queue
    val pushBack: queue * item -> queue

    val popFront: queue -> (queue * item) option
end

signature MONO_DEQUE = sig
    include MONO_QUEUE

    val pushFront: queue * item -> queue

    val popBack: queue -> (queue * item) option
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

structure LengthyList :> LENGTHY_LIST = struct
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

signature BANKERS_DEQUE_CONFIG = sig
    type item 
    val proportion: int
end

functor BankersDeque (Config: BANKERS_DEQUE_CONFIG) :> MONO_DEQUE where type item = Config.item
= struct
    type item = Config.item
    val proportion = Config.proportion
    structure LL = LengthyList

    type queue = { front: item LL.list
                 , back: item LL.list }

    fun reSplit (xs, ys) =
        let val xsLength' = (#length xs + #length ys) div 2
            val (xs', zs) = LL.split (xsLength', xs)
        in (xs', LL.append (ys, LL.rev zs))
        end

    fun balance (deque as {front, back}) =
        if #length front > proportion * #length back + 1
        then let val (front, back) = reSplit (front, back)
             in {front, back}
             end
        else if #length back > proportion * #length front + 1
        then let val (back, front) = reSplit (back, front)
             in {front, back}
             end
        else deque 

    val empty = {front = LL.empty, back = LL.empty}
    
    fun pushFront ({front, back}, x) = balance {front = LL.pushFront (front, x), back}

    fun pushBack ({front, back}, x) = balance {front, back = LL.pushFront (back, x)}

    fun popFront {front, back} =
        case LL.popFront front
        of SOME (front', x) => SOME (balance {front = front', back}, x)
         | NONE => Option.map (fn (_, x) => (empty, x)) (LL.popFront back)

    fun popBack {front, back} =
        case LL.popFront back
        of SOME (back', x) => SOME (balance {front, back = back'}, x)
         | NONE => Option.map (fn (_, x) => (empty, x)) (LL.popFront front)
end

structure GameState :> sig
    type marble = int
    type t

    val initial: int -> t
    val turn: marble * t -> t
    val points: t -> int vector
end = struct
    type player = int
    type marble = int

    structure Circle :> sig
        type t

        val initial: t
        val turn: marble * t -> t * marble option
    end = struct
        structure Impl = BankersDeque(struct
            type item = marble
            val proportion = 1
        end)

        type t = Impl.queue

        val initial = Impl.pushBack (Impl.empty, 0)

        fun reEnqueue queue =
            case Impl.popFront queue
            of SOME (queue, x) => Impl.pushBack (queue, x)
             | NONE => queue

        fun insert circle marble =
            let val circle = reEnqueue (reEnqueue circle)
            in Impl.pushFront (circle, marble)
            end

        fun revReEnqueue queue =
            case Impl.popBack queue
            of SOME (queue, x) => Impl.pushFront (queue, x)
             | NONE => queue

        fun acquire circle =
            case Impl.popBack (IntRange.foldl (fn (_, acc) => revReEnqueue acc) circle (IntRange.to 6))
            of SOME (circle, prize) => (circle, prize)
             | NONE => raise Fail "unreachable"

        fun turn (marbleToInsert, circle) =
            if Int.rem (marbleToInsert, 23) <> 0
            then (insert circle marbleToInsert, NONE)
            else let val (circle', prizeMarble) = acquire circle
                 in (circle', SOME prizeMarble)
                 end
    end

    type t = { points: int vector
             , currentPlayer: player
             , circle: Circle.t }

    fun initial playerCount =
        { points = Vector.tabulate (playerCount, fn _ => 0)
        , currentPlayer = 0
        , circle = Circle.initial }

    fun nextPlayer playerCount currentPlayer =
        let val nextPlayer = currentPlayer + 1
        in if nextPlayer < playerCount
           then nextPlayer
           else 0
        end

    fun turn (marbleToInsert, {points, currentPlayer, circle}) =
        case Circle.turn (marbleToInsert, circle)
        of (circle', SOME prizeMarble) =>
            { points = let val currentPlayerPoints = Vector.sub (points, currentPlayer)
                           val currentPlayerPoints' = currentPlayerPoints
                                                    + marbleToInsert
                                                    + prizeMarble
                       in Vector.update (points, currentPlayer, currentPlayerPoints')
                       end
            , currentPlayer = nextPlayer (Vector.length points) currentPlayer
            , circle = circle' }
         | (circle', NONE) =>
           { points = points
           , currentPlayer = nextPlayer (Vector.length points) currentPlayer
           , circle = circle' }

    val points: t -> int vector = #points
end
