structure Util :> sig
    val split: int * 'a list -> 'a list * 'a list

    val maxi: ('a * 'a -> order) -> 'a vector -> (int option * 'a) option
end = struct
    fun split (n, xs) =
        let fun doSplit (n, front, back) =
                if n > 0
                then case back
                     of x :: back' => doSplit (n - 1, x :: front, back')
                      | [] => raise Subscript
                else (List.rev front, back)
        in doSplit (n, [], xs)
        end

    fun maxi compare coll =
        let val rec step =
                fn (i, v, acc as SOME (oldMaxIndex, oldMax)) =>
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

structure Marbles :> sig
    val main: string list -> unit
end = struct
    type player = int
    type marble = int

    structure Circle :> sig
        type t

        val initial: t
        val turn: t -> marble -> t * marble option
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
            let fun loop circle n =
                    if n > 0
                    then loop (revReEnqueue circle) (n - 1)
                    else circle
            in case Impl.popBack (loop circle 6)
               of SOME (circle, prize) => (circle, prize)
                | NONE => raise Fail "unreachable"
            end

        fun turn circle marbleToInsert =
            if Int.rem (marbleToInsert, 23) <> 0
            then (insert circle marbleToInsert, NONE)
            else let val (circle', prizeMarble) = acquire circle
                 in (circle', SOME prizeMarble)
                 end
    end

    structure GameState :> sig
        type t = { points: int vector
                 , currentPlayer: player
                 , marbleToInsert: marble
                 , circle: Circle.t }

        val initial: int -> int -> t
        val turn: t -> t
    end = struct
        type t = { points: int vector
                 , currentPlayer: player
                 , marbleToInsert: marble
                 , circle: Circle.t }

        fun initial playerCount marbleCount =
            { points = Vector.tabulate (playerCount, fn _ => 0)
            , currentPlayer = 0
            , marbleToInsert = 1
            , circle = Circle.initial }

        fun nextPlayer playerCount currentPlayer =
            let val nextPlayer = currentPlayer + 1
            in if nextPlayer < playerCount
               then nextPlayer
               else 0
            end

        fun turn {points, currentPlayer, marbleToInsert, circle} =
            case Circle.turn circle marbleToInsert
            of (circle', SOME prizeMarble) =>
                { points = let val currentPlayerPoints = Vector.sub (points, currentPlayer)
                               val currentPlayerPoints' = currentPlayerPoints
                                                        + marbleToInsert
                                                        + prizeMarble
                           in Vector.update (points, currentPlayer, currentPlayerPoints')
                           end
                , currentPlayer = nextPlayer (Vector.length points) currentPlayer
                , marbleToInsert = marbleToInsert + 1
                , circle = circle' }
             | (circle', NONE) =>
               { points = points
               , currentPlayer = nextPlayer (Vector.length points) currentPlayer
               , marbleToInsert = marbleToInsert + 1
               , circle = circle' }
    end

    fun play playerCount marbleCount =
        let val initialState = GameState.initial playerCount marbleCount
            fun gameLoop state =
                if #marbleToInsert state > marbleCount
                then state
                else gameLoop (GameState.turn state)
        in if playerCount < 1
           then initialState
           else gameLoop (initialState)
        end

    fun printErr s = TextIO.output (TextIO.stdErr, s)

    fun printArgCountError argc =
        printErr ("Error: expected 2 arguments, got " ^ Int.toString argc ^ ".\n")

    val main =
        fn [] => printArgCountError 0
         | [_] => printArgCountError 1
         | [playerCountStr, marbleCountStr] =>
            (case Int.fromString playerCountStr
             of SOME playerCount =>
                 (case Int.fromString marbleCountStr
                  of SOME marbleCount =>
                      let val finalPoints = #points (play playerCount marbleCount)
                          fun pointsLine (player, points) =
                              "Player " ^ Int.toString player
                                ^ " has " ^ Int.toString points ^ " points.\n"
                      in Vector.app print (Vector.mapi pointsLine finalPoints)
                       ; print "\n"
                       ; case Util.maxi Int.compare finalPoints
                         of SOME (SOME winner, winnerPoints) =>
                             print ("The winner is player " ^ Int.toString (winner + 1)
                                      ^ " with " ^ Int.toString winnerPoints ^ " points.\n")
                          | SOME (NONE, maxPoints) =>
                             print ("There no clear winner, max points was "
                                      ^ Int.toString maxPoints ^ ".\n")
                          | NONE => print "There were no players.\n"
                      end
                   | NONE => printErr ("Non-integral marble count: " ^ marbleCountStr ^ ".\n"))
              | NONE => printErr ("Non-integral player count: " ^ playerCountStr ^ ".\n"))
         | args => printArgCountError (List.length args)
end

val () = Marbles.main (CommandLine.arguments ())
