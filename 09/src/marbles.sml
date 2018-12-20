(* Marble circle interface. *)
signature CIRCLE = sig
    type marble = int
    type t

    val initial: t
    (* Given a marble, either insert it into the circle or shuffle the circle and return the
       prize marble. *)
    val turn: marble * t -> t * marble option
end

signature MARBLES_GAME = sig
    type marble = int
    type t

    val initial: int -> t
    (* Given a marble and a game state, compute the next game state. *)
    val turn: marble * t -> t
    val points: t -> int vector
end

(* The CIRCLE module parameterized over the DEQUE implementation. *)
functor Circle (Impl: DEQUE) :> CIRCLE = struct
    type marble = int
    type t = marble Impl.queue

    val initial = Impl.pushBack (Impl.empty, 0)

    (* Move an item fron the front of the queue to the back. *)
    fun reEnqueue queue =
        case Impl.popFront queue
        of SOME (queue, x) => Impl.pushBack (queue, x)
         | NONE => queue

    (* Move an item front tha back of the queue to the front. *)
    fun revReEnqueue queue =
        case Impl.popBack queue
        of SOME (queue, x) => Impl.pushFront (queue, x)
         | NONE => queue

    (* The default turn. *)
    fun insert circle marble =
        let val circle = reEnqueue (reEnqueue circle)
        in Impl.pushFront (circle, marble)
        end

    (* The points-providing turn. *)
    fun acquire circle =
        case Impl.popBack (IntRange.foldl (revReEnqueue o #2) circle (IntRange.to 6))
        of SOME (circle, prize) => (circle, prize)
         | NONE => raise Fail "unreachable"

    datatype marble_action = INSERT | ACQUIRE

    fun marbleAction marbleToInsert =
        if Int.rem (marbleToInsert, 23) = 0 then ACQUIRE else INSERT

    fun turn (marbleToInsert, circle) =
        case marbleAction marbleToInsert
        of INSERT => (insert circle marbleToInsert, NONE)
         | ACQUIRE => Pair.second SOME (acquire circle)
end

(* The MARBLES_GAME module paramterized over the CIRCLE implementation. *)
functor MarblesGame (Circle: CIRCLE) :> MARBLES_GAME = struct
    type player = int
    type marble = int

    type t = { points: int vector
             , currentPlayer: player
             , circle: Circle.t }

    fun initial playerCount =
        { points = Vector.tabulate (playerCount, Fn.const 0)
        , currentPlayer = 0
        , circle = Circle.initial }

    (* The index of the next player. *)
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
