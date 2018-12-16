signature CIRCLE = sig
    type marble
    type t

    val initial: t
    val turn: marble * t -> t * marble option
end

signature MARBLES_GAME = sig
    type marble = int
    type t

    val initial: int -> t
    val turn: marble * t -> t
    val points: t -> int vector
end

structure MarblesGame :> MARBLES_GAME = struct
    type player = int
    type marble = int

    structure Circle :> CIRCLE where type marble = marble = struct
        structure Impl = BankersDeque(struct
            type item = marble
            val proportion = 1
        end)

        type marble = marble
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
            case Impl.popBack (IntRange.foldl (revReEnqueue o #2) circle (IntRange.to 6))
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
        { points = Vector.tabulate (playerCount, Fn.const 0)
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
