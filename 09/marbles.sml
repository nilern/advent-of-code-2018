structure Util :> sig
    val split: 'a list * int -> ('a list * 'a list) option

    val maxi: ('a * 'a -> order) -> 'a vector -> (int option * 'a) option
end = struct
    fun split (xs, n) =
        let fun loop ls rs n =
                if n = 0
                then SOME (List.rev ls, rs)
                else case rs
                     of r :: rs' => loop (r :: ls) rs' (n - 1)
                      | [] => NONE
        in loop [] xs n
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
        val op rem = Int.rem

        type t = { left: marble list
                 , current: marble
                 , right: marble list }

        val initial = { left = []
                      , current = 0
                      , right = [] }

        fun ensureRight (circle as {left, current, right}) =
            case right
            of [] =>
                (case Util.split (left, 7)
                 of SOME (left', revRight') =>
                     { left = left'
                     , current = current
                     , right = List.rev revRight' }
                  | NONE =>
                     { left = []
                     , current = current
                     , right = List.rev left })
             | _ => circle

        fun insert (circle as {left, current, right}) marbleToInsert =
            let val {left, current, right} = ensureRight circle
            in case right
               of r :: rs =>
                   { left = r :: current :: left
                   , current = marbleToInsert
                   , right = rs }
                | [] =>
                   if marbleToInsert = 1
                   then { left = current :: left
                        , current = marbleToInsert
                        , right = right }
                   else raise Fail "unreachable"
            end

        fun acquire {left, current, right} =
            case Util.split (left, 5)
            of SOME (xs, current' :: prize :: left') =>
                ( { left = left'
                  , current = current'
                  , right = List.revAppend (xs, right) }
                , prize )
             | _ => raise Fail "unreachable"

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
