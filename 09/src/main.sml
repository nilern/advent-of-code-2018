structure Main :> sig
    val main: string list -> unit
end = struct
    fun play playerCount marbleCount =
        let val initialState = MarblesGame.initial playerCount
        in if playerCount < 1
           then initialState
           else IntRange.foldl MarblesGame.turn initialState {start = 1, stop = marbleCount + 1}
        end

    fun printErr s = TextIO.output (TextIO.stdErr, s)

    fun printArgCountError argc =
        printErr ("Error: expected 2 arguments, got " ^ Int.toString argc ^ ".\n")

    val main =
        fn [playerCountStr, marbleCountStr] =>
            (case Int.fromString playerCountStr
             of SOME playerCount =>
                 (case Int.fromString marbleCountStr
                  of SOME marbleCount =>
                      let val finalPoints = MarblesGame.points (play playerCount marbleCount)
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

val () = Main.main (CommandLine.arguments ())
