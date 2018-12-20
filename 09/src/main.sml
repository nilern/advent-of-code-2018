structure Main :> sig
    (* Entry point. There's nothing special about Main.main but I like C-style entry points. *)
    val main: string list -> unit
end = struct
    (* This elegant and somehow amusing module expression injects dependencies and puts together
       our fancy layered architecture. And MLton guarantees this to have zero runtime overhead! *)
    structure Game = MarblesGame(Circle(struct
        structure Impl = BankersDeque(struct
            (* We don't need persistence, so a simple cons list is best: *)
            structure LengthyList = LengthyConsList
            (* One would be a good default, but bigger seems to be faster. Finding the optimum
               would probably take some analysis that isn't very relevant for the task at hand. *)
            val proportion = 2
        end)

        structure Consts = struct
            val valuableFactor = 23
            val insertionIndex = 2
            val prizeIndex = 7
        end
    end))

    (* Run through the game non-interactively. *)
    fun play playerCount marbleCount =
        let val initialState = Game.initial playerCount
        in if playerCount < 1
           then initialState
           else IntRange.foldl Game.turn initialState {start = 1, stop = marbleCount + 1}
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
                      let val finalPoints = Game.points (play playerCount marbleCount)
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

(* Since there is nothing special about the entry point, it needs to be called manually, kind of
   like in a scripting language (which ML orginally was; the MetaLanguage of the Edinburgh LCF
   theorem prover). *)
val () = Main.main (CommandLine.arguments ())
