(* The interface of unidirectional queues. *)
signature QUEUE = sig
    type 'a queue

    val empty: 'a queue
    val pushBack: 'a queue * 'a -> 'a queue
    val popFront: 'a queue -> ('a queue * 'a) option
end

(* The interface of bidirectional queues (also enables stack-like usage). *)
signature DEQUE = sig
    include QUEUE

    val pushFront: 'a queue * 'a -> 'a queue
    val popBack: 'a queue -> ('a queue * 'a) option
end

(* Parameters for BankersDeque. *)
signature BANKERS_DEQUE_CONFIG = sig
    (* The list module to use. Cons lists give good performance in 'single-timeline' use cases
       but risk quadratic behaviour if persistence is required. *)
    structure LengthyList: LENGTHY_LIST
    (* Limit for the ratio of front and back queue lengths. *)
    val proportion: int
end

(* Banker's deque adapted from Okasaki's Purely Functional Data Structures. The implementation
   here is also parameterized over the list implementation and more DRY. *)
functor BankersDeque (Config: BANKERS_DEQUE_CONFIG) :> DEQUE = struct
    structure LL = Config.LengthyList
    val proportion = Config.proportion

    type 'a queue = { front: 'a LL.list
                    , back: 'a LL.list }

    (* Juggle items between the fronts of xs and ys to make their lengths as close as possible. *)
    fun reSplit (xs, ys) =
        let val xsLength' = (#length xs + #length ys) div 2
            val (xs', zs) = LL.split (xsLength', xs)
        in (xs', LL.append (ys, LL.rev zs))
        end

    (* Use reSplit to rebalance front and back if their lengths diverge too much. *)
    fun balance (deque as {front, back}) =
        (* The + 1 allows storing the only item of singleton deques in either list. *)
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

    (* The following operations are symmetric and could be DRYed further but I'm not sure
       whether that would add any real value. *)
    
    fun pushFront ({front, back}, x) = balance {front = LL.pushFront (front, x), back}

    fun pushBack ({front, back}, x) = balance {front, back = LL.pushFront (back, x)}

    fun popFront {front, back} =
        case LL.popFront front
        of SOME (front', x) => SOME (balance {front = front', back}, x)
         | NONE => Option.map (Pair.first (Fn.const empty)) (LL.popFront back)

    fun popBack {front, back} =
        case LL.popFront back
        of SOME (back', x) => SOME (balance {front, back = back'}, x)
         | NONE => Option.map (Pair.first (Fn.const empty)) (LL.popFront front)
end
