signature QUEUE = sig
    type 'a queue

    val empty: 'a queue
    val pushBack: 'a queue * 'a -> 'a queue
    val popFront: 'a queue -> ('a queue * 'a) option
end

signature DEQUE = sig
    include QUEUE

    val pushFront: 'a queue * 'a -> 'a queue
    val popBack: 'a queue -> ('a queue * 'a) option
end

signature BANKERS_DEQUE_CONFIG = sig
    structure LengthyList: LENGTHY_LIST
    val proportion: int
end

functor BankersDeque (Config: BANKERS_DEQUE_CONFIG) :> DEQUE = struct
    structure LL = Config.LengthyList
    val proportion = Config.proportion

    type 'a queue = { front: 'a LL.list
                    , back: 'a LL.list }

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
         | NONE => Option.map (Pair.first (Fn.const empty)) (LL.popFront back)

    fun popBack {front, back} =
        case LL.popFront back
        of SOME (back', x) => SOME (balance {front, back = back'}, x)
         | NONE => Option.map (Pair.first (Fn.const empty)) (LL.popFront front)
end
