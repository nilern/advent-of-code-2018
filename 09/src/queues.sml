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
         | NONE => Option.map (Pair.map (Fn.const empty)) (LL.popFront back)

    fun popBack {front, back} =
        case LL.popFront back
        of SOME (back', x) => SOME (balance {front, back = back'}, x)
         | NONE => Option.map (Pair.map (Fn.const empty)) (LL.popFront front)
end
