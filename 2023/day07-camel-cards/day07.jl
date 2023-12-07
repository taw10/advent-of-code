function cardval(c)
    c == 'A' && return 14
    c == 'K' && return 13
    c == 'Q' && return 12
    c == 'J' && return 11
    c == 'T' && return 10
    return parse(Int, c)
end

cardbids = map(eachline("input")) do line
    c,b = split(line)
    map(cardval, collect(c)),parse(Int, b)
end


function handq(card)
    noccur = map(card) do x
        count(y->x==y, card)
    end
    n = maximum(noccur)
    if n >= 4
        return n+2
    end
    if n == 3
        if minimum(noccur) == 2
            return 5
        else
            return 4
        end
    end
    if n == 2
        if count(x->x==2, noccur) == 4
            return 3
        else
            return 2
        end
    end
    return 1
end



function cardrank(card1, card2)
    r1 = handq(card1);  r2 = handq(card2)
    if r1 != r2
        return r1<r2
    else
        for (c1,c2) in zip(card1, card2)
            if c1 != c2
                return c1<c2
            end
        end
        return false
    end
end

order = sort(cardbids, lt=cardrank, by=x->x[1])
val = sum(map(enumerate(order)) do x
    x[1] * x[2][2]
end)
println("Part 1: ", val)
