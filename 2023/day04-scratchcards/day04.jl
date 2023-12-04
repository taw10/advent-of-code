function readcards(filename)
    Channel() do ch
        for line in eachline(filename)
            r = split(line, "|")
            l = split(r[1])[3:end]
            m = split(r[2])
            put!(ch, (map(x->parse(Int32, x), l),
                      map(x->parse(Int32, x), m)))
        end
    end
end


function nwin(winners, card)
    score = 0
    for winner in winners
        if winner ∈ card
            score += 1
        end
    end
    score
end


function score(winners, card)
    let s = nwin(winners, card)
        if s > 0
            2^(s-1)
        else
            0
        end
    end
end


let allcards = collect(readcards("input"))

    println("Part 1: ", sum(map(x->score(x[1],x[2]), allcards)))

    let pending = [1 for _ in allcards], ncards = 0
        for card in allcards
            ncopies = popfirst!(pending)
            ncards += ncopies
            nmatch = nwin(card[1], card[2])
            for i in 1:nmatch
                pending[i] += ncopies
            end
        end
        println("Part 2: ", ncards)
    end

end
