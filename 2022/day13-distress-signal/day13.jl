# Advent of code 2022, day 13: Distress Signal

smaller(a::Int, b::Int) = cmp(b,a)
smaller(a::Int, b::Vector) = smaller([a], b)
smaller(a::Vector, b::Int) = smaller(a, [b])

function smaller(a::Vector, b::Vector)
    for (p,q) in zip(a,b)
        s = smaller(p, q)
        if s != 0
            return s
        end
    end
    cmp(length(b), length(a))
end


let total = 0
    for (idx,line) in enumerate(Iterators.partition(readlines("input"), 3))
        a = eval(Meta.parse(line[1]))
        b = eval(Meta.parse(line[2]))
        if smaller(a, b) == 1
            total += idx
        end
    end
    println("Part 1: ", total)
end


function packetlt(a, b)
    if smaller(a, b) == 1
        true
    else
        false
    end
end

let inp = [eval(Meta.parse(x)) for x in readlines("input") if x != ""]
    push!(inp, [[2]])
    push!(inp, [[6]])
    srt = sort(inp, lt=packetlt)
    println("Part 2: ", findfirst(isequal([[2]]), srt) * findfirst(isequal([[6]]), srt))
end
