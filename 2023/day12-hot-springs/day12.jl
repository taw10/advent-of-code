function parsespring(line)
    return split(line)[1], map(x->parse(Int,x),
                               split(split(line)[2], ","))
end

function issolution(str, lengths)
    springs = split(strip(str, ['.']), '.', keepempty=false)
    length(springs) == length(lengths) && all(map((spring,sprlen)->length(spring)==sprlen,
                                                  springs, lengths))
end

function solvesprings(springs, lengths)
    ndamage = count('?', springs)
    solutions = Vector{AbstractString}()
    for i in 0:2^ndamage-1
        bit = 0
        cand = map(springs) do ch
            if ch == '?'
                out = (i & 1<<bit != 0) ? '#' : '.'
                bit += 1
                out
            else
                ch
            end
        end
        if issolution(String(cand), lengths)
            push!(solutions, String(cand))
        end
    end
    solutions
end

input = map(parsespring, eachline("input"))
println("Part 1: ", sum(map(length, map(x->solvesprings(x[1],x[2]), input))))
