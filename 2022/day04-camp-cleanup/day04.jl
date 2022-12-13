# Advent of code 2022, day 4

struct SecRange
    first
    last
end


function parse_range(a)
    b = split(a, "-")
    SecRange(parse(Int, b[1]), parse(Int, b[2]))
end


function load(filename)
    v = Vector{Tuple{SecRange,SecRange}}()
    open(filename, "r") do fh
        for line in eachline(fh)
            s = split(line, ",")
            push!(v, tuple(parse_range(s[1]), parse_range(s[2])))
        end
    end
    v
end


function countbothways(f, v)
    n = 0
    for x in v
        if f(x[1], x[2]) || f(x[2], x[1])
            n += 1
        end
    end
    n
end


function contains(a, b)
    (a.first >= b.first) && (a.last <= b.last)
end


v = load("input")
println("Part 1: ", countbothways(contains, v))


function overlap(a, b)
    ((a.first >= b.first) && (a.first <= b.last)
     || (a.last >= b.first) && (a.last <= b.last))
end

println("Part 2: ", countbothways(overlap, v))
