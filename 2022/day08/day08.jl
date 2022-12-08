# Advent of code 2022, day 8

function strings_to_array(s)
    w = maximum(map(length, s))
    h = length(s)
    arr = Array{Int}(undef, w, h)
    for (y,l) in enumerate(s)
        for (x,c) in enumerate(l)
            arr[x, y] = parse(Int, c)
        end
    end
    arr
end


function read_array(fh)
    s = Vector{String}()
    for line in eachline(fh)
        push!(s, line)
    end
    return strings_to_array(s)
end


function load(filename)
    open(filename, "r") do fh
        s = read_array(fh)
        return s
    end
end


function visible1d(v, xc)
    lower = [h < v[xc] for h in v]
    all(lower[1:xc-1]) || all(lower[xc+1:length(v)])
end


function visible2d(v, xc, yc)
    visible1d(v[xc,:], yc) || visible1d(v[:,yc], xc)
end


v = load("input")
let nvis = 0
    for x in 1:size(v, 1), y in 1:size(v, 2)
        if visible2d(v, x, y)
            nvis += 1
        end
    end
    println("Part 1: ", nvis)
end


function viewdist(v, h)
    length(v) <  1 && return 0
    l = findfirst(x->x>=h, v)
    l === nothing && return length(v)
    return l
end


function viewdist1d(v, xc)
    left = viewdist([v[x] for x in xc-1:-1:1], v[xc])
    right = viewdist([v[x] for x in xc+1:length(v)], v[xc])
    left * right
end


function viewdist2d(v, xc, yc)
    viewdist1d(v[xc,:], yc) * viewdist1d(v[:,yc], xc)
end


let max = 0
    for x in 1:size(v, 1), y in 1:size(v, 2)
        d = viewdist2d(v, x, y)
        if d > max
            max = d
        end
    end
    println("Part 2: ", max)
end
