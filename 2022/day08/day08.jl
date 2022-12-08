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


function visible1d(v, xc)
    lower = [h < v[xc] for h in v]
    all(lower[begin:xc-1]) || all(lower[xc+1:end])
end


function visible2d(v, xc, yc)
    visible1d(v[xc,:], yc) || visible1d(v[:,yc], xc)
end


function viewdist(v, h)
    length(v) <  1 && return 0
    l = findfirst(>=(h), v)
    l === nothing && return length(v)
    return l
end


function viewdist1d(v, xc)
    left = viewdist(v[xc-1:-1:begin], v[xc])
    right = viewdist(v[xc+1:end], v[xc])
    left * right
end


function viewdist2d(v, xc, yc)
    viewdist1d(v[xc,:], yc) * viewdist1d(v[:,yc], xc)
end


v = strings_to_array(readlines("input"))

nvis = count(visible2d(v, x, y) for x in axes(v,1), y in axes(v,2))
println("Part 1: ", nvis)

max = maximum(viewdist2d(v, x, y) for x in axes(v,1), y in axes(v,2))
println("Part 2: ", max)
