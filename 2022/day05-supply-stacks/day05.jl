# Advent of code 2022, day 5

function strings_to_array(s)
    w = maximum(map(length, s))
    h = length(s)
    arr = Array{Char}(undef, w, h)
    for (y,l) in enumerate(s)
        for (x,c) in enumerate(l)
            arr[x, y] = c
        end
    end
    arr
end


function array_to_stacks(arr)
    n_stacks = Int((size(arr, 1)+1)/4)
    height = size(arr, 2)
    s = Vector{Vector{Char}}()
    for x in 1:n_stacks
        stack = Vector{Char}()
        for pos in 1:height-1
            ch = arr[4*(x-1)+2, pos]
            if ch  != ' '
                pushfirst!(stack, ch)
            end
        end
        push!(s, stack)
    end
    s
end


function read_array(fh)
    s = Vector{String}()
    for line in eachline(fh)
        if line == ""
            return strings_to_array(s)
        end
        push!(s, line)
    end
end


function move_crates!(from, to, n, transform)
    substack = splice!(from, length(from)-n+1:length(from))
    append!(to, transform(substack))
end


function parse_move(line)
    _,n,_,from,_,to = split(line)
    parse(Int, n), parse(Int, from), parse(Int, to)
end


function load(filename, trans)
    local s
    open(filename, "r") do fh
        s = array_to_stacks(read_array(fh))
        for line in eachline(fh)
            n, from, to = parse_move(line)
            move_crates!(s[from], s[to], n, trans)
        end
    end
    return s
end


v = load("input", x->reverse(x))
println("Part 1: ", last.(v))

v = load("input", x->x)
println("Part 2: ", last.(v))
