# Advent of code 2022, day 5

struct Stack
    a::Vector{Char}
end

Stack() = Stack(Vector{Char}())
Base.push!(s::Stack, c::Char) = push!(s.a, c)
Base.pushfirst!(s::Stack, c::Char) = pushfirst!(s.a, c)
Base.pop!(s::Stack) = pop!(s.a)
Base.popfirst!(s::Stack) = popfirst!(s.a)
Base.getindex(s::Stack, i) = getindex(s.a, i)


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
    s = Vector{Stack}()
    for x in 1:n_stacks
        stack = Stack()
        for pos in 1:height-1
            ch = arr[4*(x-1)+2, pos]
            if ch  != ' '
                push!(stack, ch)
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


function read_stacks(fh)
    array_to_stacks(read_array(fh))
end


function move_n_times!(from::Stack, to::Stack, n)
    for _ in 1:n
        pushfirst!(to, popfirst!(from))
    end
end


function move_n_crates!(from::Stack, to::Stack, n)
    substack = Vector{Char}()
    for _ in 1:n
        pushfirst!(substack, popfirst!(from))
    end
    for _ in 1:n
        pushfirst!(to, popfirst!(substack))
    end
end


function load(filename, move!)
    local s
    open(filename, "r") do fh
        s = read_stacks(fh)
        for line in eachline(fh)
            sp = split(line, " ")
            n_to_move = parse(Int, sp[2])
            from = parse(Int, sp[4])
            to = parse(Int, sp[6])
            move!(s[from], s[to], n_to_move)
        end
    end
    return s
end


v = load("input", move_n_times!)
println("Part 1: ", map(x->x[1], v))

v = load("input", move_n_crates!)
println("Part 2: ", map(x->x[1], v))
