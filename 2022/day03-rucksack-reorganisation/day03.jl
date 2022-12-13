# Advent of code 2022, day 3

function load(filename)
    v = Vector{String}()
    open(filename, "r") do fh
        for line in eachline(fh)
            push!(v, line)
        end
    end
    v
end


function to_set(str)
    Set(split(str, ""))
end


function prio(a)
    findfirst(a[1], "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
end


function compartments(str)
    len = length(str)
    h = Int(len/2)
    str[1:h], str[1+h:len]
end


function dupl(a)
    b = intersect(to_set(a[1]), to_set(a[2]))
    collect(b)[1]
end


v = load("input")

s = map(compartments, v)
common = map(dupl, s)
println("Part 1: ", sum(map(prio, common)))


function badge(a)
    b = intersect(to_set(a[1]), to_set(a[2]), to_set(a[3]))
    collect(b)[1]
end

t = Iterators.partition(v, 3)
u = map(badge, t)
println("Part 2: ", sum(map(prio, u)))
