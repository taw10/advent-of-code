# Advent of code 2022, day 10

function make_xvals(s)
    r = Int[]
    x = 1
    for line in s
        cmd = split(line)
        if cmd[1] == "noop"
            push!(r, x)
        else
            push!(r, x)
            push!(r, x)
            x += parse(Int, cmd[2])
        end
    end
    r
end


xvals = make_xvals(readlines("input"))
println("Part 1: ", sum([i*xvals[i] for i in 20:40:220]))

println("Part 2:")
for y in 1:6
    for x in 1:40
        if abs((x-1)-xvals[x+(y-1)*40])<=1
            print("#")
        else
            print(".")
        end
    end
    println("")
end
