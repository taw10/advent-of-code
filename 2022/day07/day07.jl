# Advent of code 2022, day 7

struct inode
    name
    size::Union{Int,Nothing}
    children::Vector{inode}
end


function load(filename)
    open(filename, "r") do fh
        for line in eachline(fh)
            s = split(line)
            if s[1] == "\$"
                println("Command: ",line)
            end
        end
    end
    v
end


v = load("input")
println("Part 1: ", 0)
