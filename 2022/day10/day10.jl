# Advent of code 2022, day 10

function parse_cmd(s)
    s = split(s)
    if s[1] == "noop"
        1,0
    else
        2,parse(Int, s[2])
    end
end


input = map(parse_cmd, readlines("input"))

seek = [i for i in 20:40:220]
let cycle = 1, signal = 1, total = 0
    for cmd in input
        println(cycle, " : ", signal)
        if length(seek) > 0 && cycle+cmd[1] > seek[1]
            println(seek[1], " * ", signal, " = ", seek[1]*signal)
            total += seek[1]*signal
            popfirst!(seek)
        end
        cycle += cmd[1]
        signal += cmd[2]
    end
    println("Part 1: ", total)
end
