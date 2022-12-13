# Advent of code 2022, day 6

function all_different(str)
    last = " "
    for ch in sort(split(str, ""))
        if ch == last
            return false
        end
        last = ch
    end
    true
end


function find_start(str,  mark_len)
    for i in 1:length(str)
        if all_different(str[i:i+mark_len-1])
            return i+mark_len-1
        end
    end
    return 0
end


inp = read("input", String)
println("Part 1: ", find_start(inp, 4))
println("Part 2: ", find_start(inp, 14))
