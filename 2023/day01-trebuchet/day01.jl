function startswithtoken(s)

    if isdigit(s[1])
        return parse(Int, s[1:1])
    end

    if length(s) >= 3
        if s[1:3] == "one"
            return 1
        elseif s[1:3] == "two"
            return 2
        elseif s[1:3] == "six"
            return 6
        end
    end

    if length(s) >= 4
        if s[1:4] == "four"
            return 4
        elseif s[1:4] == "five"
            return 5
        elseif s[1:4] == "nine"
            return 9
        end
    end

    if length(s) >= 5
        if s[1:5] == "three"
            return 3
        elseif s[1:5] == "seven"
            return 7
        elseif s[1:5] == "eight"
            return 8
        end
    end

    return nothing
end


function firsttoken(line)
    let l = length(line)
        for i in 1:l
            let s = startswithtoken(line[i:end])
                if !isnothing(s)
                    return s
                end
            end
        end
    end
end


function lasttoken(line)
    let l = length(line)
        for i in l:-1:1
            let s = startswithtoken(line[i:end])
                if !isnothing(s)
                    return s
                end
            end
        end
    end
end


function line2calib2(line)
    10*firsttoken(line) + lasttoken(line)
end


function line2calib1(line)
    rx1 = r"^[a-z]*([0-9])"
    rx2 = r"([0-9])[a-z]*$"
    d1 = match(rx1, line).captures[1]
    d2 = match(rx2, line).captures[1]
    10*parse(Int, d1) + parse(Int, d2)
end


println("Part 1: ", sum(map(line2calib1, eachline("input"))))
println("Part 2: ", sum(map(line2calib2, eachline("input"))))
