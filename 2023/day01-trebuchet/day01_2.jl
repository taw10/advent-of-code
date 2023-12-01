function line2calib(line)
    rx1 = r"^[a-z]*([0-9])"
    rx2 = r"([0-9])[a-z]*$"
    d1 = match(rx1, line).captures[1]
    d2 = match(rx2, line).captures[1]
    10*parse(Int, d1) + parse(Int, d2)
end


function todigit(s)
    if s == "one"
        1
    elseif s == "two"
        2
    elseif s == "three"
        3
    elseif s == "four"
        4
    elseif s == "five"
        5
    elseif s == "six"
        6
    elseif s == "seven"
        7
    elseif s == "eight"
        8
    elseif s == "nine"
        9
    else
        parse(Int, s)
    end
end


function line2calib2(line)
    rx1 = r"^[a-z]*?(\d|one|two|three|four|five|six|seven|eight|nine)"
    rx2 = r".*(one|two|three|four|five|six|seven|eight|nine|\d)"
    d1 = match(rx1, line).captures[1]
    d2 = match(rx2, line).captures[1]
    10*todigit(d1) + todigit(d2)
end


println("Part 1: ", sum(map(line2calib, eachline("input"))))
println("Part 2: ", sum(map(line2calib2, eachline("input"))))
