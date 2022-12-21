# Advent of code 2022, day 21: Monkey Math

abstract type Monkey end

struct NumMonkey <: Monkey
    val
end

struct MathMonkey <: Monkey
    op
    v1::Monkey
    v2::Monkey
end


function op(str)
   if str == "+"
       +
   elseif str == "-"
       -
   elseif str == "*"
       *
   elseif str == "/"
       ÷
   else
       error("Unrecognised op", str)
   end
end


function parsemonkey(name, lines)
    l = split(lines[name])
    if length(l) == 1
        NumMonkey(parse(Int, l[1]))
    else
        MathMonkey(op(l[2]), parsemonkey(l[1], lines), parsemonkey(l[3], lines))
    end
end


function readmonkeys(filename)
    lines = Dict{AbstractString,AbstractString}()
    for line in readlines(filename)
        name, op = split(line, ":")
        lines[name] = op
    end
    parsemonkey("root", lines)
end


eval_monkey(m::NumMonkey) = m.val
function eval_monkey(m::MathMonkey)
    m.op(eval_monkey(m.v1), eval_monkey(m.v2))
end

root = readmonkeys("input")
println("Part 1: ", eval_monkey(root))
println("Part 2: ", eval_monkey(root))
