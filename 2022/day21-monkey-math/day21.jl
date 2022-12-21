# Advent of code 2022, day 21: Monkey Math

abstract type Monkey end

struct NumMonkey <: Monkey
    val
end

struct MathMonkey <: Monkey
    op
    v1
    v2
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


function parseop(str)
    l = split(str)
    if length(l) == 1
        NumMonkey(parse(Int, l[1]))
    else
        MathMonkey(op(l[2]), l[1], l[3])
    end
end


function readmonkeys(filename)
    monkeys = Dict{AbstractString,Monkey}()
    for line in readlines(filename)
        name, op = split(line, ":")
        monkeys[name] = parseop(op)
    end
    monkeys
end


eval_monkey(m::NumMonkey, _) = m.val

function eval_monkey(m::MathMonkey, monkeys)
    m.op(eval_monkey(monkeys[m.v1], monkeys), eval_monkey(monkeys[m.v2], monkeys))
end

v = readmonkeys("input")
println("Part 1: ", eval_monkey(v["root"], v))
