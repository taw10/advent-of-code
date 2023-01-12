# Advent of code 2022, day 21: Monkey Math

abstract type Monkey end

struct HumanMonkey <: Monkey
    val
end

struct NumMonkey <: Monkey
    val
end

struct MathMonkey <: Monkey
    op
    v1::Monkey
    v2::Monkey
end

struct RootMonkey <: Monkey
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
    if name == "humn"
        HumanMonkey(parse(Int, l[1]))
    elseif name == "root"
        RootMonkey(op(l[2]), parsemonkey(l[1], lines), parsemonkey(l[3], lines))
    elseif length(l) == 1
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
eval_monkey(m::HumanMonkey) = m.val
function eval_monkey(m::MathMonkey)
    m.op(eval_monkey(m.v1), eval_monkey(m.v2))
end
function eval_monkey(m::RootMonkey)
    m.op(eval_monkey(m.v1), eval_monkey(m.v2))
end


has_human(m::HumanMonkey) = true
has_human(m::NumMonkey) = false
function has_human(m::MathMonkey)
    has_human(m.v1) || has_human(m.v2)
end

find_human_val(m::HumanMonkey, n) = n

function find_human_val(m::RootMonkey)
    if has_human(m.v1)
        find_human_val(m.v1, eval_monkey(m.v2))
    else
        find_human_val(m.v2, eval_monkey(m.v1))
    end
end

function find_human_val(m::MathMonkey, n)
    if has_human(m.v1)
        if m.op == +
            find_human_val(m.v1, n-eval_monkey(m.v2))
        elseif m.op == -
            find_human_val(m.v1, n+eval_monkey(m.v2))
        elseif m.op == *
            find_human_val(m.v1, n÷eval_monkey(m.v2))
        elseif m.op == ÷
            find_human_val(m.v1, n*eval_monkey(m.v2))
        end
    else
        if m.op == +
            find_human_val(m.v2, n-eval_monkey(m.v1))
        elseif m.op == -
            find_human_val(m.v2, eval_monkey(m.v1)-n)
        elseif m.op == *
            find_human_val(m.v2, n÷eval_monkey(m.v1))
        elseif m.op == ÷
            find_human_val(m.v2, eval_monkey(m.v1)÷n)
        end
    end
end


root = readmonkeys("input")
println("Part 1: ", eval_monkey(root))
println("Part 2: ", find_human_val(root))
