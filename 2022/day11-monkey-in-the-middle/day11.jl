# Advent of code 2022, day 11: Monkey in the Middle

mutable struct Monkey
    items
    worry_op
    worry_test
    throw_true
    throw_false
    inspections
end

example = [Monkey([79,98], x->x*19, 23, 2, 3, 0),
           Monkey([54,65,75,74], x->x+6, 19, 2, 0, 0),
           Monkey([79,60,97], x->x*x, 13, 1, 3, 0),
           Monkey([74], x->x+3, 17, 0, 1, 0)]

monkeys = [Monkey([64,89,65,95], x->x*7, 3, 4, 1, 0),
           Monkey([76,66,74,87,70,56,51,66], x->x+5, 13, 7, 3, 0),
           Monkey([91,60,63], x->x*x, 2, 6, 5, 0),
           Monkey([92,61,79,97,79], x->x+6, 11, 2, 6, 0),
           Monkey([93,54], x->x*11, 5, 1, 7, 0),
           Monkey([60,79,92,69,88,82,70], x->x+8, 17, 4, 0, 0),
           Monkey([64,57,73,89,55,53], x->x+1, 19, 0, 5, 0),
           Monkey([62], x->x+4, 7, 3, 2, 0)]


function monkey_business_level(monkeys_in, rounds, ndiv)
    monkeys = deepcopy(monkeys_in)
    for _ in 1:rounds
        for monkey in monkeys
            for item in monkey.items
                item = monkey.worry_op(item) ÷ ndiv
                item = item % prod(map(x->x.worry_test, monkeys))
                if item % monkey.worry_test == 0
                    push!(monkeys[monkey.throw_true+1].items, item)
                else
                    push!(monkeys[monkey.throw_false+1].items, item)
                end
                monkey.inspections += 1
            end
            monkey.items = []
        end
    end
    business = reverse(sort(map(x->x.inspections, monkeys)))
    business[1] * business[2]
end

println("Part 1: ", monkey_business_level(monkeys, 20, 3))
println("Part 2: ", monkey_business_level(monkeys, 10000, 1))

