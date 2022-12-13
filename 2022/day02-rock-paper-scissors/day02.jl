# Advent of code 2022, day 2

val_table = Dict('X' => 1, 'Y' => 2, 'Z' => 3)

result_table = Dict('A'=> Dict('X' => 3, 'Y' => 6, 'Z' => 0),
                    'B'=> Dict('X' => 0, 'Y' => 3, 'Z' => 6),
                    'C'=> Dict('X' => 6, 'Y' => 0, 'Z' => 3))

function rps(strategy_func)
    score = 0
    open("input", "r") do fh
        for line in eachline(fh)
            yours = line[1]
            mine = strategy_func(yours, line[3])
            score += val_table[mine] + result_table[yours][mine]
        end
    end
    score
end


function passthrough(yours, mine)
    mine
end


strategy_table = Dict('X'=> Dict('A' => 'Z', 'B' => 'X', 'C' => 'Y'),
                      'Y'=> Dict('A' => 'X', 'B' => 'Y', 'C' => 'Z'),
                      'Z'=> Dict('A' => 'Y', 'B' => 'Z', 'C' => 'X'))

function strategy(yours, mine)
    strategy_table[mine][yours]
end


println("Part 1: ", rps(passthrough))
println("Part 2: ", rps(strategy))
