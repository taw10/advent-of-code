mutable struct Hand
    red
    green
    blue
end


function game_possible(hands)
    for hand in hands
        if hand.red > 12
            return false
        elseif hand.green > 13
            return false
        elseif hand.blue > 14
            return false
        end
    end
    return true
end


function game_min_cubes_power(hands)
    minhand = Hand(0,0,0)
    for hand in hands
        if hand.red  > minhand.red
            minhand.red = hand.red
        end
        if hand.green  > minhand.green
            minhand.green = hand.green
        end
        if hand.blue  > minhand.blue
            minhand.blue = hand.blue
        end
    end
    minhand.red * minhand.blue * minhand.green
end


function parse_game(line)

    g = split(line, ":")[1]
    gn = split(g, " ")[2]
    h = split(split(line, ":")[2], ";")

    hands = map(h) do hand
        s = Hand(0,0,0)
        for cubes in map(strip, split(strip(hand), ","))
            n = parse(Int, split(cubes)[1])
            col = split(cubes)[2]
            if col == "red"
                s.red = n
            elseif col == "green"
                s.green = n
            elseif col == "blue"
                s.blue = n
            end
        end
        s
    end

    return parse(Int, gn),hands

end


function sum_possible_games(filename)
    total = 0
    for line in eachline(filename)
        gamenum,hands = parse_game(line)
        if game_possible(hands)
            total += gamenum
        end
    end
    total
end


function sum_min_powers(filename)
    total = 0
    for line in eachline(filename)
        _,hands = parse_game(line)
        total += game_min_cubes_power(hands)
    end
    total
end


println("Part 1: ", sum_possible_games("input"))
println("Part 2: ", sum_min_powers("input"))
