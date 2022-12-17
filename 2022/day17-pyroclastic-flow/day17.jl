# Advent of code 2022, day 17: Pyroclastic flow

import Base.+
import Base.-

rocks = [[[0,0],[1,0],[2,0],[3,0]],
         [[0,1],[1,0],[1,1],[2,1],[1,2]],
         [[0,0],[1,0],[2,0],[2,1],[2,2]],
         [[0,0],[0,1],[0,2],[0,3]],
         [[0,0],[1,0],[0,1],[1,1]]]


function width(rock)
    maximum(x->x[1], rock)
end


function height(rock)
    maximum(x->x[2], rock)
end


+(a::CartesianIndex, b::Vector{Int}) = +(a, CartesianIndex(b...))
+(a::Vector{Int}, b::CartesianIndex) = +(CartesianIndex(a...), b)
-(a::CartesianIndex, b::Vector{Int}) = -(a, CartesianIndex(b...))
-(a::Vector{Int}, b::CartesianIndex) = -(CartesianIndex(a...), b)


function can_move(pos, board, rock)
    pos.I[1] > 0 &&
    pos.I[1]+width(rock) < 8 &&
    pos.I[2] > 0 &&
    all(x->board[x+pos]==0, rock)
end


function drop_rock!(board, cmds, rock, h)
    pos = CartesianIndex(3, h+4)
    while true

        cmd = take!(cmds)

        if can_move(pos+cmd, board, rock)
            pos += cmd
        end

        if can_move(pos-[0,1], board, rock)
            pos -= [0,1]
        else
            for i in rock
                board[i+pos] = 1
            end
            return max(h,pos.I[2]+height(rock))
        end

    end
end


cmds = Channel() do ch
    for i in Iterators.cycle(split(readlines("input")[1], ""))
        if i == "<"
            j = [-1,0]
        elseif i == ">"
            j = [1,0]
        else
            println("??? ", i)
        end
        put!(ch, j)
    end
end

board = zeros(Int64, 7, 2022*4)

let h = 0
    for r in 1:2022
        h = drop_rock!(board, cmds, rocks[1+(r-1)%5], h)
    end
    println("Part 1: ", h)
end
