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


function highest_filled(board, h)
    for y in h:-1:1
        if all(board[:,y] .== 1)
            return y
        end
    end
    nothing
end


function height_after_rocks(n)

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

    h = 0
    scroll = 0
    bh = 100000
    board = zeros(Int64, 7, bh)

    for r in 1:n
        h = drop_rock!(board, cmds, rocks[1+(r-1)%5], h)
        row = highest_filled(board, h)
        if row !== nothing
            #println("After ", r, " rocks, row ", row, " is full.  Height is ", h)
            nr = h - row
            board[:,1:nr] = board[:,row+1:h]
            board[:,nr+1:h] .= 0
            scroll += row
            h -= row
        end
    end

    h+scroll

end


println("Part 1: ",  height_after_rocks(2022))
println("Part 2: ",  height_after_rocks(1000000000000))
