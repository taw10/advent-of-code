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


function drop_rock!(board, cmds, rock, h, cmd_pos)
    pos = CartesianIndex(3, h+4)
    while true

        cmd = cmds[cmd_pos]
        cmd_pos += 1
        if cmd_pos > length(cmds)
            cmd_pos = 1
        end

        if can_move(pos+cmd, board, rock)
            pos += cmd
        end

        if can_move(pos-[0,1], board, rock)
            pos -= [0,1]
        else
            for i in rock
                board[i+pos] = 1
            end
            return max(h,pos.I[2]+height(rock)),cmd_pos
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


struct RockState
    rn
    board
    cmd_pos
    h
    scroll
end

function isequal(a::RockState, b::RockState)
    a.board == b.board && a.cmd_pos == b.cmd_pos && a.h == b.h
    # ... but not scroll
end

function height_after_rocks(n)

    cmds = map(split(readlines("input")[1], "")) do x
        if x == "<"
            return [-1,0]
        elseif x == ">"
            return [1,0]
        else
            println("??? ", x)
            return [0,0]
        end
    end

    cmd_pos = 1

    h = 0
    scroll = 0
    bh = 1000
    board = zeros(Int64, 7, bh)

    prev = RockState[]

    r = 0
    while r < n

        # Drop a rock
        h,cmd_pos = drop_rock!(board, cmds, rocks[1+r%5], h, cmd_pos)
        r += 1

        # Get rid of everything below a filled row
        row = highest_filled(board, h)
        if row !== nothing
            #println("After ", r, " rocks, row ", row, " is full.  Height is ", h)
            nr = h - row
            board[:,1:nr] = board[:,row+1:h]
            board[:,nr+1:h] .= 0
            scroll += row
            h -= row
        end

        # Skip lots if we have a cycle
        cur_state = RockState(r, deepcopy(board), cmd_pos, h, scroll)
        pn = findfirst(x->isequal(x, cur_state), prev)
        if pn === nothing
            push!(prev, cur_state)
        else
            prev_state = prev[pn]
            #println("Found cycle! ", prev_state.rn, " -> ", cur_state.rn)
            cycles_needed = (n-r)÷(cur_state.rn - prev_state.rn)
            rocks_to_skip = cycles_needed*(cur_state.rn - prev_state.rn)
            #println("Skipping ", cycles_needed, " cycles = ", rocks_to_skip, " rocks")
            r += rocks_to_skip
            scroll += cycles_needed*(cur_state.scroll - prev_state.scroll)
            prev = RockState[]
        end

    end

    h+scroll

end


println("Part 1: ",  height_after_rocks(2022))
println("Part 2: ",  height_after_rocks(1000000000000))
# NB: Part 2 doesn't work for the example input.  There are no filled rows,
# so the board doesn't get cleared out, and therefore can't match the previous
# layout.
