# Advent of code 2022, day 9

struct Cmd
    dir
    n
end

dvec = Dict("R"=>(1,0), "L"=>(-1,0), "U"=>(0,1), "D"=>(0,-1))

function parse_cmd(s)
    dir,n = split(s)
    Cmd(dvec[dir], parse(Int, n))
end


function overlap(v)
    (v[1]==0 && v[2]==0) || (v[1]==0 && v[2]==0)
end

function one_space_move(v)
    (v[1]==0 && abs(v[2])==1) || (abs(v[1])==1 && v[2]==0)
end

function diagonal_move(v)
    (abs(v[1])==1 && abs(v[2])==1) || (abs(v[1])==1 && abs(v[2])==1)
end

function two_space_move(v)
    (v[1]==0 && abs(v[2])==2) || (abs(v[1])==2 && v[2]==0)
end


function knights_move(v)
    (abs(v[1])==1 && abs(v[2])==2) || (abs(v[1])==2 && abs(v[2])==1)
end

function two_space_diagonal(v)
    (abs(v[1])==2 && abs(v[2])==2) || (abs(v[1])==2 && abs(v[2])==2)
end

function tmove(diff)
    if two_space_move(diff) || two_space_diagonal(diff)
        diff .÷ 2
    elseif knights_move(diff)
        if abs(diff[1]) == 2
            diff[1]÷2, diff[2]
        elseif abs(diff[2]) == 2
            diff[1], diff[2]÷2
        else
            error("Invalid knight's move ", diff)
        end
    elseif one_space_move(diff) || diagonal_move(diff) || overlap(diff)
        0,0
    else
        error("Unrecognised move ", diff)
    end
end


function move(dir, head, tail)
    head = dir .+ head
    tail = tail .+ tmove(head .- tail)
    return head, tail
end


v = map(parse_cmd, readlines("input"))

let head = (1000,1000), tail = head, grid = zeros(Bool, 2000,2000)
    grid[tail[1],tail[2]] = 1
    for cmd in v
        for i in 1:cmd.n
            head, tail = move(cmd.dir, head, tail)
            grid[tail[1],tail[2]] = true
        end
    end
    println("Part 1: ", count(grid))
end


function move_snake(dir, snake)
    snake[1] = snake[1] .+ dir
    for i in 2:length(snake)
        snake[i] = snake[i] .+ tmove(snake[i-1] .- snake[i])
    end
    snake
end

let snake = fill((1000,1000),10), grid = zeros(Bool, 2000,2000)
    grid[snake[10][1],snake[10][2]] = true
    for cmd in v
        for i in 1:cmd.n
            snake = move_snake(cmd.dir, snake)
            grid[snake[10][1],snake[10][2]] = true
        end
    end
    println("Part 2: ", count(grid))
end
