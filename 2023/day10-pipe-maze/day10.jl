function strings_to_array(s)
    w = maximum(map(length, s))
    h = length(s)
    [s[y][x] for x=1:w, y=1:h]
end

read2d(filename) = strings_to_array(readlines(filename))

function findstart(grid)
    for idx in eachindex(IndexCartesian(), grid)
        if grid[idx] == 'S'
            return idx
        end
    end
end


function followpipe(pipe, dir)

    if pipe == 'F'
        dir == CartesianIndex(-1, 0) && return CartesianIndex(0,1)
        dir == CartesianIndex(0, -1) && return CartesianIndex(1,0)
    end

    if pipe == 'L'
        dir == CartesianIndex(-1, 0) && return CartesianIndex(0,-1)
        dir == CartesianIndex(0, 1) && return CartesianIndex(1,0)
    end

    if pipe == 'J'
        dir == CartesianIndex(1, 0) && return CartesianIndex(0,-1)
        dir == CartesianIndex(0, 1) && return CartesianIndex(-1,0)
    end

    if pipe == '7'
        dir == CartesianIndex(1, 0) && return CartesianIndex(0,1)
        dir == CartesianIndex(0, -1) && return CartesianIndex(-1,0)
    end

    return dir

end


function showgrid(grid)
    for y in 1:size(grid,2)
        for x in 1:size(grid,1)
            print(grid[x,y])
        end
        print("\n")
    end
end


function areainside(grid)
    points = 0
    for y in 1:size(grid, 2)
        inside = 0
        lastturn = nothing
        for x in 1:size(grid, 1)
            if grid[x,y] == '|'
                inside = 1 - inside
                lastturn = nothing
            elseif grid[x,y] in ['F','L']
                lastturn = grid[x,y]
            elseif grid[x,y] == 'J' && lastturn == 'L'
                lastturn = nothing
            elseif grid[x,y] == '7' && lastturn == 'L'
                lastturn = nothing
                inside = 1 - inside
            elseif grid[x,y] == 'J' && lastturn == 'F'
                lastturn = nothing
                inside = 1 - inside
            elseif grid[x,y] == '7' && lastturn == 'F'
                lastturn = nothing
            elseif grid[x,y] == '.' && inside == 1
                points += 1
                grid[x,y] = '*'
            end
        end
    end
    showgrid(grid)
    points
end


let grid = read2d("input"),
    cleangrid = similar(grid),
    st = findstart(grid),
    dir = CartesianIndex(0, 1),
    pos = st+dir,
    nsteps = 1

    fill!(cleangrid, '.')
    cleangrid[st] = '|'  # Insert true value of 'S' here.  '|' for my input
    while pos != st
        cleangrid[pos] = grid[pos]
        dir = followpipe(grid[pos], dir)
        pos += dir
        nsteps += 1
    end
    println("Part 1: ", nsteps÷2)
    println("Part 2: ", areainside(cleangrid))
end

