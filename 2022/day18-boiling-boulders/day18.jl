# Advent of code 2022, day 18: Boiling Boulders

using OffsetArrays

function read_cubes(filename)
    cubes = Vector[]
    for line in readlines(filename)
        coords = map(x->parse(Int,x), split(line, ","))
        push!(cubes, coords)
    end
    cubes
end


function surfacearea(grid, v)
    sa = 0
    for cube in cubes
        neighbours = [[1,0,0], [-1,0,0], [0,1,0], [0,-1,0], [0,0,1], [0,0,-1]]
        sa += count(x->grid[cube+x...]==v, neighbours)
    end
    sa
end


function flood(grid, v, seed)
    neighbours = [[1,0,0], [-1,0,0], [0,1,0], [0,-1,0], [0,0,1], [0,0,-1]]
    if grid[seed...] == 0
        grid[seed...] = v
        for n in neighbours
            if all(seed+n .>= -1) && all(seed+n .< 29)
                flood(grid, v, seed+n)
            end
        end
    end
end


cubes = read_cubes("input")
grid = OffsetArray(zeros(Int8, 31, 31, 31), -2, -2, -2)
for cube in cubes
    grid[cube...] = 1
end

println("Part 1: ", surfacearea(grid, 0))

flood(grid, 2, [-1,-1,-1])
println("Part 2: ", surfacearea(grid, 2))
