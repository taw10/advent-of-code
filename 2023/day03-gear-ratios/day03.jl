function strings_to_array(s)
    w = maximum(map(length, s))
    h = length(s)
    [s[y][x] for x=1:w, y=1:h]
end

read2d(filename) = strings_to_array(readlines(filename))


function arr2string(arr)
    total = 0
    for p in arr
        total *= 10
        total += parse(Int, p)
    end
    total
end


function findnum(line)
    let fdigit=0, ldigit=0
        for x in 1:length(line)
            if isdigit(line[x])
                if fdigit == 0
                    fdigit = x
                end
                ldigit = x
            else
                fdigit != 0 && break
            end
        end
        if fdigit != 0
            val = arr2string(line[fdigit:ldigit])
            line[fdigit:ldigit] .= '.'
            return val,fdigit:ldigit
        end
    end
    return false
end


function gears(instars)

    out = []
    stars = sort(instars)
    for idx in 1:length(stars)-1
        if stars[idx][1] == stars[idx+1][1]
            push!(out, stars[idx][2]*stars[idx+1][2])
        end
    end

    out
end


issymbol(ch) = (ch != '.') && !isdigit(ch)

function adjacent(xr, y, w, h)

    coords = []

    # Row above
    if y>1
        for x in max(xr.start-1, 1):min(xr.stop+1,size(grid)[1])
            push!(coords, (x,y-1))
        end
    end

    # Row below
    if y<size(grid)[2]
        for x in max(xr.start-1, 1):min(xr.stop+1,size(grid)[1])
            push!(coords, (x,y+1))
        end
    end

    # To the left and right
    xr.start>1 && push!(coords, (xr.start-1,y))
    xr.stop<size(grid)[1] && push!(coords, (xr.stop+1,y))

    return coords

end


grid = read2d("input")

let total = 0
    for y in 1:size(grid)[2]
        while (s = findnum(view(grid, :,y))) != false
            if any(c->issymbol(grid[c...]), adjacent(s[2], y, size(grid)...))
                total += s[1]
            end
        end
    end
    println("Part 1: ", total)
end


grid = read2d("input")

let stars = []
    for y in 1:size(grid)[2]
        while (s = findnum(view(grid, :,y))) != false
            let adj = adjacent(s[2], y, size(grid)...)
                l = findall(c->grid[c...]=='*', adj)
                for lx in l
                    push!(stars, (adj[lx], s[1]))
                end
            end
        end
    end

    println("Part 2: ", sum(gears(stars)))

end
