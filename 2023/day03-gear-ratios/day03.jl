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


issymbol(ch) = (ch != '.') && !isdigit(ch)

function symboladj(grid, xr, y)

    # Row above
    if y>1
        for x in max(xr.start-1, 1):min(xr.stop+1,size(grid)[1])
            issymbol(grid[x,y-1]) && return true
        end
    end

    # Row below
    if y<size(grid)[2]
        for x in max(xr.start-1, 1):min(xr.stop+1,size(grid)[1])
            issymbol(grid[x,y+1]) && return true
        end
    end

    # To the left and right
    xr.start>1 && issymbol(grid[xr.start-1,y]) && return true
    xr.stop<size(grid)[1] && issymbol(grid[xr.stop+1,y]) && return true

    return false

end


grid = read2d("input")

let total = 0
    for y in 1:size(grid)[2]
        while (s = findnum(view(grid, :,y))) != false
            print("Number at ", s)
            if symboladj(grid, s[2], y)
                total += s[1]
                println("  valid")
            else
                println("")
            end
        end
    end
    println("Part 1: ", total)
end
