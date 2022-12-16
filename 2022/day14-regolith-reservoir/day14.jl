# Advent of code 2022, day 14: Regolith Reservoir


function parsecoords(s)
    xs,ys = split(s, ",")
    x = parse(Int, xs)
    y = parse(Int, ys)
    CartesianIndex(x,y)
end


function drawline!(arr, p1, p2)
    for p in p1:p2
        arr[p] = 1
    end
    for p in p2:p1
        arr[p] = 1
    end
end


function readinput(filename)
    arr = zeros(Int8, 1000, 1000)
    floor = 0
    for line in readlines(filename)
        points = split(line, " -> ")
        p1 = parsecoords(points[1])
        for point in points[2:end]
            p2 = parsecoords(point)
            drawline!(arr, p1, p2)
            p1 = p2
            floor = max(p2.I[2], floor)
        end
    end
    arr,floor
end


function dropgrain(arr, start, floor)
    while true
        start[2] > floor && return false  # Grain falls into the abyss
        prefs = [start+CartesianIndex(0,1), start+CartesianIndex(-1,1), start+CartesianIndex(1,1)]
        i = findfirst(x->arr[x]==0, prefs)
        if i === nothing
            # Grain comes to rest here
            arr[start] = 1
            return true
        end
        start = prefs[i]
    end
end


let (arr,floor) = readinput("input")
    i = 0
    while dropgrain(arr, CartesianIndex(500,0), floor)
        i += 1
    end
    println("Part 1: ", i)
end


