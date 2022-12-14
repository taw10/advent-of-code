# Advent of code 2022, day 12: Hill Climbing Algorithm


function char_to_n(ch)
    findfirst(ch, "abcdefghijklmnopqrstuvwxyz")
end


function read_input(s)
    w = length(s[1])
    h = length(s)
    arr = Array{Int}(undef, w, h)
    local start, finish
    for x=1:w, y=1:h
        let v = s[y][x]
            if v == 'S'
                arr[x,y] = 1
                start = CartesianIndex(x,y)
            elseif v == 'E'
                arr[x,y] =  26
                finish = CartesianIndex(x,y)
            else
                arr[x,y] = char_to_n(v)
            end
        end
    end
    arr, start, finish
end


function neighbours(pos, limits)
    neigh = CartesianIndex[]
    if pos[1] > 1
        push!(neigh, pos-CartesianIndex(1,0))
    end
    if pos[2] > 1
        push!(neigh, pos-CartesianIndex(0,1))
    end
    if pos[1] < limits[1]
        push!(neigh, pos+CartesianIndex(1,0))
    end
    if pos[2] < limits[2]
        push!(neigh, pos+CartesianIndex(0,1))
    end
    neigh
end


function unvisitedneighbours(current, visited, arr)
    filter(neighbours(current, size(arr))) do x
        arr[x] <= arr[current]+1 && !visited[x]
    end
end


function dijkstra(arr, start, finish)
    visited = similar(arr, Bool)
    visited .= false
    dist = similar(arr, Int)
    dist .= 99999999
    prev = similar(arr, CartesianIndex)
    dist[start] = 0
    queue = [start]
    current = start
    while current != finish && length(queue) > 0
        _,idx = findmin(dist[queue])
        current = queue[idx]

        for neigh in unvisitedneighbours(current, visited, arr)
            let new_dist = dist[current]+1
                if new_dist < dist[neigh]
                    dist[neigh] = dist[current] + 1
                    prev[neigh] = current
                end
            end
            push!(queue, neigh)
        end
        visited[current] = true
        queue = [x for x in queue if x != current]
    end

    if visited[current]
        path = CartesianIndex[]
        current = finish
        while current != start
            push!(path, current)
            if isassigned(prev, current.I...)
                current = prev[current]
            else
                return 9999999
            end
        end
        length(path)
    else
        9999999
    end
end


input,start,finish = read_input(readlines("input"))
println("Part 1: ", dijkstra(input, start, finish))

startpoints = findall(isequal(1), input)
println("Part 2: ", minimum(x->dijkstra(input, x, finish), startpoints))
