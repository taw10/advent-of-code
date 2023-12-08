fh = open("input")

# Read turns
tstring = collect(readline(fh))
turns = map(tstring) do x
    x == 'L' && return 1
    x == 'R' && return 2
end
readline(fh)

# Read nodes
nodes = Dict{AbstractString,Tuple{AbstractString,AbstractString}}()
while !eof(fh)
    line = readline(fh)
    node = line[1:3]
    nodeleft = line[8:10]
    noderght = line[13:15]
    nodes[node] = (nodeleft,noderght)
end

function pathlength(pred, start, nodes, turns)
    let node = start, n = 0, pos=1
        while !pred(node)
            node = nodes[node][turns[pos]]
            n += 1
            pos += 1
            if pos > length(turns)
                pos = 1
            end
        end
        return n
    end
end

println("Part 1: ", pathlength(x->x=="ZZZ", "AAA", nodes, turns))

let ghosts = filter(x->x[end]=='A', collect(keys(nodes)))
    paths = map(g->pathlength(x->x[end]=='Z', g, nodes, turns), ghosts)
    println("Part 2: ", lcm(paths...))
end
