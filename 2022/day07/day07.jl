# Advent of code 2022, day 7

struct INode
    name
    size::Union{Int,Nothing}
    children::Vector{INode}
    parent::Union{INode,Nothing}
end

INode(name, size, parent) = INode(name, size, INode[], parent)
INode(name, parent) = INode(name, nothing, INode[], parent)


function add_file(parent, name, size)
    push!(parent.children, INode(name, size, parent))
end


function add_subdir(parent, name)
    push!(parent.children, INode(name, parent))
end


function find_subdir(parent, name)
    idx = findfirst(x->x.name==name, parent.children)
    if idx === nothing
        error("Can't find ", name, " under ", parent)
    end
    parent.children[idx]
end


function load(filename)
    root = INode("/", nothing)
    cur = root
    open(filename, "r") do fh
        for line in eachline(fh)
            s = split(line)
            if s[1] == "\$" && s[2] == "cd"
                if s[2] == "cd"
                    if s[3] == "/"
                        cur = root
                    elseif s[3] == ".."
                        cur = cur.parent
                    else
                        cur = find_subdir(cur, s[3])
                    end
                end
            elseif s[1] == "dir"
                add_subdir(cur, s[2])
            elseif s[1] != "\$"
                add_file(cur, s[2], parse(Int, s[1]))
            end
        end
    end
    root
end


function walk_tree(f, root)
    f(root)
    for subdir in root.children
        walk_tree(f, subdir)
    end
end


function total_size(e)
    s = 0
    for ch in e.children
        if ch.size !== nothing
            s += ch.size
        else
            s += total_size(ch)
        end
    end
    s
end


tree = load("input")
let sum = 0
    walk_tree(tree) do e
        if e.size === nothing
            sz = total_size(e)
            if sz <= 100000
                sum += sz
            end
        end
    end
    println("Part 1: ", sum)
end


space_used = total_size(tree)
space_free = 70000000 - space_used
space_needed = 30000000 - space_free
let min = 7000000000
    walk_tree(tree) do e
        if e.size === nothing
            sz = total_size(e)
            if sz >= space_needed && sz < min
                min = sz
            end
        end
    end
    println("Part 2: ", min)
end
