# Advent of code 2022, day 7

using Match

struct INode
    name
    size::Union{Int,Nothing}      # if nothing, this is a directory
    children::Vector{INode}
    parent::Union{INode,Nothing}  # if nothing, this is the root
end

INode(name, size, parent) = INode(name, size, INode[], parent)
isdir(n) = (n.size === nothing)


function find_subdir(parent, name)
    idx = findfirst(x->x.name==name, parent.children)
    @assert(idx !== nothing)
    parent.children[idx]
end


function load(filename)
    root = INode("/", nothing, nothing)
    cur = root
    open(filename, "r") do fh
        for line in eachline(fh)
            @match split(line) begin
                ["\$", "cd", "/"]  => (cur = root)
                ["\$", "cd", ".."] => (cur = cur.parent)
                ["\$", "cd", n]    => (cur = find_subdir(cur, n))
                ["dir", n]         => push!(cur.children, INode(n, nothing, cur))
                ["\$", "ls"]       => false
                [sz, n]            => push!(cur.children, INode(n, parse(Int, sz), cur))
                _                  => error("Didn't understand ", line)
            end
        end
    end
    root
end


# Call f for all *directories* from root downwards
function walk_tree(f, root)
    f(root)
    for subdir in root.children
        isdir(subdir) && walk_tree(f, subdir)
    end
end


function total_size(e)
    s = 0
    for ch in e.children
        if isdir(ch)
            s += total_size(ch)
        else
            s += ch.size
        end
    end
    s
end


tree = load("input")
let sum = 0
    walk_tree(tree) do e
        sz = total_size(e)
        if sz <= 100000
            sum += sz
        end
    end
    println("Part 1: ", sum)
end

space_used = total_size(tree)
space_free = 70000000 - space_used
space_needed = 30000000 - space_free
let min = 999999999999
    walk_tree(tree) do e
        sz = total_size(e)
        if sz >= space_needed && sz < min
            min = sz
        end
    end
    println("Part 2: ", min)
end
