function readinput(filename)
    lines = []
    for line in eachline(filename)
        push!(lines, map(x->parse(Int,x), split(line)))
    end
    lines
end


function first_and_last_of_differences(seq)
    out = []
    while any(x->x!=0, seq)
        push!(out, (seq[1], seq[end]))
        seq = [seq[i+1]-seq[i] for i in 1:length(seq)-1]
    end
    out
end


data = readinput("input")
diffs = map(first_and_last_of_differences, data)

nextvals = map(x->sum(map(y->y[2], x)), diffs)
println("Part 1: ", sum(nextvals))

prevvals = map(x->foldr(-, map(y->y[1], x)), diffs)
println("Part 2: ", sum(prevvals))
