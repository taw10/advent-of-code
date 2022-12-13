# Advent of code 2022, day 1

function load_file(filename)
    calories = Vector{Int64}()
    sum = 0
    open(filename, "r") do fh
        for line in eachline(fh)
            if line == ""
                push!(calories, sum)
                sum = 0
            else
                sum += parse(Int, line)
            end
        end
    end
    calories
end

calories = load_file("input")
cals_sorted = sort(calories, rev=true, alg=PartialQuickSort(3))
println("Part 1: ", cals_sorted[1])
println("Part 2: ", sum(cals_sorted[1:3]))
