using Base.Iterators

function readinput(filename)

    fh = eachline(filename)

    seeds = map(x->parse(Int, x), split(first(fh))[2:end])
    first(fh)

    maps = []
    ranges = []
    for l in fh

        if l != ""

            # Skip header
            if l[end] == ':'
                continue
            end

            nums = map(x->parse(Int, x), split(l))
            source_start = nums[2]
            source_end = nums[2]+nums[3]-1
            dest_start = nums[1]
            dest_end = nums[1]+nums[3]-1
            push!(ranges, (source_start:source_end,dest_start:dest_end))

        else
            push!(maps, ranges)
            ranges = []
        end

   end

   push!(maps, ranges)
   return seeds,maps

end


function runmap(ranges, x)
    r = findlast(ranges) do range
        x in range[1]
    end
    if isnothing(r)
        return x
    else
        src_range = ranges[r][1]
        dst_range = ranges[r][2]
        return dst_range.start + x - src_range.start
    end
end

seeds,maps = readinput("example")
locations = map(seeds) do x
    for m in maps
        x = runmap(m, x)
    end
    x
end
println("Part 1: ", minimum(locations))


seedranges = [seeds[i]:seeds[i]+seeds[i+1]-1 for i in 1:2:length(seeds)]
println(seedranges)

let minloc = 9999999999999999
    for seedrange in seedranges
        println(seedrange, "  = ", seedrange.stop-seedrange.start)
        for seed in seedrange
            x = seed
            for m in maps
                x = runmap(m, x)
                println(m)
            end
            minloc = min(x, minloc)
        end
    end
    println("Part 2: ", minloc)
end
