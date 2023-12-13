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


function runmaprange(ranges, xranges)

    for omap in ranges

        newranges = []
        for xrange in xranges

            srange = omap[1]
            drange = omap[2]
            offs = drange.start - srange.start

            println(xrange, "    ", srange, "   ->  ", offs)
    
            if xrange.stop < srange.start
                # Case 1: range fully below map
                push!(newranges, xrange)
    
            elseif xrange.start > srange.stop
                # Case 2: range fully above map
                push!(newranges, xrange)
    
            elseif xrange.start<srange.start && srange.start<=xrange.stop<=srange.stop
                # Case 3: range overlaps from below map to within
                push!(newranges, xrange.start:srange.start-1)
                push!(newranges, (srange.start:xrange.stop) .+ offs)
    
            elseif srange.start<=xrange.start<=srange.stop && xrange.stop>srange.stop
                # Case 4: range overlaps from within map to above
                push!(newranges, (xrange.start:srange.stop) .+ offs)
                push!(newranges, srange.stop+1:xrange.stop)
    
            elseif srange.start<=xrange.start && srange.stop>=xrange.stop
                # Case 5: range fully within map
                push!(newranges, xrange .+ offs)
    
            elseif xrange.start<=srange.start && xrange.stop>=srange.stop
                # Case 6: range fully surrounds map
                push!(newranges, xrange.start:srange.start-1)
                push!(newranges, srange .+ offs)
                push!(newranges, srange.stop+1:xrange.stop)
                
            else
                println("Couldn't figure out case: ", xrange, " ", srange)
            end


        end
        println("       ----> ", newranges)
        xranges = newranges

    end
    return xranges
end

seedranges = [seeds[i]:seeds[i]+seeds[i+1]-1 for i in 1:2:length(seeds)]
println(seedranges)

let xranges = seedranges
    for m in maps
        xranges = runmaprange(m, xranges)
    end

    println("Part 2: ", minimum(map(x->x.start, xranges)))
end
