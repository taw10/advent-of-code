function readgalaxies(filename)
    galaxies = []
    for (y,line) in enumerate(eachline(filename))
        for galx in findall(x->x=='#', line)
            push!(galaxies, (galx,y))
        end
    end
    galaxies
end


function expanduniverse(galaxies, n)

    xs = map(x->x[1], galaxies)
    ys = map(x->x[2], galaxies)
    w = maximum(xs)
    h = maximum(ys)

    emptycols = []
    for x in w:-1:1
        if !any(q->q==x, xs)
            push!(emptycols, x)
        end
    end

    emptyrows = []
    for y in h:-1:1
        if !any(q->q==y, ys)
            push!(emptyrows, y)
        end
    end

    for ecol in emptycols
        galaxies = map(galaxies) do gal
            if gal[1] > ecol
                (gal[1]+n,gal[2])
            else
                gal
            end
        end
    end

    for erow in emptyrows
        galaxies = map(galaxies) do gal
            if gal[2] > erow
                (gal[1],gal[2]+n)
            else
                gal
            end
        end
    end

    galaxies

end


function sumdist(galaxies)
    total = 0
    for i in eachindex(galaxies)
        for j in i+1:length(galaxies)
            c = galaxies[i] .- galaxies[j]
            total += abs(c[1])+abs(c[2])
        end
    end
    total
end


origalaxies = readgalaxies("input")
println("Part 1: ", sumdist(expanduniverse(origalaxies, 1)))
println("Part 2: ", sumdist(expanduniverse(origalaxies, 999999)))
