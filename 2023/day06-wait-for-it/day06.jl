racetimes = [40,82,84,92]
distrecords = [233,1011,1110,1487]

nwinning(racetime, distrecord) = count(x->x*racetime-x*x>distrecord, 0:racetime)
println("Part 1: ", prod(x->nwinning(x...), zip(racetimes, distrecords)))
println("Part 2: ", nwinning(40828492,233101111101487))
