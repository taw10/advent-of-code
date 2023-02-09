# Advent of code 2022, day 15

import Base.push!
import Base.contains
import Base.length

struct Sensor
    x::Int64
    y::Int64
    cbx::Int64
    cby::Int64
end

mutable struct RangeSet
    ranges::Vector{UnitRange{Int64}}
end

RangeSet() = RangeSet(Vector{UnitRange}())
length(rset::RangeSet) = length(rset.ranges)

function overlap(a, b)
    !((a.stop+1 < b.start) || (b.stop+1 < a.start))
end

function push!(rset::RangeSet, x::UnitRange)
    if x != 0:0
        keep = Vector{UnitRange{Int64}}()
        for r in rset.ranges
            if overlap(r, x)
                x = min(r.start, x.start) : max(r.stop, x.stop)
            else
                push!(keep, r)
            end
        end
        push!(keep, x)
        rset.ranges = keep
    end
    rset
end

function entire_range(rset::RangeSet)
    minimum([x.start for x in rset.ranges]) : maximum([x.stop for x in rset.ranges])
end

sensors = [Sensor(2391367, 3787759, 2345659, 4354867),
           Sensor(1826659, 2843839, 1654342, 3193298),
           Sensor(980874,  2369046, 31358,   2000000),
           Sensor(2916267, 2516612, 3064453, 2107409),
           Sensor(3304786, 844925,  3064453, 2107409),
           Sensor(45969,   76553,   31358,   2000000),
           Sensor(2647492, 1985479, 2483905, 2123337),
           Sensor(15629,   2015720, 31358,   2000000),
           Sensor(3793239, 3203486, 3528871, 3361675),
           Sensor(3998240, 15268,   4731853, 1213406),
           Sensor(3475687, 3738894, 3528871, 3361675),
           Sensor(3993022, 3910207, 3528871, 3361675),
           Sensor(258318,  2150378, 31358,   2000000),
           Sensor(1615638, 1108834, 2483905, 2123337),
           Sensor(1183930, 3997648, 1654342, 3193298),
           Sensor(404933,  3377916, 1654342, 3193298),
           Sensor(3829801, 2534117, 3528871, 3361675),
           Sensor(2360813, 2494240, 2483905, 2123337),
           Sensor(2286195, 3134541, 1654342, 3193298),
           Sensor(15626,   1984269, 31358,   2000000),
           Sensor(3009341, 3849969, 3528871, 3361675),
           Sensor(1926292, 193430 , 1884716, -881769),
           Sensor(3028318, 3091480, 3528871, 3361675)]


example = [Sensor(2, 18, -2, 15),
           Sensor(9, 16, 10, 16),
           Sensor(13, 2, 15, 3),
           Sensor(12, 14, 10, 16),
           Sensor(10, 20, 10, 16),
           Sensor(14, 17, 10, 16),
           Sensor(8, 7, 2, 10),
           Sensor(2, 0, 2, 10),
           Sensor(0, 11, 2, 10),
           Sensor(20, 14, 25, 17),
           Sensor(17, 20, 21, 22),
           Sensor(16, 7, 15, 3),
           Sensor(14, 3, 15, 3),
           Sensor(20, 1, 15, 3)]


function project_to_row(sensor, y)
    rad = abs(sensor.x-sensor.cbx) + abs(sensor.y-sensor.cby)
    yoffs = abs(y - sensor.y)
    this_rad = rad - yoffs
    if this_rad < 0
        return 0:0
    else
        return sensor.x-this_rad:sensor.x+this_rad
    end
end


function contains(rset::RangeSet, x)
    for r in rset.ranges
        if x in r
            return true
        end
    end
    return false
end


function sensor_ranges(sensors, y)
    ranges = RangeSet()
    for sensor in sensors
        r = project_to_row(sensor, y)
        push!(ranges, r)
    end
    ranges
end


function positions_without_beacon(sensors, y)

    ranges = sensor_ranges(sensors, y)

    sx = Set{Int64}()
    for sensor in sensors
        if sensor.cby == y
            push!(sx, sensor.cbx)
        end
    end

    n = 0
    for x in entire_range(ranges)
        if contains(ranges, x) && !(x in sx)
            n += 1
        end
    end

    n

end


function beacon_in_row(sensors, y, size)

    ranges = sensor_ranges(sensors, y)

    if length(ranges) > 1
        for x in 1:size
            if !contains(ranges, x)
                return x
            end
        end
    end

    false

end


function beacon_pos(sensors, size)
    for y in 1:size
        x = beacon_in_row(sensors, y, size)
        if x !== false
            return x,y
        end
    end
    return false
end


freq(x, y) = 4000000 * x + y

println("Part 1 example: ", positions_without_beacon(example, 10))
println("Part 1: ", positions_without_beacon(sensors, 2000000))

println("Part 2 example: ", freq(beacon_pos(example, 20)...))
println("Part 2: ", freq(beacon_pos(sensors, 4000000)...))
