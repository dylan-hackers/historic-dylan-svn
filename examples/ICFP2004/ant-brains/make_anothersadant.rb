def print_state(name, othername, bit, direction, details)
    basename = "#{name}#{direction}"

    print <<END_OF_STATE
#{basename}:
Move . #{basename}_blocked:
#{if (((direction + 4) % 6) & 0x1) != 0 then 'Mark' else 'Unmark' end} #{(bit + 3) % 6} .
#{if (((direction + 4) % 6) & 0x2) != 0 then 'Mark' else 'Unmark' end} #{(bit + 4) % 6} .
#{if (((direction + 4) % 6) & 0x4) != 0 then 'Mark' else 'Unmark' end} #{(bit + 5) % 6} .
#{details}
Turn Right .
Turn Right .
Turn Right #{othername}#{(direction + 3) % 6}:

#{basename}_trail:
Sense Here #{basename}_trail024: . Marker #{bit + 0}
Sense Here #{basename}_trail125: . Marker #{bit + 1}
Sense Here #{basename}_trail345: #{basename}: Marker #{bit + 2}

#{basename}_blocked:
Turn Right #{name}#{(direction + 1) % 6}:

#{basename}_trail024:
Sense Here #{basename}_trail2: . Marker #{(bit + 1) % 6}
Sense Here #{basename}_trail4: #{basename}_trail0: Marker #{(bit + 2) % 6}

#{basename}_trail125:
Sense Here #{basename}_trail2: . Marker #{(bit + 0) % 6}
Sense Here #{basename}_trail5: #{basename}_trail0: Marker #{(bit + 2) % 6}

#{basename}_trail345:
Sense Here #{basename}_trail4: . Marker #{(bit + 0) % 6}
Sense Here #{basename}_trail5: #{basename}_trail3: Marker #{(bit + 1) % 6}

END_OF_STATE

    0.upto(5) do |trail_direction|
        larger_trail_direction = trail_direction
        if trail_direction < direction then
            larger_trail_direction += 6
        end
        
        puts "#{basename}_trail#{trail_direction}:"
        if larger_trail_direction > direction then
            (larger_trail_direction - direction - 1).times do
                puts "Turn Right ."
            end
            puts "Turn Right #{name}#{trail_direction}:"
        else
            puts "Flip 2 #{basename}: #{basename}:"
        end
        puts ""
    end
end

puts <<END_OF_PROLOGUE
Flip 6 search0_trail0: .
Flip 5 search0_trail1: .
Flip 4 search0_trail2: .
Flip 3 search0_trail3: .
Flip 2 search0_trail4: search0_trail5:

END_OF_PROLOGUE

0.upto(5) do |direction|
    print_state('search', 'return', 0, direction, <<END_OF_SEARCH_DETAILS
Sense Here search#{direction}_trail: . Home
PickUp . search#{direction}:
END_OF_SEARCH_DETAILS
)
end

0.upto(5) do |direction|
    print_state('return', 'search', 3, direction, <<END_OF_RETURN_DETAILS
Sense Here . return#{direction}_trail: Home
Drop .
END_OF_RETURN_DETAILS
)
end