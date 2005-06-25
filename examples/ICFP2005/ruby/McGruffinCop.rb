def brain_pick_initial_transport()
    'cop-car'.intern
end

def mgc_sort_locations(locations)
    sorted_locations = []

    $information.each do |cop, information|
        information.each do |info|
            if info.certainty > 0 then
                if locations.include?(info.location) then
                    sorted_locations << info.location
                    #$stderr.puts "preferring #{info.location} because #{cop} is #{info.certainty} certain"
                end
            end
        end
    end
    
    locations.each do |location|
        node = node_for_location(location)
        if node.is_bank? then
            sorted_locations << location
            #$stderr.puts "preferring #{location} because it's a bank"
        end
    end
    
    leftover_locations = locations.dup
    leftover_locations.delete_if do |location|
        sorted_locations.include?(location) || $current_location == location
    end
    leftover_locations.shuffle!
    
    sorted_locations += leftover_locations
end

def mgc_pick_next_location(location, transport)
    locations = reachable_locations(location, transport)
    locations = mgc_sort_locations(locations)
    
    weighted_locations = []
    locations.each_with_index do |location, i|
        weighted_locations += [location] * (locations.size - i)
    end
    weighted_locations[rand(weighted_locations.size)]
end

def brain_pick_next_location()
    mgc_pick_next_location($current_location, $transport)
end

def brain_make_informs()
    []
end

def brain_make_plans()
    $cops.collect do |cop|
        Plan.new(
            cop,
            mgc_pick_next_location($player_locations[cop],
                                   $player_transport[cop]),
            $player_transport[cop],
            $world_number + 1)
    end
end

def brain_make_votes()
    $cops
end
