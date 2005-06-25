def brain_pick_initial_transport()
    'cop-car'.intern
end

def mgc_pick_next_location(location, transport)
    locations = reachable_locations(location, transport)
    locations = locations.collect do |loc|
        node_for_location(loc)
    end
    locations[rand(locations.size)].name
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
