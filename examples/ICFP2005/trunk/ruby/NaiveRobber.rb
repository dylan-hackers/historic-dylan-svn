def brain_pick_next_location()
    bad_locations = locations_cops_can_smell()
    reachable = reachable_locations($current_location, :robber)
    locations = reachable.dup
    
    locations.delete_if do |location|
        bad_locations.include?(location)
    end
    
    locations = locations.collect do |location|
        node_for_location(location)
    end
    
    locations.each do |location|
        if location.is_bank? then
            return location.name
        end
    end
    
    locations.delete(node_for_location($current_location))
    
    if locations.size == 0 then
        locations = reachable
        locations = locations.collect do |location|
            node_for_location(location)
        end
    end
    
    locations[rand(locations.size)].name
end
