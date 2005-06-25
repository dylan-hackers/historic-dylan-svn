def pick_next_location()
    locations = reachable_locations($current_location, :robber)
    locations = locations.collect do |location|
        node_for_location(location)
    end
    locations.each do |location|
        if location.is_bank? then
            return location.name
        end
    end
    locations[rand(locations.size)].name
end
