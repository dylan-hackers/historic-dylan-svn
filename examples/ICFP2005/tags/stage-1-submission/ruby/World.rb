BOLD = '' << 27 << '[1m'
RESET = '' << 27 << '[0m'

class Node
    attr_reader :name

    def initialize(string)
        string =~ /nod:\s(\S+)\s(\S+)\s(\d+)\s(\d+)/ or raise('malformed node')
        @name = $1.intern
        @type = $2.intern
        @x = $3.to_i
        @y = $4.to_i
        
        @@robber_start = 'robber-start'.intern
    end
    
    def to_s
        "node name #{BOLD}#{@name}#{RESET} type #{BOLD}#{@type}#{RESET}"
    end
    
    def is_robber_start?()
        @type == @@robber_start
    end
    
    def is_bank?()
        @type == :bank
    end
    
    def is_hq?()
        @type == :hq
    end
end

class Edge
    attr_reader :from, :to, :edge_type

    def initialize(string)
        string =~ /edg:\s(\S+)\s(\S+)\s(\S+)/ or raise('malformed edge')
        @from = $1.intern
        @to = $2.intern
        @edge_type = $3.intern
    end
    
    def to_s
        "edge from #{BOLD}#{@from}#{RESET} to #{BOLD}#{@to}#{RESET} type #{BOLD}#{@edge_type}#{RESET}"
    end
end

def reachable_locations(location, transport)
    locations = [location]

    $edges.each do |edge|
        if transport == 'cop-car'.intern then
            next unless edge.from == location
            locations << edge.to
        else
            next unless edge.from == location || edge.to == location
            next if edge.edge_type == :car
            
            if edge.from == location then
                locations << edge.to
            else
                locations << edge.from
            end
        end
        
        locations << edge.to
    end
    
    locations
end

def locations_cops_can_smell()
    locations = []

    $cops.each do |cop|
        location = $player_locations[cop]
        transport = $player_transport[cop]
        
        locations << location
        
        reachable = reachable_locations(location, transport)
        if transport == 'cop-car'.intern then
            locations += reachable
        else
            locations += reachable.collect do |new_location|
                reachable_locations(new_location, transport)
            end.flatten + reachable
        end
    end
    
    locations.uniq
end

def node_for_location(location)
    $nodes.each do |node|
        if node.name == location
            return node
        end
    end
    
    raise 'no such node'
end

def read_world(stream)
    $nodes = []
    $edges = []

    line = stream.gets.chomp
    if line != 'nod\\' then
        raise 'malformed world -- missing node group'
    end
    line = stream.gets.chomp
    while line =~ /^nod:/ do
        $nodes << Node.new(line)
        line = stream.gets.chomp
    end
    if line != 'nod/' then
        raise 'malformed world -- unclosed node group'
    end
    
    line = stream.gets.chomp
    if line != 'edg\\' then
        raise 'malformed world -- missing edge group'
    end
    line = stream.gets.chomp
    while line =~ /^edg:/ do
        $edges << Edge.new(line)
        line = stream.gets.chomp
    end
    if line != 'edg/' then
        raise 'malformed world -- unclosed edge group'
    end
end

def read_world_skeleton(stream)
    line = stream.gets.chomp
    if line != 'wsk\\' then
        raise 'malformed skeleton -- missing skeleton group'
    end
    
    line = stream.gets.chomp
    if line !~ /name:\s(\S+)/ then
        raise 'malformed skeleton -- missing player name'
    end
    $brain = $1
    
    line = stream.gets.chomp
    if line !~ /robber:\s(\S+)/ then
        raise 'malformed skeleton -- missing robber name'
    end
    $robber = $1
    
    $cops = []
    1.upto(5) do
        line = stream.gets.chomp
        if line !~ /cop:\s(\S+)/ then
            raise 'malformed skeleton -- missing cop name'
        end
        $cops << $1.intern
    end
    
    read_world(stream)
    
    line = stream.gets.chomp
    if line != 'wsk/' then
        raise 'malformed skeleton -- unclosed skeleton group'
    end
end

def robber_start()
    $nodes.each do |node|
        if node.is_robber_start? then
            return node
        end
    end
    raise 'malformed world -- no robber start'
end

def cop_hq()
    $nodes.each do |node|
        if node.is_hq? then
            return node
        end
    end
    raise 'malformed world -- no cop hq'
end

def read_bank_values(stream)
    line = stream.gets.chomp
    if line != 'bv\\' then
        raise 'malformed state -- missing bank value group'
    end
    
    1.upto(6) do
        line = stream.gets.chomp
        line =~ /bv:\s(\S+)\s(\d+)/ or raise('malformed state -- bank value')
        $bank_values[$1.intern] = $2.to_i
    end
    
    line = stream.gets.chomp
    if line != 'bv/' then
        raise 'malformed state -- unclosed bank value group'
    end
end

def read_evidence(stream)
    line = stream.gets.chomp
    if line != 'ev\\' then
        raise 'malformed state -- missing evidence group'
    end
    
    $evidence = {}
    line = stream.gets.chomp
    while line =~ /ev:\s(\S+)\s(\d+)/ do
        $evidence[$1.intern] = $2.to_i
    end
    
    if line != 'ev/' then
        raise 'malformed state -- unclosed evidence group'
    end
end

def read_player_locations(stream)
    line = stream.gets.chomp
    if line != 'pl\\' then
        raise 'malformed state -- missing player group'
    end
    
    $player_locations = {}
    $player_transport = {}
    line = stream.gets.chomp
    while line =~ /pl:\s(\S+)\s(\S+)\s(\S+)/ do
        $player_locations[$1.intern] = $2.intern
        $player_transport[$1.intern] = $3.intern
        line = stream.gets.chomp
    end
    
    if line != 'pl/' then
        raise 'malformed state -- unclosed player group'
    end
end

def read_state(stream)
    line = stream.gets.chomp
    line =~ /wor:\s(\d+)/ or raise('malformed state -- world number')
    $world_number = $1.to_i
    
    line = stream.gets.chomp
    line =~ /rbd:\s(\d+)/ or raise('malformed state -- robbed')
    $robbed = $1.to_i
    
    read_bank_values(stream)
    read_evidence(stream)
    
    line = stream.gets.chomp
    line =~ /smell:\s(\d+)/ or raise('malformed state -- smell')
    $smell_distance = $1.to_i
    
    read_player_locations(stream)
    
    line = stream.gets.chomp
    if line != 'wor/' then
        raise 'malformed state -- unclosed world group'
    end
end
