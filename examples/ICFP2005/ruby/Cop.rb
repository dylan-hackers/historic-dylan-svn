#!/usr/bin/env ruby -w

$: << File.dirname($0)

require 'Common'
require 'World'

def send_register_message()
    $transport = brain_pick_initial_transport()
    send_message "reg: #{$brain} #{$transport}"
end

def receive_world_skeleton()
    read_world_skeleton($stdin)
    
    $current_location = cop_hq().name
end

def send_inform()
    send_message <<END_OF_INFORM
inf\\
inf/
END_OF_INFORM
end

def receive_informs()
    line = $stdin.gets.chomp
    if line != 'from\\' then
        raise 'malformed inform -- missing from group'
    end
    
    line = $stdin.gets.chomp
    while line != 'from/' do
        line =~ /from:\s(\S+)/ or raise('malformed from')
        bot = $1
        
        line = $stdin.gets.chomp
        while line != 'inf/' do
            line = $stdin.gets.chomp
        end
        
        line = $stdin.gets.chomp
    end
end

def send_plan()
    send_message <<END_OF_PLAN
plan\\
plan/
END_OF_PLAN
end

def receive_plans()
    line = $stdin.gets.chomp
    if line != 'from\\' then
        raise 'malformed plan -- missing from group'
    end
    
    line = $stdin.gets.chomp
    while line != 'from/' do
        line =~ /from:\s(\S+)/ or raise('malformed from')
        bot = $1
        
        line = $stdin.gets.chomp
        while line != 'plan/' do
            line = $stdin.gets.chomp
        end
        
        line = $stdin.gets.chomp
    end
end

def send_vote()
    send_message("vote\\\n" + $cops.collect do |cop|
        "vote: #{cop}"
    end.join("\n") + "\nvote/")
end

def receive_votes()
    line = $stdin.gets.chomp
    if line =~ /winner:\s(\S+)/
    
    elsif line != 'nowinner' then
        raise 'malformed vote result'
    end
end

def send_move_message()
    $current_location = brain_pick_next_location()
    send_message "mov: #{$current_location} #{$transport}"
    
end

ACTIONS = {
    :initial => Proc.new do
        send_register_message()
        $current_state = :waiting_for_world
    end,
    :waiting_for_world => Proc.new do
        receive_world_skeleton()
        $current_state = :waiting_for_turn
    end,
    :waiting_for_turn => Proc.new do
        if receive_world_message() then
            $current_state = :inform
        else
            $current_state = :game_over
        end
    end,
    :inform => Proc.new do
        send_inform()
        $current_state = :waiting_for_inform
    end,
    :waiting_for_inform => Proc.new do
        receive_informs()
        $current_state = :planning
    end,
    :planning => Proc.new do
        send_plan()
        $current_state = :waiting_for_plans
    end,
    :waiting_for_plans => Proc.new do
        receive_plans()
        $current_state = :voting
    end,
    :voting => Proc.new do
        send_vote()
        $current_state = :waiting_for_results
    end,
    :waiting_for_results => Proc.new do
        receive_votes()
        $current_state = :moving
    end,
    :moving => Proc.new do
        send_move_message()
        $current_state = :waiting_for_turn
    end,
    :game_over => Proc.new do
        exit(0)
    end
}

$current_state = :initial
$current_location = nil
$bank_values = {}
$player_locations = {}
$player_transport = {}
$evidence = {}

$brain = ARGV[0]
require $brain

loop do
    ACTIONS[$current_state].call
end
