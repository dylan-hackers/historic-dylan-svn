#!/usr/bin/env ruby -w

$: << File.dirname($0)

require 'Common'
require 'World'

def send_register_message()
    send_message "reg: #{$brain} #{brain_pick_initial_transport()}"
end

def receive_world_skeleton()
    read_world_skeleton($stdin)
    
    $current_location = cop_hq().name
end

def send_inform()

end

def receive_informs()

end

def send_plan()

end

def receive_plans()

end

def send_vote()

end

def receive_votes()

end

def send_move_message()

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
