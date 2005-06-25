#!/usr/bin/env ruby -w

$: << File.dirname($0)

require 'Common'
require 'World'

def send_register_message()
    send_message "reg: #{$brain} robber"
end

def receive_world_skeleton()
    read_world_skeleton($stdin)
    
    $current_location = robber_start().name
    #$stderr.puts $cops
    #$stderr.puts $nodes
    #$stderr.puts $edges
end

def send_move_message()
    $current_location = brain_pick_next_location()
    send_message "mov: #{$current_location} robber"
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
            $current_state = :moving
        else
            $current_state = :game_over
        end
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
