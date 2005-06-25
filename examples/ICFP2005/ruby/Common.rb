def send_message(message)
    $stdout.puts(message)
    $stdout.flush
end

def receive_world_message()
    line = $stdin.gets.chomp
    if line == 'game-over' then
        false
    else
        if line != 'wor\\' then
            raise 'malformed state -- missing world group'
        end
        read_state($stdin)
        true
    end
end

