WINDOW_SIZE = 200_000
MSG_COUNT   = 1_000_000

$worst = 0

def mk_message(n)
  (n % 255).chr('BINARY') * 1024
end

def push_msg(channel, high_id)
  start = Time.new
  m = mk_message(high_id)
  channel[high_id % WINDOW_SIZE] = m
  elapsed = Time.new - start
  $worst = elapsed if elapsed > $worst
end

channel = []
MSG_COUNT.times do |i|
  push_msg(channel, i)
end
puts "Worst push time: #{$worst}"
