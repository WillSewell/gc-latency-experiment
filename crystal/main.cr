WINDOW_SIZE =  200000
MSG_COUNT   = 1000000

alias Message = Array(UInt8)
alias Store = Array(Message)

def mk_message(n : UInt8) : Message
  m = Message.new(1024, n)
end

def push_msg(s : Store, id : Int32) : Time::Span
  elapsed_time = Time.measure do
    m = mk_message((id % UInt8::MAX).to_u8)
    s[id % WINDOW_SIZE] = m
  end

  elapsed_time
end

worst = Time::Span.zero
store = Store.new(WINDOW_SIZE) { Message.new(1024) { 0u8 } }

MSG_COUNT.times do |i|
  elapsed = push_msg(store, i)
  if elapsed > worst
    worst = elapsed
  end
end

puts "Worst push time: #{worst.total_milliseconds}ms"
