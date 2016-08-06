#!/usr/bin/env ruby

hour_counter = 0
STDIN.each do |line|
  fields = line.chomp.split(',')
  if hour_counter == 0
    fields.shift
    puts ["#hour_counter", "day", "hour", fields].flatten.join("\t")
  else
    moment = fields.shift
    moment.sub!(/^ /,'')
    day, hour = moment.split(/ +/)
    puts [hour_counter, day, hour, fields].flatten.join("\t")
  end
  hour_counter += 1
end
