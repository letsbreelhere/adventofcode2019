# frozen_string_literal: true

require_relative 'intcode'

def output_with_phases(input, phases)
  amps = phases.map do |phase|
    Intcode.new(input, inputs: [phase])
  end

  res = 0
  amps.each do |amp|
    amp.add_input(res)
    amp.run
    res = amp.outputs.first
  end

  res
end

def feedback_output(input, phases)
  amps = phases.map do |phase|
    Intcode.new(input, inputs: [phase])
  end

  res = 0
  ix = 0
  until amps.last.halt
    amp = amps[ix]
    amp.add_input(res) if res
    amp.run_until_wait
    res = amp.take_output
    ix = (ix + 1) % phases.length
  end

  res
end

input = File.read('7.txt').split(',').map(&:to_i)

# Part 1
guesses = (0..4).to_a.permutation.map do |phases|
  output_with_phases(input, phases)
end
puts guesses.max

# Part 2
guesses = (5..9).to_a.permutation.map do |phases|
  feedback_output(input, phases)
end
puts guesses.max
