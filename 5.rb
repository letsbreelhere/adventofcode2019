# frozen_string_literal: true

require_relative 'intcode'

input = File.read('5.txt').split(',').map(&:to_i)

# Part 1
computer = Intcode.new(input, inputs: [1])
computer.run
p computer.outputs

# Part 1
computer = Intcode.new(input, inputs: [5])
computer.run
p computer.outputs
