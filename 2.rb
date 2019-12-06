# frozen_string_literal: true

require_relative 'intcode'

def output_with_noun_and_verb(ints, noun, verb)
  new_ints = ints.dup
  new_ints[1] = noun
  new_ints[2] = verb
  Intcode.new(new_ints).output
end

input = File.read('2.txt').split(',').map(&:to_i)

p output_with_noun_and_verb(input, 12, 2)

def find_output(input, output)
  max_index = input.length - 1

  (0..99).each do |noun|
    (0..99).each do |verb|
      if output_with_noun_and_verb(input, noun, verb) == output
        return [noun, verb]
      end
    end
  end
end

noun, verb = find_output(input, 19_690_720)
puts noun * 100 + verb
