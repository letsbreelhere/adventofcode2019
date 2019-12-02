# frozen_string_literal: true

class Intcode
  attr_reader :ints, :index, :halt

  UnknownOpcode = Struct.new(:opcode)

  def initialize(ints)
    @ints = ints.dup
    @index = 0
    @halt = false
  end

  def opcode
    @ints[@index]
  end

  def step
    if ints.length - index > 2
      operand1 = ints[ints[index + 1]]
      operand2 = ints[ints[index + 2]]
      result_index = ints[index + 3]
    end

    case opcode
    when 1
      @ints[result_index] = operand1 + operand2
    when 2
      @ints[result_index] = operand1 * operand2
    when 99
      @halt = true
    else
      raise UnknownOpcode, opcode
    end

    @index += 4
  end

  def run
    step until halt
    self
  end
end

def output(ints)
  Intcode.new(ints).run.ints[0]
end

def output_with_noun_and_verb(ints, noun, verb)
  new_ints = ints.dup
  new_ints[1] = noun
  new_ints[2] = verb
  Intcode.new(new_ints).run.ints[0]
end

input = File.read('2.txt').split(',').map(&:to_i)

p output_with_noun_and_verb(input, 12, 2)

def find_output(input, output)
  max_index = input.length - 1

  (0..max_index).each do |noun|
    (0..max_index).each do |verb|
      if output_with_noun_and_verb(input, noun, verb) == output
        return [noun, verb]
      end
    end
  end
end

noun, verb = find_output(input, 19_690_720)
puts noun * 100 + verb
