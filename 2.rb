# frozen_string_literal: true

class Intcode
  attr_reader :ints, :ip, :halt

  UnknownOpcode = Struct.new(:opcode)

  def initialize(ints)
    @ints = ints.dup
    @ip = 0
    @halt = false
  end

  def opcode
    @ints[@ip]
  end

  def step
    if ints.length - ip > 2
      operand1 = ints[ints[ip + 1]]
      operand2 = ints[ints[ip + 2]]
      result_ip = ints[ip + 3]
    end

    case opcode
    when 1
      @ints[result_ip] = operand1 + operand2
      @ip += 4
    when 2
      @ints[result_ip] = operand1 * operand2
      @ip += 4
    when 99
      @halt = true
    else
      raise UnknownOpcode, opcode
    end
  end

  def output
    step until halt
    ints[0]
  end
end

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
