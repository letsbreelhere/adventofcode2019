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
    mode = opcode / 10

    if ints.length - ip > 3
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
