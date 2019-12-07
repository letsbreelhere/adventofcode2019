# frozen_string_literal: true

class Intcode
  attr_reader :ints, :ip, :halt, :waiting, :inputs
  attr_accessor :outputs

  def initialize(ints, inputs: [], debug: false)
    @ints = ints.dup
    @ip = 0
    @halt = false
    @waiting = false
    @inputs = inputs
    @outputs = []
    @debug = debug
  end

  def run
    step until halt
  end

  def run_until_wait
    @waiting = false
    step until waiting || halt
  end

  # Only used for day 2
  def output
    run
    ints[0]
  end

  def take_output
    @outputs.shift
  end

  def add_input(value)
    @inputs << value
  end

  private

  def advance
    @ip += parameter_length + 1
  end

  def mode
    ints[ip].to_s.gsub(/.?.$/, '').reverse
  end

  def set(value)
    result_index = ints[ip + parameter_length]
    @ints[result_index] = value
  end

  def opcode
    ints[ip] % 100
  end

  def parameter_length
    case opcode
    when 1, 2, 7, 8
      3
    when 5, 6
      2
    when 3, 4
      1
    when 99
      0
    else
      raise "Unknown opcode #{opcode}"
    end
  end

  def step
    debug "opcode:#{opcode}, modes:#{mode}, ip:#{ip}"
    debug "IN: #{@inputs}; OUT: #{@outputs}"
    debug ''

    operands = (1..parameter_length).map do |i|
      operand_mode = mode[i - 1] || '0'
      parameter = ints[ip + i]

      case operand_mode
      when '0'
        ints[parameter]
      when '1'
        parameter
      else
        raise "Unknown address mode #{operand_mode}"
      end
    end

    debug "operands: #{operands.inspect}"

    case opcode
    when 1
      set(operands[0] + operands[1])
    when 2
      set(operands[0] * operands[1])
    when 3
      input = read_input
      if input
        set(input)
      else
        debug 'Waiting on input'
        return
      end
    when 4
      @outputs += operands
    when 5
      if operands[0] != 0
        @ip = operands[1]
        return
      end
    when 6
      if operands[0] == 0
        @ip = operands[1]
        return
      end
    when 7
      set(operands[0] < operands[1] ? 1 : 0)
    when 8
      set(operands[0] == operands[1] ? 1 : 0)
    when 99
      @halt = true
    else
      raise "Unknown opcode #{opcode}, ip: #{ip}"
    end

    advance
  end

  def read_input
    res = @inputs.shift
    @waiting = !res

    res
  end

  def debug(str)
    puts str if @debug
  end
end
