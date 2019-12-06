# frozen_string_literal: true

require 'pry'
require 'set'

class Tree
  attr_reader :edges

  def initialize(edges)
    @edges = edges
    @parents = {}
  end

  def self.from_edges(edges)
    adj = {}
    edges.each do |parent, child|
      adj[parent] ||= []
      adj[parent] << child
    end

    new(adj)
  end

  def height(node)
    parents(node).length
  end

  def nodes
    (edges.values.flatten + edges.keys).uniq
  end

  def distance_between(l, r)
    lub = least_upper_bound(l, r)
    height_to(lub, l) + height_to(lub, r)
  end

  private

  def least_upper_bound(l, r)
    r_parents = Set.new(parents(r))
    parents(l).find { |p| r_parents.include?(p) }
  end

  def height_to(parent, child)
    parents(child).index(parent)
  end

  def parents(node)
    return @parents[node] if @parents[node]

    parent = edges.find { |_, c| c.include?(node) }
    @parents[node] = if parent
                       [node] + parents(parent.first)
                     else
                       []
                     end
  end
end

tree = Tree.from_edges(
  File.read('6.txt').split("\n").map { |line| line.split(')') }
)

# Part 1
puts tree.nodes.map { |p| tree.height(p) }.sum
# Part 2
puts tree.distance_between('YOU', 'SAN') - 2
