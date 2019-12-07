# frozen_string_literal: true

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
      adj[child] = parent
    end

    new(adj)
  end

  def height(node)
    parents(node).length
  end

  def nodes
    edges.keys
  end

  def distance_between(l, r)
    lub = least_upper_bound(l, r)
    height_to(lub, l) + height_to(lub, r)
  end

  private

  def least_upper_bound(l, r)
    l_parents = parents(l)
    r_parents = parents(r)
    height = [l_parents.length, r_parents.length].max
    visited = Set.new

    height.times do |i|
      l = l_parents[i]
      r = r_parents[i]

      return l if visited.include?(l)

      visited << l if l

      return r if visited.include?(r)

      visited << r if r
    end
  end

  def height_to(parent, child)
    parents(child).index(parent)
  end

  def parents(node)
    return @parents[node] if @parents[node]

    parent = edges[node]
    @parents[node] = if parent
                       [node] + parents(parent)
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
