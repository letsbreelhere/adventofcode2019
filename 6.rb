# frozen_string_literal: true

require 'pry'

def build_tree(edges)
  tree = {}
  edges.each do |parent, child|
    tree[parent] ||= []
    tree[parent] << child
  end

  tree
end

def parents(tree, node)
  parent = tree.find { |_, c| c.include?(node) }
  if parent
    [node] + parents(tree, parent[0])
  else
    []
  end
end

@heights = {}
def height(tree, node)
  return @heights[node] if @heights[node]

  parent = tree.find { |_, c| c.include?(node) }
  @heights[node] = if parent
                     1 + height(tree, parent[0])
                   else
                     0
  end
end

def height_to(tree, parent, child)
  return 0 if child == parent

  p = tree.find { |_, c| c.include?(child) }

  if p
    1 + height_to(tree, parent, p[0])
  else
    0
 end
end

def distance_between(tree, l, r)
  l_parents = parents(tree, l)
  r_parents = parents(tree, r)
  lub = (l_parents & r_parents)[0]
  l_h = height_to(tree, lub, l)
  r_h = height_to(tree, lub, r)
  l_h + r_h - 2
end

# Part 1
tree = build_tree(File.read('6.txt').split("\n").map { |line| line.split(')') })
nodes = (tree.values.flatten + tree.keys).uniq
p nodes.map { |p| height(tree, p) }.sum

# Part 2
puts distance_between(tree, 'YOU', 'SAN')
