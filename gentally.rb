#!/usr/bin/env ruby
#
#  Created by Alexy Khrabrov on 2008-12-21.
#  Copyright (c) 2008. All rights reserved.

require 'pp'

fullfile = ARGV[0]
gensfile = ARGV[1]

full = File.open(fullfile).readlines.map{|s|s.split}
a = File.open(gensfile).readlines

gens = []
prefix = []
suffixes = []

def incr h,k
  if h.member? k
    h[k] += 1
  else
    h[k] = 1
  end
end

def suffix_length suffixes
  suffixes[0].length
end

def prefix_length full,suffixes
end

def choices full,suffixes
  suflen = suffix_length suffixes
  pos = (1..suflen).to_a.map{Hash.new}
  hit      = [0] * suflen
  sum_hits = [0] * (suflen+1)

  suffixes.each_with_index do |suffix,i|
    hits = 0
    preflen = full.length - suflen
    suffix.each_with_index do |word,j|
      incr pos[j],word
      if full[preflen+j] == word
        hits += 1
        hit[j] += 1
      end
    end
    sum_hits[hits] += 1
  end
  [pos,hit,sum_hits]
end

a << ""
a.each do |line|
  if line =~ /^#/
    next
  end
  # giving prefix [|60 62 3 61 60|] and length 10...
  if line == "" or line =~ /giving prefix \[\|([0-9 ]+)\|\]/
    if prefix != [] 
      gens << [prefix,suffixes]
      suffixes = []
    end
    if line != ""
      numstr = $1
      nums = numstr.scan /[0-9]+/ # or just split
      puts (nums.join ", ")
      prefix = nums
      next
    end
  end
  nums = line.scan /[0-9]+/
  suffixes << nums
end

gens.each_with_index do | (prefix, suffixes), i |
  suflen = suffix_length suffixes
  preflen = full[i].length - suflen
  pos,hit,sum_hits = choices (full[i],suffixes)
  pos_lens = pos.map {|h| h.size}
  puts "sample: #{prefix.join ' '}\n||\t\t#{full[i][preflen..full.length].join "\t"}"
  puts "choices: \t#{pos_lens.join "\t"}"
  puts "poshits: \t#{hit.join "\t"}"
  puts "sumhits: #{sum_hits.join "\t"}"
  puts
end
