#!/usr/bin/env ruby
#
#  Created by Alexy Khrabrov on 2008-12-21.
#  Copyright (c) 2008. All rights reserved.

require 'pp'

fullfile = ARGV[0]
gensfile = ARGV[1]

full = File.open(fullfile).readlines.map{|s|s.split}
#puts full.join ','
a = File.open(gensfile).readlines

gens = []
prefix = []
suffixes = []

def incr h,k
  if h.member? k
    h[k] += 1
    #puts "++ #{k}"
  else
    h[k] = 1
    #puts "new #{k}"
  end
end

def choices full,suffixes
  suflen = suffixes[0].length
  pos = (1..suflen).to_a.map{Hash.new}
  hit      = [0] * suflen
  sum_hits = [0] * (suflen+1)

  suffixes.each_with_index do |suffix,i|
    hits = 0
    #puts "i => #{i}"
    preflen = full.length - suflen
    suffix.each_with_index do |word,j|
      #puts "i #{i}, word #{word}"
      incr pos[j],word
      if full[preflen+j] == word
        hits += 1
        hit[j] += 1
      end
    end
    sum_hits[hits] += 1
    #pp pos
    #lens = pos.map {|h| h.size}
    #puts "choices: #{lens.join ','}"
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
  #puts "prefix [| #{prefix.join ' '} |]: #{suffixes.length} suffixes"
  pos,hit,sum_hits = choices (full[i],suffixes)
  pos_lens = pos.map {|h| h.size}
  puts "number of choices in each suffix position: #{pos_lens.join ' '}"
  puts "number of hits in each suffix position: #{hit.join ' '}"
  puts "total number of suffixes with a given number of hits: #{sum_hits.join ' '}"
end
