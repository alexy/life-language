#!/bin/zsh
points=$1
echo doing $points points
setopt extendedglob
# grep -v 0 # 0 is the overall list
print -l -- *~*[^0-9]* | sort -n | grep -v "^0$" | while read dir; do
 cd $dir
 echo -n "in $PWD: "
 echo "~/ht/ngram/fertilize $points $dir > $dir.fert"
 ~/ht/ngram/fertilize $points $dir > $dir.fert
 cd ..
done