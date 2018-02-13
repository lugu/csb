#!/usr/bin/gnuplot --persist
# set style line 1 lt 1 lw 2 pt 7 pi -1 ps 1.5
set pointintervalbox 3
plot 'fitness.dat' using 1:2 with linespoints ls 1 lc rgb '#0060ad' title 'training set' , '' using 1:3 with linespoints ls 1 lc rgb '#6000ad' title 'test set
