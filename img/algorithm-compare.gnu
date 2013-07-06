set terminal postscript eps enhanced color "Times-Roman" 20 
set output "algorithm-compare.ps"
set termopt enhanced

set xtics nomirror scale 0
set xtics font "Times-Roman,20"
set ytics font "Times-Roman,20"
set ytics ("0" 0, "2" 0.002, "4" 0.004, "6" 0.006, "8" 0.008)
set yrange [0:0.008]
#set log y
#set border 3
set style data histogram
set style fill solid border -1


set key font ",15"
unset key

set label "unboxed\nvector" at graph 0.1, 0.21 tc rgb "#0000ff" font ",20" center
set arrow from graph 0.1, 0.12 to graph 0.18,0.08 lc rgb "#0000ff"

set label "boxed \nvector" at graph 0.15, 0.51 tc rgb "#ff0000" font ",20" center
set arrow from graph 0.15, 0.42 to graph 0.22,0.28 lc rgb "#ff0000"

set label "functor-lazy vector" at graph 0.4, 0.95 tc rgb "#007700" font ",20" left
set arrow from graph 0.38, 0.95 to graph 0.32,0.945 lc rgb "#007700"

set label "functor-lazy vector, but fmap \nwas applied before sorting" at graph 0.43, 0.75 tc rgb "#007700" font ",20" left
set arrow from graph 0.41, 0.73 to graph 0.37,0.725 lc rgb "#007700"

#set xlabel "sorting algorithm"
set ylabel "run time on 10000 elements (msec)

plot "algorithm-compare.dat" using 2:xtic(1) ti col linecolor rgb "#0000ff" lt 1, '' u 3 ti col lt 1 linecolor rgb "#ff0000" , '' u 4 ti col lc rgb "#007700" lt 1, '' u 5 ti col lc rgb "#007700" fill pattern 2 
