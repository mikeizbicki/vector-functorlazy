set terminal postscript "Times-Roman" 25
set output "functorlazy-v-boxed.ps"
set termopt enhanced

set xtics font "Times-Roman,20" nomirror
set ytics font "Times-Roman,20" nomirror

set log x
set log y
set xrange [1e0:1e6]
set yrange [1e-8:10e-2]
#set grid mytics lt 1 ls 1, lt 2 ls 2
#set grid mxtics lt 1 ls 1, lt 2 ls 2
set grid xtics lt 1, mxtics, ytics lt 1, mytics
set ytics ("10^0" 1e0, \
    "10^{-1}" 1e-1, "" 2e-2 1,"" 3e-2 1,"" 4e-2 1,"" 5e-2 1,"" 6e-2 1,"" 7e-2 1,"" 8e-2 1,"" 9e-2 1, \
    "10^{-2}" 1e-2, "" 2e-3 1,"" 3e-3 1,"" 4e-3 1,"" 5e-3 1,"" 6e-3 1,"" 7e-3 1,"" 8e-3 1,"" 9e-3 1, \
    "10^{-3}" 1e-3, "" 2e-4 1,"" 3e-4 1,"" 4e-4 1,"" 5e-4 1,"" 6e-4 1,"" 7e-4 1,"" 8e-4 1,"" 9e-4 1, \
    "10^{-4}" 1e-4, "" 2e-5 1,"" 3e-5 1,"" 4e-5 1,"" 5e-5 1,"" 6e-5 1,"" 7e-5 1,"" 8e-5 1,"" 9e-5 1, \
    "10^{-5}" 1e-5, "" 2e-6 1,"" 3e-6 1,"" 4e-6 1,"" 5e-6 1,"" 6e-6 1,"" 7e-6 1,"" 8e-6 1,"" 9e-6 1, \
    "10^{-6}" 1e-6, "" 2e-7 1,"" 3e-7 1,"" 4e-7 1,"" 5e-7 1,"" 6e-7 1,"" 7e-7 1,"" 8e-7 1,"" 9e-7 1, \
    "10^{-7}" 1e-7, "" 2e-8 1,"" 3e-8 1,"" 4e-8 1,"" 5e-8 1,"" 6e-8 1,"" 7e-8 1,"" 8e-8 1,"" 9e-8 1, \
    "10^{-8}" 1e-8)
set xtics (\
    "10^0" 1e0, "" 2e0 1,"" 3e0 1,"" 4e0 1,"" 5e0 1,"" 6e0 1,"" 7e0 1,"" 8e0 1,"" 9e0 1, \
    "10^1" 1e1, "" 2e1 1,"" 3e1 1,"" 4e1 1,"" 5e1 1,"" 6e1 1,"" 7e1 1,"" 8e1 1,"" 9e1 1, \
    "10^2" 1e2, "" 2e2 1,"" 3e2 1,"" 4e2 1,"" 5e2 1,"" 6e2 1,"" 7e2 1,"" 8e2 1,"" 9e2 1, \
    "10^3" 1e3, "" 2e3 1,"" 3e3 1,"" 4e3 1,"" 5e3 1,"" 6e3 1,"" 7e3 1,"" 8e3 1,"" 9e3 1, \
    "10^4" 1e4, "" 2e4 1,"" 3e4 1,"" 4e4 1,"" 5e4 1,"" 6e4 1,"" 7e4 1,"" 8e4 1,"" 9e4 1, \
    "10^5" 1e5, "" 2e5 1,"" 3e5 1,"" 4e5 1,"" 5e5 1,"" 6e5 1,"" 7e5 1,"" 8e5 1,"" 9e5 1, \
    "10^6" 1e6)
#set border 3
unset key

set xlabel "size of vector (n)"
set ylabel "run time of fmap (sec)"

set label "boxed vector" at 140,0.000175 tc rgb "#ff0000" center font "Times-Roman-bold"
set arrow from 160,0.00010 to 1000,0.0000145 lc rgb "#ff0000"

set label "functor-lazy vector" at 14000,0.000000175 tc rgb "#007700" center font "Times-Roman-bold"
set arrow from 14000,0.00000011 to 30000,0.000000032 lc rgb "#007700"

#set label "group" at 255,0.045 tc rgb "#007700" center
#set arrow from 255,0.031 to 275,0.01 lc rgb "#007700"

plot "functorlazy-v-boxed.dat" using (2**($1)):2 lc rgb "#ff0000" lt 1 lw 8 with lines,\
     "functorlazy-v-boxed.dat" using (2**($1)):3 lc rgb "#007700" lt 1 lw 8 with lines
