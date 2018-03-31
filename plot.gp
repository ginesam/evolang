#!/usr/bin/gnuplot

set style line 1 lt 1 lc rgb '#0025ad' lw 2
set style line 2 lt 2 lc rgb '#00ada4' lw 2
set style line 3 lt 1 lc rgb '#00ad31' lw 2
set style line 4 lt 2 lc rgb '#9acd32' lw 2

set key right bottom

set terminal unknown
plot "./plot/adopt.txt" using (step*$0):1

set terminal pngcairo dashed size 1024,768 enhanced font 'Verdana,10'

set xlabel "Number of games played"
set ylabel "Ratio communicative success and coherence"
set y2label "Average times invention and adoption"

set xrange[GPVAL_DATA_X_MIN:GPVAL_DATA_X_MAX]
set yrange[0:1.05]
set y2range[floor(GPVAL_DATA_Y_MIN):GPVAL_DATA_Y_MAX*1.75]

set xtics GPVAL_DATA_X_MIN, int(GPVAL_DATA_X_MAX/15), GPVAL_DATA_X_MAX
set ytics nomirror 0, 0.1, 1.05
set y2tics floor(GPVAL_DATA_Y_MIN), int(GPVAL_DATA_Y_MAX/5), GPVAL_DATA_Y_MAX*1.75

set output "./plot/evol.png"
plot "./plot/evol_com.txt" using (step*$0):1 axes x1y1 with line ls 1 title 'success', '' using (step*$0):2 axes x1y1 with line ls 2 title 'coherence', "./plot/adopt.txt" using (step*$0):1 axes x1y2 with line ls 3 title 'adoption', "./plot/invent.txt" using (step*$0):1 axes x1y2 with line ls 4 title 'invention'

