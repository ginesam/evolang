#!/bin/sh
# usage: ./run.sh [runs] [mode] [iterations] [step] [agents]

i=1
t=`expr $5 / 3 \* 2`
make all
mkdir run_
ev_files=""
ad_files=""
in_files=""

while [ $i -le $1 ]
do
	mkdir run
	/bin/sh ini_population.sh $5
	sleep $t
	if [ "$2" = "emerge" ]; then
		./language_game $3 $5 $4 &
		SIM_PID=$!
		wait $SIM_PID
	fi;
	if [ "$2" = "trans" ]; then
		./language_game_trans $3 $5 $4 &
		SIM_PID=$!
		wait $SIM_PID
	fi;
	mv run "run_/$i"
	pkill -f xterm &
	END_PID=$!
	wait $END_PID

	ev_files="$ev_files run/$i/evol_com.txt"
	
	j=1
	while [ $j -le $5 ]
	do
		ad_files="$ad_files run/$i/ad_$j.txt"
		in_files="$in_files run/$i/inv_$j.txt"
		j=`expr $j + 1`
	done
	i=`expr $i + 1`
done

mv run_ run
mkdir plot
paste -d" " $ev_files | awk -v R=$1 '
BEGIN { for (i = 1; i <= 2; i++) total[i] = 0 }
{
    for (i = 1; i <= 2*R; i++){
	ind = ((i-1)%2)+1
	total[ind] += $i
    }
    for (i = 1; i <= 2; i=i+2){
	print total[i]/R" "total[i+1]/R
	total[i] = 0
        total[i+1] = 0
    }
}' > plot/evol_com.txt

paste -d" " $ad_files | awk -v R=$1 -v Ag=$5 '
BEGIN { for (i = 1; i <= R; i++) total[i] = 0 }
{
    for (i = 1; i <= NF; i++){
	ind = int((i-1)/Ag)+1
	total[ind] += $i
    }
    sum = 0
    for (i = 1; i <= R; i++){
	sum = sum + (total[i]/Ag)
	total[i] = 0
    }
    print sum/R
    sum = 0
}' > plot/adopt.txt

paste -d" " $in_files | awk -v R=$1 -v Ag=$5 '
BEGIN { for (i = 1; i <= R; i++) total[i] = 0 }
{
    for (i = 1; i <= NF; i++){
	ind = int((i-1)/Ag)+1
	total[ind] += $i
    }
    sum = 0
    for (i = 1; i <= R; i++){
	sum = sum + (total[i]/Ag)
	total[i] = 0
    }
    print sum/R
    sum = 0
}' > plot/invent.txt
gnuplot -e "step=$4" plot.gp

