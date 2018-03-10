#!/bin/bash


STRING1="^Some information for this report"
STRING="Arial"
STRING="*"

search()
{
    for i in `seq 100 199 | cut -c2-3`; do 
	for j in `ls /net/gsb/gsb/raw/p${i}/`; do 
	    N=`cat /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep "${STRING}" | wc -l`
            if [ $N -gt 0 ]; then
		echo -n "$i/$j ($N)"
	        cat /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep "${STRING} | head -1"
	    fi
	done
    done
}

search2()
{
    for i in `seq 100 199 | cut -c2-3`; do 
	echo -n "part $i -"
	for j in `ls /net/gsb/gsb/raw/p${i}/`; do 
	    if [ `cat /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep "${STRING}" | wc -l` == "1" ]; then
		LEN=`cat /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep . | wc -l`
		for TAIL in `seq 8 ${LEN}`; do
		    N=`cat /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep . | tail -${TAIL} | head -1 | grep "${STRING}" | wc -l`
		    if [ $N == "1" ]; then
			echo "$i/$j (tail=${TAIL}): "
			cat -e /net/gsb/gsb/raw/p${i}/${j}/en.raw | grep . | tail -${TAIL}
			echo "-------"
		    fi
		done
	    fi
	done
    done
}

search

exit 0
