#!/bin/bash

SET=$1
RTE=working/rte/${SET}
MOD=working/rte.mod
ARF1=working/WordNetTask.arff
ARF2=working/ModelsTask.arff
ARF3=working/Overlap.arff
ARF1simple=working/WordNet.arff
ARF2simple=working/Models.arff
ARF3simple=working/ModelsOnly.arff

train()
{
    RTE=$1
    for i in `ls ${RTE}/ | sort -n`; do
	echo -n "$i "
	if [ -f ${RTE}/$i/task.txt ]; then
	    TASK=`cat ${RTE}/$i/task.txt`
	else
	    TASK="UNKNOWN"
	fi
	if [ -f ${RTE}/$i/modsizedif.txt ]; then
	    if [ `cat ${RTE}/$i/prediction.txt | grep '(model)' | wc -l` -ge 1 ]; then
		echo -n "mod(${SET},'${TASK}',$i,`cat ${RTE}/$i/gold.txt`, " >> $MOD
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'domain novelty' | cut -d' ' -f1 | sed 's/.$//'`, " >> $MOD
		echo    "`cat ${RTE}/$i/modsizedif.txt | grep 'relation novelty' | cut -d' ' -f1 | sed 's/.$//'`)." >> $MOD
		echo -n "alt(${SET},'${TASK}',$i,`cat ${RTE}/$i/gold.txt`, " >> $MOD
		echo    "`cat ${RTE}/$i/modsizedif.txt | grep 'wordnet novelty' | cut -d' ' -f1 | sed 's/.$//'`)." >> $MOD
	    fi
	    if [ `cat ${RTE}/$i/prediction.txt | grep -v 'error' | wc -l` -ge 1 ]; then
		echo -n "mwn(${SET}, '${TASK}', $i, `cat ${RTE}/$i/gold.txt`, " >> $MOD
		echo    "`cat ${RTE}/$i/modsizedif.txt | grep 'wordnet novelty' | cut -d' ' -f1 | sed 's/.$//'`). " >> $MOD

		echo -n "'${TASK}'," >> $ARF1
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'wordnet novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF1
		echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF1

		echo -n "'${TASK}'," >> $ARF2
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'prover' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF2
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'domain novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF2
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'relation novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF2
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'wordnet novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF2
		echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF2

		if [ `cat ${RTE}/$i/modsizedif.txt | grep 'word overlap' | wc -l` -ge 1 ]; then
		    echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'word overlap' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF3
		    echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF3
		fi
	    fi
	fi
    done
    echo "ready"
    
}

init()
{
    echo "Checking directory: ${RTE}"
    echo ":- discontiguous mod/8, alt/6, mwn/6." > $MOD
    
    echo "@relation ${SET}" > $ARF1
    echo "@attribute task {'IE', 'IR', 'QA', 'SUM'}" >> $ARF1
    echo "@attribute novelty real" >> $ARF1
    echo "@attribute gold {informative, entailment}" >> $ARF1
    echo "@data" >> $ARF1

    echo "@relation ${SET}" > $ARF2
    echo "@attribute task {'IE', 'IR', 'QA', 'SUM'}" >> $ARF2
    echo "@attribute prover {proof, contradiction, unknown}" >> $ARF2
    echo "@attribute dom_novelty real" >> $ARF2
    echo "@attribute rel_novelty real" >> $ARF2
    echo "@attribute mwn_novelty real" >> $ARF2
    echo "@attribute gold {informative, entailment}" >> $ARF2
    echo "@data" >> $ARF2

    echo "@relation ${SET}" > $ARF3
    echo "@attribute overlap real" >> $ARF3
    echo "@attribute gold {informative, entailment}" >> $ARF3
    echo "@data" >> $ARF3
}

classify()
{
    echo "-------------------------------"
    echo "Classifier for $1"
    echo "-------------------------------"
    java weka.classifiers.trees.J48 -t $1
}

if [ "${SET}" = "" ]; then
    echo "Not a directory: ${RTE}"
    echo "Usage: _trainRTE <RTESET>"
    echo "Example: _trainRTE dev3"
elif [ -d ${RTE} ]; then 
    init
    train ${RTE}
    classify ${ARF1}
    cat ${ARF1} | grep "^@" | grep -v "task" > ${ARF1simple}
    cat ${ARF1} | grep "^'" | cut -d"," -f2- >> ${ARF1simple}
    classify ${ARF1simple}
    classify ${ARF2}
    cat ${ARF2} | grep "^@" | grep -v "task" > ${ARF2simple}
    cat ${ARF2} | grep "^'" | cut -d"," -f2- >> ${ARF2simple}
    classify ${ARF2simple}
    cat ${ARF2simple} | grep "^@" | grep -v "mwn" > ${ARF3simple}
    cat ${ARF2} | grep "^'" | cut -d"," -f2,3,4,6 | grep -v "\-" >> ${ARF3simple}
    classify ${ARF3simple}
    classify ${ARF3}
fi

exit 0
