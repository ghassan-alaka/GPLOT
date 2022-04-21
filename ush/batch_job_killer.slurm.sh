#! /bin/sh --login

THISUSER="$1"
JOB_DESC="$2"
STATUS="$3"
MAX_TIME="$4"

# Get the full jobnames
#jobids=`qstat -u ${1} | awk '{print $1}' | tail -n +6`
jobids=`squeue -u ${THISUSER} -o "%.10i %.10P %.50j %.15u %.10T %.10M %.10L %.4D %R" | awk '{print $1}' | tail -n +1`

if [ ! -z "$jobids" ]; then
    for jobid in $jobids; do
        #echo $jobid

        # Read the output for 'qstat -f' for the current jobid
        if scontrol show job ${jobid} >/dev/null 2>&1 ; then
            stat=`scontrol show job ${jobid}`
        else
            continue
        fi

        # Job Name
        jobname=`echo "${stat}" | grep "JobName=" | awk '{$1=$1};1' | cut -d " " -f 2 | cut -d "=" -f 2 | tr -d '[:space:]'`
        echo "$jobname"
        # State that the job is in
        state=`echo "${stat}" | grep "JobState=" | awk '{$1=$1};1' | cut -d " " -f 1 | cut -d "=" -f 2 | tr -d '[:space:]'`
        if [ "$STATUS" == "ALL" ] ; then
            state="ALL"
        fi
        echo "$state"

        # Check if job name contains $JOB_DESC
        if echo $jobname | grep "$JOB_DESC" >/dev/null 2>&1 ; then
        if [ "$state" == "$STATUS" ] ; then
            echo "Found jobid ${jobid} that matches $JOB_DESC"

            # Time that the job was queued
            qtime=`echo "${stat}" | grep "SubmitTime=" | awk '{$1=$1};1' | cut -d " " -f 1 | cut -d "=" -f 2 | tr -d '[:space:]'`
            d1=$(date '+%s')
            d2=$(date -d "$qtime" '+%s')
            ddiff=$(( d1-d2  ))
            echo "$ddiff  $MAX_TIME"

            if [ $ddiff -gt $MAX_TIME ]; then
                echo "Deleting this job because ${ddiff} > ${MAX_TIME}"
                scancel ${jobid} >/dev/null 2>&1
            fi
       fi
       fi
    done
fi
