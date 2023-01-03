#!/bin/ksh -l
#
# This script will merge a number of user-specified a-deck ATCF files.
# The first argument is the output directory where the resulting merged
# ATCF files will be placed. The remaining arguments are the input
# ATCF files to be merged.
#
# Example invocation:
#
#   >> mkdir /lfs2/projects/$USER/MERGED-ATCF
#   >> ./createADeck.ksh /lfs2/projects/$USER/MERGED-ATCF/ file1 file2 file3...
#
# Author: Thiago.Quirino@noaa.gov, live long and prosper \/_
# Last edited by Ghassan.Alaka@noaa.gov

set -aeu

#Greet the user.
echo "-Started on `date`."

#Check if at least 2 arguments were specified.
if(test $# -lt 2) then
    echo "Error: At least 2 arguments are required. See script header for details."
    exit 1
fi

#Parse in the path to the output directory.
outputDir=`readlink -m "$1"`
if(! test -d "$outputDir") then
    echo "Error: The specified output directory '$1' does not exist."
    exit 2
fi
echo "-Using output directory: $outputDir"

#Remove argument 1. All remaining arguments are assumed to be paths to input a-deck files.
shift 1

#Create a temporary directory.
tmpDir="$outputDir/.atcfMergeADeck_tmp_${USER}_${HOSTNAME}_PROC-$$_RAND-$RANDOM"
mkdir -p "$tmpDir"
if(test $? -ne 0 || ! test -d "$tmpDir") then
    echo "Error: Cannot create temporary directory '$tmpDir'. Please check permissions and availability of disk space."
    exit 4
fi
echo "-Created temporary directory: $tmpDir"
rm -rf "$tmpDir"/*

#Parse a list of the the input a-deck files to be processed.
echo "-Creating a list of input a-deck files."
numSpecifiedFiles="$#"
while(test $# -ge 1)
do
    #Append file path to temporary file.
    readlink -m "$1" >> "$tmpDir/.inputFilesList"    #Convert from relative to full path and store in temporary directory.

    #Remove this file from the list of input arguments.
    shift 1
done

#Display some info to user
echo "-Total files to be processed: ${numSpecifiedFiles}"

#Iterate through each input a-deck file and sort them by storm, date, and model. Example temp file: AL032010___2010052600___HWRF
#printf "%b" "-Processing input files: 0.00% complete"
count=0
while read file
do
echo "Processing file: $file"
    #Increment file counter.
    count=`expr "$count" + 1`

    #Ensure this is a valid file.
    if(! test -s "$file") then
        printf "\nError: Invalid input a-deck file '$file'.\n"
        #rm -rf "$tmpDir"
        #exit 15
	printf "\nSkipping...\n"
	continue
    fi

    #Iterate through each line in the file, removing leading and trailing whitespaces.
    isTempFileCreator=0 #Did this file lead to the creation of a temporary file.
    cat "$file" | awk '{ gsub(/^[ \t]+|[ \t]+$/, ""); print }'| awk '(length($0)>0){print}' | while read line
    do
        #Get the storm name, forecast date, and model name.
        stormId=`echo "$line" | awk -F "," '{print $1,$2,$3}' | tr -d ' ' | cut -c1-8 | tr '[a-z]' '[A-Z]' | grep "^[A-Z]\{2,2\}[0-9]\{6,6\}$"`
        forecastDate=`echo "$line" | awk -F "," '{print $3}' | tr -d ' ' | grep "^[0-9]\{10,10\}$"`
        modelName=`echo "$line" | awk -F "," '{print $5}' | tr -d ' '`
        fHour=`echo "$line" | awk -F "," '{print $6}' | tr -d ' '`

        #Ensure all arguments are valid.
        if(test -z "$stormId" || test -z "$forecastDate" || test -z "$modelName") then
            printf "\nError: Invalid line in input a-deck file '$file'. Please check the file format.\nLine: $line\n"
            rm -rf "$tmpDir"
            exit 5
        fi

        #Check if the temporary file exists.
        if(! test -f "$tmpDir/${stormId}___${forecastDate}___${modelName}") then
            #echo "-Input file '$file' is the creator of temp file '$tmpDir/${stormId}___${forecastDate}___${modelName}'."
            isTempFileCreator=1

            #If this input file contains the forecast of a single model for a single storm starting 
            #at a single date, then simply copy the input file as the temporary file.
            if(test `awk -F "," '{print $1,$2,$3}' "$file" | tr -d ' ' | cut -c1-8 | sort -u | wc -l` -eq 1 && test `awk -F "," '{print $3}' "$file" | tr -d ' ' | sort -u | wc -l` -eq 1 && test `awk -F "," '{print $5}' "$file" | tr -d ' ' | sort -u | wc -l` -eq 1) then
                #echo "-File '$file' is cool."
                #cat "$file" > "$tmpDir/${stormId}___${forecastDate}___${modelName}"
                ln -sf "$file" "$tmpDir/${stormId}___${forecastDate}___${modelName}"
                break
            fi
        fi

echo "    Point 4"
        #Search for an existing entry in the temporary file, and quit if found.
        if(test "$isTempFileCreator" -ne 1 && ! test -z `awk -F "," -v h="$fHour" '(1.0*h==1.0*$6){print}' "$tmpDir/${stormId}___${forecastDate}___${modelName}"`) then
            printf "%b" "\nFatal: Duplicate entry found for storm '$stormId', forecast date '$forecastDate', model '$modelName', and forecast hour '$fHour'.\n"
            exit 100
        fi

echo "    Point 5"
        #Ensure we are not attempting to write into a symbolic link temporary file
        #That represents an input ATCF file containing a unique forecast.
        if(test -h "$tmpDir/${stormId}___${forecastDate}___${modelName}") then
            printf "%b" "\nFatal: Tried to write data from input file '$file' into temporary symbolic link file '$tmpDir/${stormId}___${forecastDate}___${modelName}' which points to a previously processed input ATCF file. This means that the ATCF forecast data for storm '$stormId', forecast date '$forecastDate', and model '$modelName' are located in more than 1 file. You are on your own here, buddy. I don't handle that and I don't get paid enough. Why would you do that anyway? That is so weird, man.\n"
            exit 101
        fi

echo "    Point 6"
        #Write line data to the specified file.
        echo "$line" >> "$tmpDir/${stormId}___${forecastDate}___${modelName}"

echo "    Point 7"
    done

    #Update the progress bar.
    pct=`echo "$count $numSpecifiedFiles" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
    #printf "%b" "\r-Processing input files: ${pct}% complete"
done < "$tmpDir/.inputFilesList"

#Count the number of intermediary files created.
numTempFiles=`ls "$tmpDir" | wc -l | tr -d ' '`
if(test "$numTempFiles" -eq 0) then
    printf "\nError: All lines in the input a-deck files were empty.\n"
    rm -rf "$tmpDir"
    exit 6
else
    printf "\n-Unique model forecasts found: $numTempFiles\n"
fi

#Get a count of the final number of output merged a-deck files.
uniqueStormIds=`ls "$tmpDir" | awk -F "___" '{print $1}' | sort -u` #Get the unique storm IDs.
numStorms=`printf "$uniqueStormIds\n" | wc -l | tr -d ' '`        #The number of merged files.

#Merge all files belonging to the same storm into a single a-deck file.
printf "%b" "-Merging unique forecasts: 0.00% complete"
count=0
printf "$uniqueStormIds\n" | while read stormId #Iterate through every storm.
do
    #Increment file counter.
    count=`expr "$count" + 1`

    #Lowercase the storm ID for use in the merged a-deck file name.
    stormIdLower=`echo "$stormId" | tr '[A-Z]' '[a-z]'`

    #Remove any older version of the destination merged file.
    mergedFile="$outputDir/a${stormIdLower}.dat"
    rm -f "$mergedFile"
    if(test -f "$mergedFile") then
        printf "\nError: Could not remove older version of destination file '$mergedFile'.\n"
        rm -rf "$tmpDir"
        exit 7
    fi

    #Iterate through the unique forecasts of this storm.
    ls "$tmpDir" | grep "^${stormId}___" | awk -F "___" '{printf "%s___%s\n",$2,$3}' | sort -u | while read dateAndModel
    do
        cat "$tmpDir/${stormId}___${dateAndModel}" >> "$mergedFile"
    done

    #Update the progress bar.
    pct=`echo "$count $numStorms" | awk '{printf "%3.2f\n",(100.0*$1)/$2}'`
    printf "%b" "\r-Merging unique forecasts: ${pct}% complete"
done

#Delete the temporary directory.
printf "\n-Created $numStorms merged a-deck ATCF files.\n"
rm -rf "$tmpDir"
echo "-Ended on `date`."















