#!/bin/ksh --login
#
# This script links GFS Forecast GRIB2 data
# from the last 1 month to be plotted with
# the GPLOT plotting package.

set -aeu

date_old=`/bin/date +"%Y%m%d%H" --date="10 days ago"`
date_now=`/bin/date +"%Y%m%d%H"`
echo $date_old $date_now
echo "MSG: Maintain links to GFS data for last 1-10 days."
DSOURCE=$1

if [[ $DSOURCE == "HWRF" ]]; then
	gfsdir="/lfs4/HFIP/hwrf-data/hwrf-input/FORECAST/"
elif [[ $DSOURCE == "PUB" ]]; then
	gfsdir="/lfs4/BMC/public/data/grids/gfs/0p25deg/grib2/"
	gfsdir2="/lfs4/BMC/public/data/grib/ftp/7/0/81/0_1038240_0/"
fi
#pdir="/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/GFSdata/"
odir="/lfs1/HFIP/hur-aoml/Ghassan.Alaka/pytmp/GFS_Forecast/"

allFcst=`ls $gfsdir | xargs -n 1 basename`

for i in $allFcst; do
	if [[ $DSOURCE == "PUB" ]]; then
		YY=`echo $i | cut -c1-2`
		JJJ=`echo $i | cut -c3-5`
		HH=`echo $i | cut -c6-7`
		FHR=`echo $i | cut -c11-13`

		gfsDate=`date -d "20${YY}-01-01 +${JJJ}days -1days" +"%Y%m%d"`${HH}
		A="gfs.${gfsDate}"

		echo $i
		echo $A  $YY $JJJ $HH $FHR

		#Check if directory exists
		if [ ! -d $odir$A ]; then
			mkdir -p $odir$A
		fi

		#Check if the file exists. If not, link it.
		if [ ! -f $odir$A/gfs.t${HH}z.pgrb2.0p25.f${FHR}.grb2 ]; then
			echo "MSG: Linking GFS file: $A/gfs.t${HH}z.pgrb2.0p25.f${FHR}.grb2"
			ln -sf $gfsdir$i $odir$A/gfs.t${HH}z.pgrb2.0p25.f${FHR}.grb2
		else
			echo "MSG: GFS file already linked: $A/gfs.t${HH}z.pgrb2.0p25.f${FHR}.grb2"
		fi

        #Check if the 000 file exists. If not, link it.
        if [ ! -f $odir$A/gfs.t${HH}z.pgrb2.0p25.f000.grb2 ]; then
            echo "MSG: Linking GFS file: $A/gfs.t${HH}z.pgrb2.0p25.f000.grb2"
            ln -sf ${gfsdir2}${YY}${JJJ}${HH}000000 $odir$A/gfs.t${HH}z.pgrb2.0p25.f000.grb2
        fi

	elif  [[ $DSOURCE == "HWRF" ]]; then
		gfsDate=`echo $i | awk -F "." '{print $NF}'`
		echo $gfsDate
		if [ $gfsDate -ge $date_old ]; then
	
			#Check if directory exists
			if [ ! -d $odir$i ]; then
				mkdir -p $odir$i
			fi

			# Check if directory is empty
			if ls $gfsdir$i/*pgrb2.* 1> /dev/null 2>&1; then
				echo "MSG: GFS files are available."
				rm -rf $odir$i/*grb2*
				ln -sf $gfsdir$i/*pgrb2.* $odir$i/.
				find $odir$i/gfs* -exec mv '{}' '{}'.grb2 \;
				echo "MSG: GFS data successfully linked."
				ls $odir$i
			else
				echo "MSG: GFS files are not available."
			fi
		else
			if [ -d $odir$i ]; then
				rm -rf $odir$i
				echo "MSG: Deleting old directory."
			else
				echo "MSG: Directory does not exist. Skipping deletion."
			fi
		fi
	fi
done


# Remove directories for which GFS files are no longer available.
if [[ $DSOURCE == "PUB" ]]; then
	echo "MSG: Checking available GFS files."

	allGFS=`ls $odir | xargs -n 1 basename`	

	for i in $allGFS; do
		iDate=`echo $i | awk -F "." '{print $NF}'`
		echo "MSG: Checking for: ${iDate}"
		YYYY=`echo $iDate | cut -c1-4`
		YY=`echo $iDate | cut -c3-4`
		MM=`echo $iDate | cut -c5-6`
		DD=`echo $iDate | cut -c7-8`
		HH=`echo $iDate | cut -c9-10`

		JJJ=`/bin/date -d "$YYYY-$MM-$DD" +"%j"`

		if ls $gfsdir/${YY}${JJJ}${HH}* 1> /dev/null 2>&1; then
			echo "MSG: GFS files are available for: ${iDate}"
			echo "MSG: Keeping links."
		else
			echo "MSG: GFS files are no longer available for: ${iDate}"
			echo "MSG: Deleting links."
			rm -rf $odir$i
		fi
	done
fi

echo "COMPLETE!"
