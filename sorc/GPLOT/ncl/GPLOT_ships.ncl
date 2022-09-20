;================================================================
;
; File Name: GPLOT_ships.ncl
;
; Description:	This script extracts important data from model
;		output and writes it to *.DAT files to be;
;		accessed later by other GPLOT scripts. For
;		example, this script is capable of efficiently
;		producing many Large-Scale Diagnostic (LSDIAG)
;		variables necessary for the SHIPS model.
;
; Namelist:	/home/Ghassan.Alaka/NCL/scripts/namelist.GPLOT
;
; Created By:    Ghassan Alaka Jr.
; Date Created:  April 24, 2017
; Last Modified: February 23, 2018
;
; Last Modified by Mu-Chieh Ko: Jun 04, 2018
;
; Last Modified by Lewis J. Gramer: Aug 16, 2019
;
; V1:		original, based on GPLOT_realtime.v6.ncl
; 2018-02-23 --> moved here from GPLOT_data.ncl
; 2018-06-04 --> bugs fixed for non-realtime mode (check MKedit)
; 2019-08-16 --> added DO_CONVERTGIF to call ImageMagick to 'convert' to GIF
;================================================================
;
;================================================================
; Load scripts
;================================================================

load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_util.ncl"
load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_func.ncl"
load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_main.ncl"

;external FILTER121 "$GPLOT_DIR/ncl/wrapit/filter121.so"
external SPH2CART "$GPLOT_DIR/sorc/GPLOT/ncl/wrapit/sph2cart.so"
;external HBFILTER "$GPLOT_DIR/ncl/wrapit/hbfilter.so"



;================================================================
; Begin the code.
;================================================================

begin

print("MSG: GPLOT_ships.ncl started at "+systemfunc("date"))
print("")
print("MSG: Welcome to GPLOT, Ships Module.")
print("MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.")
print("MSG: The Ships Module produces graphical products based on the Statistical")
print("MSG: Hurricane Intensity Prediction Scheme (SHIPS) and similar vortex-centric")
print("MSG: evaluations.")
print("MSG: Written by Ghassan Alaka [NOAA/OAR/AOML/HRD].")
print("MSG: All queries should be sent to Ghassan.Alaka@noaa.gov")
print("MSG: This module was last updated on 2019-06-15.")
print("")



;================================================================
; PART I. Set constants. DO NOT CHANGE.
;================================================================

C	= load_constants()
g	= C@g
pi	= C@pi
d2r	= C@d2r
r	= C@r
omega	= C@omega
ms2kts	= C@ms2kts
fval	= C@fval
fval2	= C@fval2
fval3	= C@fval3
tunits	= C@tunits




;================================================================
; PART II. Check for certain command line input & environmental
;          variables.
;================================================================

; 1. GPLOT_DIR must be set in the environment
GPLOT_DIR = systemfunc("echo $GPLOT_DIR")
if(GPLOT_DIR.eq."")then	print("ERROR: $GPLOT_DIR must be set as an environmental variable.")
			exit
else			print("MSG: Found this GPLOT location: "+GPLOT_DIR)
end if


; 2. Get the user & home from the environment
USER = systemfunc("echo $USER")
HOME = systemfunc("echo $HOME")


; 3. Check for ensemble member from command line
if(isvar("ENSID"))then	print("MSG: Found this ensemble ID: "+ENSID)
else			ENSID = ""
end if


; 4. Check for the level of verbosity
;    The default level is 0
if(.not.isvar("verbose"))then   verbose = 0   end if
if(typeof("verbose").eq."string")then   verbose := toint(verbose)   end if
print("MSG: Verbose level = "+tostring(verbose))





;================================================================
; PART III. Read Master Namelist and Assign Variables 
;================================================================

; 1. Set the namelist path.
;    Preferable to get this from the command line.
;    Default namelist is set as a backup.
if(.not.isvar("MASTER_NML_IN"))then		MASTER_NML = GPLOT_DIR+"/parm/namelist.input.default"
else
	if(fileexists(MASTER_NML_IN))then	MASTER_NML = MASTER_NML_IN
	else					MASTER_NML = GPLOT_DIR+"/parm/"+MASTER_NML_IN
	end if
end if
print("MSG: Using this master namelist:  "+MASTER_NML)


; 2. Read the master namelist into NCL
;data = asciiread(MASTER_NML,-1,"string")
NML = Read_Master_Namelist(MASTER_NML)

; 3. Get basic variables from the master namelist:
if(.not.isvar("DOMAIN"))then            DOMAIN = NML@DOMAIN                     end if
if(.not.isvar("TIER"))then              TIER = NML@TIER                         end if
if(.not.isvar("DSOURCE"))then           DSOURCE = NML@DSOURCE                   end if
if(.not.isvar("MACHINE"))then           MACHINE = NML@MACHINE                   end if
if(.not.isvar("IDATE"))then             IDATE = NML@IDATE                       end if
if(.not.isvar("SID"))then               SID = NML@SID                           end if
if(.not.isvar("MODELID"))then           MODELID = NML@MODELID                   end if
if(.not.isvar("ATCF_REQD"))then         ATCF_REQD = NML@ATCF_REQD               end if
if(.not.isvar("EXPT"))then              EXPT = NML@EXPT                         end if
if(.not.isvar("IDIR"))then              IDIR = NML@IDIR                         end if
if(.not.isvar("ITAG"))then              ITAG = NML@ITAG                         end if
if(.not.isvar("EXT"))then               EXT = NML@EXT                           end if
if(.not.isvar("ODIR"))then              ODIR = NML@ODIR                         end if
if(.not.isvar("ODIR_TYPE"))then         ODIR_TYPE = NML@ODIR_TYPE               end if
if(.not.isvar("INIT_HR"))then           INIT_HR = NML@INIT_HR                   end if
if(.not.isvar("FNL_HR"))then            FNL_HR = NML@FNL_HR                     end if
if(.not.isvar("FMT_HR"))then            FMT_HR = NML@FMT_HR                     end if
if(.not.isvar("DT"))then                DT = NML@DT                             end if
if(.not.isvar("IS_MSTORM"))then         IS_MSTORM = NML@IS_MSTORM               end if
if(.not.isvar("DO_RMWHITE"))then        DO_RMWHITE = NML@DO_RMWHITE             end if
if(.not.isvar("DO_SRCLBL"))then         DO_SRCLBL = NML@DO_SRCLBL               end if
if(.not.isvar("PIV"))then               PIV = NML@MAP_PIV                       end if
if(.not.isvar("DO_CONVERTGIF"))then     DO_CONVERTGIF = NML@DO_CONVERTGIF       end if
if(.not.isvar("DO_PDF"))then		DO_PDF = NML@DO_PDF			end if
if(.not.isvar("FORCE"))then		FORCE = NML@FORCE
else					FORCE := where(FORCE.eq."True",True,False)
end if

if(ODIR_TYPE.eq.1)then      ODIR = str_sub_str(ODIR+"/"+DOMAIN+"/","//","/")
else                        ODIR = str_sub_str(ODIR+"/"+EXPT+"/"+ENSID+"/"+IDATE+"/"+DOMAIN+"/","//","/")
end if
print("MSG: I will place graphics in this location --> "+ODIR)



;================================================================
; PART IV. Print some information to the terminal
;================================================================

print("MSG: Running GPLOT with these settings:")
if(IS_MSTORM)then	print("MSG:   Multi-Storm:        ON")
else			print("MSG:   Multi-Storm:        OFF")
end if
print("MSG:   Experiment Name:    "+EXPT)
print("MSG:   Data Source:        "+DSOURCE)
print("MSG:   Forecast Cycle:     "+IDATE)
print("MSG:   Storm ID:           "+SID)
print("MSG:   Domain/Region:      "+DOMAIN)
print("MSG:   Graphics Tier:      "+TIER)
if(ENSID.ne."")then     print("MSG:   Ensemble Member:    "+ENSID)      end if
print("MSG:   Input Directory:    "+IDIR)
print("MSG:   Output Directory:   "+ODIR)
print("MSG:   System Environment: "+MACHINE)
print("MSG:   Input File Tag(s):  "+stringOut(ITAG))
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
print("MSG:   Hour Format:        "+FMT_HR)
print("MSG:   Time Step [h]:      "+DT)
if(DO_RMWHITE)then	print("MSG:   Trim graphics:      ON")
else			print("MSG:   Trim graphics:      OFF")
end if
if(DO_SRCLBL)then       print("MSG:   Source Labels:      ON")
else                    print("MSG:   Source Labels:      OFF")
end if
if(DO_CONVERTGIF)then   print("MSG:   Convert to GIF:     ON")
else                    print("MSG:   Convert to GIF:     OFF")
end if




;================================================================
; PART V. Set/Change some variables here.
;         These options may eventually be moved into functions
;         and/or namelist options.
;================================================================

; 1. We require DOMAIN="ships"
;    This should be identical to "d03"
if(DOMAIN.ne."ships")then       print("MSG: Updating DOMAIN to 'ships'. Might not work.")
                                DOMAIN = "ships"
end if


; 2. Make the Output Directory (ODIR)
;    It should already exist, so this is a safety check.
system("mkdir -p "+ODIR)


; 3. Graphic domain boundaries
;    Ex: (/lat_north, lat_south, lon_west, lon_east/)
;    To add a new domain, please add it to GPLOT_func.ncl
;    BOCO will be updated for storm-centered domains.
;BOCO   = (/0.,0.,0.,0./)
BOCO    = getDmnBds(DOMAIN,DSOURCE)


; 5. Check if this configuration is storm-centric.
;    This will control the graphic name and other options.
;    Storm-centric configurations will append the storm name
;    and storm ID to the front of the file name.
;    SID must be not equal to NONE to advance
SC_DOMAIN = True
ATCF_REQD = True
if(SID.eq."NONE")then
	print("ERROR: No storms identified, but ATCF is required.")
	print("ERROR: Therefore, can't create storm-centered graphics.")
	exit
end if


; 6. If DSOURCE includes FVGFS, all times and levels are 
;    located in one input file.
if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then	ONE_FILE = True
else						ONE_FILE = False
end if


; 7. 'fvgfs' data sources require DT=6 for the storm-centered domains.
if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then   DT = 6   end if


; 8. Create a string that contains the SID for storm-centered domains
;    and is blank for non storm-centered domains.
SIDtag = "."+str_upper(SID)


; 9. Choose number of forecasts for model trend plots
nTrend		= 6


; 10. Set up workstation resources
if(DO_PDF)then	wtype = "pdf"
else		wtype	= "png"
		wtype@wkWidth = 2048
		wtype@wkHeight = 2048
end if


; 11. Parse the cycle (IDATE) for later use
;     Expected format is YYYYMMDDHH
YYYY	= systemfunc("echo "+IDATE+" | cut -c1-4")
MM	= systemfunc("echo "+IDATE+" | cut -c5-6")
DD	= systemfunc("echo "+IDATE+" | cut -c7-8")
HH	= systemfunc("echo "+IDATE+" | cut -c9-10")


; 12. Determine the extension of post-processed graphics.
if(DO_CONVERTGIF)then	wtype2 = "gif"
else			wtype2 = wtype
end if



;================================================================
; PART VI. Read Graphics Namelist
;================================================================

; 1. Set the Graphics Namelist (VAR_NML)
;    Uncomment this to set default namelist.
;VAR_NML		= "~/GPLOT/parm/namelist.ships.test"
if(isvar("VAR_NML"))then
	print("MSG: Detected custom graphics namelist. Will try that first.")
	if(fileexists(GPLOT_DIR+"/parm/"+VAR_NML))then
		VAR_NML = GPLOT_DIR+"/parm/"+VAR_NML
	else
		print("WARNING: Custom graphics namelist not found. Trying defaults.")
		delete(VAR_NML)
	end if
end if
if(.not.isvar("VAR_NML"))then   
	if(fileexists(GPLOT_DIR+"/parm/namelist.ships."+EXPT+"."+TIER))then
		VAR_NML = GPLOT_DIR+"/parm/namelist.ships."+EXPT+"."+TIER
	end if
end if
if(.not.isvar("VAR_NML"))then
	if(fileexists(GPLOT_DIR+"/parm/namelist.ships.default."+TIER))then
		VAR_NML = GPLOT_DIR+"/parm/namelist.ships.default."+TIER
	end if
end if
if(.not.isvar("VAR_NML"))then
	if(fileexists(GPLOT_DIR+"/parm/namelist.ships.default"))then
		VAR_NML	= GPLOT_DIR+"/parm/namelist.ships.default"
	end if
end if
if(.not.fileexists(VAR_NML))then	print("ERROR: Graphics namelist not available.")
					exit
end if
print("MSG: Using this graphics namelist: "+VAR_NML)


; 2. Read the Graphics Namelist
;    DATA_ON --> compute the data and print to an ASCII file
;    PLOT_ON --> produce graphics of the data type
;    FNAME   --> name of the data (for ASCII and graphic files)
data		:= asciiread(VAR_NML,-1,"string")
headers		:= str_split(data(0),"   ")
DATA_ON		= where(str_squeeze(str_get_field(data(1:),ind(headers.eq."DATA_ON")+1,"   ")).eq."T",True,False)
nData		= dimsizes(DATA_ON)
PLOT_ON		= where(str_squeeze(str_get_field(data(1:),ind(headers.eq."PLOT_ON")+1,"   ")).eq."T",True,False)
FNAME		= str_squeeze(str_get_field(data(1:),ind(headers.eq."FILE_NAME")+1,"   "))
print("MSG: Will produce data for:     "+stringOut(FNAME(ind(DATA_ON))))
print("MSG: Will produce graphics for: "+stringOut(FNAME(ind(PLOT_ON))))
if(verbose.ge.1)then	print(VAR_NML)
			print(DATA_ON+"  "+PLOT_ON+"  "+FNAME)
end if


; 3. Print some information if high verbosity requested
if(verbose.ge.1)then
	print("MSG: Here's some information you might find useful:")
	print("MSG: TC Name: "+str_upper(TCNAME))
	print("MSG: TC ID:   "+SID)
	print("MSG: Have a great day!")
end if




;================================================================
; PART VII. Get the list of input files.
;================================================================

print("")
print("MSG: Get the list of input files for the graphics.")


; 1. Get the list of files from UnplottedFiles
iFiles = asciiread(ODIR+"UnplottedFiles."+DOMAIN+"."+TIER+SIDtag+".log", -1, "string")
if(all(ismissing(iFiles)))then		print("ERROR:  No input files found. Something went wrong.")
					exit
end if


; 2. Remove missing values to create final file list for IDATE
iFiles	:= iFiles(ind(.not.ismissing(iFiles)))
nFiles	:= dimsizes(iFiles)


; 3. Find out which files have not been modified in the last 30 minutes.
;    If any files meet this criteria, then final status can't be "complete".
FNL_IFILES = new(nFiles,logical)
do fff = 0,nFiles-1
	IFILES_MOD := systemfunc("find "+iFiles(fff)+" -mmin +15")
	if(ismissing(IFILES_MOD(0)))then	FNL_IFILES(fff) = False
	else					FNL_IFILES(fff) = True
	end if
end do


; 4. Get the list of Forecast Hours (FHR) from a given input file
;    As a backup option, the forecast hours can be parsed from the 
;    input file names, but this is hard-coded and might not work.
;    It is HIGHLY recommended that this job be submitted via
;    spawn_maps.sh. However, if you realy want to submit this
;    script yourself, you must mimic "AllForecastHours" by listing
;    the corresponding forecast hours for each input file listed in
;    "UnplottedFiles'. Each forecast hour should be on its own line.
FHR_LIST=ODIR+"AllForecastHours."+DOMAIN+"."+TIER+SIDtag+".log"
FHR_TEST=systemfunc("ls "+FHR_LIST+" 2>/dev/null")
if(ismissing(FHR_TEST))then
	if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then
		FHR	:= ispan(1,132,1)
	else
		FHR	= new(nFiles,"integer")
		do fff = 0,nFiles-1
			; Parse the path for each file
			fSplit  := str_split(iFiles(fff),"/")
			fSplit	:= str_split(fSplit(dimsizes(fSplit)-1),".")
	
	        	if(isStrSubset(DSOURCE,"HWRF"))then
				tmp     := fSplit(dimsizes(fSplit)-2)
		                ;NameIDTag = fSplit(0)
		        else if(DSOURCE.eq."NMMB")then
		                tmp     := fSplit(dimsizes(fSplit)-1)
		        else if(DSOURCE.eq."GFS")then
		               	tmp     := fSplit(dimsizes(fSplit)-2)
			else if(DSOURCE.eq."GEFS")then
				tmp     := fSplit(dimsizes(fSplit)-1)
		       	end if  end if  end if  end if
	
			; Get the forecast hour as an integer
			tmp	:= str_split(tmp,"f")
		        FHR(fff) = toint(tmp(dimsizes(tmp)-1))
		end do
		qsort(FHR)
	end if
else
	FHR=asciiread(FHR_LIST, -1, "integer")
end if
nFHR = dimsizes(FHR)
FHR@format = "%0.1i"
print("MSG: Found these forecast hours --> "+stringOut(FHR))


; 5. Update the final forecast hour to produce graphics
;MY_FNL_HR = max(FHR)
;fnlStr = ".f"+sprinti("%0.3i",MY_FNL_HR)
;print("MSG: The final forecast hour is "+sprinti("%0.3i",FNL_HR)+".")


; 6. Define the status file. This will be updated throughout.
;    If running the GPLOT wrappers, this must match.
STATUS_FILE = ODIR+"status."+DOMAIN+"."+TIER+SIDtag+".log"
LOCK_FILE = STATUS_FILE+".lock"
print("MSG: GPLOT Status file is "+STATUS_FILE)


; 7. 'force=True' will overwrite every graphic, even if it exists.
;    Sometimes, that functionality is desired, but if 'force'
;    is undefined by now, then it should be set to 'force=False'
;    (may be deprecated)
;if(.not.isvar("force"))then   force = False   end if




;================================================================
; PART VIII. GET TC COORDINATES FROM ATCF
;    This section requires an ATCF that matches the model output.
;    For TC-core graphics (i.e., "d03"), GPLOT fails if it does 
;    not find the matching ATCF.
;    For domains other than "d03", GPLOT will label active TCs
;    when the ATCF is supplied correctly.
;    We want to get the coordinates whenever fff = 0. Typically,
;    this is the first pass.
;    ATCF is only supported for HWRF, GFS & FVGFS.
;================================================================

print("")
print("MSG: Searching for matching ATCFs.")
		
; Get the list of ATCF files from a pre-existing text file
; If the text file does not exist, then set ATCF_FILES=NONE
if(fileexists(ODIR+"ATCF_FILES.dat"))then	ATCF_FILES = asciiread(ODIR+"ATCF_FILES.dat",-1,"string")
else						ATCF_FILES = "NONE"
end if
		
; Check if there are no ATCF files
if(ATCF_FILES(0).eq."NONE")then
	print("ERROR: Found no ATCF files. Can not proceed for storm-centered domain.")
	exit
end if
	
; Find the ATCF that corresponds to this case.
; ATCF is required, so script will fail if it is missing.
val := ind(isStrSubset2(str_lower(ATCF_FILES),str_lower(SID)+"."))
if(ismissing(val(0)))then
	print("ERROR: Found no ATCF files. Can not proceed for storm-centered domain.")
	exit
end if
ATCF_FILES := ATCF_FILES(val(0))
print("MSG: Working on this ATCF file: "+ATCF_FILES)
	
	
; Setup variables
tcLats	:= new((/nFHR/),float)
tcLons	:= new((/nFHR/),float)
tcFHR	:= new((/nFHR/),integer)
tcINT	:= new((/nFHR/),integer)
tcM6h	:= new((/nFHR/),float)
tcM6hB	:= new((/nFHR/),float)
		

; Storm ID (SID) should already be defined.
; Get the long Storm ID, e.g., HARVEY09L
; Get the storm number, e.g., 9
LONGSID = str_upper(systemfunc("echo "+ATCF_FILES+" | rev | cut -d'/' -f1 | rev | cut -d'.' -f1"))
;ALL_SID		= str_upper(systemfunc("echo "+ALL_LONGSID+" | rev | cut -c1-3 | rev"))
SNUM	= toint(systemfunc("echo "+SID+" | rev | cut -c1-2 | rev"))
TCNAME	= str_upper(systemfunc("echo "+LONGSID+" | rev | cut -c4- | rev"))
SID_OLD = getInvestSID(YYYY,SID)
			

; Read the ATCF data
data := adeckRead(ATCF_FILES,"",IDATE)
do fff = 0,nFHR-1		
	val	:= ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.FHR(fff))
	if(fff.eq.nFHR-1)then	val1	= default_fillvalue("integer")
	else			val1	:= ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.(FHR(fff)+DT))
	end if
	if(fff.eq.0)then	val2	= default_fillvalue("integer")
	else			val2	:= ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.(FHR(fff)-DT))
	end if
				 
	if(ismissing(val(0)))then
		; If the bounding forecast hours exist, then interpolate
		if(.not.ismissing(val1(0)) .and. .not.ismissing(val2(0)))then
			dHr1 = abs(FHR(fff) - toint(data(2,val1(0))))
			dHr2 = abs(FHR(fff) - toint(data(2,val2(0))))
			pctHr1	= tofloat(dHr1)/(tofloat(dHr1)+tofloat(dHr2))
			pctHr2	= tofloat(dHr2)/(tofloat(dHr1)+tofloat(dHr2))
			
			tcLats(fff) = pctHr1*data(0,val1(0)) + pctHr2*data(0,val2(0))
			tcLons(fff) = pctHr1*data(1,val1(0)) + pctHr2*data(1,val2(0))
			tcFHR(fff)  = FHR(fff)
			tcINT(fff)  = toint(pctHr1*data(3,val1(0)) + pctHr2*data(3,val2(0)))
		else
			continue
		end if
	else
		tcLats(fff) = data(0,val(0))
		tcLons(fff) = data(1,val(0))
		tcFHR(fff)  = toint(data(2,val(0)))
		tcINT(fff)  = toint(data(3,val(0)))
	end if
		
	vals	:= ind(.not.ismissing(tcFHR(:)))
	if(ismissing(vals(0)))then   continue   end if
	do aaa = 0,dimsizes(vals)-1
		if(ismissing(tcFHR(vals(aaa))))then   continue   end if
		valM := ind(tcFHR(:).eq.(tcFHR(vals(aaa)))-6.)
		if(ismissing(valM))then   continue   end if
		tcM6h(vals(aaa)) = C@ms2kts*gc_latlon(tcLats(vals(aaa)),tcLons(vals(aaa)),tcLats(valM),tcLons(valM),2,3)/21000.
		tcM6hB(vals(aaa))= C@r2d*atan2((tcLons(vals(aaa))-tcLons(valM)),(tcLats(vals(aaa))-tcLats(valM)))
	end do
end do ;fff
			
			
; Determine if the ATCF file has been updated within 2 hours
; If so, consider the ATCF final. This is used for non
; storm-centered graphics because they are not necessarily 
; tied to a particular storm. So, graphics may still be
; produced even if there is no ATCF entry.
ATCF_MOD := systemfunc("find "+ATCF_FILES+" -mmin +20")
if(ismissing(ATCF_MOD(0)))then	FNL_ATCF = False
else				FNL_ATCF = True
end if

		
; Update the final forecast hour to match the final ATCF hour
FNL_HR = max(tcFHR)
	

if(.not.isvar("NameIDTag"))then   NameIDTag = "NONAME"+SID   end if
if(.not.isvar("TCNAME"))then   TCNAME = "NONAME"   end if



;================================================================
; LOOP OVER ALL INPUT FILES.
; Loop will not complete until 'fnlStr' file or until last
; available file has been plotted.
;================================================================

print("")
print("")
fff = 0
DO_PLOT = True
if(DO_PLOT)then
do fff = 0,nFiles-1

	;================================================================
	; START WORKING ON THE NEXT FILE
	; Tell the STATUS file that we're still working...
	;================================================================

	; Update the status file
        system("lockfile -r-1 -l 180 "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
        system("echo 'working' > "+STATUS_FILE)
        system("rm -f "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

	myInd	:= ind(isStrSubset2(iFiles,".f"+sprinti(FMT_HR,FHR(fff))))
    	if(ismissing(myInd))then
	        myInd   := ind(isStrSubset2(iFiles,"."+sprinti(FMT_HR,FHR(fff))+"."))
	end if

	print("MSG: Current file: "+sprinti("%0.2i",fff+1)+"   "+iFiles(myInd))
	
	
	;================================================================
	; Only compute data if forecast hour is divisible by 'dt'
	; dt is usually 3 or 6.
	;================================================================
	if(FHR(fff)%DT.ne.0)then	print("MSG: Forecast hour is not divisible by "+DT+". Skipping.")
					continue
	end if
	

	;================================================================
	; Check if an ATCF entry exists for the current forecast hour.
	; Required for "d03" graphics
	; Suggested for large-scale graphics
	;================================================================
	myFHR := ind(tcFHR.eq.FHR(fff))
		
	if(ismissing(myFHR(0)))then
		print("WARNING: Forecast hour not found in ATCF. Skipping.")
		continue
	end if
	
	
	;Check if this forecast hour has been completed
	; Double-check the logic here.
	FHRDONE = True
	do ppp = 0,nData-1
		if(DATA_ON(ppp))then
			DAT_FILE	= ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".dat"
			;DAT_FILE2	= ODIR+str_lower(LONGSID)+".TC_CenterFix."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))+".dat"
	
			; Get the TC centers based on the geopotential height centroid
			if(FNAME(ppp).eq."TCCEN")then		NCOL = 5
			else if(FNAME(ppp).eq."TCHODO")then	NCOL = 4
			else					NCOL = 2
			end if  end if
				
			; 1) Check if the file exists, and if the data
			; for this particular forecast hours has been written.
			if(fileexists(DAT_FILE))then
				DAT_SZ	:= toint(systemfunc("stat -c %s "+DAT_FILE))
				if(ismissing(DAT_SZ(0)))then	FHRDONE = False
								break
                                else if(DAT_SZ.eq.0)then        print("WARNING: One of the data files is unexpectedly empty --> "+DAT_FILE)
                                                                print("WARNING: Will remove this empty file and will also remove the list of plotted files to force reproduction.")
                                                                PLOTTED_FILES = ODIR+"PlottedFiles."+DOMAIN+"."+TIER+SIDtag+".log"
                                                                print("WARNING: Plotted Files list --> "+PLOTTED_FILES)
                                                                FHRDONE = False
                                                                system("rm -f "+DAT_FILE)
                                                                system("rm -f "+PLOTTED_FILES)
                                                                break
				end if  end if
				NL	:= dimsizes(asciiread(DAT_FILE,-1,"string"))
				data	:= asciiread(DAT_FILE,(/NL,NCOL/),"float")
				if(any(toint(data(:,0)).eq.FHR(fff)))then   continue   end if
			end if
			FHRDONE = False
			break
		end if
	end do
	print("MSG: Made it! Does at least one graphic type requires more processing? "+FHRDONE)
	if(.not.FORCE)then
		if(FHRDONE .and. FHR(fff).ne.max(FHR))then
			print("MSG: Data already completed for this forecast hour.")
			continue
		end if
	end if
	print("MSG: Yes, looks like at least one graphic type requires more processing for this lead time.")
	

	;================================================================
	; Read the file
	;================================================================
	f	= addfile(iFiles(myInd),"r")


	;================================================================
	; Get the valid time
	;================================================================
	validDate	= changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",tofloat(str_sub_str(sprinti(FMT_HR,FHR(fff)),"f","")))

		
	;================================================================
	; GET DATA VALUES
	;================================================================
	ppp2	= 0
	do ppp = 0,nData-1
	if(DATA_ON(ppp))then	
		; Basic Dimension Info
		nDims	:= dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"U","")))
		dNames	:= getfilevardims(f,findVarName(DSOURCE,"U",""))
		nDims@dNames = dNames
		
		; Get info based on domain
		B = getDmnInfo("ships",DSOURCE,"bdstype",False)

		; Loop over all available dimensions
		; Only need to do this once
		if(ppp2.eq.0)then
		do ddd = 0,nDims-1
			; LATITUDE
			if(isStrSubset(dNames(ddd),"lat"))then
				lat := get_dim_lat(f,dNames(ddd),BOCO,tcLats,tcLons,fff,B)
				if(dimsizes(lat).eq.2)then	lat := 9.96921e+36
								lat@_FillValue = 9.96921e+36
				end if
				if(ismissing(lat(0)))then   break   end if
				dy := abs(lat(1)-lat(0))
				dym := dy*111200.
				BOCO = lat@BOCO
			end if
			
			
			; LONGITUDE
			if(isStrSubset(dNames(ddd),"lon"))then
				BOCO@PivotLon = PIV
				lon := get_dim_lon(f,dNames(ddd),BOCO,tcLons,tcLats,fff,B)
                                if(dimsizes(lon).eq.2)then      lon := 9.96921e+36
                                                                lon@_FillValue = 9.96921e+36
                                end if
                                if(ismissing(lon(0)))then   break   end if
				dx := abs(lon(1)-lon(0))
				dxm := dx*111200.
				BOCO = lon@BOCO
				flipFlag = lon@flipFlag
				
				; Need the full longitude array
				lonF := lon@LONF
				lonF!0 = "lon"
				lonF&lon = lonF
				lonF@units = "degrees_east"
			end if
			
			; ISOBARIC LEVELS
			; We only need 200 hPa to 1000 hPa
			if(isStrSubset(dNames(ddd),"ISBL") .or. isStrSubset(str_lower(dNames(ddd)),"pres"))then
				
				if(f->$dNames(ddd)$@units.eq."Pa")then		mf	= 100.
				else if(f->$dNames(ddd)$@units.eq."hPa")then	mf	= 1.
				end if
				end if
				LEV	= (/f->$dNames(ddd)$({200.*mf:1000.*mf})/)
				LEV!0 = "lev"
				LEV&lev = LEV
				LEV@units = f->$dNames(ddd)$@units
				LEV@mf	= mf
							end if
			
			; HEIGHT LEVELS
			if(isStrSubset(dNames(ddd),"HTGL"))then
				mf	= 1.
			end if
		end do

		OUT_OF_BDS = False
		if(isvar("lat"))then
		if(ismissing(lat(0)))then
		        print("WARNING: Model domain and graphic domain do not overlap. Nothing to do.")
		        OUT_OF_BDS = True
		end if
		end if
		if(isvar("lon"))then
		if(ismissing(lon(0)))then
		        print("WARNING: Model domain and graphic domain do not overlap. Nothing to do.")
		        OUT_OF_BDS = True
		end if
		end if
		if(OUT_OF_BDS)then
		        ; Update PlottedFiles file if all files are done and:
		        ;    -- ATCF is required and forecast hour is less than or equal to the minimum hour from ATCFs
		        ;    -- ATCF is not required
	                print("MSG: All graphics & data files found for the current forecast hour ("+sprinti("%0.3i", FHR(fff))+").")
		
	                ; Get the input file base name
	                IFILE_BASE := str_split(iFiles(myInd(0)),"/")
	                IFILE_BASE := IFILE_BASE(dimsizes(IFILE_BASE)-1)
		
	                PLOTTED_FILES = ODIR+"PlottedFiles."+DOMAIN+"."+TIER+SIDtag+".log"
	                ;if(ATCF_REQD .and. ALL_ATCF)then
	                if(FHR(fff).le.FNL_HR)then
	                        if(fileexists(PLOTTED_FILES))then
	                                system("sed -i '/"+IFILE_BASE+"/d' "+PLOTTED_FILES)
	                                system("echo "+str_get_dq()+iFiles(myInd(0))+" 1"+str_get_dq()+" >> "+PLOTTED_FILES)
	                        else
	                                system("echo "+str_get_dq()+iFiles(myInd(0))+" 1"+str_get_dq()+" > "+PLOTTED_FILES)
	                        end if
	                end if
	                system("sort -u "+PLOTTED_FILES+" > "+PLOTTED_FILES+".TMP")
	                system("mv "+PLOTTED_FILES+".TMP "+PLOTTED_FILES)

			continue
	        end if
		
		
		; mf is the multiplicative factor for the vertical coordinate
		; If not already defined, set it to 0
		if(.not.isvar("mf"))then   mf = 0.   end if
		
		end if
		
		; BOCO2 is critical to read in the data exactly as planned.
		; BOCO is used to set the graphical bounds.
		; Is BOCO2 necessary for the 'findCenter' function?
		BOCO2 = (/max(lat),min(lat),min(lon),max(lon)/)
		BOCO2@PivotLon = PIV
		
		
		;================================================================
		; SET RESOURCES & OTHER PLOTTING OPTIONS
		; These don't change, but are necessary for nice graphics.
		;================================================================
		;wtype	= "png"
		;wtype	= "x11"
		;wtype@wkWidth = 2048
		;wtype@wkHeight = 2048
		
		; Basic Plotting Resources
		; Do not apply to TCCEN or TCHODO
		res			:= True
		res@gsnDraw		= False
		res@gsnFrame		= False
		res@gsnMaximize		= False
		res@xyDashPattern	= 0
		res@xyLineThicknessF	= 15.0
		res@xyMarkLineMode	= "MarkLines"
		res@xyMarkerColor	= "red"
		res@xyMarkerSizeF	= 0.020
		res@xyMarkerThicknessF	= 10.0
		;res@mpShapeMode		= "FreeAspect"
		res@vpWidthF		= 0.8
		res@vpHeightF		= 0.4
		res@tiMainFont		= 22
		res@tiMainFontHeightF	= 0.025
		res@tiMainOffsetYF	= 0.05
		res@tiXAxisString	= "Forecast Time [h]"
		res@tiXAxisOffsetYF	= 0.00
		res@tiYAxisOffsetXF	= 0.00
		res@tmXBMode		= "Explicit"	
		res@tmXTOn		= False
		res@tmYLMode		= "Explicit"	
		res@tmYROn		= False
		res@trYMaxF		= 360.
		res@trYMinF		= 0.
		
		; TCCEN map background resouces
		type = 9
		resC  := GenPlotRes(type,False)
		resC@mpFillDrawOrder = "PreDraw"
		
		; General polyline resources
		plres			:= True

		; General marker resources
		mkres			:= True
		
		; TCCEN marker resources
		mresC			= True
		mresC@gsMarkerIndex	= 4;5
		mresC@gsMarkerSizeF	= 12.0
		mresC@gsMarkerThicknessF	= 2.5
	
		; General text resources
		txres			:= True
		txres@txFontHeightF	= 0.010
		txres@txJust		= "CenterLeft"
		
		txres2			:= True
		txres2@txJust		= "CenterCenter"
		
		txres3			:= True
		txres3@txFontHeightF	= 0.010
		txres3@txAngleF		= 0.
		txres3@txFontColor	= "black"
		txres3@txJust		= "CenterCenter"
		
		txres4			:= True
		txres4@txFontHeightF	= 0.008
		txres4@txJust		= "CenterLeft"
		
		; TCCEN text resources
		txresC			= True
		txresC@txFontHeightF	= 0.0135
		txresC@txJust		= "CenterCenter"
		
		; General polygon resources
		pgres			:= True
		pgres@gsEdgesOn		= True
		pgres@gsEdgeColor	= "black"
		pgres@gsFillColor	= "white"
		pgres@gsFillOpacityF	= 0.75					
		

		; Set location of legend entries manually here
		xleg = (/0.22,0.22,0.36,0.36,0.50,0.50,0.64,0.64/)   ; Location of
		xtxt = (/0.25,0.25,0.39,0.39,0.53,0.53,0.67,0.67/)   ; legend markers
		yleg = (/0.83,0.81,0.83,0.81,0.83,0.81,0.83,0.81/)   ; and text
		ytxt = (/0.83,0.81,0.83,0.81,0.83,0.81,0.83,0.81/)   ; strings.
		
		; Faded-Rainbow color map
		TRDcolors	:= (/"red1","darkorange2","gold2","green3","blue3","purple4"/)
		
		TRDmarkers	:= (/8,7,6,0,4,16/)
		

		; Update the coordinates for plotting
		;print("  Plotting Region: "+DOMAIN(bbb))
		;iBOCO2		= (/iBOCO(0)+2.,iBOCO(1)-2.,iBOCO(2)-2.,iBOCO(3)+2./)
		pctLat		= abs(BOCO(0)-BOCO(1))/100.
		pctLon		= abs(BOCO(3)-BOCO(2))/100.
		

		; IF APPLICABLE, compute values and write to a file for future use
		; For simplicity, we will only do this for HWRF D03
		print("MSG: Processing this parameter: "+FNAME(ppp))			
			
		; Find pre-written data for this variable
		DAT_FILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".dat"
		W = "w"
		if(.not.any((/"TCCEN","TCHODO"/).eq.FNAME(ppp)))then
			OFILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN
			NCOL := 2
			; 1) Check if the file exists. If YES, change the write option
			;    from overwrite (w) to append (a). Check if the data for
			;    this particular forecast hour has been written. 
			if(fileexists(DAT_FILE))then
				NL	:= dimsizes(asciiread(DAT_FILE,-1,"string"))
				data	:= asciiread(DAT_FILE,(/NL,NCOL/),"float")
				; If the forecast hour exists in the file and is not the maximum hour.
				if(.not.FORCE .and. any(toint(data(:,0)).eq.FHR(fff)) .and. FHR(fff).ne.max(FHR))then
					print("MSG: Skipping "+FNAME(ppp)+" because it has already been processed.")
					continue  
				; If the forecast hour exists in the file and is the maximum hour.
				else if(.not.FORCE .and. any(toint(data(:,0)).eq.FHR(fff)) .and. FHR(fff).eq.max(FHR))then
					W = "NO"
				; If the forecast hour does not exist in the file.
				else
					W = "a"
				end if  end if
			end if
		else
			OFILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))
			if(FNAME(ppp).eq."TCCEN")then		NCOL = 5
			else if(FNAME(ppp).eq."TCHODO")then	NCOL = 4
			end if  end if
			; 1) Check if the file exists. If YES, change the write option
			;    from overwrite (w) to append (a). Check if the data for
			;    this particular forecast hour has been written. 
			if(fileexists(DAT_FILE))then
				NL	:= dimsizes(asciiread(DAT_FILE,-1,"string"))
				data	:= asciiread(DAT_FILE,(/NL,NCOL/),"float")
				if(.not.FORCE .and. any(toint(data(:,0)).eq.FHR(fff)) .and. fileexists(OFILE+"."+wtype2))then
					print("MSG: Skipping "+FNAME(ppp)+" because it has already been processed.")
					continue
				else if(.not.FORCE .and. any(toint(data(:,0)).eq.FHR(fff)) .and. .not.fileexists(OFILE+"."+wtype2))then
					W = "NO"
				else
					W = "a"
				end if  end if
			end if
			if(fileexists(OFILE+"."+wtype2))then	SKIP_PLOT = True
			else					SKIP_PLOT = False
			end if
		end if

			
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Deep Layer Vertical Wind Shear (200-800km) ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Magnitude and Heading
		; SHIPS --> SHRD: 850-200 mb shear magnitude (kt *10) vs time (200-800 km)
		;       --> SHTD: Heading (deg) of SHRD shear vector
		;	--> SHRS: 850-500 mb shear magnitude (kt *10) vs time (200-800 km)
		;	--> SHTS: Heading (deg) of SHRS shear vector
		if(any((/"SHRD","SHTD","SHRS","SHTS"/).eq.FNAME(ppp)))then
			; 1) Set up the cartesian coordinates
			X	:= tofloat(ispan(-800,800,10))
			Y	:= tofloat(ispan(-800,800,10))
				
			; 2) Get the right level code to compute difference
			if(any((/"SHRD","SHTD"/).eq.FNAME(ppp)))then		levCode	= "02000850d"
			else if(any((/"SHRS","SHTS"/).eq.FNAME(ppp)))then	levCode = "05000850d"
			end if  end if
			
			; 3) Get U & V data (uncombined)
			U	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			U(0,:,:)= getVar2d(f,DSOURCE,(/"U",levCode/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			U@_FillValue = -999.
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"V",levCode/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			V@_FillValue = -999.
				
			; 4) Convert both U & V to cartesian (X,Y) coordinates
			U1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",U@_FillValue)
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(U,U1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),U@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)

			; 5) Average U & V in a 200-800km annulus (see SHIPS)
			U1	= where(     sqrt(conform(U1,X,2)^2.+conform(U1,Y,1)^2.).ge.200.\
					.and.sqrt(conform(U1,X,2)^2.+conform(U1,Y,1)^2.).le.800.,\
					U1,U1@_FillValue)

			if(all(ismissing(U1)))then	U1 := U@_FillValue
			else				U1 := avg(U1)
			end if
			V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
					.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
					V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if

			; 6) Calculate the variable here
			if(any((/"SHRD","SHRS"/).eq.FNAME(ppp)))then
				; 7) FOR WIND SHEAR MAGNITUDE
				V1	:= sqrt(U1^2. + V1^2.)
				V1@units = "kts"
				V1@long_name = "VWS Magnitude"
				V1@_FillValue = U1@_FillValue
			else if(any((/"SHTD","SHTS"/).eq.FNAME(ppp)))then
				; 6) FOR WIND SHEAR DIRECTION
				V1	:= C@r2d*atan2(U1,V1)
				if(.not.ismissing(V1))then
					if(V1.lt.0)then   V1 = 360.+V1   end if
				end if
				V1@units = "degrees"
				V1@long_name = "VWS Direction"
			end if  end if

			; 7) Write data to a .DAT file.
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
			
			; 8) Write data to table (if applicable)	
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %8.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 9) Update the title
			if(levCode.eq."02000850d")then		TITLE = "200-850 hPa Vertical Wind Shear"
			else if(levCode.eq."05000850d")then	TITLE = "500-850 hPa Vertical Wind Shear"
			end if  end if
			TITLE = TITLE+" ["+V1@units+"]"			
			
			
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		; Deep Layer Vertical Wind Shear (0-500km around 850mb center) ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> SHDC: Same as SHRD but with vortex removed and averaged from 0-500 km relative
		;		  to 850 mb vortex center
		;	    SDDC: Heading (deg) of SHDC shear vector
		; Caution: This variable takes longer to compute since the vortex
		else if(FNAME(ppp).eq."SHDC")then
			; 1) Set up the cartesian coordinates
			X	:= tofloat(ispan(-500,500,10))
			Y	:= tofloat(ispan(-500,500,10))
				
			; 2) Get U & V data (uncombined)
			U	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			U(0,:,:)= getVar2d(f,DSOURCE,(/"U","02000850d"/),nDims,BOCO2,mf,(/flipFlag,True/),lonF)
			U@_FillValue = -999.
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"V","02000850d"/),nDims,BOCO2,mf,(/flipFlag,True/),lonF)
			V@_FillValue = -999.

			; 3) Get 850 hPa TC center
			if(.not.fileexists(str_sub_str(DAT_FILE,FNAME(ppp),"TCCEN")))then   continue   end if
			NL	:= dimsizes(asciiread(str_sub_str(DAT_FILE,FNAME(ppp),"TCCEN"),-1,"string"))
			data	:= asciiread(tmpDir2+NameIDTag+".TCCEN."+IDATE_s+".dat",(/NL,5/),"float")
			LAT850	:= data(ind(toint(data(:,0)).eq.fHr(fff) .and. toint(data(:,1)).eq.850),2)
			LON850	:= data(ind(toint(data(:,0)).eq.fHr(fff) .and. toint(data(:,1)).eq.850),3)
				
			; 4) Convert both U & V to cartesian (X,Y) coordinates
			U1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",U@_FillValue)
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(U,U1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),LON850,LAT850,U@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),LON850,LAT850,V@_FillValue)

			; 5) Average U & V in a 500km annulus (see SHIPS)
			U1	= where(sqrt(conform(U1,X,2)^2.+conform(U1,Y,1)^2.).le.500.,U1,U1@_FillValue)
			if(all(ismissing(U1)))then	U1 := U@_FillValue
			else				U1 := avg(U1)
			end if
			V1	= where(sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.500.,V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if

			; 6) FOR WIND SHEAR DIRECTION
			V2	= C@r2d*atan2(U1,V1)
			if(ismissing(V2))then
				if(V2.lt.0)then   V2 = 360.+V2   end if
			end if
				
			; 7) FOR WIND SHEAR MAGNITUDE
			V3	= sqrt(U1^2. + V1^2.)

			; 8) SHDC: Same as SHRD but with vortex removed and averaged from 0-500 km relative
			;          to 850 mb vortex center
			;	   Write  data to a .DAT file.
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V3/)
			
			; 9) Write data to table (if applicable)	
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %8.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
				
			;10) SDDC: Heading (deg) of above SHDC vector
			;    Check if the file exists. If YES, change the write option
			;    from overwrite (w) to append (a). Check if the data for
			;    this particular forecast hour has been written. Then, get
			;    the data and write it to a .DAT file.
			DAT_FILE2 = str_sub_str(DAT_FILE,FNAME(ppp),"SDDC")
			W = "w"
			if(fileexists(DAT_FILE2))then
				NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
				data	:= asciiread(DAT_FILE2,(/NL,NCOL/),"float")
				if(.not.FORCE .and. any(toint(data(:,0)).eq.FHR(fff)))then	W = "NO"
				else								W = "a"
				end if
			end if
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V2/)
			if(W.ne."NO")then	print("MSG: Printing values for: SDDC")
						write_table(DAT_FILE2,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %8.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE2+" | sort -u > "+DAT_FILE2+".TMP")
						system("mv "+DAT_FILE2+".TMP "+DAT_FILE2)
			end if
			
			; 9) Update the title for the graphic.
			TITLE = "Deep Shear w/ Vortex Removed"
			TITLE = TITLE+" ["+V1@units+"]"	
			
		
		;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		; Mean Sea Level Pressure ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> MSLP: Minimum Mean Sea level pressure (hPa)
		;	--> PENV: Surface Pressure (200-800km)
		else if(any((/"MSLP","PENV"/).eq.FNAME(ppp)))then
			; 1) Get the MSLP
			MSLPdims	:= dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"MSLP","")))
			MSLPdNames	:= getfilevardims(f,findVarName(DSOURCE,"MSLP",""))
			MSLPdims@dNames	= MSLPdNames
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"MSLP",""/),MSLPdims,BOCO2,1.,flipFlag,lonF)
			V@long_name = "MSLP"
			V@_FillValue = -999.
			
			; 2) Define MSLP
			if(FNAME(ppp).eq."MSLP")then
				if(all(ismissing(V)))then	V1 := V@_FillValue
				else				V1 := min(V)
				end if
				TITLE	= "Min. Mean Sea-Level Pressure"
			
			; or Define PENV
			else if(FNAME(ppp).eq."PENV")then
				; Set up the cartesian coordinates
				X	:= tofloat(ispan(-800,800,10))
				Y	:= tofloat(ispan(-800,800,10))
					
				; Convert both U & V to cartesian (X,Y) coordinates
				V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",default_fillvalue("float"))
				SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
				; Average U & V in a 200-800km annulus (see SHIPS)
				V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
						.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
						V1,V1@_FillValue)
				if(all(ismissing(V1)))then	V1 := V@_FillValue
				else				V1 := avg(V1)
				end if
				TITLE	= "Environmental Surface Pressure"
			end if  end if
			copy_VarAtts(V,V1)
				
			; 3) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 4) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %10.3f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 5) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
			
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Maximum Surface (10-m) Wind ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> VMAX: Maximum surface wind (kt)
		;	--> IKE34: 34-kt Integrated Kinetic Energy (based on 10-m wind)
		;	--> IKE50: 50-kt Integrated Kinetic Energy (based on 10-m wind)
		;	--> IKE64: 64-kt Integrated Kinetic Energy (based on 10-m wind)
		else if(any((/"VMAX","IKE34","IKE50","IKE64"/).eq.FNAME(ppp)))then
			; 1) Get the 10-m wind
			UV10dims	:= dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"UV","10")))
			UV10dNames	:= getfilevardims(f,findVarName(DSOURCE,"UV","10"))
			UV10dims@dNames	= UV10dNames
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"UV","10"/),UV10dims,BOCO2,1.,flipFlag,lonF)
			V@long_name = "10-m Wind"
			V@_FillValue = -999.
			
			; 2) Define VMAX
			if(FNAME(ppp).eq."VMAX")then
				if(all(ismissing(V)))then	V1 := V@_FillValue
				else				V1 := max(V)
				end if
				copy_VarAtts(V,V1)
				V1@units = "kts"
				V1@long_name = "Max. Intensity"
				TITLE	= "Maximum Intensity"
			
			; or Define IKE
			else if(isStrSubset(FNAME(ppp),"IKE"))then
				; Set up the cartesian coordinates
				dx_cart = 10. ; units of km
				dy_cart = dx_cart
				X	:= tofloat(ispan(-1000,1000,toint(dx_cart)))
				Y	:= tofloat(ispan(-1000,1000,toint(dy_cart)))

				; Define constants for the IKE calculation
				VOL = (dx_cart*1000.)*(dy_cart*1000.)*10.	; grid cell volume in m^3 (assume height of 10 m)
				RHO = 1.225					; density of dry air at sea level in kg / m^3

				; Convert both U & V to cartesian (X,Y) coordinates
				V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",default_fillvalue("float"))
				copy_VarAtts(V,V1)
				SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
			
				if(FNAME(ppp).eq."IKE34")then
					V1	= where(V1.lt.34.,V1@_FillValue,0.5*(V1/C@ms2kts)*(V1/C@ms2kts)*RHO*VOL)
					TITLE	= "Integrated Kinetic Energy (34+ kts)"
				else if(FNAME(ppp).eq."IKE50")then
					V1	= where(V1.lt.50.,V1@_FillValue,0.5*(V1/C@ms2kts)*(V1/C@ms2kts)*RHO*VOL)
					TITLE	= "Integrated Kinetic Energy (50+ kts)"
				else if(FNAME(ppp).eq."IKE64")then
					V1	= where(V1.lt.64.,V1@_FillValue,0.5*(V1/C@ms2kts)*(V1/C@ms2kts)*RHO*VOL)
					TITLE	= "Integrated Kinetic Energy (64+ kts)"
				end if  end if  end if
				if(all(ismissing(V1)))then	V1 := 0.
				else				V1:= sum(V1)/(10.^12.)
				end if
				V1@long_name = "IKE"
				V1@units = "TJ"
			end if  end if
				
			; 3) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/(/tofloat(FHR(fff))/), (/V1/)/)
				
			; 4) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i%8.3f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 5) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
		
		;;;;;;;;;;;;;;;;;;;;;;	
		; 200 hPa Zonal Wind ;
		;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> U200: 200 hPa Zonal Wind (200-800km)
		; SHIPS --> U20C: 200 hPa Zonal Wind (0-500km)
		else if(any((/"U200","U20C"/).eq.FNAME(ppp)))then
			; 1) Get U
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"U","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			V@long_name = "Zonal Wind"
			V@_FillValue = -999.
			
			; 2) Define U200
			if(FNAME(ppp).eq."U200")then
				; 2) Set up the cartesian coordinates
				X	:= tofloat(ispan(-800,800,10))
				Y	:= tofloat(ispan(-800,800,10))
			
				; 4) Convert both U to cartesian (X,Y) coordinates
				V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
				SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
				; 5) Average U in a 200-800km annulus (see SHIPS)
				V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
						.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
						V1,V1@_FillValue)
				if(all(ismissing(V1)))then	V1 := V@_FillValue
				else				V1 := avg(V1)
				end if
				TITLE	= "200 hPa Zonal Wind (200-800km)"
			
			; or Define U20C
			else if(FNAME(ppp).eq."U20C")then				
				; 2) Set up the cartesian coordinates
				X	:= tofloat(ispan(-500,500,10))
				Y	:= tofloat(ispan(-500,500,10))
				
				; 4) Convert both U to cartesian (X,Y) coordinates
				V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
				SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
				; 5) Average U in a 200-800km annulus (see SHIPS)
				V1	= where(sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.500.,V1,V1@_FillValue)
				if(all(ismissing(V1)))then	V1 := V@_FillValue
				else				V1 := avg(V1)
				end if
				TITLE	= "200 hPa Zonal Wind (0-500km)"
			end if  end if
			copy_VarAtts(V,V1)
				
			; 3) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 4) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 5) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
			
		;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		; 200 hPa Meridional Wind ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> V20C: 200 hPa Meridional Wind (0-500km)
		else if(any((/"V20C"/).eq.FNAME(ppp)))then
			; 1) Get V data
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"V","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			V@long_name = "Meridional Wind"
			V@_FillValue = -999.
			TITLE	= "200 hPa Meridional Wind (0-500km)"
				
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-500,500,10))
			Y	:= tofloat(ispan(-500,500,10))
				
			; 4) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
			; 5) Average U & V in a 200-800km annulus (see SHIPS)
			V1	= where(sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.500.,V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if
			copy_VarAtts(V,V1)

			; 8) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 9) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 9) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	

			
		;;;;;;;;;;;;;;;;;;;;;	
		; Relative Humidity ;
		;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> RHLO: 850-700 hPa Relative Humidity (200-800km)
		;	    RHMD: 700-500 hPa Relative Humidity (200-800km)
		;	    RHHI: 500-300 hPa Relative Humidity (200-800km)
		;	    R000: 1000 hPa Relative Humidity (200-800km)
		else if(any((/"RHLO","RHMD","RHHI"/).eq.FNAME(ppp)))then				
			; 1) Retrieve RH data
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			if(FNAME(ppp).eq."RHLO")then		V(0,:,:)= getVar2d(f,DSOURCE,(/"RH","07000850m"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
								V@long_name = "RH"
								TITLE	= "850-700 hPa Relative Humidity (200-800km)"
			else if(FNAME(ppp).eq."RHMD")then	V(0,:,:)= getVar2d(f,DSOURCE,(/"RH","05000700m"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
								V@long_name = "RH"
								TITLE	= "700-500 hPa Relative Humidity (200-800km)"
			else if(FNAME(ppp).eq."RHHI")then	V(0,:,:)= getVar2d(f,DSOURCE,(/"RH","03000500m"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
								V@long_name = "RH"
								TITLE	= "500-300 hPa Relative Humidity (200-800km)"
			end if  end if  end if
			V@_FillValue = -999.
				
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-800,800,10))
			Y	:= tofloat(ispan(-800,800,10))
				
			; 3) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
			; 4) Only retain V1 values within a 200-800km annulus of the TC center (see SHIPS)
			V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
					.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
						V1,V1@_FillValue)

			; 5) Since this is RH, set any values outside of (0,100) to a missing value.
			V1	= where(V1.gt.100. .or. V1.lt.0., V1@_FillValue, V1)

			; 6) Average all non-missing values of V1
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if
			copy_VarAtts(V,V1)

			; 7) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 8) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 9) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
		else if(any((/"R000"/).eq.FNAME(ppp)))then				
			; 1) Retrieve RH data
			if(isvar("V") .and. V@long_name.ne."1000 hPa Relative Humidity")then
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"RH","1000"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V@long_name = "Relative Humidity"
				TITLE	= "1000 hPa Relative Humidity (200-800km)"
			else if(.not.isvar("V"))then
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"RH","1000"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V@long_name = "Relative Humidity"
				TITLE	= "1000 hPa Relative Humidity (200-800km)"
			end if  end if
			V@_FillValue = -999.
				
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-800,800,10))
			Y	:= tofloat(ispan(-800,800,10))
				
			; 3) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
			; 4) Average U & V in a 200-800km annulus (see SHIPS)
			V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
					.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
						V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if
			copy_VarAtts(V,V1)

			; 5) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 6) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 7) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
			
		;;;;;;;;;;;;;;;;;;;;;;
		; Relative Vorticity ;
		;;;;;;;;;;;;;;;;;;;;;;
		; SHIPS --> Z850: 850 hPa Relative Vorticity (0-1000km)
		else if(any((/"Z850"/).eq.FNAME(ppp)))then
			; 1) Retrieve Vorticity data
			if(isvar("V") .and. V@long_name.ne."850 hPa Relative Vorticity")then
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"RVO","0850"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V@long_name = "Relative Vorticity"
				TITLE = "850 hPa Relative Vorticity (0-1000km)"
			else if(.not.isvar("V"))then
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"RVO","0850"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V@long_name = "Relative Vorticity"
				TITLE = "850 hPa Relative Vorticity (0-1000km)"
			end if  end if
			V@_FillValue = -999.
				
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-1000,1000,10))
			Y	:= tofloat(ispan(-1000,1000,10))
				
			; 4) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
			; 5) Average U & V in a 200-800km annulus (see SHIPS)
			V1	= where(sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.1000.,V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := 100.*avg(V1)
			end if
			copy_VarAtts(V,V1)
			V1@units = "s-1 x 10**-7"

			; 6) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 7) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 8) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
			
		;;;;;;;;;;;;;;
		; Divergence ;
		;;;;;;;;;;;;;;
		; SHIPS --> D200: 200 hPa Divergence (sec-1 * 10**7) vs time (r=0-1000 km)
		;	--> DIVC: Same as D200, but centered at 850 hPa vortex location
		else if(any((/"D200","DIVC"/).eq.FNAME(ppp)))then
			; 1)  Retrieve wind data
			if(isvar("V") .and. V@long_name.ne."200 hPa Meridional Wind")then
				U	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				U(0,:,:)= getVar2d(f,DSOURCE,(/"U","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"V","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V	:= uv2dv_cfd(U,V,lat,lon,2)
				V@long_name = "Divergence"
				V@units = "s-1"
			else if(.not.isvar("V") .and. .not.isvar("U"))then
				U	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				U(0,:,:)= getVar2d(f,DSOURCE,(/"U","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
				V(0,:,:)= getVar2d(f,DSOURCE,(/"V","0200"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
				V	:= uv2dv_cfd(U,V,lat,lon,2)
				V@long_name = "Divergence"
				V@units = "s-1"
			end if  end if
			U@_FillValue = -999.
			V@_FillValue = -999.

			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-1000,1000,10))
			Y	:= tofloat(ispan(-1000,1000,10))
				
			; 3) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			if(FNAME(ppp).eq."D200")then
				SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				TITLE = "200 hPa Divergence (0-1000km)"
			else if(FNAME(ppp).eq."DIVC")then
				; 4) Get 850 hPa TC center
				DAT_FILE2 = str_sub_str(DAT_FILE,FNAME(ppp),"TCCEN")
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,5/),"float")
					LAT850	:= data(ind(toint(data(:,0)).eq.FHR(fff) .and. toint(data(:,1)).eq.850),2)
					LON850	:= data(ind(toint(data(:,0)).eq.FHR(fff) .and. toint(data(:,1)).eq.850),3)
				
					SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),LON850,LAT850,V@_FillValue)
					TITLE = "200 hPa Divergence (0-1000km), 850 hPa center"
				else
					print("ERROR: Could not find TC Center Fixes: "+DAT_FILE2)
					print("ERROR: Skipping current variable: "+FNAME(ppp))
					continue
				end if
			end if  end if
				
			; 4) Average U & V in a 200-800km annulus (see SHIPS)
			V1	= where(sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.1000.,V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := 10000000.*avg(V1)/C@ms2kts
			end if
			copy_VarAtts(V,V1)
			V1@units = "s-1 x 10**-7"

			; 5) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 6) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
			
			; 7) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
			
		;;;;;;;;;;;;;;;
		; Temperature ;
		;;;;;;;;;;;;;;;
		; SHIPS --> T000: 100 hPa temperature (deg C) (200-800km)
		else if(any((/"T000"/).eq.FNAME(ppp)))then
			; 1) Retrieve Temperature data
			V	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V(0,:,:)= getVar2d(f,DSOURCE,(/"T","1000"/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			V	= V - 273.15
			V@_FillValue = -999.
			TITLE	= "1000 hPa Temperature (200-800km)"
				
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-800,800,10))
			Y	:= tofloat(ispan(-800,800,10))
				
			; 4) Convert both U & V to cartesian (X,Y) coordinates
			V1	:= new((/1,dimsizes(Y),dimsizes(X)/),"float",V@_FillValue)
			SPH2CART::sph2cart(V,V1,lon,dimsizes(lon),lat,dimsizes(lat),toint(1),X,dimsizes(X),Y,dimsizes(Y),tcLons(fff),tcLats(fff),V@_FillValue)
				
			; 5) Average U & V in a 200-800km annulus (see SHIPS)
			V1	= where(     sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).ge.200.\
					.and.sqrt(conform(V1,X,2)^2.+conform(V1,Y,1)^2.).le.800.,\
						V1,V1@_FillValue)
			if(all(ismissing(V1)))then	V1 := V@_FillValue
			else				V1 := avg(V1)
			end if
			copy_VarAtts(V,V1)

			; 8) Append new entry
			data	:= new((/1,2/),"float")
			data(0,:)= (/tofloat(FHR(fff)), V1/)
				
			; 9) Write data block to .DAT file
			if(W.ne."NO")then	print("MSG: Printing values for: "+FNAME(ppp))
						write_table(DAT_FILE,W,[/toint(data(:,0)), data(:,1)/], "%0.3i  %9.2f")
						system("awk '{a[$1]++}!(a[$1]-1)' "+DAT_FILE+" | sort -u > "+DAT_FILE+".TMP")
						system("mv "+DAT_FILE+".TMP "+DAT_FILE)
						
			end if
			
			; 9) Update the title
			TITLE = TITLE+" ["+V1@units+"]"	
			
		
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Tropical Cyclone Centers ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Get the TC centers based on the geopotential height centroid
		else if(FNAME(ppp).eq."TCCEN")then
			; 1) Use NCL & Fortran functions to find the TC centers
			CTR	:= findCenter(f,DSOURCE,"HGT",LEV,lat,lonF,flipFlag,BOCO2,1)
			if(all(ismissing(CTR)))then   continue   end if
			;print(CTR@value)
			;exit				
			
			; 2) Append new entry
			data	:= new((/dimsizes(LEV),5/),"float")
			data(:,0)= (/tofloat(FHR(fff))/)
			data(:,1)= (/LEV/mf/)
			data(:,2)= (/CTR(0,:)/)
			data(:,3)= (/CTR(1,:)/)
			data(:,4)= (/where(CTR@use,1,0)/)
				
			; 3) Write data block to .DAT file
			if(W.ne."NO")then
				print("MSG: Printing values for: "+FNAME(ppp))
				write_table(DAT_FILE,W,[/toint(data(:,0)), toint(data(:,1)), data(:,2), data(:,3), toint(data(:,4))/],"%0.3i  %6.0i  %9.3f  %10.3f  %0.1i")
				system("sort -u -nk1,1 -nk2,2 "+DAT_FILE+" > "+DAT_FILE+".TMP")
				system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
				
			if(PLOT_ON(ppp) .and. .not.SKIP_PLOT)then
				print("MSG: Producing graphic for: "+FNAME(ppp))
				OFILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))
				
				; 5) Produce TC center fixes graphic
				wks = gsn_open_wks(wtype,OFILE)
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				cmapC := read_colormap_file("BlAqGrYeOrReVi200")
		
				; Update resources
				resC@mpMaxLonF		= tcLons(fff)+5.
				resC@mpMinLonF		= tcLons(fff)-5.
				resC@mpCenterLonF	= (resC@mpMaxLonF+resC@mpMinLonF)/2.
				resC@mpMaxLatF		= tcLats(fff)+5.
				resC@mpMinLatF		= tcLats(fff)-5.
				getLatLonLbl(resC,fspan(tcLons(fff)-5,tcLons(fff)+5,6),fspan(tcLats(fff)-5,tcLats(fff)+5,6))
				pctX	= 0.01*(resC@mpMaxLonF-resC@mpMinLonF)
				pctY	= 0.01*(resC@mpMaxLatF-resC@mpMinLatF)
				delete([/resC@gsnLeftStringFontHeightF,resC@gsnRightStringFontHeightF,\
					 resC@lbLabelFontHeightF/])

				; Blank plot
				plot	:= gsn_csm_map_ce(wks,resC)
				
				; Retrieve viewport info from 'plot'
				getvalues plot
					"vpYF" : vpTOP
					"vpXF" : vpLFT
					"vpHeightF" : vpHGT
					"vpWidthF" : vpWID
				end getvalues
				vpRGT  = vpLFT+vpWID
				vpPCTX = 0.01*(vpRGT-vpLFT)
				vpBOT  = vpTOP-vpHGT
				vpPCTY = 0.01*(vpTOP-vpBOT)
				
				MARKER01:= new(dimsizes(LEV),"graphic")
				do kkk = 0,dimsizes(LEV)-1
					if(max(LEV).ne.LEV(0))then	kkk2 = dimsizes(LEV)-kkk-1	end if
					if(CTR@value(kkk2).lt.0.)then	continue			end if
					if(.not.ismissing(CTR(0,kkk2)))then
						mresC@gsMarkerColor	:= cmapC(kkk2*dimsizes(cmapC(:,0))/dimsizes(LEV),:)
						txresC@txFontColor	:= cmapC(kkk2*dimsizes(cmapC(:,0))/dimsizes(LEV),:)
						if(ismissing(CTR@use(kkk2)))then
							continue
						else if(.not.CTR@use(kkk2))then
							mresC@gsMarkerIndex = 5
						else if(CTR@value(kkk2).eq.min(CTR@value(ind(CTR@value.gt.0))))then
							mresC@gsMarkerIndex = 12
						else	mresC@gsMarkerIndex = 4
						end if  end if  end if
						MARKER01(kkk2)	= gsn_add_polymarker(wks,plot,CTR(1,kkk2),CTR(0,kkk2),mresC)
						gsn_text_ndc(wks,level_convert(LEV(kkk2),LEV@units,"string","hPa"),vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY-2.5*vpPCTY*tofloat(kkk2),txresC)
					end if
				end do
				txresC@txFontColor := "black"
				gsn_text_ndc(wks,"Level [hPa]",vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY+3.*vpPCTY,txresC)
				
				; Graphic Title
				add_graphic_title(wks,FNAME(ppp),vpLFT,vpTOP+0.023,0.010)
				
				; Time Title
				TimeTitle = "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+\
					    "  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR(fff)),"f","")+"]  valid at "+validDate
				add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.008,"left")
				
				; Storm Title
				add_storm_title(wks,str_upper(LONGSID),vpRGT,vpTOP+0.008,0.008,"right")
				
				; Add the Model to the Title
				if(DO_SRCLBL)then	add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.010)
							add_disclaimer(wks,plot,EXPT,resC@mpMinLonF + 50.*pctX,resC@mpMinLatF + 2.*pctY,0.009,"center")
				end if
				
				; Add marker legend to the map
				MARKER02:= new(3,"graphic")
				TEXT03	:= new(3,"graphic")
				BOX02 	:= gsn_add_polygon(wks,plot,(/resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 15.*pctX,\
								      resC@mpMinLonF + 15.*pctX,resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 1.*pctX/),\
								    (/resC@mpMinLatF +  1.*pctY,resC@mpMinLatF +  1.*pctY,\
								      resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 1.*pctY/),\
							   pgres)
				mresC@gsMarkerColor	:= "black"
				mresC@gsMarkerIndex	= 12
				MARKER02(0)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+13.*pctY,mresC)
				TEXT03(0)	= gsn_add_text(wks,plot,"Lowest",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+13.*pctY,txres4)
				mresC@gsMarkerIndex	= 4
				MARKER02(1)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+8.*pctY,mresC)
				TEXT03(1)	= gsn_add_text(wks,plot,"Vortex",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+8.*pctY,txres4)
				mresC@gsMarkerIndex	= 5
				MARKER02(2)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+3.*pctY,mresC)
				TEXT03(2)	= gsn_add_text(wks,plot,"Non-Vtx?",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+3.*pctY,txres4)
									
				; Add SHRD/SHTD and SHRS/SHTS info to this graphic, if available
				DAT_FILE2= str_sub_str(DAT_FILE,"TCCEN","SHRD")
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,2/),"float")
					if(any(data(:,0).eq.tofloat(FHR(fff))))then
						SHRD := data(ind(data(:,0).eq.tofloat(FHR(fff))),1)
						if(dimsizes(SHRD).gt.1)then   SHRD := SHRD(0)   end if
					else
						SHRD := default_fillvalue("float")
					end if
				else
					SHRD := default_fillvalue("float")
				end if
				DAT_FILE2= str_sub_str(DAT_FILE,"TCCEN","SHTD")
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,2/),"float")
					if(any(data(:,0).eq.tofloat(FHR(fff))))then
						SHTD := data(ind(data(:,0).eq.tofloat(FHR(fff))),1)
						if(dimsizes(SHTD).gt.1)then   SHTD := SHTD(0)   end if
					else
						SHTD := default_fillvalue("float")
					end if
				else
					SHTD := default_fillvalue("float")
				end if
				DAT_FILE2= str_sub_str(DAT_FILE,"TCCEN","SHRS")
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,2/),"float")
					if(any(data(:,0).eq.tofloat(FHR(fff))))then
						SHRS := data(ind(data(:,0).eq.tofloat(FHR(fff))),1)
						if(dimsizes(SHRS).gt.1)then   SHRS := SHRS(0)   end if
					else
						SHRS := default_fillvalue("float")
					end if
				else
					SHRS := default_fillvalue("float")
				end if
				DAT_FILE2= str_sub_str(DAT_FILE,"TCCEN","SHTS")
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,2/),"float")
					if(any(data(:,0).eq.tofloat(FHR(fff))))then
						SHTS := data(ind(data(:,0).eq.tofloat(FHR(fff))),1)
						if(dimsizes(SHTS).gt.1)then   SHTS := SHTS(0)   end if
					else
						SHTS := default_fillvalue("float")
					end if
				else
					SHTS := default_fillvalue("float")
				end if
				
				; Draw vertical wind shear information
				; Print arrows & magnitude
				TEXT04 := new(9,"graphic")
				BOX01  := new(3,"graphic")					
				sclWND = max((/SHRD,SHRS,tcM6h(fff)/))
					
				if(.not.ismissing(SHRD) .and. .not.ismissing(SHTD))then
					; Background box (transparent)
					; Place this box in the upper right
					BOX01(0) = gsn_add_polygon(wks,plot,(/resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 15.*pctX,\
									      resC@mpMaxLonF - 15.*pctX,resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 1.*pctX/),\
									    (/resC@mpMaxLatF -  1.*pctY,resC@mpMaxLatF -  1.*pctY,\
									      resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*SHTD
					txres2@txFontColor	= "blue"
					txres2@txFontHeightF	= 0.025*(SHRD/sclWND)
					TEXT04(0) = gsn_add_text(wks,plot,"~F34~-",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(1) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",SHRD))+" kt",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 13.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(2) = gsn_add_text(wks,plot,"850-200 SHR",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 3.*pctY,txres3)
				end if
				if(.not.ismissing(SHRS) .and. .not.ismissing(SHTS))then
					; Background box (transparent)
					; Place this box in the upper left
					BOX01(1) = gsn_add_polygon(wks,plot,(/resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 15.*pctX,\
									      resC@mpMaxLonF - 15.*pctX,resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 1.*pctX/),\
									    (/resC@mpMinLatF +  1.*pctY,resC@mpMinLatF +  1.*pctY,\
									      resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*SHTS
					txres2@txFontColor	= "green4"
					txres2@txFontHeightF	= 0.025*(SHRS/sclWND)
					TEXT04(3) = gsn_add_text(wks,plot,"~F34~-",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(4) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",SHRS))+" kt",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 3.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(5) = gsn_add_text(wks,plot,"850-500 SHR",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 13.*pctY,txres3)
				end if
				if(.not.ismissing(tcM6h(fff)))then
					; Background box (transparent)
					; Put this box in the lower right
					BOX01(2) = gsn_add_polygon(wks,plot,(/resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 15.*pctX,\
									      resC@mpMinLonF + 15.*pctX,resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 1.*pctX/),\
									    (/resC@mpMaxLatF -  1.*pctY,resC@mpMaxLatF -  1.*pctY,\
									      resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*tcM6hB(fff)
					txres2@txFontColor	= "firebrick"
					txres2@txFontHeightF	= 0.025*(tcM6h(fff)/sclWND)
					TEXT04(6) = gsn_add_text(wks,plot,"~F34~-",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(7) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",tcM6h(fff)))+" kt",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 13.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(8) = gsn_add_text(wks,plot,"MOTION",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 3.*pctY,txres3)
				end if
					
				; Add motion vector, SHRD vetor and SHRS vector
				; Put vectors on a bullseye
				;lresC1			:= True
				;lresC1@gsLineColor	= "black"
				;lresC1@gsLineDashPattern= 2
				;lresC1@gsLineThicknessF	= 1.0
				;circle_ll(wks,plot,resC@mpMaxLonF - 10.*pctX,resC@mpMaxLatF - 10.*pctX, 10.*pctX,lresC1)
				;circle_ll(wks,plot,resC@mpMaxLonF - 10.*pctX,resC@mpMaxLatF - 10.*pctX, 7.5*pctX,lresC1)
				;circle_ll(wks,plot,resC@mpMaxLonF - 10.*pctX,resC@mpMaxLatF - 10.*pctX, 5.*pctX,lresC1)
				;circle_ll(wks,plot,resC@mpMaxLonF - 10.*pctX,resC@mpMaxLatF - 10.*pctX, 2.5*pctX,lresC1)
				;lresC1@gsLineDashPattern= 0
				;LINE01	:= new(2,"graphic")
				;LINE01(0)	= gsn_add_polyline(wks,plot,(/resC@mpMinLonF,resC@mpMaxLonF/),(/avg((/resC@mpMaxLatF,resC@mpMinLatF/)),avg((/resC@mpMaxLatF,resC@mpMinLatF/))/),lresC1)
				;LINE01(1)	= gsn_add_polyline(wks,plot,(/avg((/resC@mpMaxLonF,resC@mpMinLonF/)),avg((/resC@mpMaxLonF,resC@mpMinLonF/))/),(/resC@mpMinLatF,resC@mpMaxLatF/),lresC1)
						
				; Finish (draw) the plot
				draw(plot)
				frame(wks)
				
				; Remove white space on plot using the terminal
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)
				end if  end if  end if
				
				print("MSG: Producing graphic for: "+FNAME(ppp)+"_zoom")
				; This is the "zoomed" version of the TC center fix graphic
				OFILE	= ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"_zoom."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))
				wks	= gsn_open_wks(wtype,OFILE)
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				
				EDGE	= max((/1.10*abs(max(CTR(1,:))-min(CTR(1,:))),1.10*abs(max(CTR(0,:))-min(CTR(0,:)))/))
				if(EDGE.eq.0)then   EDGE = 0.5   end if
				resC@mpMaxLonF		= avg(CTR(1,:))+EDGE
				resC@mpMinLonF		= avg(CTR(1,:))-EDGE
				resC@mpCenterLonF	= (resC@mpMaxLonF+resC@mpMinLonF)/2.
				resC@mpMaxLatF		= avg(CTR(0,:))+EDGE
				resC@mpMinLatF		= avg(CTR(0,:))-EDGE
				getLatLonLbl2(resC,fspan(avg(CTR(1,:))-EDGE,avg(CTR(1,:))+EDGE,6),fspan(avg(CTR(0,:))-EDGE,avg(CTR(0,:))+EDGE,6),False)
				pctX	= 0.01*(resC@mpMaxLonF-resC@mpMinLonF)
				pctY	= 0.01*(resC@mpMaxLatF-resC@mpMinLatF)
				plot	:= gsn_csm_map_ce(wks,resC)
				
				; Retrieve viewport info from 'plot'
				getvalues plot
					"vpYF" : vpTOP
					"vpXF" : vpLFT
					"vpHeightF" : vpHGT
					"vpWidthF" : vpWID
				end getvalues
				vpRGT  = vpLFT+vpWID
				vpPCTX = 0.01*(vpRGT-vpLFT)
				vpBOT  = vpTOP-vpHGT
				vpPCTY = 0.01*(vpTOP-vpBOT)
				
				; Plot markers and legend
				; Marker & Legend colors should be synced
				MARKER01:= new(dimsizes(LEV),"graphic")
				do kkk = 0,dimsizes(LEV)-1
					if(max(LEV).ne.LEV(0))then	kkk2 = dimsizes(LEV)-kkk-1	end if
					if(CTR@value(kkk2).lt.0.)then	continue			end if
					if(.not.ismissing(CTR(0,kkk2)))then
						mresC@gsMarkerColor	:= cmapC(kkk2*dimsizes(cmapC(:,0))/dimsizes(LEV),:)
						txresC@txFontColor	:= cmapC(kkk2*dimsizes(cmapC(:,0))/dimsizes(LEV),:)
						if(ismissing(CTR@use(kkk2)))then
							continue
						else if(.not.CTR@use(kkk2))then
							mresC@gsMarkerIndex = 5
						else if(CTR@value(kkk2).eq.min(CTR@value(ind(CTR@value.gt.0))))then
							mresC@gsMarkerIndex = 12
						else	mresC@gsMarkerIndex = 4
						end if  end if  end if
						MARKER01(kkk2)	= gsn_add_polymarker(wks,plot,CTR(1,kkk2),CTR(0,kkk2),mresC)
						gsn_text_ndc(wks,level_convert(LEV(kkk2),LEV@units,"string","hPa"),vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY-2.5*vpPCTY*tofloat(kkk2),txresC)
					end if
				end do
				txresC@txFontColor	:= "black"
				gsn_text_ndc(wks,"Level [hPa]",vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY+3.*vpPCTY,txresC)
				
				; Graphic Title
				add_graphic_title(wks,FNAME(ppp)+"_zoom",vpLFT,vpTOP+0.023,0.010)
				
				; Time Title
				TimeTitle	= "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+\
				                  "  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR(fff)),"f","")+"]  valid at "+validDate
				add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.008,"left")
				
				; Storm Title
				add_storm_title(wks,str_upper(LONGSID),vpRGT,vpTOP+0.008,0.008,"right")
			
				; Add the Model to the Title
				if(DO_SRCLBL)then	add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.010)
							add_disclaimer(wks,plot,EXPT,resC@mpMinLonF + 50.*pctX,resC@mpMinLatF + 2.*pctY,0.009,"center")
				end if
				
				; Add marker legend to the map
				MARKER02	:= new(3,"graphic")
				TEXT03		:= new(3,"graphic")
				BOX02 	:= gsn_add_polygon(wks,plot,(/resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 15.*pctX,\
								      resC@mpMinLonF + 15.*pctX,resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 1.*pctX/),\
								    (/resC@mpMinLatF +  1.*pctY,resC@mpMinLatF +  1.*pctY,\
								      resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 1.*pctY/),\
							   pgres)
				mresC@gsMarkerColor	:= "black"
				mresC@gsMarkerIndex	= 12
				MARKER02(0)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+13.*pctY,mresC)
				TEXT03(0)	= gsn_add_text(wks,plot,"Lowest",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+13.*pctY,txres4)
				mresC@gsMarkerIndex	= 4
				MARKER02(1)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+8.*pctY,mresC)
				TEXT03(1)	= gsn_add_text(wks,plot,"Vortex",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+8.*pctY,txres4)
				mresC@gsMarkerIndex	= 5
				MARKER02(2)	= gsn_add_polymarker(wks,plot,resC@mpMinLonF+3.*pctX,resC@mpMinLatF+3.*pctY,mresC)
				TEXT03(2)	= gsn_add_text(wks,plot,"Non-Vtx?",resC@mpMinLonF+5.*pctX,resC@mpMinLatF+3.*pctY,txres4)
					
				; Draw vertical wind shear information
				; Print arrows & magnitude
				TEXT04 := new(9,"graphic")
				BOX01  := new(3,"graphic")
				sclWND = max((/SHRD,SHRS,tcM6h(fff)/))
					
				if(.not.ismissing(SHRD) .and. .not.ismissing(SHTD))then
					; Background box (transparent)
					; Place this box in the upper right
					BOX01(0) = gsn_add_polygon(wks,plot,(/resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 15.*pctX,\
									      resC@mpMaxLonF - 15.*pctX,resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 1.*pctX/),\
									    (/resC@mpMaxLatF -  1.*pctY,resC@mpMaxLatF -  1.*pctY,\
									      resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*SHTD
					txres2@txFontColor	= "blue"
					txres2@txFontHeightF	= 0.025*(SHRD/sclWND)
					TEXT04(0) = gsn_add_text(wks,plot,"~F34~-",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(1) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",SHRD))+" kt",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 13.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(2) = gsn_add_text(wks,plot,"850-200 SHR",resC@mpMaxLonF - 8.*pctX,resC@mpMaxLatF - 3.*pctY,txres3)
				end if
				if(.not.ismissing(SHRS) .and. .not.ismissing(SHTS))then
					; Background box (transparent)
					; Place this box in the upper left
					BOX01(1) = gsn_add_polygon(wks,plot,(/resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 15.*pctX,\
									      resC@mpMaxLonF - 15.*pctX,resC@mpMaxLonF -  1.*pctX,resC@mpMaxLonF - 1.*pctX/),\
									    (/resC@mpMinLatF +  1.*pctY,resC@mpMinLatF +  1.*pctY,\
									      resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 15.*pctY,resC@mpMinLatF + 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*SHTS
					txres2@txFontColor	= "green4"
					txres2@txFontHeightF	= 0.025*(SHRS/sclWND)
					TEXT04(3) = gsn_add_text(wks,plot,"~F34~-",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(4) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",SHRS))+" kt",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 3.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(5) = gsn_add_text(wks,plot,"850-500 SHR",resC@mpMaxLonF - 8.*pctX,resC@mpMinLatF + 13.*pctY,txres3)
				end if
				if(.not.ismissing(tcM6h(fff)))then
					; Background box (transparent)
					; Put this box in the lower right
					BOX01(2) = gsn_add_polygon(wks,plot,(/resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 15.*pctX,\
									      resC@mpMinLonF + 15.*pctX,resC@mpMinLonF +  1.*pctX,resC@mpMinLonF + 1.*pctX/),\
									    (/resC@mpMaxLatF -  1.*pctY,resC@mpMaxLatF -  1.*pctY,\
									      resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 15.*pctY,resC@mpMaxLatF - 1.*pctY/),\
								   pgres)
					
					; Arrow
					txres2@txAngleF		= -1.*tcM6hB(fff)
					txres2@txFontColor	= "firebrick"
					txres2@txFontHeightF	= 0.025*(tcM6h(fff)/sclWND)
					TEXT04(6) = gsn_add_text(wks,plot,"~F34~-",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 8.*pctY,txres2)
					
					; Wind speed
					txres3@txFontHeightF	= 0.010
					TEXT04(7) = gsn_add_text(wks,plot,""+str_squeeze(sprintf("%5.1f",tcM6h(fff)))+" kt",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 13.*pctY,txres3)
					
					; Label
					txres3@txFontHeightF	= 0.008
					TEXT04(8) = gsn_add_text(wks,plot,"MOTION",resC@mpMinLonF + 8.*pctX,resC@mpMaxLatF - 3.*pctY,txres3)
				end if
				
				
				; Finish (draw) the plot
				draw(plot)
				frame(wks)
				
				; Remove white space on plot using the terminal
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)
				end if  end if  end if
				
			end if ; PLOT_ON			
			
			
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Wind Shear Hodograph (200-800km) ;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Calculated from Magnitude and Heading
		else if(any((/"TCHODO"/).eq.FNAME(ppp)))then
			; 2) Set up the cartesian coordinates
			X	:= tofloat(ispan(-800,800,10))
			Y	:= tofloat(ispan(-800,800,10))
				
			; 3) Use NCL & Fortran functions to find the TC centers
			;    Check if 'CTR' exists
			;    Comment this to base the center at every level on the surface center
			;if(.not.isvar("CTR"))then   CTR := findCenter(f,DSOURCE,"HGT",LEV,iBOCO,1)   end if
			DAT_FILE2= str_sub_str(DAT_FILE,"TCHODO","TCCEN")
			if(fileexists(DAT_FILE2))then
				NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
				data	:= asciiread(DAT_FILE2,(/NL,5/),"float")
				if(any(data(:,0).eq.tofloat(FHR(fff))))then
					CTR := data(ind(data(:,0).eq.tofloat(FHR(fff))),2:3)
					LEVC := data(ind(data(:,0).eq.tofloat(FHR(fff))),1)
					LEVC@units = "hPa"
				else
					CTR := default_fillvalue("float")
					LEVC := default_fillvalue("float")
				end if
			else
				CTR := default_fillvalue("float")
				LEVC := default_fillvalue("float")
			end if
			if(all(ismissing(LEVC)))then   LEVC := LEV/mf   end if
			

			; 4A) Get U & V data (uncombined)
			;U1	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			U1 := getVar3d(f,DSOURCE,(/"U",sprinti("%0.4i",toint(min(LEVC)))+sprinti("%0.4i",toint(max(LEVC)))/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			U1@_FillValue = -999.
			;V1	:= new((/1,dimsizes(lat),dimsizes(lon)/),"float")
			V1 := getVar3d(f,DSOURCE,(/"V",sprinti("%0.4i",toint(min(LEVC)))+sprinti("%0.4i",toint(max(LEVC)))/),nDims,BOCO2,mf,(/flipFlag,False/),lonF)
			V1@_FillValue = -999.
			
				
			; 4B) Convert both U & V to cartesian (X,Y) coordinates
			U2	:= new((/dimsizes(LEVC),dimsizes(Y),dimsizes(X)/),"float",U1@_FillValue)
			V2	:= new((/dimsizes(LEVC),dimsizes(Y),dimsizes(X)/),"float",V1@_FillValue)
			if(all(ismissing(CTR)))then	SPH2CART::sph2cart(U1,U2,lon,dimsizes(lon),lat,dimsizes(lat),dimsizes(LEVC),X,dimsizes(X),Y,dimsizes(Y),\
									   conform(LEVC,tcLons(fff),-1),conform(LEVC,tcLats(fff),-1),U1@_FillValue)
							SPH2CART::sph2cart(V1,V2,lon,dimsizes(lon),lat,dimsizes(lat),dimsizes(LEVC),X,dimsizes(X),Y,dimsizes(Y),\
									   conform(LEVC,tcLons(fff),-1),conform(LEVC,tcLats(fff),-1),V1@_FillValue)
			else				SPH2CART::sph2cart(U1,U2,lon,dimsizes(lon),lat,dimsizes(lat),dimsizes(LEVC),X,dimsizes(X),Y,dimsizes(Y),\
									   CTR(:,1),CTR(:,0),U1@_FillValue)
							SPH2CART::sph2cart(V1,V2,lon,dimsizes(lon),lat,dimsizes(lat),dimsizes(LEVC),X,dimsizes(X),Y,dimsizes(Y),\
									   CTR(:,1),CTR(:,0),V1@_FillValue)
			end if

			; 4C) Average U & V in a 200-800km annulus (see SHIPS)
			U2	:= where(     sqrt(conform(U2,X,2)^2.+conform(U2,Y,1)^2.).ge.200.\
					.and.sqrt(conform(U2,X,2)^2.+conform(U2,Y,1)^2.).le.800.,\
					U2,U2@_FillValue)
			if(all(ismissing(U2)))then	U2 := new(dimsizes(LEVC),"float",U1@_FillValue)
							U2 = U1@_FillValue
			else				U2 := dim_avg_n(U2,(/1,2/))
			end if
			V2	:= where(     sqrt(conform(V2,X,2)^2.+conform(V2,Y,1)^2.).ge.200.\
					.and.sqrt(conform(V2,X,2)^2.+conform(V2,Y,1)^2.).le.800.,\
					V2,V2@_FillValue)
			if(all(ismissing(V2)))then	V2 := new(dimsizes(LEVC),"float",V1@_FillValue)
							V2 = V1@_FillValue
			else				V2 := dim_avg_n(V2,(/1,2/))
			end if

			; 4D) FOR WIND MAGNITUDE
			V3	:= sqrt(U2^2. + V2^2.)
			V3@units = "kts"
			V3@long_name = "Wind Magnitude"
			V3@_FillValue = U1@_FillValue
			if(all(ismissing(V3)))then   continue   end if
					
			; 4E) FOR WIND DIRECTION
			V4	:= C@r2d*atan2(U2,V2)
			if(any(V4.lt.0))then	V4(ind(V4.lt.0)) = 360.+V4(ind(V4.lt.0))   end if
			V4@units = "degrees"
			V4@long_name = "Wind Direction"
			if(all(ismissing(V4)))then   continue   end if

			; 4F) Delete the large arrays to release memory
			delete([/U1,V1/])
				
			; 8) Write data to a .DAT file.
			data	:= new((/dimsizes(LEVC),4/),"float")
			data(:,0) = (/tofloat(FHR(fff))/)
			data(:,1) = (/tofloat(LEVC)/)
			data(:,2) = (/V3/)
			data(:,3) = (/V4/)

			if(W.ne."NO")then
				print("MSG: Printing values for: "+FNAME(ppp))
				write_table(DAT_FILE,W,[/toint(data(:,0)), toint(data(:,1)), data(:,2), data(:,3)/], "%0.3i  %6.0i  %8.2f  %8.2f")
				system("sort -u -nk1,1 -nk2,2 "+DAT_FILE+" > "+DAT_FILE+".TMP")
				system("mv "+DAT_FILE+".TMP "+DAT_FILE)
			end if
				
			if(PLOT_ON(ppp) .and. .not.SKIP_PLOT)then
				print("MSG: Producing graphic for: "+FNAME(ppp))
				OFILE	= ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))
			
				; 5) Produce hodograph graphic
				TITLE = "Near-Storm Hodograph [kts]"
				wks	= gsn_open_wks(wtype,OFILE)
				;wks	= gsn_open_wks("x11","HODOGRAPH_TEST")
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				cmapH	:= read_colormap_file("BlAqGrYeOrReVi200")
		
				; For the map background
				HODO_MAX := 60.
				type = 57
				resH  := GenPlotRes(type,False)
				resH@trXMaxF			=  1.10*HODO_MAX
				resH@trXMinF			= -1.10*HODO_MAX
				resH@trYMaxF			=  1.10*HODO_MAX
				resH@trYMinF			= -1.10*HODO_MAX
				pctX	= 0.01*(resH@trXMaxF-resH@trXMinF)
				pctY	= 0.01*(resH@trYMaxF-resH@trYMinF)
			
				; For the markers
				mresH			= True
				mresH@gsMarkerIndex	= 4;5
				mresH@gsMarkerSizeF	= 8.0
				mresH@gsMarkerThicknessF= 2.0
			
				; For the legend
				txresH			= True
				txresH@txFontHeightF	= 0.0135
				txresH@txJust		= "CenterCenter"
				
				txres01			= True
				txres01@txFontHeightF	= 0.012
				txres01@txJust		= "CenterCenter"
				txres01@txFontColor	= "black"
				
				; For the axes
				lresH1			= True
				;lresH1@gsLineColor	= "grey40"
				lresH1@gsLineColor	= "black"
				lresH1@gsLineDashPattern= 2
				lresH1@gsLineThicknessF	= 1.0
				
				; For the axes
				lresH2			= True
				lresH2@gsLineColor	= "black"
				lresH2@gsLineDashPattern= 0
				lresH2@gsLineThicknessF	= 1.0
						
				plot := gsn_csm_blank_plot(wks,resH)
				
				
				; Retrieve viewport info from 'plot'
				getvalues plot
					"vpYF" : vpTOP
					"vpXF" : vpLFT
					"vpHeightF" : vpHGT
					"vpWidthF" : vpWID
				end getvalues
				vpRGT  = vpLFT+vpWID
				vpPCTX = 0.01*(vpRGT-vpLFT)
				vpBOT  = vpTOP-vpHGT
				vpPCTY = 0.01*(vpTOP-vpBOT)
				
				; Add the X and Y axes
				LINE01	:= new(2,"graphic")
				LINE01(0)	= gsn_add_polyline(wks,plot,(/resH@trXMinF,resH@trXMaxF/),(/avg((/resH@trYMaxF,resH@trYMinF/)),avg((/resH@trYMaxF,resH@trYMinF/))/),lresH1)
				LINE01(1)	= gsn_add_polyline(wks,plot,(/avg((/resH@trXMaxF,resH@trXMinF/)),avg((/resH@trXMaxF,resH@trXMinF/))/),(/resH@trYMinF,resH@trYMaxF/),lresH1)
				circle_ll(wks,plot,0.,0., 0.25*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 0.50*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 0.75*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 1.00*HODO_MAX,lresH1)
				TEXT01	:= new(8,"graphic")
				TEXT01(0)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.25*HODO_MAX)),".0",""),0.25*HODO_MAX,0.,txres01)
				TEXT01(1)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.50*HODO_MAX)),".0",""),0.50*HODO_MAX,0.,txres01)
				TEXT01(2)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.75*HODO_MAX)),".0",""),0.75*HODO_MAX,0.,txres01)
				TEXT01(3)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",1.00*HODO_MAX)),".0",""),1.00*HODO_MAX,0.,txres01)
				TEXT01(4)	= gsn_add_text(wks,plot,"E",resH@trXMaxF - 1.5*pctX,0.,txres01)
				TEXT01(5)	= gsn_add_text(wks,plot,"N",0.,resH@trYMaxF - 1.5*pctY,txres01)
				TEXT01(6)	= gsn_add_text(wks,plot,"W",resH@trXMinF + 1.5*pctX,0.,txres01)
				TEXT01(7)	= gsn_add_text(wks,plot,"S",0.,resH@trYMinF + 1.5*pctY,txres01)
				
				; Add markers, lines and legend
				MARKER01:= new(dimsizes(LEVC),"graphic")
				LINE02	:= new(dimsizes(LEVC),"graphic")
				do kkk = 0,dimsizes(LEVC)-1
					if(max(LEVC).ne.LEVC(0))then   kkk2 = dimsizes(LEVC)-kkk-1   end if
					
					if(kkk.gt.0)then	MX2 = MX
								MY2 = MY
					end if
					MX = V3(kkk2)*sin(C@d2r*V4(kkk2))
					MY = V3(kkk2)*cos(C@d2r*V4(kkk2))
					
					if(.not.ismissing(MX) .and. .not.ismissing(MY))then
						mresH@gsMarkerColor	:= cmapH(kkk2*dimsizes(cmapH(:,0))/dimsizes(LEVC),:)
						txresH@txFontColor	:= cmapH(kkk2*dimsizes(cmapH(:,0))/dimsizes(LEVC),:)
						if(LEV(kkk2).eq.max(LEV))then		mresH@gsMarkerIndex = 12
						else if(LEV(kkk2).eq.min(LEV))then	mresH@gsMarkerIndex = 6
						else					mresH@gsMarkerIndex = 4
						end if  end if
						MARKER01(kkk2)	= gsn_add_polymarker(wks,plot,MX,MY,mresH)
						if(kkk.gt.0)then   LINE02(kkk2) = gsn_add_polyline(wks,plot,(/MX2,MX/),(/MY2,MY/),lresH2)   end if
						gsn_text_ndc(wks,level_convert(LEVC(kkk2),LEVC@units,"string","hPa"),vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY-2.5*vpPCTY*tofloat(kkk2),txresH)
					end if
				end do

				txresH@txFontColor	:= "black"
				gsn_text_ndc(wks,"Level [hPa]",vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY+3.0*vpPCTY,txresH)
				
				; Add motion vector
				plres2		= True
				plres2@gsLineColor = "black"
				plres2@gsLineThicknessF = 3.0
				if(.not.ismissing(tcM6h(fff)))then	MMX = tcM6h(fff)*sin(C@d2r*tcM6hB(fff))
									MMY = tcM6h(fff)*cos(C@d2r*tcM6hB(fff))
									plres2@gsLineThicknessF = 3.0
									ARROW01 = arrow(wks,plot,(/0.,MMX/),(/0.,MMY/),plres2)
				end if
				
				; Add marker legend to the map
				MARKER02	:= new(2,"graphic")
				TEXT03		:= new(4,"graphic")
				mresC@gsMarkerColor	:= "black"
				mresC@gsMarkerIndex	= 12
				MARKER02(0)	= gsn_add_polymarker(wks,plot,resH@trXMinF+3.*pctX,resH@trYMaxF-3.*pctY,mresC)
				TEXT03(0)	= gsn_add_text(wks,plot,"Lowest",resH@trXMinF+5.*pctX,resH@trYMaxF-3.*pctY,txres4)
				mresC@gsMarkerIndex	= 6
				MARKER02(1)	= gsn_add_polymarker(wks,plot,resH@trXMinF+3.*pctX,resH@trYMaxF-8.*pctY,mresC)
				TEXT03(1)	= gsn_add_text(wks,plot,"Highest",resH@trXMinF+5.*pctX,resH@trYMaxF-8.*pctY,txres4)
				; Arrow
				txres2@txAngleF		= 0.
				txres2@txFontColor	= "black"
				txres2@txFontHeightF	= 0.012
				TEXT03(3) 	= gsn_add_text(wks,plot,"~F34~-",resH@trXMinF + 3.*pctX,resH@trYMaxF-13.*pctY,txres2)
				TEXT03(2)	= gsn_add_text(wks,plot,"Motion",resH@trXMinF+5.*pctX,resH@trYMaxF-13.*pctY,txres4)
				
				; Graphic Title
				add_graphic_title(wks,FNAME(ppp),vpLFT,vpTOP+0.023,0.010)
				
				; Time Title
				TimeTitle	= "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+\
				                  "  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR(fff)),"f","")+"]  valid at "+validDate
				add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.008,"left")
				
				; Storm Title
				add_storm_title(wks,str_upper(LONGSID),vpRGT,vpTOP+0.008,0.008,"right")
				
				; Add the Model to the Title
				if(DO_SRCLBL)then	add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.010)
							add_disclaimer(wks,plot,EXPT,resH@trXMinF + 2.*pctX,resH@trYMinF + 2.*pctY,0.009,"left")
				end if
				
				; Finish (draw) the plot
				draw(plot)
				frame(wks)
				
				; Remove white space on plot using the terminal
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)
				end if  end if  end if
				
				
				print("MSG: Producing graphic for: "+FNAME(ppp)+"_zoom")
				; For the zoomed hodograph background
				OFILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"_zoom."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i",FHR(fff))
				HODO_ZOOM = False
				HODO_MAX := (/5.,10.,20.,30.,50.,80./)
				HN = 0
				do while (.not.HODO_ZOOM)
					if(max(V3).gt.HODO_MAX(HN))then		HN=HN+1
					else					resH@trXMaxF	=  1.10*HODO_MAX(HN)
										resH@trXMinF	= -1.10*HODO_MAX(HN)
										resH@trYMaxF	=  1.10*HODO_MAX(HN)
										resH@trYMinF	= -1.10*HODO_MAX(HN)
										HODO_ZOOM = True
					end if
					if(HN.gt.dimsizes(HODO_MAX)-1)then	break	end if
				end do
				if(.not.HODO_ZOOM)then   continue   end if
				HODO_MAX := HODO_MAX(HN)
				pctX	= 0.01*(resH@trXMaxF-resH@trXMinF)
				pctY	= 0.01*(resH@trYMaxF-resH@trYMinF)
				
				; 5) Produce zoomed hodograph graphic
				TITLE = "Near-Storm Hodograph (zoom) [kts]"
				wks	= gsn_open_wks(wtype,OFILE)
				;wks	= gsn_open_wks("x11","HODOGRAPH_ZOOM_TEST")
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				plot	:= gsn_csm_blank_plot(wks,resH)
				;print(data(:,0)+"  "+data(:,1)+"  "+data(:,2))			
				
				; Retrieve viewport info from 'plot'
				getvalues plot
					"vpYF" : vpTOP
					"vpXF" : vpLFT
					"vpHeightF" : vpHGT
					"vpWidthF" : vpWID
				end getvalues
				vpRGT  = vpLFT+vpWID
				vpPCTX = 0.01*(vpRGT-vpLFT)
				vpBOT  = vpTOP-vpHGT
				vpPCTY = 0.01*(vpTOP-vpBOT)
				
				; Add the X and Y axes
				LINE01	:= new(2,"graphic")
				LINE01(0)	= gsn_add_polyline(wks,plot,(/resH@trXMinF,resH@trXMaxF/),(/avg((/resH@trYMaxF,resH@trYMinF/)),avg((/resH@trYMaxF,resH@trYMinF/))/),lresH1)
				LINE01(1)	= gsn_add_polyline(wks,plot,(/avg((/resH@trXMaxF,resH@trXMinF/)),avg((/resH@trXMaxF,resH@trXMinF/))/),(/resH@trYMinF,resH@trYMaxF/),lresH1)
				circle_ll(wks,plot,0.,0., 0.25*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 0.50*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 0.75*HODO_MAX,lresH1)
				circle_ll(wks,plot,0.,0., 1.00*HODO_MAX,lresH1)
				TEXT01	:= new(8,"graphic")
				TEXT01(0)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.25*HODO_MAX)),".0",""),0.25*HODO_MAX,0.,txres01)
				TEXT01(1)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.50*HODO_MAX)),".0",""),0.50*HODO_MAX,0.,txres01)
				TEXT01(2)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",0.75*HODO_MAX)),".0",""),0.75*HODO_MAX,0.,txres01)
				TEXT01(3)	= gsn_add_text(wks,plot,str_sub_str(str_squeeze(sprintf("%2.1f",1.00*HODO_MAX)),".0",""),1.00*HODO_MAX,0.,txres01)
				TEXT01(4)	= gsn_add_text(wks,plot,"E",resH@trXMaxF - 1.5*pctX,0.,txres01)
				TEXT01(5)	= gsn_add_text(wks,plot,"N",0.,resH@trYMaxF - 1.5*pctY,txres01)
				TEXT01(6)	= gsn_add_text(wks,plot,"W",resH@trXMinF + 1.5*pctX,0.,txres01)
				TEXT01(7)	= gsn_add_text(wks,plot,"S",0.,resH@trYMinF + 1.5*pctY,txres01)
				
				; Place markers and legend
				MARKER01:= new(dimsizes(LEV),"graphic")
				LINE02	:= new(dimsizes(LEV),"graphic")
				do kkk = 0,dimsizes(LEVC)-1
					if(max(LEVC).ne.LEVC(0))then   kkk2 = dimsizes(LEVC)-kkk-1   end if
					
					if(kkk.gt.0)then	MX2 = MX
								MY2 = MY
					end if
					MX = V3(kkk2)*sin(C@d2r*V4(kkk2))
					MY = V3(kkk2)*cos(C@d2r*V4(kkk2))
					
					if(.not.ismissing(MX) .and. .not.ismissing(MY))then
						mresH@gsMarkerColor	:= cmapH(kkk2*dimsizes(cmapH(:,0))/dimsizes(LEVC),:)
						txresH@txFontColor	:= cmapH(kkk2*dimsizes(cmapH(:,0))/dimsizes(LEVC),:)
						if(LEV(kkk2).eq.max(LEV))then		mresH@gsMarkerIndex = 12
						else if(LEV(kkk2).eq.min(LEV))then	mresH@gsMarkerIndex = 6
						else					mresH@gsMarkerIndex = 4
						end if  end if
						MARKER01(kkk2)	= gsn_add_polymarker(wks,plot,MX,MY,mresH)
						if(kkk.gt.0)then   LINE02(kkk2) = gsn_add_polyline(wks,plot,(/MX2,MX/),(/MY2,MY/),lresH2)   end if
						gsn_text_ndc(wks,level_convert(LEVC(kkk2),LEVC@units,"string","hPa"),vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY-2.5*vpPCTY*tofloat(kkk2),txresH)
					end if
				end do
				txresH@txFontColor	:= "black"
				gsn_text_ndc(wks,"Level [hPa]",vpRGT+10.*vpPCTX,vpTOP-5.*vpPCTY+3.*vpPCTY,txresH)
				
				; Add motion vector
				if(.not.ismissing(tcM6h(fff)))then	MMX = tcM6h(fff)*sin(C@d2r*tcM6hB(fff))
									MMY = tcM6h(fff)*cos(C@d2r*tcM6hB(fff))
									plres2@gsLineThicknessF = 6.0
									ARROW01 = arrow(wks,plot,(/0.,MMX/),(/0.,MMY/),plres2)
				end if
				
				; Add marker legend to the map
				MARKER02	:= new(2,"graphic")
				TEXT03		:= new(4,"graphic")
				mresC@gsMarkerColor	:= "black"
				mresC@gsMarkerIndex	= 12
				MARKER02(0)	= gsn_add_polymarker(wks,plot,resH@trXMinF+3.*pctX,resH@trYMaxF-3.*pctY,mresC)
				TEXT03(0)	= gsn_add_text(wks,plot,"Lowest",resH@trXMinF+5.*pctX,resH@trYMaxF-3.*pctY,txres4)
				mresC@gsMarkerIndex	= 6
				MARKER02(1)	= gsn_add_polymarker(wks,plot,resH@trXMinF+3.*pctX,resH@trYMaxF-8.*pctY,mresC)
				TEXT03(1)	= gsn_add_text(wks,plot,"Highest",resH@trXMinF+5.*pctX,resH@trYMaxF-8.*pctY,txres4)
				; Arrow
				txres2@txAngleF		= 0.
				txres2@txFontColor	= "black"
				txres2@txFontHeightF	= 0.012
				TEXT03(3) 	= gsn_add_text(wks,plot,"~F34~-",resH@trXMinF + 3.*pctX,resH@trYMaxF-13.*pctY,txres2)
				TEXT03(2)	= gsn_add_text(wks,plot,"Motion",resH@trXMinF+5.*pctX,resH@trYMaxF-13.*pctY,txres4)
					
				; Graphic Title
				add_graphic_title(wks,FNAME(ppp),vpLFT,vpTOP+0.023,0.010)
				
				; Time Title
				TimeTitle	= "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+\
				                  "  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR(fff)),"f","")+"]  valid at "+validDate
				add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.008,"left")
				
				; Storm Title
				add_storm_title(wks,str_upper(LONGSID),vpRGT,vpTOP+0.008,0.008,"right")
				
				; Add the Model to the Title
				if(DO_SRCLBL)then	add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.010)			
							add_disclaimer(wks,plot,EXPT,resH@trXMinF + 2.*pctX,resH@trYMinF + 2.*pctY,0.009,"left")
				end if
				
				; Finish (draw) the plot
				draw(plot)
				frame(wks)
				
				; Remove white space on plot using the terminal
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+"."+wtype+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+"."+wtype)
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+"."+wtype+" "+OFILE+"."+wtype)
				end if  end if  end if
				
			end if ; ppp
				
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if
			
			
			
		;;;;;;;;;;;;;;;;;;;;;;;;;
		; PLOT PARAMETER TRENDS ;
		;;;;;;;;;;;;;;;;;;;;;;;;;
		; 10) Create graphic after final forecast hour
		if(PLOT_ON(ppp) .and.\
		   FHR(fff).eq.max(FHR) .and.\
		   .not.any((/"TCCEN","TCHODO"/).eq.FNAME(ppp)))then
			;================================================================
			; Produce trends for current variable.
			;================================================================
			print("MSG: Producing graphic for: "+FNAME(ppp))
			
			; These are plotting variables
			IDATE2	:= new(nTrend,"string")
			Vp	:= new((/nTrend,100/),"float")
			FHR2	:= new((/nTrend,100/),"float")
					
			; Get the data from this and previous forecasts
			tmp	:= tochar(IDATE)
			iii	= 0
			do ddd = -1*6*(nTrend-1),0,6
				if(ddd.eq.0)then	IDATE2(iii) = IDATE
							DAT_FILE2 = DAT_FILE
							DAT_FILE3 = ""
				else			; Update IDATE to be Valid Time instead of Initial Time
							utc	= cd_inv_calendar(toint(YYYY),toint(MM),toint(DD),toint(HH),0,0,tunits,0)
							utc	= utc+tofloat(ddd)
							myDate	:= tostring(toint(cd_calendar(utc,3)))
							IDATE2(iii) = myDate
							DAT_FILE2 = str_sub_str(DAT_FILE,IDATE,IDATE2(iii))
							DAT_FILE3 = str_sub_str(DAT_FILE2,str_lower(LONGSID),"invest"+str_lower(SID_OLD))
				end if
				
				if(fileexists(DAT_FILE2))then
					NL	:= dimsizes(asciiread(DAT_FILE2,-1,"string"))
					data	:= asciiread(DAT_FILE2,(/NL,2/),"float")
					vals := dim_pqsort_n(data(:,0),1,0)
					FHR2(iii,:NL-1)	= data(vals,0)+tofloat(ddd)
					Vp(iii,:NL-1)	= data(vals,1)
				else if(fileexists(DAT_FILE3))then
					NL	:= dimsizes(asciiread(DAT_FILE3,-1,"string"))
					data	:= asciiread(DAT_FILE3,(/NL,2/),"float")
					vals := dim_pqsort_n(data(:,0),1,0)
					FHR2(iii,:NL-1)	= data(vals,0)+tofloat(ddd)
					Vp(iii,:NL-1)	= data(vals,1)
				end if  end if
						
				iii=iii+1
			end do
			Vp = where(Vp.gt.10.e10 .or. Vp.lt.-10.e10,Vp@_FillValue,Vp)
					
			; Find plot boundaries. Resize if necessary
			if(isvar("IntMjrGrd"))then   delete(IntMjrGrd)   end if
			if(any((/"SHTD","SHTS"/).eq.FNAME(ppp)))then
				IntMjrGrd := ispan(0,360,90)
			else if(any((/"RHLO","RHMD","RHHI"/).eq.FNAME(ppp)))then
				IntMjrGrd := ispan(0,100,20)
			else
				if(any((/"VMAX","IKE34","IKE50","IKE64","SHRD","SHRS"/).eq.FNAME(ppp)))then	minInt = 0.
														maxInt = ceil(max(Vp)+0.10*max(Vp))
				else if(any((/"PENV","MSLP"/).eq.FNAME(ppp)))then				minInt = floor(min(Vp))
														maxInt = ceil(max(Vp))
				else										minInt = floor(min(Vp)-0.10*abs(min(Vp)))
														maxInt = ceil(max(Vp)+0.10*abs(max(Vp)))
				end if  end if
				rngInt	:= abs(maxInt-minInt)
				if(maxInt.eq.minInt)then
					IntMjrGrd := fspan(0.,1.,5)
				else
					dInt = 0.
					dInt_rng = (/0.00001,0.0001,0.001,0.01,0.1,1.0,10.0,100.0,1000.0,10000.0/)
					dInt_div = (/10.0,9.0,8.0,7.0,6.0,5.0,4.0/)
					hhh = 0
					do while(dInt.eq.0 .and. hhh.le.dimsizes(dInt_div)-1)
						dInt_guess = rngInt/dInt_div(hhh)
						
						iii = 0
						do while(dInt.eq.0 .and. iii.le.dimsizes(dInt_rng)-2)
						;do iii = 0,dimsizes(dInt_rng)-2
							;print(dInt_guess+"  "+dInt_div(hhh)+"  "+dInt_rng(iii))	
							if(dInt_guess.ge.dInt_rng(iii) .and. dInt_guess.lt.dInt_rng(iii+1))then
								dInt = floor(dInt_guess/dInt_rng(iii))*dInt_rng(iii)
							end if
							if(dInt.eq.0)then	iii = iii+1
										continue
							end if
							
							IntMjrGrd := minInt
							do while(max(IntMjrGrd).lt.maxInt)
								IntMjrGrd := array_append_record(IntMjrGrd,max(IntMjrGrd)+dInt,0)
							end do
							
							if(dimsizes(IntMjrGrd).gt.10)then	dInt = 0
												delete(IntMjrGrd)
							end if
							iii = iii+1
						end do
						hhh = hhh+1
					end do
				end if
				if(dInt.eq.0 .and. .not.isvar("IntMjrGrd"))then
					print("WARNING: No suitable major grid array could be found.")
					IntMjrGrd = fspan(min(Vp),max(Vp),8)
				end if

			end if  end if
			myHrs	:= ispan(-24,toint(max(FHR2)),12)
	
			; Set up the output file
			OFILE	= ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN
			wks	= gsn_open_wks(wtype,OFILE)
                        setvalues NhlGetWorkspaceObjectId()
                                "wsMaximumSize" : 500000000
                        end setvalues

			; Update resources
			res@tiMainString	= "";TITLE
			res@tiYAxisString	= V1@long_name+" ["+V1@units+"]"
			res@tiYAxisFontHeightF	= 0.015	
			res@tiXAxisFontHeightF	= 0.015	
			res@tmXBValues		= myHrs
			res@tmXBLabels		= "" + res@tmXBValues
			res@tmXBLabelFontHeightF= 0.015
			res@tmYLLabelFontHeightF= 0.015
			res@trXMinF		= min(myHrs)-6
			res@trXMaxF		= max(myHrs)+6
			res@tmYLValues		= IntMjrGrd
			res@tmYLLabels		= "" + res@tmYLValues
			res@trYMaxF		= max(IntMjrGrd)
			res@trYMinF		= min(IntMjrGrd)

			; Add values at each time
			if(isvar("plot"))then   delete(plot)   end if
			do ddd = 0,nTrend-1

				tmp	:= toint(where(ismissing(FHR2(ddd,:)), fval2, FHR2(ddd,:)))
				vals	:= ind(tmp%6.eq.0)

				; Check if the model data for previous forecasts is available
				if(all(ismissing(Vp(ddd,:))) .or. dimsizes(vals).eq.1)then
					print("  MSG: "+FNAME(ppp)+" unavailable for "+IDATE2(ddd))
					continue
				end if
				print("  MSG: Adding "+FNAME(ppp)+" for "+IDATE2(ddd))
	
				; Update resources
				res@xyMarkLineMode	= "Lines"
				res@xyLineThicknessF	= 12.0
				if(dimsizes(TRDcolors).eq.2)then	res@xyLineColor := TRDcolors(ddd,:)
				else					res@xyLineColor := TRDcolors(ddd)
				end if
	
				; Overlay the new intensity line
				if(.not.isvar("plot"))then	plot	= gsn_csm_xy(wks,FHR2(ddd,vals),Vp(ddd,vals),res)
				else				overlay(plot,gsn_csm_xy(wks,FHR2(ddd,vals),Vp(ddd,vals),res))
				end if
	
				; Add legend info for each model
				plres@gsLineThicknessF	= 10.0
				if(dimsizes(TRDcolors).eq.2)then	plres@gsLineColor := TRDcolors(ddd,:)
				else					plres@gsLineColor := TRDcolors(ddd)
				end if
				plres@gsLineDashPattern	= 0
				plres@tfPolyDrawOrder	= "PostDraw"
	
				gsn_polyline_ndc(wks,(/xleg(ddd)-0.02,xleg(ddd)+0.02/),(/yleg(ddd),yleg(ddd)/),plres)
				gsn_text_ndc(wks,IDATE2(ddd),xtxt(ddd),ytxt(ddd),txres)
			end do

			if(isvar("plot"))then
				; Plot a vertical black line at 0 h
				LINE01	:= new(dimsizes(myHrs),"graphic")
				LINE02	:= new(dimsizes(IntMjrGrd),"graphic")
						
				plres@gsLineThicknessF	= 10.0
				plres@gsLineColor	:= "black"
				plres@gsLineDashPattern	= 0
				plres@tfPolyDrawOrder	= "PreDraw"
				LINE01(ind(myHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/res@trYMinF,res@trYMaxF/),plres)
	
				plres@gsLineThicknessF	= 1.5
				plres@gsLineDashPattern	= 11
				plres@gsLineColor	= "grey70"
				do ggg = 0,dimsizes(myHrs)-1
					if(myHrs(ggg).eq.0)then continue end if
					LINE01(ggg)	= gsn_add_polyline(wks,plot,(/tofloat(myHrs(ggg)),tofloat(myHrs(ggg))/),(/res@trYMinF,res@trYMaxF/),plres)
				end do
				do ggg = 0,dimsizes(IntMjrGrd)-1
					LINE02(ggg)	= gsn_add_polyline(wks,plot,(/res@trXMinF,res@trXMaxF/),(/IntMjrGrd(ggg),IntMjrGrd(ggg)/),plres)
				end do
					
				; Retrieve viewport info from 'plot'
				getvalues plot
					"vpYF" : vpTOP
					"vpXF" : vpLFT
					"vpHeightF" : vpHGT
					"vpWidthF" : vpWID
				end getvalues
				vpRGT  = vpLFT+vpWID
				vpPCTX = 0.01*(vpRGT-vpLFT)
				vpBOT  = vpTOP-vpHGT
				vpPCTY = 0.01*(vpTOP-vpBOT)
				
				; Graphic Title
				add_graphic_title(wks,TITLE,vpLFT,vpTOP+0.045,0.015)
					
				; Time Title
				TimeTitle	= "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)
				add_time_title(wks,TimeTitle,vpRGT,vpTOP+0.023,0.012,"right")
					
				; Storm Title
				add_storm_title(wks,str_upper(LONGSID),vpRGT,vpTOP+0.008,0.012,"right")
					
				; Add the Model to the Title & Disclaimer to the bottom
				if(DO_SRCLBL)then	add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.065,0.016)
							add_disclaimer(wks,plot,EXPT,-24.,min(IntMjrGrd)+0.05*abs(max(IntMjrGrd)-min(IntMjrGrd)),0.010,"left")
				end if
	
				; Finish plot
				draw(plot)
				frame(wks)
	
				; Remove white space on plot
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+".png "+OFILE+".png")   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
			end if
			
		end if		
		
		ppp2 = ppp2+1
	end if
	end do ;ppp
	
	
	
	; Indicate that file has been processed.
	; Check that all variables for this graphic have been produced.
	; Loop over all expected graphics to check for missing graphics
	print("MSG: Checking that all graphics were produced for current forecast hour ("+sprinti("%0.3i", FHR(fff))+").")
	FILE_DONE = True
	do ppp = 0,dimsizes(FNAME)-1
		if(PLOT_ON(ppp))then
			if(any((/"TCCEN","TCHODO"/).eq.FNAME(ppp)))then
				PNG_FILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i", FHR(fff))+".gif"
			else
				if(FHR(fff).ne.max(FHR))then   continue   end if
				if(FHR(fff).eq.0 .and. dimsizes(FHR).eq.1)then   continue   end if
				PNG_FILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".gif"
			end if
			p := systemfunc("ls "+PNG_FILE+" 2>/dev/null")
			if(ismissing(p(0)))then
				print("MSG: Graphics not found for "+FNAME(ppp)+".")
				FILE_DONE = False
				break
			end if
		end if
		if(DATA_ON(ppp))then
			DAT_FILE = ODIR+str_lower(LONGSID)+"."+FNAME(ppp)+"."+IDATE+"."+DOMAIN+".dat"
			if(FNAME(ppp).eq."TCCEN")then		NCOL = 5
			else if(FNAME(ppp).eq."TCHODO")then	NCOL = 4
			else					NCOL = 2
			end if  end if
			; 1) Check if the file exists. If YES, change the write option
			;    from overwrite (w) to append (a). Check if the data for
			;    this particular forecast hour has been written. 
			if(fileexists(DAT_FILE))then; .and. fileexists(oFile))then
				NL	:= dimsizes(asciiread(DAT_FILE,-1,"string"))
				data	:= asciiread(DAT_FILE,(/NL,NCOL/),"float")
				if(any(toint(data(:,0)).eq.FHR(fff)))then
					continue
				else	print("MSG: Data entry not found for "+FNAME(ppp)+" at "+sprinti("%0.3i", FHR(fff))+"-h.")
					FILE_DONE = False
					break
				end if
			end if
		end if
		
	end do ;ppp
	
	
	; Update PlottedFiles file if all files are done and:
	;    -- ATCF is required and forecast hour is less than or equal to the minimum hour from ATCFs
	;    -- ATCF is not required
	if(FILE_DONE)then
		print("MSG: All graphics & data files found for the current forecast hour ("+sprinti("%0.3i", FHR(fff))+").")

		; Get the input file base name
		IFILE_BASE := str_split(iFiles(myInd(0)),"/")
		IFILE_BASE := IFILE_BASE(dimsizes(IFILE_BASE)-1)

		PLOTTED_FILES = ODIR+"PlottedFiles."+DOMAIN+"."+TIER+SIDtag+".log"
		;if(ATCF_REQD .and. ALL_ATCF)then
		if(FHR(fff).le.FNL_HR)then
			if(fileexists(PLOTTED_FILES))then
				system("sed -i '/"+IFILE_BASE+"/d' "+PLOTTED_FILES)
				system("echo "+str_get_dq()+iFiles(myInd(0))+" 1"+str_get_dq()+" >> "+PLOTTED_FILES)
			else
				system("echo "+str_get_dq()+iFiles(myInd(0))+" 1"+str_get_dq()+" > "+PLOTTED_FILES)
			end if
		end if
		system("sort -u "+PLOTTED_FILES+" > "+PLOTTED_FILES+".TMP")
		system("mv "+PLOTTED_FILES+".TMP "+PLOTTED_FILES)
	end if
	
end do ; fff
end if ; DO_PLOT




; IF YOU GOT HERE, THEN THE SCRIPT HAS FINISHED RUNNING WITHOUT
; FATAL, DEBILITATING ERRORS. WOW! I DIDN'T THINK ANYONE WOULD
; ACTUALLY GET THIS FAR. LET'S REJOICE! Cheers!
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
if(.not.FNL_ATCF)then
	print("")
	print("MSG: INCOMPLETE! Graphics were produced, but might be missing info from ATCFs.")
	print("MSG: Graphics will be reproduced once ATCFs become available.")
	system("echo 'incomplete' > "+STATUS_FILE)
else if(.not.all(FNL_IFILES))then
	print("")
	print("MSG: INCOMPLETE! Graphics were produced, but new input files might be available.")
	print("MSG: If new input files are found, a new session will be initiated.")
	system("echo 'incomplete' > "+STATUS_FILE)
else
	print("")
	print("MSG: COMPLETE! Graphics were produced! Way to go! You rock!")
	;print("MSG: This forecast will be double-checked for completeness.")
	system("echo 'complete' > "+STATUS_FILE)	
end if  end if
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")




;================================================================
; PART IV. Print some information to the terminal
;================================================================

print("")
print("MSG: Ran GPLOT with these settings:")
print("********************")
if(IS_MSTORM)then	print("MSG:   Multi-Storm:        ON")
else			print("MSG:   Multi-Storm:        OFF")
end if
print("MSG:   Experiment Name:    "+EXPT)
print("MSG:   Data Source:        "+DSOURCE)
print("MSG:   Forecast Cycle:     "+IDATE)
print("MSG:   Storm ID:           "+SID)
print("MSG:   Domain/Region:      "+DOMAIN)
print("MSG:   Graphics Tier:      "+TIER)
if(ENSID.ne."")then	print("MSG:   Ensemble Member:    "+ENSID)	end if
print("MSG:   Input Directory:    "+IDIR)
print("MSG:   Output Directory:   "+ODIR)
print("MSG:   System Environment: "+MACHINE)
print("MSG:   Input File Tag(s):  "+stringOut(ITAG))
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
print("MSG:   Hour Format:        "+FMT_HR)
print("MSG:   Time Step [h]:      "+DT)
if(ATCF_REQD)then	print("MSG:   ATCF Required:      ON")
else			print("MSG:   ATCF Required:      OFF")
end if
if(DO_RMWHITE)then	print("MSG:   Trim graphics:      ON")
else			print("MSG:   Trim graphics:      OFF")
end if
if(DO_SRCLBL)then	print("MSG:   Source Labels:      ON")
else			print("MSG:   Source Labels:      OFF")
end if
if(DO_CONVERTGIF)then	print("MSG:   Convert to GIF:     ON")
else			print("MSG:   Convert to GIF:     OFF")
end if
print("MSG:   Plotted Files file: "+PLOTTED_FILES)
print("MSG:   Status file:        "+STATUS_FILE)
print("********************")



;================================================================
; End of file.
;================================================================

print("")
print("MSG: GPLOT_ships.ncl succeeded at "+systemfunc("date"))

exit

end

