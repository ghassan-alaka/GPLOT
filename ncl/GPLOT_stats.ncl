;================================================================
;
; File Name: GPLOT_stats.ncl
;
; Description:	This script creates large-scale plots from model
;		output. The format is PNG. White space is removed
;		from the image edge. A namelist file controls the
;		fields that are plotted. Plot titles must be
;		added manually.
;
; Namelist:	/home/Ghassan.Alaka/NCL/scripts/namelist.GPLOT
;
; Created By:    Ghassan Alaka Jr.
; Date Created:  July 20, 2016
; Last Modified: February 23, 2018
;
; V1:		original
; V2:		Tweaked code to allow for model input. Now, I
;		can switch between "HB15" and "HB16" with ease.
; V3:		Code now checks for a raw, early ATCF to draw
;		partial track/intensity forecasts plots.
; V4:		Added real-time verification calculation and
;		plotting.
; V5:		Added pressure-based guidance.
;		Refined graphics
; V6:		Added early model guidance and verification
; V7:		Added lifetime track & intensities for HB17
;		Cleaned up queueing logic
; V8:		Extended lifetime track & intensity to all trend models
; 2018-02-23 --> file moved to GPLOT_stats.v1.ncl
; 2018-06-05 --> bug fixed by Mu-Chieh Ko 
;
;================================================================
;
;================================================================
; Load scripts
;================================================================

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$GPLOT_DIR/ncl/GPLOT_func.ncl"
load "$GPLOT_DIR/ncl/GPLOT_util.ncl"



;================================================================
; Begin the code.
;================================================================

begin

print("MSG: GPLOT Graphics Package - Stats V1 (for Guidance & Verification)")
print("MSG: Written by Ghassan Alaka [AOML/HRD & UMiami/CIMAS]")
print("MSG: All queries should be sent to Ghassan.Alaka@noaa.gov")
print("MSG: Last recorded update:  2018-03-01")
print("MSG: Start Time:  "+systemfunc("date"))


system("export LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:$LD_LIBRARY_PATH")


;================================================================
; PART I. Set constants. DO NOT CHANGE.
;================================================================

g		= 9.80665
pi		= 3.14159265359
d2r		= pi/180.
r		= 6371000.
omega		= 0.0000727
ms2kts		= 1.94384449
fval		= 9.96921e+36
fval2		= -2147483647
fval3		= 1.0e+20
tunits		= "hours since 1970-01-01 00:00;00"
C		= load_constants()




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


; 2. Get the user from the environment
USER = systemfunc("echo $USER")


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
if(.not.isvar("MASTER_NML_IN"))then	MASTER_NML = GPLOT_DIR+"/nmlist/namelist.master.default"
else					MASTER_NML = GPLOT_DIR+"/nmlist/"+MASTER_NML_IN
end if
print("MSG: Using this master namelist:  "+MASTER_NML)


; 2. Read the master namelist into NCL
NML_DATA = asciiread(MASTER_NML,-1,"string")


; 3. Get basic variables from the master namelist:
;    DOMAIN, TIER, DSOURCE, SYS_ENV, IDATE, SID, ATCF_REQD
;    First, check if basic variables should be automatically defined (AUTO_VAR).
;    Any of these variables can be overwritten by command line arguments.
AUTO_VAR	= NML_DATA(ind(isStrSubset2(NML_DATA,"AUTO_VAR =")))
AUTO_VAR	:= str_squeeze(str_split(AUTO_VAR," = "))
AUTO_VAR	:= where(AUTO_VAR(1).eq."True",True,False)
if(AUTO_VAR)then	print("MSG: Auto-Variable functionality is ON. Ignoring basic settings in namelist.")
			print("MSG: Please change namelist if this is not desired.")
			if(isvar("DSOURCE"))then	print("MSG: Detected command line variable: DSOURCE")
			else				DSOURCE = "HWRF"
			end if
			if(isvar("SYS_ENV"))then	print("MSG: Detected command line variable: SYS_ENV")
			else				SYS_ENV = "JET"
			end if
			if(isvar("IDATE"))then		print("MSG: Detected command line variable: IDATE")
			else				IDATE   = ""
			end if
			if(isvar("SID"))then		print("MSG: Detected command line variable: SID")
			else				SID     = "NONE"
			end if
			if(isvar("FORCE"))then		print("MSG: Detected command line variable: FORCE")
			else				FORCE = False
			end if

else			print("MSG: Auto-Variable functionality is OFF. Using basic settings in namelist.")
			print("MSG: Please change namelist if this is not desired.")
				
			if(isvar("DSOURCE"))then	print("MSG: Detected command line variable: DSOURCE")
			else				DSOURCE	= NML_DATA(ind(isStrSubset2(NML_DATA,"DSOURCE =")))
							DSOURCE	:= str_squeeze(str_split(DSOURCE," = "))
							DSOURCE	:= DSOURCE(1)
			end if
			
			if(isvar("SYS_ENV"))then	print("MSG: Detected command line variable: SYS_ENV")
			else				SYS_ENV	= NML_DATA(ind(isStrSubset2(NML_DATA,"SYS_ENV =")))
							SYS_ENV	:= str_squeeze(str_split(SYS_ENV," = "))
							SYS_ENV	:= SYS_ENV(1)
			end if
			
			if(isvar("IDATE"))then		print("MSG: Detected command line variable: IDATE")
			else				IDATE	= NML_DATA(ind(isStrSubset2(NML_DATA,"IDATE =")))
							IDATE	:= str_squeeze(str_split(IDATE," = "))
							if(dimsizes(IDATE).eq.2)then	IDATE := IDATE(1)
							else				IDATE := ""
							end if
			end if
			
			if(isvar("SID"))then		print("MSG: Detected command line variable: SID")
			else				SID	= NML_DATA(ind(isStrSubset2(NML_DATA,"SID =")))
							SID	:= str_squeeze(str_split(SID," = "))
							if(dimsizes(SID).eq.2)then	SID := SID(1)
							else				SID := ""
							end if
			end if

			if(isvar("FORCE"))then		print("MSG: Detected command line variable: FORCE")
			else				val := ind(isStrSubset2(NML_DATA,"FORCE ="))
							if(.not.ismissing(val(0)))then	FORCE	= NML_DATA(val)
											FORCE	:= str_squeeze(str_split(FORCE," = "))
											FORCE	:= where(FORCE(1).eq."True",True,False)
							else				FORCE	:= False
							end if
			end if
end if


; 4. These variables must be defined. Otherwise, exit.
if(DSOURCE.eq."")then	print("ERROR: DSOURCE must be set in the namelist or as a command line arg.")
			exit
end if
if(IDATE.eq."")then	print("ERROR: IDATE must be set in the namelist or as a command line arg.")
			exit
end if
if(SID.eq."")then	print("ERROR: SID must be set in the namelist or as a command line arg.")
			exit
end if


; 5. Set these variables if still undefined.
;    Also, fix the type, if necessary.
;    The script can proceed, but perhaps not as intended.
if(SID.eq."")then	print("WARNING: SID not found. Setting it SID=NONE.")
			SID = "NONE"
end if
if(SYS_ENV.eq."")then	print("WARNING: SYS_ENV not found. Setting it SID=NONE.")
			SYS_ENV = "JET"
end if
if(.not.isvar("ATCF_REQD"))then   ATCF_REQD = True   end if
if(.not.isvar("FORCE"))then		FORCE = False
else
	if(FORCE.eq."")then		FORCE := False
	else if(typeof(FORCE).eq."string")then
					FORCE := where(FORCE.eq."True",True,False)
	end if  end if
end if


; 7. Get path variables from the master namelist:
;    EXPT, IDIR, ATCF1_DIR, ATCF1_TAG, ATCF2_DIR, ATCF2_TAG, ITAG, IDIR
;    First, check if paths should be automatically defined (AUTO_DIR)
AUTO_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"AUTO_DIR =")))
AUTO_DIR	:= str_squeeze(str_split(AUTO_DIR," = "))
AUTO_DIR	:= where(AUTO_DIR(1).eq."True",True,False)
if(AUTO_DIR)then	print("MSG: Auto-Directory functionality is ON. Ignoring paths defined in namelist.")
			print("MSG: Please change namelist if this is not desired.")
			EXPT	= getAutoDir(SYS_ENV,DSOURCE,"EXPT")
			IDIR	= getAutoDir(SYS_ENV,DSOURCE,"IDIR")
			ATCF2_DIR	= getAutoDir(SYS_ENV,DSOURCE,"ATCFDIR")
			ATCF2_TAG	= ".atcfunix"
			if(isStrSubset(DSOURCE,"HWRF"))then
				if(DOMAIN.eq."d03")then		ITAG	= "hwrfprs.storm"
				else if(DOMAIN.eq."d02")then	ITAG	= "hwrfprs.synoptic"
				else				ITAG	= "hwrfprs.global"
								;ITAG	= (/"hwrfprs.global","hwrfsat.global"/)
				end if  end if
			else if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then
				ITAG = (/"atmos_sos.nest02_nested_ltd","nggps2d.nest02_nested_ltd"/)
			else if(str_lower(DSROUCE).eq."gfs")then
				ITAG = (/"pgrb2.0p25"/)
			else
				ITAG = ""
			end if  end if  end if
			EXT = ".grb2"
			ATCF1_DIR	= IDIR
			ATCF1_TAG	= ".trak.hwrf.raw"
			ODIR		= "/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/"+EXPT+"/"+IDATE+"/multi_model/"	
else			print("MSG: Auto-Directory functionality is OFF. Using paths defined in namelist.")
			print("MSG: Please change namelist if this is not desired.")
			EXPT	= NML_DATA(ind(isStrSubset2(NML_DATA,"EXPT =")))
			EXPT	:= str_squeeze(str_split(EXPT," = "))
			EXPT	:= EXPT(1)

			IDIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"IDIR =")))
			IDIR	:= str_squeeze(str_split(IDIR," = "))
			IDIR	:= IDIR(1)
	
			ITAG	= NML_DATA(ind(isStrSubset2(NML_DATA,"ITAG =")))
			ITAG	:= str_squeeze(str_split(ITAG," = "))
			if(dimsizes(ITAG).eq.1)then	ITAG = ""
			else				ITAG := ITAG(1)
							ITAG := str_squeeze(str_split(ITAG," "))
			end if
				
			EXT	= NML_DATA(ind(isStrSubset2(NML_DATA,"EXT =")))
			EXT	:= str_squeeze(str_split(EXT," = "))
			if(dimsizes(EXT).eq.1)then	EXT = ""
			else				EXT := EXT(1)
							EXT := str_squeeze(str_split(EXT," "))
			end if

			ODIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"ODIR =")))
			ODIR	:= str_squeeze(str_split(ODIR," = "))
			ODIR	:= ODIR(1)
			ODIR	= ODIR+EXPT+"/"+IDATE+"/multi_model/"
	
			ATCF1_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"ATCF1_DIR =")))
			ATCF1_DIR	:= str_squeeze(str_split(ATCF1_DIR," = "))
			ATCF1_DIR	:= ATCF1_DIR(1)

			ATCF1_TAG	= NML_DATA(ind(isStrSubset2(NML_DATA,"ATCF1_TAG =")))
			ATCF1_TAG	:= str_squeeze(str_split(ATCF1_TAG," = "))
			ATCF1_TAG	:= ATCF1_TAG(1)
				
			ATCF2_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"ATCF2_DIR =")))
			ATCF2_DIR	:= str_squeeze(str_split(ATCF2_DIR," = "))
			ATCF2_DIR	:= ATCF2_DIR(1)

			ATCF2_TAG	= NML_DATA(ind(isStrSubset2(NML_DATA,"ATCF2_TAG =")))
			ATCF2_TAG	:= str_squeeze(str_split(ATCF2_TAG," = "))
			ATCF2_TAG	:= ATCF2_TAG(1)
end if


; 8. Get non-automated variables from master namelist
; a. This is the actual initial forecast hour for which graphics will be produced.
INIT_HR	= NML_DATA(ind(isStrSubset2(NML_DATA,"INIT_HR =")))
INIT_HR	:= str_squeeze(str_split(INIT_HR," = "))
INIT_HR	:= toint(tofloat(INIT_HR(1)))

; b. This is the actual final forecast hour for which graphics will be produced.
;    Its value may change based on the availability of ATCF data.
FNL_HR	= NML_DATA(ind(isStrSubset2(NML_DATA,"FNL_HR =")))
FNL_HR	:= str_squeeze(str_split(FNL_HR," = "))
FNL_HR	:= toint(tofloat(FNL_HR(1)))

; c. This is the format code for the forecast hour used in the input file name
FMT_HR	= NML_DATA(ind(isStrSubset2(NML_DATA,"FMT_HR =")))
FMT_HR	:= str_squeeze(str_split(FMT_HR," = "))
FMT_HR	:= FMT_HR(1)

; d. This is the time step between forecast hours
DT	= NML_DATA(ind(isStrSubset2(NML_DATA,"DT =")))
DT	:= str_squeeze(str_split(DT," = "))
DT	:= toint(tofloat(DT(1)))

; g. If True, trim the white space from graphics.
DO_RMWHITE	= NML_DATA(ind(isStrSubset2(NML_DATA,"DO_RMWHITE =")))
DO_RMWHITE	:= str_squeeze(str_split(DO_RMWHITE," = "))
DO_RMWHITE	:= where(DO_RMWHITE(1).eq."True",True,False)

; h. If True, add the data source to the graphic title.
DO_SRCLBL	= NML_DATA(ind(isStrSubset2(NML_DATA,"DO_SRCLBL =")))
DO_SRCLBL	:= str_squeeze(str_split(DO_SRCLBL," = "))
DO_SRCLBL	:= where(DO_SRCLBL(1).eq."True",True,False)

ADECK_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"ADECK_DIR =")))
ADECK_DIR	:= str_squeeze(str_split(ADECK_DIR," = "))
ADECK_DIR	:= ADECK_DIR(1)

BDECK1_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"BDECK1_DIR =")))
BDECK1_DIR	:= str_squeeze(str_split(BDECK1_DIR," = "))
BDECK1_DIR	:= BDECK1_DIR(1)

BDECK2_DIR	= NML_DATA(ind(isStrSubset2(NML_DATA,"BDECK2_DIR =")))
BDECK2_DIR	:= str_squeeze(str_split(BDECK2_DIR," = "))
BDECK2_DIR	:= BDECK2_DIR(1)

; Get the original model code. Default is "HWRF"
val		:= ind(isStrSubset2(NML_DATA,"MORIG ="))
if(ismissing(val(0)))then
	MORIG	= "HWRF"
else
	MORIG	= NML_DATA(val)
	MORIG	:= str_squeeze(str_split(MORIG," = "))
	MORIG	:= MORIG(1)
end if

MCODE		= NML_DATA(ind(isStrSubset2(NML_DATA,"MCODE =")))
MCODE		:= str_squeeze(str_split(MCODE," = "))
MCODE		:= MCODE(1)

MCODEI		= NML_DATA(ind(isStrSubset2(NML_DATA,"MCODEI =")))
MCODEI		:= str_squeeze(str_split(MCODEI," = "))
MCODEI		:= MCODEI(1)

if(MCODE.eq."AVNO")then    MCODEV = "GFSO"
else			   MCODEV = MCODE
end if
if(MCODEI.eq."AVNI")then   MCODEVI = "GFSI"
else			   MCODEVI = MCODEI
end if

TRKmodels00	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKM00 =")))
TRKmodels00	:= str_squeeze(str_split(TRKmodels00," = "))
TRKmodels00	:= array_append_record(MCODE,TRKmodels00(1:),0)

TRKmodels06	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKM06 =")))
TRKmodels06	:= str_squeeze(str_split(TRKmodels06," = "))
TRKmodels06	:= array_append_record(MCODE,TRKmodels06(1:),0)

INTmodels	= NML_DATA(ind(isStrSubset2(NML_DATA,"INTM =")))
INTmodels	:= str_squeeze(str_split(INTmodels," = "))
INTmodels	:= array_append_record(MCODE,INTmodels(1:),0)

PRSmodels	= NML_DATA(ind(isStrSubset2(NML_DATA,"PRSM =")))
PRSmodels	:= str_squeeze(str_split(PRSmodels," = "))
PRSmodels	:= array_append_record(MCODE,PRSmodels(1:),0)

TRKmodelsI00	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKMI00 =")))
TRKmodelsI00	:= str_squeeze(str_split(TRKmodelsI00," = "))
TRKmodelsI00	:= array_append_record(MCODEI,TRKmodelsI00(1:),0)

TRKmodelsI06	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKMI06 =")))
TRKmodelsI06	:= str_squeeze(str_split(TRKmodelsI06," = "))
TRKmodelsI06	:= array_append_record(MCODEI,TRKmodelsI06(1:),0)

INTmodelsI	= NML_DATA(ind(isStrSubset2(NML_DATA,"INTMI =")))
INTmodelsI	:= str_squeeze(str_split(INTmodelsI," = "))
INTmodelsI	:= array_append_record(MCODEI,INTmodelsI(1:),0)

TRKmodelsT00	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKMT00 =")))
TRKmodelsT00	:= str_squeeze(str_split(TRKmodelsT00," = "))
TRKmodelsT00	:= array_append_record(MCODE,TRKmodelsT00(1:),0)

TRKmodelsT06	= NML_DATA(ind(isStrSubset2(NML_DATA,"TRKMT06 =")))
TRKmodelsT06	:= str_squeeze(str_split(TRKmodelsT06," = "))
TRKmodelsT06	:= array_append_record(MCODE,TRKmodelsT06(1:),0)

INTmodelsT	= NML_DATA(ind(isStrSubset2(NML_DATA,"INTMT =")))
INTmodelsT	:= str_squeeze(str_split(INTmodelsT," = "))
INTmodelsT	:= array_append_record(MCODE,INTmodelsT(1:),0)

etModels	= NML_DATA(ind(isStrSubset2(NML_DATA,"ETM =")))
etModels	:= str_squeeze(str_split(etModels," = "))
etModels	:= array_append_record(MCODEVI,etModels(1:),0)

eiModels	= NML_DATA(ind(isStrSubset2(NML_DATA,"EIM =")))
eiModels	:= str_squeeze(str_split(eiModels," = "))
eiModels	:= array_append_record(MCODEVI,eiModels(1:),0)

ltModels	= NML_DATA(ind(isStrSubset2(NML_DATA,"LTM =")))
ltModels	:= str_squeeze(str_split(ltModels," = "))
ltModels	:= array_append_record(MCODEV,ltModels(1:),0)

liModels	= NML_DATA(ind(isStrSubset2(NML_DATA,"LIM =")))
liModels	:= str_squeeze(str_split(liModels," = "))
liModels	:= array_append_record(MCODEV,liModels(1:),0)




;================================================================
; PART IV. Print some information to the terminal
;================================================================

print("MSG: Running GPLOT with these settings:")
print("MSG:   Experiment Name:    "+EXPT)
print("MSG:   Data Source:        "+DSOURCE)
print("MSG:   Model Name:         "+MCODE)
print("MSG:   Interp. Model Name: "+MCODEI)
print("MSG:   Orig. Model Name:   "+MORIG)
print("MSG:   Forecast Cycle:     "+IDATE)
print("MSG:   Storm ID:           "+SID)
if(ENSID.ne."")then	print("MSG:   Ensemble Member:    "+ENSID)	end if
print("MSG:   Input Directory:    "+IDIR)
print("MSG:   Output Directory:   "+ODIR)
print("MSG:   System Environment: "+SYS_ENV)
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
print("MSG:   Hour Format:        "+FMT_HR)
print("MSG:   Time Step [h]:      "+DT)
print("MSG:   Final ATCF Path:    "+ATCF2_DIR)
print("MSG:   Final ATCF Tag:     "+ATCF2_TAG)
if(ATCF1_DIR.ne.ATCF2_DIR .and. ATCF1_TAG.ne.ATCF2_TAG)then
	print("MSG:   Temp. ATCF Path:     "+ATCF1_DIR)
	print("MSG:   Temp. ATCF Tag:      "+ATCF1_TAG)
end if
print("MSG:   ADECK Directory:    "+ADECK_DIR)
print("MSG:   BDECK Directory 1:  "+BDECK1_DIR)
print("MSG:   BDECK Directory 2:  "+BDECK2_DIR)
if(DO_RMWHITE)then	print("MSG:   Trim graphics:      ON")
else			print("MSG:   Trim graphics:      OFF")
end if
if(DO_SRCLBL)then	print("MSG:   Source Labels:      ON")
else			print("MSG:   Source Labels:      OFF")
end if
;print("MSG:   TRKmodels00:        "+stringOut(TRKmodels00))
;print("MSG:   TRKmodels06:        "+stringOut(TRKmodels06))
;print("MSG:   INTmodels:          "+stringOut(INTmodels))
;print("MSG:   PRSmodels:          "+stringOut(PRSmodels))
;print("MSG:   TRKmodelsI00:       "+stringOut(TRKmodelsI00))
;print("MSG:   TRKmodelsI06:       "+stringOut(TRKmodelsI06))
;print("MSG:   INTmodelsI:         "+stringOut(INTmodelsI))
;print("MSG:   TRKmodelsT00:       "+stringOut(TRKmodelsT00))
;print("MSG:   TRKmodelsT06:       "+stringOut(TRKmodelsT06))
;print("MSG:   INTmodelsT:         "+stringOut(INTmodelsT))
print("MSG:   etModels:           "+stringOut(etModels))
print("MSG:   eiModels:           "+stringOut(eiModels))
print("MSG:   ltModels:           "+stringOut(ltModels))
print("MSG:   liModels:           "+stringOut(liModels))




;================================================================
; PART V. Set/Change some variables here.
;         These options may eventually be moved into functions
;         and/or namelist options.
;================================================================

; 2. Make the Output Directory (ODIR)
;    It should already exist, so this is a safety check.
system("mkdir -p "+ODIR)




;================================================================
; USER INPUT SECTION.
;================================================================

; Graphics Namelist
; Comment this to set default namelist.
if(.not.isvar("VAR_NML"))then   
	if(fileexists(GPLOT_DIR+"/nmlist/namelist.stats."+EXPT))then
		VAR_NML = GPLOT_DIR+"/nmlist/namelist.stats."+EXPT
	else if(fileexists(GPLOT_DIR+"/nmlist/namelist.stats.default"))then
		VAR_NML = GPLOT_DIR+"/nmlist/namelist.stats.default"
	else
		print("ERROR: Default variable namelist not available.")
		exit
	end if  end if
end if
print("MSG: Using this variable namelist: "+VAR_NML)


NML_DATA	:= asciiread(VAR_NML,-1,"string")
headers		:= str_squeeze(str_split(NML_DATA(0)," "))


; MASTER PLOTTING CONTROLS
doPlot1	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Track_Guidance"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doPlot2	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Intensity_Guidance"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doPlot3	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Pressure_Guidance"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doPlot4	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Track_Trends"))),		ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doPlot5	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Intensity_Trends"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doPlot6	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Pressure_Trends"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
doVer	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Verification"))),		ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	doETVer	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Early_Track_Ver"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	doEIVer	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Early_Intensity_Ver"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	doLTVer	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Late_Track_Ver"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	doLIVer	= where(str_squeeze(str_get_field(NML_DATA(ind(isStrSubset2(NML_DATA,"Late_Intensity_Ver"))),	ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)

print("")
print("MSG: Will produce the following graphics:")
print("MSG:   Track Guidance:        "+doPlot1)
print("MSG:   Intensity Guidance:    "+doPlot2)
print("MSG:   Pressure Guidance:     "+doPlot3)
print("MSG:   Track Trends:          "+doPlot4)
print("MSG:   Intensity Trends:      "+doPlot5)
print("MSG:   Pressure Trends:       "+doPlot6)
print("MSG:   Verification:          "+doVer)
print("MSG:   --> Early Track:       "+doETVer)
print("MSG:   --> Early Intensity:   "+doEIVer)
print("MSG:   --> Late Track:        "+doLTVer)
print("MSG:   --> Late Intensity:    "+doLIVer)




;================================================================
; MANUAL OVERRIDES.
;================================================================

; Graphics Production
;doPlot1	= True		; Track Guidance
;doPlot2	= True		; Intensity Guidance
;doPlot3	= True		; Pressure Guidance
;doPlot4	= True		; HWRF-B Track Trends
;doPlot5	= True		; HWRF-B Intensity Trends
;doPlot6	= True		; HWRF-B Pressure Trends (not ready)
;doVer	= True		; Verify cycles up to this point and create plots
;	doETVer = True
;	doEIVer = True
;	doLTVer	= True
;	doLIVer	= True


; DECK directories
;adeckDir	= "/home/Ghassan.Alaka/adeck/ftp.nhc.noaa.gov/atcf/aid_public/"
;bdeckDir1	= "/home/Ghassan.Alaka/bdeck/ftp.nhc.noaa.gov/atcf/btk/"
;bdeckDir2	= "/home/Ghassan.Alaka/bdeck/"


; Models
; Choose late models: track (TRKmodels), intensity (INTmodels), pressure (PRSmodels)
;TRKmodels00	= (/mcode,"HWRF","AVNO","HMON","NVGM","CTCX","EGRR"/)
;TRKmodels06	= (/mcode,"HWRF","AVNO","HMON","NVGM","CTCX"/)
;INTmodels	= (/mcode,"HWRF","HMON","DSHP","LGEM","CTCX"/)
;PRSmodels	= (/mcode,"HWRF","AVNO","HMON","NVGM","CTCX"/)

; Choose early models: track (TRKmodelsI), intensity (INTmodelsI)
;TRKmodelsI00	= (/mcodeI,"HWFI","AVNI","HMNI","EGRI","CTCI","TVCN","OFCL"/)
;TRKmodelsI06	= (/mcodeI,"HWFI","AVNI","HMNI","NVGI","CTCI","TVCN","OFCL"/)
;INTmodelsI	= (/mcodeI,"HWFI","HMNI","DSHP","LGEM","CTCI","IVCN","OFCL"/)

; Choose trend models: track (TRKmodelsT), intensity (INTmodelsT)
;TRKmodelsT00	= (/mcode,"HWRF","HMON","AVNO","EGRR","OFCL"/)
;TRKmodelsT06	= (/mcode,"HWRF","HMON","AVNO","EGRR","OFCL"/)
;INTmodelsT	= (/mcode,"HWRF","HMON","DSHP","OFCL"/)

; Choose early & late models for track & intensity verification
;etModels	= (/mcodeI,"HWFI","GFSI","HMNI","NVGI","CTCI","TVCN","OFCL"/)
;eiModels	= (/mcodeI,"HWFI","HMNI","DSHP","LGEM","CTCI","IVCN","OFCL"/)
;ltModels	= (/mcode, "HWRF","GFSO","HMON","NVGM","CTCX"/)
;ltModels	= (/mcode, "HWRF","GFSO","HMON","CTCX"/)
;liModels	= (/mcode, "HWRF","HMON","CTCX"/)




;================================================================
; USER INPUT SECTION.
;================================================================

; Add a lat/lon grid on map plots
gridOn	= True		


; Set experiment, directories, etc.
; MKedit: change variable name from long_name to title
; MKnote: function getModelInfo is updated in util and must have ModelInfo.dat in GPLOT/tbl
; long_name = getModelInfo(MCODE,"title") ; MKnote: I think this variable was mislabeled
title = getModelInfo(MCODE,"title")

; Set a secondary model to verify along with the current
; This example was to plot HB15 along with HB16 when they ran together in 2016
;if(MCODE.eq."HB16")then		expt2	 = "hwrf-basinscale_multistorms-rev4_2015_realtime"
;				pyDir2	 = "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2015-rev4_realtime/"+expt2+"/com/"
;				atcfDir2 = "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2015-rev4_realtime/"+expt2+"/"
;end if


; Choose number of forecasts for model trend plots
nTrend		= 6


; Choose forecast hours to verify
; Eventually move this into the namelist
verTimes	= (/"00","12","24","36","48","60","72","84","96","108","120"/)


; If SID was predefined, then we need to make sure the forecast
; found matches that SID too.
if(SID.ne."")then	USER_SID = True
else			USER_SID = False
end if


; 11. Parse the cycle (IDATE) for later use
;     Expected format is YYYYMMDDHH
YYYY	= systemfunc("echo "+IDATE+" | cut -c1-4")
MM	= systemfunc("echo "+IDATE+" | cut -c5-6")
DD	= systemfunc("echo "+IDATE+" | cut -c7-8")
HH	= systemfunc("echo "+IDATE+" | cut -c9-10")


; Convert IDATE into hours from a start date.
; This makes it easier to add/subtract hours to find other cycles.
utc_base = cd_inv_calendar(toint(YYYY),toint(MM),toint(DD),toint(HH),0,0,tunits,0)
utc_base@units = tunits


; Get the cycle 6 hours in the future for early forecast models.
; Also, make the directory in case it doesn't exist.
utc	= utc_base+6
utc@units = tunits
IDATE06 := tostring(toint(cd_calendar(utc,3)))
system("mkdir -p "+str_sub_str(ODIR,IDATE,IDATE06))


; Hours for XY Graphics (e.g., intensity, pressure)
XYHrs	:= ispan(0,120,12)	; regular guidance
XYHrs2	:= ispan(-24,120,12)	; trends


; Define some executables
ADECK_EXE = GPLOT_DIR+"/shell/createADeck.ksh"
INTERP_HOME = GPLOT_DIR+"/HWRF_interp/"
INTERP_EXE = "run_interp.sh"


; Option to do interpolation. Should be set to True unless testing
DO_INTERP = True



;================================================================
; If 'iDate' is undefined, then search for dates in the 'com'
; directory. Then, check if these dates have been processed.
;================================================================

print("")
print("MSG: ***************STEP ONE***************")
print("MSG: Check for available forecasts and check status of each.")
print("MSG: Will automatically include any forecasts within 6 days.")
print("MSG: First check for real-time, temporary ATCF file.")


if(fileexists(ODIR+"ATCF_FILES.dat"))then
	print("MSG: Found a list of ATCF files for this particular cycle --> "+ODIR+"ATCF_FILES.dat")
	
	; Read ATCF_FILES.dat, which is created by the shell wrapper.
	; This file contains all ATCFs that match the cycle (IDATE)
	ATCF_FILE := asciiread(ODIR+"ATCF_FILES.dat",-1,"string")
	
	; Retain only the ATCF that matched the storm ID (SID)
	val := ind(isStrSubset2(ATCF_FILE,str_lower(SID)))
	
	; Check that1 vald ATCF is found. If not, exit.
	if(dimsizes(val).gt.1)then
		print("ERROR: Found more than 1 matching ATCF file found in "+ODIR+"ATCF_FILES.dat")
		exit
	end if
	if(ismissing(val(0)))then
		print("ERROR: Could not find matching ATCF in "+ODIR+"ATCF_FILES.dat")
		exit
	end if
	
	; If the code gets here, we're good to go!
	ATCF_FILE := ATCF_FILE(val)
else
	;Add functionality to automatically search for ATCFs as a back up?
	print("MSG: Could not find list of ATCF files. Searching based on namelist.")
	
	; Search for the ATCF
	ATCF_FILE := systemfunc("ls "+ATCF2_DIR+"*"+str_lower(SID)+"*"+IDATE+"*"+ATCF2_TAG+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("ls "+ATCF2_DIR+"*"+IDATE+"*"+str_lower(SID)+"*"+ATCF2_TAG+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("ls "+ATCF1_DIR+"*"+str_lower(SID)+"*"+IDATE+"*"+ATCF1_TAG+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("ls "+ATCF1_DIR+"*"+IDATE+"*"+str_lower(SID)+"*"+ATCF1_TAG+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then
		print("ERROR: Could not find matching ATCF at locations specified in namelist.")
		exit
	end if
end if
print("MSG: Found this ATCF file --> "+ATCF_FILE)
ATCF_BASE = systemfunc("basename "+ATCF_FILE)
ATCF_DIR = str_sub_str(ATCF_FILE,ATCF_BASE,"")
ATCF_MOD = systemfunc("find "+ATCF_FILE+" -mmin +60")


; Check if the ATCF file is the final ATCF
; The temporary ATCF file could be used if it is old enough.
if(isStrSubset(ATCF_FILE,ATCF2_DIR))then
	print("MSG: This is the final ATCF.")
	FNL_ATCF = True
else if(.not.ismissing(ATCF_MOD))then
	print("MSG: This is the final ATCF.")
	FNL_ATCF = True
else
	print("MSG: This is not the final ATCF.")
	FNL_ATCF = False
end if  end if


; Parse important info from the file name
fSplit	:= str_split(ATCF_BASE,".")
LONGSID	:= fSplit(0)
tmp	:= tochar(LONGSID)
TC_NAME	:= str_upper(tostring(tmp(0)))+""+tostring(tmp(1:dimsizes(tmp)-4))
TC_NUM	:= tostring(tmp(dimsizes(tmp)-3:dimsizes(tmp)-2))
BASIN1	:= tostring(tmp(dimsizes(tmp)-1))
if(BASIN1.eq."l")then   BASIN2 = "al"   end if
if(BASIN1.eq."e")then   BASIN2 = "ep"   end if
if(.not.isvar("BASIN2"))then
	print("ERROR: Two-character basin could not be defined.")
	exit
end if
SID2	= BASIN2+TC_NUM+YYYY
BDECK	= "b"+SID2+".dat"
ADECK	= "a"+SID2+".dat"
ADECK_M = "a"+SID2+"_"+str_lower(MCODE)+".dat"
ADECK_I = "a"+SID2+"_"+str_lower(MCODEI)+".dat"
ADECK_N = "a"+SID2+"_new.dat"


; Verification is not performed for invests because NHC rules ignore them.
if(toint(TC_NUM).gt.50)then
	print("MSG:  Verification turned OFF since this is an invest.")
	doVer = False
else
	doVer = True
end if
		
		

;YMDH6	= systemfunc("date '+%Y%m%d%H' -d "+str_get_dq()+MM+"/"+DD+"/"+YYYY+" "+HH+":00:00 UTC - 6 hours"+str_get_dq())
;tmpDir3	= str_sub_str(ODIR,IDATE,YMDH6)


; Choose different TRACK models based on the HOUR
if(any((/"00","12"/).eq.HH))then	TRKmodels  := TRKmodels00
					TRKmodelsI := TRKmodelsI00
					TRKmodelsT := TRKmodelsT00
else if(any((/"06","18"/).eq.HH))then	TRKmodels  := TRKmodels06
					TRKmodelsI := TRKmodelsI06
					TRKmodelsT := TRKmodelsT06
else					TRKmodels  := TRKmodels00
					TRKmodelsI := TRKmodelsI00
					TRKmodelsT := TRKmodelsT00
end if  end if




;================================================================
; Create color arrays for models
;================================================================

TRKcolors   = getModelInfo(TRKmodels,"color")
TRKmarkers  = getModelInfo(TRKmodels,"marker")
TRKlongname = getModelInfo(TRKmodels,"long_name")

INTcolors   = getModelInfo(INTmodels,"color")
INTmarkers  = getModelInfo(INTmodels,"marker")
INTlongname = getModelInfo(INTmodels,"long_name")

TRKcolorsI  = getModelInfo(TRKmodelsI,"color")
TRKmarkersI = getModelInfo(TRKmodelsI,"marker")
TRKlongnameI    = getModelInfo(TRKmodelsI,"long_name")

INTcolorsI  = getModelInfo(INTmodelsI,"color")
INTmarkersI = getModelInfo(INTmodelsI,"marker")
INTlongnameI    = getModelInfo(INTmodelsI,"long_name")

PRScolors   = getModelInfo(PRSmodels,"color")
PRSmarkers  = getModelInfo(PRSmodels,"marker")
PRSlongname = getModelInfo(PRSmodels,"long_name")

ETcolors    = getModelInfo(etModels,"color")
ETmarkers   = getModelInfo(etModels,"marker")
ETlongname  = getModelInfo(etModels,"long_name")

EIcolors    = getModelInfo(eiModels,"color")
EImarkers   = getModelInfo(eiModels,"marker")
EIlongname  = getModelInfo(eiModels,"long_name")

LTcolors    = getModelInfo(ltModels,"color")
LTmarkers   = getModelInfo(ltModels,"marker")
LTlongname  = getModelInfo(ltModels,"long_name")

LIcolors    = getModelInfo(liModels,"color")
LImarkers   = getModelInfo(liModels,"marker")
LIlongname  = getModelInfo(liModels,"long_name")


; Get a blue-faded color map
;cmap   := read_colormap_file("WhiteBlue")
;TRDcolors  = cmap(70:250:30,:)
cmap    := read_colormap_file("NCV_blue_red")
cmap    := cmap(:120:-1,:)  ; keep blues only
TRDcolors   = cmap(2:92:18,:)
TRDmarkers  = (/8,7,6,0,4,16/)
TRKlongnameT    = getModelInfo(TRKmodelsT,"long_name")
INTlongnameT    = getModelInfo(INTmodelsT,"long_name")


; Get a rainbow color map (support up to 100 forecasts)
tmp := read_colormap_file("default")
LIFEcolors  := array_append_record(tmp(2:27,:),tmp(2:27,:),0)
LIFEcolors  := array_append_record(LIFEcolors(:,:),tmp(2:27,:),0)
LIFEcolors  := array_append_record(LIFEcolors(:,:),tmp(2:27,:),0)



print("")
print("MSG: *************STEP TWO************")
print("MSG: Get the latest adeck and bdeck.")

;================================================================
; Create a new ADECK, copy latest BDECK
;================================================================

print("MSG: Create new A-DECK")
system("echo 'working' > "+ODIR+"status."+LONGSID+".log")

; Create a new ADECK file that includes the HWRF-B forecast
ADIR	= ODIR+"atcf/"
system("mkdir -p "+ADIR)


; Search for other ATCFs that match the storm ID in the final ATCF directory
; If other ATCFs are found, concatenate everything together into a new ATCF
; If other ATCFS not found, copy the original ATCF file to a new ATCF
test := systemfunc("ls "+ATCF2_DIR+"*"+str_lower(SID)+"*"+ATCF2_TAG+" 2>/dev/null")
if(.not.ismissing(test(0)))then
	system(ADECK_EXE+" "+ADIR+" "+ATCF_DIR+"*"+str_lower(SID)+"* >/dev/null 2>&1")
	system("mv "+ADIR+ADECK+" "+ADIR+ADECK_M)
	system("cat "+ATCF_FILE+" "+ADIR+ADECK_M+" | sort -u | sort -k3,3 -k5,5 -k6,6n > "+ADIR+"tmp.dat")
	system("mv "+ADIR+"tmp.dat "+ADIR+ADECK_M)
else
	system("cp "+ATCF_FILE+" "+ADIR+ADECK_M)
end if


; If the original model code and the new model code do not match, update the ATCF
if(MORIG.ne.MCODE)then
	system("sed -i 's/"+MORIG+"/"+MCODE+"/g' "+ADIR+ADECK_M)
end if


if(fileexists(ADECK_DIR+ADECK))then
	print("MSG: NHC A-DECK found in namelist location --> "+ADECK_DIR+ADECK)
	system("cp "+ADECK_DIR+ADECK+" "+ADIR+ADECK)
	system("cat "+ADIR+ADECK+" "+ADIR+ADECK_M+" | sort -u | sort -k3,3 -k5,5 -k6,6n > "+ADIR+ADECK_N)
else if(fileexists(ADIR+ADECK))then
	print("MSG: NHC A-DECK found in working directory --> "+ADIR+ADECK)
	system("cat "+ADIR+ADECK+" "+ADIR+ADECK_M+" | sort -u | sort -k3,3 -k5,5 -k6,6n > "+ADIR+ADECK_N)
else
	print("WARNING: NHC A-DECK not found --> "+ADECK_DIR+ADECK)
	print("WARNING: This is OK, but graphics will be incomplete.")
	system("cp "+ADIR+ADECK_M+" "+ADIR+ADECK_N)
end if  end if


print("MSG: Copy latest BDECK")
if(fileexists(BDECK1_DIR+BDECK))then
	print("MSG: NHC B-DECK found --> "+BDECK1_DIR+BDECK)
	system("cp "+BDECK1_DIR+BDECK+" "+ADIR+BDECK)
	sleep(5)
else if(fileexists(BDECK2_DIR+BDECK))then
	print("MSG: NHC B-DECK found --> "+BDECK2_DIR+BDECK)
	system("cp "+BDECK2_DIR+BDECK+" "+ADIR+BDECK)
	sleep(5)
else if(fileexists(ADIR+BDECK))then
	print("MSG: NHC B-DECK found in working directory --> "+ADIR+BDECK)
else
	print("WARNING: NHC B-DECK file not found.")
	print("WARNING: Verification turned OFF.")
	doVer = False
end if  end if  end if




;================================================================
; Produce Interpolated Forecasts
;================================================================
if(DO_INTERP)then
print("MSG: Produce Interpolated Forecasts.")


; Set up the interpolation running directory.
; Copy this from GPLOT_DIR to avoid too many jobs from accessing
; it at the same time.
INTERP_RUN = ADIR+"HWRF_interp/"
system("rm -rf "+INTERP_RUN)
system("cp -r "+INTERP_HOME+" "+INTERP_RUN)


; Set up the interpolation ATCF directory.
INTERP_DIR = ADIR+"interp/"
INTERP_FILE = INTERP_DIR+"/a"+SID2+"."+MCODE+".interp"
system("mkdir -p "+INTERP_DIR)
system("ln -sf "+ADIR+ADECK_N+" "+INTERP_DIR+ADECK_N)
system("ln -sf "+ADIR+BDECK+" "+INTERP_DIR+BDECK)


; Run the interpolation executable to produce early forecasts.
INTERP_EXE = INTERP_RUN+INTERP_EXE
system(INTERP_EXE+" "+MCODE+" "+str_get_dq()+INTERP_RUN+str_get_dq()+" "+str_get_dq()+INTERP_DIR+str_get_dq()+" "+str_get_dq()+"_new"+str_get_dq()+" "+BASIN2+" "+YYYY+" "+TC_NUM)
system("cat "+ADIR+ADECK_N+" "+INTERP_FILE+\
       "| sort -u | sort -k3,3 -k5,5 -k6,6n  > "+ADIR+"tmp.dat")
system("mv "+ADIR+"tmp.dat "+ADIR+ADECK_N)
system("sed '/"+MCODEI+"/p' "+INTERP_FILE+" | sort -u | sort -k3,3 -k5,5 -k6,6n > "+ADIR+ADECK_I)


; Remove interpolation directories as they are no longer needed.
;system("rm -rf "+INTERP_DIR)
;system("rm -rf "+INTERP_RUN)


end if



;================================================================
; Read ADECK data.
;================================================================
print("MSG: Reading A-DECK file --> "+ADIR+ADECK_N)
data	:= asciiread(ADIR+ADECK_N,-1,"string")

; Get Wind Radii - only keep indices for 34 kts.
windRad	= toint(str_squeeze(str_get_field(data,12,",")))
vals	:= ind(windRad.eq.34 .or. windRad.eq.0)

; Get Latitude
tmp	:= tochar(str_squeeze(str_get_field(data(vals),7,",")))
if(dimsizes(dimsizes(tmp)).eq.1)then
	tmp := new((/1,dimsizes(tochar(str_squeeze(str_get_field(data(vals),7,","))))/),"character")
	tmp(0,:) = tochar(str_squeeze(str_get_field(data(vals),7,",")))
end if
tcLats	= tofloat(tostring(tmp(:,:dimsizes(tmp(0,:))-2)))/10.;



; Get Longitude
tmp         := tochar(str_squeeze(str_get_field(data(vals),8,",")))
if(dimsizes(dimsizes(tmp)).eq.1)then
	tmp := new((/1,dimsizes(tochar(str_squeeze(str_get_field(data(vals),8,","))))/),"character")
	tmp(0,:) = tochar(str_squeeze(str_get_field(data(vals),8,",")))
end if
tcLons	= -1.*tofloat(tostring(tmp(:,:dimsizes(tmp(0,:))-2)))/10.
;print(tcLats(:100)+"   "+tcLons(:100))
;exit

; Get Forecast Hours
tcFHR	= toint(str_squeeze(str_get_field(data(vals),6,",")))

; Get Intensity
tcINT	= toint(str_squeeze(str_get_field(data(vals),9,",")))

; Get Central Pressure
tcPRES	= toint(str_squeeze(str_get_field(data(vals),10,",")))

; Get Models
tcModel	= str_squeeze(str_get_field(data(vals),5,","))

; Get Initial Times
tcInit	= str_squeeze(str_get_field(data(vals),3,","))


; Get all the dates that apply to MCODE
vals	:= ind(tcModel.eq.MCODE .and. tcFHR%6.eq.0 .and. toint(tcInit).le.toint(IDATE))
if(ismissing(vals(0)))then
	print("ERROR: No cycles in the A-Deck match "+MCODE+". Can't continue.")
	exit
end if
allDates := remove_duplicates(toint(tcInit(vals)))



; TRACK GUIDANCE
if(doPlot1)then
	print("MSG: Retrieving these late track models at "+IDATE+" --> "+stringOut(TRKmodels))
	aTRK	= new((/dimsizes(TRKmodels),3,50/),"float",fval3)
	do aaa = 0,dimsizes(TRKmodels)-1
		vals	:= ind(tcModel.eq.TRKmodels(aaa) .and. tcInit.eq.IDATE .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("WARNING: "+TRKmodels(aaa)+" is unavailable for "+IDATE+".")
			continue
		end if
		aTRK(aaa,0,:dimsizes(tcLats(vals))-1) = tcLats(vals)
		aTRK(aaa,1,:dimsizes(tcLons(vals))-1) = tcLons(vals)
		aTRK(aaa,2,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
	end do
	vals := ind(.not.ismissing(aTRK(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these late track models at "+IDATE+" --> "+stringOut(TRKmodels(vals)))
	else				print("WARNING: None of the late track models were found.")
	end if
	
	print("MSG: Retrieving these early track models at "+IDATE06+" --> "+stringOut(TRKmodelsI))
	aTRKI	= new((/dimsizes(TRKmodelsI),3,50/),"float",fval3)
	do aaa = 0,dimsizes(TRKmodelsI)-1
		vals	:= ind(tcModel.eq.TRKmodelsI(aaa) .and. tcInit.eq.IDATE06 .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("MSG: "+TRKmodelsI(aaa)+" is unavailable for "+IDATE06+".")
			continue
		end if
		aTRKI(aaa,0,:dimsizes(tcLats(vals))-1) = tcLats(vals)
		aTRKI(aaa,1,:dimsizes(tcLons(vals))-1) = tcLons(vals)
		aTRKI(aaa,2,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
	end do
	vals := ind(.not.ismissing(aTRKI(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these early track models at "+IDATE06+" --> "+stringOut(TRKmodelsI(vals)))
	else				print("WARNING: None of the early track models were found.")
	end if
	
	; Only retain values within 126 h (to match HWRF forecasts)
	; Do we really want to do this?
	aTRK(:,0,:)	= where(aTRK(:,2,:).gt.126.,fval3,aTRK(:,0,:))
	aTRK(:,1,:)	= where(aTRK(:,2,:).gt.126.,fval3,aTRK(:,1,:))
end if



; TRACK TRENDS
if(doPlot4)then
	print("MSG: Searching for forecasts trends from these track models --> "+stringOut(TRKmodelsT))
	aTRK2	= new((/dimsizes(TRKmodelsT),nTrend,3,50/),"float",fval3)
	IDATES	= new(nTrend,"string")
	aTRK3	= new((/dimsizes(TRKmodelsT),dimsizes(allDates),3,50/),"float",fval3)
	
	; Loop over all models in 'TRKmodelsT'
	do mmm = 0,dimsizes(TRKmodelsT)-1
		; Get the last 6 cycles for each model
		print("MSG: Retrieving last 6 cycles for "+TRKmodelsT(mmm))
		iii	= 0
		do ddd = -30,0,6
	
			; Update 'bDate' to be Valid Time instead of Initial Time
			utc	:= utc_base+tofloat(ddd)
			utc@units = tunits
			IDATET	:= tostring(toint(cd_calendar(utc,3)))
			IDATES(iii) = IDATET
			
			vals	:= ind(tcModel.eq.TRKmodelsT(mmm) .and. tcInit.eq.IDATET .and. tcFHR%6.eq.0)
		
			if(.not.ismissing(vals(0)))then
				aTRK2(mmm,iii,0,:dimsizes(tcLats(vals))-1)	= tcLats(vals)
				aTRK2(mmm,iii,1,:dimsizes(tcLons(vals))-1)	= tcLons(vals)
				aTRK2(mmm,iii,2,:dimsizes(tcFHR(vals))-1)	= tofloat(tcFHR(vals))+tofloat(ddd)
			end if
			iii = iii+1
		end do
		vals := ind(.not.ismissing(aTRK2(mmm,:,0,0)))
		if(.not.ismissing(vals(0)))then	print("MSG: Found these cycles --> "+stringOut(IDATES(vals)))
		else				print("WARNING: No cycles were found.")
		end if
	
		; Get all forecast cycles for each model
		print("MSG: Retrieving lifetime tracks for "+TRKmodelsT(mmm))
		do ddd = 0,dimsizes(allDates)-1
			tmp	:= tochar(tostring(allDates(ddd)))
			tmp0	:= tochar(tostring(allDates(0)))
			offset	= cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
						  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0)\
				- cd_inv_calendar(toint(tostring(tmp0(0:3))),toint(tostring(tmp0(4:5))),\
						  toint(tostring(tmp0(6:7))),toint(tostring(tmp0(8:9))),0,0,tunits,0)
		
			vals	:= ind(tcModel.eq.TRKmodelsT(mmm) .and. tcInit.eq.allDates(ddd) .and. tcFHR%6.eq.0)
			if(ismissing(vals(0)))then   continue   end if
			aTRK3(mmm,ddd,0,:dimsizes(tcLats(vals))-1)	= tcLats(vals)
			aTRK3(mmm,ddd,1,:dimsizes(tcLons(vals))-1)	= tcLons(vals)
			aTRK3(mmm,ddd,2,:dimsizes(tcFHR(vals))-1)	= tofloat(tcFHR(vals))+tofloat(offset)
		end do
		vals := ind(.not.ismissing(aTRK3(mmm,:,0,0)))
		if(.not.ismissing(vals(0)))then	print("MSG: Found these cycles --> "+stringOut(allDates(vals)))
		else				print("WARNING: No cycles were found.")
		end if
	
	end do
end if



; INTENSITY GUIDANCE
if(doPlot2)then
	print("MSG: Retrieving these late intensity models at "+IDATE+" --> "+stringOut(INTmodels))
	aINT	= new((/dimsizes(INTmodels),3,50/),"float",fval3)
	do aaa = 0,dimsizes(INTmodels)-1
		vals	:= ind(tcModel.eq.INTmodels(aaa) .and. tcInit.eq.IDATE .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("WARNING: "+INTmodels(aaa)+" is unavailable for "+IDATE+".")
			continue
		end if
		aINT(aaa,0,:dimsizes(tcINT(vals))-1) = tofloat(tcINT(vals))
		aINT(aaa,1,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
	end do
	vals := ind(.not.ismissing(aINT(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these late intensity models at "+IDATE+" --> "+stringOut(INTmodels(vals)))
	else				print("WARNING: None of the late intensity models were found.")
	end if
	
	print("MSG: Retrieving these early intensity models at "+IDATE06+" --> "+stringOut(INTmodelsI))
	aINTI	= new((/dimsizes(INTmodelsI),3,50/),"float",fval3)
	do aaa = 0,dimsizes(INTmodelsI)-1
		vals	:= ind(tcModel.eq.INTmodelsI(aaa) .and. tcInit.eq.IDATE06 .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then   continue   end if
		aINTI(aaa,0,:dimsizes(tcINT(vals))-1) = tofloat(tcINT(vals))
		aINTI(aaa,1,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
	end do
	vals := ind(.not.ismissing(aINTI(:,0,0)))
	if(.not.ismissing(vals(0)))then		print("MSG: Found these early intensity models at "+IDATE06+" --> "+stringOut(INTmodelsI(vals)))
	else					print("WARNING: None of the early intensity models were found.")
	end if
end if



; INTENSITY TRENDS
if(doPlot5)then
	print("MSG: Searching for forecasts trends from these intensity models --> "+stringOut(INTmodelsT))
	aINT2	= new((/dimsizes(INTmodelsT),nTrend,3,50/),"float",fval3)
	IDATES2 = new(nTrend,"string")
	aINT3	= new((/dimsizes(INTmodelsT),dimsizes(allDates),2,50/),"float",fval3)
	
	; Loop over all models in 'INTmodelsT'
	do mmm = 0,dimsizes(INTmodelsT)-1
		; Get the last 6 cycles for each model
		print("MSG: Retrieving last 6 cycles for "+INTmodelsT(mmm))
		iii	= 0
		do ddd = -30,0,6
	
			; Update 'bDate' to be Valid Time instead of Initial Time
			
			utc	:= utc_base+tofloat(ddd)
			utc@units = tunits
			IDATET	:= tostring(toint(cd_calendar(utc,3)))
			IDATES2(iii) = IDATET
		
			vals	:= ind(tcModel.eq.INTmodelsT(mmm) .and. tcInit.eq.IDATET .and. tcFHR%6.eq.0)
			if(.not.ismissing(vals(0)))then
				aINT2(mmm,iii,0,:dimsizes(tcINT(vals))-1)	= tofloat(tcINT(vals))
				aINT2(mmm,iii,1,:dimsizes(tcFHR(vals))-1)	= tofloat(tcFHR(vals))+tofloat(ddd)
			end if
			iii = iii+1
		end do
		vals := ind(.not.ismissing(aINT2(mmm,:,0,0)))
		if(.not.ismissing(vals(0)))then		print("MSG: Found these cycles --> "+stringOut(IDATES2(vals)))
		else					print("WARNING: No cycles were found.")
		end if
		
		; Get all forecast cycles for each model
		print("MSG: Retrieving lifetime tracks for "+INTmodelsT(mmm))
		do ddd = 0,dimsizes(allDates)-1
			tmp	:= tochar(tostring(allDates(ddd)))
			tmp0	:= tochar(tostring(allDates(0)))
			offset	= cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
						  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0)\
				- cd_inv_calendar(toint(tostring(tmp0(0:3))),toint(tostring(tmp0(4:5))),\
						  toint(tostring(tmp0(6:7))),toint(tostring(tmp0(8:9))),0,0,tunits,0)
		
			vals	:= ind(tcModel.eq.INTmodelsT(mmm) .and. tcInit.eq.allDates(ddd) .and. tcFHR%6.eq.0)
			if(.not.ismissing(vals(0)))then
				aINT3(mmm,ddd,0,:dimsizes(tcINT(vals))-1)	= tcINT(vals)
				aINT3(mmm,ddd,1,:dimsizes(tcFHR(vals))-1)	= tofloat(tcFHR(vals))+tofloat(offset)
			end if
		end do
		vals := ind(.not.ismissing(aINT3(mmm,:,0,0)))
		if(.not.ismissing(vals(0)))then	print("MSG: Found these cycles --> "+stringOut(allDates(vals)))
		else				print("WARNING: No cycles were found.")
		end if
	end do
end if



; PRESSURE GUIDANCE
if(doPlot3)then
	print("MSG: Retrieving these late pressure models at "+IDATE+" --> "+stringOut(PRSmodels))
	aPRS	= new((/dimsizes(PRSmodels),2,50/),"float",fval3)
	do aaa = 0,dimsizes(PRSmodels)-1
		vals	:= ind(tcModel.eq.PRSmodels(aaa) .and. tcInit.eq.IDATE .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("MSG: "+PRSmodels(aaa)+" is unavailable for "+IDATE+".")
			continue
		end if
	
		aPRS(aaa,0,:dimsizes(tcPRES(vals))-1) = where(tcPRES(vals).eq.0,fval3,tofloat(tcPRES(vals)))
		aPRS(aaa,1,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
	end do
	vals := ind(.not.ismissing(aPRS(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these late pressure models at "+IDATE+" --> "+stringOut(PRSmodels(vals)))
	else				print("WARNING: None of the late pressure models were found.")
	end if
end if



; PRESSURE TRENDS
if(doPlot6)then
	print("MSG: Retrieving last 6 cycles for "+MCODE)
	aPRS2	= new((/nTrend,2,50/),"float",fval3)
	IDATES3 = new(nTrend,"string")
	
	iii	= 0
	do ddd = -30,0,6
	
		; Update 'bDate' to be Valid Time instead of Initial Time
		utc	:= utc_base+tofloat(ddd)
		utc@units = tunits
		IDATET	:= tostring(toint(cd_calendar(utc,3)))
		IDATES3(iii) = IDATET
		
		vals	:= ind(tcModel.eq.MCODE .and. tcInit.eq.IDATET .and. tcFHR%6.eq.0)
		if(.not.ismissing(vals(0)))then
			aPRS2(iii,0,:dimsizes(tcPRES(vals))-1)	= where(tcPRES(vals).eq.0,fval3,tofloat(tcPRES(vals)))
			aPRS2(iii,1,:dimsizes(tcFHR(vals))-1)	= tofloat(tcFHR(vals))+tofloat(ddd)
		end if
		iii = iii+1
	end do
	vals := ind(.not.ismissing(aPRS2(:,0,0)))
	if(.not.ismissing(vals(0)))then		print("MSG: Found these cycles --> "+stringOut(IDATES3(vals)))
	else					print("WARNING: No cycles were found.")
	end if
end if




;================================================================
; Read BDECK data.
;================================================================

if(fileexists(ADIR+BDECK))then

print("MSG: Reading BDECK file --> "+ADIR+BDECK)
system("echo 'working' > "+ODIR+"status."+LONGSID+".log")
data	:= asciiread(ADIR+BDECK,-1,"string")

; Get Wind Radii - only keep indices for 34 kts.
windRadB	= toint(str_squeeze(str_get_field(data,12,",")))
vals		:= ind(windRadB.eq.34 .or. windRadB.eq.0)

; Get Latitude
tmp	:= tochar(str_squeeze(str_get_field(data(vals),7,",")))
;printVarSummary(tmp)
if(dimsizes(dimsizes(tmp)).eq.1)then
	tmp := new((/1,dimsizes(tochar(str_squeeze(str_get_field(data(vals),7,","))))/),"character")
	tmp(0,:) = tochar(str_squeeze(str_get_field(data(vals),7,",")))
end if
;print(N)
tcLatsB	= tofloat(tostring(tmp(:,:dimsizes(tmp(0,:))-2)))/10.;
;print(tcLats(:100))


; Get Longitude
tmp         := tochar(str_squeeze(str_get_field(data(vals),8,",")))
if(dimsizes(dimsizes(tmp)).eq.1)then
	tmp := new((/1,dimsizes(tochar(str_squeeze(str_get_field(data(vals),7,","))))/),"character")
	tmp(0,:) = tochar(str_squeeze(str_get_field(data(vals),7,",")))
end if
tcLonsB	= -1.*tofloat(tostring(tmp(:,:dimsizes(tmp(0,:))-2)))/10.

; Get Intensity
tcINTB	= toint(str_squeeze(str_get_field(data(vals),9,",")))

; Get Central Pressure
tcPRESB	= toint(str_squeeze(str_get_field(data(vals),10,",")))

; Get Dates
tcValidB= str_squeeze(str_get_field(data(vals),3,","))

; Get Classification
tcCLASB	= str_squeeze(str_get_field(data(vals),11,","))

else
	tcValidB = "MISSING"
end if


; Add best track (1 "future" location, 1 current location, 5 prior locations)
; Note: bHrs should be a dynamic variable
bHrs	= tofloat(ispan(-30,126,6))
	bHrs!0 = "bHrs"
	bHrs&bHrs = bHrs
bHrsI	= bHrs-6
bTRK	= new((/2,dimsizes(bHrs)/),"float",fval3)
	bTRK!1 = "bHrs"
	bTRK&bHrs = bHrs
bINT	= new((/2,dimsizes(bHrs)/),"float",fval3)
	bINT!1 = "bHrs"
	bINT&bHrs = bHrs


iii	= 0
do ddd = -30,126,6
	
	; Update 'bDate' to be Valid Time instead of Initial Time
	utc	:= utc_base+tofloat(ddd)
	utc@units = tunits
	if(isvar("IDATESB"))then	IDATESB := array_append_record(IDATESB,tostring(toint(cd_calendar(utc,3))),0)
	else				IDATESB	:= tostring(toint(cd_calendar(utc,3)))
	end if
	
	vals := ind(tcValidB.eq.IDATESB(iii))
	if(.not.ismissing(vals(0)))then
		bTRK(0,iii)	= tcLatsB(vals)
		bTRK(1,iii)	= tcLonsB(vals)
		bINT(0,iii)	= tcINTB(vals)
		bINT(1,iii)	= tcPRESB(vals)
	end if
	iii = iii+1
end do


bHrs3	= new((/dimsizes(allDates)/),"float",fval3)
bTRK3	= new((/2,dimsizes(allDates)/),"float",fval3)
	bTRK3!1 = "bHrs"
bINT3	= new((/2,dimsizes(allDates)/),"float",fval3)
	bINT3!1 = "bHrs"
bCLAS3	= new((/dimsizes(allDates)/),"string")
TC_FOUND = False
do ddd = 0,dimsizes(allDates)-1
	tmp	:= tochar(tostring(allDates(ddd)))
	tmp0	:= tochar(tostring(allDates(0)))
	offset	= cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
				  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0)\
		- cd_inv_calendar(toint(tostring(tmp0(0:3))),toint(tostring(tmp0(4:5))),\
				  toint(tostring(tmp0(6:7))),toint(tostring(tmp0(8:9))),0,0,tunits,0)
	
	vals		:= ind(tcValidB.eq.allDates(ddd))
	bHrs3(ddd)	= tofloat(offset)
	if(ismissing(vals(0)))then	continue
	else				bTRK3(0,ddd)	= tcLatsB(vals)
					bTRK3(1,ddd)	= tcLonsB(vals)
					bINT3(0,ddd)	= tcINTB(vals)
					bINT3(1,ddd)	= tcPRESB(vals)
					bCLAS3(ddd)	= tcCLASB(vals)
	end if
	
	
	; Determine if any TC cycles found for NHC verification rules
	if(.not.TC_FOUND)then
		if(any((/"HU","TS","TD","SS","SD"/).eq.bCLAS3(ddd)))then   TC_FOUND = True   end if
	end if
end do

; Turn off verification is no TC cycles were found.
if(.not.TC_FOUND)then
	print("WARNING: No cycles found with this storm as an official TC.")
	print("WARNING: Turning off Verification.")
	doVer = False
end if





print("")
print("MSG: *************STEP THREE************")
print("MSG: Produce guidance graphics.")
system("echo 'working' > "+ODIR+"status."+LONGSID+".log")





; Workstation Resources
wtype	= "png"
;wtype	= "x11"
wtype@wkWidth = 2048
wtype@wkHeight = 2048


; General Track Resources
mpres				:= True
mpres@gsnDraw			= False
mpres@gsnFrame			= False
mpres@gsnMaximize		= False
mpres@mpLimitMode		= "LatLon"
mpres@mpFillOn			= False
;mpres@mpFillDrawOrder		= "PreDraw"
mpres@mpDataBaseVersion		= "MediumRes"
mpres@mpDataSetName		= "Earth..4"
mpres@mpDataResolution		= "Coarsest"
mpres@mpOutlineDrawOrder	= "PreDraw"
mpres@mpOutlineBoundarySets	= "AllBoundaries"
mpres@mpOutlineSpecifiers	= "United States : States"
mpres@mpGeophysicalLineColor	= "gray20"
mpres@mpGeophysicalLineThicknessF= 1.5
mpres@mpNationalLineColor	= "gray20"
mpres@mpNationalLineThicknessF	= 1.5
mpres@mpUSStateLineColor	= "gray20"
mpres@mpUSStateLineThicknessF	= 1.5
if(gridOn)then
	mpres@mpGridAndLimbOn		= True
	mpres@mpGridLineColor		= "gray70"
	mpres@mpGridLineThicknessF	= 1.5
	mpres@mpGridLineDashPattern	= 2
end if
mpres@mpShapeMode		= "FreeAspect"
mpres@vpWidthF			= 0.8
mpres@vpHeightF			= 0.4
mpres@tiMainString		= "Late Track Guidance"
mpres@tiMainOffsetYF		= 0.05
mpres@tmXBMajorLengthF		= 0.005
mpres@tmXBMajorOutwardLengthF	= 0.0
mpres@tmXBMinorLengthF		= 0.0025
mpres@tmXBMinorOutwardLengthF	= 0.0
mpres@tmYLMajorLengthF		= 0.005
mpres@tmYLMajorOutwardLengthF	= 0.0
mpres@tmYLMinorLengthF		= 0.0025
mpres@tmYLMinorOutwardLengthF	= 0.0


; General Intensity Resources
xyres			:= True
xyres@gsnDraw		= False
xyres@gsnFrame		= False
xyres@gsnMaximize	= False
xyres@xyDashPattern	= 0
xyres@xyLineThicknessF	= 10.0
xyres@xyMarkLineMode	= "MarkLines"
xyres@xyMarkerColor	= "black"
xyres@xyMarkerSizeF	= 0.020
xyres@xyMarkerThicknessF= 8.0
;xyres@mpShapeMode	= "FreeAspect"
xyres@vpWidthF		= 0.8
xyres@vpHeightF		= 0.4
xyres@tiMainFont	= 22
xyres@tiMainFontHeightF	= 0.025
xyres@tiMainOffsetYF	= 0.05
xyres@tiXAxisString	= "Forecast Lead Time [h]"
xyres@tiXAxisOffsetYF	= 0.015
xyres@tiYAxisOffsetXF	= 0.015


; General polyline resources
plres			:= True


; General marker resources
mkres			:= True

; General text resources (A)
txares			:= True
txares@txFontHeightF	= 0.009
txares@txJust		= "CenterLeft"

; General text resources (B)
txbres			:= True
txbres@txJust		= "CenterLeft"
txbres@txFont		= 25
txbres@txFontColor	= "black"
txbres@txFontHeightF	= 0.009






;================================================================
; Produce track guidance for current forecast
;================================================================

if(doPlot1)then

print("MSG: Producing graphics for late model track guidance.")
DO_THIS = True

; Find optimal map boundaries by automatically resizing (if necessary)
maxLat	= max((/max(aTRK(:,0,:)),max(bTRK(0,:))/))
minLat	= min((/min(aTRK(:,0,:)),min(bTRK(0,:))/))
rngLat	= abs(maxLat-minLat)
maxLon	= max((/max(aTRK(:,1,:)),max(bTRK(1,:))/))
minLon	= min((/min(aTRK(:,1,:)),min(bTRK(1,:))/))
rngLon	= abs(maxLon-minLon)
llratio	= rngLat/rngLon
if(ismissing(llratio))then
	print("WARNING: Late model track guidance can't be produced.")
	DO_THIS = False
end if



if(DO_THIS)then

if(llratio.lt.0.5)then
	maxLat	= maxLat + 0.5*abs(0.5*rngLon - rngLat)
	minLat	= minLat - 0.5*abs(0.5*rngLon - rngLat)
	rngLat	= abs(maxLat-minLat)
else if(llratio.gt.0.5)then
	maxLon	= maxLon + 0.5*abs(2.*rngLat - rngLon)
	minLon	= minLon - 0.5*abs(2.*rngLat - rngLon)
	rngLon	= abs(maxLon-minLon)
end if  end if
;print("MSG: Correcting map lat/lon ratio from "+llratio+" to "+(rngLat/rngLon))
	

; Set up the output file
oFile	= ODIR+"TrackGuidance."+LONGSID+"."+IDATE
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Add additional resources for this part "1a"
mpres1a = mpres
if(1.1*rngLat.lt.20.)then
	mpres1a@gsnMajorLatSpacing	= 2.
	mpres1a@gsnMinorLatSpacing	= 1.
else
	mpres1a@gsnMajorLatSpacing	= 5.
	mpres1a@gsnMinorLatSpacing	= 1.
end if
if(1.1*rngLon.lt.20.)then
	mpres1a@gsnMajorLonSpacing	= 2.
	mpres1a@gsnMinorLonSpacing	= 1.
else
	mpres1a@gsnMajorLonSpacing	= 5.
	mpres1a@gsnMinorLonSpacing	= 1.
end if
mpres1a@mpMaxLonF			= maxLon+(0.05*rngLon)
mpres1a@mpMinLonF			= minLon-(0.05*rngLon)
mpres1a@mpMaxLatF			= maxLat+(0.05*rngLat)
mpres1a@mpMinLatF			= minLat-(0.05*rngLat)
mpres1a@tiMainString			= "Late Track Guidance"
if(gridOn)then
	mpres1a@mpGridLatSpacingF	= mpres1a@gsnMajorLatSpacing
	mpres1a@mpGridLonSpacingF	= mpres1a@gsnMajorLonSpacing
end if


plres1a = plres
mkres1a = mkres
txares1a = txares
txbres1a = txbres


; Plot a blank map
plot	:= gsn_csm_map_ce(wks,mpres1a)
;dum1	:= new((/dimsizes(TRKmodels)+1,50/),"graphic")
dum1	:= new(dimsizes(TRKmodels)+1,"graphic")
;dum2	:= new((/dimsizes(TRKmodels)+1,50/),"graphic")
dum2	:= new(dimsizes(TRKmodels)+1,"graphic")
dum3	:= new((/dimsizes(TRKmodels)+1,50/),"graphic")


; Info about plot size
getvalues plot
	"vpYF" : vpTOP
	"vpXF" : vpLFT
	"vpHeightF" : vpHGT
	"vpWidthF" : vpWID
end getvalues
vpRGT = vpLFT+vpWID
vpBOT = vpTOP-vpHGT


; Add best track
plres1a@gsLineThicknessF	= 15.0
plres1a@gsLineColor	= "black"
mkres1a@gsMarkerSizeF	= 12.0
mkres1a@gsMarkerColor	= "black"
mkres1a@gsMarkerIndex	= tc_marker
mkres1a@gsMarkerThicknessF= 5.0
txares1a@txFontColor	= "black"
txares1a@txFontHeightF	= 0.005
vals	:= ind(bHrs.ge.-12. .and. bHrs.le.126.)
vals2	:= ind(toint(bHrs)%12.eq.0 .and. bHrs.ge.-12. .and. bHrs.le.126.)
if(.not.all(ismissing(bTRK(:,vals))))then
	dum1(dimsizes(TRKmodels))	= gsn_add_polyline(wks,plot,bTRK(1,vals),bTRK(0,vals),plres1a)
	dum2(dimsizes(TRKmodels))	= gsn_add_polymarker(wks,plot,bTRK(1,vals),bTRK(0,vals),mkres1a)
	dum3(dimsizes(TRKmodels),vals2)	= gsn_add_text(wks,plot,tostring(toint(bHrs(vals2))),\
								bTRK(1,vals2)+0.3,\
								bTRK(0,vals2)+0.3,\
								txares1a)
end if


; Add best track legend
plres1a@gsLineThicknessF	= 5.0
mkres1a@gsMarkerSizeF = 8.0
mkres1a@gsMarkerThicknessF = 3.0
txares1a@txFontColor	= "black"
txares1a@txFontHeightF	= 0.009
gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+0.0525,mkres1a)
gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+0.0525,vpTOP+0.0525/),plres1a)
gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+0.0525,txares1a)


; Add model tracks
do ppp = 0,dimsizes(TRKmodels)-1
	ppp2 = dimsizes(TRKmodels)-1-ppp
	if(all(ismissing(aTRK(ppp2,0,:))))then	print("WARNING: "+TRKmodels(ppp)+" track is unavailable for "+IDATE+".")
						continue
	end if
	
	tmp	:= toint(where(ismissing(aTRK(ppp2,2,:)), fval2, aTRK(ppp2,2,:)))
	vals2	:= ind(tmp%12.eq.0)
		
	plres1a@gsLineColor   = TRKcolors(ppp2)
	plres1a@gsLineThicknessF	= 8.0
	mkres1a@gsMarkerSizeF = 8.0
	mkres1a@gsMarkerColor = TRKcolors(ppp2)
	mkres1a@gsMarkerIndex = TRKmarkers(ppp2)
	mkres1a@gsMarkerThicknessF = 3.0
	txares1a@txFontColor   = TRKcolors(ppp2)
	txares1a@txFontHeightF	= 0.005

	dum1(ppp)	= gsn_add_polyline(wks,plot,aTRK(ppp2,1,:),aTRK(ppp2,0,:),plres1a)
	dum2(ppp)	= gsn_add_polymarker(wks,plot,aTRK(ppp2,1,:),aTRK(ppp2,0,:),mkres1a)		
	
	if(.not.ismissing(vals2(0)))then
		dum3(ppp,:dimsizes(vals2)-1) = gsn_add_text(wks,plot,tostring(toint(aTRK(ppp2,2,vals2))),\
							    aTRK(ppp2,1,vals2)+0.3,aTRK(ppp2,0,vals2)+0.3,txares1a)
	end if		
	
	; Add legend info for each model
	plres1a@gsLineThicknessF	= 5.0
	mkres1a@gsMarkerSizeF = 8.0
	mkres1a@gsMarkerThicknessF = 3.0
	txares1a@txFontHeightF	= 0.009
	gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ppp2%2),mkres1a)
	gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp2/2)*0.25*vpWID),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.04/),\
			     (/vpTOP+0.0325-0.02*tofloat(ppp2%2),vpTOP+0.0325-0.02*tofloat(ppp2%2)/),plres1a)
	gsn_text_ndc(wks,TRKlongname(ppp2),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ppp2%2),txares1a)
end do


; Add a sub-title
txares1a@txFontHeightF	= 0.015
txares1a@txJust		= "CenterCenter"
txares1a@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,0.50,0.75,txares1a)


; Add disclaimer
xloc = mpres1a@mpMinLonF + 0.02*(mpres1a@mpMaxLonF-mpres1a@mpMinLonF)
yloc = mpres1a@mpMinLatF + 0.02*(mpres1a@mpMaxLatF-mpres1a@mpMinLatF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")


; Finish plot
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if


end if ;DO_THIS



print("MSG: Producing graphics for early model track guidance.")
DO_THIS = True

; Find map boundaries. Resize if necessary
maxLat	= max((/max(aTRKI(:,0,:)),max(bTRK(0,:))/))
minLat	= min((/min(aTRKI(:,0,:)),min(bTRK(0,:))/))
rngLat	= abs(maxLat-minLat)
maxLon	= max((/max(aTRKI(:,1,:)),max(bTRK(1,:))/))
minLon	= min((/min(aTRKI(:,1,:)),min(bTRK(1,:))/))
rngLon	= abs(maxLon-minLon)
llratio	= rngLat/rngLon
if(ismissing(llratio))then
	print("WARNING: Early model track guidance can't be produced.")
	DO_THIS = False
end if


if(DO_THIS)then

if(llratio.lt.0.5)then
	maxLat	= maxLat + 0.5*abs(0.5*rngLon - rngLat)
	minLat	= minLat - 0.5*abs(0.5*rngLon - rngLat)
	rngLat	= abs(maxLat-minLat)
else if(llratio.gt.0.5)then
	maxLon	= maxLon + 0.5*abs(2.*rngLat - rngLon)
	minLon	= minLon - 0.5*abs(2.*rngLat - rngLon)
	rngLon	= abs(maxLon-minLon)
end if  end if
;print("MSG: Correcting map lat/lon ratio from "+llratio+" to "+(rngLat/rngLon))
	

; Set up the output file
oFile	= str_sub_str(ODIR,IDATE,IDATE06)+"TrackGuidance."+LONGSID+"."+IDATE06+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Set resources for plots and additional features
mpres1b	= mpres
if(1.1*rngLat.lt.20.)then
	mpres1b@gsnMajorLatSpacing	= 2.
	mpres1b@gsnMinorLatSpacing	= 1.
else
	mpres1b@gsnMajorLatSpacing	= 5.
	mpres1b@gsnMinorLatSpacing	= 1.
end if
if(1.1*rngLon.lt.20.)then
	mpres1b@gsnMajorLonSpacing	= 2.
	mpres1b@gsnMinorLonSpacing	= 1.
else
	mpres1b@gsnMajorLonSpacing	= 5.
	mpres1b@gsnMinorLonSpacing	= 1.
end if
mpres1b@mpMaxLonF			= maxLon+(0.05*rngLon)
mpres1b@mpMinLonF			= minLon-(0.05*rngLon)
mpres1b@mpMaxLatF			= maxLat+(0.05*rngLat)
mpres1b@mpMinLatF			= minLat-(0.05*rngLat)
if(gridOn)then
	mpres1b@mpGridLatSpacingF	= mpres1b@gsnMajorLatSpacing
	mpres1b@mpGridLonSpacingF	= mpres1b@gsnMajorLonSpacing
end if
mpres1b@tiMainString		= "Early Track Guidance"


plres1b = plres
mkres1b = mkres
txares1b = txares
txbres1b = txbres


; Plot a blank map
plot	:= gsn_csm_map_ce(wks,mpres1a)
;dum1	:= new((/dimsizes(TRKmodels)+1,50/),"graphic")
dum1	:= new(dimsizes(TRKmodelsI)+1,"graphic")
;dum2	:= new((/dimsizes(TRKmodels)+1,50/),"graphic")
dum2	:= new(dimsizes(TRKmodelsI)+1,"graphic")
dum3	:= new((/dimsizes(TRKmodelsI)+1,50/),"graphic")


; Info about plot size
getvalues plot
	"vpYF" : vpTOP
	"vpXF" : vpLFT
	"vpHeightF" : vpHGT
	"vpWidthF" : vpWID
end getvalues
vpRGT = vpLFT+vpWID
vpBOT = vpTOP-vpHGT


; Add best track
plres1b@gsLineThicknessF	= 15.0
plres1b@gsLineColor	= "black"
mkres1b@gsMarkerSizeF	= 12.0
mkres1b@gsMarkerColor	= "black"
mkres1b@gsMarkerIndex	= tc_marker
mkres1b@gsMarkerThicknessF= 5.0
txares1b@txFontColor	= "black"
txares1b@txFontHeightF	= 0.005
vals	:= ind(bHrsI.ge.-12. .and. bHrsI.le.126.)
vals2	:= ind(toint(bHrsI)%12.eq.0 .and. bHrsI.ge.-6. .and. bHrsI.le.126.)
if(.not.all(ismissing(bTRK(:,vals))))then
	dum1(dimsizes(TRKmodelsI))	= gsn_add_polyline(wks,plot,bTRK(1,vals),bTRK(0,vals),plres1b)
	dum2(dimsizes(TRKmodelsI))	= gsn_add_polymarker(wks,plot,bTRK(1,vals),bTRK(0,vals),mkres1b)
	dum3(dimsizes(TRKmodelsI),vals2)	= gsn_add_text(wks,plot,tostring(toint(bHrsI(vals2))),\
								bTRK(1,vals2)+0.3,\
								bTRK(0,vals2)+0.3,\
								txares1b)
end if


; Add best track legend
plres1b@gsLineThicknessF	= 5.0
mkres1b@gsMarkerSizeF = 8.0
mkres1b@gsMarkerThicknessF = 3.0
txares1b@txFontColor	= "black"
txares1b@txFontHeightF	= 0.009
gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+0.0525,mkres1b)
gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+0.0525,vpTOP+0.0525/),plres1b)
gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+0.0525,txares1b)


; Add model tracks
do ppp = 0,dimsizes(TRKmodelsI)-1
	ppp2 = dimsizes(TRKmodelsI)-1-ppp
	if(all(ismissing(aTRKI(ppp2,0,:))))then	print("WARNING: "+TRKmodelsI(ppp2)+" track is unavailable for "+IDATE06+".")
						continue
	end if
	
	tmp	:= toint(where(ismissing(aTRKI(ppp2,2,:)), fval2, aTRKI(ppp2,2,:)))
	vals2	:= ind(tmp%12.eq.0)
		
	plres1b@gsLineColor   = TRKcolorsI(ppp2)
	plres1b@gsLineThicknessF	= 8.0
	mkres1b@gsMarkerSizeF = 8.0
	mkres1b@gsMarkerColor = TRKcolorsI(ppp2)
	mkres1b@gsMarkerIndex = TRKmarkersI(ppp2)
	mkres1b@gsMarkerThicknessF = 3.0
	txares1b@txFontColor   = TRKcolorsI(ppp2)
	txares1b@txFontHeightF	= 0.005
	
	dum1(ppp)	= gsn_add_polyline(wks,plot,aTRKI(ppp2,1,:),aTRKI(ppp2,0,:),plres1b)
	dum2(ppp)	= gsn_add_polymarker(wks,plot,aTRKI(ppp2,1,:),aTRKI(ppp2,0,:),mkres1b)	
		
	if(.not.ismissing(vals2(0)))then
		dum3(ppp,:dimsizes(vals2)-1)	= gsn_add_text(wks,plot,tostring(toint(aTRKI(ppp2,2,vals2))),\
									aTRKI(ppp2,1,vals2)+0.3,\
									aTRKI(ppp2,0,vals2)+0.3,\
									txares1b)
	end if
			
	; Add legend info for each model
	plres1b@gsLineThicknessF	= 5.0
	mkres1b@gsMarkerSizeF = 8.0
	mkres1b@gsMarkerThicknessF = 3.0
	txares1b@txFontHeightF	= 0.009
	gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ppp2%2),mkres1b)
	gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp2/2)*0.25*vpWID),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.04/),\
			     (/vpTOP+0.0325-0.02*tofloat(ppp2%2),vpTOP+0.0325-0.02*tofloat(ppp2%2)/),plres1b)
	gsn_text_ndc(wks,TRKlongnameI(ppp2),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ppp2%2),txares1b)
end do


; Add a sub-title
txares1b@txFontHeightF	= 0.015
txares1b@txJust		= "CenterCenter"
txares1b@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE06,0.50,0.75,txares1b)


; Add disclaimer
xloc = mpres1b@mpMinLonF + 0.02*(mpres1b@mpMaxLonF-mpres1b@mpMinLonF)
yloc = mpres1b@mpMinLatF + 0.02*(mpres1b@mpMaxLatF-mpres1b@mpMinLatF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")


; Finish plot
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if


end if ;DO_THIS

end if ;doPlot1



;================================================================
; Produce intensity guidance for current forecast (wind)
;================================================================

if(doPlot2)then

print("MSG: Producing graphics for late model intensity guidance.")
DO_THIS = True

; Find the XY plot boundaries
maxInt	:= max((/max(aINT(:,0,:)),max(bINT(0,:))/))
minInt	:= min((/min(aINT(:,0,:)),min(bINT(0,:))/))
rngInt	:= abs(maxInt-minInt)
if(ismissing(rngInt))then
	print("WARNING: Late model intensity guidance can't be produced.")
	DO_THIS = False
end if



if(DO_THIS)then

if(1.2*rngInt.ge.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,\
				 20)
else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,\
				 10)
else if(1.2*rngInt.lt.50.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,\
				 5)
end if  end if  end if


; Setup the output file & workstation
oFile	= ODIR+"IntensityGuidance."+LONGSID+"."+IDATE
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Update resources
xyres2a = xyres
xyres2a@tiMainString	= "Late Intensity Guidance"
xyres2a@tiYAxisString	= "Wind Speed [kt]"
xyres2a@tmXBMode	= "Explicit"	
xyres2a@tmXBValues	= XYHrs
xyres2a@tmXBLabels	= "" + xyres2a@tmXBValues
xyres2a@tmXTOn		= False
xyres2a@trXMinF		= min(XYHrs)-6
xyres2a@trXMaxF		= max(XYHrs)+6
xyres2a@tmYLMode	= "Explicit"	
xyres2a@tmYLValues	= IntMjrGrd
xyres2a@tmYLLabels	= "" + xyres2a@tmYLValues
xyres2a@tmYROn		= False
xyres2a@trYMaxF		= maxInt+(0.10*rngInt)
xyres2a@trYMinF		= minInt-(0.10*rngInt)
xyres2a@xyMarkers		= tc_marker

plres2a = plres
mkres2a = mkres
txares2a = txares
txbres2a = txbres


; Plot the best track
;plot	:= gsn_csm_xy(wks,bHrs({-6:6.}),bINT(0,{-6:6.}),res)
plot	:= gsn_csm_xy(wks,bHrs,bINT(0,:),xyres2a)


; Info about plot size
getvalues plot
	"vpYF" : vpTOP
	"vpXF" : vpLFT
	"vpHeightF" : vpHGT
	"vpWidthF" : vpWID
end getvalues
vpRGT = vpLFT+vpWID
vpBOT = vpTOP-vpHGT


; Plot the best track legend
plres2a@gsLineThicknessF	= 5.0
plres2a@gsLineColor	= "black"
plres2a@gsLineDashPattern	= 0
mkres2a@gsMarkerSizeF	= 10.0
mkres2a@gsMarkerColor	= "black"
mkres2a@gsMarkerIndex	= tc_marker
mkres2a@gsMarkerThicknessF= 3.0
txares2a@txFontColor	= "black"
gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+0.0525,mkres2a)
gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+0.0525,vpTOP+0.0525/),plres2a)
gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+0.0525,txares2a)


; Draw the background grid
dum1	:= new(dimsizes(XYHrs),"graphic")
dum2	:= new(dimsizes(IntMjrGrd),"graphic")

plres2a@gsLineThicknessF	= 5.0
plres2a@gsLineColor	= "black"
plres2a@gsLineDashPattern	= 0
plres2a@tfPolyDrawOrder	= "PreDraw"
dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres2a@trYMinF,xyres2a@trYMaxF/),plres2a)

plres2a@gsLineThicknessF	= 1.5
plres2a@gsLineDashPattern	= 11
plres2a@gsLineColor	= "grey70"
do fff = 0,dimsizes(XYHrs)-1
	if(XYHrs(fff).eq.0)then continue end if
	dum1(fff)	= gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres2a@trYMinF,xyres2a@trYMaxF/),plres2a)
end do
do fff = 0,dimsizes(IntMjrGrd)-1
	dum2(fff)	= gsn_add_polyline(wks,plot,(/xyres2a@trXMinF,xyres2a@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres2a)
end do


do ppp = 0,dimsizes(INTmodels)-1
	ppp2	= dimsizes(INTmodels)-1-ppp
	tmp	:= toint(where(ismissing(aINT(ppp2,1,:)), fval2, aINT(ppp2,1,:)))
	vals	:= ind(tmp%6.eq.0)
	
	if(ismissing(vals(0)) .or. \
	   all(ismissing(aINT(ppp2,0,vals))) .or. \
	   dimsizes(vals).eq.1)then
		print("WARNING: "+INTmodels(ppp2)+" intensity is unavailable for "+IDATE+".")
		continue
	end if

	; Update resources
	xyres2a@xyLineThicknessF	= 10.0
	xyres2a@xyMarkers		= INTmarkers(ppp2)
	xyres2a@xyMarkerColor	= INTcolors(ppp2)
	xyres2a@xyMarkerThicknessF	= 6.0
	xyres2a@xyMarkerSizeF	= 0.015
	xyres2a@xyLineColor		= INTcolors(ppp2)
	xyres2a@tiXAxisString	= ""
	xyres2a@tiYAxisString	= ""
	
	plres2a@gsLineThicknessF	= 5.0
	plres2a@gsLineColor	= INTcolors(ppp2)
	plres2a@gsLineDashPattern	= 0
		
	mkres2a@gsMarkerSizeF	= 10.0
	mkres2a@gsMarkerColor	= INTcolors(ppp2)
	mkres2a@gsMarkerIndex	= INTmarkers(ppp2)
	mkres2a@gsMarkerThicknessF= 3.0
		
	txares2a@txFontColor   	= INTcolors(ppp2)
	
	; Overlay the new intensity line
	overlay(plot,gsn_csm_xy(wks,aINT(ppp2,1,vals),aINT(ppp2,0,vals),xyres2a))
	
	; Add legend info for each model
	gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ppp2%2),mkres2a)
	gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp2/2)*0.25*vpWID),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.04/),\
			     (/vpTOP+0.0325-0.02*tofloat(ppp2%2),vpTOP+0.0325-0.02*tofloat(ppp2%2)/),plres2a)
	gsn_text_ndc(wks,INTlongname(ppp2),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ppp2%2),txares2a)
end do


; Add a sub-title
txares2a@txFontHeightF	= 0.018
txares2a@txJust		= "CenterCenter"
txares2a@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,0.59,0.855,txares2a)


; Add disclaimer
xloc = xyres2a@trXMinF + 0.02*(xyres2a@trXMaxF-xyres2a@trXMinF)
yloc = xyres2a@trYMinF + 0.02*(xyres2a@trYMaxF-xyres2a@trYMinF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")


; Actually plot here
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if


end if ;DO_THIS



print("MSG: Producing graphics for early model intensity guidance.")

; Find the XY plot boundaries
maxInt	:= max((/max(aINTI(:,0,:)),max(bINT(0,:))/))
minInt	:= min((/min(aINTI(:,0,:)),min(bINT(0,:))/))
rngInt	:= abs(maxInt-minInt)
if(ismissing(rngInt))then
	print("WARNING: Early model intensity guidance can't be produced.")
	DO_THIS = False
end if



if(DO_THIS)then

if(1.2*rngInt.ge.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,\
				 20)
else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,\
				 10)
else if(1.2*rngInt.lt.50.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,\
				 5)
end if  end if  end if


; Setup the output file
oFile	= str_sub_str(ODIR,IDATE,IDATE06)+"IntensityGuidance."+LONGSID+"."+IDATE06+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Update resources
xyres2b = xyres
xyres2b@tiMainString	= "Early Intensity Guidance"
xyres2b@tiYAxisString	= "Wind Speed [kt]"
xyres2b@tmXBMode	= "Explicit"	
xyres2b@tmXBValues	= XYHrs
xyres2b@tmXBLabels	= "" + xyres2b@tmXBValues
xyres2b@tmXTOn		= False
xyres2b@trXMinF		= min(XYHrs)-6
xyres2b@trXMaxF		= max(XYHrs)+6
xyres2b@tmYLMode	= "Explicit"	
xyres2b@tmYLValues	= IntMjrGrd
xyres2b@tmYLLabels	= "" + xyres2b@tmYLValues
xyres2b@tmYROn		= False
xyres2b@trYMaxF		= maxInt+(0.10*rngInt)
xyres2b@trYMinF		= minInt-(0.10*rngInt)
xyres2b@xyMarkers	= tc_marker

plres2b = plres
mkres2b = mkres
txares2b = txares
txbres2b = txbres


; Plot the best track
;plot	:= gsn_csm_xy(wks,bHrs({-6:6.}),bINT(0,{-6:6.}),res)
plot	:= gsn_csm_xy(wks,bHrsI,bINT(0,:),xyres2b)


; Info about plot size
getvalues plot
	"vpYF" : vpTOP
	"vpXF" : vpLFT
	"vpHeightF" : vpHGT
	"vpWidthF" : vpWID
end getvalues
vpRGT = vpLFT+vpWID
vpBOT = vpTOP-vpHGT


; Plot the best track legend
plres2b@gsLineThicknessF	= 5.0
plres2b@gsLineColor	= "black"
plres2b@gsLineDashPattern	= 0
mkres2b@gsMarkerSizeF	= 10.0
mkres2b@gsMarkerColor	= "black"
mkres2b@gsMarkerIndex	= tc_marker
mkres2b@gsMarkerThicknessF= 3.0
txares2b@txFontColor	= "black"
gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+0.0525,mkres2b)
gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+0.0525,vpTOP+0.0525/),plres2b)
gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+0.0525,txares2b)


; Draw the background grid
dum1	:= new(dimsizes(XYHrs),"graphic")
dum2	:= new(dimsizes(IntMjrGrd),"graphic")

plres2b@gsLineThicknessF	= 5.0
plres2b@gsLineColor	= "black"
plres2b@gsLineDashPattern	= 0
plres2b@tfPolyDrawOrder	= "PreDraw"
dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres2b@trYMinF,xyres2b@trYMaxF/),plres2b)

plres2b@gsLineThicknessF	= 1.5
plres2b@gsLineDashPattern	= 11
plres2b@gsLineColor	= "grey70"
do fff = 0,dimsizes(XYHrs)-1
	if(XYHrs(fff).eq.0)then continue end if
	dum1(fff)	= gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres2b@trYMinF,xyres2b@trYMaxF/),plres2b)
end do
do fff = 0,dimsizes(IntMjrGrd)-1
	dum2(fff)	= gsn_add_polyline(wks,plot,(/xyres2b@trXMinF,xyres2b@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres2b)
end do


do ppp = 0,dimsizes(INTmodelsI)-1
	ppp2	= dimsizes(INTmodelsI)-1-ppp
	tmp	:= toint(where(ismissing(aINTI(ppp2,1,:)), fval2, aINTI(ppp2,1,:)))
	vals	:= ind(tmp%6.eq.0)
	
	if(ismissing(vals(0)) .or. \
	   all(ismissing(aINTI(ppp2,0,vals))) .or. \
	   dimsizes(vals).eq.1)then
		print("WARNING: "+INTmodelsI(ppp2)+" early intensity is unavailable for "+IDATE06+".")
		continue
	end if

	; Update resources
	xyres2b@xyLineThicknessF= 10.0
	xyres2b@xyMarkers	= INTmarkersI(ppp2)
	xyres2b@xyMarkerColor	= INTcolorsI(ppp2)
	xyres2b@xyMarkerThicknessF = 6.0
	xyres2b@xyMarkerSizeF	= 0.015
	xyres2b@xyLineColor	= INTcolorsI(ppp2)
	xyres2b@tiXAxisString	= ""
	xyres2b@tiYAxisString	= ""
	
	plres2b@gsLineThicknessF	= 5.0
	plres2b@gsLineColor	= INTcolorsI(ppp2)
	plres2b@gsLineDashPattern	= 0
		
	mkres2b@gsMarkerSizeF	= 10.0
	mkres2b@gsMarkerColor	= INTcolorsI(ppp2)
	mkres2b@gsMarkerIndex	= INTmarkersI(ppp2)
	mkres2b@gsMarkerThicknessF= 3.0
		
	txares2b@txFontColor   	= INTcolorsI(ppp2)
	
	; Overlay the new intensity line
	overlay(plot,gsn_csm_xy(wks,aINTI(ppp2,1,vals),aINTI(ppp2,0,vals),xyres2b))

	; Add legend info for each model
	gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ppp2%2),mkres2b)
	gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp2/2)*0.25*vpWID),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.04/),\
			     (/vpTOP+0.0325-0.02*tofloat(ppp2%2),vpTOP+0.0325-0.02*tofloat(ppp2%2)/),plres2b)
	gsn_text_ndc(wks,INTlongnameI(ppp2),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ppp2%2),txares2b)
end do


; Add a sub-title
txares2b@txFontHeightF	= 0.018
txares2b@txJust		= "CenterCenter"
txares2b@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE06,0.59,0.855,txares2b)


; Add disclaimer
xloc = xyres2b@trXMinF + 0.02*(xyres2b@trXMaxF-xyres2b@trXMinF)
yloc = xyres2b@trYMinF + 0.02*(xyres2b@trYMaxF-xyres2b@trYMinF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")

; Actually plot here
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if

end if ;DO_THIS

end if ;doPlot2



;================================================================
; Produce intensity guidance for current forecast (pressure)
;================================================================

if(doPlot3)then

print("MSG: Producing graphics for late model pressure guidance.")
DO_THIS = True

; Find the XY plot boundaries
maxPrs	:= max((/max(aPRS(:,0,:)),max(bINT(1,:))/))
minPrs	:= min((/min(aPRS(:,0,:)),min(bINT(1,:))/))
rngPrs	:= abs(maxPrs-minPrs)
if(ismissing(rngPrs))then
	print("WARNING: Late model pressure guidance can't be produced.")
	DO_THIS = False
end if



if(DO_THIS)then

if(1.2*rngPrs.ge.50.)then
	PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%10 + 10,\
				 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%10,\
				 10)
else if(1.2*rngPrs.ge.20. .and. 1.2*rngPrs.lt.50.)then
	PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%5 + 5,\
				 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%5,\
				 5)
else if(1.2*rngPrs.lt.20.)then
	PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%2 + 2,\
				 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%2,\
				 2)
end if  end if  end if


; Setup the output file
oFile	= ODIR+"PressureGuidance."+LONGSID+"."+IDATE
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Set resources
xyres3a = xyres
xyres3a@tiMainString	= "Late Pressure Guidance"
xyres3a@tiYAxisString	= "Pressure [mb]"
xyres3a@tiYAxisOffsetXF	= 0.015
xyres3a@tmXBMode	= "Explicit"	
xyres3a@tmXBValues	= XYHrs
xyres3a@tmXBLabels	= "" + xyres3a@tmXBValues
xyres3a@tmXTOn		= False
xyres3a@trXMinF		= min(XYHrs)-6
xyres3a@trXMaxF		= max(XYHrs)+6
xyres3a@tmYLMode	= "Explicit"	
xyres3a@tmYLValues	= PrsMjrGrd
xyres3a@tmYLLabels	= "" + xyres3a@tmYLValues
xyres3a@tmYROn		= False
xyres3a@trYMaxF		= maxPrs+(0.10*rngPrs)
xyres3a@trYMinF		= minPrs-(0.10*rngPrs)
xyres3a@xyMarkers	= tc_marker

plres3a = plres
mkres3a = mkres
txares3a = txares
txbres3a = txbres


; Plot the best track
;plot	:= gsn_csm_xy(wks,bHrs({-6:6.}),bINT(1,{-6:6.}),res)
plot	:= gsn_csm_xy(wks,bHrs,bINT(1,:),xyres3a)


; Info about plot size
getvalues plot
	"vpYF" : vpTOP
	"vpXF" : vpLFT
	"vpHeightF" : vpHGT
	"vpWidthF" : vpWID
end getvalues
vpRGT = vpLFT+vpWID
vpBOT = vpTOP-vpHGT


; Plot the best track legend
plres3a@gsLineThicknessF	= 5.0
plres3a@gsLineColor	= "black"
plres3a@gsLineDashPattern	= 0
mkres3a@gsMarkerSizeF	= 10.0
mkres3a@gsMarkerColor	= "black"
mkres3a@gsMarkerIndex	= tc_marker
mkres3a@gsMarkerThicknessF= 3.0
txares3a@txFontColor	= "black"
gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+0.0525,mkres3a)
gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+0.0525,vpTOP+0.0525/),plres3a)
gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+0.0525,txares3a)


; Draw the background grid
dum1	:= new(dimsizes(XYHrs),"graphic")
dum2	:= new(dimsizes(PrsMjrGrd),"graphic")

plres3a@gsLineThicknessF	= 5.0
plres3a@gsLineColor	= "black"
plres3a@gsLineDashPattern	= 0
plres3a@tfPolyDrawOrder	= "PreDraw"
dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres3a@trYMinF,xyres3a@trYMaxF/),plres3a)

plres3a@gsLineThicknessF	= 1.5
plres3a@gsLineDashPattern	= 11
plres3a@gsLineColor	= "grey70"
do fff = 0,dimsizes(XYHrs)-1
	if(XYHrs(fff).eq.0)then   continue   end if
	dum1(fff) = gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres3a@trYMinF,xyres3a@trYMaxF/),plres3a)
end do
do fff = 0,dimsizes(PrsMjrGrd)-1
	dum2(fff) = gsn_add_polyline(wks,plot,(/xyres3a@trXMinF,xyres3a@trXMaxF/),(/PrsMjrGrd(fff),PrsMjrGrd(fff)/),plres3a)
end do


do ppp = 0,dimsizes(PRSmodels)-1
	ppp2	= dimsizes(PRSmodels)-1-ppp
	tmp	:= toint(where(ismissing(aPRS(ppp2,1,:)), fval2, aPRS(ppp2,1,:)))
	vals	:= ind(tmp%6.eq.0)
	
	if(ismissing(vals(0)) .or. \
	   all(ismissing(aPRS(ppp2,0,vals))) .or. \
	   dimsizes(vals).eq.1)then
		print("WARNING: "+PRSmodels(ppp2)+" pressure is unavailable for "+IDATE+".")
		continue
	end if
	
	; Update resources
	xyres3a@xyLineThicknessF= 10.0
	xyres3a@xyMarkers	= PRSmarkers(ppp2)
	xyres3a@xyMarkerColor	= PRScolors(ppp2)
	xyres3a@xyMarkerThicknessF = 6.0
	xyres3a@xyMarkerSizeF	= 0.015
	xyres3a@xyLineColor	= PRScolors(ppp2)
	xyres3a@tiXAxisString	= ""
	xyres3a@tiYAxisString	= ""
	
	plres3a@gsLineThicknessF	= 5.0
	plres3a@gsLineColor	= PRScolors(ppp2)
	plres3a@gsLineDashPattern	= 0
		
	mkres3a@gsMarkerSizeF	= 10.0
	mkres3a@gsMarkerColor	= PRScolors(ppp2)
	mkres3a@gsMarkerIndex	= PRSmarkers(ppp2)
	mkres3a@gsMarkerThicknessF= 3.0
		
	txares3a@txFontColor   	= PRScolors(ppp2)
		
	; Overlay the new pressure line
	overlay(plot,gsn_csm_xy(wks,aPRS(ppp2,1,vals),aPRS(ppp2,0,vals),xyres3a))
	
	; Add legend info for each model
	gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ppp2%2),mkres3a)
	gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp2/2)*0.25*vpWID),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.04/),\
			     (/vpTOP+0.0325-0.02*tofloat(ppp2%2),vpTOP+0.0325-0.02*tofloat(ppp2%2)/),plres3a)
	gsn_text_ndc(wks,PRSlongname(ppp2),vpLFT+(tofloat(ppp2/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ppp2%2),txares3a)
end do


; Add a sub-title
txares3a@txFontHeightF	= 0.018
txares3a@txJust		= "CenterCenter"
txares3a@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,0.59,0.855,txares3a)


; Add disclaimer
xloc = xyres3a@trXMinF + 0.02*(xyres3a@trXMaxF-xyres3a@trXMinF)
yloc = xyres3a@trYMinF + 0.02*(xyres3a@trYMaxF-xyres3a@trYMinF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")

; Actually plot here
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if

end if ;DO_THIS

end if ;doPlot3



;================================================================
; Produce track forecast trends for current TC
;================================================================

if(doPlot4)then

; Update resources
mpres4a = mpres
plres4a = plres
plres4a@gsLineThicknessF = 5.0
mkres4a = mkres
txares4a = txares
txbres4a = txbres

mpres4b = mpres
mpres4b@tiMainOffsetYF = 0.015
plres4b = plres
mkres4b = mkres
txares4b = txares
txbres4b = txbres


; Loop over selected models to produce 6-cycle trends & lifetime track maps
do mmm = 0,dimsizes(TRKmodelsT)-1

	thisModel = TRKmodelsT(mmm)
	
	if(all(ismissing(aTRK3(mmm,:,0,:))))then	print("WARNING: "+thisModel+" track is unavailable through "+IDATE+".")
							continue
	end if

	print("MSG: Producing graphic for "+thisModel+" track trend.")

	; Find map boundaries. Resize if necessary
	maxLat	:= max((/max(aTRK2(mmm,:,0,:)),max(bTRK(0,:))/))
	minLat	:= min((/min(aTRK2(mmm,:,0,:)),min(bTRK(0,:))/))
	rngLat	:= abs(maxLat-minLat)
	maxLon	:= max((/max(aTRK2(mmm,:,1,:)),max(bTRK(1,:))/))
	minLon	:= min((/min(aTRK2(mmm,:,1,:)),min(bTRK(1,:))/))
	rngLon	:= abs(maxLon-minLon)
	llratio	:= rngLat/rngLon
	if(ismissing(llratio))then
		print("WARNING: "+thisModel+" track trend can't be produced.")
		continue
	end if
	if(llratio.lt.0.5)then
		maxLat	= maxLat + 0.5*abs(0.5*rngLon - rngLat)
		minLat	= minLat - 0.5*abs(0.5*rngLon - rngLat)
		rngLat	= abs(maxLat-minLat)
	else if(llratio.gt.0.5)then
		maxLon	= maxLon + 0.5*abs(2.*rngLat - rngLon)
		minLon	= minLon - 0.5*abs(2.*rngLat - rngLon)
		rngLon	= abs(maxLon-minLon)
	end if  end if
	;print("MSG: Correcting map lat/lon ratio from "+llratio+" to "+(rngLat/rngLon))
	

	; Set up the output file
	oFile	= ODIR+"TrackTrend."+thisModel+"."+LONGSID+"."+IDATE
	if(fileexists(oFile))then   system("rm -f "+oFile)   end if
	wks	:= gsn_open_wks(wtype,oFile)
	
	
	;Create a hurricane symbol marker
	tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


	; Set resources for plots and additional features
	;res@tiMainString		= TRKmodelsT(mmm)+" Late Track Trend"
	if(1.1*rngLat.lt.20.)then
		mpres4a@gsnMajorLatSpacing	= 2.
		mpres4a@gsnMinorLatSpacing	= 1.
	else
		mpres4a@gsnMajorLatSpacing	= 5.
		mpres4a@gsnMinorLatSpacing	= 1.
	end if
	if(1.1*rngLon.lt.20.)then
		mpres4a@gsnMajorLonSpacing	= 2.
		mpres4a@gsnMinorLonSpacing	= 1.
	else
		mpres4a@gsnMajorLonSpacing	= 5.
		mpres4a@gsnMinorLonSpacing	= 1.
	end if
	mpres4a@mpMaxLonF			= maxLon+(0.05*rngLon)
	mpres4a@mpMinLonF			= minLon-(0.05*rngLon)
	mpres4a@mpMaxLatF			= maxLat+(0.05*rngLat)
	mpres4a@mpMinLatF			= minLat-(0.05*rngLat)
	if(gridOn)then
		mpres4a@mpGridLatSpacingF	= mpres4a@gsnMajorLatSpacing
		mpres4a@mpGridLonSpacingF	= mpres4a@gsnMajorLonSpacing
	end if


	; Plot a blank map
	plot	:= gsn_csm_map_ce(wks,mpres4a)
	dum1	:= new(nTrend+1,"graphic")
	dum2	:= new(nTrend+1,"graphic")
	dum3	:= new((/nTrend+1,50/),"graphic")


	; Info about plot size
	getvalues plot
		"vpYF" : vpTOP
		"vpXF" : vpLFT
		"vpHeightF" : vpHGT
		"vpWidthF" : vpWID
	end getvalues
	vpRGT = vpLFT+vpWID
	vpBOT = vpTOP-vpHGT


	; Add best track
	plres4a@gsLineColor	:= "black"
	plres4a@gsLineThicknessF= 12.0
	mkres4a@gsMarkerSizeF	= 15.0
	mkres4a@gsMarkerColor	:= "red"
	mkres4a@gsMarkerIndex	= tc_marker
	mkres4a@gsMarkerThicknessF= 5.0
	if(.not.all(ismissing(bTRK)))then
		dum1(nTrend)	= gsn_add_polyline(wks,plot,bTRK(1,:),bTRK(0,:),plres4a)
		dum2(nTrend)	= gsn_add_polymarker(wks,plot,bTRK(1,:),bTRK(0,:),mkres4a)
	end if


	; Add best track legend
	plres4a@gsLineThicknessF	= 3.5
	mkres4a@gsMarkerSizeF	= 8.0
	mkres4a@gsMarkerThicknessF= 5.0
	txares4a@txFontHeightF	= 0.009
	txares4a@txFontColor	:= "black"
	txares4a@txJust		= "CenterLeft"
	gsn_polymarker_ndc(wks,vpLFT+(0.75*vpWID)+0.02,vpTOP+0.0325,mkres4a)
	gsn_polyline_ndc(wks,(/vpLFT+(0.75*vpWID),vpLFT+(0.75*vpWID)+0.04/),(/vpTOP+0.0325,vpTOP+0.0325/),plres4a)
	gsn_text_ndc(wks,"BEST: Best Track",vpLFT+(0.75*vpWID)+0.045,vpTOP+0.0325,txares4a)


	; Add model tracks
	do ttt = 0,nTrend-1
		; Check if the HWRF-B track is available
		if(all(ismissing(aTRK2(mmm,ttt,0,:))))then
			print("WARNING: "+thisModel+" track is unavailable for "+IDATES(ttt)+".")
			continue
		end if
	
		; Retain only forecast hours that are divisible by 6
		tmp	:= toint(where(ismissing(aTRK2(mmm,ttt,2,:)), fval2, aTRK2(mmm,ttt,2,:)))
		vals	:= ind(tmp%6.eq.0)
		vals2	:= ind(tmp%12.eq.0)	
		if(ismissing(vals2(0)) .or. ismissing(vals(0)))then   continue   end if
		if(dimsizes(vals).eq.1)then   continue   end if
	
		
		; Add each model as polyline & polymarker objects
		plres4a@gsLineColor	:= TRDcolors(ttt,:)
		plres4a@gsLineThicknessF= 10.0
		mkres4a@gsMarkerSizeF	= 6.0
		mkres4a@gsMarkerColor	:= TRDcolors(ttt,:)
		mkres4a@gsMarkerIndex	= TRDmarkers(ttt)
		mkres4a@gsMarkerThicknessF= 5.0
		txares4a@txFontColor	:= TRDcolors(ttt,:)
		txares4a@txFontHeightF	= 0.005
		dum1(ttt)	= gsn_add_polyline(wks,plot,aTRK2(mmm,ttt,1,vals),aTRK2(mmm,ttt,0,vals),plres4a)
		dum2(ttt)	= gsn_add_polymarker(wks,plot,aTRK2(mmm,ttt,1,vals),aTRK2(mmm,ttt,0,vals),mkres4a)
		dum3(ttt,:dimsizes(vals2)-1)	= gsn_add_text(wks,plot,tostring(toint(aTRK2(mmm,ttt,2,vals2))),aTRK2(mmm,ttt,1,vals2)+0.2,aTRK2(mmm,ttt,0,vals2)+0.2,txares4a)
	
		; Add legend info for each model
		plres4a@gsLineThicknessF	= 3.5
		mkres4a@gsMarkerSizeF	= 8.0
		mkres4a@gsMarkerThicknessF= 5.0
		txares4a@txFontHeightF	= 0.009
		gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres4a)
		gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ttt/2)*0.25*vpWID),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.04/),\
				     (/vpTOP+0.0325-0.02*tofloat(ttt%2),vpTOP+0.0325-0.02*tofloat(ttt%2)/),plres4a)
		gsn_text_ndc(wks,IDATES(ttt),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ttt%2),txares4a)
	end do


	; Add a title,sub-title
	txares4a@txFont		= 22
	txares4a@txFontHeightF	= 0.018
	txares4a@txFontColor	:= "black"
	txares4a@txJust		= "CenterLeft"
	gsn_text_ndc(wks,thisModel+" Late Track Trend",vpLFT,vpTOP+0.075,txares4a)
	txares4a@txFont		= 21
	txares4a@txFontHeightF	= 0.016
	gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+") valid on "+IDATE,vpLFT,vpTOP+0.055,txares4a)
	
	
	; Add disclaimer
	xloc = mpres4a@mpMinLonF + 0.02*(mpres4a@mpMaxLonF-mpres4a@mpMinLonF)
	yloc = mpres4a@mpMinLatF + 0.02*(mpres4a@mpMaxLatF-mpres4a@mpMinLatF)
	add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")

	; Finish plot
	draw(plot)
	frame(wks)
	
	
	; Remove white space on plot
	if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if
	


	; Produce lifetime "spaghetti" tracks the current model.
	print("MSG: Producing graphic for "+thisModel+" lifetime track.")
	
	if(all(ismissing(aTRK3(mmm,:,0,:))))then	print("WARNING: "+INTmodelsT(mmm)+" track is unavailable for any cycles.")
							continue
	end if

	; Find map boundaries. Resize if necessary
	maxLat	:= max((/max(aTRK3(mmm,:,0,:)),max(bTRK(0,:))/))
	minLat	:= min((/min(aTRK3(mmm,:,0,:)),min(bTRK(0,:))/))
	rngLat	:= abs(maxLat-minLat)
	maxLon	:= max((/max(aTRK3(mmm,:,1,:)),max(bTRK(1,:))/))
	minLon	:= min((/min(aTRK3(mmm,:,1,:)),min(bTRK(1,:))/))
	rngLon	:= abs(maxLon-minLon)
	llratio	:= rngLat/rngLon
	if(llratio.lt.0.5)then		maxLat	= maxLat + 0.5*abs(0.5*rngLon - rngLat)
					minLat	= minLat - 0.5*abs(0.5*rngLon - rngLat)
					rngLat	= abs(maxLat-minLat)
	else if(llratio.gt.0.5)then	maxLon	= maxLon + 0.5*abs(2.*rngLat - rngLon)
					minLon	= minLon - 0.5*abs(2.*rngLat - rngLon)
					rngLon	= abs(maxLon-minLon)
	end if  end if
	;print("MSG: Correcting map lat/lon ratio from "+llratio+" to "+(rngLat/rngLon))

	; Set up the output file
	oFile	= ODIR+"AllTracks."+TRKmodelsT(mmm)+"."+LONGSID+"."+IDATE
	if(fileexists(oFile))then   system("rm -f "+oFile)   end if
	wks	:= gsn_open_wks(wtype,oFile)
			
	;Create a hurricane symbol marker
	tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
	
	; Set resources for plots and additional features
	;mpres4b@tiMainString		= MCODE+" Lifetime Tracks"
	if(1.1*rngLat.lt.20.)then	mpres4b@gsnMajorLatSpacing	= 2.
					mpres4b@gsnMinorLatSpacing	= 1.
	else				mpres4b@gsnMajorLatSpacing	= 5.
					mpres4b@gsnMinorLatSpacing	= 1.
	end if
	if(1.1*rngLon.lt.20.)then	mpres4b@gsnMajorLonSpacing	= 2.
					mpres4b@gsnMinorLonSpacing	= 1.
	else				mpres4b@gsnMajorLonSpacing	= 5.
					mpres4b@gsnMinorLonSpacing	= 1.
	end if
	mpres4b@mpMaxLonF		= maxLon+(0.05*rngLon)
	mpres4b@mpMinLonF		= minLon-(0.05*rngLon)
	mpres4b@mpMaxLatF		= maxLat+(0.05*rngLat)
	mpres4b@mpMinLatF		= minLat-(0.05*rngLat)
	if(gridOn)then			mpres4b@mpGridLatSpacingF	= mpres4b@gsnMajorLatSpacing
					mpres4b@mpGridLonSpacingF	= mpres4b@gsnMajorLonSpacing
	end if
	
	
	; Plot a blank map
	plot	:= gsn_csm_map_ce(wks,mpres4b)
	dum1	:= new(dimsizes(allDates),"graphic")
	dum2	:= new(dimsizes(allDates),"graphic")
	dum3	:= new(dimsizes(allDates),"graphic")
	dum4	:= new(dimsizes(allDates),"graphic")
	
	; Info about plot size
	getvalues plot
		"vpYF" : vpTOP
		"vpXF" : vpLFT
		"vpHeightF" : vpHGT
		"vpWidthF" : vpWID
	end getvalues
	vpRGT = vpLFT+vpWID
	vpBOT = vpTOP-vpHGT


	; Add best track legend
	plres4b@gsLineThicknessF	= 2.5
	plres4b@gsLineColor	:= "black"
	mkres4b@gsMarkerSizeF	= 6.0
	mkres4b@gsMarkerColor	:= "black"
	mkres4b@gsMarkerIndex	= tc_marker
	mkres4b@gsMarkerThicknessF= 3.5
	txares4b@txFontColor	:= "black"
	txares4b@txFontHeightF	= 0.005
	txares4b@txFont		= 22
	txares4b@txJust		= "CenterLeft"
	gsn_polymarker_ndc(wks,vpRGT+0.02,vpTOP-0.005,mkres4b)
	gsn_polyline_ndc(wks,(/vpRGT+0.01,vpRGT+0.03/),(/vpTOP-0.005,vpTOP-0.005/),plres4b)
	gsn_text_ndc(wks,"BEST",vpRGT+0.035,vpTOP-0.005,txares4b)


	; Add model tracks
	dum3	:= new(dimsizes(allDates),"graphic")
	do ttt = 0,dimsizes(allDates)-1
		; Check if the HWRF-B track is available
		if(all(ismissing(aTRK3(mmm,ttt,0,:))))then   continue   end if
	
		tmp	:= tochar(tostring(allDates(ttt)))
		dTXT	= tostring(tmp(6:7))+"/"+tostring(tmp(8:9))
	
		; Retain only forecast hours that are divisible by 6
		tmp	:= toint(where(ismissing(aTRK3(mmm,ttt,2,:)), fval2, aTRK3(mmm,ttt,2,:)))
		vals	:= ind(tmp%6.eq.0)
		vals2	:= ind(aTRK3(mmm,ttt,2,:).ge.0 .and. tmp%6.eq.0)
		if(ismissing(vals2(0)))then   continue   end if
		if(dimsizes(vals2).eq.1)then  continue   end if
		
		; Add each model as polyline & polymarker objects
		plres4b@gsLineColor	:= LIFEcolors(ttt,:)
		plres4b@gsLineThicknessF	= 6.0
		dum3(ttt)	= gsn_add_polyline(wks,plot,aTRK3(mmm,ttt,1,vals),aTRK3(mmm,ttt,0,vals),plres4b)
		
		; Add legend info for each model
		plres4b@gsLineThicknessF	= 3.5
		txares4b@txFontHeightF	= 0.005
		txares4b@txFontColor	:= LIFEcolors(ttt,:)
		txares4b@txFont		= 22
		txares4b@txJust		= "CenterLeft"
		;gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres)
		gsn_polyline_ndc(wks,(/vpRGT+0.01,vpRGT+0.03/),(/vpTOP-0.005-tofloat(ttt+1)*0.008,vpTOP-0.005-tofloat(ttt+1)*0.008/),plres4b)
		gsn_text_ndc(wks,dTXT,vpRGT+0.035,vpTOP-0.005-tofloat(ttt+1)*0.008,txares4b)
	end do

	; Add best track
	plres4b@gsLineColor	:= "black"
	plres4b@gsLineThicknessF= 12.0
	txares4b@txJust		= "CenterCenter"
	txares4b@txFontColor	:= "black"
	txares4b@txFontHeightF	= 0.009
	if(.not.all(ismissing(bTRK3)))then
	do ttt = 0,dimsizes(allDates)-1
		
		; Draw the best track line. Make sure the best track value is not missing.
		; Also, NHC-defined tropical cyclones have solid lines. Other classes have dashed lines.
		if(any((/"HU","TS","TD","SS","SD"/).eq.bCLAS3(ttt)))then	plres4b@gsLineDashPattern = 0
		else								plres4b@gsLineDashPattern = 11
		end if
		if(ttt.lt.dimsizes(allDates)-1 .and. ttt.lt.dimsizes(bTRK3(0,:))-1)then
			if(.not.ismissing(bTRK3(0,ttt)) .and. .not.ismissing(bTRK3(0,ttt+1)))then
				dum1(ttt) = gsn_add_polyline(wks,plot,bTRK3(1,ttt:ttt+1),bTRK3(0,ttt:ttt+1),plres4b)
			end if
		end if
	
		; Draw the symbol that correspond to the best track classification.
		if(.not.ismissing(bCLAS3(ttt)))then
			if(bCLAS3(ttt).eq."HU")then				txares4b@txFont = 37
										dum2(ttt)	= gsn_add_text(wks,plot,"p",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			else if(any((/"TS","SS"/).eq.bCLAS3(ttt)))then		txares4b@txFont = 35
										dum2(ttt)	= gsn_add_text(wks,plot,"m",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			else if(any((/"TD","SD"/).eq.bCLAS3(ttt)))then		txares4b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"O",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			else if(bCLAS3(ttt).eq."LO")then			txares4b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"L",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			else if(any((/"DB","WV","EX"/).eq.bCLAS3(ttt)))then	txares4b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"x",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			else							txares4b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"?",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
			end if  end if  end if  end if  end if
		else
			txares4b@txFont = 22
			dum2(ttt)	= gsn_add_text(wks,plot,"?",bTRK3(1,ttt),bTRK3(0,ttt),txares4b)
		end if
	end do ;ttt
	end if


	; Add a title,sub-title
	txares4b@txFont		= 22
	txares4b@txFontHeightF	= 0.018
	txares4b@txFontColor	:= "black"
	txares4b@txJust		= "CenterLeft"
	gsn_text_ndc(wks,TRKmodelsT(mmm)+" Lifetime Track Forecasts",vpLFT,vpTOP+0.03,txares4b)
	txares4b@txFont		= 21
	txares4b@txFontHeightF	= 0.016
	txares4b@txJust		= "CenterRight"
	gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+")",vpRGT,vpTOP+0.01,txares4b)
	txares4b@txJust		= "CenterLeft"
	gsn_text_ndc(wks,sprinti("%0.2i",dimsizes(allDates))+" Forecasts thru "+IDATE,vpLFT,vpTOP+0.01,txares4b)
	
	; Add disclaimer
	xloc = mpres4b@mpMinLonF + 0.02*(mpres4b@mpMaxLonF-mpres4b@mpMinLonF)
	yloc = mpres4b@mpMinLatF + 0.02*(mpres4b@mpMaxLatF-mpres4b@mpMinLatF)
	add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
	
	; Finish plot
	draw(plot)
	frame(wks)
	
	; Remove white space on plot
	if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if

end do ;mmm


end if ;doPlot4


;================================================================
; Produce intensity forecast trends for current TC
;================================================================

if(doPlot5)then

; Update resources
xyres5a = xyres
xyres5a@xyLineThicknessF	= 15.0
xyres5a@xyLineColor		:= "black"
xyres5a@xyMarkerColor	= "red"
xyres5a@xyMarkerThicknessF	= 10.0
xyres5a@tiXAxisOffsetYF	= 0.014
xyres5a@tiXAxisFontHeightF	= 0.020
xyres5a@tiYAxisString	= "Wind Speed [kt]"
xyres5a@tiYAxisOffsetXF	= 0.014
xyres5a@tiYAxisFontHeightF	= 0.020
xyres5a@tmXBMode		= "Explicit"	
xyres5a@tmXBValues		= XYHrs2
xyres5a@tmXBLabels		= "" + xyres5a@tmXBValues
xyres5a@tmXTOn		= False
xyres5a@trXMinF		= min(XYHrs2)-6
xyres5a@trXMaxF		= max(XYHrs2)+6
xyres5a@tmYLMode		= "Explicit"
xyres5a@tmXBLabelFontHeightF:= 0.018
xyres5a@tmYLLabelFontHeightF:= 0.018
xyres5a@tmXBLabelDeltaF	:= -0.75
xyres5a@tmYLLabelDeltaF	:= -0.75
plres5a = plres
plres5a@gsLineDashPattern = 0
mkres5a = mkres
txares5a = txares
txares5a@txFontHeightF	= 0.010
txbres5a = txbres

xyres5b = xyres
xyres5b@tiXAxisString	= "Date [DD/HH]"
xyres5b@tiXAxisOffsetYF	= 0.012
xyres5b@tiXAxisFontHeightF= 0.020
xyres5b@tiYAxisString	= "Wind Speed [kt]"
xyres5b@tiYAxisOffsetXF	= 0.012
xyres5b@tiYAxisFontHeightF= 0.020
xyres5b@tmXBMode	= "Explicit"	
xyres5b@tmXBValues	:= bHrs3(ind(toint(bHrs3)%24.eq.0))
xyres5b@tmXBLabels	:= "" + changeTimeFmt(tostring(allDates(ind(toint(bHrs3)%24.eq.0))),"YYYYMMDDHH","DD/HH",0.)
xyres5b@tmXTOn		= False
xyres5b@tmXBLabelFontHeightF:= 0.015
xyres5b@tmYLLabelFontHeightF:= 0.018
xyres5b@tmXBLabelDeltaF	:= -0.75
xyres5b@tmYLLabelDeltaF	:= -0.75
xyres5b@trXMinF		:= min(bHrs3)-6
xyres5b@trXMaxF		:= max(bHrs3)+6
xyres5b@vpXF		:= 0.1
delete([/xyres5b@xyDashPattern,xyres5b@xyLineThicknessF,xyres5b@xyMarkLineMode,xyres5b@xyMarkerColor,xyres5b@xyMarkerSizeF,xyres5b@xyMarkerThicknessF/])
plres5b = plres
mkres5b = mkres
txares5b = txares
txares5b@txFontThicknessF	= 5.0
txbres5b = txbres


do mmm = 0,dimsizes(INTmodelsT)-1

	; Define the current intensity trend model as 'thisModel'
	thisModel = INTmodelsT(mmm)
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; PART ONE --> INTENSITY TREND GRAPHICS ;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Print some information to the terminal
	print("MSG: Producing graphic for "+thisModel+" intensity trend.")
	if(all(ismissing(aINT3(mmm,:,0,:))))then	print("WARNING: "+thisModel+" intensity is unavailable through "+IDATE+".")
							continue
	end if

	; Find intensity graphic Y limits.
	maxInt	:= max((/max(aINT2(mmm,:,0,:)),max(bINT(0,:))/))
	minInt	:= min((/min(aINT2(mmm,:,0,:)),min(bINT(0,:))/))
	if(maxInt.eq.minInt)then
		rngInt := 50
	else if(ismissing(maxInt) .or. ismissing(minInt))
		rngInt := fval3
		rngInt@_FillValue = fval3
	else
		rngInt := abs(maxInt-minInt)
	end if  end if
	if(ismissing(rngInt))then	print("WARNING: Unable to retrieve intensity range for "+thisModel+".")
					continue
	end if

	; Setup an array of intensity values
	if(1.2*rngInt.ge.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,\
					 20)
	else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,\
					 10)
	else if(1.2*rngInt.lt.50.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,\
					 5)
	end if  end if  end if
		
	; Set up the output file
	oFile	= ODIR+"IntensityTrend."+thisModel+"."+LONGSID+"."+IDATE
	if(fileexists(oFile))then   system("rm -f "+oFile)   end if
	wks	= gsn_open_wks(wtype,oFile)
	
	;Create a hurricane symbol marker
	tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)

	; Update resources
	xyres5a@xyMarkers	= tc_marker
	;xyres5a@tiMainString	= thisModel+" Late Intensity Trend"
	xyres5a@tmYLValues	:= IntMjrGrd
	xyres5a@tmYLLabels	:= "" + xyres5a@tmYLValues
	xyres5a@tmYROn		= False
	xyres5a@trYMaxF		= maxInt+(0.10*rngInt)
	xyres5a@trYMinF		= max((/0.,minInt-(0.10*rngInt)/))

	; Plot the best track
	plot	:= gsn_csm_xy(wks,bHrs(:),bINT(0,:),xyres5a)
	;plot	:= gsn_csm_blank_plot(wks,res)
	
	; Info about plot size
	getvalues plot
		"vpYF" : vpTOP
		"vpXF" : vpLFT
		"vpHeightF" : vpHGT
		"vpWidthF" : vpWID
	end getvalues
	vpRGT = vpLFT+vpWID
	vpBOT = vpTOP-vpHGT

	; Plot best track legend
	mkres5a@gsMarkerColor	:= "red"
	mkres5a@gsMarkerIndex	= tc_marker
	mkres5a@gsMarkerThicknessF= 3.0
	mkres5a@gsMarkerSizeF	= 8.0
	plres5a@gsLineThicknessF	= 3.5
	plres5a@gsLineColor	:= "black"
	plres5a@gsLineDashPattern	= 0
	txares5a@txFontColor	:= "black"
	txares5a@txFontHeightF	= 0.009
	gsn_polymarker_ndc(wks,vpLFT+(0.75*vpWID)+0.02,vpTOP+0.0325,mkres5a)
	gsn_polyline_ndc(wks,(/vpLFT+(0.75*vpWID),vpLFT+(0.75*vpWID)+0.04/),(/vpTOP+0.0325,vpTOP+0.0325/),plres5a)
	gsn_text_ndc(wks,"BEST: Best Track",vpLFT+(0.75*vpWID)+0.045,vpTOP+0.0325,txares5a)

	; Draw grid lines on the graphic.
	dum1	:= new(dimsizes(XYHrs2),"graphic")
	dum2	:= new(dimsizes(IntMjrGrd),"graphic")

	; Draw a thick vertical black line at 0 h
	plres5a@gsLineThicknessF	= 5.0
	plres5a@gsLineColor	:= "black"
	plres5a@gsLineDashPattern	= 0
	plres5a@tfPolyDrawOrder	= "PreDraw"
	dum1(ind(XYHrs2.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres5a@trYMinF,xyres5a@trYMaxF/),plres5a)

	; Draw thinner vertical grey lines for major forecast hours (dum1)
	; Draw thinner horizontal grey lines for major intensity levels (dum2)
	plres5a@gsLineThicknessF	= 1.5
	plres5a@gsLineDashPattern	= 11
	plres5a@gsLineColor	:= "grey70"
	do fff = 0,dimsizes(XYHrs2)-1
		if(XYHrs2(fff).eq.0)then continue end if
		dum1(fff)	= gsn_add_polyline(wks,plot,(/tofloat(XYHrs2(fff)),tofloat(XYHrs2(fff))/),(/xyres5a@trYMinF,xyres5a@trYMaxF/),plres5a)
	end do
	do fff = 0,dimsizes(IntMjrGrd)-1
		dum2(fff)	= gsn_add_polyline(wks,plot,(/xyres5a@trXMinF,xyres5a@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres5a)
	end do

	; Add model tracks
	do ttt = 0,nTrend-1

		tmp	:= toint(where(ismissing(aINT2(mmm,ttt,1,:)), fval2, aINT2(mmm,ttt,1,:)))
		vals	:= ind(tmp%6.eq.0)

		; Check if current model intensity is available
		if(all(ismissing(aINT2(mmm,ttt,0,:))) .or. dimsizes(vals).eq.1)then
			print("WARNING: "+thisModel+" intensity is unavailable for "+IDATES2(ttt)+".")
			continue
		end if
	
		; Update resources
		xyres5a@xyMarkLineMode	= "MarkLines"
		xyres5a@xyLineThicknessF= 12.0
		xyres5a@xyMarkers	:= TRDmarkers(ttt)
		xyres5a@xyMarkerColor	:= TRDcolors(ttt,:)
		xyres5a@xyMarkerThicknessF	= 4.0
		xyres5a@xyMarkerSizeF	= 0.01
		xyres5a@xyLineColor	:= TRDcolors(ttt,:)
		xyres5a@tiXAxisString	= ""
		xyres5a@tiYAxisString	= ""
	
		; Overlay the new intensity line
		overlay(plot,gsn_csm_xy(wks,aINT2(mmm,ttt,1,vals),aINT2(mmm,ttt,0,vals),xyres5a))
	
		; Add legend info for each model
		plres5a@gsLineColor	:= TRDcolors(ttt,:)
		plres5a@gsLineThicknessF	= 3.5
		plres5a@gsLineDashPattern	= 0
		plres5a@tfPolyDrawOrder	= "PostDraw"
			
		mkres5a@gsMarkerSizeF	= 8.0
		mkres5a@gsMarkerColor	:= TRDcolors(ttt,:)
		mkres5a@gsMarkerIndex	= TRDmarkers(ttt)
		mkres5a@gsMarkerThicknessF= 5.0
			
		txares5a@txFontColor	:= TRDcolors(ttt,:)
		txares5a@txFontHeightF	= 0.009
			
		gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres5a)
		gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ttt/2)*0.25*vpWID),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.04/),\
				     (/vpTOP+0.0325-0.02*tofloat(ttt%2),vpTOP+0.0325-0.02*tofloat(ttt%2)/),plres5a)
		gsn_text_ndc(wks,IDATES2(ttt),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ttt%2),txares5a)
	end do ;ttt

	; Add a title,sub-title
	txares5a@txFont		= 22
	txares5a@txFontHeightF	= 0.018
	txares5a@txFontColor	:= "black"
	txares5a@txJust		= "CenterLeft"
	gsn_text_ndc(wks,thisModel+" Late Intensity Trend",vpLFT,vpTOP+0.075,txares5a)
	txares5a@txFont		= 21
	txares5a@txFontHeightF	= 0.016
	gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+") initialized on "+IDATE,vpLFT,vpTOP+0.052,txares5a)
	
	; Add disclaimer
	xloc = xyres5a@trXMinF + 0.02*(xyres5a@trXMaxF-xyres5a@trXMinF)
	yloc = xyres5a@trYMinF + 0.02*(xyres5a@trYMaxF-xyres5a@trYMinF)
	add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")

	; Finish graphic
	draw(plot)
	frame(wks)

	; Remove white space from graphic
	if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if
	


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; PART TWO --> INTENSITY LIFETIME GRAPHICS ;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Produce lifetime "spaghetti" intensity graphic for the current intensity trend model.
	print("MSG: Producing graphic for "+thisModel+" lifetime intensity.")
	if(all(ismissing(aINT3(mmm,:,0,:))))then	print("WARNING: "+thisModel+" intensity is unavailable for any cycles.")
							continue
	end if
	
	; Find plot Max/Min and major tick marks. Resize if necessary
	maxInt	:= max((/max(aINT3(mmm,:,0,:)),max(bINT3(0,:))/))
	minInt	:= min((/min(aINT3(mmm,:,0,:)),min(bINT3(0,:))/))
	if(maxInt.eq.minInt)then
		rngInt := 50
	else if(ismissing(maxInt) .or. ismissing(minInt))
		rngInt := fval3
		rngInt@_FillValue = fval3
	else
		rngInt := abs(maxInt-minInt)
	end if  end if
	if(ismissing(rngInt))then	print("WARNING: Unable to retrieve intensity range for "+thisModel+".")
					continue
	end if
	
	; Setup an array of intensity values
	if(1.2*rngInt.ge.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,\
					 20)
	else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,\
					 10)
	else if(1.2*rngInt.lt.50.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,\
					 5)
	end if  end if  end if

	; Set up the output file
	oFile	= ODIR+"AllIntensity."+INTmodelsT(mmm)+"."+LONGSID+"."+IDATE
	if(fileexists(oFile))then   system("rm -f "+oFile)   end if
	wks	:= gsn_open_wks(wtype,oFile)
			
	;Create a hurricane symbol marker
	tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
	
	; Update resources
	xyres5b@tmYLMode	= "Explicit"
	xyres5b@tmYLValues	:= IntMjrGrd
	xyres5b@tmYLLabels	:= "" + xyres5b@tmYLValues
	xyres5b@tmYROn		= False
	xyres5b@tmYLValues	:= IntMjrGrd
	xyres5b@tmYLLabels	:= "" + xyres5b@tmYLValues
	xyres5b@trYMaxF		= maxInt+(0.10*rngInt)
	xyres5b@trYMinF		= max((/0.,minInt-(0.10*rngInt)/))
	xyres5b@trYMaxF		= maxInt+(0.10*rngInt)
	xyres5b@trYMinF		= max((/0.,minInt-(0.10*rngInt)/))
	;xyres5b@tiMainString	= thisModel+" Late Intensity Trend"

	; Plot a blank map
	;plot	:= gsn_csm_xy(wks,bHrs3(:),bINT3(0,:),res)
	plot	:= gsn_csm_blank_plot(wks,xyres5b)
	dum1	:= new(dimsizes(allDates),"graphic")
	dum2	:= new(dimsizes(allDates),"graphic")
	
	; Info about plot size
	getvalues plot
		"vpYF" : vpTOP
		"vpXF" : vpLFT
		"vpHeightF" : vpHGT
		"vpWidthF" : vpWID
	end getvalues
	vpRGT = vpLFT+vpWID
	vpBOT = vpTOP-vpHGT
	
	; Draw grid lines on the graphic.
	dumA	:= new(dimsizes(bHrs3),"graphic")
	dumB	:= new(dimsizes(IntMjrGrd),"graphic")
	
	; Draw vertical grey lines at major forecast hours
	; Draw horizontal grey lines at major intensity levels
	plres5b@tfPolyDrawOrder	= "PreDraw"
	plres5b@gsLineThicknessF= 1.5
	plres5b@gsLineDashPattern= 11
	plres5b@gsLineColor	:= "grey70"
	do fff = 0,dimsizes(bHrs3)-1
		dumA(fff)	= gsn_add_polyline(wks,plot,(/tofloat(bHrs3(fff)),tofloat(bHrs3(fff))/),(/xyres5b@trYMinF,xyres5b@trYMaxF/),plres5b)
	end do
	do fff = 0,dimsizes(IntMjrGrd)-1
		dumB(fff)	= gsn_add_polyline(wks,plot,(/xyres5b@trXMinF,xyres5b@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres5b)
	end do
	
	; Add best track legend
	plres5b@gsLineThicknessF	= 2.5
	plres5b@gsLineColor	:= "black"
	plres5b@gsLineDashPattern	= 0
	mkres5b@gsMarkerSizeF	= 6.0
	mkres5b@gsMarkerColor	:= "black"
	mkres5b@gsMarkerIndex	= tc_marker
	mkres5b@gsMarkerThicknessF= 3.5
	txares5b@txFontColor	:= "black"
	txares5b@txFontHeightF	= 0.005
	txares5b@txFont		= 22
	txares5b@txJust		= "CenterLeft"
	gsn_polymarker_ndc(wks,vpRGT+0.02,vpTOP-0.005,mkres5b)
	gsn_polyline_ndc(wks,(/vpRGT+0.01,vpRGT+0.03/),(/vpTOP-0.005,vpTOP-0.005/),plres5b)
	gsn_text_ndc(wks,"BEST",vpRGT+0.035,vpTOP-0.005,txares5b)
	
	; Add model intensities
	dum3	:= new(dimsizes(allDates),"graphic")
	do ttt = 0,dimsizes(allDates)-1
		; Check if the HWRF-B track is available
		if(all(ismissing(aINT3(mmm,ttt,0,:))))then   continue   end if
	
		;print("  MSG: Adding track for "+allDates(ttt))
		
		tmp	:= tochar(tostring(allDates(ttt)))
		dTXT	= tostring(tmp(6:7))+"/"+tostring(tmp(8:9))
	
		; Retain only forecast hours that are divisible by 6
		tmp	:= toint(where(ismissing(aINT3(mmm,ttt,1,:)), fval2, aINT3(mmm,ttt,1,:)))
		vals	:= ind(tmp%6.eq.0)
		vals2	:= ind(aINT3(mmm,ttt,1,:).ge.0 .and. tmp%6.eq.0)
		if(ismissing(vals2(0)))then    continue   end if
		if(dimsizes(vals2).eq.1)then   continue   end if
		
			
		; Add each model as polyline & polymarker objects
		plres5b@gsLineColor	:= LIFEcolors(ttt,:)
		plres5b@gsLineThicknessF	= 6.0
		dum3(ttt)	= gsn_add_polyline(wks,plot,aINT3(mmm,ttt,1,vals),aINT3(mmm,ttt,0,vals),plres5b)
		
		; Add legend info for each model
		plres5b@gsLineThicknessF	= 3.5
		txares5b@txFontHeightF	= 0.005
		txares5b@txFontColor	:= LIFEcolors(ttt,:)
		txares5b@txFont		= 22
		txares5b@txJust		= "CenterLeft"
		;gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres)
		gsn_polyline_ndc(wks,(/vpRGT+0.01,vpRGT+0.03/),(/vpTOP-0.005-tofloat(ttt+1)*0.008,vpTOP-0.005-tofloat(ttt+1)*0.008/),plres5b)
		gsn_text_ndc(wks,dTXT,vpRGT+0.035,vpTOP-0.005-tofloat(ttt+1)*0.008,txares5b)
	end do

	; Add best track
	plres5b@gsLineColor	:= "black"
	plres5b@gsLineThicknessF= 12.0
	txares5b@txJust		= "CenterCenter"
	txares5b@txFontColor	:= "black"
	txares5b@txFontHeightF	= 0.015
	if(.not.all(ismissing(bINT3(0,:))))then
	do ttt = 0,dimsizes(allDates)-1
		
		; Draw the best track line. Make sure the best track value is not missing.
		; Also, NHC-defined tropical cyclones have solid lines. Other classes have dashed lines.
		if(any((/"HU","TS","TD","SS","SD"/).eq.bCLAS3(ttt)))then	plres5b@gsLineDashPattern = 0
		else								plres5b@gsLineDashPattern = 11
		end if
		if(ttt.lt.dimsizes(allDates)-1 .and. ttt.lt.dimsizes(bINT(0,:))-1)then
			if(.not.ismissing(bINT3(0,ttt)) .and. .not.ismissing(bINT3(0,ttt+1)))then
				dum1(ttt) = gsn_add_polyline(wks,plot,bHrs3(ttt:ttt+1),bINT3(0,ttt:ttt+1),plres5b)
			end if
		end if
		
		if(.not.ismissing(bCLAS3(ttt)))then
			if(bCLAS3(ttt).eq."HU")then				txares5b@txFont = 37
										dum2(ttt)	= gsn_add_text(wks,plot,"p",bHrs3(ttt),bINT3(0,ttt),txares5b)
			else if(any((/"TS","SS"/).eq.bCLAS3(ttt)))then		txares5b@txFont = 35
										dum2(ttt)	= gsn_add_text(wks,plot,"m",bHrs3(ttt),bINT3(0,ttt),txares5b)
			else if(any((/"TD","SD"/).eq.bCLAS3(ttt)))then		txares5b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"O",bHrs3(ttt),bINT3(0,ttt),txares5b)
			else if(bCLAS3(ttt).eq."LO")then			txares5b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"L",bHrs3(ttt),bINT3(0,ttt),txares5b)
			else if(any((/"DB","WV","EX"/).eq.bCLAS3(ttt)))then	txares5b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"x",bHrs3(ttt),bINT3(0,ttt),txares5b)
			else							txares5b@txFont = 22
										dum2(ttt)	= gsn_add_text(wks,plot,"?",bHrs3(ttt),bINT3(0,ttt),txares5b)
			end if  end if  end if  end if  end if
		else
			txares5b@txFont = 22
			dum2(ttt)	= gsn_add_text(wks,plot,"?",bHrs3(ttt),bINT3(0,ttt),txares5b)
		end if
	end do ;ttt
	end if
	
	; Add a title,sub-title
	txares5b@txFont		= 22
	txares5b@txFontHeightF	= 0.018
	txares5b@txFontColor	:= "black"
	txares5b@txJust		= "CenterLeft"
	gsn_text_ndc(wks,INTmodelsT(mmm)+" Lifetime Intensity Forecasts",vpLFT,vpTOP+0.03,txares5b)
	txares5b@txFont		= 21
	txares5b@txFontHeightF	= 0.016
	txares5b@txJust		= "CenterRight"
	gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+")",vpRGT,vpTOP+0.01,txares5b)
	txares5b@txJust		= "CenterLeft"
	gsn_text_ndc(wks,sprinti("%0.2i",dimsizes(allDates))+" Forecasts thru "+IDATE,vpLFT,vpTOP+0.01,txares5b)
	
	; Add disclaimer
	xloc = xyres5b@trXMinF + 0.02*(xyres5b@trXMaxF-xyres5b@trXMinF)
	yloc = xyres5b@trYMinF + 0.02*(xyres5b@trYMaxF-xyres5b@trYMinF)
	add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
	
	; Finish plot
	draw(plot)
	frame(wks)
	
	; Remove white space on plot
	if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if
	
	
end do ;mmm

end if ;doPlot5



;================================================================
; Produce MSLP forecast trends for current TC
;================================================================

if(doPlot6)then



print("MSG: Producing graphic for late pressure trend.")
print("WARNING: Under construction.")

; Find map boundaries. Resize if necessary
maxInt	:= max((/max(aPRS2(:,0,:)),max(bINT(1,:))/))
minInt	:= min((/min(aPRS2(:,0,:)),min(bINT(1,:))/))
rngInt	:= abs(maxInt-minInt)
if(ismissing(rngInt))then
	print("WARNING: Early model intensity guidance can't be produced.")
	DO_THIS = False
end if



if(DO_THIS)then

if(1.2*rngInt.ge.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,\
				 20)
else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,\
				 10)
else if(1.2*rngInt.lt.50.)then
	IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
				 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,\
				 5)
end if  end if  end if
	

; Set up the output file
oFile	= ODIR+"PressureTrend."+MCODE+"."+LONGSID+"."+IDATE
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
wks	= gsn_open_wks(wtype,oFile)


; Set resources
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
;MK edit: change variable name from long_name to title
;res@tiMainString	= long_name+" Pressure Trend"
res@tiMainString	= title+" Pressure Trend"
res@tiMainFont		= 22
res@tiMainFontHeightF	= 0.025
res@tiMainOffsetYF	= 0.05
res@tiXAxisString	= "Forecast Time [h]"
res@tiXAxisOffsetYF	= 0.015
res@tiYAxisString	= "Pressure [hPa]"
res@tiYAxisOffsetXF	= 0.015
res@tmXBMode		= "Explicit"	
res@tmXBValues		= XYHrs2
res@tmXBLabels		= "" + res@tmXBValues
res@tmXTOn		= False
res@trXMinF		= min(XYHrs2)-6
res@trXMaxF		= max(XYHrs2)+6
res@tmYLMode		= "Explicit"	
res@tmYLValues		= IntMjrGrd
res@tmYLLabels		= "" + res@tmYLValues
res@tmYROn		= False
res@trYMaxF		= maxInt+(0.10*rngInt)
res@trYMinF		= minInt-(0.10*rngInt)

plres			:= True
plres@gsLineDashPattern	= 0

mkres			:= True

txres			:= True
txres@txFontHeightF	= 0.010
txres@txJust		= "CenterLeft"

txres2			:= True
txres2@txJust		= "CenterLeft"
txres2@txFont		= 25
txres2@txFontColor	= "black"
txres2@txFontHeightF	= 0.009


;Create a hurricane symbol marker
tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


; Set location of legend entries manually here
xleg = (/0.22,0.22,0.36,0.36,0.50,0.50,0.64,0.64/)   ; Location of
xtxt = (/0.25,0.25,0.39,0.39,0.53,0.53,0.67,0.67/)   ; legend markers
yleg = (/0.83,0.81,0.83,0.81,0.83,0.81,0.83,0.81/)   ; and text
ytxt = (/0.83,0.81,0.83,0.81,0.83,0.81,0.83,0.81/)   ; strings.


; Plot the best track
res@xyMarkers		= tc_marker
plot	:= gsn_csm_xy(wks,bHrs(:),bINT(1,:),res)


; Plot best track legend
mkres@gsMarkerColor	= "red"
mkres@gsMarkerIndex	= tc_marker
mkres@gsMarkerThicknessF= 3.0
mkres@gsMarkerSizeF	= 8.0
plres@gsLineThicknessF	= 3.5
plres@gsLineColor	= "black"
plres@gsLineDashPattern	= 0
txres@txFontColor	= "black"
gsn_polymarker_ndc(wks,xleg(nTrend),yleg(nTrend),mkres)
gsn_polyline_ndc(wks,(/xleg(nTrend)-0.02,xleg(nTrend)+0.02/),(/yleg(nTrend),yleg(nTrend)/),plres)
gsn_text_ndc(wks,"BEST: Best Track",xtxt(nTrend),ytxt(nTrend),txres)


; Plot a vertical black line at 0 h
dum1	:= new(dimsizes(XYHrs2),"graphic")
dum2	:= new(dimsizes(IntMjrGrd),"graphic")

plres@gsLineThicknessF	= 5.0
plres@gsLineColor	= "black"
plres@gsLineDashPattern	= 0
plres@tfPolyDrawOrder	= "PreDraw"
dum1(ind(XYHrs2.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/res@trYMinF,res@trYMaxF/),plres)

plres@gsLineThicknessF	= 1.5
plres@gsLineDashPattern	= 11
plres@gsLineColor	= "grey70"
do fff = 0,dimsizes(XYHrs2)-1
	if(XYHrs2(fff).eq.0)then continue end if
	dum1(fff) = gsn_add_polyline(wks,plot,(/tofloat(XYHrs2(fff)),tofloat(XYHrs2(fff))/),(/res@trYMinF,res@trYMaxF/),plres)
end do
do fff = 0,dimsizes(IntMjrGrd)-1
	dum2(fff) = gsn_add_polyline(wks,plot,(/res@trXMinF,res@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres)
end do


; Add model tracks
do ttt = 0,nTrend-1

	tmp	:= toint(where(ismissing(aPRS2(ttt,1,:)), fval2, aPRS2(ttt,1,:)))
	vals	:= ind(tmp%6.eq.0)

	; Check if the HWRF-B track is available
	if(all(ismissing(aPRS2(ttt,0,:))) .or. dimsizes(vals).eq.1)then
		print("WARNING: "+MCODE+" pressure is unavailable for "+IDATES3(ttt)+".")
		continue
	end if
	
	; Update resources
	res@xyMarkLineMode	= "Lines"
	res@xyLineThicknessF	= 12.0
	;res@xyMarkers		:= TRDmarkers(ttt)
	;res@xyMarkerColor	:= TRDcolors(ttt,:)
	;res@xyMarkerThicknessF	= 3.0
	res@xyLineColor		:= TRDcolors(ttt,:)
	res@tiXAxisString	= ""
	res@tiYAxisString	= ""

	
	; Overlay the new intensity line
	overlay(plot,gsn_csm_xy(wks,aPRS2(ttt,1,vals),aPRS2(ttt,0,vals),res))
	
	
	; Add legend info for each model
	plres@gsLineThicknessF	= 3.5
	plres@gsLineColor	:= TRDcolors(ttt,:)
	plres@gsLineDashPattern	= 0
	plres@tfPolyDrawOrder	= "PostDraw"
	
	;mkres@gsMarkerSizeF	= 8.0
	;mkres@gsMarkerColor	:= TRDcolors(ttt,:)
	;mkres@gsMarkerIndex	:= TRDmarkers(ttt)
	;mkres@gsMarkerThicknessF= 2.0
	;gsn_polymarker_ndc(wks,xleg(ttt),yleg(ttt),mkres)
	gsn_polyline_ndc(wks,(/xleg(ttt)-0.02,xleg(ttt)+0.02/),(/yleg(ttt),yleg(ttt)/),plres)
	gsn_text_ndc(wks,IDATES3(ttt),xtxt(ttt),ytxt(ttt),txres)
end do


; Add a sub-title
txres@txFontHeightF	= 0.018
txres@txJust		= "CenterCenter"
txres@txFontColor	= "black"
gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,0.59,0.855,txres)


; Add disclaimer
xloc = xyres3a@trXMinF + 0.02*(xyres3a@trXMaxF-xyres3a@trXMinF)
yloc = xyres3a@trYMinF + 0.02*(xyres3a@trYMaxF-xyres3a@trYMinF)
add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")


; Finish plot
draw(plot)
frame(wks)


; Remove white space on plot
if(DO_RMWHITE)then   system("convert -trim "+oFile+".png "+oFile+".png")   end if

end if ;DO_THIS

end if ; doPlot6





;================================================================
; Produce up-to-date track/intensity verification for current TC
;================================================================

if(doVer)then
;if(fnlATCF)then

print("")
print("MSG: *************STEP FOUR************")
print("MSG: Produce verification and associated graphics using MET-TC.")
print("MSG: Using NHC verification rules, which state that a storm")
print("MSG: must be a TC at the initial time and the verification")
print("MSG: time (HU,TS,TD,SS,SD).")
system("echo 'working' > "+ODIR+"status."+LONGSID+".log")


; 1) Set up variables
adeckFile	= ODIR+"atcf/"+ADECK_N
bdeckFile	= ODIR+"atcf/"+BDECK
TCPconf		= ODIR+"mettc/TCPairsConfig."+SID+"."+IDATE
TCSconf_f	= ODIR+"mettc/TCStatConfig_filter."+SID+"."+IDATE
TCSconf_s	= ODIR+"mettc/TCStatConfig_summary."+SID+"."+IDATE
TCPout1		= ODIR+"mettc/tcpairs."+SID+"."+IDATE+".earlyTRK"
TCPout2		= ODIR+"mettc/tcpairs."+SID+"."+IDATE+".earlyINT"
TCPout3		= ODIR+"mettc/tcpairs."+SID+"."+IDATE+".lateTRK"
TCPout4		= ODIR+"mettc/tcpairs."+SID+"."+IDATE+".lateINT"
TCSfout1	= ODIR+"mettc/tcstat.filter."+SID+"."+IDATE+".earlyTRK"
TCSfout2	= ODIR+"mettc/tcstat.filter."+SID+"."+IDATE+".earlyINT"
TCSfout3	= ODIR+"mettc/tcstat.filter."+SID+"."+IDATE+".lateTRK"
TCSfout4	= ODIR+"mettc/tcstat.filter."+SID+"."+IDATE+".lateINT"
TCSsout1	= ODIR+"mettc/tcstat.summary."+SID+"."+IDATE+".earlyTRK"
TCSsout2	= ODIR+"mettc/tcstat.summary."+SID+"."+IDATE+".earlyINT"
TCSsout3	= ODIR+"mettc/tcstat.summary."+SID+"."+IDATE+".lateTRK"
TCSsout4	= ODIR+"mettc/tcstat.summary."+SID+"."+IDATE+".lateINT"


; 2) set up files and directories
system("mkdir -p "+ODIR+"mettc/")
system("cp /home/Ghassan.Alaka/mettc/config/TCPairsConfig_match "+TCPconf)
if(.not.fileexists(TCPconf))then
	print("ERROR: TC_PAIRS config file not found.")
	exit
end if
system("cp /home/Ghassan.Alaka/mettc/config/TCStatConfig_filter "+TCSconf_f)
if(.not.fileexists(TCSconf_f))then
	print("ERROR: TC_STAT filter config file not found.")
	exit
end if
system("cp /home/Ghassan.Alaka/mettc/config/TCStatConfig_summary "+TCSconf_s)
if(.not.fileexists(TCSconf_s))then
	print("ERROR: TC_STAT summary config file not found.")
	exit
end if



; MET-TC expression for EARLY TRACK models
etM_exp		= "["
nnn = 0
do mmm = 0,dimsizes(etModels)-1
	if(etModels(mmm).eq."")then continue end if
	if(nnn.gt.0)then etM_exp = etM_exp+"," end if
	etM_exp	= etM_exp+"\"+str_get_dq()+etModels(mmm)+"\"+str_get_dq()
	nnn = nnn+1
end do
etM_exp		= etM_exp+"];"

; MET-TC expression for EARLY INTENSITY models
eiM_exp		= "["
nnn = 0
do mmm = 0,dimsizes(eiModels)-1
	if(eiModels(mmm).eq."")then continue end if
	if(nnn.gt.0)then eiM_exp = eiM_exp+"," end if
	eiM_exp	= eiM_exp+"\"+str_get_dq()+eiModels(mmm)+"\"+str_get_dq()
	nnn = nnn+1
end do
eiM_exp		= eiM_exp+"];"

; MET_TC expression for LATE TRACK models
ltM_exp		= "["
nnn = 0
do mmm = 0,dimsizes(ltModels)-1
	if(ltModels(mmm).eq."")then continue end if
	if(nnn.gt.0)then ltM_exp = ltM_exp+"," end if
	ltM_exp	= ltM_exp+str_get_dq()+ltModels(mmm)+str_get_dq()
	nnn = nnn+1
end do
ltM_exp		= ltM_exp+"];"

; MET_TC expression for LATE INTENSITY models
liM_exp		= "["
nnn = 0
do mmm = 0,dimsizes(liModels)-1
	if(liModels(mmm).eq."")then continue end if
	if(nnn.gt.0)then liM_exp = liM_exp+"," end if
	liM_exp	= liM_exp+str_get_dq()+liModels(mmm)+str_get_dq()
	nnn = nnn+1
end do
liM_exp		= liM_exp+"];"

initEnd		= "\"+str_get_dq()+changeTimeFmt(IDATE,"YYYYMMDDHH","YYYYMMDD_HH",0.)+"\"+str_get_dq()+";"
summJob		= "jobs = [\"+str_get_dq()+"-job summary -column TRACK -column WIND -column TI -column AMAX_WIND-BMAX_WIND -by AMODEL\"+str_get_dq()+"];"
NHCStrName	= "[\"+str_get_dq()+"LEVEL\"+str_get_dq()+",\"+str_get_dq()+"LEVEL\"+str_get_dq()+"," \
		  +"\"+str_get_dq()+"LEVEL\"+str_get_dq()+",\"+str_get_dq()+"LEVEL\"+str_get_dq()+"," \
		  +"\"+str_get_dq()+"LEVEL\"+str_get_dq()+"];"
NHCStrVal	= "[\"+str_get_dq()+"HU\"+str_get_dq()+",\"+str_get_dq()+"TS\"+str_get_dq()+"," \
		  +"\"+str_get_dq()+"TD\"+str_get_dq()+",\"+str_get_dq()+"SS\"+str_get_dq()+"," \
		  +"\"+str_get_dq()+"SD\"+str_get_dq()+"];"


; 4) Set basic plotting resources
res			:= True
res@gsnFrame		= False
res@gsnDraw		= False
res@gsnMaximize		= False
res@xyDashPattern	= 0
res@xyMarkLineMode	= "MarkLines"			; Markers *and* lines
res@xyMarkerSizeF	= 0.015
res@xyMarkerThicknessF	= 2.0
res@xyLineThicknessF	= 7.5
res@pmLegendDisplayMode= "Always"
res@pmLegendWidthF	= 0.15				; Change width and
;res@pmLegendHeightF	= 0.15				; height of legend.
res@pmLegendSide	= "Right"
;res@pmLegendParallelPosF   =  0.90			; move units right
;res@pmLegendOrthogonalPosF = -0.45			; move units down
res@lgPerimOn		= True				; turn off box around
res@lgLabelFontHeightF	= .012				; label font height
res@lgBoxBackground	= "white"
res@lgPerimFillColor	= "white"
res@lgPerimFill		= 0
res@tmXBMode		= "Explicit"
res@tmXBValues		:= tofloat(verTimes)
res@tmXBLabels		= ""+verTimes
res@tmXBLabelFontHeightF = 0.02
res@tmXBLabelDeltaF	= -0.25
res@tmYLLabelDeltaF	= 0.05
;res@tmXBLabelAngleF	= 90.
res@tmXMajorGrid                = True          ; implement x grid 
res@tmXMajorGridThicknessF      = 1.0           ; 2.0 is default
res@tmXMajorGridLineDashPattern = 2             ; select short dash lines
res@tmYMajorGrid                = True          ; implement y grid
res@tmYMajorGridThicknessF      = 1.0           ; 2.0 is default
res@tmYMajorGridLineDashPattern = 2             ; select short dash lines
res@tmXTOn		= False			; Turn off top axis tick marks

txres			:= True				; text mods desired
txres@txFontHeightF	= 0.015				; font smaller. default big
txres@txFontColor	= "brown"
txres@txPerimOn		= True
txres@txPerimColor	= "white"
txres@txBackgroundFillColor = "white"

txres2			:= True
txres2@txJust		= "CenterLeft"
txres2@txFont		= 25
txres2@txFontColor	= "black"
txres2@txFontHeightF	= 0.009



if(doLTVer)then
; 5) Verify LATE TRACK models
;	a) Run TC_PAIRS
print("MSG: Running TC_PAIRS for late track models.")
system("perl -pi -e 's/model = .*/model = "+ltM_exp+"/g' "+TCPconf)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh --login"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
;system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:/lfs2/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+O+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
;system("echo "+str_get_dq()+"echo \$LD_LIBRARY_PATH"+str_get_dq()+" >> "+tmpDir+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"which tc_pairs"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"echo \${PATH}"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+" -out "+TCPout3+" -log "+TCPout3+".log"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_pairs_wrapper.sh")
system(ODIR+"/mettc/tc_pairs_wrapper.sh >> "+ODIR+"/mettc/tc_pairs_wrapper.log")

;	b) Run TC_STAT in filter mode
print("MSG: Running TC_STAT in filter mode for late track models.")
filtJob	= "jobs = [\"+str_get_dq()+"-job filter -dump_row "+str_sub_str(TCSfout3,"/","\/")+".tcst\"+str_get_dq()+"];"
system("perl -pi -e 's/amodel = .*/amodel = "+ltM_exp+"/g' "+TCSconf_f)
system("perl -pi -e 's/jobs = .*/"+filtJob+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

; Create shell script to run TC_STAT with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh --login"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout3+".tcst -config "+TCSconf_f+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_statf_wrapper.sh")
system(ODIR+"/mettc/tc_statf_wrapper.sh")


;	c) Run TC_STAT in summary mode
print("MSG: Running TC_STAT in summary mode for late track models.")
system("perl -pi -e 's/amodel = .*/amodel = "+ltM_exp+"/g' "+TCSconf_s)
system("perl -pi -e 's/jobs = .*/"+summJob+"/g' "+TCSconf_s)
TK_ERR		:= new((/dimsizes(ltModels),dimsizes(verTimes)/),"float",fval3)
ALTK_ERR	:= TK_ERR
CRTK_ERR	:= TK_ERR
N_ERR		:= TK_ERR

do fff = 0,dimsizes(verTimes)-1
	;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
	tmp	:= TCSsout3+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".tcst"
	tmp2	:= TCSsout3+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".log"
	system("perl -pi -e 's/lead       = .*/lead       = [\"+str_get_dq()+verTimes(fff)+"0000\"+str_get_dq()+"];/g' "+TCSconf_s)
	
	; Create shell script to run TC_STAT with shell options (better)
	system("echo "+str_get_dq()+"#!/bin/sh --login"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout3+".tcst -config "+TCSconf_s+" -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("chmod +x "+ODIR+"/mettc/tc_stats_wrapper.sh")
	system(ODIR+"/mettc/tc_stats_wrapper.sh")
	
	data	:= asciiread(tmp,-1,"string")
	errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
	vals	:= ind(isStrSubset2(data,"SUMMARY:"))
	if(ismissing(vals(0)))then continue end if
	data	:= data(vals)
	errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
	errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
	do mmm = 0,dimsizes(ltModels)-1
		vals	:= ind(errModels.eq.ltModels(mmm))
		if(ismissing(vals(0)))then continue end if
		if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR"))))then
			continue
		else
			TK_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."MEAN")+1," ")))
			N_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."TOTAL")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."ALTK_ERR"))))then
			continue
		else
			ALTK_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."ALTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."CRTK_ERR"))))then
			continue
		else
			CRTK_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."CRTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
		end if
		
	end do
	
end do


; 	d) Plot!
print("MSG: Producing graphics for late track model verifications.")

;		i) TK_ERR
if(.not.all(ismissing(TK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(TK_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.TK_ERR."+LONGSID+"."+IDATE+".late"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","TK_ERR")
wks = gsn_open_wks("png",oFile)
res@xyLineColors	:= LTcolors
res@xyMarkers		:= LTmarkers
res@xyMarkerColors	:= LTcolors
res@tiMainString	= "LATE TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
res@gsnRightString	= ""+str_upper(LONGSID)
res@gsnLeftString	= "Valid through "+IDATE
res@xyExplicitLegendLabels := " "+ltModels
res@pmLegendHeightF	= 0.03*tofloat(dimsizes(ltModels))
;res@vpWidthF		= 0.6                      ; set width and height
res@vpHeightF		= 0.4
if(min(TK_ERR).lt.0.)then
	res@trYMinF	= floor(min(TK_ERR) - 0.1*(max(TK_ERR)-min(TK_ERR)))
	res@trYMaxF	= ceil(max(TK_ERR) + 0.1*(max(TK_ERR)-min(TK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(TK_ERR))
end if
plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))),TK_ERR(ind(ltModels.ne.""),ind(.not.ismissing(TK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		ii) ALTK_ERR
if(.not.all(ismissing(ALTK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(ALTK_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.ALTK_ERR."+LONGSID+"."+IDATE+".late"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","ALTK_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "LATE ALONG TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+str_upper(longSID)
;res@gsnLeftString	= "ALTK_ERR"
if(min(ALTK_ERR).lt.0.)then
	res@trYMinF	= floor(min(ALTK_ERR) - 0.1*(max(ALTK_ERR)-min(ALTK_ERR)))
	res@trYMaxF	= ceil(max(ALTK_ERR) + 0.1*(max(ALTK_ERR)-min(ALTK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(ALTK_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))),ALTK_ERR(ind(ltModels.ne.""),ind(.not.ismissing(ALTK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)


draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		iii) CRTK_ERR
if(.not.all(ismissing(CRTK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(CRTK_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.CRTK_ERR."+LONGSID+"."+IDATE+".late"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","CRTK_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "LATE CROSS TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+longSID
;res@gsnLeftString	= "CRTK_ERR"
if(min(CRTK_ERR).lt.0.)then
	res@trYMinF	= floor(min(CRTK_ERR) - 0.1*(max(CRTK_ERR)-min(CRTK_ERR)))
	res@trYMaxF	= ceil(max(CRTK_ERR) + 0.1*(max(CRTK_ERR)-min(CRTK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(CRTK_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))),CRTK_ERR(ind(ltModels.ne.""),ind(.not.ismissing(CRTK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)


draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if

end if



if(doLIVer)then
; 6) Verify LATE INTENSITY models
;	a) Run TC_PAIRS
print("MSG: Running TC_PAIRS for late intensity models.")
system("perl -pi -e 's/model = .*/model = "+liM_exp+"/g' "+TCPconf)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+" -out "+TCPout4+" -log "+TCPout4+".log"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_pairs_wrapper.sh")
system(ODIR+"/mettc/tc_pairs_wrapper.sh")


;	b) Run TC_STAT in filter mode
print("MSG: Running TC_STAT in filter mode for late intensity models.")
filtJob	= "jobs = [\"+str_get_dq()+"-job filter -dump_row "+str_sub_str(TCSfout4,"/","\/")+".tcst\"+str_get_dq()+"];"
system("perl -pi -e 's/amodel = .*/amodel = "+liM_exp+"/g' "+TCSconf_f)
system("perl -pi -e 's/jobs = .*/"+filtJob+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout4+".tcst -config "+TCSconf_f+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_statf_wrapper.sh")
system(ODIR+"/mettc/tc_statf_wrapper.sh")


;	c) Run TC_STAT in summary mode
print("MSG: Running TC_STAT in summary mode for late intensity models.")
system("perl -pi -e 's/amodel = .*/amodel = "+liM_exp+"/g' "+TCSconf_s)
system("perl -pi -e 's/jobs = .*/"+summJob+"/g' "+TCSconf_s)
WIND_ERR	:= new((/dimsizes(liModels),dimsizes(verTimes)/),"float",fval3)
ABS_WIND_ERR	:= WIND_ERR
N_ERR		:= WIND_ERR

do fff = 0,dimsizes(verTimes)-1
	;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
	tmp	:= TCSsout4+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".tcst"
	tmp2	:= TCSsout4+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".log"
	system("perl -pi -e 's/lead       = .*/lead       = [\"+str_get_dq()+verTimes(fff)+"0000\"+str_get_dq()+"];/g' "+TCSconf_s)
	
	system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout4+".tcst -config "+TCSconf_s+" -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("chmod +x "+ODIR+"/mettc/tc_stats_wrapper.sh")
	system(ODIR+"/mettc/tc_stats_wrapper.sh")
	
	data	:= asciiread(tmp,-1,"string")
	errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
	vals	:= ind(isStrSubset2(data,"SUMMARY:"))
	if(ismissing(vals(0)))then continue end if
	data	:= data(vals)
	errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
	errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
	do mmm = 0,dimsizes(liModels)-1
		vals	:= ind(errModels.eq.liModels(mmm))
		if(ismissing(vals(0)))then continue end if
		if(ismissing(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)"))))then
			continue
		else
			ABS_WIND_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)")),ind(errCols.eq."MEAN")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND"))))then
			continue
		else
			WIND_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."MEAN")+1," ")))
			N_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."TOTAL")+1," ")))
		end if
	end do
	
end do


; 	d) Plot!
print("MSG: Producing graphics for late intensity model verifications.")
;		i) ABS_WIND_ERR
if(.not.all(ismissing(ABS_WIND_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(ABS_WIND_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.ABS_WIND_ERR."+LONGSID+"."+IDATE+".late"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","ABS_WIND_ERR")
wks = gsn_open_wks("png",oFile)
res@xyLineColors	:= LIcolors
res@xyMarkers		:= LImarkers
res@xyMarkerColors	:= LIcolors
res@tiMainString	= "LATE INTENSITY ERROR (absolute)"
res@tiYAxisString	= "Intensity Error"+" [kt]"
res@tiXAxisString	= "Lead Time [h]"
res@gsnRightString	= ""+str_upper(LONGSID)
res@gsnLeftString	= "Valid through "+IDATE
res@xyExplicitLegendLabels := " "+liModels(ind(liModels.ne.""))
res@pmLegendHeightF	= 0.03*tofloat(dimsizes(liModels(ind(liModels.ne.""))))
;res@vpWidthF		= 0.6                      ; set width and height
;res@vpHeightF		= 0.2
if(min(ABS_WIND_ERR).lt.0.)then
	res@trYMinF	= floor(min(ABS_WIND_ERR) - 0.1*(max(ABS_WIND_ERR)-min(ABS_WIND_ERR)))
	res@trYMaxF	= ceil(max(ABS_WIND_ERR) + 0.1*(max(ABS_WIND_ERR)-min(ABS_WIND_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(ABS_WIND_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))),ABS_WIND_ERR(ind(liModels.ne.""),ind(.not.ismissing(ABS_WIND_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)


draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		ii) WIND_ERR
if(.not.all(ismissing(WIND_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(WIND_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.WIND_ERR."+LONGSID+"."+IDATE+".late"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","WIND_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "LATE INTENSITY ERROR (bias)"
res@tiYAxisString	= "Intensity Error"+" [kt]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+longSID
;res@gsnLeftString	= "WIND_ERR"
if(min(WIND_ERR).lt.0.)then
	res@trYMinF	= floor(min(WIND_ERR) - 0.1*(max(WIND_ERR)-min(WIND_ERR)))
	res@trYMaxF	= ceil(max(WIND_ERR) + 0.1*(max(WIND_ERR)-min(WIND_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(WIND_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))),WIND_ERR(ind(liModels.ne.""),ind(.not.ismissing(WIND_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if

end if



if(doETVer)then
; 3) Verify EARLY TRACK models
;	a) Run TC_PAIRS
print("MSG: Running TC_PAIRS for early track models.")
system("perl -pi -e 's/model = .*/model = "+etM_exp+"/g' "+TCPconf)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+" -out "+TCPout1+" -log "+TCPout1+".log"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_pairs_wrapper.sh")
system(ODIR+"/mettc/tc_pairs_wrapper.sh")


;	b) Run TC_STAT in filter mode
print("MSG: Running TC_STAT in filter mode for early track models.")
filtJob	= "jobs = [\"+str_get_dq()+"-job filter -dump_row "+str_sub_str(TCSfout1,"/","\/")+".tcst\"+str_get_dq()+"];"
system("perl -pi -e 's/amodel = .*/amodel = "+etM_exp+"/g' "+TCSconf_f)
system("perl -pi -e 's/jobs = .*/"+filtJob+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout1+".tcst -config "+TCSconf_f+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_statf_wrapper.sh")
system(ODIR+"/mettc/tc_statf_wrapper.sh")


;	c) Run TC_STAT in summary mode
print("MSG: Running TC_STAT in summary mode for early track models.")
system("perl -pi -e 's/amodel = .*/amodel = "+etM_exp+"/g' "+TCSconf_s)
system("perl -pi -e 's/jobs = .*/"+summJob+"/g' "+TCSconf_s)
TK_ERR		:= new((/dimsizes(etModels),dimsizes(verTimes)/),"float",fval3)
ALTK_ERR	:= TK_ERR
CRTK_ERR	:= TK_ERR
N_ERR		:= TK_ERR

do fff = 0,dimsizes(verTimes)-1
	;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
	tmp	:= TCSsout1+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".tcst"
	tmp2	:= TCSsout1+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".log"
	system("perl -pi -e 's/lead       = .*/lead       = [\"+str_get_dq()+verTimes(fff)+"0000\"+str_get_dq()+"];/g' "+TCSconf_s)
	
	system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout1+".tcst -config "+TCSconf_s+" -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("chmod +x "+ODIR+"/mettc/tc_stats_wrapper.sh")
	system(ODIR+"/mettc/tc_stats_wrapper.sh")
	
	data	:= asciiread(tmp,-1,"string")
	errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
	vals	:= ind(isStrSubset2(data,"SUMMARY:"))
	if(ismissing(vals(0)))then continue end if
	data	:= data(vals)
	errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
	errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
	do mmm = 0,dimsizes(etModels)-1
		vals	:= ind(errModels.eq.etModels(mmm))
		if(ismissing(vals(0)))then continue end if
		if(ismissing(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."TK_ERR"))))then
			continue
		else
			TK_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."MEAN")+1," ")))
			N_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."TOTAL")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."ALTK_ERR"))))then
			continue
		else
			ALTK_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."ALTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."CRTK_ERR"))))then
			continue
		else
			CRTK_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.etModels(mmm) .and. errFields.eq."CRTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
		end if
	end do
	
end do


; 	d) Plot!
print("MSG: Producing graphics for early track model verifications.")
;		i) TK_ERR
if(.not.all(ismissing(TK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(TK_ERR(0,:))).gt.1))then

oFile	:= ODIR+"Verification.TK_ERR."+LONGSID+"."+IDATE+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","TK_ERR")
wks = gsn_open_wks("png",oFile)
res@xyLineColors	:= ETcolors
res@xyMarkers		:= ETmarkers
res@xyMarkerColors	:= ETcolors
res@tiMainString	= "EARLY TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
res@gsnRightString	= ""+str_upper(LONGSID)
res@gsnLeftString	= "Valid through "+IDATE
res@xyExplicitLegendLabels := " "+etModels
res@pmLegendHeightF	= 0.03*tofloat(dimsizes(etModels(ind(etModels.ne.""))))
;res@vpWidthF		= 1.0                      ; set width and height
res@vpHeightF		= 0.4
if(min(TK_ERR).lt.0.)then
	res@trYMinF	= floor(min(TK_ERR) - 0.1*(max(TK_ERR)-min(TK_ERR)))
	res@trYMaxF	= ceil(max(TK_ERR) + 0.1*(max(TK_ERR)-min(TK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(TK_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))),TK_ERR(ind(etModels.ne.""),ind(.not.ismissing(TK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(TK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		ii) ALTK_ERR
if(.not.all(ismissing(ALTK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(ALTK_ERR(0,:))).gt.1))then

oFile	:= ODIR+"Verification.ALTK_ERR."+LONGSID+"."+IDATE+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","ALTK_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "EARLY ALONG TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+longSID
;res@gsnLeftString	= "ALTK_ERR"
if(min(ALTK_ERR).lt.0.)then
	res@trYMinF	= floor(min(ALTK_ERR) - 0.1*(max(ALTK_ERR)-min(ALTK_ERR)))
	res@trYMaxF	= ceil(max(ALTK_ERR) + 0.1*(max(ALTK_ERR)-min(ALTK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(ALTK_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))),ALTK_ERR(ind(etModels.ne.""),ind(.not.ismissing(ALTK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(ALTK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		iii) CRTK_ERR
if(.not.all(ismissing(CRTK_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(CRTK_ERR(0,:))).gt.1))then

oFile	:= ODIR+"Verification.CRTK_ERR."+LONGSID+"."+IDATE+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","CRTK_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "EARLY CROSS TRACK ERROR"
res@tiYAxisString	= "Track Error"+" [n mi]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+longSID
;res@gsnLeftString	= "CRTK_ERR"
if(min(CRTK_ERR).lt.0.)then
	res@trYMinF	= floor(min(CRTK_ERR) - 0.1*(max(CRTK_ERR)-min(CRTK_ERR)))
	res@trYMaxF	= ceil(max(CRTK_ERR) + 0.1*(max(CRTK_ERR)-min(CRTK_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(CRTK_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))),CRTK_ERR(ind(etModels.ne.""),ind(.not.ismissing(CRTK_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(CRTK_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if

end if



if(doEIVer)then
; 4) Verify EARLY INTENSITY models
;	a) Run TC_PAIRS
print("MSG: Running TC_PAIRS for early intensity models.")
system("perl -pi -e 's/model = .*/model = "+eiM_exp+"/g' "+TCPconf)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+" -out "+TCPout2+" -log "+TCPout2+".log"+str_get_dq()+" >> "+ODIR+"mettc/tc_pairs_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_pairs_wrapper.sh")
system(ODIR+"/mettc/tc_pairs_wrapper.sh")


;	b) Run TC_STAT in filter mode
print("MSG: Running TC_STAT in filter mode for early intensity models.")
filtJob	= "jobs = [\"+str_get_dq()+"-job filter -dump_row "+str_sub_str(TCSfout2,"/","\/")+".tcst\"+str_get_dq()+"];"
system("perl -pi -e 's/amodel = .*/amodel = "+eiM_exp+"/g' "+TCSconf_f)
system("perl -pi -e 's/jobs = .*/"+filtJob+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
system("perl -pi -e 's/init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

; Create shell script to run TC_PAIRS with shell options (better)
system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout2+".tcst -config "+TCSconf_f+str_get_dq()+" >> "+ODIR+"mettc/tc_statf_wrapper.sh")
system("chmod +x "+ODIR+"/mettc/tc_statf_wrapper.sh")
system(ODIR+"/mettc/tc_statf_wrapper.sh")


;	c) Run TC_STAT in summary mode
print("MSG: Running TC_STAT in summary mode for early intensity models.")
system("perl -pi -e 's/amodel = .*/amodel = "+eiM_exp+"/g' "+TCSconf_s)
system("perl -pi -e 's/jobs = .*/"+summJob+"/g' "+TCSconf_s)
WIND_ERR	:= new((/dimsizes(eiModels),dimsizes(verTimes)/),"float",fval3)
ABS_WIND_ERR	:= WIND_ERR
N_ERR		:= WIND_ERR

do fff = 0,dimsizes(verTimes)-1
	;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
	tmp	:= TCSsout2+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".tcst"
	tmp2	:= TCSsout2+".f"+sprinti("%0.3i",toint(verTimes(fff)))+".log"
	system("perl -pi -e 's/lead       = .*/lead       = [\"+str_get_dq()+verTimes(fff)+"0000\"+str_get_dq()+"];/g' "+TCSconf_s)
	
	system("echo "+str_get_dq()+"#!/bin/sh"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/met-6.0/bin:\${PATH}"+str_get_dq()+" > "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"LD_LIBRARY_PATH=/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib:\$LD_LIBRARY_PATH"+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout2+".tcst -config "+TCSconf_s+" -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+ODIR+"mettc/tc_stats_wrapper.sh")
	system("chmod +x "+ODIR+"/mettc/tc_stats_wrapper.sh")
	system(ODIR+"/mettc/tc_stats_wrapper.sh")
	
	data	:= asciiread(tmp,-1,"string")
	errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
	vals	:= ind(isStrSubset2(data,"SUMMARY:"))
	if(ismissing(vals(0)))then continue end if
	data	:= data(vals)
	errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
	errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
	do mmm = 0,dimsizes(eiModels)-1
		vals	:= ind(errModels.eq.eiModels(mmm))
		if(ismissing(vals(0)))then   continue   end if
		if(ismissing(data(ind(errModels.eq.eiModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)"))))then
			continue
		else
			ABS_WIND_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.eiModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)")),ind(errCols.eq."MEAN")+1," ")))
		end if
		if(ismissing(data(ind(errModels.eq.eiModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND"))))then
			continue
		else
			WIND_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.eiModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."MEAN")+1," ")))
			N_ERR(mmm,fff)		= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.eiModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."TOTAL")+1," ")))
		end if
	end do
	
end do


; 	d) Plot!
print("MSG: Producing graphics for early intensity model verifications.")
;		i) ABS_WIND_ERR
if(.not.all(ismissing(ABS_WIND_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(ABS_WIND_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.ABS_WIND_ERR."+LONGSID+"."+IDATE+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","ABS_WIND_ERR")
wks = gsn_open_wks("png",oFile)
res@xyLineColors	:= EIcolors
res@xyMarkers		:= EImarkers
res@xyMarkerColors	:= EIcolors
res@tiMainString	= "EARLY INTENSITY ERROR (absolute)"
res@tiYAxisString	= "Intensity Error"+" [kt]"
res@tiXAxisString	= "Lead Time [h]"
res@gsnRightString	= ""+str_upper(LONGSID)
res@gsnLeftString	= "Valid through "+IDATE
res@xyExplicitLegendLabels := " "+eiModels
res@pmLegendHeightF	= 0.03*tofloat(dimsizes(eiModels(ind(eiModels.ne.""))))
;res@vpWidthF		= 0.6                      ; set width and height
;res@vpHeightF		= 0.2
if(min(ABS_WIND_ERR).lt.0.)then
	res@trYMinF	= floor(min(ABS_WIND_ERR) - 0.1*(max(ABS_WIND_ERR)-min(ABS_WIND_ERR)))
	res@trYMaxF	= ceil(max(ABS_WIND_ERR) + 0.1*(max(ABS_WIND_ERR)-min(ABS_WIND_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(ABS_WIND_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))),ABS_WIND_ERR(ind(eiModels.ne.""),ind(.not.ismissing(ABS_WIND_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(ABS_WIND_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if


;		ii) WIND_ERR
if(.not.all(ismissing(WIND_ERR(0,:))) .and.\
   dimsizes(ind(.not.ismissing(WIND_ERR(0,:)))).gt.1)then

oFile	:= ODIR+"Verification.WIND_ERR."+LONGSID+"."+IDATE+".early"
if(fileexists(oFile))then   system("rm -f "+oFile)   end if
;wks = gsn_open_wks("x11","WIND_ERR")
wks = gsn_open_wks("png",oFile)
res@tiMainString	= "EARLY INTENSITY ERROR (bias)"
res@tiYAxisString	= "Intensity Error"+" [kt]"
res@tiXAxisString	= "Lead Time [h]"
;res@gsnRightString	= ""+longSID
;res@gsnLeftString	= "WIND_ERR"
if(min(WIND_ERR).lt.0.)then
	res@trYMinF	= floor(min(WIND_ERR) - 0.1*(max(WIND_ERR)-min(WIND_ERR)))
	res@trYMaxF	= ceil(max(WIND_ERR) + 0.1*(max(WIND_ERR)-min(WIND_ERR)))
else
	res@trYMinF	= 0.
	res@trYMaxF	= ceil(1.1*max(WIND_ERR))
end if

plot	= gsn_csm_xy(wks,tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))),WIND_ERR(ind(eiModels.ne.""),ind(.not.ismissing(WIND_ERR(0,:)))),res) ; create plot

; Add the number of cases to the plot
dum1	:= new(dimsizes(verTimes),"graphic")
do fff = 0,dimsizes(verTimes)-1
	if(.not.ismissing(N_ERR(0,fff)))then
		dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(verTimes(fff)),\
				         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
	end if
end do

; Add disclaimer
TEXT14	= gsn_add_text(wks,plot,"*Experimental Product of NOAA/AOML/HRD*",\
				min(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:)))))) + 0.02*(max(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))))-min(tofloat(verTimes(ind(.not.ismissing(WIND_ERR(0,:))))))),\
				res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF),txres2)

draw(plot)
frame(wks)

if(DO_RMWHITE)then system("convert -trim "+oFile+".png "+oFile+".png") end if		; Remove white space on plot

end if

end if



;end if ;fnlATCF
end if ;doVer



;================================================================
; Perform final checks. If all looks good, write "complete" to
; the status log file.
;================================================================

if(doPlot1)then
	chk	:= systemfunc("ls "+ODIR+"TrackGuidance*png 2>/dev/null")
	if(ismissing(chk(0)))then
		print("ERROR: Track Guidance did not plot.")
		exit
	end if
else if(doPlot2)then
	chk	:= systemfunc("ls "+ODIR+"IntensityGuidance*png 2>/dev/null")
	if(ismissing(chk(0)))then
		print("ERROR: Intensity Guidance did not plot.")
		exit
	end if
else if(doPlot3)then
	chk	:= systemfunc("ls "+ODIR+"PressureGuidance*png 2>/dev/null")
	if(ismissing(chk(0)))then
		print("ERROR: Pressure Guidance did not plot.")
		exit
	end if
else if(doPlot4)then
	chk	:= systemfunc("ls "+ODIR+"TrackTrend*png 2>/dev/null")
	if(ismissing(chk(0)))then
		print("ERROR: "+MCODE+" Track Trend did not plot.")
		exit
	end if
else if(doPlot5)then
	chk	:= systemfunc("ls "+ODIR+"IntensityTrend*png 2>/dev/null")
	if(ismissing(chk(0)))then
		print("ERROR: "+MCODE+" Intensity Trend did not plot.")
		exit
	end if
else if(doVer)then
	if(doETVer)then
		chk	:= systemfunc("ls "+ODIR+"Verification*TK_ERR*early*png 2>/dev/null")
		if(ismissing(chk(0)))then
			print("ERROR: Early Track Model Verification did not plot.")
			exit
		end if
	else if(doEIVer)then
		chk	:= systemfunc("ls "+ODIR+"Verification*WIND_ERR*early*png 2>/dev/null")
		if(ismissing(chk(0)))then
			print("ERROR: Early Intensity Model Verification did not plot.")
			exit
		end if
	else if(doLTVer)then
		chk	:= systemfunc("ls "+ODIR+"Verification*TK_ERR*late*png 2>/dev/null")
		if(ismissing(chk(0)))then
			print("ERROR: Late Track Model Verification did not plot.")
			exit
		end if
	else if(doLIVer)then
		chk	:= systemfunc("ls "+ODIR+"Verification*WIND_ERR*late*png 2>/dev/null")
		if(ismissing(chk(0)))then
			print("ERROR: Late Intensity Model Verification did not plot.")
			exit
		end if
	end if  end if  end if  end if
end if  end if  end if  end if  end if  end if



;================================================================
; Write final message to status file.
;================================================================

if(FNL_ATCF)then	system("echo 'complete' > "+ODIR+"status."+LONGSID+".log")
else			system("echo 'incomplete' > "+ODIR+"status."+LONGSID+".log")
end if



;================================================================
; End of file.
;================================================================

print("")
print("File has finished running.")
print("Time Finished:  "+systemfunc("date"))

exit

end




