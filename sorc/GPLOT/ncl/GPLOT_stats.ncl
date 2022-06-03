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
; Last Modified: September 1, 2021
;
; Last Modified by Lewis J. Gramer: Aug 16, 2019
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
; 2019-08-16 --> (LJG) added DO_CONVERTGIF to call ImageMagick to 'convert' to GIF
; 2019-09-25 --> GJA added DO_MARKERS to turn markers on/off on all graphics.
; 2021-09-01 --> Major upgrade to produce reliable guidance all over the world,
;		 taking into account longitude discontinuities, etc. (GJA)
;
;================================================================
;
;================================================================
; Load scripts
;================================================================

load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_func.ncl"
load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_util.ncl"
load "$GPLOT_DIR/sorc/GPLOT/ncl/GPLOT_main.ncl"



;================================================================
; Begin the code.
;================================================================

begin

print("MSG: GPLOT_stats.ncl started at "+systemfunc("date"))
print("")
print("MSG: Welcome to GPLOT, Stats Module.")
print("MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.")
print("MSG: The Stats Module produces graphical products for guidance, trends, &")
print("MSG: verification. It uses the EMC interpolation code to produce 'early' forecasts")
print("MSG: and the Model Evaluation Tools for Tropical Cyclones (MET-TC) to produce")
print("MSG: verification of forecast errors.")
print("MSG: Written by Ghassan Alaka [NOAA/OAR/AOML/HRD].")
print("MSG: All queries should be sent to Ghassan.Alaka@noaa.gov")
print("MSG: This module was last updated on 2019-06-15.")
print("")




;================================================================
; PART I. Set constants. DO NOT CHANGE.
;         Constants are defined in GPLOT_util.ncl
;================================================================

C	= load_constants()
g	= C@g			; Gravitational acceleration [m s-2]
pi	= C@pi			; Mathematical constant
d2r	= C@d2r			; Degrees-to-radians conversion [rad deg-1]
r	= C@r			; Earth's radius [m]
omega	= C@omega		; Earth's angular rotation rate [rad s-1]
ms2kts	= C@ms2kts		; Meters-per-second-to-knots conversion [m s-1 kt-1]
fval	= C@fval		; Missing value for float #1 [n/a]
fval2	= C@fval2		; Missing value for integer [n/a]
fval3	= C@fval3		; Missing value for float #2 [n/a]
tunits	= C@tunits		; Baseline date used for date conversions




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
if(.not.isvar("MASTER_NML_IN"))then             MASTER_NML = GPLOT_DIR+"/parm/namelist.master.default"
else
        if(fileexists(MASTER_NML_IN))then       MASTER_NML = MASTER_NML_IN
        else                                    MASTER_NML = GPLOT_DIR+"/parm/"+MASTER_NML_IN
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
if(.not.isvar("DO_PDF"))then            DO_PDF = NML@DO_PDF                     end if
if(.not.isvar("DO_TITLES"))then		DO_TITLES = NML@DO_TITLES		end if
if(.not.isvar("DO_DISCLAIMER"))then	DO_DISCLAIMER = NML@DO_DISCLAIMER	end if
if(.not.isvar("ATCF1_DIR"))then		ATCF1_DIR = NML@ATCF1_DIR		end if
if(.not.isvar("ATCF1_TAG"))then		ATCF1_TAG = NML@ATCF1_TAG		end if
if(.not.isvar("ATCF2_DIR"))then		ATCF2_DIR = NML@ATCF2_DIR		end if
if(.not.isvar("ATCF2_TAG"))then		ATCF2_TAG = NML@ATCF2_TAG		end if
if(.not.isvar("ADECK_DIR"))then		ADECK_DIR = NML@ADECK_DIR		end if
if(.not.isvar("BDECK_DIR"))then		BDECK_DIR = NML@BDECK_DIR		end if
if(.not.isvar("MCODE"))then		MCODE = NML@MCODE			end if
if(.not.isvar("MCODEI"))then		MCODEI = NML@MCODEI			end if
if(.not.isvar("MCODE12"))then		MCODE12 = NML@MCODE12			end if
if(.not.isvar("MORIG"))then		MORIG = NML@MORIG			end if
if(.not.isvar("TRKmodels"))then		TRKmodels = NML@TRKmodels		end if
if(.not.isvar("TRKINTmodels"))then	TRKINTmodels = NML@TRKINTmodels		end if
if(.not.isvar("INTmodels"))then		INTmodels = NML@INTmodels		end if
if(.not.isvar("PRSmodels"))then		PRSmodels = NML@PRSmodels		end if
if(.not.isvar("TRKmodelsI"))then	TRKmodelsI = NML@TRKmodelsI		end if
if(.not.isvar("INTmodelsI"))then	INTmodelsI = NML@INTmodelsI		end if
if(.not.isvar("TRKmodelsT"))then	TRKmodelsT = NML@TRKmodelsT		end if
if(.not.isvar("INTmodelsT"))then	INTmodelsT = NML@INTmodelsT		end if
if(.not.isvar("PRSmodelT"))then		PRSmodelsT = NML@PRSmodelsT		end if
if(.not.isvar("etModels"))then		etModels = NML@etModels			end if
if(.not.isvar("eiModels"))then		eiModels = NML@eiModels			end if
if(.not.isvar("ltModels"))then		ltModels = NML@ltModels			end if
if(.not.isvar("liModels"))then		liModels = NML@liModels			end if
if(.not.isvar("LEAD_TIMES"))then	LEAD_TIMES = NML@LEAD_TIMES		end if
if(.not.isvar("nTrend"))then		nTrend = NML@nTrend			end if
if(.not.isvar("DO_INTERP"))then		DO_INTERP = NML@DO_INTERP		end if
if(.not.isvar("DO_MARKERS"))then	DO_MARKERS = NML@DO_MARKERS		end if
if(.not.isvar("MAX_FHR"))then		MAX_FHR = NML@MAX_FHR			end if
if(.not.isvar("DO_FHRLABELS"))then	DO_FHRLABELS = NML@DO_FHRLABELS		end if
if(.not.isvar("FORCE"))then             FORCE = NML@FORCE
else                                    FORCE := where(FORCE.eq."True",True,False)
end if

if(ODIR_TYPE.eq.1)then      ODIR = str_sub_str(ODIR+"/guidance/","//","/")
else                        ODIR = str_sub_str(ODIR+"/"+EXPT+"/"+ENSID+"/"+IDATE+"/guidance/","//","/")
end if
ODIR_ADECK = ODIR+"/adeck/"
print("MSG: I will place graphics in this location --> "+ODIR)




;================================================================
; PART IV. Print some information to the terminal
;================================================================

print("===========================================")
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
if(DO_INTERP)then	print("MSG:   Interpolation:      ON")
else			print("MSG:   Interpolation:      OFF")
end if
print("MSG:   System Environment: "+MACHINE)
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
;print("MSG:   Hour Format:        "+FMT_HR)
;print("MSG:   Time Step [h]:      "+DT)
print("MSG:   Final ATCF Path:    "+ATCF2_DIR)
print("MSG:   Final ATCF Tag:     "+ATCF2_TAG)
if(ATCF1_DIR.ne.ATCF2_DIR .and. ATCF1_TAG.ne.ATCF2_TAG)then
	print("MSG:   Temp. ATCF Path:     "+ATCF1_DIR)
	print("MSG:   Temp. ATCF Tag:      "+ATCF1_TAG)
end if
print("MSG:   ADECK Directory:    "+ADECK_DIR)
print("MSG:   BDECK Directory:    "+BDECK_DIR)
if(DO_RMWHITE)then	print("MSG:   Trim graphics:      ON")
else			print("MSG:   Trim graphics:      OFF")
end if
if(DO_CONVERTGIF)then	print("MSG:   Convert to GIF:     ON")
else			print("MSG:   Convert to GIF:     OFF")
end if
if(DO_MARKERS)then	print("MSG:   Line Markers:       ON")
else			print("MSG:   Line Markers:       OFF")
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
print("===========================================")




;================================================================
; PART V. Set/Change some variables here.
;         These options may eventually be moved into functions
;         and/or namelist options.
;================================================================

; 1. Setup variables to achieve an optimal environment
MOD_FILE = GPLOT_DIR+"/modulefiles/modulefile.gplot."+str_lower(MACHINE)

; 2. Make the Output Directory (ODIR)
;    It should already exist, so this is a safety check.
system("mkdir -p "+ODIR)
system("mkdir -p "+ODIR_ADECK)



;================================================================
; USER INPUT SECTION.
;================================================================

; Graphics Namelist
; Comment this to set default namelist.
if(.not.isvar("VAR_NML"))then   
	if(fileexists(GPLOT_DIR+"/parm/namelist.stats."+EXPT))then
		VAR_NML = GPLOT_DIR+"/parm/namelist.stats."+EXPT
	else if(fileexists(GPLOT_DIR+"/parm/namelist.stats.default"))then
		VAR_NML = GPLOT_DIR+"/parm/namelist.stats.default"
	else
		print("ERROR: Default variable namelist not available.")
		exit
	end if  end if
end if
print("MSG: Using this variable namelist: "+VAR_NML)


NML_DATA	:= asciiread(VAR_NML,-1,"string")
headers		:= str_squeeze(str_split(NML_DATA(0)," "))



;================================================================
; MASTER PLOTTING CONTROLS
;================================================================

; Guidance
val := ind(isStrSubset2(NML_DATA,"GUIDANCE"))
if(.not.ismissing(val(0)))then
	DO_GUIDANCE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	
	if(DO_GUIDANCE)then
		
		; Late Track Guidance
		val := ind(isStrSubset2(NML_DATA,"LATE_TK_GUIDE"))
		if(ismissing(val(0)))then
			DO_LT_GUIDE = False
		else	DO_LT_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if

		; Late Track/Intensity Guidance
		val := ind(isStrSubset2(NML_DATA,"LATE_TKINT_GUIDE"))
		if(ismissing(val(0)))then
			DO_LTI_GUIDE = False
		else	DO_LTI_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if

		
		; Early Track Guidance
		val := ind(isStrSubset2(NML_DATA,"EARLY_TK_GUIDE"))
		if(ismissing(val(0)))then
			DO_ET_GUIDE = False
		else	DO_ET_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Intensity Guidance
		val := ind(isStrSubset2(NML_DATA,"LATE_INT_GUIDE"))
		if(ismissing(val(0)))then
			DO_LI_GUIDE = False
		else	DO_LI_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Early Intensity Guidance
		val := ind(isStrSubset2(NML_DATA,"EARLY_INT_GUIDE"))
		if(ismissing(val(0)))then
			DO_EI_GUIDE = False
		else	DO_EI_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Pressure Guidance
		val := ind(isStrSubset2(NML_DATA,"LATE_PRS_GUIDE"))
		if(ismissing(val(0)))then
			DO_LP_GUIDE = False
		else	DO_LP_GUIDE = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
	else
		; If no guidance, then set all of these to False.
		DO_LT_GUIDE = False
		DO_LTI_GUIDE = False
		DO_ET_GUIDE = False
		DO_LI_GUIDE = False
		DO_EI_GUIDE = False
		DO_LP_GUIDE = False
	end if
else	DO_GUIDANCE = False
	DO_LT_GUIDE = False
	DO_LTI_GUIDE = False
	DO_ET_GUIDE = False
	DO_LI_GUIDE = False
	DO_EI_GUIDE = False
	DO_LP_GUIDE = False
end if

; Trends
val := ind(isStrSubset2(NML_DATA,"TRENDS"))
if(.not.ismissing(val(0)))then
	DO_TRENDS = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	
	if(DO_TRENDS)then
		
		; Late Track Trends
		val := ind(isStrSubset2(NML_DATA,"LATE_TK_TREND"))
		if(ismissing(val(0)))then
			DO_LT_TREND = False
		else	DO_LT_TREND = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Intensity Trends
		val := ind(isStrSubset2(NML_DATA,"LATE_INT_TREND"))
		if(ismissing(val(0)))then
			DO_LI_TREND = False
		else	DO_LI_TREND = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Pressure Trends
		val := ind(isStrSubset2(NML_DATA,"LATE_PRS_TREND"))
		if(ismissing(val(0)))then
			DO_LP_TREND = False
		else	DO_LP_TREND = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
	else
		; If no trends, then set all of these to False.
		DO_LT_TREND = False
		DO_LI_TREND = False
		DO_LP_TREND = False
	end if
else	DO_TRENDS = False
	DO_LT_TREND = False
	DO_LI_TREND = False
	DO_LP_TREND = False
end if

; Verification
val := ind(isStrSubset2(NML_DATA,"VERIFICATION"))
if(.not.ismissing(val(0)))then
	DO_VERIFICATION	= where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
	
	if(DO_VERIFICATION)then
		
		; Early Track Verification
		val := ind(isStrSubset2(NML_DATA,"EARLY_TK_VER"))
		if(ismissing(val(0)))then
			DO_ET_VER = False
		else	DO_ET_VER = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Early Intensity Verification
		val := ind(isStrSubset2(NML_DATA,"EARLY_INT_VER"))
		if(ismissing(val(0)))then
			DO_EI_VER = False
		else	DO_EI_VER = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Track Verification
		val := ind(isStrSubset2(NML_DATA,"LATE_TK_VER"))
		if(ismissing(val(0)))then
			DO_LT_VER = False
		else	DO_LT_VER = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
		
		; Late Intensity Verification
		val := ind(isStrSubset2(NML_DATA,"LATE_INT_VER"))
		if(ismissing(val(0)))then
			DO_LI_VER = False
		else	DO_LI_VER = where(str_squeeze(str_get_field(NML_DATA(val(0)),ind(headers.eq."CHOICE")+1," ")).eq."True",True,False)
		end if
	else
		; If no verification, then set all of these to False
		DO_ET_VER = False
		DO_EI_VER = False
		DO_LT_VER = False
		DO_LI_VER = False
	end if
else	DO_VERIFICATION = False
	DO_ET_VER = False
	DO_EI_VER = False
	DO_LT_VER = False
	DO_LI_VER = False
end if


print("")
print("MSG: Will produce the following graphics:")
if(DO_GUIDANCE)then
	print("MSG: Guidance turned ON.")
	print("MSG: Early Track Guidance:     "+DO_ET_GUIDE)
	print("MSG: Late Track Guidance:      "+DO_LT_GUIDE)
	print("MSG: Late Track/Int. Guidance: "+DO_LTI_GUIDE)
	print("MSG: Early Intensity Guidance: "+DO_EI_GUIDE)
	print("MSG: Late Intensitsy Guidance: "+DO_LI_GUIDE)
	print("MSG: Late Pressure Guidance:   "+DO_LP_GUIDE)
else
	print("MSG: Guidance turned OFF.")
end if
if(DO_TRENDS)then
	print("MSG: Trends turned ON.")
	print("MSG: Late Track Trends:        "+DO_LT_TREND)
	print("MSG: Late Intensity Trends:    "+DO_LI_TREND)
	print("MSG: Late Pressure Trends:     "+DO_LP_TREND)
else
	print("MSG: Trends turned OFF.")
end if
if(DO_VERIFICATION)
	print("MSG: Verification turned ON.")
	print("MSG: Verify Early Track:       "+DO_ET_VER)
	print("MSG: Verify Early Intensity:   "+DO_EI_VER)
	print("MSG: Verify Late Track:        "+DO_LT_VER)
	print("MSG: Verify Late Intensity:    "+DO_LI_VER)
else
	print("MSG: Verification turned OFF.")
end if




;================================================================
; MANUAL OVERRIDES. FOR TESTING PURPOSES ONLY!
;================================================================

; Graphics Production
;doPlot1	= True		; Track Guidance
;doPlot2	= True		; Intensity Guidance
;doPlot3	= True		; Pressure Guidance
;doPlot4	= True		; HWRF-B Track Trends
;doPlot5	= True		; HWRF-B Intensity Trends
;doPlot6	= True		; HWRF-B Pressure Trends (not ready)
;DO_VERIFICATION	= True		; Verify cycles up to this point and create plots
;	DO_ET_VER = True
;	DO_EI_VER = True
;	DO_LT_VER	= True
;	DO_LI_VER	= True


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
; ADDITIONAL CONFIGURATION OPTIONS
;================================================================

; Add a lat/lon grid on map plots
gridOn	= True		


; Set experiment, directories, etc.
; MKedit: change variable name from long_name to title
; MKnote: function getModelInfo is updated in util and must have ModelInfo.dat in GPLOT/tbl
; long_name = getModelInfo(MCODE,"title") ; MKnote: I think this variable was mislabeled
title = getModelInfo(MCODE,"title")


; Choose number of forecasts for model trend plots
;nTrend		= 6


; Choose forecast hours to verify
; Eventually move this into the namelist
; deprecated
;verTimes	= (/"00","12","24","36","48","60","72","84","96","108","120"/)


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
;system("mkdir -p "+str_sub_str(ODIR,IDATE,IDATE06))


; Hours for XY Graphics (e.g., intensity, pressure)
XYHrs	:= LEAD_TIMES						; regular guidance/verification
XYHrs2	:= array_append_record((/-24,-12/),LEAD_TIMES,0)	; trends


; Define some executables
ADECK_EXE = GPLOT_DIR+"/shell/createADeck.ksh"
INTERP_HOME = GPLOT_DIR+"/NHC_interp/"
INTERP_EXE = "run.sh"
INTERP_NML = "intrfcst.input"


; Get the old SID (the invest), if it exists
SID_OLD = getInvestSID(YYYY,SID)



;================================================================;
;============================STEP ONE============================;
;================================================================;
;                     SETUP ATCF & STORM INFO                    ;
;================================================================;

print("")
print("MSG: ***************STEP ONE***************")
print("MSG: Check out the ATCF file. Also, get some info about the storm.")


;================================================================
; Setup the ATCF file and some information about the storm.
;================================================================

if(fileexists(ODIR+"ATCF_FILES.dat"))then
	print("MSG: Found a list of ATCF files for this particular cycle --> "+ODIR+"ATCF_FILES.dat")
	
	; Read ATCF_FILES.dat, which is created by the shell wrapper.
	; This file contains all ATCFs that match the cycle (IDATE)
	ATCF_FILE := asciiread(ODIR+"ATCF_FILES.dat",-1,"string")
	
	; Retain only the ATCF that matched the storm ID (SID)
	val := ind(isStrSubset2(ATCF_FILE,str_lower(SID)))
	
	; Check that 1 valid ATCF is found. If not, exit.
	if(dimsizes(val).gt.1)then
		print("WARNING: Found more than 1 matching ATCF file found in "+ODIR+"/ATCF_FILES.dat")
		print("WARNING: Will process only the first ATCF file.")
		val := val(dimsizes(val)-1)
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
	ATCF_FILE := systemfunc("find "+ATCF2_DIR+" -name "+str_get_dq()+"*"+str_lower(SID)+"*"+IDATE+"*"+ATCF2_TAG+str_get_dq());+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("find "+ATCF2_DIR+" -name "+str_get_dq()+"*"+IDATE+"*"+str_lower(SID)+"*"+ATCF2_TAG);+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("find "+ATCF1_DIR+" -name "+str_get_dq()+"*"+str_lower(SID)+"*"+IDATE+"*"+ATCF1_TAG);+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then
		ATCF_FILE := systemfunc("find "+ATCF1_DIR+" -name "+str_get_dq()+"*"+IDATE+"*"+str_lower(SID)+"*"+ATCF1_TAG);+" 2>/dev/null | xargs -n 1 basename 2>/dev/null")
	end if
	if(ismissing(ATCF_FILE))then	print("ERROR: Could not find matching ATCF at locations specified in namelist.")
					exit
	end if
end if
print("MSG: Found this ATCF file --> "+ATCF_FILE)
ATCF_BASE = systemfunc("basename "+ATCF_FILE)
ATCF_DIR = str_sub_str(ATCF_FILE,ATCF_BASE,"")
ATCF_MOD = systemfunc("find "+ATCF_FILE+" -mmin +60")


; Check if the ATCF file is the final ATCF
; The temporary ATCF file could be used if it is old enough.
if(isStrSubset(ATCF_FILE,ATCF2_DIR))then	print("MSG: This is the final ATCF.")
						FNL_ATCF = True
else if(.not.ismissing(ATCF_MOD))then		print("MSG: This is the final ATCF.")
						FNL_ATCF = True
else						print("MSG: This is not the final ATCF.")
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
if(BASIN1.eq."c")then   BASIN2 = "cp"   end if
if(BASIN1.eq."w")then   BASIN2 = "wp"   end if
if(BASIN1.eq."s")then   BASIN2 = "sh"   end if
if(BASIN1.eq."p")then   BASIN2 = "sh"   end if
if(BASIN1.eq."a")then   BASIN2 = "io"   end if
if(BASIN1.eq."b")then   BASIN2 = "io"   end if
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
ADECK_T = "a"+SID2+"_tmp.dat"


; Parse some important info about the old SID, if it exists.
if(.not.ismissing(SID_OLD))then
	tmp := tochar(SID_OLD)
	TC_NUM_OLD := tostring(tmp(:1))
	SID2_OLD    = BASIN2+tostring(tmp(:1))+YYYY
	ADECK_OLD   = "a"+SID2_OLD+".dat"
	ADECK_M_OLD = "a"+SID2_OLD+"_"+str_lower(MCODE)+".dat"
	ADECK_N_OLD = "a"+SID2_OLD+"_new.dat"
end if


; Verification is not performed for invests because NHC rules ignore them.
if(toint(TC_NUM).gt.50)then
	print("MSG: Verification turned OFF because this is an invest.")
	DO_VERIFICATION = False
else
	DO_VERIFICATION = True
end if


; Choose different TRACK models based on the HOUR
;if(any((/"00","12"/).eq.HH))then	TRKmodels  := TRKmodels00
;					TRKmodelsI := TRKmodelsI00
;					TRKmodelsT := TRKmodelsT00
;else if(any((/"06","18"/).eq.HH))then	TRKmodels  := TRKmodels06
;					TRKmodelsI := TRKmodelsI06
;					TRKmodelsT := TRKmodelsT06
;else					TRKmodels  := TRKmodels00
;					TRKmodelsI := TRKmodelsI00
;					TRKmodelsT := TRKmodelsT00
;end if  end if

; Update graphical products options based on what models were found.
if(TRKmodels(0).eq."MISSING" .and. DO_LT_GUIDE)then
	print("WARNING: Late track guidance turned OFF because no models were found (TRKM).")
	DO_LT_GUIDE = False
end if
if(TRKINTmodels(0).eq."MISSING" .and. DO_LTI_GUIDE)then
	print("WARNING: Late track/intensity combined guidance turned OFF because no models were found (TRKINTM).")
	DO_LTI_GUIDE = False
end if
if(TRKmodelsI(0).eq."MISSING" .and. DO_ET_GUIDE)then
	print("WARNING: Early track guidance turned OFF because no models were found (TRKMI).")
	DO_ET_GUIDE = False
end if
if(INTmodels(0).eq."MISSING" .and. DO_LI_GUIDE)then
	print("WARNING: Late max. intensity guidance turned OFF because no models were found (INTM).")
	DO_LI_GUIDE = False
end if
if(INTmodelsI(0).eq."MISSING" .and. DO_EI_GUIDE)then
	print("WARNING: Early max. intensity guidance turned OFF because no models were found (INTMI).")
	DO_EI_GUIDE = False
end if
if(PRSmodels(0).eq."MISSING" .and. DO_LP_GUIDE)then
	print("WARNING: Late min. pressure guidance turned OFF because no models were found (PRSM).")
	DO_LP_GUIDE = False
end if
if(TRKmodelsT(0).eq."MISSING" .and. DO_LT_TREND)then
	print("WARNING: Late track trend guidance turned OFF because no models were found (TRKMT).")
	DO_LT_TREND = False
end if
if(INTmodelsT(0).eq."MISSING" .and. DO_LI_TREND)then
	print("WARNING: Late max. intensity trend guidance turned OFF because no models were found (INTMT).")
	DO_LI_TREND = False
end if
if(PRSmodelsT(0).eq."MISSING" .and. DO_LP_TREND)then
	print("WARNING: Late min. pressure trend guidance turned OFF because no models were found (PRSMT).")
	DO_LP_TREND = False
end if
if(ltModels(0).eq."MISSING" .and. DO_LT_VER)then
	print("WARNING: Late track verification turned OFF because no models were found (LTM).")
	DO_LT_VER = False
end if
if(etModels(0).eq."MISSING" .and. DO_ET_VER)then
	print("WARNING: Early track verification turned OFF because no models were found (ETM).")
	DO_ET_VER = False
end if
if(liModels(0).eq."MISSING" .and. DO_LI_VER)then
	print("WARNING: Late max. intensity verification turned OFF because no models were found (LIM).")
	DO_EI_VER = False
end if

if(eiModels(0).eq."MISSING" .and. DO_EI_VER)then
	print("WARNING: Late max. intensity verification turned OFF because no models were found (EIM).")
	DO_EI_VER = False
end if




;================================================================
; Create color arrays for models
;================================================================

;TRKlongnameT    = getModelInfo(TRKmodelsT,"long_name")
;INTlongnameT    = getModelInfo(INTmodelsT,"long_name")


; Get a rainbow color map (support up to 100 forecasts)
tmp := read_colormap_file("default")
LIFEcolors  := array_append_record(tmp(2:27,:),tmp(2:27,:),0)
LIFEcolors  := array_append_record(LIFEcolors(:,:),tmp(2:27,:),0)
LIFEcolors  := array_append_record(LIFEcolors(:,:),tmp(2:27,:),0)




;================================================================
;============================STEP TWO===========================;
;================================================================
;   GET THE LATEST DECKS TO COMPARE FORECASTS & OBSERVATIONS    ;
;================================================================

print("")
print("MSG: *************STEP TWO************")
print("MSG: Get the latest A-DECK and B-DECK.")


;================================================================
; Create a new ADECK, copy latest BDECK
;================================================================

print("MSG: Create new A-DECK")

; Update the status file.
STATUS_FILE = ODIR+"status."+LONGSID+".log"
LOCK_FILE = STATUS_FILE+".lock"
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
system("echo 'working' > "+STATUS_FILE)
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

; Create a new ADECK file that includes the HWRF-B forecast
ADIR	= ODIR+"atcf/"
system("mkdir -p "+ADIR)


; Search for other ATCFs that match the storm ID in the final ATCF directory
; If other ATCFs are found, concatenate everything together into a new ATCF
; If other ATCFS not found, copy the original ATCF file to a new ATCF
test := systemfunc("ls "+ATCF2_DIR+"*"+str_lower(SID)+"*"+ATCF2_TAG+" 2>/dev/null")
if(.not.ismissing(test(0)))then
	TMP := systemfunc("date +%N")
	system(ADECK_EXE+" "+ADIR+" "+ATCF_DIR+"*"+str_lower(SID)+"* >/dev/null 2>&1")
	system("mv "+ADIR+ADECK+" "+ADIR+ADECK_M)
	system("cat "+ATCF_FILE+" "+ADIR+ADECK_M+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+"/"+TMP+"."+ADECK_M)
	system("mv "+ADIR+"/"+TMP+"."+ADECK_M+" "+ADIR+ADECK_M)
else
	system("cp "+ATCF_FILE+" "+ADIR+ADECK_M)
end if


; If the original model code and the new model code do not match, update the ATCF
if(MORIG.ne.MCODE)then   system("sed -i 's/"+MORIG+"/"+MCODE+"/g' "+ADIR+ADECK_M)   end if


; Merge the NHC ADECK file, if available
if(fileexists(ADECK_DIR+"/"+ADECK))then
	print("MSG: NHC A-DECK found in namelist location --> "+ADECK_DIR+"/"+ADECK)
	system("cp "+ADECK_DIR+"/"+ADECK+" "+ADIR+ADECK)
	system("cat "+ADIR+ADECK+" "+ADIR+ADECK_M+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+ADECK_N)
else if(fileexists(ADIR+ADECK))then
	print("MSG: NHC A-DECK found in working directory --> "+ADIR+ADECK)
	TMP := systemfunc("date +%N")
	system("grep "+str_get_dq()+str_upper(BASIN2)+", "+TC_NUM+","+str_get_dq()+" "+ADIR+ADECK+" > "+ADIR+"/"+TMP+"."+ADECK)
	system("mv "+ADIR+"/"+TMP+"."+ADECK+" "+ADIR+ADECK)
	system("cat "+ADIR+ADECK+" "+ADIR+ADECK_M+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+ADECK_N)
else
	print("WARNING: NHC A-DECK not found --> "+ADECK_DIR+"/"+ADECK)
	print("WARNING: This is OK, but graphics will be incomplete.")
	system("cp "+ADIR+ADECK_M+" "+ADIR+ADECK_N)
end if  end if


; Get the ATCFs that match the old storm ID, if appllicable
if(.not.ismissing(SID_OLD))then
	test := systemfunc("ls "+ATCF2_DIR+"*"+str_lower(SID_OLD)+"*"+ATCF2_TAG+" 2>/dev/null")
	if(.not.ismissing(test(0)))then
		system(ADECK_EXE+" "+ADIR+" "+ATCF_DIR+"*"+str_lower(SID_OLD)+"* >/dev/null 2>&1")
		system("mv "+ADIR+ADECK_OLD+" "+ADIR+ADECK_M_OLD)
	else
		system("echo "+str_get_dq()+str_get_dq()+" > "+ADIR+ADECK_M_OLD)
	end if
	
	if(MORIG.ne.MCODE)then   system("sed -i 's/"+MORIG+"/"+MCODE+"/g' "+ADIR+ADECK_M_OLD)   end if
	
	; Merge the NHC ADECK file, if available
	if(fileexists(ADECK_DIR+"/"+ADECK_OLD))then
		print("MSG: NHC A-DECK found in namelist location --> "+ADECK_DIR+"/"+ADECK_OLD)
		system("cp "+ADECK_DIR+"/"+ADECK_OLD+" "+ADIR+ADECK_OLD)
		system("cat "+ADIR+ADECK_OLD+" "+ADIR+ADECK_M_OLD+" | tac | " \
		       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+ADECK_N_OLD)
	else if(fileexists(ADIR+ADECK_OLD))then
		print("MSG: NHC A-DECK found in working directory --> "+ADIR+ADECK_OLD)
		TMP := systemfunc("date +%N")
		system("grep "+str_get_dq()+str_upper(BASIN2)+", "+TC_NUM_OLD+","+str_get_dq()+" "+ADIR+ADECK_OLD+" > "+ADIR+"/"+TMP+"."+ADECK_OLD)
		system("mv "+ADIR+"/"+TMP+"."+ADECK_OLD+" > "+ADIR+ADECK_OLD)
		system("cat "+ADIR+ADECK_OLD+" "+ADIR+ADECK_M_OLD+" | tac | " \
		       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+ADECK_N_OLD)
	else
		print("WARNING: NHC A-DECK not found --> "+ADECK_DIR+"/"+ADECK_OLD)
		print("WARNING: This is OK, but graphics will be incomplete.")
		system("cp "+ADIR+ADECK_M_OLD+" "+ADIR+ADECK_N_OLD)
	end if  end if
	

	TMP := systemfunc("date +%N")
	system("sed 's/"+str_upper(BASIN2)+", "+TC_NUM_OLD+",/"+str_upper(BASIN2)+", "+TC_NUM+",/g' "+ADIR+ADECK_N_OLD+" > "+ADIR+"/"+TMP+"."+ADECK_N_OLD)
	system("cat "+ADIR+"/"+TMP+"."+ADECK_N_OLD+" "+ADIR+ADECK_N+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+"/"+TMP+"."+ADECK_N)
	system("mv "+ADIR+"/"+TMP+"."+ADECK_N+" "+ADIR+ADECK_N)
	system("rm -f "+ADIR+"/"+TMP+"."+ADECK_N_OLD+" "+ADIR+"/"+TMP+"."+ADECK_N)
end if


if(fileexists(ODIR_ADECK+ADECK_N))then
	print("MSG: Existing A-DECK found in the GPLOT output directory --> "+ODIR_ADECK+ADECK_N)
	LOCK_FILE = ODIR_ADECK+ADECK_N+".lock"
	do while(fileexists(LOCK_FILE))
		print("MSG: '"+ODIR_ADECK+ADECK_N+"' is locked. Sleeping for 5 seconds...")
		sleep(5)
		LOCK_TEST := systemfunc("find "+LOCK_FILE+" -mmin +5 2>/dev/null")
		if(.not.ismissing(LOCK_TEST))then   system("rm -f "+LOCK_FILE)   end if
	end do

	TMP := systemfunc("date +%N")
	system("grep '^"+str_upper(BASIN2)+", "+TC_NUM+",' "+ODIR_ADECK+ADECK_N+" > "+ADIR+ADECK_T)
	system("awk '!(/"+MCODE+"/ && /"+IDATE+"/)' "+ADIR+ADECK_T+" > "+ADIR+"/"+TMP+"."+ADECK_T)
	system("mv "+ADIR+"/"+TMP+"."+ADECK_T+" "+ADIR+ADECK_T)
	if(MCODEI.ne."MISSING")then
		system("awk '!(/"+MCODEI+"/ && /"+IDATE06+"/)' "+ADIR+ADECK_T+" > "+ADIR+"/"+TMP+"."+ADECK_T)
		system("mv "+ADIR+"/"+TMP+"."+ADECK_T+" "+ADIR+ADECK_T)
	end if
	system("cat "+ADIR+ADECK_T +" "+ADIR+ADECK_N+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+"/"+TMP+"."+ADECK_N)
	system("mv "+ADIR+"/"+TMP+"."+ADECK_N+" "+ADIR+ADECK_N)
end if


; Remove any blank lines from ADECK_N
TMP := systemfunc("date +%N")
;system("sed -i '/^$/d' "+ADIR+ADECK_N)
system("awk -F',' 'NF > 12 {print}' "+ADIR+ADECK_N+" > "+ADIR+"/"+TMP+"."+ADECK_N)
system("mv "+ADIR+"/"+TMP+"."+ADECK_N+" "+ADIR+ADECK_N)


print("MSG: Copy latest B-DECK")
if(fileexists(BDECK_DIR+"/"+BDECK))then
	print("MSG: NHC B-DECK found --> "+BDECK_DIR+"/"+BDECK)
	system("cp "+BDECK_DIR+"/"+BDECK+" "+ADIR+BDECK)
	sleep(5)
else if(fileexists(ADIR+BDECK))then
	print("MSG: NHC B-DECK found in working directory --> "+ADIR+BDECK)
else
	print("WARNING: NHC B-DECK file not found.")
	print("WARNING: Verification turned OFF. Interpolation turned OFF.")
	DO_VERIFICATION = False
	DO_INTERP = False
end if  end if




;================================================================
; Produce Interpolated Forecasts
;================================================================

; Check if the interpolated forecast already exists. If it does,
; skip the interpolation here.
;TEST := systemfunc("grep "+str_get_dq()+MCODEI")

if(DO_INTERP)then
if(MCODEI.ne."MISSING" .and. MCODE12.ne."MISSING")then
	print("MSG: Produce Interpolated Forecasts.")

	; Set up the interpolation running directory.
	; Copy this from GPLOT_DIR to avoid too many jobs from accessing
	; it at the same time.
	INTERP_DIR = ADIR+"NHC_interp"
	system("rm -rf "+INTERP_DIR)
	system("cp -r "+INTERP_HOME+" "+INTERP_DIR)

	; Set up the interpolation ATCF directory.
	INTERP_ODIR = INTERP_DIR+"/atcf"
	INTERP_FILE = INTERP_ODIR+"/a"+SID2+".gun"
	system("mkdir -p "+INTERP_ODIR)
	system("ln -sf "+ADIR+ADECK_N+" "+INTERP_ODIR+"/"+ADECK)
	system("ln -sf "+ADIR+BDECK+" "+INTERP_ODIR+"/"+BDECK)
	
	; Set up the namelist
	system("cp "+INTERP_DIR+"/"+INTERP_NML+".template "+INTERP_DIR+"/"+INTERP_NML)
	system("sed -i 's/AAAA/"+MCODE+"/g' "+INTERP_DIR+"/"+INTERP_NML)
	system("sed -i 's/BBBB/"+MCODE12+"/g' "+INTERP_DIR+"/"+INTERP_NML)
	system("sed -i 's/CCCC/"+MCODEI+"/g' "+INTERP_DIR+"/"+INTERP_NML)
	system("sed -i 's/DDD/006/g' "+INTERP_DIR+"/"+INTERP_NML)
	system("sed -i 's/EEE/"+sprinti("%0.3i",FNL_HR)+"/g' "+INTERP_DIR+"/"+INTERP_NML)
	

	; Run the interpolation executable to produce early forecasts.
	INTERP_EXE = INTERP_DIR+"/"+INTERP_EXE
	system("cp "+INTERP_EXE+".template "+INTERP_EXE)
	system("sed -i 's#AAAA#"+INTERP_DIR+"#g' "+INTERP_EXE)
	system("sed -i 's#BBBB#"+INTERP_ODIR+"#g' "+INTERP_EXE)
	print("MSG: "+INTERP_EXE+" "+SID2+" doall "+INTERP_NML)
	system(INTERP_EXE+" "+SID2+" doall "+INTERP_NML+" &> "+INTERP_ODIR+"/a"+SID2+".log")
	
	if(fileexists(INTERP_FILE))then
		TMP := systemfunc("date +%N")
                system("cat "+ADIR+ADECK_N+" "+INTERP_FILE+" | tac | " \
                       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ADIR+"/"+TMP+"."+ADECK_N)
		system("mv "+ADIR+"/"+TMP+"."+ADECK_N+" "+ADIR+ADECK_N)
		system("sed '/"+MCODEI+"/p' "+INTERP_FILE+" | sort -u | sort -k3,3 -k5,5 -k6,6n > "+ADIR+ADECK_I)
	end if

end if
end if




;================================================================
; Copy the new ADECK to a common GPLOT path (ODIR_ADECK).
; Because multiple jobs might try to access this ADECK at the
; same time, check for a lock file. If the lock file exists, then
; wait until it is deleted by another job. Once the coast is
; clear, create a new lock file, merge/copy the new ADECK into
; the common GPLOT path. Then, delete the lock file so other jobs
; may proceed.
;================================================================
LOCK_FILE = ODIR_ADECK+ADECK_N+".lock"
system("lockfile -r-1 -l 180 "+LOCK_FILE);+" 2>&1")
print("MSG: '"+ODIR_ADECK+ADECK_N+"' has been locked ("+LOCK_FILE+").")

if(fileexists(ODIR_ADECK+ADECK_N))then
	
	; Check is the file is empty
	SIZE_TEST := systemfunc("find "+ODIR_ADECK+ADECK_N+" -size 0 2>/dev/null")
	if(.not.ismissing(SIZE_TEST))then
		print("ERROR: File is empty before writing --> "+ODIR_ADECK+ADECK_N)
	end if

	TMP := systemfunc("date +%N")
	system("cat "+ADIR+ADECK_N+" "+ODIR_ADECK+ADECK_N+" | tac | " \
	       +"sort -s -t, -k3,3 -k5,5 -k6,6n -k12,12 -u > "+ODIR_ADECK+"/"+TMP+"."+ADECK_N)
	if(.not.fileexists(ODIR_ADECK+"/"+TMP+"."+ADECK_N))then
		print("ERROR: File does not exist --> "+ODIR_ADECK+"/"+TMP+"."+ADECK_N)
	end if
	
	SIZE_TEST := systemfunc("find "+ODIR_ADECK+"/"+TMP+"."+ADECK_N+" -size 0 2>/dev/null")
	if(ismissing(SIZE_TEST))then
		print("MSG: File is not empty --> "+ODIR_ADECK+"/"+TMP+"."+ADECK_N)
		system("awk -F',' 'NF > 12 {print}' "+ODIR_ADECK+"/"+TMP+"."+ADECK_N+" | grep '^"+str_upper(BASIN2)+", "+TC_NUM+",' > "+ODIR_ADECK+ADECK_N)
	else
		print("ERROR: File is empty --> "+ODIR_ADECK+"/"+TMP+"."+ADECK_N)
		print("ERROR: File is empty --> "+ADIR+ADECK_N)
	end if
	system("rm -f "+ODIR_ADECK+"/"+TMP+"."+ADECK_N)
else
	system("cp "+ADIR+ADECK_N+" "+ODIR_ADECK+ADECK_N)
end if

; Check if the file is empty
SIZE_TEST := systemfunc("find "+ODIR_ADECK+ADECK_N+" -size 0 2>/dev/null")
if(.not.ismissing(SIZE_TEST))then
	print("ERROR: File is empty after writing --> "+ODIR_ADECK+ADECK_N)
end if

; Remove the lock file so other jobs can access the common ATCF.
print("MSG: '"+ODIR_ADECK+ADECK_N+"' has been unlocked ("+LOCK_FILE+").")
system("rm -f "+LOCK_FILE)




;================================================================
; Read ADECK data.
;================================================================
print("MSG: Reading A-DECK file --> "+ADIR+ADECK_N)
ADATA	:= adeckRead(ADIR+ADECK_N, "", "")
tcLats	:= ADATA(0,:)
tcLons	:= ADATA(1,:)
tcFHR	:= toint(ADATA(2,:))
tcINT	:= toint(ADATA(3,:))
tcPRES	:= toint(ADATA(4,:))
tcModel	:= ADATA@models
tcInit	:= ADATA@all_init_date
delete(ADATA)


; Get all the dates that apply to MCODE
vals	:= ind(tcModel.eq.MCODE .and. tcFHR%6.eq.0 .and. toint(tcInit).le.toint(IDATE))
if(ismissing(vals(0)))then
	print("ERROR: No cycles in the A-Deck match "+MCODE+". Can't continue.")

        ; Update the status file.
        system("lockfile -r-1 -l 180 "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
        system("echo 'failed' > "+STATUS_FILE)
        system("rm -f "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

	exit
end if
allDates := remove_duplicates(toint(tcInit(vals)))

; Remove all dates that are not within a week of any other date.
; This removes old invest forecasts that we don't want.
allOffset := new(dimsizes(allDates),"float")
allOffsetD := new(dimsizes(allDates),"float")
MaxOffset := 7.*24.
tmp := tochar(tostring(IDATE))
do ddd = 0,dimsizes(allDates)-1

	; Get the offset of each date from the current date (IDATE)
	tmp0	:= tochar(tostring(allDates(ddd)))
	allOffset(ddd)	= tofloat(cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
					  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0))\
			- tofloat(cd_inv_calendar(toint(tostring(tmp0(0:3))),toint(tostring(tmp0(4:5))),\
					  toint(tostring(tmp0(6:7))),toint(tostring(tmp0(8:9))),0,0,tunits,0))
	
	; Get the offset difference to find large jumps in time.
	if(ddd.ge.1)then   allOffsetD(ddd) = abs(allOffset(ddd) - allOffset(ddd-1))   end if
	
	; Only retain blocks of dates that include allOffset(ddd) = 0.
	if(.not.ismissing(allOffsetD(ddd)))then
		if(allOffsetD(ddd).gt.MaxOffset)then
			if(isvar("allDates2") .and. isvar("allOffset2"))then
				if(.not.any(allOffset2.eq.0))then	delete([/allDates2,allOffset2/])
				else					break
				end if
			end if
		end if
		
		if(isvar("allDates2"))then
			allDates2 := array_append_record(allDates2,allDates(ddd),0)
			allOffset2 := array_append_record(allOffset2,allOffset(ddd),0)
		else
			allDates2 = allDates(ddd)
			allOffset2 = allOffset(ddd)
		end if
	else if(dimsizes(allDates).eq.1)then
		allDates2 := allDates(ddd)
		allOffset2 := allOffset(ddd)
		allOffsetD := allOffset@_FillValue
	end if  end if
end do
allDates := allDates2
delete([/allOffset2,allDates2,allOffsetD/])


; LATE TRACK GUIDANCE
if(DO_LT_GUIDE)then
	print("MSG: Retrieving these late track models at "+IDATE+" --> "+stringOut(TRKmodels))
	aTRK	= new((/dimsizes(TRKmodels),4,100/),"float",fval3)
	do aaa = 0,dimsizes(TRKmodels)-1
		vals	:= ind(tcModel.eq.TRKmodels(aaa) .and. tcInit.eq.IDATE .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("WARNING: "+TRKmodels(aaa)+" is unavailable for "+IDATE+".")
			continue
		end if
		aTRK(aaa,0,:dimsizes(tcLats(vals))-1) = tcLats(vals)
		aTRK(aaa,1,:dimsizes(tcLons(vals))-1) = tcLons(vals)
		aTRK(aaa,2,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
		aTRK(aaa,3,:dimsizes(tcINT(vals))-1) = tofloat(tcINT(vals))
	end do
	vals := ind(.not.ismissing(aTRK(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these late track models at "+IDATE+" --> "+stringOut(TRKmodels(vals)))
	else				print("WARNING: None of the late track models were found.")
	end if
end if

; LATE TRACK/INTENSITY GUIDANCE
if(DO_LTI_GUIDE)then
	print("MSG: Retrieving these late track/intensity models at "+IDATE+" --> "+stringOut(TRKINTmodels))
	aTRKINT	= new((/dimsizes(TRKINTmodels),4,100/),"float",fval3)
	do aaa = 0,dimsizes(TRKINTmodels)-1
		vals	:= ind(tcModel.eq.TRKINTmodels(aaa) .and. tcInit.eq.IDATE .and. tcFHR%6.eq.0)
		if(ismissing(vals(0)))then
			print("WARNING: "+TRKINTmodels(aaa)+" is unavailable for "+IDATE+".")
			continue
		end if
		aTRKINT(aaa,0,:dimsizes(tcLats(vals))-1) = tcLats(vals)
		aTRKINT(aaa,1,:dimsizes(tcLons(vals))-1) = tcLons(vals)
		aTRKINT(aaa,2,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))
		aTRKINT(aaa,3,:dimsizes(tcINT(vals))-1) = tofloat(tcINT(vals))
	end do
	vals := ind(.not.ismissing(aTRKINT(:,0,0)))
	if(.not.ismissing(vals(0)))then	print("MSG: Found these late track/intensity models at "+IDATE+" --> "+stringOut(TRKINTmodels(vals)))
	else				print("WARNING: None of the late track models were found.")
	end if
end if



; EARLY TRACK GUIDANCE	
if(DO_ET_GUIDE)then
	system("mkdir -p "+str_sub_str(ODIR,IDATE,IDATE06))
	print("MSG: Retrieving these early track models at "+IDATE06+" --> "+stringOut(TRKmodelsI))
	aTRKI	= new((/dimsizes(TRKmodelsI),3,100/),"float",fval3)
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
end if


; LATE TRACK TRENDS
if(DO_LT_TREND)then
	print("MSG: Searching for forecasts trends from these track models --> "+stringOut(TRKmodelsT))
	aTRK2	= new((/dimsizes(TRKmodelsT),nTrend,3,100/),"float",fval3)
	IDATES	= new(nTrend,"string")
	aTRK3	= new((/dimsizes(TRKmodelsT),dimsizes(allDates),3,100/),"float",fval3)
	
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



; LATE INTENSITY GUIDANCE
if(DO_LI_GUIDE)then
	print("MSG: Retrieving these late intensity models at "+IDATE+" --> "+stringOut(INTmodels))
	aINT	= new((/dimsizes(INTmodels),3,100/),"float",fval3)
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
	aINT(:,0,:) = where(aINT(:,0,:).le.0, aINT@_FillValue, aINT(:,0,:))
end if


; EARLY INTENSITY GUIDANCE
if(DO_EI_GUIDE)then
	system("mkdir -p "+str_sub_str(ODIR,IDATE,IDATE06))
	print("MSG: Retrieving these early intensity models at "+IDATE06+" --> "+stringOut(INTmodelsI))
	aINTI	= new((/dimsizes(INTmodelsI),3,100/),"float",fval3)
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
	aINTI(:,0,:) = where(aINTI(:,0,:).le.0, aINTI@_FillValue, aINTI(:,0,:))
end if


; LATE INTENSITY TRENDS
if(DO_LI_TREND)then
	print("MSG: Searching for forecasts trends from these intensity models --> "+stringOut(INTmodelsT))
	aINT2	= new((/dimsizes(INTmodelsT),nTrend,3,100/),"float",fval3)
	IDATES2 = new(nTrend,"string")
	aINT3	= new((/dimsizes(INTmodelsT),dimsizes(allDates),2,100/),"float",fval3)
	
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
				aINT3(mmm,ddd,0,:dimsizes(tcINT(vals))-1) = tcINT(vals)
				aINT3(mmm,ddd,1,:dimsizes(tcFHR(vals))-1) = tofloat(tcFHR(vals))+tofloat(offset)
			end if
		end do
		vals := ind(.not.ismissing(aINT3(mmm,:,0,0)))
		if(.not.ismissing(vals(0)))then	print("MSG: Found these cycles --> "+stringOut(allDates(vals)))
		else				print("WARNING: No cycles were found.")
		end if
	end do
	aINT2(:,:,0,:) = where(aINT2(:,:,0,:).le.0, aINT2@_FillValue, aINT2(:,:,0,:))
	aINT3(:,:,0,:) = where(aINT3(:,:,0,:).le.0, aINT3@_FillValue, aINT3(:,:,0,:))
end if


; LATE PRESSURE GUIDANCE
if(DO_LP_GUIDE)then
	print("MSG: Retrieving these late pressure models at "+IDATE+" --> "+stringOut(PRSmodels))
	aPRS	= new((/dimsizes(PRSmodels),2,100/),"float",fval3)
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


; LATE PRESSURE TRENDS
if(DO_LP_TREND)then
	print("MSG: Retrieving last 6 cycles for "+MCODE)
	aPRS2	= new((/nTrend,2,100/),"float",fval3)
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

	; Update the status file.
        system("lockfile -r-1 -l 180 "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").") 
	system("echo 'working' > "+STATUS_FILE)
        system("rm -f "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

	; Read the b-deck
	data	:= bdeckRead(ADIR+BDECK,"none")
	tcLatsB	= data(0,:)		; Latitude
	tcLonsB	= data(1,:)		; Longitude
	tcINTB	= data(3,:)		; Maximum Intensity
	tcPRESB	= data(4,:)		; Minimum Pressure
	tcValidB= data@valid_times	; Valid Time
	tcCLASB	= data@type		; Classification Type

else
	tcValidB = "MISSING"
end if


; Add best track (1 "future" location, 1 current location, 5 prior locations)
; Note: bHrs should be a dynamic variable
bHrs	= tofloat(ispan(-30,MAX_FHR,6))
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
do ddd = -30,MAX_FHR,6
	
	; Update 'bDate' to be Valid Time instead of Initial Time
	utc	:= utc_base+tofloat(ddd)
	utc@units = tunits
	if(isvar("IDATESB"))then IDATESB := array_append_record(IDATESB,tostring(toint(cd_calendar(utc,3))),0)
	else			 IDATESB := tostring(toint(cd_calendar(utc,3)))
	end if
	
	vals := ind(tcValidB.eq.IDATESB(iii))
	if(.not.ismissing(vals(0)))then
		bTRK(0,iii) = tcLatsB(vals(0))
		bTRK(1,iii) = tcLonsB(vals(0))
		bINT(0,iii) = tcINTB(vals(0))
		bINT(1,iii) = tcPRESB(vals(0))
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
	else				bTRK3(0,ddd)	= tcLatsB(vals(0))
					bTRK3(1,ddd)	= tcLonsB(vals(0))
					bINT3(0,ddd)	= tcINTB(vals(0))
					bINT3(1,ddd)	= tcPRESB(vals(0))
					bCLAS3(ddd)	= tcCLASB(vals(0))
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
	DO_VERIFICATION = False
end if




;================================================================;
;===========================STEP THREE===========================;
;================================================================;
;              PRODUCE GRAPHICAL GUIDANCE PRODUCTS               ;
;================================================================;

if(DO_GUIDANCE)then

print("")
print("MSG: *************STEP THREE************")
print("MSG: PRODUCE GUIDANCE GRAPHICS.")

; Update the status file.
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
system("echo 'working' > "+STATUS_FILE)
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

; Workstation Resources
wtype	= "png"
;wtype	= "x11"
;wtype@wkWidth = 2048
wtype@wkWidth = 2560
;wtype@wkHeight = 2048
wtype@wkHeight = 2560


; General Track Resources
mpres = StatPlotRes(1)
if(gridOn)then
	mpres@mpGridAndLimbOn		= True
	mpres@mpGridLineColor		= "gray70"
	mpres@mpGridLineThicknessF	= 1.5
	mpres@mpGridLineDashPattern	= 2
end if

; General Intensity Resources
xyres = StatPlotRes(2)

; Text resources
txares = StatPlotRes(3)
txbres = StatPlotRes(4)
txres1 = StatPlotRes(5)
txres2 = StatPlotRes(6)
txres3 = StatPlotRes(7)
txres4 = StatPlotRes(8)
txres5 = StatPlotRes(9)
txres6 = StatPlotRes(10)

; Polyline Resources
plres = True
plres1 = StatPlotRes(11)
plres2 = StatPlotRes(12)
plres3 = StatPlotRes(13)
plres4 = StatPlotRes(14)
plres5 = StatPlotRes(15)

; Marker resources
mkres = True
mkres1 = StatPlotRes(16)
mkres2 = StatPlotRes(17)
mkres3 = StatPlotRes(18)




;================================================================
; Produce late track guidance graphics.
;================================================================
if(DO_LT_GUIDE)then

	print("MSG: Producing LATE MODEL TRACK GUIDANCE graphic.")
	DO_THIS = True

	; Find optimal map boundaries by including all tracks and
	; automatically resizing to fit everything with some buffer
	; between the edge of the map and the tracks.
	vals   := ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
	aTRK_LT = where(aTRK(:,2,:).le.tofloat(MAX_FHR),aTRK(:,0,:),aTRK@_FillValue)
	maxLat = max((/max(aTRK_LT),max(bTRK(0,vals))/))
	minLat = min((/min(aTRK_LT),min(bTRK(0,vals))/))
	rngLat = abs(maxLat-minLat)
	aTRK_LT := where(aTRK(:,2,:).le.tofloat(MAX_FHR),aTRK(:,1,:),aTRK@_FillValue)
	maxLon = max((/max(aTRK_LT),max(bTRK(1,vals))/))
	minLon = min((/min(aTRK_LT),min(bTRK(1,vals))/))
	rngLon = abs(maxLon-minLon)
	if(rngLon.gt.360. .and. maxLon.gt.180.)then		maxLon = maxLon - 360.
								rngLon = abs(maxLon-minLon)
	else if(rngLon.gt.360. .and. minLon.lt.-180.)then	minLon = minLon + 360.
								rngLon = abs(maxLon-minLon)
	end if  end if

	; Calculate the lat/lon ratio and correct it to make a nice graphic.
	llratio	= rngLat/rngLon
	if(llratio.lt.0.5)then		maxLat = maxLat + 0.5*abs(0.5*rngLon - rngLat)
					minLat = minLat - 0.5*abs(0.5*rngLon - rngLat)
					rngLat = abs(maxLat-minLat)
	else if(llratio.gt.0.5)then	maxLon = maxLon + 0.5*abs(2.*rngLat - rngLon)
					minLon = minLon - 0.5*abs(2.*rngLat - rngLon)
					rngLon = abs(maxLon-minLon)
	end if  end if


	if(ismissing(rngLon) .or. ismissing(rngLat) .or. rngLon.eq.0)then
		print("WARNING: Late model track guidance can't be produced.")
		DO_THIS = False
		DO_LT_GUIDE = False
	end if

	; Get model information
	TRKcolors   = getModelInfo(TRKmodels,"color")
	TRKmarkers  = getModelInfo(TRKmodels,"marker")
	TRKlongname = getModelInfo(TRKmodels,"long_name")
	

	; Enter here if everything looks good for graphical production.
	if(DO_THIS)then

		; Set up the output file
		OFILE	= ODIR+"TrackGuidance."+LONGSID+"."+IDATE+".late"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
		setvalues NhlGetWorkspaceObjectId()
                	"wsMaximumSize" : 500000000
		end setvalues

		;Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)

		; Add additional resources for this part "1a"
		mpres1a = mpres
		if(1.1*rngLat.lt.20.)then	mpres1a@gsnMajorLatSpacing = 2.
						mpres1a@gsnMinorLatSpacing = 1.
		else if(1.1*rngLat.lt.75.)then	mpres1a@gsnMajorLatSpacing = 5.
						mpres1a@gsnMinorLatSpacing = 1.
		else				mpres1a@gsnMajorLatSpacing = 10.
						mpres1a@gsnMinorLatSpacing = 2.
		end if  end if
		if(1.1*rngLon.lt.20.)then	mpres1a@gsnMajorLonSpacing = 2.
						mpres1a@gsnMinorLonSpacing = 1.
		else if(1.1*rngLon.lt.75.)then	mpres1a@gsnMajorLonSpacing = 5.
						mpres1a@gsnMinorLonSpacing = 1.
		else				mpres1a@gsnMajorLonSpacing = 10.
						mpres1a@gsnMinorLonSpacing = 2.
		end if  end if
		mpres1a@mpMaxLonF	= maxLon+(0.05*rngLon)
		mpres1a@mpMinLonF	= minLon-(0.05*rngLon)
		mpres1a@mpMaxLatF	= maxLat+(0.05*rngLat)
		mpres1a@mpMinLatF	= minLat-(0.05*rngLat)
		mpres1a@mpCenterLonF	= avg((/mpres1a@mpMaxLonF,mpres1a@mpMinLonF/))
		;mpres1a@tiMainString	= "Late Track Guidance"
		if(gridOn)then
			mpres1a@mpGridLatSpacingF = mpres1a@gsnMajorLatSpacing
			mpres1a@mpGridLonSpacingF = mpres1a@gsnMajorLonSpacing
		end if

		; Plot a blank map
		plot := gsn_csm_map_ce(wks,mpres1a)
		dum1 := new(dimsizes(TRKmodels)+1,"graphic")
		dum2 := new(dimsizes(TRKmodels)+1,"graphic")
		dum3 := new((/dimsizes(TRKmodels)+1,100/),"graphic")

		; Info about plot size
		getvalues plot
			"vpYF" : vpTOP
			"vpXF" : vpLFT
			"vpHeightF" : vpHGT
			"vpWidthF" : vpWID
		end getvalues
		vpRGT = vpLFT+vpWID
		vpBOT = vpTOP-vpHGT
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(TRKmodels(ind(.not.ismissing(aTRK(:,0,0)))))-1)/4)

		; Add best track
		mkres1@gsMarkerColor      = "black"
		mkres1@gsMarkerIndex	  = tc_marker
		mkres1@gsMarkerThicknessF = 5.0
		txres3@txFontColor        = "black"
		plres1@gsLineColor	= "black"
		vals  := ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
		vals2 := ind(toint(bHrs)%12.eq.0 .and. bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
		if(all(ismissing(bTRK(0,:))))then	vals3 := fval2
							vals3@_FillValue = fval2
		else					vals3 := vals2(ind(.not.ismissing(bTRK(0,vals2))))
		end if

		if(.not.ismissing(vals(0)) .and. .not.all(ismissing(bTRK(:,vals))) .and. .not.all(ismissing(bTRK(:,vals2))))then
			dum1(dimsizes(TRKmodels))	= gsn_add_polyline(wks,plot,bTRK(1,vals),bTRK(0,vals),plres1)
			dum2(dimsizes(TRKmodels))	= gsn_add_polymarker(wks,plot,bTRK(1,vals),bTRK(0,vals),mkres1)
			if(.not.ismissing(vals3(0)))then
				dum3(dimsizes(TRKmodels),vals3)	= gsn_add_text(wks,plot,tostring(toint(bHrs(vals2))),\
									       bTRK(1,vals2)+0.3,bTRK(0,vals2)+0.3,txres3)
			end if
		end if

		; Add best track legend
		mkres2@gsMarkerColor = "black"
		mkres2@gsMarkerIndex = tc_marker
		txres4@txFontColor   = "black"
		plres2@gsLineColor   = "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres4)


		; Add model tracks
		ppp3 = dimsizes(TRKmodels(ind(.not.ismissing(aTRK(:,0,0)))))-1
		do ppp = 0,dimsizes(TRKmodels)-1
			ppp2 = dimsizes(TRKmodels)-1-ppp
			if(all(ismissing(aTRK(ppp2,0,:))))then	print("WARNING: "+TRKmodels(ppp2)+" track is unavailable for "+IDATE+".")
								continue
			end if
	
			tmp	:= toint(where(ismissing(aTRK(ppp2,2,:)), fval2, aTRK(ppp2,2,:)))
			vals	:= ind(tmp.le.tofloat(MAX_FHR))
			vals2	:= ind(tmp%12.eq.0 .and. tmp.le.tofloat(MAX_FHR))
			if(all(ismissing(aTRK(ppp2,0,:))))then	vals3 = fval2
								vals@_FillValue = fval2
			else					vals3	:= vals2(ind(.not.ismissing(aTRK(ppp2,0,vals2))))
			end if
		
			plres2@gsLineColor   = TRKcolors(ppp2)
			plres3@gsLineColor   = TRKcolors(ppp2)
			mkres2@gsMarkerColor = TRKcolors(ppp2)
			mkres2@gsMarkerIndex = TRKmarkers(ppp2)
			txres3@txFontColor   = TRKcolors(ppp2)
			txres4@txFontColor   = TRKcolors(ppp2)

			dum1(ppp) = gsn_add_polyline(wks,plot,aTRK(ppp2,1,vals),aTRK(ppp2,0,vals),plres3)
			if(DO_MARKERS)then
				dum2(ppp) = gsn_add_polymarker(wks,plot,aTRK(ppp2,1,vals),aTRK(ppp2,0,vals),mkres2)
			end if
	
			if(DO_FHRLABELS .and. .not.ismissing(vals2(0)))then
				if(.not.ismissing(vals3(0)))then
					dum3(ppp,vals3) = gsn_add_text(wks,plot,tostring(toint(aTRK(ppp2,2,vals2))),\
								       aTRK(ppp2,1,vals2)+0.3,aTRK(ppp2,0,vals2)+0.3,txres3)
				end if
			end if		
	
			; Add legend info for each model
			if(DO_MARKERS)then
				gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.02,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%4)*0.25*vpWID),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/4),vpTOP+vpHEAD-0.02*tofloat(ppp3/4)/),plres2)
			gsn_text_ndc(wks,TRKlongname(ppp2),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),txres4)

			ppp3 = ppp3-1
		end do
	
		; Add a main title and a sub-title
		if(DO_TITLES)then	gsn_text_ndc(wks,"Late Track Guidance",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres1)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres2)
		end if

		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = mpres1a@mpMinLonF + 0.02*(mpres1a@mpMaxLonF-mpres1a@mpMinLonF)
					yloc = mpres1a@mpMinLatF + 0.02*(mpres1a@mpMaxLatF-mpres1a@mpMinLatF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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

	end if ;DO_THIS

end if ;DO_LT_GUIDE



;================================================================
; Produce late track/intensity combined guidance graphics.
;================================================================
if(DO_LTI_GUIDE)then

	print("MSG: Producing LATE MODEL TRACK INTENSITY-CODED GUIDANCE graphic.")
	DO_THIS = True

	; Find optimal map boundaries by including all tracks and
	; automatically resizing to fit everything with some buffer
	; between the edge of the map and the tracks.
	vals   := ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
	aTRKINT_LT = where(aTRKINT(:,2,:).le.tofloat(MAX_FHR),aTRKINT(:,0,:),aTRKINT@_FillValue)
	maxLat = max((/max(aTRKINT_LT),max(bTRK(0,vals))/))
	minLat = min((/min(aTRKINT_LT),min(bTRK(0,vals))/))
	rngLat = abs(maxLat-minLat)
	aTRKINT_LT := where(aTRKINT(:,2,:).le.tofloat(MAX_FHR),aTRKINT(:,1,:),aTRKINT@_FillValue)
	maxLon = max((/max(aTRKINT_LT),max(bTRK(1,vals))/))
	minLon = min((/min(aTRKINT_LT),min(bTRK(1,vals))/))
	rngLon = abs(maxLon-minLon)
	if(rngLon.gt.360. .and. maxLon.gt.180.)then		maxLon = maxLon - 360.
								rngLon = abs(maxLon-minLon)
	else if(rngLon.gt.360. .and. minLon.lt.-180.)then	minLon = minLon + 360.
								rngLon = abs(maxLon-minLon)
	end if  end if

	; Calculate the lat/lon ratio and correct it to make a nice graphic.
	llratio	= rngLat/rngLon
	if(llratio.lt.0.5)then		maxLat = maxLat + 0.5*abs(0.5*rngLon - rngLat)
					minLat = minLat - 0.5*abs(0.5*rngLon - rngLat)
					rngLat = abs(maxLat-minLat)
	else if(llratio.gt.0.5)then	maxLon = maxLon + 0.5*abs(2.*rngLat - rngLon)
					minLon = minLon - 0.5*abs(2.*rngLat - rngLon)
					rngLon = abs(maxLon-minLon)
	end if  end if

	if(ismissing(rngLon) .or. ismissing(rngLat) .or. rngLon.eq.0)then
		print("WARNING: Late model track intensity-coded guidance can't be produced.")
		DO_THIS = False
		DO_LTI_GUIDE = False
	end if

	; Get model information
	;TRKINTcolors   = getModelInfo(TRKINTmodels,"color")
	;TRKINTmarkers  = getModelInfo(TRKINTmodels,"marker")
	TRKINTlongname = getModelInfo(TRKINTmodels,"long_name")

	; Enter here if everything looks good for graphical production.
	if(DO_THIS)then

		; Set up the output file
		OFILE	= ODIR+"TrackIntensityGuidance."+LONGSID+"."+IDATE+".late"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
		setvalues NhlGetWorkspaceObjectId()
                	"wsMaximumSize" : 500000000
		end setvalues

		; Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
		cmap = read_colormap_file("tcwinds1")

		; Add additional resources for this part "3a"
		; At first, copy resources from "1a"
		mpres3a := mpres1a
		plres3a := plres1
		txres3a := txres4
		txres3a@txFontHeightF = 0.007
		txres3a@txFontColor = "black"

		; Add additional resources for this part "1a"
		mpres3a = mpres
		if(1.1*rngLat.lt.20.)then	mpres3a@gsnMajorLatSpacing = 2.
						mpres3a@gsnMinorLatSpacing = 1.
		else if(1.1*rngLat.lt.75.)then	mpres3a@gsnMajorLatSpacing = 5.
						mpres3a@gsnMinorLatSpacing = 1.
		else				mpres3a@gsnMajorLatSpacing = 10.
						mpres3a@gsnMinorLatSpacing = 2.
		end if  end if
		if(1.1*rngLon.lt.20.)then	mpres3a@gsnMajorLonSpacing = 2.
						mpres3a@gsnMinorLonSpacing = 1.
		else if(1.1*rngLon.lt.75.)then	mpres3a@gsnMajorLonSpacing = 5.
						mpres3a@gsnMinorLonSpacing = 1.
		else				mpres3a@gsnMajorLonSpacing = 10.
						mpres3a@gsnMinorLonSpacing = 2.
		end if  end if
		mpres3a@mpMaxLonF	= maxLon+(0.05*rngLon)
		mpres3a@mpMinLonF	= minLon-(0.05*rngLon)
		mpres3a@mpMaxLatF	= maxLat+(0.05*rngLat)
		mpres3a@mpMinLatF	= minLat-(0.05*rngLat)
		mpres3a@mpCenterLonF	= avg((/mpres3a@mpMaxLonF,mpres3a@mpMinLonF/))
		;mpres1a@tiMainString	= "Late Track Guidance"
		if(gridOn)then
			mpres3a@mpGridLatSpacingF = mpres3a@gsnMajorLatSpacing
			mpres3a@mpGridLonSpacingF = mpres3a@gsnMajorLonSpacing
		end if
		mpres3a@vpYF = 0.5

		; Plot a blank map
		plot := gsn_csm_map_ce(wks,mpres3a)
		dum1 := new((/dimsizes(TRKINTmodels)+1,100/),"graphic")
		dum2 := new((/dimsizes(TRKINTmodels)+1,100/),"graphic")
		dum3 := new((/dimsizes(TRKINTmodels)+1,100/),"graphic")
		dum4 := new(100,"graphic")

		; Info about plot size
		getvalues plot
			"vpYF" : vpTOP
			"vpXF" : vpLFT
			"vpHeightF" : vpHGT
			"vpWidthF" : vpWID
		end getvalues
		vpRGT = vpLFT+vpWID
		vpBOT = vpTOP-vpHGT
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(TRKINTmodels(ind(.not.ismissing(aTRKINT(:,0,0)))))-1)/5)

		; Add model tracks
		ppp3 = dimsizes(TRKINTmodels(ind(.not.ismissing(aTRKINT(:,0,0)))))-1
		do ppp = 0,dimsizes(TRKINTmodels)-1
			ppp2 = dimsizes(TRKINTmodels)-1-ppp
			if(all(ismissing(aTRKINT(ppp2,0,:))))then	print("WARNING: "+TRKINTmodels(ppp2)+" track is unavailable for "+IDATE+".")
								continue
			end if
	
			tmp	:= toint(where(ismissing(aTRKINT(ppp2,2,:)), fval2, aTRKINT(ppp2,2,:)))
			vals	:= ind(tmp.le.tofloat(MAX_FHR) .and. .not.ismissing(aTRKINT(ppp2,0,:)))
	
			model_marker := NhlNewMarker(wks,tostring(ppp2+1),25,0.0,0.0,1.0,1.0,0.0)	
			mkres2@gsMarkerIndex = model_marker
			if(.not.ismissing(vals(0)) .and. .not.all(ismissing(aTRKINT(ppp2,0,vals))))then
				do aaa = 0,dimsizes(vals)-1

					; Determine line/marker colors based on intensity
					if(aTRKINT(ppp2,3,vals(aaa)).lt.20)then		mkres2@gsMarkerColor := cmap(1,:)
											plres3@gsLineColor := cmap(1,:)
											txres3@txFontColor := cmap(1,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.34)then	mkres2@gsMarkerColor := cmap(2,:)
											plres3@gsLineColor := cmap(2,:)
											txres3@txFontColor := cmap(2,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.50)then	mkres2@gsMarkerColor := cmap(3,:)
											plres3@gsLineColor := cmap(3,:)
											txres3@txFontColor := cmap(3,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.64)then	mkres2@gsMarkerColor := cmap(4,:)
											plres3@gsLineColor := cmap(4,:)
											txres3@txFontColor := cmap(4,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.83)then	mkres2@gsMarkerColor := cmap(5,:)
											plres3@gsLineColor := cmap(5,:)
											txres3@txFontColor := cmap(5,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.96)then	mkres2@gsMarkerColor := cmap(6,:)
											plres3@gsLineColor := cmap(6,:)
											txres3@txFontColor := cmap(6,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.114)then	mkres2@gsMarkerColor := cmap(7,:)
											plres3@gsLineColor := cmap(7,:)
											txres3@txFontColor := cmap(7,:)
					else if(aTRKINT(ppp2,3,vals(aaa)).lt.137)then	mkres2@gsMarkerColor := cmap(8,:)
											plres3@gsLineColor := cmap(8,:)
											txres3@txFontColor := cmap(8,:)
					else						mkres2@gsMarkerColor := cmap(9,:)
											plres3@gsLineColor := cmap(9,:)
											txres3@txFontColor := cmap(9,:)
					end if  end if  end if  end if
					end if  end if  end if  end if
	
					; Plot the line from the current back to the previous point
					if(aaa.lt.dimsizes(vals)-1)then   dum1(ppp,aaa) = gsn_add_polyline(wks,plot,aTRKINT(ppp2,1,vals(aaa):vals(aaa+1)),\
														    aTRKINT(ppp2,0,vals(aaa):vals(aaa+1)),plres3)
					end if
	
					; Add the marker for the current point
					if(aaa.eq.dimsizes(vals)-1)then
						dum2(ppp,aaa) = gsn_add_polymarker(wks,plot,aTRKINT(ppp2,1,vals(aaa)),aTRKINT(ppp2,0,vals(aaa)),mkres2)
					end if
				end do
			end if

			; Add legend info for each model
			; Determine line/marker colors based on intensity
			if(max(aTRKINT(ppp2,3,vals)).lt.20)then		mkres2@gsMarkerColor := cmap(1,:)
									plres2@gsLineColor := cmap(1,:)
									txres3@txFontColor := cmap(1,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.34)then	mkres2@gsMarkerColor := cmap(2,:)
									plres2@gsLineColor := cmap(2,:)
									txres3@txFontColor := cmap(2,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.50)then	mkres2@gsMarkerColor := cmap(3,:)
									plres2@gsLineColor := cmap(3,:)
									txres3@txFontColor := cmap(3,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.64)then	mkres2@gsMarkerColor := cmap(4,:)
									plres2@gsLineColor := cmap(4,:)
									txres3@txFontColor := cmap(4,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.83)then	mkres2@gsMarkerColor := cmap(5,:)
									plres2@gsLineColor := cmap(5,:)
									txres3@txFontColor := cmap(5,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.96)then	mkres2@gsMarkerColor := cmap(6,:)
									plres2@gsLineColor := cmap(6,:)
									txres3@txFontColor := cmap(6,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.114)then	mkres2@gsMarkerColor := cmap(7,:)
									plres2@gsLineColor := cmap(7,:)
									txres3@txFontColor := cmap(7,:)
			else if(max(aTRKINT(ppp2,3,vals)).lt.137)then	mkres2@gsMarkerColor := cmap(8,:)
									plres2@gsLineColor := cmap(8,:)
									txres3@txFontColor := cmap(8,:)
			else						mkres2@gsMarkerColor := cmap(9,:)
									plres2@gsLineColor := cmap(9,:)
									txres3@txFontColor := cmap(9,:)
			end if  end if  end if  end if
			end if  end if  end if  end if
			if(DO_MARKERS)then
				gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%5)*0.2*vpWID)+0.02,vpTOP+vpHEAD-0.02*tofloat(ppp3/5),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%5)*0.2*vpWID),vpLFT+(tofloat(ppp3%5)*0.2*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/5),vpTOP+vpHEAD-0.02*tofloat(ppp3/5)/),plres2)
			gsn_text_ndc(wks,TRKINTlongname(ppp2),vpLFT+(tofloat(ppp3%5)*0.2*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/5),txres3a)

			ppp3 = ppp3-1
		end do

		; Add best track
		mkres1@gsMarkerIndex	  = tc_marker
		mkres1@gsMarkerThicknessF = 5.0
		txres3@txFontColor        := "black"
		plres3aa := plres3a
		plres3aa@gsLineColor	= "black"
		plres3aa@gsLineThicknessF = 17.0
		;plres3a@gsEdgesOn	= True
		;plres3a@gsEdgeColor	= "black"
		;plres3a@gsEdgeThicknessF = 2.0
		vals  := ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR) .and. .not.ismissing(bTRK(0,:)))

		if(.not.ismissing(vals(0)) .and. .not.all(ismissing(bTRK(:,vals))))then
			do aaa = 0,dimsizes(vals)-1
				; Determine line/marker colors based on intensity
				if(bINT(0,vals(aaa)).lt.20)then		mkres1@gsMarkerColor := cmap(1,:)
									plres3a@gsLineColor := cmap(1,:)
				else if(bINT(0,vals(aaa)).lt.34)then	mkres1@gsMarkerColor := cmap(2,:)
									plres3a@gsLineColor := cmap(2,:)
				else if(bINT(0,vals(aaa)).lt.50)then	mkres1@gsMarkerColor := cmap(3,:)
									plres3a@gsLineColor := cmap(3,:)
				else if(bINT(0,vals(aaa)).lt.64)then	mkres1@gsMarkerColor := cmap(4,:)
									plres3a@gsLineColor := cmap(4,:)
				else if(bINT(0,vals(aaa)).lt.83)then	mkres1@gsMarkerColor := cmap(5,:)
									plres3a@gsLineColor := cmap(5,:)
				else if(bINT(0,vals(aaa)).lt.96)then	mkres1@gsMarkerColor := cmap(6,:)
									plres3a@gsLineColor := cmap(6,:)
				else if(bINT(0,vals(aaa)).lt.114)then	mkres1@gsMarkerColor := cmap(7,:)
									plres3a@gsLineColor := cmap(7,:)
				else if(bINT(0,vals(aaa)).lt.137)then	mkres1@gsMarkerColor := cmap(8,:)
									plres3a@gsLineColor := cmap(8,:)
				else					mkres1@gsMarkerColor := cmap(9,:)
									plres3a@gsLineColor := cmap(9,:)
				end if  end if  end if  end if
				end if  end if  end if  end if

				; Plot the line from the current back to the previous point
				if(aaa.gt.0)then
					dum4(aaa) = gsn_add_polyline(wks,plot,bTRK(1,vals(aaa-1):vals(aaa)),bTRK(0,vals(aaa-1):vals(aaa)),plres3aa)
					dum1(dimsizes(TRKINTmodels),aaa) = gsn_add_polyline(wks,plot,bTRK(1,vals(aaa-1):vals(aaa)),bTRK(0,vals(aaa-1):vals(aaa)),plres3a)
				end if

				; Add the marker for the current point
				dum2(dimsizes(TRKINTmodels),aaa) = gsn_add_polymarker(wks,plot,bTRK(1,vals(aaa)),bTRK(0,vals(aaa)),mkres1)

				; Add a forecast hour label every 12 h
				if(toint(bHrs(vals(aaa)))%12.eq.0 .and. bHrs(vals(aaa)).ge.-12. .and. bHrs(vals(aaa)).le.tofloat(MAX_FHR))then
					dum3(dimsizes(TRKINTmodels),aaa) = gsn_add_text(wks,plot,tostring(toint(bHrs(vals(aaa)))),\
										     bTRK(1,vals(aaa))+(2.*rngLon/100.),bTRK(0,vals(aaa))+(2.*rngLon/100.),txres3)
				end if
			end do
		end if


		; Add best track legend
		mkres2@gsMarkerColor := "black"
		mkres2@gsMarkerIndex = tc_marker
		txres3a@txFontColor   := "black"
		plres2@gsLineColor   := "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres3a)


		; Add color bar for intensity levels
		lbres := True
		lbres@vpWidthF           = 0.2
		lbres@vpHeightF          = 0.05
		lbres@lbBoxLinesOn       = True
		lbres@lbFillColors       = cmap(1:,:)
		lbres@lbMonoFillPattern  = True
		lbres@lbOrientation      = "Horizontal"
		lbres@lbPerimOn          = False
		lbres@lbLabelFontHeightF = 0.008
		lbres@lbLabelAutoStride  = False 
		lbres@lbLabelAlignment	 = "InteriorEdges"
		lbres@lbTitleString	 = "Maximum Wind [kt]"
		gsn_labelbar_ndc(wks,9,(/"20","34","50","64","83","96","114","137"/),vpRGT-0.27*vpWID,vpTOP-0.05*vpHGT,lbres)
	
		; Add a main title and a sub-title
		if(DO_TITLES)then	gsn_text_ndc(wks,"Late Track Guidance (Intensity-Colored)",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres1)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres2)
		end if

		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = mpres3a@mpMinLonF + 0.02*(mpres3a@mpMaxLonF-mpres3a@mpMinLonF)
					yloc = mpres3a@mpMinLatF + 0.02*(mpres3a@mpMaxLatF-mpres3a@mpMinLatF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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

	end if ;DO_THIS

end if ;DO_LTI_GUIDE



;================================================================
; Produce early track guidance graphics.
;================================================================
if(DO_ET_GUIDE)then

	print("MSG: Producing EARLY MODEL TRACK GUIDANCE graphic.")
	DO_THIS = True

	; Find map boundaries. Resize if necessary
        vals   := ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
        aTRKI_0 = where(aTRKI(:,2,:).le.tofloat(MAX_FHR),aTRKI(:,0,:),aTRKI@_FillValue)
        maxLat = max((/max(aTRKI_0),max(bTRK(0,vals))/))
        minLat = min((/min(aTRKI_0),min(bTRK(0,vals))/))
        rngLat = abs(maxLat-minLat)
        aTRKI_1 := where(aTRKI(:,2,:).le.tofloat(MAX_FHR),aTRKI(:,1,:),aTRKI@_FillValue)
        maxLon = max((/max(aTRKI_1),max(bTRK(1,vals))/))
        minLon = min((/min(aTRKI_1),min(bTRK(1,vals))/))
	rngLon	= abs(maxLon-minLon)
	if(rngLon.gt.360. .and. maxLon.gt.180.)then		maxLon = maxLon - 360.
								rngLon = abs(maxLon-minLon)
	else if(rngLon.gt.360. .and. minLon.lt.-180.)then	minLon = minLon + 360.
								rngLon = abs(maxLon-minLon)
	end if  end if
	if(ismissing(rngLon) .or. ismissing(rngLat) .or. rngLon.eq.0)then
		print("WARNING: Late model track guidance can't be produced.")
		DO_THIS = False
		DO_ET_GUIDE = False
	end if
	

	if(DO_THIS)then

		; Get model information
		TRKcolorsI  = getModelInfo(TRKmodelsI,"color")
		TRKmarkersI = getModelInfo(TRKmodelsI,"marker")
		TRKlongnameI    = getModelInfo(TRKmodelsI,"long_name")

		; Calculate the lat/lon ratio and correct it to make a nice graphic.
		llratio	= rngLat/rngLon
		if(llratio.lt.0.5)then		maxLat = maxLat + 0.5*abs(0.5*rngLon - rngLat)
						minLat = minLat - 0.5*abs(0.5*rngLon - rngLat)
						rngLat = abs(maxLat-minLat)
		else if(llratio.gt.0.5)then	maxLon = maxLon + 0.5*abs(2.*rngLat - rngLon)
						minLon = minLon - 0.5*abs(2.*rngLat - rngLon)
						rngLon = abs(maxLon-minLon)
		end if  end if
		;print("MSG: Correcting map lat/lon ratio from "+llratio+" to "+(rngLat/rngLon))
	

		; Set up the output file
		OFILE	= str_sub_str(ODIR,IDATE,IDATE06)+"TrackGuidance."+LONGSID+"."+IDATE06+".early"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
                setvalues NhlGetWorkspaceObjectId()
	                "wsMaximumSize" : 500000000
                end setvalues


		;Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


		; Set resources for plots and additional features
		mpres1b	= mpres
		if(1.1*rngLat.lt.20.)then	mpres1b@gsnMajorLatSpacing = 2.
						mpres1b@gsnMinorLatSpacing = 1.
		else if(1.1*rngLat.lt.75.)then	mpres1b@gsnMajorLatSpacing = 5.
						mpres1b@gsnMinorLatSpacing = 1.
		else				mpres1b@gsnMajorLatSpacing = 10.
						mpres1b@gsnMinorLatSpacing = 2.
		end if  end if
		if(1.1*rngLon.lt.20.)then	mpres1b@gsnMajorLonSpacing = 2.
						mpres1b@gsnMinorLonSpacing = 1.
		else if(1.1*rngLon.lt.75.)then	mpres1b@gsnMajorLonSpacing = 5.
						mpres1b@gsnMinorLonSpacing = 1.
		else				mpres1b@gsnMajorLonSpacing = 10.
						mpres1b@gsnMinorLonSpacing = 2.
		end if  end if
		mpres1b@mpMaxLonF = maxLon+(0.05*rngLon)
		mpres1b@mpMinLonF = minLon-(0.05*rngLon)
		mpres1b@mpMaxLatF = maxLat+(0.05*rngLat)
		mpres1b@mpMinLatF = minLat-(0.05*rngLat)
		mpres1b@mpCenterLonF = avg((/mpres1b@mpMaxLonF,mpres1b@mpMinLonF/))
		if(gridOn)then
			mpres1b@mpGridLatSpacingF = mpres1b@gsnMajorLatSpacing
			mpres1b@mpGridLonSpacingF = mpres1b@gsnMajorLonSpacing
		end if


		; Plot a blank map
		plot	:= gsn_csm_map_ce(wks,mpres1b)
		dum1	:= new(dimsizes(TRKmodelsI)+1,"graphic")
		dum2	:= new(dimsizes(TRKmodelsI)+1,"graphic")
		dum3	:= new((/dimsizes(TRKmodelsI)+1,100/),"graphic")


		; Info about plot size
		getvalues plot
			"vpYF" : vpTOP
			"vpXF" : vpLFT
			"vpHeightF" : vpHGT
			"vpWidthF" : vpWID
		end getvalues
		vpRGT = vpLFT+vpWID
		vpBOT = vpTOP-vpHGT
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(TRKmodelsI(ind(.not.ismissing(aTRKI(:,0,0)))))-1)/4)


		; Add best track
		mkres1@gsMarkerColor      := "black"
		mkres1@gsMarkerIndex	  := tc_marker
		mkres1@gsMarkerThicknessF := 5.0
		txres3@txFontColor        := "black"
		plres1@gsLineColor        := "black"
		vals	:= ind(bHrsI.ge.-12. .and. bHrsI.le.tofloat(MAX_FHR))
		vals2	:= ind(toint(bHrsI)%12.eq.0 .and. bHrsI.ge.-12. .and. bHrsI.le.tofloat(MAX_FHR))
		if(ismissing(vals2(0)) .or. all(ismissing(bTRK(0,:))))then
			vals3   := fval2
			vals3@_FillValue = fval2
		else
			vals3	:= vals2(ind(.not.ismissing(bTRK(0,vals2))))
		end if
		if(.not.all(ismissing(bTRK(:,vals))) .and. .not.ismissing(vals3(0)))then
			dum1(dimsizes(TRKmodelsI))	= gsn_add_polyline(wks,plot,bTRK(1,vals),bTRK(0,vals),plres1)
			dum2(dimsizes(TRKmodelsI))	= gsn_add_polymarker(wks,plot,bTRK(1,vals),bTRK(0,vals),mkres1)
			dum3(dimsizes(TRKmodelsI),vals3) = gsn_add_text(wks,plot,tostring(toint(bHrsI(vals2))),\
									bTRK(1,vals2)+0.3,bTRK(0,vals2)+0.3,txres3)
		end if


		; Add best track legend
		mkres2@gsMarkerColor = "black"
		mkres2@gsMarkerIndex = tc_marker
		txres4@txFontColor   = "black"
		plres2@gsLineColor   = "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres4)


		; Add model tracks
		ppp3 = dimsizes(TRKmodelsI(ind(.not.ismissing(aTRKI(:,0,0)))))-1
		do ppp = 0,dimsizes(TRKmodelsI)-1
			ppp2 = dimsizes(TRKmodelsI)-1-ppp
			if(all(ismissing(aTRKI(ppp2,0,:))))then	print("WARNING: "+TRKmodelsI(ppp2)+" track is unavailable for "+IDATE06+".")
								continue
			end if
	
			tmp	:= toint(where(ismissing(aTRKI(ppp2,2,:)), fval2, aTRKI(ppp2,2,:)))
			vals	:= ind(tmp.le.tofloat(MAX_FHR))
			vals2	:= ind(tmp%12.eq.0 .and. tmp.le.tofloat(MAX_FHR))
			vals3	:= vals2(ind(.not.ismissing(aTRKI(ppp2,0,vals2))))
		
			plres2@gsLineColor   := TRKcolorsI(ppp2)
			plres3@gsLineColor   := TRKcolorsI(ppp2)
			mkres2@gsMarkerColor := TRKcolorsI(ppp2)
			mkres2@gsMarkerIndex := TRKmarkersI(ppp2)
			txres3@txFontColor   := TRKcolorsI(ppp2)
			txres4@txFontColor   := TRKcolorsI(ppp2)
	
			dum1(ppp) = gsn_add_polyline(wks,plot,aTRKI(ppp2,1,vals),aTRKI(ppp2,0,vals),plres3)
			if(DO_MARKERS)then
				dum2(ppp) = gsn_add_polymarker(wks,plot,aTRKI(ppp2,1,vals),aTRKI(ppp2,0,vals),mkres2)
			end if
			if(DO_FHRLABELS .and. .not.ismissing(vals2(0)) .and. .not.ismissing(vals3(0)))then
				dum3(ppp,vals3) = gsn_add_text(wks,plot,tostring(toint(aTRKI(ppp2,2,vals2))),\
							       aTRKI(ppp2,1,vals2)+0.3,aTRKI(ppp2,0,vals2)+0.3,txres3)
			end if
			
			; Add legend info for each model
			if(DO_MARKERS)then
				gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.02,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%4)*0.25*vpWID),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/4),vpTOP+vpHEAD-0.02*tofloat(ppp3/4)/),plres2)
			gsn_text_ndc(wks,TRKlongnameI(ppp2),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),txres4)

			ppp3 = ppp3-1
		end do
		
		
		; Add a main title and a sub-title
		if(DO_TITLES)then	gsn_text_ndc(wks,"Early Track Guidance",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres1)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE06,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres2)
		end if


		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = mpres1b@mpMinLonF + 0.02*(mpres1b@mpMaxLonF-mpres1b@mpMinLonF)
					yloc = mpres1b@mpMinLatF + 0.02*(mpres1b@mpMaxLatF-mpres1b@mpMinLatF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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


	end if ;DO_THIS

end if ;DO_ET_GUIDE



;================================================================
; Produce late intensity guidance graphics.
;================================================================
if(DO_LI_GUIDE)then

	print("MSG: Producing LATE MODEL INTENSITY GUIDANCE graphic.")
	DO_THIS = True

	; Find the XY plot boundaries
	vals	:= ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
	if(ismissing(vals(0)))then
		print("WARNING: Late model intensity guidance can't be produced.")
		DO_THIS = False
		DO_LI_GUIDE = False
	end if
	if(DO_THIS)then
		aINT_0	= where(aINT(:,1,:).le.tofloat(MAX_FHR),aINT(:,0,:),aINT@_FillValue)
		maxInt	:= max((/max(aINT_0),max(bINT(0,vals))/))
		minInt	:= min((/min(aINT_0),min(bINT(0,vals))/))
		;maxInt	:= max((/max(aINT(:,0,:)),max(bINT(0,:))/))
		;minInt	:= min((/min(aINT(:,0,:)),min(bINT(0,:))/))
		if(ismissing(maxInt) .or. ismissing(minInt) .or. maxInt.eq.minInt)then
			print("WARNING: Late model intensity guidance can't be produced.")
			DO_THIS = False
			DO_LI_GUIDE = False
		end if
	end if

	; Get model information
	INTcolors   = getModelInfo(INTmodels,"color")
	INTmarkers  = getModelInfo(INTmodels,"marker")
	INTlongname = getModelInfo(INTmodels,"long_name")


	if(DO_THIS)then

		rngInt	:= abs(maxInt-minInt)
		if(1.2*rngInt.ge.100.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,20)
		else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,10)
		else if(1.2*rngInt.lt.50.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,5)
		end if  end if  end if

		; Apply forecast hour cutoff
		vals    := ind(bHrs.le.tofloat(MAX_FHR) .and. .not.ismissing(bINT(0,:)))

		; Setup the output file & workstation
		OFILE	= ODIR+"IntensityGuidance."+LONGSID+"."+IDATE+".late"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
                setvalues NhlGetWorkspaceObjectId()
	                "wsMaximumSize" : 500000000
                end setvalues


		;Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


		; Update resources
		xyres2a			= xyres
		;xyres2a@tiMainString	= "Late Intensity Guidance"
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
		;xyres2a@tmXBLabelFontHeightF:= 0.018
		;xyres2a@tmYLLabelFontHeightF:= 0.018
		xyres2a@trYMaxF		= maxInt+(0.10*rngInt)
		xyres2a@trYMinF		= minInt-(0.10*rngInt)
		xyres2a@xyMarkLineMode	= "MarkLines"
		xyres2a@xyMarkers	= tc_marker

		; Plot the best track
		;plot	:= gsn_csm_xy(wks,bHrs({-6:6.}),bINT(0,{-6:6.}),res)
		;plot	:= gsn_csm_xy(wks,bHrs,bINT(0,:),xyres2a)
                if(.not.ismissing(vals(0)) .and. dimsizes(vals).ge.2)then
			plot	:= gsn_csm_xy(wks,bHrs(vals),bINT(0,vals),xyres2a)
		else
			xyres2a@tmXBLabelFontHeightF = 0.018
			xyres2a@tmYLLabelFontHeightF = 0.018
			xyres2a@tiXAxisFontHeightF = 0.018
			xyres2a@tiYAxisFontHeightF = 0.018
			plot	:= gsn_csm_blank_plot(wks,xyres2a)
		end if


		; Info about plot size
		getvalues plot
			"vpYF" : vpTOP
			"vpXF" : vpLFT
			"vpHeightF" : vpHGT
			"vpWidthF" : vpWID
		end getvalues
		vpRGT = vpLFT+vpWID
		vpBOT = vpTOP-vpHGT
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(INTmodels(ind(.not.ismissing(aINT(:,0,0)))))-1)/4)


		; Add best track legend
		mkres2@gsMarkerColor := "black"
		mkres2@gsMarkerIndex := tc_marker
		txres4@txFontColor   := "black"
		plres2@gsLineColor   := "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres4)


		; Draw the background grid
		dum1	:= new(dimsizes(XYHrs),"graphic")
		dum2	:= new(dimsizes(IntMjrGrd),"graphic")
		dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres2a@trYMinF,xyres2a@trYMaxF/),plres4)
		do fff = 0,dimsizes(XYHrs)-1
			if(XYHrs(fff).eq.0)then   continue   end if
			dum1(fff) = gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres2a@trYMinF,xyres2a@trYMaxF/),plres5)
		end do
		do fff = 0,dimsizes(IntMjrGrd)-1
			dum2(fff) = gsn_add_polyline(wks,plot,(/xyres2a@trXMinF,xyres2a@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres5)
		end do


		ppp3 = dimsizes(INTmodels(ind(.not.ismissing(aINT(:,0,0)))))-1
		do ppp = 0,dimsizes(INTmodels)-1
			ppp2	= dimsizes(INTmodels)-1-ppp
			tmp	:= toint(where(ismissing(aINT(ppp2,1,:)), fval2, aINT(ppp2,1,:)))
			;vals	:= ind(tmp%6.eq.0)
			vals	:= ind(tmp%6.eq.0 .and. tmp.le.tofloat(MAX_FHR) .and. .not.ismissing(tmp))
	
			if(ismissing(vals(0)) .or. \
			   all(ismissing(aINT(ppp2,0,vals))) .or. \
			   dimsizes(vals).eq.1)then
				print("WARNING: "+INTmodels(ppp2)+" intensity is unavailable for "+IDATE+".")
				continue
			end if

			; Update resources
			xyres2a@xyLineThicknessF	= 10.0
			xyres2a@xyLineColor		:= INTcolors(ppp2)
			xyres2a@tiXAxisString		= ""
			xyres2a@tiYAxisString		= ""
			if(DO_MARKERS)then
				xyres2a@xyMarkLineMode		= "MarkLines"
				xyres2a@xyMarkers               := INTmarkers(ppp2)
				xyres2a@xyMarkerColor           := INTcolors(ppp2)
				xyres2a@xyMarkerThicknessF      = 6.0
				xyres2a@xyMarkerSizeF           = 0.015
			else
				xyres2a@xyMarkLineMode  	= "Lines"
			end if
	
			plres2@gsLineColor   = INTcolors(ppp2)
			txres4@txFontColor   = INTcolors(ppp2)
	
			; Overlay the new intensity line
			overlay(plot,gsn_csm_xy(wks,aINT(ppp2,1,vals),aINT(ppp2,0,vals),xyres2a))
	
			; Add legend info for each model
			if(DO_MARKERS)then
				mkres2@gsMarkerColor := INTcolors(ppp2)
				mkres2@gsMarkerIndex := INTmarkers(ppp3)
				gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.02,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%4)*0.25*vpWID),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/4),vpTOP+vpHEAD-0.02*tofloat(ppp3/4)/),plres2)
			gsn_text_ndc(wks,INTlongname(ppp2),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),txres4)

			ppp3 = ppp3-1
		end do
		
		
		; Add a main title and a sub-title.
		if(DO_TITLES)then	gsn_text_ndc(wks,"Late Intensity Guidance",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres5)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres6)
		end if


		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = xyres2a@trXMinF + 0.02*(xyres2a@trXMaxF-xyres2a@trXMinF)
					yloc = xyres2a@trYMinF + 0.02*(xyres2a@trYMaxF-xyres2a@trYMinF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
		end if


		; Actually plot here
		draw(plot)
		frame(wks)


		; Remove white space on plot
		if (DO_RMWHITE .and. DO_CONVERTGIF) then
		    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
		else if (DO_CONVERTGIF) then
		    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
		else if (DO_RMWHITE) then
		    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
		end if  end if  end if


	end if ;DO_THIS

end if ;DO_LI_GUIDE


;================================================================
; Produce early intensity guidance graphics.
;================================================================
if(DO_EI_GUIDE)then

	print("MSG: Producing EARLY MODEL INTENSITY GUIDANCE graphic.")
		
	; Find the XY plot boundaries
	vals	:= ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
	if(ismissing(vals(0)))then
		print("WARNING: Early model intensity guidance can't be produced.")
		DO_THIS = False
		DO_EI_GUIDE = False
	end if
	if(DO_THIS)then
		aINTI_0	= where(aINTI(:,1,:).le.tofloat(MAX_FHR),aINTI(:,0,:),aINTI@_FillValue)
		maxInt	:= max((/max(aINTI_0),max(bINT(0,vals))/))
		minInt	:= min((/min(aINTI_0),min(bINT(0,vals))/))
		if(ismissing(maxInt) .or. ismissing(minInt) .or. maxInt.eq.minInt)then
			print("WARNING: Early model intensity guidance can't be produced.")
			DO_THIS = False
			DO_EI_GUIDE = False
		end if
	end if


	if(DO_THIS)then

		; Get model information
		INTcolorsI  = getModelInfo(INTmodelsI,"color")
		INTmarkersI = getModelInfo(INTmodelsI,"marker")
		INTlongnameI    = getModelInfo(INTmodelsI,"long_name")

		rngInt	:= abs(maxInt-minInt)
		if(1.2*rngInt.ge.100.)then
			IntMjrGrd := ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
					   toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,20)
		else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
			IntMjrGrd := ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
					   toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,10)
		else if(1.2*rngInt.lt.50.)then
			IntMjrGrd := ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
					   toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,5)
		end if  end if  end if

		; Apply forecast hour cutoff
		vals    := ind(bHrs.le.tofloat(MAX_FHR) .and. .not.ismissing(bINT(0,:)))


		; Setup the output file
		OFILE	= str_sub_str(ODIR,IDATE,IDATE06)+"IntensityGuidance."+LONGSID+"."+IDATE06+".early"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
                setvalues NhlGetWorkspaceObjectId()
	                "wsMaximumSize" : 500000000
                end setvalues


		;Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


		; Update resources
		xyres2b = xyres
		;xyres2b@tiMainString	= "Early Intensity Guidance"
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
		xyres2b@xyMarkLineMode	= "MarkLines"
		xyres2b@xyMarkers	= tc_marker
	
		; Plot the best track
		if(.not.ismissing(vals(0)) .and. dimsizes(vals).ge.2)then
			plot	:= gsn_csm_xy(wks,bHrs(vals),bINT(0,vals),xyres2a)
		else
			plot	:= gsn_csm_blank_plot(wks,xyres2a)
		end if

		; Info about plot size
		getvalues plot
			"vpYF" : vpTOP
			"vpXF" : vpLFT
			"vpHeightF" : vpHGT
			"vpWidthF" : vpWID
		end getvalues
		vpRGT = vpLFT+vpWID
		vpBOT = vpTOP-vpHGT
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(INTmodelsI(ind(.not.ismissing(aINTI(:,0,0)))))-1)/4)


		; Add best track legend
		mkres2@gsMarkerColor := "black"
		mkres2@gsMarkerIndex := tc_marker
		txres4@txFontColor   := "black"
		plres2@gsLineColor   := "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres4)


		; Draw the background grid
		dum1	:= new(dimsizes(XYHrs),"graphic")
		dum2	:= new(dimsizes(IntMjrGrd),"graphic")
		dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres2b@trYMinF,xyres2b@trYMaxF/),plres4)
		do fff = 0,dimsizes(XYHrs)-1
			if(XYHrs(fff).eq.0)then   continue   end if
			dum1(fff) = gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres2b@trYMinF,xyres2b@trYMaxF/),plres5)
		end do
		do fff = 0,dimsizes(IntMjrGrd)-1
			dum2(fff) = gsn_add_polyline(wks,plot,(/xyres2b@trXMinF,xyres2b@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres5)
		end do


		ppp3 = dimsizes(INTmodelsI(ind(.not.ismissing(aINTI(:,0,0)))))-1
		do ppp = 0,dimsizes(INTmodelsI)-1
			ppp2	= dimsizes(INTmodelsI)-1-ppp
			tmp	:= toint(where(ismissing(aINTI(ppp2,1,:)), fval2, aINTI(ppp2,1,:)))
			vals	:= ind(tmp%6.eq.0 .and. tmp.le.toint(MAX_FHR) .and. .not.ismissing(tmp))
		
			if(ismissing(vals(0)) .or. all(ismissing(aINTI(ppp2,0,vals))) .or. \
			   dimsizes(vals).eq.1)then
				print("WARNING: "+INTmodelsI(ppp2)+" early intensity is unavailable for "+IDATE06+".")
				continue
			end if

			; Update resources
			xyres2b@xyLineThicknessF= 10.0
			xyres2b@xyLineColor	:= INTcolorsI(ppp2)
			xyres2b@tiXAxisString	= ""
			xyres2b@tiYAxisString	= ""
			if(DO_MARKERS)then	xyres2b@xyMarkLineMode  = "MarkLines"
						xyres2b@xyMarkers       := INTmarkersI(ppp2)
						xyres2b@xyMarkerColor   := INTcolorsI(ppp2)
						xyres2b@xyMarkerThicknessF = 6.0
						xyres2b@xyMarkerSizeF   = 0.015
			else			xyres2b@xyMarkLineMode  = "Lines"
			end if
	
			plres2@gsLineColor   := INTcolorsI(ppp2)
			txres4@txFontColor   := INTcolorsI(ppp2)
	
			; Overlay the new intensity line
			overlay(plot,gsn_csm_xy(wks,aINTI(ppp2,1,vals),aINTI(ppp2,0,vals),xyres2b))

			; Add legend info for each model
			if(DO_MARKERS)then	mkres2@gsMarkerColor := INTcolorsI(ppp2)
						mkres2@gsMarkerIndex := INTmarkersI(ppp2)
						gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.02, \
								       vpTOP+vpHEAD-0.02*tofloat(ppp3/4),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%4)*0.25*vpWID),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/4),vpTOP+vpHEAD-0.02*tofloat(ppp3/4)/),plres2)
			gsn_text_ndc(wks,INTlongnameI(ppp2),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),txres4)

			ppp3 = ppp3-1
		end do
		
		
		; Add a main title and a sub-title
		if(DO_TITLES)then	gsn_text_ndc(wks,"Early Intensity Guidance",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres5)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE06,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres6)
		end if


		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = xyres2b@trXMinF + 0.02*(xyres2b@trXMaxF-xyres2b@trXMinF)
					yloc = xyres2b@trYMinF + 0.02*(xyres2b@trYMaxF-xyres2b@trYMinF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
		end if

		; Actually plot here
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

	end if ;DO_THIS

end if ;DO_EI_GUIDE


;================================================================
; Produce late pressure guidance graphics
;================================================================
if(DO_LP_GUIDE)then

	print("MSG: Producing LATE MODEL PRESSURE GUIDANCE graphic.")
	DO_THIS = True

	; Find the XY plot boundaries
	vals	:= ind(bHrs.ge.-12. .and. bHrs.le.tofloat(MAX_FHR))
	if(ismissing(vals(0)))then
		print("WARNING: Late model pressure guidance can't be produced.")
		DO_THIS = False
		DO_LP_GUIDE = False
	end if
	if(DO_THIS)then
		aPRS_0	= where(aPRS(:,1,:).le.tofloat(MAX_FHR),aPRS(:,0,:),aPRS@_FillValue)
		maxPrs	:= max((/max(aPRS_0),max(bINT(1,vals))/))
		minPrs	:= min((/min(aPRS_0),min(bINT(1,vals))/))
		if(ismissing(maxPrs) .or. ismissing(minPrs) .or. maxPrs.eq.minPrs)then
			print("WARNING: Late model pressure guidance can't be produced.")
			DO_THIS = False
			DO_LP_GUIDE = False
		end if
	end if

	; Get model information
	PRScolors   = getModelInfo(PRSmodels,"color")
	PRSmarkers  = getModelInfo(PRSmodels,"marker")
	PRSlongname = getModelInfo(PRSmodels,"long_name")


	if(DO_THIS)then

		rngPrs	:= abs(maxPrs-minPrs)
		if(1.2*rngPrs.ge.50.)then
			PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%10 + 10,\
						 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%10,10)
		else if(1.2*rngPrs.ge.20. .and. 1.2*rngPrs.lt.50.)then
			PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%5 + 5,\
						 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%5,5)
		else if(1.2*rngPrs.lt.20.)then
			PrsMjrGrd	:= ispan(toint(minPrs-(0.10*rngPrs)) - toint(minPrs-(0.10*rngPrs))%2 + 2,\
						 toint(maxPrs+(0.10*rngPrs)) - toint(maxPrs+(0.10*rngPrs))%2,2)
		end if  end if  end if


		; Setup the output file
		OFILE	= ODIR+"PressureGuidance."+LONGSID+"."+IDATE+".late"
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
                setvalues NhlGetWorkspaceObjectId()
	                "wsMaximumSize" : 500000000
                end setvalues


		;Create a hurricane symbol marker
		tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)


		; Set resources
		xyres3a = xyres
		;xyres3a@tiMainString	= "Late Pressure Guidance"
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
		xyres3a@xyMarkLineMode	= "MarkLines"
		xyres3a@xyMarkers	= tc_marker

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
		
		; Determine how to use space on top of graphic
		vpHEAD = 0.0125+0.02*tofloat((dimsizes(PRSmodels(ind(.not.ismissing(aPRS(:,0,0)))))-1)/4)


		; Add best track legend
		mkres2@gsMarkerColor := "black"
		mkres2@gsMarkerIndex := tc_marker
		txres4@txFontColor   := "black"
		plres2@gsLineColor   := "black"
		gsn_polymarker_ndc(wks,vpLFT+0.02,vpTOP+vpHEAD+0.02,mkres2)
		gsn_polyline_ndc(wks,(/vpLFT,vpLFT+0.04/),(/vpTOP+vpHEAD+0.02,vpTOP+vpHEAD+0.02/),plres2)
		gsn_text_ndc(wks,"BEST: Best Track",vpLFT+0.045,vpTOP+vpHEAD+0.02,txres4)


		; Draw the background grid
		dum1	:= new(dimsizes(XYHrs),"graphic")
		dum2	:= new(dimsizes(IntMjrGrd),"graphic")
		dum1(ind(XYHrs.eq.0))	= gsn_add_polyline(wks,plot,(/0.,0./),(/xyres3a@trYMinF,xyres3a@trYMaxF/),plres4)
		do fff = 0,dimsizes(XYHrs)-1
			if(XYHrs(fff).eq.0)then   continue   end if
			dum1(fff) = gsn_add_polyline(wks,plot,(/XYHrs(fff),XYHrs(fff)/),(/xyres3a@trYMinF,xyres3a@trYMaxF/),plres5)
		end do
		do fff = 0,dimsizes(IntMjrGrd)-1
			dum2(fff) = gsn_add_polyline(wks,plot,(/xyres3a@trXMinF,xyres3a@trXMaxF/),(/IntMjrGrd(fff),IntMjrGrd(fff)/),plres5)
		end do


		ppp3 = dimsizes(PRSmodels(ind(.not.ismissing(aPRS(:,0,0)))))-1
		do ppp = 0,dimsizes(PRSmodels)-1
			ppp2	= dimsizes(PRSmodels)-1-ppp
			tmp	:= toint(where(ismissing(aPRS(ppp2,1,:)), fval2, aPRS(ppp2,1,:)))
			vals	:= ind(tmp%6.eq.0)
			
			if(ismissing(vals(0)) .or. all(ismissing(aPRS(ppp2,0,vals))) .or. dimsizes(vals).eq.1)then
				print("WARNING: "+PRSmodels(ppp2)+" pressure is unavailable for "+IDATE+".")
				continue
			end if
	
			; Update resources
			xyres3a@xyLineThicknessF= 10.0
			xyres3a@xyLineColor	:= PRScolors(ppp2)
			xyres3a@tiXAxisString	= ""
			xyres3a@tiYAxisString	= ""
			if(DO_MARKERS)then
				xyres3a@xyMarkLineMode  = "MarkLines"
				xyres3a@xyMarkers       := PRSmarkers(ppp2)
				xyres3a@xyMarkerColor   := PRScolors(ppp2)
				xyres3a@xyMarkerThicknessF = 6.0
				xyres3a@xyMarkerSizeF   = 0.015
			else
				xyres3a@xyMarkLineMode  = "Lines"
			end if
	
			plres2@gsLineColor   := PRScolors(ppp2)
			txres4@txFontColor   := PRScolors(ppp2)
		
			; Overlay the new pressure line
			overlay(plot,gsn_csm_xy(wks,aPRS(ppp2,1,vals),aPRS(ppp2,0,vals),xyres3a))
	
			; Add legend info for each model
			if(DO_MARKERS)then
				mkres2@gsMarkerColor := PRScolors(ppp2)
				mkres2@gsMarkerIndex := PRSmarkers(ppp2)
				gsn_polymarker_ndc(wks,vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.02,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),mkres2)
			end if
			gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ppp3%4)*0.25*vpWID),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.04/),\
					     (/vpTOP+vpHEAD-0.02*tofloat(ppp3/4),vpTOP+vpHEAD-0.02*tofloat(ppp3/4)/),plres2)
			gsn_text_ndc(wks,PRSlongname(ppp2),vpLFT+(tofloat(ppp3%4)*0.25*vpWID)+0.045,vpTOP+vpHEAD-0.02*tofloat(ppp3/4),txres4)

			ppp3 = ppp3-1
		end do
		
		
		; Add a main title and a sub-title
		if(DO_TITLES)then	gsn_text_ndc(wks,"Late Pressure Guidance",vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.05,txres5)
					gsn_text_ndc(wks,"Storm: "+TC_NAME+" ("+str_upper(SID)+") valid "+IDATE,vpLFT+0.50*vpWID,vpTOP+vpHEAD+0.02,txres6)
		end if


		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = xyres3a@trXMinF + 0.02*(xyres3a@trXMaxF-xyres3a@trXMinF)
					yloc = xyres3a@trYMinF + 0.02*(xyres3a@trYMaxF-xyres3a@trYMinF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
		end if

		; Actually plot here
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

	end if ;DO_THIS

end if ;DO_LP_GUIDE


end if ; DO_GUIDANCE




;================================================================;
;===========================STEP FOUR============================;
;================================================================;
;               PRODUCE GRAPHICAL TREND PRODUCTS                 ;
;================================================================;

if(DO_TRENDS)then

print("")
print("MSG: *************STEP FOUR************")
print("MSG: PRODUCE TREND & LIFETIME GRAPHICS.")

; Update the status file.
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
system("echo 'working' > "+STATUS_FILE)
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

; Get model information.
cmap    := read_colormap_file("NCV_blue_red")
cmap    := cmap(:120:-1,:)	; keep blues only
TRDcolors   = cmap(2:92:18,:)	; blue-faded color map
TRDmarkers  = (/8,7,6,0,4,16/)


;================================================================
; Produce late track trend graphics.
;================================================================
if(DO_LT_TREND)then
	DO_LT_LIFE = DO_LT_TREND

	; ********************
	; FOR TREND GRAPHICS

	; Update resources
	mpres4a = mpres
	plres4a = plres
	plres4a@gsLineThicknessF = 5.0
	mkres4a = mkres
	txares4a = txares
	txbres4a = txbres

	; Find map boundaries for trend graphics
	maxLat	:= max((/max(aTRK2(:,:,0,:)),max(bTRK(0,:))/))
	minLat	:= min((/min(aTRK2(:,:,0,:)),min(bTRK(0,:))/))
	rngLat	:= abs(maxLat-minLat)
	maxLon	:= max((/max(aTRK2(:,:,1,:)),max(bTRK(1,:))/))
	minLon	:= min((/min(aTRK2(:,:,1,:)),min(bTRK(1,:))/))
	rngLon	:= abs(maxLon-minLon)
	if(rngLon.gt.360. .and. maxLon.gt.180.)then		maxLon = maxLon - 360.
								rngLon = abs(maxLon-minLon)
	else if(rngLon.gt.360. .and. minLon.lt.-180.)then	minLon = minLon + 360.
								rngLon = abs(maxLon-minLon)
	end if  end if
	if(ismissing(rngLat) .or. ismissing(rngLon) .or. rngLon.eq.0)then
		print("WARNING: Track trend graphics can't be produced.")
		DO_LT_TREND = False
	end if
	
	if(DO_LT_TREND)then
		; Resize the boundaries to create a nice graphic.
		llratio	:= rngLat/rngLon
		if(llratio.lt.0.5)then		maxLat	= maxLat + 0.5*abs(0.5*rngLon - rngLat)
						minLat	= minLat - 0.5*abs(0.5*rngLon - rngLat)
						rngLat	= abs(maxLat-minLat)
		else if(llratio.gt.0.5)then	maxLon	= maxLon + 0.5*abs(2.*rngLat - rngLon)
						minLon	= minLon - 0.5*abs(2.*rngLat - rngLon)
						rngLon	= abs(maxLon-minLon)
		end if  end if
	end if


	; ********************
	; FOR LIFETIME TRACK GRAPHICS

	; Update resources
	mpres4b = mpres
	mpres4b@tiMainOffsetYF = 0.015
	plres4b = plres
	mkres4b = mkres
	txares4b = txares
	txbres4b = txbres

	; Only show model track positions that match up with Best Track positions.
	aTRK3 = where(conform(aTRK3,aTRK3(:,:,2,:),(/0,1,3/)).gt.max(bHrs3), aTRK3@_FillValue, aTRK3)

	; Find map boundaries for trend graphics
	maxLat3	:= max((/max(aTRK3(:,:,0,:)),max(bTRK3(0,:))/))
	minLat3	:= min((/min(aTRK3(:,:,0,:)),min(bTRK3(0,:))/))
	rngLat3	:= abs(maxLat3-minLat3)
	maxLon3	:= max((/max(aTRK3(:,:,1,:)),max(bTRK3(1,:))/))
	minLon3	:= min((/min(aTRK3(:,:,1,:)),min(bTRK3(1,:))/))
	rngLon3	:= abs(maxLon3-minLon3)
	if(rngLon3.gt.360. .and. maxLon3.gt.180.)then		maxLon3 = maxLon3 - 360.
								rngLon3 = abs(maxLon3-minLon3)
	else if(rngLon3.gt.360. .and. minLon3.lt.-180.)then	minLon3 = minLon3 + 360.
								rngLon3 = abs(maxLon3-minLon3)
	end if  end if
	if(ismissing(rngLat3) .or. ismissing(rngLon3) .or. rngLon3.eq.0)then
		print("WARNING: Lifetime track graphics can't be produced.")
		DO_LT_LIFE = False
	end if
	
	; Resize the boundaries to create a nice graphic.
	if(DO_LT_LIFE)then
		llratio3 := rngLat3/rngLon3
		if(llratio3.lt.0.5)then		maxLat3	= maxLat3 + 0.5*abs(0.5*rngLon3 - rngLat3)
						minLat3	= minLat3 - 0.5*abs(0.5*rngLon3 - rngLat3)
						rngLat3	= abs(maxLat3-minLat3)
		else if(llratio3.gt.0.5)then	maxLon3	= maxLon3 + 0.5*abs(2.*rngLat3 - rngLon3)
						minLon3	= minLon3 - 0.5*abs(2.*rngLat3 - rngLon3)
						rngLon3	= abs(maxLon3-minLon3)
		end if  end if
	end if


	; Loop over selected models to produce 6-cycle trends & lifetime track maps
	if(DO_LT_TREND .or. DO_LT_LIFE)then

		; Remember the original values for trend and lifetime graphics
		DO_LT_TREND2 = DO_LT_TREND
		DO_LT_LIFE2 = DO_LT_LIFE
		DO_LT_TREND = False
		DO_LT_LIFE = False
	
		do mmm = 0,dimsizes(TRKmodelsT)-1
	
			; Simple variable for current model
			thisModel = TRKmodelsT(mmm)
		
			; Go in here if producing trend graphics.
			if(DO_LT_TREND2)then
				print("MSG: Producing TRACK TREND graphic for "+thisModel)
	
				if(all(ismissing(aTRK2(mmm,:,0,:))))then	print("WARNING: "+thisModel+" track is unavailable through "+IDATE+".")
										continue
				end if
		
				; Set up the output file
				OFILE	= ODIR+"TrackTrend."+thisModel+"."+LONGSID+"."+IDATE
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks	:= gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues
			
				;Create a hurricane symbol marker
				tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
		
				; Set resources for plots and additional features
				;res@tiMainString		= TRKmodelsT(mmm)+" Late Track Trend"
				if(1.1*rngLat.lt.20.)then	mpres4a@gsnMajorLatSpacing	= 2.
								mpres4a@gsnMinorLatSpacing	= 1.
				else if(1.1*rngLat.lt.75.)then	mpres4a@gsnMajorLatSpacing	= 5.
								mpres4a@gsnMinorLatSpacing	= 1.
				else				mpres4a@gsnMajorLatSpacing      = 10.
								mpres4a@gsnMinorLatSpacing      = 2.
				end if  end if
				if(1.1*rngLon.lt.20.)then	mpres4a@gsnMajorLonSpacing	= 2.
								mpres4a@gsnMinorLonSpacing	= 1.
				else if(1.1*rngLon.lt.75.)then	mpres4a@gsnMajorLonSpacing	= 5.
								mpres4a@gsnMinorLonSpacing	= 1.
				else				mpres4a@gsnMajorLonSpacing      = 10.
								mpres4a@gsnMinorLonSpacing      = 2.
				end if  end if
				mpres4a@mpMaxLonF			= maxLon+(0.05*rngLon)
				mpres4a@mpMinLonF			= minLon-(0.05*rngLon)
				mpres4a@mpMaxLatF			= maxLat+(0.05*rngLat)
				mpres4a@mpMinLatF			= minLat-(0.05*rngLat)
				mpres4a@mpCenterLonF			= avg((/mpres4a@mpMaxLonF,mpres4a@mpMinLonF/))
				if(gridOn)then	mpres4a@mpGridLatSpacingF = mpres4a@gsnMajorLatSpacing
						mpres4a@mpGridLonSpacingF = mpres4a@gsnMajorLonSpacing
				end if
		
				; Plot a blank map & setup dummy arrays for graphic add-ons
				plot	:= gsn_csm_map_ce(wks,mpres4a)
				dum1	:= new(nTrend+1,"graphic")
				dum2	:= new(nTrend+1,"graphic")
				dum3	:= new((/nTrend+1,100/),"graphic")
		
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
	
					; Check if the track is available
					if(all(ismissing(aTRK2(mmm,ttt,0,:))))then
						print("WARNING: "+thisModel+" track is unavailable for "+IDATES(ttt)+".")
						continue
					end if
			
					; Retain only forecast hours that are divisible by 6
					tmp	:= toint(where(ismissing(aTRK2(mmm,ttt,2,:)), fval2, aTRK2(mmm,ttt,2,:)))
					vals	:= ind(tmp%6.eq.0)
					vals2	:= ind(tmp%12.eq.0)
					if(ismissing(vals2(0)))then  vals2 := ind((tmp+6)%12.eq.0)  end if	;; extra step in case lead times are 6,18,30,42,...
					if(ismissing(vals2(0)) .or. ismissing(vals(0)))then   continue   end if
					if(dimsizes(vals).eq.1)then   continue   end if
					vals3	:= vals2(ind(.not.ismissing(aTRK2(mmm,ttt,0,vals2))))
			
				
					; Add each model as polyline & polymarker objects
					plres4a@gsLineColor	:= TRDcolors(ttt,:)
					plres4a@gsLineThicknessF= 10.0
					txares4a@txFontColor	:= TRDcolors(ttt,:)
					txares4a@txFontHeightF	= 0.005
					dum1(ttt)	= gsn_add_polyline(wks,plot,aTRK2(mmm,ttt,1,vals),aTRK2(mmm,ttt,0,vals),plres4a)
					if(DO_MARKERS)then
						mkres4a@gsMarkerSizeF   = 6.0
						mkres4a@gsMarkerColor   := TRDcolors(ttt,:)
						mkres4a@gsMarkerIndex   = TRDmarkers(ttt)
						mkres4a@gsMarkerThicknessF= 5.0
						dum2(ttt)	= gsn_add_polymarker(wks,plot,aTRK2(mmm,ttt,1,vals),aTRK2(mmm,ttt,0,vals),mkres4a)
					end if
					dum3(ttt,vals3)	= gsn_add_text(wks,plot,tostring(toint(aTRK2(mmm,ttt,2,vals2))),aTRK2(mmm,ttt,1,vals2)+0.2,aTRK2(mmm,ttt,0,vals2)+0.2,txares4a)
			
					; Add legend info for each model
					plres4a@gsLineThicknessF	= 3.5
					txares4a@txFontHeightF	= 0.009
					if(DO_MARKERS)then
						mkres4a@gsMarkerSizeF   = 8.0
						mkres4a@gsMarkerThicknessF= 5.0
						gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres4a)
					end if
					gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ttt/2)*0.25*vpWID),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.04/),\
							     (/vpTOP+0.0325-0.02*tofloat(ttt%2),vpTOP+0.0325-0.02*tofloat(ttt%2)/),plres4a)
					gsn_text_ndc(wks,IDATES(ttt),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ttt%2),txares4a)
				end do
		
		
				; Add a title,sub-title
				if(DO_TITLES)then
					txares4a@txFont		= 22
					txares4a@txFontHeightF	= 0.018
					txares4a@txFontColor	:= "black"
					txares4a@txJust		= "CenterLeft"
					gsn_text_ndc(wks,thisModel+" Late Track Trend",vpLFT,vpTOP+0.075,txares4a)
					txares4a@txFont		= 21
					txares4a@txFontHeightF	= 0.016
					gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+") valid on "+IDATE,vpLFT,vpTOP+0.055,txares4a)
				end if
			
				; Add disclaimer
				if(DO_DISCLAIMER)then	xloc = mpres4a@mpMinLonF + 0.02*(mpres4a@mpMaxLonF-mpres4a@mpMinLonF)
							yloc = mpres4a@mpMinLatF + 0.02*(mpres4a@mpMaxLatF-mpres4a@mpMinLatF)
							add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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
		
				; Mark that a trend graphic has been produced
				DO_LT_TREND = True
	
			end if ;DO_LT_TREND
			
		
	
			; ***********************************
			; PRODUCE LIFFETIME "SPAGHETTI"
			; TRACKS FOR THE CURRENT MODEL.
			; ***********************************
	
			; Go in here if producing lifetime track graphics
			if(DO_LT_LIFE2)then
	
				; Eventually make this its own option
				print("MSG: Producing LIFETIME TRACK graphic for "+thisModel)
			
				if(all(ismissing(aTRK3(mmm,:,0,:))))then	print("WARNING: "+thisModel+" track is unavailable for any cycles.")
										continue
				end if
		
				; Set up the output file
				OFILE	= ODIR+"AllTracks."+thisModel+"."+LONGSID+"."+IDATE
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks	:= gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues
					
				;Create a hurricane symbol marker
				tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
			
				; Set resources for plots and additional features
				;mpres4b@tiMainString		= MCODE+" Lifetime Tracks"
				if(1.1*rngLat3.lt.20.)then		mpres4b@gsnMajorLatSpacing = 2.
									mpres4b@gsnMinorLatSpacing = 1.
				else if(1.1*rngLat3.lt.75.)then		mpres4b@gsnMajorLatSpacing = 5.
									mpres4b@gsnMinorLatSpacing = 1.
				else					mpres4b@gsnMajorLatSpacing = 10.
									mpres4b@gsnMinorLatSpacing = 2.
				end if  end if
				if(1.1*rngLon3.lt.20.)then		mpres4b@gsnMajorLonSpacing = 2.
									mpres4b@gsnMinorLonSpacing = 1.
				else if(1.1*rngLon3.lt.75.)then		mpres4b@gsnMajorLonSpacing = 5.
									mpres4b@gsnMinorLonSpacing = 1.
				else					mpres4b@gsnMajorLonSpacing = 10.
									mpres4b@gsnMinorLonSpacing = 2.
				end if  end if
				mpres4b@mpMaxLonF		= maxLon3+(0.05*rngLon3)
				mpres4b@mpMinLonF		= minLon3-(0.05*rngLon3)
				mpres4b@mpMaxLatF		= maxLat3+(0.05*rngLat3)
				mpres4b@mpMinLatF		= minLat3-(0.05*rngLat3)
				mpres4b@mpCenterLonF		= avg((/mpres4b@mpMaxLonF,mpres4b@mpMinLonF/))
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
					; Check if the current model track is available for this cycle
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
				if(DO_TITLES)then
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
					gsn_text_ndc(wks,sprinti("%0.2i",dimsizes(ind(.not.ismissing(aTRK3(mmm,:,0,0)))))+" Forecasts thru "+IDATE,vpLFT,vpTOP+0.01,txares4b)
				end if
			
				; Add disclaimer
				if(DO_DISCLAIMER)then	xloc = mpres4b@mpMinLonF + 0.02*(mpres4b@mpMaxLonF-mpres4b@mpMinLonF)
							yloc = mpres4b@mpMinLatF + 0.02*(mpres4b@mpMaxLatF-mpres4b@mpMinLatF)
							add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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

				; Mark that a  fetime track graphic has been produced
				DO_LT_LIFE = True
	
			end if ;DO_LT_LIFE2
		end do ;mmm
	end if ;DO_LT_TREND .or. DO_LT_LIFE
end if ;DO_LT_TREND


;================================================================
; Produce late intensity trend graphics
;================================================================
if(DO_LI_TREND)then
	DO_LI_LIFE = DO_LI_TREND

	; ********************
	; FOR TREND GRAPHICS

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

	; Find intensity graphic Y limits.
	maxInt	:= max((/max(aINT2(:,:,0,:)),max(bINT(0,:))/))
	minInt	:= min((/min(aINT2(:,:,0,:)),min(bINT(0,:))/))
	if(ismissing(maxInt) .or. ismissing(minInt))then
		print("WARNING: Unable to retrieve intensity range for "+thisModel+".")
		DO_LI_TREND = False
	end if
	if(maxInt.eq.minInt)then
		rngInt := 50
	else if(ismissing(maxInt) .or. ismissing(minInt))
		rngInt := fval3
		rngInt@_FillValue = fval3
	else
		rngInt := abs(maxInt-minInt)
	end if  end if

	; Setup an array of intensity values
	if(1.2*rngInt.ge.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,20)
	else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,10)
	else if(1.2*rngInt.lt.50.)then
		IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
					 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,5)
	end if  end if  end if


	; ********************
	; FOR LIFETIME GRAPHICS

	; Update resources
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

	; Only show model maximum winds that match up with Best Track maximum winds.
	aINT3 = where(conform(aINT3,aINT3(:,:,1,:),(/0,1,3/)).gt.max(bHrs3), aINT3@_FillValue, aINT3)

	; Find plot Max/Min and major tick marks. Resize if necessary
	maxInt3	:= max((/max(aINT3(:,:,0,:)),max(bINT3(0,:))/))
	minInt3	:= min((/min(aINT3(:,:,0,:)),min(bINT3(0,:))/))
	if(ismissing(maxInt3) .or. ismissing(minInt3))then
		print("WARNING: Unable to retrieve intensity range for "+thisModel+".")
		continue
	end if
	if(maxInt3.eq.minInt3)then
		rngInt3 := 50
	else if(ismissing(maxInt) .or. ismissing(minInt))
		rngInt3 := fval3
		rngInt3@_FillValue = fval3
	else
		rngInt3 := abs(maxInt3-minInt3)
	end if  end if

	; Setup an array of intensity values
	if(1.2*rngInt3.ge.100.)then
		IntMjrGrd3 := ispan(toint(minInt3-(0.10*rngInt3)) - toint(minInt3-(0.10*rngInt3))%20 + 20,\
				    toint(maxInt3+(0.10*rngInt3)) - toint(maxInt3+(0.10*rngInt3))%20,20)
	else if(1.2*rngInt3.ge.50. .and. 1.2*rngInt3.lt.100.)then
		IntMjrGrd3 := ispan(toint(minInt3-(0.10*rngInt3)) - toint(minInt3-(0.10*rngInt3))%10 + 10,\
				    toint(maxInt3+(0.10*rngInt3)) - toint(maxInt3+(0.10*rngInt3))%10,10)
	else if(1.2*rngInt3.lt.50.)then
		IntMjrGrd3 := ispan(toint(minInt3-(0.10*rngInt3)) - toint(minInt3-(0.10*rngInt3))%5 + 5,\
				    toint(maxInt3+(0.10*rngInt3)) - toint(maxInt3+(0.10*rngInt3))%5,5)
	end if  end if  end if

	; Loop over selected models to produce 6-cycle trends & lifetime intensity graphics
	if(DO_LI_TREND .or. DO_LI_LIFE)then

		; Remember the original values for trend and lifetime graphics
		DO_LI_TREND2 = DO_LI_TREND
		DO_LI_LIFE2 = DO_LI_LIFE
		DO_LI_TREND = False
		DO_LI_LIFE = False

		do mmm = 0,dimsizes(INTmodelsT)-1

			; Define the current intensity trend model as 'thisModel'
			thisModel = INTmodelsT(mmm)


			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			; PART ONE --> INTENSITY TREND GRAPHICS ;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			; Go in here if producing trend graphics.
			if(DO_LI_TREND2)then
			
				; Print some information to the terminal
				print("MSG: Producing INTENSITY TREND graphic for "+thisModel)
				if(all(ismissing(aINT2(mmm,:,0,:))))then	print("WARNING: "+thisModel+" intensity is unavailable through "+IDATE+".")
										continue
				end if
				
				; Set up the output file
				OFILE	= ODIR+"IntensityTrend."+thisModel+"."+LONGSID+"."+IDATE
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks	= gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues
			
				;Create a hurricane symbol marker
				tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
		
				; Update resources
				;xyres5a@tiMainString	= thisModel+" Late Intensity Trend"
				xyres5a@tmYLValues	:= IntMjrGrd
				xyres5a@tmYLLabels	:= "" + xyres5a@tmYLValues
				xyres5a@tmYROn		= False
				xyres5a@trYMaxF		= maxInt+(0.10*rngInt)
				xyres5a@trYMinF		= max((/0.,minInt-(0.10*rngInt)/))
				xyres5a@xyLineThicknessF	= 15.0
				xyres5a@xyLineColor		:= "black"
				xyres5a@xyMarkLineMode	= "MarkLines"
				xyres5a@xyMarkers	= tc_marker
				xyres5a@xyMarkerColor	:= "red"
				xyres5a@xyMarkerThicknessF	= 10.0
				xyres5a@xyMarkerSizeF	= 0.020
		
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
					vals	:= ind(tmp%6.eq.0 .and. .not.ismissing(aINT2(mmm,ttt,0,:)))
		
					; Check if current model intensity is available
					if(all(ismissing(aINT2(mmm,ttt,0,:))) .or. dimsizes(vals).eq.1)then
						print("WARNING: "+thisModel+" intensity is unavailable for "+IDATES2(ttt)+".")
						continue
					end if
			
					; Update resources
					xyres5a@xyMarkLineMode	= "MarkLines"
					xyres5a@xyLineThicknessF= 12.0
					xyres5a@xyLineColor	:= TRDcolors(ttt,:)
					xyres5a@tiXAxisString	= ""
					xyres5a@tiYAxisString	= ""
					if(DO_MARKERS)then
						xyres5a@xyMarkLineMode  = "MarkLines"
						xyres5a@xyMarkers       := TRDmarkers(ttt)
						xyres5a@xyMarkerColor   := TRDcolors(ttt,:)
						xyres5a@xyMarkerThicknessF      = 4.0
						xyres5a@xyMarkerSizeF   = 0.01
					else
						xyres5a@xyMarkLineMode  = "Lines"
					end if
			
					; Overlay the new intensity line
					overlay(plot,gsn_csm_xy(wks,aINT2(mmm,ttt,1,vals),aINT2(mmm,ttt,0,vals),xyres5a))
			
					; Add legend info for each model
					plres5a@gsLineColor	:= TRDcolors(ttt,:)
					plres5a@gsLineThicknessF	= 3.5
					plres5a@gsLineDashPattern	= 0
					plres5a@tfPolyDrawOrder	= "PostDraw"
					
					txares5a@txFontColor	:= TRDcolors(ttt,:)
					txares5a@txFontHeightF	= 0.009
		
					if(DO_MARKERS)then
						mkres5a@gsMarkerSizeF   = 8.0
						mkres5a@gsMarkerColor   := TRDcolors(ttt,:)
						mkres5a@gsMarkerIndex   = TRDmarkers(ttt)
						mkres5a@gsMarkerThicknessF= 5.0
						gsn_polymarker_ndc(wks,vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.02,vpTOP+0.0325-0.02*tofloat(ttt%2),mkres5a)
					end if
					gsn_polyline_ndc(wks,(/vpLFT+(tofloat(ttt/2)*0.25*vpWID),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.04/),\
							     (/vpTOP+0.0325-0.02*tofloat(ttt%2),vpTOP+0.0325-0.02*tofloat(ttt%2)/),plres5a)
					gsn_text_ndc(wks,IDATES2(ttt),vpLFT+(tofloat(ttt/2)*0.25*vpWID)+0.045,vpTOP+0.0325-0.02*tofloat(ttt%2),txares5a)
				end do ;ttt
		
				; Add a title,sub-title
				if(DO_TITLES)then
					txares5a@txFont		= 22
					txares5a@txFontHeightF	= 0.018
					txares5a@txFontColor	:= "black"
					txares5a@txJust		= "CenterLeft"
					gsn_text_ndc(wks,thisModel+" Late Intensity Trend",vpLFT,vpTOP+0.075,txares5a)
					txares5a@txFont		= 21
					txares5a@txFontHeightF	= 0.016
					gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+") initialized on "+IDATE,vpLFT,vpTOP+0.052,txares5a)
				end if
			
				; Add disclaimer
				if(DO_DISCLAIMER)then	xloc = xyres5a@trXMinF + 0.02*(xyres5a@trXMaxF-xyres5a@trXMinF)
							yloc = xyres5a@trYMinF + 0.02*(xyres5a@trYMaxF-xyres5a@trYMinF)
							add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
				end if
		
				; Finish graphic
				draw(plot)
				frame(wks)
		
				; Remove white space from graphic
				;if(DO_RMWHITE)then   system("convert -trim "+OFILE+".png "+OFILE+".png")   end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
				
				; Mark that a trend graphic has been produced
				DO_LI_TREND = True
			
			end if ;DO_LI_TREND2


			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			; PART TWO --> INTENSITY LIFETIME GRAPHICS ;
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			if(DO_LI_LIFE2)then	
		
				; Produce lifetime "spaghetti" intensity graphic for the current intensity trend model.
				print("MSG: Producing LIFETIME INTENSITY graphic for "+thisModel)
				if(all(ismissing(aINT3(mmm,:,0,:))))then	print("WARNING: "+thisModel+" intensity is unavailable for any cycles.")
										continue
				end if
		
				; Set up the output file
				OFILE	= ODIR+"AllIntensity."+thisModel+"."+LONGSID+"."+IDATE
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks	:= gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues
					
				;Create a hurricane symbol marker
				tc_marker := NhlNewMarker(wks,"p",137,0.0,0.0,1.0,1.0,0.0)
			
				; Update resources
				xyres5b@tmYLMode	= "Explicit"
				xyres5b@tmYLValues	:= IntMjrGrd3
				xyres5b@tmYLLabels	:= "" + xyres5b@tmYLValues
				xyres5b@tmYROn		= False
				xyres5b@tmYRValues	:= IntMjrGrd3
				xyres5b@tmYRLabels	:= "" + xyres5b@tmYRValues
				xyres5b@trYMaxF		= maxInt3+(0.10*rngInt3)
				xyres5b@trYMinF		= max((/0.,minInt3-(0.10*rngInt3)/))
				xyres5b@trYMaxF		= maxInt3+(0.10*rngInt3)
				xyres5b@trYMinF		= max((/0.,minInt3-(0.10*rngInt3)/))
		
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
				dumB	:= new(dimsizes(IntMjrGrd3),"graphic")
			
				; Draw vertical grey lines at major forecast hours
				; Draw horizontal grey lines at major intensity levels
				plres5b@tfPolyDrawOrder	= "PreDraw"
				plres5b@gsLineThicknessF= 1.5
				plres5b@gsLineDashPattern= 11
				plres5b@gsLineColor	:= "grey70"
				do fff = 0,dimsizes(bHrs3)-1
					dumA(fff)	= gsn_add_polyline(wks,plot,(/tofloat(bHrs3(fff)),tofloat(bHrs3(fff))/),(/xyres5b@trYMinF,xyres5b@trYMaxF/),plres5b)
				end do
				do fff = 0,dimsizes(IntMjrGrd3)-1
					dumB(fff)	= gsn_add_polyline(wks,plot,(/xyres5b@trXMinF,xyres5b@trXMaxF/),(/IntMjrGrd3(fff),IntMjrGrd3(fff)/),plres5b)
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
					vals	:= ind(tmp%6.eq.0 .and. .not.ismissing(aINT3(mmm,ttt,0,:)))
					vals2	:= ind(aINT3(mmm,ttt,1,:).ge.0 .and. tmp%6.eq.0 .and. .not.ismissing(aINT3(mmm,ttt,0,:)))
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
				if(DO_TITLES)then
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
				end if
			
				; Add disclaimer
				if(DO_DISCLAIMER)then	xloc = xyres5b@trXMinF + 0.02*(xyres5b@trXMaxF-xyres5b@trXMinF)
							yloc = xyres5b@trYMinF + 0.02*(xyres5b@trYMaxF-xyres5b@trYMinF)
							add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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

			end if ; DO_LI_LIFE2
		end do ;mmm
	end if ; DO_LI_TREND .or. DO_LI_LIFE
end if ;DO_LI_TREND



;================================================================
; Produce Late MSLP forecast trend graphics.
;================================================================
if(DO_LP_TREND)then

	print("MSG: Producing LATE PRESSURE TREND graphic.")
	print("WARNING: Under construction.")

	; Find map boundaries. Resize if necessary
	maxInt	:= max((/max(aPRS2(:,0,:)),max(bINT(1,:))/))
	minInt	:= min((/min(aPRS2(:,0,:)),min(bINT(1,:))/))
	if(ismissing(maxInt) .or. ismissing(minInt) .or. maxInt.eq.minInt)then
		print("WARNING: Early model intensity guidance can't be produced.")
		DO_THIS = False
		DO_LP_TREND = False
	end if
	

	if(DO_THIS)then

		rngInt	:= abs(maxInt-minInt)
		if(1.2*rngInt.ge.100.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%20 + 20,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%20,20)
		else if(1.2*rngInt.ge.50. .and. 1.2*rngInt.lt.100.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%10 + 10,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%10,10)
		else if(1.2*rngInt.lt.50.)then
			IntMjrGrd	:= ispan(toint(minInt-(0.10*rngInt)) - toint(minInt-(0.10*rngInt))%5 + 5,\
						 toint(maxInt+(0.10*rngInt)) - toint(maxInt+(0.10*rngInt))%5,5)
		end if  end if  end if
	

		; Set up the output file
		OFILE	= ODIR+"PressureTrend."+MCODE+"."+LONGSID+"."+IDATE
		if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
		wks	= gsn_open_wks(wtype,OFILE)
                setvalues NhlGetWorkspaceObjectId()
                        "wsMaximumSize" : 500000000
                end setvalues

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
		res@xyMarkers = tc_marker
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


		; Add a title,sub-title
		if(DO_TITLES)then
			txares5a@txFont		= 22
			txares5a@txFontHeightF	= 0.018
			txares5a@txFontColor	:= "black"
			txares5a@txJust		= "CenterLeft"
			gsn_text_ndc(wks,thisModel+" Late Pressure Trend",vpLFT,vpTOP+0.075,txares5a)
			txares5a@txFont		= 21
			txares5a@txFontHeightF	= 0.016
			gsn_text_ndc(wks,TC_NAME+" ("+str_upper(SID)+") initialized on "+IDATE,vpLFT,vpTOP+0.052,txares5a)
		end if
	
		; Add disclaimer
		if(DO_DISCLAIMER)then	xloc = xyres5a@trXMinF + 0.02*(xyres5a@trXMaxF-xyres5a@trXMinF)
					yloc = xyres5a@trYMinF + 0.02*(xyres5a@trYMaxF-xyres5a@trYMinF)
					add_disclaimer(wks,plot,EXPT,xloc,yloc,0.009,"left")
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

	end if ;DO_THIS

end if ;DO_LP_TREND


end if ;DO_TRENDS




;================================================================;
;===========================STEP FOUR============================;
;================================================================;
;           PRODUCE GRAPHICAL VERIFICATION PRODUCTS              ;
;================================================================;

if(DO_VERIFICATION)then

	print("")
	print("MSG: *************STEP FIVE************")
	print("MSG: FORECAST VERIFICATION")
	print("MSG: Produce verification and associated graphics using MET-TC.")
	print("MSG: Using NHC verification rules, which state that a storm")
	print("MSG: must be a TC at the initial time and the verification")
	print("MSG: time (HU,TS,TD,SS,SD).")
	
        ; Update the status file.
        system("lockfile -r-1 -l 180 "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
        system("echo 'working' > "+STATUS_FILE)
        system("rm -f "+LOCK_FILE)
        print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")


	; 1. Set up variables
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
	TCP_WRAPPER	= ODIR+"/mettc/tc_pairs_wrapper."+SID+"."+IDATE+".sh"
	TCF_WRAPPER	= ODIR+"/mettc/tc_statf_wrapper."+SID+"."+IDATE+".sh"
	TCS_WRAPPER	= ODIR+"/mettc/tc_stats_wrapper."+SID+"."+IDATE+".sh"
	MET_PATH	= "/lfs1/projects/dtc-hurr/MET/MET_releases/8.0/bin"
	MET_LIB		= "/lfs1/projects/dtc-hurr/MET/MET_releases/external_libs/lib"
	SH_SHEBANG	= "#!/bin/sh"


	; 2. set up files and directories
	system("mkdir -p "+ODIR+"mettc/")
	system("cp "+GPLOT_DIR+"/parm/mettc/config/TCPairsConfig_match "+TCPconf)
	if(.not.fileexists(TCPconf))then	print("ERROR: TC_PAIRS config file not found.")
						exit
	end if
	system("cp "+GPLOT_DIR+"/parm/mettc/config/TCStatConfig_filter "+TCSconf_f)
	if(.not.fileexists(TCSconf_f))then	print("ERROR: TC_STAT filter config file not found.")
						exit
	end if
	system("cp "+GPLOT_DIR+"/parm/mettc/config/TCStatConfig_summary "+TCSconf_s)
	if(.not.fileexists(TCSconf_s))then	print("ERROR: TC_STAT summary config file not found.")
						exit
	end if


	; 3. MET-TC expression for EARLY TRACK models
	etM_exp	= "["
	nnn = 0
	do mmm = 0,dimsizes(etModels)-1
		if(etModels(mmm).eq."")then continue end if
		if(nnn.gt.0)then etM_exp = etM_exp+"," end if
		etM_exp	= etM_exp+"\"+str_get_dq()+etModels(mmm)+"\"+str_get_dq()
		nnn = nnn+1
	end do
	etM_exp	= etM_exp+"];"


	; 4. MET-TC expression for EARLY INTENSITY models
	eiM_exp	= "["
	nnn = 0
	do mmm = 0,dimsizes(eiModels)-1
		if(eiModels(mmm).eq."")then continue end if
		if(nnn.gt.0)then eiM_exp = eiM_exp+"," end if
		eiM_exp	= eiM_exp+"\"+str_get_dq()+eiModels(mmm)+"\"+str_get_dq()
		nnn = nnn+1
	end do
	eiM_exp	= eiM_exp+"];"


	; 5. MET_TC expression for LATE TRACK models
	ltM_exp	= "["
	nnn = 0
	do mmm = 0,dimsizes(ltModels)-1
		if(ltModels(mmm).eq."")then continue end if
		if(nnn.gt.0)then ltM_exp = ltM_exp+"," end if
		ltM_exp	= ltM_exp+str_get_dq()+ltModels(mmm)+str_get_dq()
		nnn = nnn+1
	end do
	ltM_exp	= ltM_exp+"];"


	; 6. MET_TC expression for LATE INTENSITY models
	liM_exp	= "["
	nnn = 0
	do mmm = 0,dimsizes(liModels)-1
		if(liModels(mmm).eq."")then continue end if
		if(nnn.gt.0)then liM_exp = liM_exp+"," end if
		liM_exp	= liM_exp+str_get_dq()+liModels(mmm)+str_get_dq()
		nnn = nnn+1
	end do
	liM_exp	= liM_exp+"];"

	; 7. Setup other critical elements of the MET-TC configuration.
	initEnd		= str_get_dq()+changeTimeFmt(IDATE,"YYYYMMDDHH","YYYYMMDD_HH",0.)+str_get_dq()+";"
	summJob		= "jobs = ["+str_get_dq()+"-job summary -column TRACK -column WIND -column TI -column AMAX_WIND-BMAX_WIND -by AMODEL"+str_get_dq()+"];"
	NHCStrName	= "["+str_get_dq()+"LEVEL"+str_get_dq()+","+str_get_dq()+"LEVEL"+str_get_dq()+"," \
			 +""+str_get_dq()+"LEVEL"+str_get_dq()+","+str_get_dq()+"LEVEL"+str_get_dq()+"," \
			 +""+str_get_dq()+"LEVEL"+str_get_dq()+"];"
	NHCStrVal	= "["+str_get_dq()+"HU"+str_get_dq()+","+str_get_dq()+"TS"+str_get_dq()+"," \
			 +""+str_get_dq()+"TD"+str_get_dq()+","+str_get_dq()+"SS"+str_get_dq()+"," \
			 +""+str_get_dq()+"SD"+str_get_dq()+"];"


	; 8. Set basic plotting resources
	res			:= True
	res@gsnFrame		= False
	res@gsnDraw		= False
	res@gsnMaximize		= False
	res@xyDashPattern	= 0
	if(DO_MARKERS)then	res@xyMarkLineMode = "MarkLines"	; Markers *and* lines
	else			res@xyMarkLineMode = "Lines"		; Lines only (no markers)
	end if
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
	res@tmXBValues		:= XYHrs
	res@tmXBLabels		= ""+sprinti("%0.2i",XYHrs)
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
	res@tiXAxisString	= "Lead Time [h]"
	res@gsnRightString	= ""+str_upper(LONGSID)
	res@gsnLeftString	= "Valid through "+IDATE
	;res@vpWidthF		= 0.6				; Width of the viewport
	res@vpHeightF		= 0.4				; Height of the viewport
	
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
	

	; 9. Verify LATE TRACK models.
	;    Besides the model you chose (MCODE), the rest of the models
	;    can be controlled in the master namelist.
	if(DO_LT_VER)then

		; Get model information
		LTcolors    = getModelInfo(ltModels,"color")
		LTmarkers   = getModelInfo(ltModels,"marker")
		LTlongname  = getModelInfo(ltModels,"long_name")
		
		; a. Run the MET-TC program 'tc_pairs'
		print("MSG: Running TC_PAIRS for late track models.")
		system("sed -i -e 's/^model =.*/model = "+ltM_exp+"/g' "+TCPconf)
		system("sed -i 's/^init_end =.*/init_end = "+initEnd+"/g' "+TCPconf)
		
		; b. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+\
		       " -out "+TCPout3+" -log "+TCPout3+".log"+str_get_dq()+" >> "+TCP_WRAPPER)
		system("chmod +x "+TCP_WRAPPER)
		system(TCP_WRAPPER+" >> "+str_sub_str(TCP_WRAPPER,".sh",".log"))

		; c. Run TC_STAT in filter mode
		print("MSG: Running TC_STAT in filter mode for late track models.")
		filtJob	= "jobs = ["+str_get_dq()+"-job filter -dump_row "+TCSfout3+".tcst"+str_get_dq()+"];"
		system("sed -i 's/^amodel =.*/amodel = "+ltM_exp+"/g' "+TCSconf_f)
		system("sed -i 's@^jobs = .*@"+filtJob+"@g' "+TCSconf_f)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

		; d. Create shell script to run TC_STAT with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout3+".tcst -config "+TCSconf_f+str_get_dq()+\
		       " >> "+TCF_WRAPPER)
		system("chmod +x "+TCF_WRAPPER)
		system(TCF_WRAPPER+" >> "+str_sub_str(TCF_WRAPPER,".sh",".log"))

		; e. Run TC_STAT in summary mode
		print("MSG: Running TC_STAT in summary mode for late track models. Will loop over all lead times.")
		system("sed -i 's/^amodel =.*/amodel = "+ltM_exp+"/g' "+TCSconf_s)
		system("sed -i 's/^jobs =.*/"+summJob+"/g' "+TCSconf_s)
		TK_ERR		:= new((/dimsizes(ltModels),dimsizes(XYHrs)/),"float",fval3)
		ALTK_ERR	:= TK_ERR
		CRTK_ERR	:= TK_ERR
		N_ERR		:= TK_ERR
		do fff = 0,dimsizes(XYHrs)-1
			;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
			
			; Set critical parameters for the MET-TC configuration
			tmp	:= TCSsout3+".f"+sprinti("%0.3i",XYHrs(fff))+".tcst"
			tmp2	:= TCSsout3+".f"+sprinti("%0.3i",XYHrs(fff))+".log"
			system("sed -i 's/lead       =.*/lead       = ["+str_get_dq()+sprinti("%0.2i",XYHrs(fff))+"0000"+str_get_dq()+"];/g' "+TCSconf_s)
	
			; Create shell script to run TC_STAT with shell options (better)
			system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout3+".tcst -config "+TCSconf_s+\
			       " -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+TCS_WRAPPER)
			system("chmod +x "+TCS_WRAPPER)
			system(TCS_WRAPPER+" >> "+str_sub_str(TCS_WRAPPER,".sh",".log"))
	
			; Read and categorize the output of the TC-STAT output
			data	:= asciiread(tmp,-1,"string")
			errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
			vals	:= ind(isStrSubset2(data,"SUMMARY:"))
			if(ismissing(vals(0)))then continue end if
			data	:= data(vals)
			errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
			errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
			
			; Find track errors, along-track errors, and cross-track errors for each late track model.
			do mmm = 0,dimsizes(ltModels)-1
				vals	:= ind(errModels.eq.ltModels(mmm))
				if(ismissing(vals(0)))then continue end if
				if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR"))))then
					continue
				else
					TK_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."MEAN")+1," ")))
					N_ERR(mmm,fff)	= tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."TK_ERR")),ind(errCols.eq."TOTAL")+1," ")))
				end if
				if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."ALTK_ERR"))))then
					continue
				else
					ALTK_ERR(mmm,fff) = tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."ALTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
				end if
				if(ismissing(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."CRTK_ERR"))))then
					continue
				else
					CRTK_ERR(mmm,fff) = tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.ltModels(mmm) .and. errFields.eq."CRTK_ERR")),ind(errCols.eq."MEAN")+1," ")))
				end if
			end do
			
		end do

		
		; f. Produce the graphic for late track models (TK_ERR, ALTK_ERR, CRTK_ERR).
		;    Need to check if the track errors are missing.
		FILE_STR := (/"TK_ERR","ALTK_ERR","CRTK_ERR"/)
		TITLE_STR := (/"LATE TRACK ERROR", "LATE ALONG TRACK ERROR", "LATE CROSS TRACK ERROR"/)
		do ddd = 0,dimsizes(FILE_STR)-1

			; Get the correct data
			if(ddd.eq.0)then	ERR := TK_ERR
			else if(ddd.eq.1)then	ERR := ALTK_ERR
			else if(ddd.eq.2)then	ERR := CRTK_ERR
			end if  end if  end if

			DO_THIS = True
			vals := ind(.not.ismissing(ERR(0,:)))
			vals2 := ind(ltModels.ne."")
			if(ismissing(vals(0)))then		DO_THIS = False
			else if(dimsizes(vals).le.1)then	DO_THIS = False
			else if(ismissing(vals2(0)))then	DO_THIS = False
			end if  end if  end if
			if(DO_THIS)then
				print("MSG: Producing graphics for late track model verifications ("+FILE_STR(ddd)+").")
	
				; Setup the file, open the workstation, and update resources.
				OFILE	:= ODIR+"/Verification."+FILE_STR(ddd)+"."+LONGSID+"."+IDATE+".late"
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks = gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues

				res@xyLineColors	:= LTcolors
				res@xyMarkers		:= LTmarkers
				res@xyMarkerColors	:= LTcolors
				if(DO_TITLES)then   res@tiMainString = TITLE_STR(ddd)   end if
				res@tiYAxisString	= "Track Error"+" [n mi]"
				res@xyExplicitLegendLabels := " "+ltModels
				res@pmLegendHeightF	= 0.03*tofloat(dimsizes(ltModels))
				if(min(ERR).lt.0.)then	res@trYMinF = floor(min(ERR) - 0.1*(max(ERR)-min(ERR)))
							res@trYMaxF = ceil(max(ERR) + 0.1*(max(ERR)-min(ERR)))
				else			res@trYMinF = 0.
							res@trYMaxF = ceil(1.1*max(ERR))
				end if
			
				; Draw the main line plot on the graphic.
				plot = gsn_csm_xy(wks,tofloat(XYHrs(vals)),ERR(vals2,vals),res)
	
				; Add the number of cases to the graphic.
				dum1	:= new(dimsizes(XYHrs),"graphic")
				do fff = 0,dimsizes(XYHrs)-1
					if(.not.ismissing(N_ERR(0,fff)))then
						dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(XYHrs(fff)),\
								         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
					end if
				end do
	
				; Add disclaimer to the graphic
				if(DO_DISCLAIMER)then	XLOC = min(tofloat(XYHrs(vals)))+0.02*(max(tofloat(XYHrs(vals)))-min(tofloat(XYHrs(vals))))
							YLOC = res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF)
							add_disclaimer(wks,plot,EXPT,XLOC,YLOC,0.015,"left")
				end if
	
				; ACTUALLY PRODUCE THE GRAPHIC HERE
				draw(plot)
				frame(wks)
	
				; Remove white space from the border of the graphic.
				;if(DO_RMWHITE)then system("convert -trim "+OFILE+".png "+OFILE+".png") end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
	
			end if ;DO_THIS

		end do

	end if ;DO_LT_VER


	; 10. Verify LATE INTENSITY models.
	;     Besides the model you chose (MCODE), the rest of the models
	;     can be controlled in the master namelist.
	if(DO_LI_VER)then

	        ; Update the status file.
	        system("lockfile -r-1 -l 180 "+LOCK_FILE)
	        print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
	        system("echo 'working' > "+STATUS_FILE)
	        system("rm -f "+LOCK_FILE)
	        print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

		; Get model information
		LIcolors    = getModelInfo(liModels,"color")
		LImarkers   = getModelInfo(liModels,"marker")
		LIlongname  = getModelInfo(liModels,"long_name")

		; a. Run TC_PAIRS
		print("MSG: Running TC_PAIRS for late intensity models.")
		system("sed -i 's/^model = .*/model = "+liM_exp+"/g' "+TCPconf)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

		; b. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+TCPconf+\
		       " -out "+TCPout4+" -log "+TCPout4+".log"+str_get_dq()+" >> "+TCP_WRAPPER)
		system("chmod +x "+TCP_WRAPPER)
		system(TCP_WRAPPER+" >> "+str_sub_str(TCP_WRAPPER,".sh",".log"))

		; c. Run TC_STAT in filter mode
		print("MSG: Running TC_STAT in filter mode for late intensity models.")
		filtJob	= "jobs = ["+str_get_dq()+"-job filter -dump_row "+TCSfout4+".tcst"+str_get_dq()+"];"
		system("sed -i 's/^amodel = .*/amodel = "+liM_exp+"/g' "+TCSconf_f)
		system("sed -i 's@^jobs = .*@"+filtJob+"@g' "+TCSconf_f)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

		; d. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout4+".tcst -config "+TCSconf_f+\
		       str_get_dq()+" >> "+TCF_WRAPPER)
		system("chmod +x "+TCF_WRAPPER)
		system(TCF_WRAPPER+" >> "+str_sub_str(TCF_WRAPPER,".sh",".log"))

		; e. Run TC_STAT in summary mode
		print("MSG: Running TC_STAT in summary mode for late intensity models.")
		system("sed -i 's/^amodel = .*/amodel = "+liM_exp+"/g' "+TCSconf_s)
		system("sed -i 's/^jobs = .*/"+summJob+"/g' "+TCSconf_s)
		WIND_ERR	:= new((/dimsizes(liModels),dimsizes(XYHrs)/),"float",fval3)
		ABS_WIND_ERR	:= WIND_ERR
		N_ERR		:= WIND_ERR
		do fff = 0,dimsizes(XYHrs)-1
			;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
			
			; Set critical parameters for the MET-TC configuration.
			tmp	:= TCSsout4+".f"+sprinti("%0.3i",XYHrs(fff))+".tcst"
			tmp2	:= TCSsout4+".f"+sprinti("%0.3i",XYHrs(fff))+".log"
			system("sed -i 's/lead       = .*/lead       = ["+str_get_dq()+sprinti("%0.2i",XYHrs(fff))+"0000"+str_get_dq()+"];/g' "+TCSconf_s)
			
			; Create a shell script to run TC_STAT with shell options.
			system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout4+".tcst -config "+TCSconf_s+\
			       " -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+TCS_WRAPPER)
			system("chmod +x "+TCS_WRAPPER)
			system(TCS_WRAPPER+" >> "+str_sub_str(TCS_WRAPPER,".sh",".log"))
	
			; Read and categorize the output of the TC-STAT output
			data	:= asciiread(tmp,-1,"string")
			errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
			vals	:= ind(isStrSubset2(data,"SUMMARY:"))
			if(ismissing(vals(0)))then continue end if
			data	:= data(vals)
			errModels := str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
			errFields := str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
			; Find intensity error and absolute intensity error for each late intensity model.
			do mmm = 0,dimsizes(liModels)-1
				vals	:= ind(errModels.eq.liModels(mmm))
				if(ismissing(vals(0)))then continue end if
				if(ismissing(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)"))))then
					continue
				else
					ABS_WIND_ERR(mmm,fff) = tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."ABS(AMAX_WIND-BMAX_WIND)")),ind(errCols.eq."MEAN")+1," ")))
				end if
				if(ismissing(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND"))))then
					continue
				else
					WIND_ERR(mmm,fff) = tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."MEAN")+1," ")))
					N_ERR(mmm,fff)	  = tofloat(str_squeeze(str_get_field(data(ind(errModels.eq.liModels(mmm) .and. errFields.eq."AMAX_WIND-BMAX_WIND")),ind(errCols.eq."TOTAL")+1," ")))
				end if
			end do
		end do


		; Produce graphics for late intensity bias (WIND_ERR) and late absolute intensity error (ABS_WIND_ERR)
		FILE_STR := (/"WIND_ERR","ABS_WIND_ERR"/)
		TITLE_STR := (/"LATE INTENSITY BIAS", "LATE ABSOLUTE INTENSITY ERROR"/)
		do ddd = 0,dimsizes(FILE_STR)-1

			print("MSG: Producing graphic for late intensity model verification ("+FILE_STR(ddd)+").")

			if(ddd.eq.0)then	ERR := WIND_ERR
			else if(ddd.eq.1)then	ERR := ABS_WIND_ERR
			end if  end if
			
			; f. Produce graphic for late absolute intensity error
			DO_THIS = True
			vals := ind(.not.ismissing(ERR(0,:)))
			vals2 := ind(liModels.ne."")
			if(ismissing(vals(0)))then		DO_THIS = False
			else if(dimsizes(vals).le.1)then	DO_THIS = False
			else if(ismissing(vals2(0)))then	DO_THIS = False
			end if  end if  end if
			if(DO_THIS)then
	
				; Setup the file, open the workstation, and update resources.
				OFILE	:= ODIR+"/Verification."+FILE_STR(ddd)+"."+LONGSID+"."+IDATE+".late"
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks = gsn_open_wks(wtype,OFILE)
		                setvalues NhlGetWorkspaceObjectId()
		                        "wsMaximumSize" : 500000000
		                end setvalues
				res@xyLineColors	:= LIcolors
				res@xyMarkers		:= LImarkers
				res@xyMarkerColors	:= LIcolors
				if(DO_TITLES)then   res@tiMainString = TITLE_STR(ddd)   end if
				res@tiYAxisString	= "Intensity Error [kt]"
				res@tiXAxisString	= "Lead Time [h]"
				res@gsnRightString	= ""+str_upper(LONGSID)
				res@gsnLeftString	= "Valid through "+IDATE
				res@xyExplicitLegendLabels := " "+liModels(ind(liModels.ne.""))
				res@pmLegendHeightF	= 0.03*tofloat(dimsizes(liModels(ind(liModels.ne.""))))
				if(min(ERR).lt.0.)then	res@trYMinF = floor(min(ERR) - 0.1*(max(ERR)-min(ERR)))
							res@trYMaxF = ceil(max(ERR) + 0.1*(max(ERR)-min(ERR)))
				else			res@trYMinF = 0.
							res@trYMaxF = ceil(1.1*max(ERR))
				end if
	
				; Draw the main line plot on the graphic.
				plot = gsn_csm_xy(wks,tofloat(XYHrs(vals)),ERR(vals2,vals),res) ; create plot
	
				; Add the number of cases to the graphic.
				dum1 := new(dimsizes(XYHrs),"graphic")
				do fff = 0,dimsizes(XYHrs)-1
					if(.not.ismissing(N_ERR(0,fff)))then
						dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(XYHrs(fff)),\
								         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
					end if
				end do
	
				; Add disclaimer to the graphic.
				if(DO_DISCLAIMER)then	XLOC = min(tofloat(XYHrs(vals)))+0.02*(max(tofloat(XYHrs(vals)))-min(tofloat(XYHrs(vals))))
							YLOC = res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF)
							add_disclaimer(wks,plot,EXPT,XLOC,YLOC,0.015,"left")
				end if
	
				; ACTUALLY PRODUCE THE GRAPHIC HERE
				draw(plot)
				frame(wks)
	
				; Remove the white space from the border of the graphic.
				;if(DO_RMWHITE)then system("convert -trim "+OFILE+".png "+OFILE+".png") end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
	
			end if ;DO_THIS

		end do ;WIND_ERR, ABS_WIND_ERR

	end if ;DO_LI_VER


	; 11. Verify EARLY TRACK models.
	;     Besides the model you chose (MCODEI), the rest of the models
	;     can be controlled in the master namelist.
	if(DO_ET_VER)then

                ; Update the status file.
                system("lockfile -r-1 -l 180 "+LOCK_FILE)
                print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
                system("echo 'working' > "+STATUS_FILE)
                system("rm -f "+LOCK_FILE)
                print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

		; Get model information
		ETcolors    = getModelInfo(etModels,"color")
		ETmarkers   = getModelInfo(etModels,"marker")
		ETlongname  = getModelInfo(etModels,"long_name")
		
		; a. Run TC_PAIRS
		print("MSG: Running TC_PAIRS for early track models.")
		system("sed -i 's/^model = .*/model = "+etM_exp+"/g' "+TCPconf)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

		; b. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+\
		       TCPconf+" -out "+TCPout1+" -log "+TCPout1+".log"+str_get_dq()+" >> "+TCP_WRAPPER)
		system("chmod +x "+TCP_WRAPPER)
		system(TCP_WRAPPER+" >> "+str_sub_str(TCP_WRAPPER,".sh",".log"))

		; c. Run TC_STAT in filter mode
		print("MSG: Running TC_STAT in filter mode for early track models.")
		filtJob	= "jobs = ["+str_get_dq()+"-job filter -dump_row "+TCSfout1+".tcst"+str_get_dq()+"];"
		system("sed -i 's/^amodel = .*/amodel = "+etM_exp+"/g' "+TCSconf_f)
		system("sed -i 's@^jobs = .*@"+filtJob+"@g' "+TCSconf_f)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

		; d. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout1+".tcst -config "+TCSconf_f+\
		       str_get_dq()+" >> "+TCF_WRAPPER)
		system("chmod +x "+TCF_WRAPPER)
		system(TCF_WRAPPER+" >> "+str_sub_str(TCF_WRAPPER,".sh",".log"))

		; e. Run TC_STAT in summary mode
		print("MSG: Running TC_STAT in summary mode for early track models.")
		system("sed -i 's/^amodel = .*/amodel = "+etM_exp+"/g' "+TCSconf_s)
		system("sed -i 's/^jobs = .*/"+summJob+"/g' "+TCSconf_s)
		TK_ERR		:= new((/dimsizes(etModels),dimsizes(XYHrs)/),"float",fval3)
		ALTK_ERR	:= TK_ERR
		CRTK_ERR	:= TK_ERR
		N_ERR		:= TK_ERR
		do fff = 0,dimsizes(XYHrs)-1
			;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
			
			; Set critical parameters for the MET-TC configuration.
			tmp	:= TCSsout1+".f"+sprinti("%0.3i",XYHrs(fff))+".tcst"
			tmp2	:= TCSsout1+".f"+sprinti("%0.3i",XYHrs(fff))+".log"
			system("sed -i 's/^lead       = .*/lead       = ["+str_get_dq()+sprinti("%0.3i",XYHrs(fff))+"0000"+str_get_dq()+"];/g' "+TCSconf_s)
	
			; Create a shell script to run TC_STAT with shell options.
			system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout1+".tcst -config "+TCSconf_s+" -out "+\
			       tmp+" -log "+tmp2+str_get_dq()+" >> "+TCS_WRAPPER)
			system("chmod +x "+TCS_WRAPPER)
			system(TCS_WRAPPER+" >> "+str_sub_str(TCS_WRAPPER,".sh",".log"))
	
			; Read and categorize the output of the TC-STAT output.
			data	:= asciiread(tmp,-1,"string")
			errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
			vals	:= ind(isStrSubset2(data,"SUMMARY:"))
			if(ismissing(vals(0)))then continue end if
			data	:= data(vals)
			errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
			errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
			; Find track error, along-track error, and cross-track error for each early track model.
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


		; f. Produce the graphic for late track models (TK_ERR, ALTK_ERR, CRTK_ERR).
		;    Need to check if the track errors are missing.
		FILE_STR := (/"TK_ERR","ALTK_ERR","CRTK_ERR"/)
		TITLE_STR := (/"EARLY TRACK ERROR", "EARLY ALONG TRACK ERROR", "EARLY CROSS TRACK ERROR"/)
		do ddd = 0,dimsizes(FILE_STR)-1

			; Get the correct data
			if(ddd.eq.0)then	ERR := TK_ERR
			else if(ddd.eq.1)then	ERR := ALTK_ERR
			else if(ddd.eq.2)then	ERR := CRTK_ERR
			end if  end if  end if

			print("MSG: Producing graphics for early track model verification ("+FILE_STR(ddd)+").")
			
			; f. Produce graphic for early track errors.
			DO_THIS = True
			vals := ind(.not.ismissing(TK_ERR(0,:)))
			vals2 := ind(etModels.ne."")
			if(ismissing(vals(0)))then		DO_THIS = False
			else if(dimsizes(vals).le.1)then	DO_THIS = False
			else if(ismissing(vals2(0)))then	DO_THIS = False
			end if  end if  end if
			if(DO_THIS)then	
				; Setup the file, open the workstation, and update resources.
				OFILE	:= ODIR+"/Verification."+FILE_STR(ddd)+"."+LONGSID+"."+IDATE+".early"
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks = gsn_open_wks(wtype,OFILE)
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				res@xyLineColors	:= ETcolors
				res@xyMarkers		:= ETmarkers
				res@xyMarkerColors	:= ETcolors
				if(DO_TITLES)then   res@tiMainString = "EARLY TRACK ERROR"   end if
				res@tiYAxisString	= "Track Error"+" [n mi]"
				res@xyExplicitLegendLabels := " "+etModels
				res@pmLegendHeightF	= 0.03*tofloat(dimsizes(etModels(ind(etModels.ne.""))))
				if(min(ERR).lt.0.)then	res@trYMinF = floor(min(ERR) - 0.1*(max(ERR)-min(ERR)))
								res@trYMaxF = ceil(max(ERR) + 0.1*(max(ERR)-min(ERR)))
				else				res@trYMinF = 0.
								res@trYMaxF = ceil(1.1*max(ERR))
				end if
	
				; Draw the main line plot on the graphic.
				plot = gsn_csm_xy(wks,tofloat(XYHrs(vals)),ERR(vals2,vals),res)
	
				; Add the number of cases to the graphic.
				dum1	:= new(dimsizes(XYHrs),"graphic")
				do fff = 0,dimsizes(XYHrs)-1
					if(.not.ismissing(N_ERR(0,fff)))then
						dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(XYHrs(fff)),\
								         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
					end if
				end do
	
				; Add disclaimer to the graphic.
				if(DO_DISCLAIMER)then	XLOC = min(tofloat(XYHrs(vals)))+0.02*(max(tofloat(XYHrs(vals)))-min(tofloat(XYHrs(vals))))
							YLOC = res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF)
							add_disclaimer(wks,plot,EXPT,XLOC,YLOC,0.015,"left")
				end if
	
				; ACTUALLY PRODUCE THE GRAPHIC HERE
				draw(plot)
				frame(wks)
	
				; Remove white space from the border of the graphic.
				;if(DO_RMWHITE)then system("convert -trim "+OFILE+".png "+OFILE+".png") end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
	
			end if ;DO_THIS

		end do ; TK_ERR, ALTK_ERR, CRTK_ERR

	end if ;DO_ET_VER


	; 12. Verify EARLY INTENSITY models.
	;     Besides the model you chose (MCODEI), the rest of the models
	;     can be controlled in the master namelist.
	if(DO_EI_VER)then

                ; Update the status file.
                system("lockfile -r-1 -l 180 "+LOCK_FILE)
                print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
                system("echo 'working' > "+STATUS_FILE)
                system("rm -f "+LOCK_FILE)
                print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

		; Get model information
		EIcolors    = getModelInfo(eiModels,"color")
		EImarkers   = getModelInfo(eiModels,"marker")
		EIlongname  = getModelInfo(eiModels,"long_name")

		; a. Run TC_PAIRS
		print("MSG: Running TC_PAIRS for early intensity models.")
		system("sed -i 's/^model = .*/model = "+eiM_exp+"/g' "+TCPconf)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCPconf)

		; b. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCP_WRAPPER)
		system("echo "+str_get_dq()+"tc_pairs -v 0 -adeck "+adeckFile+" -bdeck "+bdeckFile+" -config "+\
		       TCPconf+" -out "+TCPout2+" -log "+TCPout2+".log"+str_get_dq()+" >> "+TCP_WRAPPER)
		system("chmod +x "+TCP_WRAPPER)
		system(TCP_WRAPPER+" >> "+str_sub_str(TCP_WRAPPER,".sh",".log"))

		; c. Run TC_STAT in filter mode
		print("MSG: Running TC_STAT in filter mode for early intensity models.")
		filtJob	= "jobs = ["+str_get_dq()+"-job filter -dump_row "+TCSfout2+".tcst"+str_get_dq()+"];"
		system("sed -i 's/^amodel = .*/amodel = "+eiM_exp+"/g' "+TCSconf_f)
		system("sed -i 's@^jobs = .*@"+filtJob+"@g' "+TCSconf_f)
		system("sed -i 's/^init_end = .*/init_end = "+initEnd+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_name =.*/column_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^column_str_val  =.*/column_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_name =.*/init_str_name = "+NHCStrName+"/g' "+TCSconf_f)
		system("sed -i 's/^init_str_val  =.*/init_str_val  = "+NHCStrVal+"/g' "+TCSconf_f)

		; d. Create shell script to run TC_PAIRS with shell options (better)
		system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCF_WRAPPER)
		system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCPout2+".tcst -config "+\
		       TCSconf_f+str_get_dq()+" >> "+TCF_WRAPPER)
		system("chmod +x "+TCF_WRAPPER)
		system(TCF_WRAPPER+" >> "+str_sub_str(TCF_WRAPPER,".sh",".log"))

		; e. Run TC_STAT in summary mode
		print("MSG: Running TC_STAT in summary mode for early intensity models.")
		system("sed -i 's/^amodel = .*/amodel = "+eiM_exp+"/g' "+TCSconf_s)
		system("sed -i 's/^jobs = .*/"+summJob+"/g' "+TCSconf_s)
		WIND_ERR	:= new((/dimsizes(eiModels),dimsizes(XYHrs)/),"float",fval3)
		ABS_WIND_ERR	:= WIND_ERR
		N_ERR		:= WIND_ERR
		do fff = 0,dimsizes(XYHrs)-1
			;print("MSG: Running TC_STAT in summary mode for hr = "+verTimes(fff)+".")
			
			; Set critical parameters for the MET-TC configuration.
			tmp	:= TCSsout2+".f"+sprinti("%0.3i",XYHrs(fff))+".tcst"
			tmp2	:= TCSsout2+".f"+sprinti("%0.3i",XYHrs(fff))+".log"
			system("sed -i 's/^lead       = .*/lead       = ["+str_get_dq()+sprinti("%0.2i",XYHrs(fff))+"0000"+str_get_dq()+"];/g' "+TCSconf_s)
	
			; Create a shell script to run TC_STAT with shell options.
			system("echo "+str_get_dq()+SH_SHEBANG+str_get_dq()+" > "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"source "+MOD_FILE+str_get_dq()+" >> "+TCS_WRAPPER)
			system("echo "+str_get_dq()+"tc_stat -v 0 -lookin "+TCSfout2+".tcst -config "+TCSconf_s+\
			       " -out "+tmp+" -log "+tmp2+str_get_dq()+" >> "+TCS_WRAPPER)
			system("chmod +x "+TCS_WRAPPER)
			system(TCS_WRAPPER+" >> "+str_sub_str(TCS_WRAPPER,".sh",".log"))
	
			; Read and categorize the output of the TC-STAT output.
			data	:= asciiread(tmp,-1,"string")
			errCols	= str_split(data(ind(isStrSubset2(data,"COL_NAME:")))," ")
			vals	:= ind(isStrSubset2(data,"SUMMARY:"))
			if(ismissing(vals(0)))then continue end if
			data	:= data(vals)
			errModels	:= str_squeeze(str_get_field(data,ind(errCols.eq."AMODEL")+1," "))
			errFields	:= str_squeeze(str_get_field(data,ind(errCols.eq."COLUMN")+1," "))
	
			; Find absolute intensity errors and intensity bias for all early intensity models.
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


		; Produce graphics for late intensity bias (WIND_ERR) and late absolute intensity error (ABS_WIND_ERR)
		FILE_STR := (/"WIND_ERR","ABS_WIND_ERR"/)
		TITLE_STR := (/"EARLY INTENSITY BIAS", "EARLY ABSOLUTE INTENSITY ERROR"/)
		do ddd = 0,dimsizes(FILE_STR)-1

			print("MSG: Producing graphic for early intensity model verification ("+FILE_STR(ddd)+").")

			if(ddd.eq.0)then	ERR := WIND_ERR
			else if(ddd.eq.1)then	ERR := ABS_WIND_ERR
			end if  end if

			; f. Produce graphic for early absolute intensity errors.
			DO_THIS = True
			vals := ind(.not.ismissing(ERR(0,:)))
			vals2 := ind(eiModels.ne."")
			if(ismissing(vals(0)))then		DO_THIS = False
			else if(dimsizes(vals).le.1)then	DO_THIS = False
			else if(ismissing(vals2(0)))then	DO_THIS = False
			end if  end if  end if
			if(DO_THIS)then
	
				; Setup the file, open the workstation, and update resources.
				OFILE	:= ODIR+"/Verification.ABS_WIND_ERR."+LONGSID+"."+IDATE+".early"
				if(fileexists(OFILE))then   system("rm -f "+OFILE)   end if
				wks = gsn_open_wks(wtype,OFILE)
	                        setvalues NhlGetWorkspaceObjectId()
	                                "wsMaximumSize" : 500000000
	                        end setvalues
				res@xyLineColors	:= EIcolors
				res@xyMarkers		:= EImarkers
				res@xyMarkerColors	:= EIcolors
				if(DO_TITLES)then   res@tiMainString = "EARLY INTENSITY ERROR (absolute)"   end if
				res@tiYAxisString	= "Intensity Error"+" [kt]"
				res@xyExplicitLegendLabels := " "+eiModels
				res@pmLegendHeightF	= 0.03*tofloat(dimsizes(eiModels(ind(eiModels.ne.""))))
				if(min(ERR).lt.0.)then	res@trYMinF = floor(min(ERR) - 0.1*(max(ERR)-min(ERR)))
								res@trYMaxF = ceil(max(ERR) + 0.1*(max(ERR)-min(ERR)))
				else				res@trYMinF = 0.
								res@trYMaxF = ceil(1.1*max(ERR))
				end if
	
				; Draw the main line plot on the graphic.
				plot = gsn_csm_xy(wks,tofloat(XYHrs(vals)),ERR(vals2,vals),res)
	
				; Add the number of cases to the graphic.
				dum1 := new(dimsizes(XYHrs),"graphic")
				do fff = 0,dimsizes(XYHrs)-1
					if(.not.ismissing(N_ERR(0,fff)))then
						dum1(fff) = gsn_add_text(wks,plot,""+N_ERR(0,fff),tofloat(XYHrs(fff)),\
								         res@trYMaxF - 0.03*(res@trYMaxF-res@trYMinF),txres)
					end if
				end do
	
				; Add disclaimer to the graphic.
				if(DO_DISCLAIMER)then	XLOC = min(tofloat(XYHrs(vals)))+0.02*(max(tofloat(XYHrs(vals)))-min(tofloat(XYHrs(vals))))
							YLOC = res@trYMinF + 0.02*(res@trYMaxF-res@trYMinF)
							add_disclaimer(wks,plot,EXPT,XLOC,YLOC,0.015,"left")
				end if
				
				; ACTUALLY PRODUCE THE GRAPHIC HERE
				draw(plot)
				frame(wks)
	
				; Remove white space from the border of the graphic.
				;if(DO_RMWHITE)then system("convert -trim "+OFILE+".png "+OFILE+".png") end if
				if (DO_RMWHITE .and. DO_CONVERTGIF) then
				    system("convert -trim "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_CONVERTGIF) then
				    system("convert "+OFILE+".png"+" +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
				else if (DO_RMWHITE) then
				    system("convert -trim "+OFILE+".png"+" "+OFILE+".png")
				end if  end if  end if
	
			end if ;DO_THIS

		end do ; WIND_ERR, ABS_WIND_ERR

	end if ;DO_EI_VER

end if ;DO_VERIFICATION




;================================================================;
;===========================STEP SIX=============================;
;================================================================;
;                   CHECK GRAPHICAL PRODUCTION                   ;
;================================================================;

print("")
print("MSG: *************STEP SIX************")
print("MSG: CHECK ON THE GRAPHICAL PRODUCTION.")

; Update the status file.
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
system("echo 'working' > "+STATUS_FILE)
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")

if(DO_GUIDANCE)then
	if(DO_LT_GUIDE)then
		chk := systemfunc("ls "+ODIR+"/TrackGuidance*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Track Guidance graphic not produced.")
						exit
		end if
	else if(DO_LTI_GUIDE)then
		chk := systemfunc("ls "+ODIR+"/TrackIntensityGuidance*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Track Intensity-Coded Guidance graphic not produced.")
						exit
		end if
	else if(DO_ET_GUIDE)then
		chk := systemfunc("ls "+str_sub_str(ODIR,IDATE,IDATE06)+"/TrackGuidance*"+IDATE+".early.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Early Track Guidance graphic ot produced.")
						exit
		end if
	else if(DO_LI_GUIDE)then
		chk := systemfunc("ls "+ODIR+"/IntensityGuidance*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Intensity Guidance graphic not produced.")
						exit
		end if
	else if(DO_EI_GUIDE)then
		chk := systemfunc("ls "+str_sub_str(ODIR,IDATE,IDATE06)+"/IntensityGuidance*"+IDATE+".early.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Early Intensity Guidance graphic not produced.")
						exit
		end if
	else if(DO_LP_GUIDE)then
		chk := systemfunc("ls "+ODIR+"/PressureGuidance*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Pressure Guidance graphic not produced.")
						exit
		end if
	end if  end if  end if  end if  end if  end if
else if(DO_TRENDS)then
	if(DO_LT_TREND)then
		chk := systemfunc("ls "+ODIR+"/TrackTrend."+MCODE+"*"+IDATE+".* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: "+MCODE+" Late Track Trend graphic not produced.")
						exit
		end if
	else if(DO_LT_LIFE)then
		chk := systemfunc("ls "+ODIR+"/AllTracks."+MCODE+"*"+IDATE+".* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: "+MCODE+" Late Lifetime Track graphic not produced.")
						exit
		end if
	else if(DO_LI_TREND)then
		chk := systemfunc("ls "+ODIR+"/IntensityTrend."+MCODE+"*"+IDATE+".* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: "+MCODE+" Late Intensity Trend graphic not produced.")
						exit
		end if
	else if(DO_LI_LIFE)then
		chk := systemfunc("ls "+ODIR+"/AllIntensity."+MCODE+"*"+IDATE+".* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: "+MCODE+" Late Lifetime Intensity graphic not produced.")
						exit
		end if
	else if(DO_LP_TREND)then
		chk := systemfunc("ls "+ODIR+"/PressureTrend."+MCODE+"*"+IDATE+".* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: "+MCODE+" Late Pressure Trend graphic not produced.")
						exit
		end if
	end if  end if  end if  end if  end if
else if(DO_VERIFICATION)then
	if(DO_ET_VER)then
		chk := systemfunc("ls "+ODIR+"/Verification*TK_ERR*"+IDATE+".early.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Early Track Model Verification did not plot.")
						exit
		end if
	else if(DO_EI_VER)then
		chk := systemfunc("ls "+ODIR+"/Verification*WIND_ERR*"+IDATE+".early.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Early Intensity Model Verification did not plot.")
						exit
		end if
	else if(DO_LT_VER)then
		chk := systemfunc("ls "+ODIR+"/Verification*TK_ERR*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Track Model Verification did not plot.")
						exit
		end if
	else if(DO_LI_VER)then
		chk := systemfunc("ls "+ODIR+"/Verification*WIND_ERR*"+IDATE+".late.* 2>/dev/null")
		if(ismissing(chk(0)))then	print("ERROR: Late Intensity Model Verification did not plot.")
						exit
		end if
	end if  end if  end if  end if
end if  end if  end if



;================================================================
; Write final message to status file.
;================================================================

; Update the status file.
system("lockfile -r-1 -l 180 "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been locked ("+LOCK_FILE+").")
if(FNL_ATCF)then	system("echo 'complete' > "+STATUS_FILE)
else			system("echo 'incomplete' > "+STATUS_FILE)
end if
system("rm -f "+LOCK_FILE)
print("MSG: "+STATUS_FILE+" has been unlocked ("+LOCK_FILE+").")


;================================================================
; End of file.
;================================================================

print("")
print("GPLOT_stats.ncl succeeded at "+systemfunc("date"))

exit

end




