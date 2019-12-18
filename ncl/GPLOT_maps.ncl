;
; File Name: GPLOT_maps.ncl
;
; Description:  This script creates 2D map graphics from model
;               output. The format is PNG. White space is removed
;               from the image edge. A namelist file controls the
;               fields that are plotted. Plot titles must be
;               added manually.
;
; Namelist:     /home/Ghassan.Alaka/NCL/scripts/namelist.GPLOT
;
; Created By:    Ghassan Alaka Jr.
; Assisted By:   Mu-Chieh Ko, Lewis J. Gramer
; Date Created:  September 2, 2015
;
; Last Modified: April 18, 2019
;
; Last Modified by LJG: August 16, 2019
;
; V1:           original
; V2:           05/04/2016 - Plot only 1 domain per script call 
; V3:           06/29/2016 - Add D03 plotting capability.
;                          - Check for existing products at each forecast hour.
; V4:           08/01/2016 - Script efficiency was increased.
; V5:           12/12/2016 - Script infrastructure optimized.
;                          - Cross Section capability added.
; V6            06/01/2017 - Namelist controls initial setup (different from graphics nmlist)
;                          - Functions updates to work with Fortran via WRAPIT
; V7            08/01/2017 - (time,lat,lon) read capability added (for fvGFS)
; V8            09/15/2017 - ATCF capability added for GFS
; 2018-02-23 --> Moved from GPLOT_realtime.v9.ncl
;================================================================
;  MK LOG:
; V4: update finding IDIR path logic (Function: getIDIR)
; V5: update iStorms and get rid of other checking if statements
;================================================================
;  LJG LOG:
; V1: added DO_CONVERTGIF to call ImageMagick to 'convert' to GIF
;================================================================
; Load scripts
;================================================================

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$GPLOT_DIR/ncl/GPLOT_util.ncl"
load "$GPLOT_DIR/ncl/GPLOT_func.ncl"
load "$GPLOT_DIR/ncl/GPLOT_main.ncl"

;external FILTER121 "$GPLOT_DIR/ncl/wrapit/filter121.so"
external SPH2CART "$GPLOT_DIR/ncl/wrapit/sph2cart.so"
;external HBFILTER "$GPLOT_DIR/ncl/wrapit/hbfilter.so"



;================================================================
; Begin the code.
;================================================================

begin

print("MSG: GPLOT_maps.ncl started at "+systemfunc("date"))
print("")
print("MSG: Welcome to GPLOT, Maps Module.")
print("MSG: GPLOT is the Graphical Post-processed Locus for Output for Tropical cyclones.")
print("MSG: The Maps Module produces graphical products for particular domains. These")
print("MSG: products are organized into tiers so that the most important graphics are")
print("MSG: produced first.")
print("MSG: Written by Ghassan Alaka [NOAA/OAR/AOML/HRD].")
print("MSG: Edited significantly by Mu-Chieh Ko [AOML/HRD & UMiami/CIMAS]")
print("MSG: All queries should be sent to Ghassan.Alaka@noaa.gov")
print("MSG: This module was last updated on 2019-06-15.")
print("")




;================================================================
;================================================================
;================================================================
; PART I. Set constants. DO NOT CHANGE.
;================================================================
;================================================================
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
;================================================================
;================================================================
; PART II. Check for certain command line input & environmental
;          variables.
;================================================================
;================================================================
;================================================================

; 1. GPLOT_DIR must be set in the environment
GPLOT_DIR = systemfunc("echo $GPLOT_DIR")
if(GPLOT_DIR.eq."")then print("ERROR: $GPLOT_DIR must be set as an environmental variable.")
                        exit
else                    print("MSG: Found this GPLOT location: "+GPLOT_DIR)
end if


; 2. Get the user & home from the environment
USER = systemfunc("echo $USER")
HOME = systemfunc("echo $HOME")


; 3. Check for ensemble member from command line
if(isvar("ENSID"))then  print("MSG: Found this ensemble ID: "+ENSID)
else                    ENSID = ""
end if


; 4. Check for the level of verbosity
;    The default level is 0
if(.not.isvar("verbose"))then   verbose = 0   end if
if(typeof("verbose").eq."string")then   verbose := toint(verbose)   end if
print("MSG: Verbose level = "+tostring(verbose))




;================================================================
;================================================================
;================================================================
; PART III. Read Master Namelist and Assign Variables 
;================================================================
;================================================================
;================================================================

; 1. Set the namelist path.
;    Preferable to get this from the command line.
;    Default namelist is set as a backup.
if(.not.isvar("MASTER_NML_IN"))then     MASTER_NML = GPLOT_DIR+"/nmlist/namelist.master.default"
else                                    MASTER_NML = GPLOT_DIR+"/nmlist/"+MASTER_NML_IN
end if
print("MSG: Using this master namelist:  "+MASTER_NML)


; 2. Read the master namelist into NCL
data = asciiread(MASTER_NML,-1,"string")


; 3. Get basic variables from the master namelist:
;    DOMAIN, TIER, DSOURCE, SYS_ENV, IDATE, SID, ATCF_REQD
;    First, check if basic variables should be automatically defined (AUTO_VAR).
;    Any of these variables can be overwritten by command line arguments.
AUTO_VAR        = data(ind(isStrSubset2(data,"AUTO_VAR =")))
AUTO_VAR        := str_squeeze(str_split(AUTO_VAR," = "))
AUTO_VAR        := where(AUTO_VAR(1).eq."True",True,False)
if(AUTO_VAR)then        print("MSG: Auto-Variable functionality is ON. Ignoring basic settings in namelist.")
                        print("MSG: Please change namelist if this is not desired.")
                        if(isvar("DOMAIN"))then         print("MSG: Detected command line variable: DOMAIN")
                        else                            DOMAIN  = "basin"
                        end if
                        if(isvar("TIER"))then           print("MSG: Detected command line variable: TIER")
                        else                            TIER    = "Tier1"
                        end if
                        if(isvar("DSOURCE"))then        print("MSG: Detected command line variable: DSOURCE")
                        else                            DSOURCE = "HWRF"
                        end if
                        if(isvar("SYS_ENV"))then        print("MSG: Detected command line variable: SYS_ENV")
                        else                            SYS_ENV = "JET"
                        end if
                        if(isvar("IDATE"))then          print("MSG: Detected command line variable: IDATE")
                        else                            IDATE   = ""
                        end if
                        if(isvar("SID"))then            print("MSG: Detected command line variable: SID")
                        else                            SID     = "NONE"
                        end if
                        if(isvar("ATCF_REQD"))then      print("MSG: Detected command line variable: ATCF_REQD")
                        else                            ATCF_REQD = True
                        end if

else                    print("MSG: Auto-Variable functionality is OFF. Using basic settings in namelist.")
                        print("MSG: Please change namelist if this is not desired.")
                        if(isvar("DOMAIN"))then         print("MSG: Detected command line variable: DOMAIN")
                        else                            DOMAIN  = data(ind(isStrSubset2(data,"DOMAIN =")))
                                                        DOMAIN  := str_squeeze(str_split(DOMAIN," = "))
                                                        DOMAIN  := DOMAIN(1)
                        end if
                                
                        if(isvar("TIER"))then           print("MSG: Detected command line variable: TIER")
                        else                            TIER    = data(ind(isStrSubset2(data,"TIER =")))
                                                        TIER    := str_squeeze(str_split(TIER," = "))
                                                        TIER    := TIER(1)
                        end if
                                
                        if(isvar("DSOURCE"))then        print("MSG: Detected command line variable: DSOURCE")
                        else                            DSOURCE = data(ind(isStrSubset2(data,"DSOURCE =")))
                                                        DSOURCE := str_squeeze(str_split(DSOURCE," = "))
                                                        DSOURCE := DSOURCE(1)
                        end if
                        
                        if(isvar("SYS_ENV"))then        print("MSG: Detected command line variable: SYS_ENV")
                        else                            SYS_ENV = data(ind(isStrSubset2(data,"SYS_ENV =")))
                                                        SYS_ENV := str_squeeze(str_split(SYS_ENV," = "))
                                                        SYS_ENV := SYS_ENV(1)
                        end if
                        
                        if(isvar("IDATE"))then          print("MSG: Detected command line variable: IDATE")
                        else                            IDATE   = data(ind(isStrSubset2(data,"IDATE =")))
                                                        IDATE   := str_squeeze(str_split(IDATE," = "))
                                                        if(dimsizes(IDATE).eq.2)then    IDATE := IDATE(1)
                                                        else                            IDATE := ""
                                                        end if
                        end if
                        
                        if(isvar("SID"))then            print("MSG: Detected command line variable: SID")
                        else                            SID     = data(ind(isStrSubset2(data,"SID =")))
                                                        SID     := str_squeeze(str_split(SID," = "))
                                                        if(dimsizes(SID).eq.2)then      SID := SID(1)
                                                        else                            SID := ""
                                                        end if
                        end if
                        
                        if(isvar("ATCF_REQD"))then      print("MSG: Detected command line variable: ATCF_REQD")
                        else                            val := ind(isStrSubset2(data,"ATCF_REQD ="))
                                                        if(.not.ismissing(val(0)))then  ATCF_REQD       = data(val)
                                                                                        ATCF_REQD       := str_squeeze(str_split(ATCF_REQD," = "))
                                                                                        ATCF_REQD       := where(ATCF_REQD(1).eq."True",True,False)
                                                        else                            ATCF_REQD       := True
                                                        end if
                        end if

end if


; 4. These variables must be defined. Otherwise, exit.
if(DSOURCE.eq."")then   print("ERROR: DSOURCE must be set in the namelist or as a command line arg.")
                        exit
end if
if(IDATE.eq."")then     print("ERROR: IDATE must be set in the namelist or as a command line arg.")
                        exit
end if
if(DOMAIN.eq."")then    print("ERROR: DOMAIN must be set in the namelist or as a command line arg.")
                        exit
end if
if(TIER.eq."")then      print("ERROR: TIER must be set in the namelist or as a command line arg.")
                        exit
end if


; 5. Set these variables if still undefined.
;    Also, fix the type, if necessary.
;    The script can proceed, but perhaps not as intended.
if(SID.eq."")then       print("WARNING: SID not found. Setting it SID=NONE.")
                        SID = "NONE"
end if
if(SYS_ENV.eq."")then   print("WARNING: SYS_ENV not found. Setting it SID=NONE.")
                        SYS_ENV = "JET"
end if
if(.not.isvar("ATCF_REQD"))then         ATCF_REQD = True
else
        if(ATCF_REQD.eq."")then         ATCF_REQD := True
        else if(typeof(ATCF_REQD).eq."string")then
                                        ATCF_REQD := where(ATCF_REQD.eq."True",True,False)
        end if  end if
end if


; Correct IDATE so that it is only the date.
; For example, strip "gfs." from GFS input, if applicable.
if(DSOURCE.eq."GFS")then   IDATE = str_sub_str(IDATE,"gfs.","")   end if


; 6. Update DOMAIN to d03 (if DSOURCE=d03)
;    This is a manual override of the DOMAIN variable
;    but is based on an older version of this package
;    and therefore should only be used for testing.
;    To invoke, simply append "d03" onto any DSOURCE.
;    EX: HWRF --> HWRF_d03
if(isStrSubset(DSOURCE,"d03"))then   DOMAIN = "d03"   end if


; 7. Get path variables from the master namelist:
;    EXPT, IDIR, ATCF1_DIR, ATCF1_TAG, ATCF2_DIR, ATCF2_TAG, ITAG, IDIR
;    First, check if paths should be automatically defined (AUTO_DIR)
;    'AUTO_DIR' may be deprecated. Users should know supply this info.
AUTO_DIR        = data(ind(isStrSubset2(data,"AUTO_DIR =")))
AUTO_DIR        := str_squeeze(str_split(AUTO_DIR," = "))
AUTO_DIR        := where(AUTO_DIR(1).eq."True",True,False)
if(AUTO_DIR)then        print("MSG: Auto-Directory functionality is ON. Ignoring paths defined in namelist.")
                        print("MSG: Please change namelist if this is not desired.")
                        EXPT    = getAutoDir(SYS_ENV,DSOURCE,"EXPT")
                        IDIR    = getAutoDir(SYS_ENV,DSOURCE,"IDIR")
                        ATCF2_DIR       = getAutoDir(SYS_ENV,DSOURCE,"ATCFDIR")
                        ATCF2_TAG       = ".atcfunix"
                        if(isStrSubset(DSOURCE,"HWRF"))then
                                if(DOMAIN.eq."d03")then         ITAG    = "hwrfprs.storm"
                                else if(DOMAIN.eq."d02")then    ITAG    = "hwrfprs.synoptic"
                                else                            ITAG    = "hwrfprs.global"
                                                                ;ITAG   = (/"hwrfprs.global","hwrfsat.global"/)
                                end if  end if
                        else if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then
                                ITAG = (/"atmos_sos.nest02_nested_ltd","nggps2d.nest02_nested_ltd"/)
                        else if(str_lower(DSROUCE).eq."gfs")then
                                ITAG = (/"pgrb2.0p25"/)
                        else
                                ITAG = ""
                        end if  end if  end if
                        EXT = ".grb2"
                        ATCF1_DIR       = IDIR
                        ATCF1_TAG       = ".trak.hwrf.raw"
                        ODIR            = "/lfs1/projects/hur-aoml/Ghassan.Alaka/GPLOT/"+EXPT+"/"+IDATE+"/"+DOMAIN+"/"  
else                    print("MSG: Auto-Directory functionality is OFF. Using paths defined in namelist.")
                        print("MSG: Please change namelist if this is not desired.")
                        EXPT    = data(ind(isStrSubset2(data,"EXPT =")))
                        EXPT    := str_squeeze(str_split(EXPT," = "))
                        EXPT    := EXPT(1)

                        IDIR    = data(ind(isStrSubset2(data,"IDIR =")))
                        IDIR    := str_squeeze(str_split(IDIR," = "))
                        IDIR    := IDIR(1)
        
                        ITAG    = data(ind(isStrSubset2(data,"ITAG =")))
                        ITAG    := str_squeeze(str_split(ITAG," = "))
                        if(dimsizes(ITAG).eq.1)then     ITAG = ""
                        else                            ITAG := ITAG(1)
                                                        ITAG := str_squeeze(str_split(ITAG," "))
                        end if
                                
                        EXT     = data(ind(isStrSubset2(data,"EXT =")))
                        EXT     := str_squeeze(str_split(EXT," = "))
                        if(dimsizes(EXT).eq.1)then      EXT = ""
                        else                            EXT := EXT(1)
                                                        EXT := str_squeeze(str_split(EXT," "))
                        end if

                        ODIR    = data(ind(isStrSubset2(data,"ODIR =")))
                        ODIR    := str_squeeze(str_split(ODIR," = "))
                        ODIR    := ODIR(1)
                        ODIR    = ODIR+EXPT+"/"+IDATE+"/"+DOMAIN+"/"
        
                        ATCF1_DIR       = data(ind(isStrSubset2(data,"ATCF1_DIR =")))
                        ATCF1_DIR       := str_squeeze(str_split(ATCF1_DIR," = "))
                        ATCF1_DIR       := ATCF1_DIR(1)

                        ATCF1_TAG       = data(ind(isStrSubset2(data,"ATCF1_TAG =")))
                        ATCF1_TAG       := str_squeeze(str_split(ATCF1_TAG," = "))
                        ATCF1_TAG       := ATCF1_TAG(1)
                                
                        ATCF2_DIR       = data(ind(isStrSubset2(data,"ATCF2_DIR =")))
                        ATCF2_DIR       := str_squeeze(str_split(ATCF2_DIR," = "))
                        ATCF2_DIR       := ATCF2_DIR(1)

                        ATCF2_TAG       = data(ind(isStrSubset2(data,"ATCF2_TAG =")))
                        ATCF2_TAG       := str_squeeze(str_split(ATCF2_TAG," = "))
                        ATCF2_TAG       := ATCF2_TAG(1)
end if


; 8. Get non-automated variables from master namelist
; a. This is the actual initial forecast hour for which graphics will be produced.
INIT_HR = data(ind(isStrSubset2(data,"INIT_HR =")))
INIT_HR := str_squeeze(str_split(INIT_HR," = "))
INIT_HR := toint(tofloat(INIT_HR(1)))

; b. This is the actual final forecast hour for which graphics will be produced.
;    Its value may change based on the availability of ATCF data.
FNL_HR  = data(ind(isStrSubset2(data,"FNL_HR =")))
FNL_HR  := str_squeeze(str_split(FNL_HR," = "))
FNL_HR  := toint(tofloat(FNL_HR(1)))

; c. This is the format code for the forecast hour used in the input file name
FMT_HR  = data(ind(isStrSubset2(data,"FMT_HR =")))
FMT_HR  := str_squeeze(str_split(FMT_HR," = "))
FMT_HR  := "%0."+FMT_HR(1)+"i"

; d. This is the time step between forecast hours
DT      = data(ind(isStrSubset2(data,"DT =")))
DT      := str_squeeze(str_split(DT," = "))
DT      := toint(tofloat(DT(1)))

; e. This variable determines if the case is real-time.
;    (may be deprecated)
;IS_REAL     = data(ind(isStrSubset2(data,"IS_REAL =")))
;IS_REAL    := str_squeeze(str_split(IS_REAL," = "))
;IS_REAL    := where(IS_REAL(1).eq."True",True,False)

; f. This variable determines if the case is from Basin-Scale HWRF.
;    It is important because that means multiple high-res domains
;    could exist. Domains that are not storm-centered should be not
;    be linked to a specific storm. The Operational HWRF is storm-
;    centered, so it is always linked to a specific storm.
if(.not.isvar("IS_MSTORM"))then  IS_MSTORM = data(ind(isStrSubset2(data,"IS_MSTORM =")))
                                IS_MSTORM := str_squeeze(str_split(IS_MSTORM," = "))
                                IS_MSTORM := where(IS_MSTORM(1).eq."True",True,False)
end if

; g. If True, trim the white space from graphics.
DO_RMWHITE      = data(ind(isStrSubset2(data,"DO_RMWHITE =")))
DO_RMWHITE      := str_squeeze(str_split(DO_RMWHITE," = "))
DO_RMWHITE      := where(DO_RMWHITE(1).eq."True",True,False)

; h. If True, add the data source to the graphic title.
DO_SRCLBL       = data(ind(isStrSubset2(data,"DO_SRCLBL =")))
DO_SRCLBL       := str_squeeze(str_split(DO_SRCLBL," = "))
DO_SRCLBL       := where(DO_SRCLBL(1).eq."True",True,False)

; i. If True, mask the topography.
;    This option is inactive for now.
;DO_TOPOMASK    = data(ind(isStrSubset2(data,"DO_TOPOMASK =")))
;DO_TOPOMASK    := str_squeeze(str_split(DO_TOPOMASK," = "))
;DO_TOPOMASK    := where(DO_TOPOMASK(1).eq."True",True,False)

; j. Pivot longitude. For strange regional domains (e.g., HWRF-B),
;    the longitude array must be reordered.
val := ind(isStrSubset2(data,"PIV ="))
if(.not.ismissing(val(0)))then	PIV = data(val)
				PIV := str_squeeze(str_split(PIV," = "))
				PIV := tofloat(PIV(1))
else				PIV = 180.
end if

; k. If True, convert PNG files to GIF and remove PNG files.
DO_CONVERTGIF      = data(ind(isStrSubset2(data,"DO_CONVERTGIF =")))
DO_CONVERTGIF      := str_squeeze(str_split(DO_CONVERTGIF," = "))
DO_CONVERTGIF      := where(DO_CONVERTGIF(1).eq."True",True,False)




;================================================================
;================================================================
;================================================================
; PART IV. Print some information to the terminal
;================================================================
;================================================================
;================================================================

print("MSG: Running GPLOT with these settings:")
;if(IS_REAL)then         print("MSG:   Real-Time Forecast: ON")
;else                    print("MSG:   Real-Time Forecast: OFF")
;end if
if(IS_MSTORM)then        print("MSG:   Basin-Scale HWRF:   ON")
else                    print("MSG:   Basin-Scale HWRF:   OFF")
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
print("MSG:   System Environment: "+SYS_ENV)
print("MSG:   Input File Tag(s):  "+stringOut(ITAG))
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
print("MSG:   Hour Format:        "+FMT_HR)
print("MSG:   Time Step [h]:      "+DT)
if(ATCF_REQD)then       print("MSG:   ATCF Required:      ON")
else                    print("MSG:   ATCF Required:      OFF")
end if
print("MSG:   Final ATCF Path:    "+ATCF2_DIR)
print("MSG:   Final ATCF Tag:     "+ATCF2_TAG)
if(ATCF1_DIR.ne.ATCF2_DIR .and. ATCF1_TAG.ne.ATCF2_TAG)then
        print("MSG:   Temp. ATCF Path:     "+ATCF1_DIR)
        print("MSG:   Temp. ATCF Tag:      "+ATCF1_TAG)
end if
if(DO_RMWHITE)then      print("MSG:   Trim graphics:      ON")
else                    print("MSG:   Trim graphics:      OFF")
end if
if(DO_SRCLBL)then       print("MSG:   Source Labels:      ON")
else                    print("MSG:   Source Labels:      OFF")
end if
;if(DO_TOPOMASK)then    print("MSG:   Topography mask:    ON")
;else                   print("MSG:   Topography mask:    OFF")
;end if
if(DO_CONVERTGIF)then   print("MSG:   Convert to GIF:     ON")
else                    print("MSG:   Convert to GIF:     OFF")
end if




;================================================================
;================================================================
;================================================================
; PART V. Set/Change some variables here.
;         These options may eventually be moved into functions
;         and/or namelist options.
;================================================================
;================================================================
;================================================================

; 2. Make the Output Directory (ODIR)
;    It should already exist, so this is a safety check.
system("mkdir -p "+ODIR)


; 3. Graphic domain boundaries
;    Ex: (/lat_north, lat_south, lon_west, lon_east/)
;    To add a new domain, please add it to GPLOT_func.ncl
;    BOCO will be updated for storm-centered domains.
;BOCO   = (/0.,0.,0.,0./)
BOCO    = getDmnBds(DOMAIN,DSOURCE)


; 4. Forecast hour information
;initStr         = ".f"+sprinti("%0.3i",INIT_HR)                ; might be deprecated
;fnlStr          = ".f"+sprinti("%0.3i",FNL_HR)         ; might be deprecated
;totFiles        = (FNL_HR-INIT_HR)/DT+1 ;MK - change for ensemnle: expect fh0-fh180 (61 in total) *original: 43
;if(SYS_ENV.eq."MAC")then   yesterday = systemfunc("date -v-24H '+%Y%m%d%H'")  
;else
;                          yesterday = systemfunc("date '+%Y%m%d%H' -d '24 hours ago'")
;end if


; 5. Check if this configuration is storm-centric.
;    This will control the graphic name and other options.
;    Storm-centric configurations will append the storm name
;    and storm ID to the front of the file name.
;    SID must be not equal to NONE to advance
SC_DOMAIN   = False
SC_DSOURCE  = False
SC_GRAPHICS = False
if(any((/"d02","d03","tkfull","hwrf"/).eq.DOMAIN))then    SC_DOMAIN  = True     end if
;if(.not.IS_MSTORM .and. isStrSubset(DSOURCE,"HWRF"))then   SC_DSOURCE = True     end if
if(SC_DOMAIN .or. SC_DSOURCE)then
        print("MSG: Determined that these graphics are storm-centered.")
        SC_GRAPHICS = True
        if(SID.eq."NONE" .and. ATCF_REQD)then
                print("ERROR: No storms identified, but ATCF is required.")
                print("ERROR: Therefore, can't create storm-centered graphics.")
                exit
        end if
end if


; 6. If DSOURCE includes FVGFS, all times and levels are 
;    located in one input file.
if(isStrSubset(str_lower(DSOURCE),"fvgfs"))then ONE_FILE = True
else                                            ONE_FILE = False
end if


; 7. 'fvgfs' data sources require DT=6 for the storm-centered domains.
if(isStrSubset(str_lower(DSOURCE),"fvgfs") .and. DOMAIN.eq."d03")then   DT = 6   end if


; 8. Create a string that contains the SID for storm-centered domains
;    and is blank for non storm-centered domains.
if(SC_GRAPHICS)then     SIDtag = "."+str_upper(SID)
else                    SIDtag = ""
end if


; 9. Workstation resources
wtype   = "png"
wtype@wkWidth = 2048
wtype@wkHeight = 2048




;================================================================
;================================================================
;================================================================
; PART VI. Read Graphics Namelist
;================================================================
;================================================================
;================================================================

; 1. Set the Graphics Namelist (VAR_NML)
;    Comment this to set default namelist.
;VAR_NML                = GPLOT_DIR+"/nmlist/namelist.maps.test"
if(isvar("VAR_NML"))then
        print("MSG: Detected custom graphics namelist. Will try that first.")
        if(fileexists(GPLOT_DIR+"/nmlist/"+VAR_NML))then
                VAR_NML = GPLOT_DIR+"/nmlist/"+VAR_NML
        else
                print("WARNING: Custom graphics namelist not found. Trying defaults.")
                delete(VAR_NML)
        end if
end if
if(.not.isvar("VAR_NML"))then   
        if(fileexists(GPLOT_DIR+"/nmlist/namelist.maps."+EXPT+"."+DOMAIN+"."+TIER))then
                VAR_NML = GPLOT_DIR+"/nmlist/namelist.maps."+EXPT+"."+DOMAIN+"."+TIER
        end if
end if
if(.not.isvar("VAR_NML"))then   
        if(fileexists(GPLOT_DIR+"/nmlist/namelist.maps."+EXPT+"."+TIER))then
                VAR_NML = GPLOT_DIR+"/nmlist/namelist.maps."+EXPT+"."+TIER
        end if
end if
if(.not.isvar("VAR_NML"))then
        if(fileexists(GPLOT_DIR+"/nmlist/namelist.maps."+DOMAIN+"."+TIER))then
                VAR_NML = GPLOT_DIR+"/nmlist/namelist.maps."+DOMAIN+"."+TIER
        end if
end if
if(.not.isvar("VAR_NML"))then
        if(fileexists(GPLOT_DIR+"/nmlist/namelist.maps.default."+TIER))then
                VAR_NML = GPLOT_DIR+"/nmlist/namelist.maps.default."+TIER
        end if
end if
if(.not.isvar("VAR_NML"))then
        if(fileexists(GPLOT_DIR+"/nmlist/namelist.maps.default"))then
                VAR_NML = GPLOT_DIR+"/nmlist/namelist.maps.default"
        end if
end if
if(.not.isvar("VAR_NML"))then   print("ERROR: Graphics namelist not available. Something went wrong.")
                                exit
end if
print("MSG: Using this graphics namelist: "+VAR_NML)


; 2. Read the Graphics Namelist
data            := asciiread(VAR_NML,-1,"string")
headers         := str_split(data(0),"   ")
tmp             := str_squeeze(str_get_field(data(1:),ind(headers.eq."PLOT_ON")+1,"   "))
nPlots          = dimsizes(tmp)
What2Plot       = where(tmp.eq."True",True,False)

; 2a. BASE - Contour Fill
G_BASE          = new((/2,nPlots/),"string")
G_BASE(0,:)     = str_squeeze(str_get_field(data(1:),ind(headers.eq."BASE_CN_FILL")+1,"   "))
G_BASE(1,:)     = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV1")+1,"   "))
G_BASE          = where(G_BASE.eq."N/A","",G_BASE)

; 2b. OVERLAY 1 - Wind Vectors
G_OV1           = G_BASE
G_OV1(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_VC_WIND")+1,"   "))
G_OV1(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV4")+1,"   "))
G_OV1           = where(G_OV1.eq."N/A","",G_OV1)

; 2c. OVERLAY 2 - Streamlines  (bottom)
G_OV2   = G_BASE
G_OV2(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_STLINE")+1,"   "))
G_OV2(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV6")+1,"   "))
G_OV2   = where(G_OV2.eq."N/A","",G_OV2)

; 2d. OVERLAY 2.5 - Streamlines (top)
G_OV25   = G_BASE
G_OV25(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_STLINE2")+1,"   "))
G_OV25(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV7")+1,"   "))
G_OV25   = where(G_OV25.eq."N/A","",G_OV25)

; 2e. OVERLAY 3 - Contour Lines (bottom)
G_OV3   = G_BASE
G_OV3(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_CN_LINE2")+1,"   "))
G_OV3(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV3")+1,"   "))
G_OV3   = where(G_OV3.eq."N/A","",G_OV3)

; 2f. OVERLAY 4 - Contour Lines (top)
G_OV4           = G_BASE
G_OV4(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_CN_LINE")+1,"   "))
G_OV4(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV2")+1,"   "))
G_OV4           = where(G_OV4.eq."N/A","",G_OV4)

; 2g. OVERLAY 5 - Max/Min Values & Labels
G_OV5           = G_BASE
G_OV5(0,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."OV_MAX_MIN")+1,"   "))
G_OV5(1,:)      = str_squeeze(str_get_field(data(1:),ind(headers.eq."LEV5")+1,"   "))
G_OV5           = where(G_OV5.eq."N/A","",G_OV5) 

; 2h. Graphic Name
FNAME           = str_squeeze(str_get_field(data(1:),ind(headers.eq."FILE_NAME")+1,"   "))
if(DSOURCE.eq."GFS" .and. any(FNAME.eq."REFL_MSLP"))then
        What2Plot = where(FNAME.eq."REFL_MSLP",False,What2Plot)
end if
if(DOMAIN.eq."d03" .and. any(FNAME.eq."PRCP_MSLP_Z1000500"))then
        What2Plot = where(FNAME.eq."PRCP_MSLP_Z1000500",False,What2Plot)
end if
if(DOMAIN.eq."d03" .and. any(FNAME.eq."PRCP_MSLP"))then
        What2Plot = where(FNAME.eq."PRCP_MSLP",False,What2Plot)
end if
if(.not.any(What2Plot))then     print("ERROR: No graphics turned on in this namelist. Exiting.")
                                exit
end if


; 3. These are the variables that we will plot
FNAME2 = FNAME(ind(What2Plot))
print("MSG: Will produce the following graphics:")
print("MSG: "+stringOut(FNAME2))




;================================================================
;================================================================
;================================================================
; PART VII. Get the list of input files.
;================================================================
;================================================================
;================================================================

print("")
print("MSG: Get the list of input files for graphical production.")


; 1. Get the list of files from UnplottedFiles.
;    'UnplottedFiles' is generated automatically from spawn_maps.sh
;    It is HIGHLY recommended that you submit this job via the
;    spawn_maps.sh script. However, if you really want to submit
;    this script yourself, then you must mimic 'UnplottedFiles'
;    by listing all files to be processed in a text file.
IFILES_LIST=ODIR+"UnplottedFiles."+DOMAIN+"."+TIER+SIDtag+".log"
IFILES_TEST=systemfunc("ls "+IFILES_LIST+" 2>/dev/null")
if(ismissing(IFILES_TEST))then		print("ERROR: No files listed for graphical production.")
					exit
end if
iFiles = asciiread(IFILES_LIST, -1, "string")
if(all(ismissing(iFiles)))then          print("ERROR:  No input files found. Something went wrong.")
                                        exit
end if


; 2. Remove missing values to create final file list for IDATE
iFiles  := iFiles(ind(.not.ismissing(iFiles)))
nFiles  := dimsizes(iFiles)


; 3. Find out which files have not been modified in the last 30 minutes.
;    If any files meet this criteria, then final status can't be "complete".
FNL_IFILES = new(nFiles,logical)
do fff = 0,nFiles-1
        IFILES_MOD := systemfunc("find "+iFiles(fff)+" -mmin +30")
        if(ismissing(IFILES_MOD(0)))then        FNL_IFILES(fff) = False
        else                                    FNL_IFILES(fff) = True
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
	print("ERROR: Forecast hour list not found --> "+FHR_LIST)
	print("ERROR: It is advised to submit this job via the shell wrapper scripts.")
	print("ERROR: If you manually create this file, be certain that the list lines up with")
	print("ERROR: the list of input files --> "+IFILES_LIST)
	exit
end if
FHR=asciiread(FHR_LIST, -1, "integer")
nFHR = dimsizes(FHR)
FHR@format = "%0.1i"
print("MSG: Found these forecast hours --> "+stringOut(FHR))


; 5. Define the status file. This will be updated throughout.
;    If running the GPLOT wrappers, this must match.
STATUS_FILE = ODIR+"status."+DOMAIN+"."+TIER+SIDtag+".log"
print("MSG: GPLOT Status file is "+STATUS_FILE)




;================================================================
;================================================================
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
;================================================================
;================================================================

print("")
print("MSG: Searching for matching ATCFs.")

                
; 1. Get the list of ATCF files from a pre-existing text file
;    If the text file does not exist, then set ATCF_FILES=NONE
if(fileexists(ODIR+"ATCF_FILES.dat"))then       ATCF_FILES = asciiread(ODIR+"ATCF_FILES.dat",-1,"string")
else                                            ATCF_FILES = "NONE"
end if

                
; 2. Check if there are no ATCF files
if(ATCF_FILES(0).eq."NONE")then
        if(SC_DOMAIN)then       print("ERROR: Found no ATCF files. Can not proceed for storm-centered domain.")
                                exit
        else
                if(ATCF_REQD.eq."False")then    print("WARNING: Found no ATCF files. This might be OK.")
                else                            print("ERROR: Found no ATCF files. This is not OK.")
                                                exit
                end if
        end if
end if

        
; 3. Determine how many ATCFs are needed.
;    If this case is storm-centered, then only 1 ATCF is needed
if(SC_DOMAIN)then       nAll            := 1
                        ATCF_FILES      := ATCF_FILES(ind(isStrSubset2(str_lower(ATCF_FILES),str_lower(SID))))
else                    nAll            := dimsizes(ATCF_FILES)
end if


; 4. Setup a number of variables that will be filled
;    by the loop below.
tcLats  := new((/nAll,nFHR/),float)
tcLons  := new((/nAll,nFHR/),float)
tcFHR   := new((/nAll,nFHR/),integer)
tcINT   := new((/nAll,nFHR/),integer)
FNL_ATCF:= new(nAll,logical)
tcMAXHR := new(nAll,integer)
ALL_LONGSID := new(nAll,string)
ALL_SID := new(nAll,string)
ALL_SNUM:= new(nAll,integer)

                
; 5. Loop over all of the storms
LONGSID_LIST = ""
FNL_ATCF = False
;ALL_ATCF  = True       ; assume True, but we will set this to False if necessary
do sss = 0,nAll-1
                        
        ; a. Skip this part if no ATCF files were found.
        ;    The scrip would have already exited if this is not OK.
        if(ATCF_FILES(sss).eq."NONE")then   continue   end if                   
        print("MSG: Working on this ATCF file: "+ATCF_FILES(sss))
                                        
        ; b. Parse meaningful info from the ATCF file name:
        ;      --> ALL_LONGSID: list of long Storm IDs, e.g., HARVEY09L
        ;      --> LONGSID_LIST: string of long Storm IDs for graphics
        ;      --> ALL_SID: list of short Storm IDs, e.g., 09L
        ;      --> ALL_SNUM: list of storm numbers, e.g., 09
        ALL_LONGSID(sss) = str_upper(systemfunc("echo "+ATCF_FILES(sss)+" | rev | cut -d'/' -f1 | rev | cut -d'.' -f1"))
        LONGSID_LIST = LONGSID_LIST+" "+ALL_LONGSID(sss)
        LONGSID_LIST = str_squeeze(LONGSID_LIST)
        ALL_SID(sss)    = str_upper(systemfunc("echo "+ALL_LONGSID(sss)+" | rev | cut -c1-3 | rev"))
        ALL_SNUM(sss)   = toint(systemfunc("echo "+ALL_SID(sss)+" | rev | cut -c1-2 | rev"))
                        
        ; c. Read the ATCF data
        data := atcfRead(ATCF_FILES(sss))
        
        ; d. Loop over all forecast hours (from input files)
        do fff = 0,nFHR-1               
                ; i. Find the index where the ATCF forecast hour matches the input file forecast hour.
                val     := ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.FHR(fff))
                if(fff.eq.nFHR-1)then   val1    = default_fillvalue("integer")
                else                    val1    := ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.(FHR(fff)+DT))
                end if
                if(fff.eq.0)then        val2    = default_fillvalue("integer")
                else                    val2    := ind(toint(where(ismissing(data(2,:)),default_fillvalue("integer"),data(2,:))).eq.(FHR(fff)-DT))
                end if
		val1@_FillValue = fval2
		val2@_FillValue = fval2
                
                ; ii. Extract information (lat, lon, forecast hour, intensity) from the ATCF
                ;     If the forecast hours can't be matched, the interpolate from nearby points.
                ;     This only works for the first and last forecast hour.
                ;     Most of the time, if the forecast hours can't be matched, then this data
                ;     point is left missing in the ATCF data because it might exist for other
                ;     storms.    
                if(ismissing(val(0)))then
                        ; If the bounding forecast hours exist, then interpolate
                        if(.not.ismissing(val1(0)) .and. .not.ismissing(val2(0)))then
				dHr1 = abs(FHR(fff) - toint(data(2,val1(0))))
                                dHr2 = abs(FHR(fff) - toint(data(2,val2(0))))
                                pctHr1  = tofloat(dHr1)/(tofloat(dHr1)+tofloat(dHr2))
                                pctHr2  = tofloat(dHr2)/(tofloat(dHr1)+tofloat(dHr2))
                                
                                tcLats(sss,fff) = pctHr1*data(0,val1(0)) + pctHr2*data(0,val2(0))
                                tcLons(sss,fff) = pctHr1*data(1,val1(0)) + pctHr2*data(1,val2(0))
                                tcFHR(sss,fff)  = FHR(fff)
                                tcINT(sss,fff)  = toint(pctHr1*data(3,val1(0)) + pctHr2*data(3,val2(0)))
                        else
                                continue
                        end if
                else
                        ; This is the most common extraction of ATCF data.
                        tcLats(sss,fff) = data(0,val)
                        tcLons(sss,fff) = data(1,val)
                        tcFHR(sss,fff)  = toint(data(2,val))
                        tcINT(sss,fff)  = toint(data(3,val))
                end if
        end do ;fff                     

        ; e. Determine if the ATCF file has been updated within 2 hours
        ;    If so, consider the ATCF final. This is used for non
        ;    storm-centered graphics because they are not necessarily 
        ;    tied to a particular storm. So, graphics may still be
        ;    produced even if there is no ATCF entry.
        ATCF_MOD := systemfunc("find "+ATCF_FILES(sss)+" -mmin +120")
        if(ismissing(ATCF_MOD(0)))then  FNL_ATCF(sss) = False
        else                            FNL_ATCF(sss) = True
        end if
                        
        ; f. Get the maximum forecast lead time for this ATCF
        ;    This is used to determine the final forecase hour
        ;    for which graphics should be produced for non
        ;    storm-centered domains.
        if(FNL_ATCF(sss))then   tcMAXHR(sss) = max(FHR)
        else                    tcMAXHR(sss) = max(tcFHR(sss,:))
        end if
end do ;sss
        
        
; 6. Create a string of the long storm IDs.
LONGSID_LIST = str_sub_str(LONGSID_LIST," ",", ")
        
        
; 7. Update the final forecast hour string for storm-centered domains
;    Also, update the final forecase hour 
if(SC_DOMAIN)then   FNL_HR = max(tcFHR)   end if                


; 8. Determine whether all of the final ATCFs have been found.
;    If not, GPLOT will produce graphics for now, but will
;    also re-produce these graphics when the ATCF is available.
if(all(FNL_ATCF))then   ALL_ATCF = True
else                    ALL_ATCF = False
end if
        

; 9. If ALL_ATCF has not been set by now, assume it is False
if(.not.isvar("ALL_ATCF"))then   ALL_ATCF = False   end if
if(ALL_ATCF)then        print("MSG: All final ATCFs are accounted for.")
else                    print("MSG: All final ATCFs not accounted for.")
        if(ATCF_REQD)then
                        print("MSG: ATCFs are required, so graphics for forecast hours")
                        print("MSG: without ATCF entries will need to be reproduced.")
        else            print("MSG: ATCFs are not required, so this might be OK.")
        end if
end if


; 10. Define a file that contains a list of processed files.
PLOTTED_FILES = ODIR+"PlottedFiles."+DOMAIN+"."+TIER+SIDtag+".log"
        



;================================================================
;================================================================
;================================================================
; PART IX. LOOP OVER ALL INPUT FILES.
;          'DO_PLOT' is a test feature that disables all
;          plotting if set to False.
;================================================================
;================================================================
;================================================================

; OK, let's loop over all of the input files and process what we can!
fff = 0
DO_PLOT = True
if(DO_PLOT)then
do fff = 0,nFiles-1

        ;================================================================
        ; 1. START WORKING ON THE NEXT INPUT FILE
        ;    Tell the STATUS file that we're still working...
        ;================================================================
        system("echo 'working' > "+STATUS_FILE)
        
        
        ;================================================================
        ; 2. Determine which files apply to this forecast hour.
        ;================================================================
        myInd := new((/dimsizes(ITAG)/),"integer")
        do aaa = 0,dimsizes(ITAG)-1
                if(.not.ONE_FILE)then   myInd(aaa) = ind(isStrSubset2(iFiles,"f"+sprinti(FMT_HR,FHR(fff)))\
                                                          .and. isStrSubset2(iFiles,ITAG(0)))
                else                    myInd(aaa) = aaa
                end if
        end do
        myInd   := myInd(ind(.not.ismissing(myInd)))
        
        
        ;================================================================
        ; 3. OK, if 'myInd' was found, then this input file set meets our
        ;    standards.
        ;================================================================
        print("")
        print("MSG: Current File(s) ("+sprinti("%0.2i",fff+1)+" of "+sprinti("%0.2i",nFiles)+"): "+iFiles(myInd))
        print("MSG: "+systemfunc("date"))
        
        
        ;================================================================
        ; 4. Check time standards for this file
        ;    Only plot if forecast hour is divisible by 'dt'
        ;    dt is usually 3 or 6.
        ;================================================================
        if(.not.ONE_FILE)then
                if(FHR(fff).lt.INIT_HR)then     print("WARNING: This lead time is less then the requested initial time "+tostring(toint(INIT_HR)))
                                                print("WARNING: Skipping file...")
                                                continue
                else if(FHR(fff).gt.FNL_HR)then print("WARNING: This lead time is greater then the requested final time "+tostring(toint(FNL_HR)))
                                                print("WARNING: Skipping file...")
                                                continue
                else if(FHR(fff)%DT.ne.0)then   print("WARNING: This lead time is not divisible by "+tostring(toint(DT)))
                                                print("WARNING: Skipping file...")
                                                continue
                end if  end if  end if
        end if
        
        
        ;================================================================
        ; 5. Clean up old variables
        ; THIS MAY BE DEPRECATED
        ;================================================================
        ;if(DOMAIN.eq."d03")then
        ;       if(isvar("UV10max"))then delete(UV10max) end if
        ;       if(isvar("MSLPmin"))then delete(MSLPmin) end if
        ;end if
        

        ;================================================================
        ; 6. Check if an ATCF entry exists for the current forecast hour.
        ;    Required for "d03" graphics
        ;    Suggested for large-scale graphics
        ;================================================================
 
        if(.not.ONE_FILE)then
                if(SC_GRAPHICS)then
                        myFHR := ind(tcFHR(0,:).eq.FHR(fff))
                        if(ismissing(myFHR(0)))then
                                print("WARNING: No ATCF entry for "+sprinti(FMT_HR,FHR(fff)))
                                continue
                        end if
                end if
        end if
        

        ;================================================================
        ; 7. Read the file(s). Often, 'f1' will be the file of interest.
        ;    However, this script is capable of handling multiple input
        ;    files for the same forecast lead time. Later, while creating
        ;    graphics, we will search through each available input file
        ;    to find the variables of interest.
        ;================================================================
        do aaa = 0,dimsizes(myInd)-1
                if(aaa.eq.0)then        f1 = addfile(iFiles(myInd(aaa)),"r")
                else if(aaa.eq.1)then   f2 = addfile(iFiles(myInd(aaa)),"r")
                else if(aaa.eq.2)then   f3 = addfile(iFiles(myInd(aaa)),"r")
                else if(aaa.eq.3)then   f4 = addfile(iFiles(myInd(aaa)),"r")
                end if  end if  end if  end if
        end do


        ;================================================================
        ; 8. Create 'FHR3' array to account for the "time" dimension. If
        ;    file has a time dimension, FHR3 array length will be as long
        ;    as the time coordinate
        ;================================================================
        if(ONE_FILE)then
                ; Note that this is likely hard-coded for nexted FV3 work.
                if(isfilevar(f1,"time"))then    FHR3 = toint(24.*f1->time)
                else                            print("ERROR: Time dimension could not be identified.")
                end if
                TimesPerFile    = dimsizes(FHR3)
        else
                FHR3            = FHR(fff)
                TimesPerFile    = 1
        end if
        if(verbose.ge.1)then   print("MSG: TimesPerFile = "+TimesPerFile)   end if
        
        
        ;================================================================
        ; 9. Loop over all times in current input file. Could be 1 time.
        ;================================================================
        do ttt = 0,TimesPerFile-1
        
                ;================================================================
                ; a. Set the true time index.
                ;    If ONE_FILE=True, then 'ttt' is the true index
                ;    If ONE_FILE=False, then 'fff' is the true index
                ;    To avoid confusion, set one of these to 'ti'
                ;================================================================
                if(ONE_FILE)then        ti = ttt
                else                    ti = fff
                end if
                if(verbose.ge.1)then   print("MSG: ti= "+ti+" ttt= "+ttt+"fff= "+fff)   end if
                
                ;================================================================
                ; b. Set 'FHR0' to be the forecast hour for the current input
                ;    file and time.
                ;================================================================
                FHR0 = FHR3(ttt)
                
                ;================================================================
                ; c. Only plot if forecast hour is divisible by 'DT'.
                ;    'DT' is usually 3 or 6.
                ;================================================================
                if(ONE_FILE)then
                        if(FHR0%DT.ne.0)then    print("MSG: Skipping forecast hour "+sprinti("%0.3i",FHR0))
                                                continue
                        else                    print("MSG: Current forecast hour "+sprinti("%0.3i",FHR0))
                        end if
                end if
                
                ;================================================================
                ; d. Check if an ATCF entry exists for the current forecast hour.
                ;    Required for storm-centered graphics (e.g., DOMAIN=d03).
                ;    Suggested for large-scale graphics.
                ;================================================================
                if(SC_DOMAIN .and. ONE_FILE)then
                        myFHR := ind(tcFHR(0,:).eq.FHR0)
                        print("WARNING: No ATCF entry found for this forecast hour. Something might be wrong.")
                end if

                ;================================================================
                ; e. Define and delete some variables
                ;================================================================
                validDate = changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",tofloat(str_sub_str(sprinti(FMT_HR,FHR0),"f","")))
		MSLP_MARKERS = False
		if(isvar("UV10max"))then   delete(UV10max)   end if
		if(isvar("MSLPmin"))then   delete(MSLPmin)   end if
        
                ;================================================================
                ; f. LOOP OVER ALL GRAPHIC REQUESTS AND PRODUCE THEM!
                ;================================================================
                do ppp = 0,nPlots-1
                        if(.not.What2Plot(ppp))then   continue   end if
        
                        ;================================================================
                        ; i. Perform checks
                        ;================================================================
                        
                        ; Check #1: Don't plot all graphics.
                        ;           Some do not apply to a particular data source and/or domain.
                        if(DSOURCE.eq."GFS".and. FHR0.eq.0 .and.\
                           any((/"PRCP_MSLP_Z1000500","PRCP_MSLP","TPRCP_MSLP"/).eq.FNAME(ppp)))then
                                print("WARNING: Graphic '"+FNAME(ppp)+"' can't be produced for DSOURCE="+DSOURCE+" at 0-h. Skipping.")
                                continue
                        else if(DSOURCE.eq."GFS" .and. isStrSubset(FNAME(ppp),"REFL"))then
                                print("WARNING: Graphic '"+FNAME(ppp)+"' might not work for DSOURCE="+DSOURCE+".")
                                print("WARNING: Turn this graphic off in "+VAR_NML)
                        else if(DSOURCE.eq."GFS" .and. DOMAIN.eq."d03" .and. isStrSubset(FNAME(ppp),"TPRCP"))then
                                print("WARNING: Graphic '"+FNAME(ppp)+"' might not work for DOMAIN="+DOMAIN+" and DSOURCE="+DSOURCE+".")
                                print("WARNING: Turn this graphic off in "+VAR_NML)
                        else if(DOMAIN.eq."d03" .and. any((/"PRCP_MSLP_Z1000500","PRCP_MSLP","PRCP24_MSLP"/).eq.FNAME(ppp)))then
                                print("WARNING: Graphic '"+FNAME(ppp)+"' might not work for DOMAIN="+DOMAIN+".")
                                print("WARNING: Turn this graphic off in "+VAR_NML)
                        end if  end if  end if  end if
        
                        ; Check #2: If no base variable found from the namelist, do not plot.
                        if(G_BASE(0,ppp).eq."")then     print("ERROR: Must set base plot variable.")
                                                        print("WARNING: This graphic will be skipped, but this will prevent GPLOT from successfully completing.")
                                                        print("WARNING: Please update "+VAR_NML+".")
                                                        continue
                        end if
                        
                        ;================================================================
                        ; ii. Decide whether or not to remove the vortex.
                        ;     This is currently hard-coded for certain variables, but
                        ;     what there should be a table lookup that is accessed by
                        ;     an NCL function to determine the value of 'rmVortex'.
                        ;================================================================
                        if(any((/"02500850m","05000850m","07000850m"/).eq.G_BASE(1,ppp)))then
                                rmVortex = True
                        else    rmVortex = False
                        end if
                
                        ;================================================================
                        ; iii. Get title of graphic and set graphic file name.
                        ;================================================================
                
                        ; Choose the correct title & add the ensemble member ID (is applicable)
                        List_Title      = getPlotTitle(FNAME(ppp))   ; MKnote - title added in util     
                        if(ENSID.ne."")then   List_Title=List_Title+" Member:"+ENSID   end if
                        
                        ; Define Out File String
                        if(SC_GRAPHICS)then
                                OFILE   = ODIR+str_lower(ALL_LONGSID(0))+"."+FNAME(ppp)+"."+str_sub_str(IDATE,"gfs.","")+"."+DOMAIN+".f"+sprinti("%0.3i",toint(FHR0))
                        else
                                OFILE   = ODIR+FNAME(ppp)+"."+str_sub_str(IDATE,"gfs.","")+"."+DOMAIN+".f"+sprinti("%0.3i",toint(FHR0))
                        end if
                
                        ;================================================================
                        ; iv. Delete the graphic has already been created. If the input 
                        ;     file has been included and the graphic exists, then it is
                        ;     not old enough or production is being forced.
                        ;================================================================
                        if(fileexists(OFILE+".png"))then        system("rm -f "+OFILE+".png")
                                                                print("MSG: Overwriting graphic for "+FNAME(ppp))
                        else                                    print("MSG: Creating graphic for "+FNAME(ppp))
                        end if




                        ;================================================================
                        ;================================================================
                        ; START --> GET DIMENSION INFORMATION FOR THE BASE VARIABLE
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; v. The name of some precipitation variables depends on time of
                        ;    accumulation.
                        ;================================================================
                        if(any((/"TPRCP","PRCP"/).eq.G_BASE(0,ppp)))then
				hr_offset := G_BASE(1,ppp)
				if(DSOURCE.eq."GFS")then
                                	if(FHR0.gt.6)then       G_BASE(1,ppp) = tostring(FHR0)+"h"
	                                else                    G_BASE(1,ppp) = ""
	                                end if
				else if(DSOURCE.eq."HAFS")then
					if(FHR0.ge.6)then       G_BASE(1,ppp) = "3h"
	                                else                    G_BASE(1,ppp) = ""
	                                end if
				else if(DSOURCE.eq."HWRF")then
					G_BASE(1,ppp) = ""
				end if  end if  end if
                        end if
                        
                        ;================================================================
                        ; vi. Determine which file contains the G_BASE variable
                        ;================================================================
                        varStatus = False
                        do aaa = 0,dimsizes(myInd)-1
                                if(aaa.eq.0)then        f := f1
                                else if(aaa.eq.1)then   f := f2
                                else if(aaa.eq.2)then   f := f3
                                else if(aaa.eq.3)then   f := f4
                                end if  end if  end if  end if
                                
                                if(isfilevar(f,findVarName(DSOURCE,G_BASE(0,ppp),G_BASE(1,ppp))))then
                                        varStatus = True
                                        break
                                end if
                        end do

                        ;================================================================
                        ; vii. If the variable is not found in any input files, then we
                        ;      can't continue.
                        ;================================================================
                        if(.not.varStatus)then  print("WARNING: Variable "+G_BASE(0,ppp)+" not found.")
                                                print("WARNING: "+G_BASE(0,ppp)+" will be skipped, but this will prevent GPLOT from successfully completing.")
                                                print("WARNING: Please update the Graphics Namelist or add the variable to GPLOT.")
                                                continue
                        end if
                
                        ;================================================================
                        ; viii. Find Dimension Info
                        ;================================================================
                        nDims   := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_BASE(0,ppp),G_BASE(1,ppp))))
                        dNames  := getfilevardims(f,findVarName(DSOURCE,G_BASE(0,ppp),G_BASE(1,ppp)))
                        ;MK note- might need to switch to this kine when using ncl V6.5.0 or later
                        ;dNames := getfilevardimnames(f,findVarName(DSOURCE,G_BASE(0,ppp),G_BASE(1,ppp)))
                        nDims@dNames = dNames
                
                        ;================================================================
                        ; ix. For DOMAIN=hwrf, the boundaries must be updated to include
                        ;     the entire outer nest.
                        ;================================================================
                        if(any((/"d01","hwrf"/).eq.DOMAIN))then
                                TEST := f->$findVarName(DSOURCE,"MSLP","")$
				do ddd = 0,nDims-1
                                        ; LATITUDE
                                        if(isStrSubset(dNames(ddd),"lat") .or. isStrSubset(dNames(ddd),"grid_yt"))then
                                                LATTEST := where(ismissing(TEST),fval3,conform(TEST,f->$dNames(ddd)$,0))
                                                LATTEST@_FillValue = fval3
                                                BOCO(0) = max(LATTEST)
                                                BOCO(1) = min(LATTEST)
                                        end if
                                        ; LONGITUDE
                                        if(isStrSubset(dNames(ddd),"lon") .or. isStrSubset(dNames(ddd),"grid_xt"))then
						BOCO@doPivot = False
						
						if(min(f->$dNames(ddd)$).eq.0.)then
							LONTMP := f->$dNames(ddd)$
							if(.not.all(ismissing(TEST(:,{1.}))) .and. .not.all(ismissing(TEST(:,{359.}))))then
												LONTMP := lonPivot(LONTMP,PIV)
												LONTMP := where(LONTMP.ge.PIV,LONTMP-360.,LONTMP)
												TEST := lonPivot(TEST,PIV)
												;printMaxMin(LONTMP,0)
												;printMaxMin(TEST@$dNames(ddd)$,0)
												if(max(LONTMP).ne.max(TEST&$dNames(ddd)$) \
												   .or. min(LONTMP).ne.min(TEST&$dNames(ddd)$))then
													TEST&$dNames(ddd)$ = LONTMP
												end if
												BOCO@doPivot = True
							end if
						else if(max(f->$dNames(ddd)$).gt.360.)then	LONTMP	:= (/f->$dNames(ddd)$ - 360./)
						else if(max(f->$dNames(ddd)$).gt.180. .and. \
							min(f->$dNames(ddd)$).gt.180.)then	LONTMP	:= (/f->$dNames(ddd)$ - 360./)
						else						LONTMP	:= (/f->$dNames(ddd)$/)
						end if  end if  end if
						LONTMP!0 = "lon"
						LONTMP@units = "degrees_east"
						LONTMP&lon = LONTMP
						
                                                LONTEST := where(ismissing(TEST),fval3,conform(TEST,LONTMP,1))
                                                LONTEST@_FillValue = fval3
						BOCO(2) = min(LONTEST)
                                                ;if(BOCO(2).gt.180.)then   BOCO(2) = BOCO(2) - 360.   end if
                                                BOCO(3) = max(LONTEST)
                                                ;if(BOCO(3).gt.180.)then   BOCO(3) = BOCO(3) - 360.   end if
                                        end if
                                end do
                        end if
			
			

                        ;================================================================               
                        ; x. Get info based on domain.
                        ;    'B' is used to determine how to set the figure domain size.
                        ;    Force B=1 to use the lat/lon provided by the BOCO.
                        ;================================================================
                        B = getDmnInfo(DOMAIN,"bdstype",False)
                        if(SC_GRAPHICS .and. .not.ATCF_REQD .and. any((/ATCF_FILES(0),SID/).eq."NONE"))then
                                B = 1
                        end if

			                
                        ;================================================================
                        ; xi. Read dimension data (only for available dims)
                        ;================================================================
                        do ddd = 0,nDims-1
                                ; 1. Y-DOORDINATE --> LATITUDE
                                if(isStrSubset(dNames(ddd),"lat") .or. isStrSubset(dNames(ddd),"grid_yt"))then
                                        y = ddd
                                        lat := get_dim_lat(f,dNames(ddd),BOCO,tcLats(0,:),tcLons(0,:),ti,B)
                                        BOCO = lat@BOCO
                                end if
                        
                                ; 2. X-COORDINATE --> LONGITUDE
                                if(isStrSubset(dNames(ddd),"lon") .or. isStrSubset(dNames(ddd),"grid_xt"))then
                                        x = ddd
					BOCO@PivotLon = PIV
                                        lon := get_dim_lon(f,dNames(ddd),BOCO,tcLons(0,:),tcLats(0,:),ti,B)
                                        BOCO = lon@BOCO
                                        flipFlag = lon@flipFlag
                                        
                                        ; Need the full longitude array
                                        lonF := lon@LONF
                                        lonF!0 = "lon"
                                        lonF&lon = lonF
                                        lonF@units = "degrees_east"
                                end if
                        
                                ; 3. Z-COORDINATE --> ISOBARIC LEVELS
                                if(     isStrSubset(dNames(ddd),"ISBL") .or. isStrSubset(str_lower(dNames(ddd)),"pres")\
                                   .or. isStrSubset(dNames(ddd),"lev")  .or. isStrSubset(str_lower(dNames(ddd)),"lv"))then
                                        z = ddd
                                        LEV     := (/f->$dNames(ddd)$/)
                                        LEV!0 = "lev"
                                        LEV&lev = LEV
                                        if(f->$dNames(ddd)$@units.eq."Pa")then          mf      = 100.
                                                                                        LEV@units = "Pa"
                                                                                        LEV@mf  = mf
                                        else if(f->$dNames(ddd)$@units.eq."hPa")then    mf      = 1.
                                                                                        LEV@units = "hPa"
                                                                                        LEV@mf  = mf
                                        end if  end if
                                end if
                        
                                ; 4. Z-COORDINATE --> HEIGHT LEVELS
                                if(isStrSubset(dNames(ddd),"HTGL"))then         z               = ddd
                                                                                LEV             := (/f->$dNames(ddd)$/)
                                                                                LEV!0           = "lev"
                                                                                LEV&lev         = LEV
                                                                                LEV@units       = f->$dNames(ddd)$@units
                                                                                LEV@mf          = 1.
                                                                                mf              = 1.
                                end if
                        end do
                
                        ;================================================================
                        ; xii. 'mf' is the multiplicative factor for the vertical
                        ;      coordinate. If not already defined, then get vertical
                        ;      level data from "U1000" data. This generic vertical
                        ;      coordinate might be important for overlay metadata.
                        ;================================================================
                        if(.not.isvar("mf"))then   mf = 1.   end if
                
                        ;================================================================
                        ; xiii. Check that BOCO is as expected. Also, find
                        ;       BOCO(0) --> maximum latitude (farthest north)
                        ;       BOCO(1) --> minimum latitude (farthest south)
                        ;       BOCO(2) --> minimum longitude (farthest west)
                        ;       BOCO(3) --> maximum longitude (farthest east)
                        ;================================================================
                        if(BOCO(3).lt.BOCO(2))then      tmp := BOCO
                                                        BOCO(2) = tmp(3)
                                                        BOCO(3) = tmp(2)
                        end if
                        if(BOCO(0).lt.BOCO(1))then      tmp := BOCO
                                                        BOCO(0) = tmp(1)
                                                        BOCO(1) = tmp(0)
                        end if
                        
                        ;================================================================
                        ; xvi. Find latitude & longitude percentages for given domain
                        ;      This is for placement on graphics.
                        ;================================================================
                        pctLat          = abs(BOCO(0)-BOCO(1))/100.
                        pctLon          = abs(BOCO(3)-BOCO(2))/100.
                
                        ;================================================================
                        ; xv. Create BOCO2 that is based on lat/lon limits.
                        ;     Most of the time, BOCO=BOCO2. Found that this was critical
                        ;     for the 'hwrf' domain centered on the HWRF outer nest.
                        ;================================================================
                        BOCO@PivotLon = PIV
			BOCO2 = (/max(lat),min(lat),min(lon),max(lon)/)
			BOCO2@PivotLon = PIV
			
			;print(lonF)
			;print(BOCO)
			;print(BOCO2)
			
                        
                        ;================================================================f
                        ;================================================================
                        ; END --> GET DIMENSION INFORMATION FOR THE BASE VARIABLE
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ; xvi. If the vortex will be removed, set these attributes.
                        ;================================================================
                        if(rmVortex)then        rmVortex@jcen   = avg(BOCO(:1));markY(0,dimsizes(modelName)-1,fff2)
                                                rmVortex@icen   = avg(BOCO(2:));markX(0,dimsizes(modelName)-1,fff2)
                                                rmVortex@radius = max((/abs(BOCO(0)-rmVortex@jcen),abs(BOCO(2)-rmVortex@icen)/));tcRad
                                                if(rmVortex@icen.lt.min(f->$dNames(x)$))then   rmVortex@icen = rmVortex@icen + 360.   end if
                                                rmVortex@N = 11 ; inactive option, but the function checks for it right now
                        end if
                
                        ;================================================================
                        ; xvii. These flags are important to read in the data correctly.
			;       'flipFlag' deals with the orientation of the LON array.
			;       'rmvortex' deals with the removal of the TC vortex.
                        ;================================================================
                        myFlags := (/flipFlag,rmVortex/)
                        copy_VarAtts(rmVortex,myFlags)
                        
                        ;================================================================
                        ; xviii. Print some extra info high verbosity.
                        ;================================================================
                        if(verbose.ge.1)then    print("MSG: BOCO = "+BOCO)
                                                print("MSG: Dimensions have been read: "+stringOut(dNames))
                                                print("MSG: The following flags have been set: "+myFlags)
                        end if
                
                        ;================================================================       
                        ; xxi. Get max UV10 and min MSLP (to print on D03 graphics). Do
                        ;      this for the first graphic for a given input file and
                        ;      time. If either variable can't be found, then it will be
                        ;      missing.
                        ;================================================================
                      	if(.not.isvar("UV10max") .and. DOMAIN.eq."d03")then
                        
                                ; 1. GET 10M WIND
                                ; 1a. Determine which file contains the UV10max variable.
                                ;     It will most likely be 'f1'.
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,"UV","10")))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                ; 1b. Read UV10max or assign a missing value to it.
                                if(.not.varStatus)then  print("WARNING: Variable UV10 not found. Setting as missing.")
                                                        UV10max := fval3
                                                        UV10max@_Fillvalue = fval3
                                else                    UV10dims := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"UV","10")))
                                                        UV10dNames := getfilevardims(f,findVarName(DSOURCE,"UV","10"))
                                                        UV10dims@dNames = UV10dNames
                                                        UV10max := max(getVar2d(f,DSOURCE,(/"UV","10",tostring(ttt)/),UV10dims,\
                                                                                BOCO2,1.,flipFlag,lonF))
                                end if
			end if
                        
			if(.not.isvar("MSLPmin") .and. DOMAIN.eq."d03")then
                                ; 2. GET THE MINIMUM MSLP
                                ; 2a. Determine which file contains the MSLPmin variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,"MSLP","")))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                ;2b. Read MSLP minimum or assign a missing value to it.
                                if(.not.varStatus)then  print("WARNING: Variable MSLP not found. Setting as missing.")
                                                        MSLPmin := fval3
                                                        MSLPmin@_FillValue = fval3
                                else                    MSLPdims := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"MSLP","")))
                                                        MSLPdNames := getfilevardims(f,findVarName(DSOURCE,"MSLP",""))
                                                        MSLPdims@dNames = MSLPdNames
                                                        MSLPmin := min(getVar2d(f,DSOURCE,(/"MSLP","",tostring(ttt)/),MSLPdims,\
                                                                                BOCO2,1.,flipFlag,lonF))
                                end if
                        	
                                
                                ; 3. Determine which file contains the G_BASE variable
                                ;    Reset "f" if UV10max and MSLPmin were calculated
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_BASE(0,ppp),G_BASE(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                        end if
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> WORKSTATION & RESOURCES
                        ;================================================================
                        ;================================================================
                
                        ;================================================================
                        ; xxiii. Setup Workstation
                        ;================================================================
                        ;wks    = gsn_open_wks("x11","TEST")
                        wks     := gsn_open_wks(wtype,OFILE)
                        setvalues NhlGetWorkspaceObjectId() 
                                "wsMaximumSize" : 500000000 
                        end setvalues
                        ;if(reverseMap)then gsn_reverse_colormap(wks) end if
                
                        ;================================================================
                        ; xxvi. Setup Resources
                        ;       These are mostly controlled by "GenPlotRes".  Additional
                        ;       resources may be added here, but it is recommended to add
                        ;       to 'GenPlotRes' if the change is permanent.
                        ;================================================================
                
                        ; 1) Cross-Section Resources
                        ; EVENTUALLY THIS WILL BE MOVED INTO A SEPARATE GPLOT MODULE (e.g., "XSECT")
                        if(isStrSubset(G_BASE(1,ppp),"x") .or. isStrSubset(G_BASE(1,ppp),"y"))then
                                res     := GenPlotRes(10,False)
                                if(DOMAIN.eq."d03")then         if(isStrSubset(G_BASE(1,ppp),"x"))then
                                                                        res@tmXBValues          := ispan(toint(tcLons(0,ti))-(toint(tcLons(0,ti))%2)-6,toint(tcLons(0,ti))-(toint(tcLons(0,ti))%2)+6,2)
                                                                        res@tmXBLabels          := where(res@tmXBValues.lt.0,tostring(abs(res@tmXBValues))+"W",tostring(res@tmXBValues)+"E")
                                                                else if(isStrSubset(G_BASE(1,ppp),"y"))
                                                                        res@tmXBValues          := ispan(toint(tcLats(0,ti))-(toint(tcLats(0,ti))%2)-6,toint(tcLats(0,ti))-(toint(tcLats(0,ti))%2)+6,2)
                                                                        res@tmXBLabels          := where(res@tmXBValues.lt.0,tostring(abs(res@tmXBValues))+"S",tostring(res@tmXBValues)+"N")
                                                                end if  end if
                                                                res@tmXBLabels          := where(res@tmXBValues.eq.0,tostring(res@tmXBValues),res@tmXBLabels)
                                else                            if(isStrSubset(G_BASE(1,ppp),"x"))then
                                                                        res@tmXBValues  = fspan(-180.,170.,36)
                                                                        res@tmXBLabels  = (/" 180", "170W","160W","150W","140W","130W","120W","110W",\
                                                                                                    "100W",  "90W", "80W", "70W", "60W", "50W", "40W", "30W",\
                                                                                             "20W",  "10W",   "0", "10E", "20E", "30E", "40E", "50E",\
                                                                                             "60E",  "70E", "80E", "90E","100E","110E","120E","130E",\
                                                                                            "140E", "150E","160E","170E"/)
                                                                else if(isStrSubset(G_BASE(1,ppp),"y"))
                                                                        res@tmXBValues  = fspan(-90.,90.,19)
                                                                        res@tmXBLabels  = (/"90S","80S","70S","60S","50S","40S",\
                                                                                            "30S","20S","10S", "EQ","10N","20N","30N",\
                                                                                            "40N","50N","60N","70N","80N","90N"/)
                                                                end if  end if
                                end if
                
                        ; 2) Lat/Lon Map Resources
                        else
                                res                             := GenPlotRes(0,False)
                                res@mpMaxLonF                   = BOCO(3)
                                res@mpMinLonF                   = BOCO(2)
                                res@mpCenterLonF                = (res@mpMaxLonF+res@mpMinLonF)/2.
                                res@mpMaxLatF                   = BOCO(0)
                                res@mpMinLatF                   = BOCO(1)
                                if(DOMAIN.eq."d03")then
                                        ;res@gsnRightString = ""+NameIDTag
                                        res@mpGridLatSpacingF   = 2.
                                        res@mpGridLonSpacingF   = 2.
                                
                                        res@tmXBValues          := ispan(toint(tcLons(0,ti))-(toint(tcLons(0,ti))%2)-6,toint(tcLons(0,ti))-(toint(tcLons(0,ti))%2)+6,2)
                                        res@tmXBLabels          := where(res@tmXBValues.lt.0,tostring(abs(res@tmXBValues))+"W",tostring(res@tmXBValues)+"E")
                                        res@tmXBLabels          := where(res@tmXBValues.eq.0,tostring(res@tmXBValues),res@tmXBLabels)
                                
                                        res@tmYLValues          := ispan(toint(tcLats(0,ti))-(toint(tcLats(0,ti))%2)-6,toint(tcLats(0,ti))-(toint(tcLats(0,ti))%2)+6,2)
                                        res@tmYLLabels          := where(res@tmYLValues.lt.0,tostring(abs(res@tmYLValues))+"S",tostring(res@tmYLValues)+"N")
                                        res@tmYLLabels          := where(res@tmYLValues.eq.0,"EQ",res@tmYLLabels)
                                        
                                        res@lbLabelFontHeightF  = 0.012
                                        res@tmYLLabelFontHeightF= 0.012
                                        res@tmXBLabelFontHeightF= 0.012
                                else if(DOMAIN.eq."global")then
                                        res@tmXBValues = where(toint(res@tmXBValues)%20.eq.0,res@tmXBValues,fval)
                                        vals := ind(res@tmXBValues.ne.fval)
                                        res@tmXBValues := res@tmXBValues(vals)
                                        res@tmXBLabels := res@tmXBLabels(vals)
                                        res@tmYLValues = where(toint(res@tmYLValues)%20.eq.0,res@tmYLValues,fval)
                                        vals := ind(res@tmYLValues.ne.fval)
                                        res@tmYLValues := res@tmYLValues(vals)
                                        res@tmYLLabels := res@tmYLLabels(vals)
                                end if  end if
                        end if

                        ; 3. Global Resources
                        res@gsnAddCyclic                = False
                        res@cnFillDrawOrder             = "PreDraw"
                        ;res@tiMainString                = List_Title
                        ;res@gsnLeftString              = "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+"  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR(fff)),"f","")+"]  valid at "+validDate
                        ;res@gsnRightString             = ""+str_upper(NameIDTag)
                        res@gsnLeftString               = ""
                        res@gsnRightString              = ""
                        res@cnLevelSelectionMode        = "ExplicitLevels"
                        if(isStrSubset(FNAME(ppp),"PRCP"))then  res@lbLabelStride       = 1
                                                                res@cnConstFEnableFill  = True
                        end if
                
                        ; 4. Set Colorbar & Levels
                        myColormap              = defineCMAP_name(G_BASE(0,ppp),G_BASE(1,ppp))
                        gsn_define_colormap(wks,myColormap)
                        res@cnFillPalette       = defineCMAP_fill(G_BASE(0,ppp),G_BASE(1,ppp),myColormap)
                        myLevels                := defineLevels(G_BASE(0,ppp),G_BASE(1,ppp))
                        res@cnLevels            = myLevels
                        res@lbLabelStride       = myLevels@stride
                
                        ;================================================================
                        ; xxvii. Print some extra info if requested by 'verbose'
                        ;================================================================
                        if(verbose.ge.1)then    print("MSG: myColormap = "+ myColormap)
                                                print("MSG: Workstation & resources have been set.")
                        end if
                        
                        
                        ;================================================================
                        ;================================================================
                        ; END --> WORKSTATION & RESOURCES
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> BASE VARIABLE
                        ;================================================================
                        ;================================================================
                        
                        if(verbose.ge.1)then   print("MSG: Working on base shaded contours (G_BASE="+G_BASE(0,ppp)+")")   end if
                        
                        ;================================================================
                        ; xxviii. Read the Base Variable. It could be a cross-section view
                        ;         or a 2-d plan view.
                        ;================================================================
                        if(isStrSubset(G_BASE(1,ppp),"x") .or. isStrSubset(G_BASE(1,ppp),"y"))then
                                Vbase   := getVarXC(f,DSOURCE,G_BASE(:,ppp),nDims,BOCO,mf,flipFlag,lonF)
                                Vbase&lev = Vbase&lev/mf
                        else
                                Vbase   := getVar2d(f,DSOURCE,(/G_BASE(0,ppp),G_BASE(1,ppp),tostring(ttt)/),nDims,BOCO2,mf,myFlags,lonF)
                        end if


                        ;================================================================
                        ; xxxi. If base variable is Bulk Precip Rate (PRCP) and the data
                        ;       source is HWRF, then previous file is necessary to
                        ;       compute the Bulk Precip Rate.
                        ;================================================================
                        if(G_BASE(0,ppp).eq."PRCP" .and. any((/"HWRF","HAFS","GFS"/).eq.DSOURCE))then
                                
                                ; 1. Check if we should process bulk precipitation rate
                                if(ONE_FILE)then
                                        print("WARNING: Bulk precip rate not yet supported for ONE_FILE. Skipping...")
                                        continue
                                end if
                                if(FHR3(ttt).eq.0)then  print("WARNING: Can't compute bulk precip rate at 0-h. Skipping...")
                                                        continue
                                end if
                                
                                ; 2. Get the hour offset from the previous precipitation file
                                if(hr_offset.eq."")then hr_offset := 3
                                else                    hr_offset := toint(hr_offset)
                                end if
                                
                                ; 3. If hr_offset is larger than the current forecast lead time, then not enough data is available.
                                if(FHR3(ttt).lt.hr_offset)then
                                        print("WARNING: Can't compute bulk precip rate at "+FHR3(ttt)+"-h because hr_offset. Skipping...")
                                        continue
                                end if

                                ; 4. Get the correct previous file based on hr_offset
				;    Previous file not needed if it is from 0-h.
				FHR4	= FHR3(ttt)-hr_offset
                                FHRB    = "f"+sprinti(FMT_HR,FHR3(ttt)-hr_offset)+"."
                                if(FHR4.gt.0)then
					IFILEB  = systemfunc("ls "+str_sub_str(iFiles(fff),"f"+sprinti(FMT_HR,FHR3(ttt))+".",FHRB)+" 2</dev/null")
	                                if(ismissing(IFILEB(0)))then
	                                        print("WARNING: File containing previous precipitation data at "+FHRB+"-h not found. Skipping...")
	                                        continue
        	                        end if
					
					G_BASE2 = G_BASE(:,ppp)
					if(DSOURCE.eq."GFS")then
	                                	if(FHR4.ge.9)then       G_BASE2(1) = tostring(FHR4)+"h"
		                                else                    G_BASE2(1) = ""
		                                end if
					else if(DSOURCE.eq."HAFS")then
						if(FHR4.ge.6)then       G_BASE2(1) = "3h"
		                                else                    G_BASE2(1) = ""
		                                end if
					else if(DSOURCE.eq."HWRF")then
						G_BASE2(1) = ""
					end if  end if  end if
                                
	                                ; 5. Read the previous precipitation data
	                                f2      = addfile(IFILEB,"r")
	                                Vbase2  = getVar2d(f2,DSOURCE,\
	                                                   (/G_BASE2(0),G_BASE2(1),tostring(ttt)/),\
	                                                   nDims,BOCO2,mf,flipFlag,lonF)
                                
	                                ; 6. Compute the bulk precipitation rate
	                                Vbase   = (Vbase - Vbase2)/tofloat(hr_offset)
	                                Vbase!0 = "lat"
	                                Vbase!1 = "lon"
	                                Vbase&lat = lat
	                                Vbase&lon = lon
	                                Vbase@units = "mm h-1"
                        
	                                delete([/Vbase2,FHRB,f2/])
				end if
                        end if
			
			
			; For DSOURCE=GFS, the total precipitation variable (TPRCP) only reflects the precipitation
			; that accumulated since the previous output file. To get the total precip over the entire
			; forecast, the total precip from all previous forecast lead times must be added to the
			; total precip from the current lead time.
			if(G_BASE(0,ppp).eq."TPRCP" .and. any((/"HAFS"/).eq.DSOURCE))then
				;if(FHR3(ttt).eq.0)then	print("MSG: No precipitation data at 0-h. Skipping...")
				;			continue
				;end if
				system("mkdir -p "+ODIR+"PRCPDATA")
				if(FHR3(ttt).ge.6 .and. FHR3(ttt)%3.eq.0)then
					if(.not.fileexists(ODIR+"PRCPDATA/TPRCP.f"+sprinti("%0.3i",FHR3(ttt)-3)+".nc"))then
						continue
					end if
					f2 := addfile(ODIR+"PRCPDATA/TPRCP.f"+sprinti("%0.3i",FHR3(ttt)-3)+".nc","r")
					Vbase	= Vbase + (/f2->TPRCP/)
					Vbase!0 = "lat"
					Vbase!1 = "lon"
					Vbase&lat = lat
					Vbase&lon = lon
					Vbase@units = "mm h-1"
				end if
				
				; Write TPRCP data to simple NC file for easy access. Only for GFS.	
				system("/bin/rm -f "+ODIR+"PRCPDATA/TPRCP.f"+sprinti("%0.3i",FHR3(ttt))+".nc")
				FOUT = addfile(ODIR+"PRCPDATA/TPRCP.f"+sprinti("%0.3i",FHR3(ttt))+".nc","c")
				fAtt = True
				fAtt@title = DSOURCE+" Total Precipitation as of f"+sprinti("%0.3i",FHR3(ttt))
				fAtt@source_file = iFiles(myInd)
				fAtt@creation_date = systemfunc("date")
				fileattdef(FOUT,fAtt)
				FOUT->TPRCP = Vbase
				FOUT->lat = lat
				FOUT->lon = lon
			end if
                                
                        ;================================================================
                        ; xxx. If base variable is TPRCP or REFL, skip forecast hour 0.
                        ;================================================================
                        ;if(any((/"TPRCP","REFL"/).eq.G_BASE(0,ppp)) .and. FHR3(ttt).eq.0)then   continue   end if
                        
                        ;================================================================
                        ; xxxi. Print some extra info if requested by 'verbose'
                        ;================================================================
                        if(verbose.ge.1)then    print("MSG: Read the base variable: "+G_BASE(0,ppp))
                                                printVarSummary(Vbase)
                        end if
                        if(verbose.ge.2)then    printMaxMin(Vbase,0)
                                                print(num(ismissing(Vbase)))
                        end if
                        
                        ;*****************************************
                        ; xxxii. THIS IS THE BASE PLOT FUNCTION
                        ;*****************************************
                        if(isStrSubset(G_BASE(1,ppp),"x") .or. isStrSubset(G_BASE(1,ppp),"y"))then
                                plot    = gsn_csm_contour(wks,Vbase,res)
                        else
                                plot    = gsn_csm_contour_map(wks,Vbase,res)
                        end if
                        
                        ;================================================================
                        ; xxxiii. Get some information about the graphic size.
                        ;================================================================
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
                
                        ;================================================================
                        ; xxxvi. Print some extra info if requested by 'verbose'
                        ;================================================================
                        if(verbose.ge.1)then    print("MSG: Base plot function has finished")   end if
                
                
                        ;================================================================
                        ;================================================================
                        ; END --> BASE VARIABLE
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #1, WIND VECTORS
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; xxxv. Determine whether Overlay #1 should be added to graphic.
                        ;       If G_OV1 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV1(0,ppp).eq."")then      DO_OV1 = False
                        else                            DO_OV1 = True
                        end if
                        
                        ;================================================================
                        ; xxxvi. Enter here if Overlay #1 is active.
                        ;================================================================
                        if(DO_OV1)then
                        
                                ; 1. Print some extra info if requested by 'verbose'
                                if(verbose.ge.1)then   print("MSG: Working on wind vectors (G_OV1="+G_OV1(0,ppp)+")")   end if
                        
                                ; 2. Determine which file contains the G_OV1 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV1(0,ppp),G_OV1(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ;3. If the variable was not found
                                if(.not.varStatus)then  print("WARNING: Variable "+G_OV1(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV1(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV1 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; xxxvii. Enter here if Overlay #1 is still active.
                        ;================================================================
                        if(DO_OV1)then
                                
                                ; 1. For some variables, remove the vortex
                                ;    IN THE FUTURE, THIS SHOULDN'T BE HARD-CODED
                                if(any((/"02500850m","05000850m","07000850m"/).eq.G_OV1(1,ppp)))then
                                        myFlagsOv1 = myFlags
                                else    myFlagsOv1 = flipFlag
                                end if
                
                                ; 2. Dimension Info
                                nDims_Ov1       := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV1(0,ppp),G_OV1(1,ppp))))
                                dNames_Ov1      := getfilevardims(f,findVarName(DSOURCE,G_OV1(0,ppp),G_OV1(1,ppp)))
                                nDims_Ov1@dNames= dNames_Ov1
                                
                                ; 3. Find the correct vertical coordinate for this variable.
                                ; THIS IS ONLY IMPORTANT FOR CROSS-SECTION GRAPHICS.
                                mfOv1 = 1.
                                do ddd = 0,nDims_Ov1-1
                                        ; ISOBARIC LEVELS
                                        if(isStrSubset(str_lower(dNames_Ov1(ddd)),"isbl") .or.\
                                           isStrSubset(str_lower(dNames_Ov1(ddd)),"pres"))then
                                                LEVOv1  := (/f->$dNames_Ov1(ddd)$/)
                                                LEVOv1!0 = "lev"
                                                LEVOv1&lev = LEVOv1
                                                if(str_lower(f->$dNames_Ov1(ddd)$@units).eq."pa")then
                                                        mfOv1 = 100.
                                                        LEVOv1@units = "Pa"
                                                        LEVOv1@mf = 100.
                                                else if(str_lower(f->$dNames_Ov1(ddd)$@units.eq."hpa"))then
                                                        mfOv1 = 1.
                                                        LEVOv1@units = "hPa"
                                                        LEVOv1@mf = 1.
                                                end if  end if
                                        end if
                                end do 

                                ;================================================================
                                ; 4. GET THE OVERLAY VARIABLES. Both U and V must be read to
                                ;    produce the wind vector.
                                ;================================================================
                                if(isStrSubset(G_OV1(1,ppp),"x"))then
                                        UOv1   := getVarXC(f,DSOURCE,(/"U",G_OV1(1,ppp)/),nDims_Ov1,BOCO,mfOv1,flipFlag,lonF)
                                        VOv1   := getVarXC(f,DSOURCE,(/"W",G_OV1(1,ppp)/),nDims_Ov1,BOCO,mfOv1,flipFlag,lonF)
                                        VOv1    = 100.*ms2kts*VOv1
                                        UOv1&lev = UOv1&lev/mfOv1
                                        VOv1&lev = VOv1&lev/mfOv1
                                else if(isStrSubset(G_OV1(1,ppp),"y"))then
                                        UOv1   := getVarXC(f,DSOURCE,(/"V",G_OV1(1,ppp)/),nDims_Ov1,BOCO,mfOv1,flipFlag,lonF)
                                        VOv1   := getVarXC(f,DSOURCE,(/"W",G_OV1(1,ppp)/),nDims_Ov1,BOCO,mfOv1,flipFlag,lonF)
                                        VOv1    = 100.*ms2kts*VOv1
                                        UOv1&lev = UOv1&lev/mfOv1
                                        VOv1&lev = VOv1&lev/mfOv1
                                else
                                        UOv1    := getVar2d(f,DSOURCE,(/"U",G_OV1(1,ppp),tostring(ttt)/),nDims_Ov1,BOCO2,mfOv1,myFlagsOv1,lonF) 
                                        VOv1    := getVar2d(f,DSOURCE,(/"V",G_OV1(1,ppp),tostring(ttt)/),nDims_Ov1,BOCO2,mfOv1,myFlagsOv1,lonF)
                                end if  end if
                        end if
                
                        ;================================================================
                        ; xxxviii. Enter here if Overlay #1 is still active.
                        ;================================================================
                        if(DO_OV1)then
                                
                                ; 1. Extra resources for Wind Barbs
                                if(any((/"RVO850_Z850_UV850", "RVO850_UV200"     , "RVO700_Z700_UV700"     ,\
                                         "RH700_UV700_MSLP" , "RVO500_Z500_UV500", "RH700400_UV700400_MSLP",\
                                         "RVO200_Z200_UV200", "HGT200_UV200_MSLP", "REFD_UV750",\
					 "T850_UV850_MSLP"/).eq.FNAME(ppp)))then
                                        resOv1 := GenPlotRes(5,True)
                                        resOv1@vcLevelSelectionMode     = "ExplicitLevels"
                                        resOv1@vcLevels                 = ispan(5,150,5)
                                        if(FNAME(ppp).eq."RH700_UV700_MSLP")then
                                                resOv1@vcWindBarbColor = "black"
                                        end if
                                        if (DOMAIN.eq."d03") then   ;MKedit - add if statement for d03 windbarb
                                                resOv1@vcWindBarbLineThicknessF    = 5.0
                                                resOv1@vcMinDistanceF = 0.03
                                        end if
                                end if
                
                                ; 2. Extra resources for Curley Vectors
                                if(any((/"UV850_MSLP"/).eq.FNAME(ppp)))then
                                        resOv1 := GenPlotRes(4,True)
                                        resOv1@vcRefMagnitudeF  = 7.5
                                        resOv1@vcRefLengthF     = 0.05
                                end if
                                
                                ; 3. Extra resources
                                resOv1@gsnLeftString = ""
                                resOv1@gsnRightString = ""
                        end if
                        
                        ;================================================================
                        ; xxxxi. Enter here if Overlay #1 is still active.
                        ;================================================================
                        if(DO_OV1)then          
                                
                                ; 1. DRAW OVERLAY #1 HERE
                                plot_Ov1= gsn_csm_vector(wks,UOv1,VOv1,resOv1)
                                overlay(plot,plot_Ov1)
                        end if
                        
                        ;================================================================
                        ;================================================================
                        ; END --> OVERLAY #1, WIND VECTORS
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #2A, BOTTOM STREAMLINES
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; xl. Determine whether Overlay #2A should be added to graphic.
                        ;       If G_OV2 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV2(0,ppp).eq."")then      DO_OV2 = False
                        else                            DO_OV2 = True
                        end if
                
                        ;================================================================
                        ; xli. Enter here if Overlay #2A is active.
                        ;================================================================
                        if(DO_OV2)then
                                
                                ; 1. Print some extra info if requested by 'verbose'
                                if(verbose.ge.1)then   print("MSG: Working on bottom streamlines (G_OV2="+G_OV2(0,ppp)+")")   end if
                                
                                ; 2. Determine which file contains the G_OV2 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV2(0,ppp),G_OV2(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ; 3. If the variable was not found.
                                if(.not.varStatus)then  print("ERROR: Variable "+G_OV2(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV2(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV2 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; xlii. Enter here if Overlay #2A is still active.
                        ;================================================================
                        if(DO_OV2)then
                                
                                ; 1. For some variables, remove the vortex
                                if(any((/"02500850m","05000850m","07000850m"/).eq.G_OV2(1,ppp)))then
                                        myFlagsOv2 = myFlags
                                else    myFlagsOv2 = flipFlag
                                end if
                                
                                ; 2. Dimension Info
                                nDims_Ov2       := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV2(0,ppp),G_OV2(1,ppp))))
                                dNames_Ov2      := getfilevardims(f,findVarName(DSOURCE,G_OV2(0,ppp),G_OV2(1,ppp)))
                                nDims_Ov2@dNames= dNames_Ov2
                                
                                ; 3. Get the vertical coordinate
                                mfOv2 = 1.
                                do ddd = 0,nDims_Ov2-1
                                        ; ISOBARIC LEVELS
                                        if(isStrSubset(str_lower(dNames_Ov2(ddd)),"isbl") .or.\
                                           isStrSubset(str_lower(dNames_Ov2(ddd)),"pres"))then
                                                LEVOv2  := (/f->$dNames_Ov2(ddd)$/)
                                                LEVOv2!0 = "lev"
                                                LEVOv2&lev = LEVOv2
                                                if(str_lower(f->$dNames_Ov2(ddd)$@units).eq."pa")then
                                                        mfOv2 = 100.
                                                        LEVOv2@units    = "Pa"
                                                        LEVOv2@mf       = 100.
                                                else if(str_lower(f->$dNames_Ov2(ddd)$@units).eq."hpa")then
                                                        mfOv2 = 1.
                                                        LEVOv2@units = "hPa"
                                                        LEVOv2@mf       = 1.
                                                end if  end if
                                        end if
                                end do 
        
                                ;================================================================
                                ; 4. GET THE OVERLAY VARIABLES. Both U and V must be read to
                                ;    produce the wind streamline.
                                ;================================================================
                                if(isStrSubset(G_OV2(1,ppp),"x"))then
                                        UOv2   := getVarXC(f,DSOURCE,(/"U",G_OV2(1,ppp)/),nDims_Ov2,BOCO,mfOv2,flipFlag,lonF)
                                        VOv2   := getVarXC(f,DSOURCE,(/"W",G_OV2(1,ppp)/),nDims_Ov2,BOCO,mfOv2,flipFlag,lonF)
                                        VOv2    = 100.*ms2kts*VOv2
                                        UOv2&lev = UOv2&lev/mfOv2
                                        VOv2&lev = VOv2&lev/mfOv2
                                else if(isStrSubset(G_OV2(1,ppp),"y"))then
                                        UOv2   := getVarXC(f,DSOURCE,(/"V",G_OV2(1,ppp)/),nDims_Ov2,BOCO,mfOv2,flipFlag,lonF)
                                        VOv2   := getVarXC(f,DSOURCE,(/"W",G_OV2(1,ppp)/),nDims_Ov2,BOCO,mfOv2,flipFlag,lonF)
                                        VOv2    = 100.*ms2kts*VOv2
                                        UOv2&lev = UOv2&lev/mfOv2
                                        VOv2&lev = VOv2&lev/mfOv2
                                else
                                        UOv2    := getVar2d(f,DSOURCE,(/"U",G_OV2(1,ppp),tostring(ttt)/),nDims_Ov2,BOCO2,mfOv2,myFlagsOv2,lonF)
                                        VOv2    := getVar2d(f,DSOURCE,(/"V",G_OV2(1,ppp),tostring(ttt)/),nDims_Ov2,BOCO2,mfOv2,myFlagsOv2,lonF)
                                end if  end if
                        end if
			

                        ;================================================================
                        ; xliii. Enter here if Overlay #2A is still active.
                        ;================================================================
                        if(DO_OV2)then
                                
                                ; 1. Get Resources
                                resOv2 := GenPlotRes(6,True)
                                if(any((/"SHDL","SHML","SHSL"/).eq.FNAME(ppp)))then   resOv2@stLineColor = "black"   end if
                                if(DOMAIN.eq."d03")then   resOv2@stLineColor = "black"   end if
                                resOv2@gsnLeftString = ""
                                resOv2@gsnRightString = ""

                                ; 2. Change the streamline density here.
                                zs = getStmThin(DSOURCE,DOMAIN)
                                if(DSOURCE.eq."fvGFS_ATL")then  zs = zs*4   end if
                        end if

                        ;================================================================
                        ; xlvi. Enter here if Overlay #2 is still active.
                        ;================================================================
                        if(DO_OV2)then
                                
                                ; 1. DRAW PLOT HERE
                                plot_Ov2= gsn_csm_streamline(wks,UOv2(::zs,::zs),VOv2(::zs,::zs),resOv2)
                                overlay(plot,plot_Ov2)
                        end if
                        
                        ;================================================================
                        ;================================================================
                        ; END --> OVERLAY #2A, BOTTOM STREAMLINES
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #2B, TOP STREAMLINES
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; xlv. Determine whether Overlay #2B should be added to graphic.
                        ;       If G_OV25 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV25(0,ppp).eq."")then     DO_OV25 = False
                        else                            DO_OV25 = True
                        end if
                        
                        ;================================================================
                        ; xlvi. Enter here if Overlay #2B is active.
                        ;================================================================
                        if(DO_OV25)then

                                ; 1. Print some extra info if requested by 'verbose'
                                if(verbose.ge.1)then   print("MSG: Working on bottom streamlines (G_OV25="+G_OV25(0,ppp)+")")   end if
                                
                                ; 2. Determine which file contains the G_OV25 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV25(0,ppp),G_OV25(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ; 3. If the variable was not found. 
                                if(.not.varStatus)then  print("ERROR: Variable "+G_OV25(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV25(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV25 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; xlvii. Enter here if Overlay #2B is still active.
                        ;================================================================
                        if(DO_OV25)then
                                
                                ; 1. For some variables, remove the vortex
                                if(any((/"02500850m","05000850m","07000850m"/).eq.G_OV25(1,ppp)))then
                                        myFlagsOv25 = myFlags
                                else    myFlagsOv25 = flipFlag
                                end if
                                
                                ; 2. Dimension Info
                                nDims_Ov25      := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV25(0,ppp),G_OV25(1,ppp))))
                                dNames_Ov25     := getfilevardims(f,findVarName(DSOURCE,G_OV25(0,ppp),G_OV25(1,ppp)))
                                nDims_Ov25@dNames= dNames_Ov25
                                
                                ; 3. Get the vertical coordinate
                                mfOv25 = 1.
                                do ddd = 0,nDims_Ov25-1
                                        ; ISOBARIC LEVELS
                                        if(isStrSubset(str_lower(dNames_Ov25(ddd)),"isbl") .or.\
                                           isStrSubset(str_lower(dNames_Ov25(ddd)),"pres"))then
                                                LEVOv25 := (/f->$dNames_Ov25(ddd)$/)
                                                LEVOv25!0 = "lev"
                                                LEVOv25&lev = LEVOv25
                                                if(str_lower(f->$dNames_Ov25(ddd)$@units).eq."pa")then
                                                        mfOv25 = 100.
                                                        LEVOv25@units   = "Pa"
                                                        LEVOv25@mf      = 100.
                                                else if(str_lower(f->$dNames_Ov25(ddd)$@units).eq."hpa")then
                                                        mfOv25 = 1.
                                                        LEVOv25@units = "hPa"
                                                        LEVOv25@mf      = 1.
                                                end if  end if
                                        end if
                                end do 
        
                                ;================================================================
                                ; 4. GET THE OVERLAY VARIABLES. Both U and V must be read to
                                ;    produce the wind streamline.
                                ;================================================================
                                if(isStrSubset(G_OV25(1,ppp),"x"))then
                                        UOv25   := getVarXC(f,DSOURCE,(/"U",G_OV25(1,ppp)/),nDims_Ov25,BOCO,mfOv25,flipFlag,lonF)
                                        VOv25   := getVarXC(f,DSOURCE,(/"W",G_OV25(1,ppp)/),nDims_Ov25,BOCO,mfOv25,flipFlag,lonF)
                                        VOv25   = 100.*ms2kts*VOv25
                                        UOv25&lev = UOv25&lev/mfOv25
                                        VOv25&lev = VOv25&lev/mfOv25
                                else if(isStrSubset(G_OV25(1,ppp),"y"))then
                                        UOv25   := getVarXC(f,DSOURCE,(/"V",G_OV25(1,ppp)/),nDims_Ov25,BOCO,mfOv25,flipFlag,lonF)
                                        VOv25   := getVarXC(f,DSOURCE,(/"W",G_OV25(1,ppp)/),nDims_Ov25,BOCO,mfOv25,flipFlag,lonF)
                                        VOv25   = 100.*ms2kts*VOv25
                                        UOv25&lev = UOv25&lev/mfOv25
                                        VOv25&lev = VOv25&lev/mfOv25
                                else
                                        UOv25   := getVar2d(f,DSOURCE,(/"U",G_OV25(1,ppp),tostring(ttt)/),nDims_Ov25,BOCO2,mfOv25,myFlagsOv25,lonF)
                                        VOv25   := getVar2d(f,DSOURCE,(/"V",G_OV25(1,ppp),tostring(ttt)/),nDims_Ov25,BOCO2,mfOv25,myFlagsOv25,lonF)
                                end if  end if
                        end if

                        ;================================================================
                        ; xlviii. Enter here if Overlay #2B is still active.
                        ;================================================================
                        if(DO_OV25)then
                        
                                ; 1. Get Resources
                                resOv25 := GenPlotRes(6,True)
                                resOv25@gsnLeftString = ""
                                resOv25@gsnRightString = ""
                                if(any((/"SHDL","SHML","SHSL"/).eq.FNAME(ppp)))then   resOv25@stLineColor = "black"   end if
                                if(DOMAIN.eq."d03")then   
                                        resOv25@stLineColor = "grey90" ;"white"   
                                        resOv25@stLineThicknessF = 3.5
                                        resOv25@stMinDistanceF     = 0.05
                                        resOv25@stMinArrowSpacingF = 0.05
                                        ;resOv25@stLineStartStride  = 14
                                end if
                        
                                ; 2. Change the streamline density here.
                                zs = getStmThin(DSOURCE,DOMAIN)
                                if(DSOURCE.eq."fvGFS_ATL")then  zs = zs*4   end if
                        end if
                        
                        ;================================================================
                        ; xlix. Enter here if Overlay #2B is still active.
                        ;================================================================
                        if(DO_OV25)then

                                ; 1. DRAW PLOT HERE
                                plot_Ov25= gsn_csm_streamline(wks,UOv25(::zs,::zs),VOv25(::zs,::zs),resOv25)
                                overlay(plot,plot_Ov25)
                        end if
                        
                        ;================================================================
                        ;================================================================
                        ; END --> OVERLAY #2B, TOP STREAMLINES
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #3, BOTTOM CONTOUR LINES
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; l. Determine whether Overlay #3 should be added to graphic.
                        ;       If G_OV3 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV3(0,ppp).eq."")then      DO_OV3 = False
                        else                            DO_OV3 = True
                        end if
                        
                        ;================================================================
                        ; li. Enter here if Overlay #3 is active.
                        ;================================================================
                        if(DO_OV3)then
                
                                ; 1. Print some extra info if requested by 'verbose'
                                if(verbose.ge.1)then   print("MSG: Working on bottom contour lines (G_OV3="+G_OV3(0,ppp)+")")   end if
                        
                                ; 2. Determine which file contains the G_OV3 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV3(0,ppp),G_OV3(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ; 3. If the variable can't be found.
                                if(.not.varStatus)then  print("WARNING: Variable "+G_OV3(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV3(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV3 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; lii. Enter here if Overlay #3 is still active.
                        ;================================================================
                        if(DO_OV3)then
                                
                                ; 1. For some variables, remove the vortex
                                if(any((/"NO_VARIABLES_RIGHT_NOW"/).eq.G_OV3(1,ppp)))then
                                        myFlagsOv3 = myFlags
                                else    myFlagsOv3 = flipFlag
                                end if
                                
                                ; 2. Dimension Info
                                nDims_Ov3       := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV3(0,ppp),G_OV3(1,ppp))))
                                dNames_Ov3      := getfilevardims(f,findVarName(DSOURCE,G_OV3(0,ppp),G_OV3(1,ppp)))
                                nDims_Ov3@dNames= dNames_Ov3
                                
                                ; 3. Get the vertical coordinate
                                mfOv3 = 1.
                                do ddd = 0,nDims_Ov3-1
                                        ; ISOBARIC LEVELS
                                        if(isStrSubset(str_lower(dNames_Ov3(ddd)),"isbl") .or.\
                                           isStrSubset(str_lower(dNames_Ov3(ddd)),"pres"))then
                                                LEVOv3  := (/f->$dNames_Ov3(ddd)$/)
                                                LEVOv3!0 = "lev"
                                                LEVOv3&lev = LEVOv3
                                                if(f->$dNames_Ov3(ddd)$@units.eq."Pa")then
                                                        mfOv3 = 100.
                                                        LEVOv3@units    = "Pa"
                                                        LEVOv3@mf       = 100.
                                                else if(f->$dNames_Ov3(ddd)$@units.eq."hPa")then
                                                        mfOv3 = 1.
                                                        LEVOv3@units = "hPa"
                                                        LEVOv3@mf       = 1.
                                                end if  end if
                                        end if
                                end do 
        
                                ;================================================================
                                ; 4. GET THE OVERLAY VARIABLE.
                                ;================================================================
                                if(isStrSubset(G_OV3(1,ppp),"x") .or. isStrSubset(G_OV3(1,ppp),"y"))then
                                        VOv3   := getVarXC(f,DSOURCE,G_OV3(:,ppp),nDims_Ov3,BOCO,mfOv3,myFlagsOv3,lonF)
                                        VOv3&lev = VOv3&lev/mfOv3
                                else
                                        VOv3    := getVar2d(f,DSOURCE,(/G_OV3(0,ppp),G_OV3(1,ppp),tostring(ttt)/),nDims_Ov3,BOCO2,mfOv3,myFlagsOv3,lonF)
                                end if
                        end if
                        
                        ;================================================================
                        ; liii. Enter here if Overlay #3 is still active.
                        ;================================================================
                        if(DO_OV3)then
                        
                                ; 1. Plotting Resources
                                resOv3  := GenPlotRes(1,True)
                                if(G_OV3(0,ppp).eq."HGT" .and. G_OV3(1,ppp).eq."05001000d")then
                                        resOv3@cnLevelSelectionMode     = "ExplicitLevels"
                                        resOv3@cnLevels                 = fspan(510.,588.,14)
                                        resOv3@cnLineLabelInterval      = 1
                                        resOv3@cnMonoLineColor          = False
                                        resOv3@cnLineColors             = (/"blue3","blue3","blue3","blue3",\
                                                                            "blue3","blue3", "firebrick", "firebrick",\
                                                                             "firebrick", "firebrick", "firebrick", "firebrick",\
                                                                             "firebrick", "firebrick"/)
                                        resOv3@cnMonoLineLabelFontColor = False
                                        resOv3@cnLineLabelFontColors    = resOv3@cnLineColors
                                                                        ;(/"blue","blue","blue","blue",\
                                                                         ;   "blue","blue", "red", "red",\
                                                                          ;   "red", "red", "red", "red",\
                                                                           ;  "red", "red"/)
                                        resOv3@cnMonoLineDashPattern    = True
                                        resOv3@cnLineDashPattern        = 5
                                        resOv3@cnLineThicknessF         = 3.0
                                        
                                end if
                                if(FNAME(ppp).eq."RH700_UV700_MSLP")then
                                        resOv3@cnLineColor = "black"
                                end if
                        end if
                        
                        ;================================================================
                        ; liv. Enter here if Overlay #3 is still active.
                        ;================================================================
                        if(DO_OV3)then
                                
                                ; 1. DRAW PLOT HERE
                                plot_Ov3= gsn_csm_contour(wks,VOv3,resOv3)
                                overlay(plot,plot_Ov3)
                        end if
                        
                        ;================================================================
                        ;================================================================
                        ; END --> OVERLAY #3, BOTTOM CONTOUR LINES
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #4, TOP CONTOUR LINES
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; lv. Determine whether Overlay #4 should be added to graphic.
                        ;       If G_OV4 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV4(0,ppp).eq."")then      DO_OV4 = False
                        else                            DO_OV4 = True
                        end if
                        
                        ;================================================================
                        ; lvi. Enter Overlay #4 part one.
                        ;================================================================
                        if(DO_OV4)then
                        
                                ; 1. Print some extra info if requested by 'verbose'
                                if(verbose.ge.1)then   print("MSG: Working on top contour lines (G_OV4="+G_OV4(0,ppp)+")")   end if
                                
                                ; 2. Determine which file contains the G_OV4 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV4(0,ppp),G_OV4(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ;3. If the variable can't be found.
                                if(.not.varStatus)then  print("ERROR: Variable "+G_OV4(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV4(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV4 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; lvii. Enter Overlay #4 part two.
                        ;================================================================
                        if(DO_OV4)then
                                
                                ; 1. For some variables, remove the vortex
                                if(any((/"NO_VARIABLES_RIGHT_NOW"/).eq.G_OV4(1,ppp)))then
                                        myFlagsOv4 = myFlags
                                else    myFlagsOv4 = flipFlag
                                end if
                                
                                ; 2. Dimension Info
                                nDims_Ov4       := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV4(0,ppp),G_OV4(1,ppp))))
                                dNames_Ov4      := getfilevardims(f,findVarName(DSOURCE,G_OV4(0,ppp),G_OV4(1,ppp)))
                                nDims_Ov4@dNames= dNames_Ov4
                                
                                
                                ; 3. Get the vertical coordinate
                                mfOv4 = 1.
                                do ddd = 0,nDims_Ov4-1
                                        ; ISOBARIC LEVELS
                                        if(isStrSubset(str_lower(dNames_Ov4(ddd)),"isbl") .or.\
                                           isStrSubset(str_lower(dNames_Ov4(ddd)),"pres"))then
                                                LEVOv4  := (/f->$dNames_Ov4(ddd)$/)
                                                LEVOv4!0 = "lev"
                                                LEVOv4&lev = LEVOv4
                                                if(str_lower(f->$dNames_Ov4(ddd)$@units).eq."pa")then
                                                        mfOv4 = 100.
                                                        LEVOv4@units    = "Pa"
                                                        LEVOv4@mf       = 100.
                                                else if(str_lower(f->$dNames_Ov4(ddd)$@units).eq."hpa")then
                                                        mfOv4 = 1.
                                                        LEVOv4@units = "hPa"
                                                        LEVOv4@mf       = 1.
                                                end if  end if
                                        end if
                                end do 
        
                                ;================================================================
                                ; 4. GET THE OVERLAY VARIABLE.
                                ;================================================================
                                if(isStrSubset(G_OV4(1,ppp),"x") .or. isStrSubset(G_OV4(1,ppp),"y"))then
                                        VOv4   := getVarXC(f,DSOURCE,G_OV4(:,ppp),nDims_Ov4,BOCO,mfOv4,myFlagsOv4,lonF)
                                        VOv4&lev = VOv4&lev/mfOv4
                                else
                                        VOv4    := getVar2d(f,DSOURCE,(/G_OV4(0,ppp),G_OV4(1,ppp),tostring(ttt)/),nDims_Ov4,BOCO2,mfOv4,myFlagsOv4,lonF)
                                end if
                        end if
                        
                        ;================================================================
                        ; lviii. Enter Overlay #4 part three.
                        ;================================================================
                        if(DO_OV4)then

                                ; 1. Plotting Resources
                                resOv4 := GenPlotRes(1,True)
                                if(G_OV4(0,ppp).eq."HGT")then
                                        if(G_OV4(1,ppp).eq."850")then
                                                resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                                resOv4@cnLevels                 = fspan(130.,180.,11)
                                                resOv4@cnLineLabelInterval      = 1
                                        end if
                                        if(G_OV4(1,ppp).eq."700")then
                                                resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                                resOv4@cnLevels                 = fspan(272.,340.,18)
                                                resOv4@cnLineLabelInterval      = 1
                                        end if
                                        if(G_OV4(1,ppp).eq."500")then
                                                resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                                resOv4@cnLevels                 = fspan(528.,594.,12)
                                                resOv4@cnLineLabelInterval      = 1
                                        end if
                                        resOv4@cnLineThicknessF         = 4.0
                                end if
                                if(G_OV4(0,ppp).eq."MSLP")then
                                        resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                        resOv4@cnLevels                 = fspan(980.,1040.,31)
                                        ;resOv4@cnLineLabelInterval     = 2
                                        resOv4@cnLineThicknessF         = 2.
                                        resOv4@cnLineLabelsOn           = False
                                end if
                                if(G_OV4(0,ppp).eq."RVO")then
                                        resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                        resOv4@cnLevels                 = fspan(2.,50.,13)
                                        resOv4@cnLineLabelInterval      = 2
                                end if
                                if(G_OV4(0,ppp).eq."HLCY")then
                                        resOv4@cnLevelSelectionMode     = "ExplicitLevels"
                                        resOv4@cnLevels                 = fspan(100.,1000.,10)
                                        resOv4@cnLineLabelInterval      = 2
                                        resOv4@cnLineColor              = "blue4" 
                                        resOv4@cnLineLabelFontColor     = resOv4@cnLineColor
                                        resOv4@cnLineThicknessF         = 3.0
                                end if
                                ;if(any((/"RH700_UV700_MSLP","RH700400_UV700400_MSLP","HGT500_MSLP"/).eq.FNAME(ppp)))then
                                ;       resOv4@cnLineColor      = "white" ;"black"
                                ;end if
                                if(G_BASE(0,ppp).eq."RH".or.G_BASE(0,ppp).eq."PRCP")then ;MK
                                        resOv4@cnLineColor      = "gray50"
                                        resOv4@cnLineThicknessF = 2.5
                                end if
                                if(G_BASE(0,ppp).eq."HGT")then ;MK
                                        resOv4@cnLineColor      = "blue3"
                                end if
                        end if
                        
                        ;================================================================
                        ; lix. Enter Overlay #4 part four.
                        ;================================================================
                        if(DO_OV4)then
                        
                                ; 1. DRAW PLOT HERE
                                plot_Ov4= gsn_csm_contour(wks,VOv4,resOv4)
                                overlay(plot,plot_Ov4)
                        end if
                        
                        ;================================================================
                        ;================================================================
                        ; END --> OVERLAY #4, TOP CONTOUR LINES
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #5, MSLP MARKERS (H/L)
                        ;================================================================
                        ;================================================================
                        
                        ;================================================================
                        ; lx. Determine whether Overlay #5 should be added to graphic.
                        ;       If G_OV5 is blank, then this will be skipped.
                        ;================================================================
                        if(G_OV5(0,ppp).eq."")then      DO_OV5 = False
                        else                            DO_OV5 = True
                        end if
                        
                        ;================================================================
                        ; lxi. Enter Overlay #5 part one.
                        ;================================================================
                        if(DO_OV5 .and. .not.MSLP_MARKERS)then

                                ; 1. Check that the variable is MSLP.
				if(G_OV5(0,ppp).ne."MSLP")then
					print("ERROR: MSLP Markers must be created from MSLP.")
					print("ERROR: Please change this in the variable namelist.")
					exit
				end if
				
				; 2. Print some extra info if requested by 'verbose'	
                                if(verbose.ge.1)then    print("MSG: Working on MSLP markers (G_OV5="+G_OV5(0,ppp)+")")
                                                        print("MSG: First request for MSLP markers for the current set of graphics.")
                                                        print("MSG: Calculating MSLP markers.")
                                end if
                        
                                ; 3. Determine which file contains the G_OV5 variable
                                varStatus = False
                                do aaa = 0,dimsizes(myInd)-1
                                        if(aaa.eq.0)then        f := f1
                                        else if(aaa.eq.1)then   f := f2
                                        else if(aaa.eq.2)then   f := f3
                                        else if(aaa.eq.3)then   f := f4
                                        end if  end if  end if  end if
                                        if(isfilevar(f,findVarName(DSOURCE,G_OV5(0,ppp),G_OV5(1,ppp))))then
                                                varStatus = True
                                                break
                                        end if
                                end do
                                
                                ; 4. If the variable is not found.
                                if(.not.varStatus)then  print("WARNING: Variable "+G_OV5(0,ppp)+" not found.")
                                                        print("WARNING: "+G_OV5(0,ppp)+" will be skipped, but will attempt to create the rest of the graphic.")
                                                        DO_OV5 = False
                                end if
                        end if
                        
                        ;================================================================
                        ; lxi. Enter Overlay #5 part two.
                        ;================================================================
                        if(DO_OV5 .and. .not.MSLP_MARKERS)then
                                
                                ; 1. Dimension Info
                                nDims_Ov5       := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,G_OV5(0,ppp),G_OV5(1,ppp))))
                                dNames_Ov5      := getfilevardims(f,findVarName(DSOURCE,G_OV5(0,ppp),G_OV5(1,ppp)))
                                nDims_Ov5@dNames= dNames_Ov5
        
                                ;================================================================
                                ; 2. GET THE OVERLAY VARIABLE.
                                ;================================================================
                                VOv5 := getVar2d(f,DSOURCE,(/G_OV5(0,ppp),G_OV5(1,ppp),tostring(ttt)/),nDims_Ov5,BOCO2,mf,flipFlag,lonF)
			
			end if
                        
                        ;================================================================
                        ; lxii. Enter Overlay #5 part three.
                        ;================================================================
                        if(DO_OV5)then

                                ; 1. Remove high terrain, if necessary.
				VOv5B   := VOv5
				
				; 2. Determine which file contains the PSFC variable
				varStatus = False
				do aaa = 0,dimsizes(myInd)-1
					if(aaa.eq.0)then        f := f1
					else if(aaa.eq.1)then   f := f2
					else if(aaa.eq.2)then   f := f3
					else if(aaa.eq.3)then   f := f4
					end if  end if  end if  end if
					if(isfilevar(f,findVarName(DSOURCE,"PSFC","")))then
						varStatus = True
						break
					end if
				end do
					
				; 3. If the variable is available.
				if(varStatus)then	nDims_PSFC      := dimsizes(getfilevardimsizes(f,findVarName(DSOURCE,"PSFC","")))
							dNames_PSFC     := getfilevardims(f,findVarName(DSOURCE,"PSFC",""))
							nDims_PSFC@dNames= dNames_PSFC
							PSFC    := getVar2d(f,DSOURCE,(/"PSFC","",tostring(ttt)/),nDims_PSFC,BOCO2,mf,flipFlag,lonF)
							copy_VarMeta(VOv5B,PSFC)
							; Do not mark H/L above .9 sigma surface
							VOv5B   = where(PSFC/VOv5.le.0.90,fval3,VOv5B)
				end if
				if(lat(0).ne.VOv5B&lat(0))then   VOv5B := VOv5B(::--1,:)   end if
				if(lon(0).ne.VOv5B&lon(0))then   VOv5B := VOv5B(:,::--1)   end if
					
				; 4. Apply metadata to new variable
				VOv5B!0 = "lat"
				VOv5B!1 = "lon"
				VOv5B&lat = lat
				VOv5B&lon = lon
                                                                        
				; 5. Get some information about how to place Highs and Lows
				;    llbox -  minimum distance that the closest High/Low can be
				;    maxH -  maximum number of Highs to be marked on the map
				;    maxL -  maximum number of Lows to be marked on the map
				llbox = getDmnInfo(DOMAIN,"llbox",False)
				dx = abs(lon(1)-lon(0))
				dxi = floor(llbox/dx)
				if(dxi.eq.0)then   print("WARNING: 'llbox' is smaller than 'dx'. Defaulting to 1.")   end if
				maxH = getDmnInfo(DOMAIN,"maxH",False)
				maxL = getDmnInfo(DOMAIN,"maxL",False)
			end if
			
			;================================================================
                        ; lxiii. Enter Overlay #5 part four.
                        ;================================================================
                        if(DO_OV5 .and. .not.MSLP_MARKERS)then

				; 1. Find the local High Pressure locations.
				;    These aren't necessary for 'd03' graphics.
				if(DOMAIN.ne."d03")then
                                        
					;Find the local Highs. Could be 0
					VOv5_max := local_max(VOv5B, False, 0.0)
					
					if(VOv5_max.gt.0)then
						VOv5_max@_FillValue = fval2
						
						; Reorder Highs from highest pressure to lowest
						ip              := dim_pqsort(VOv5_max@maxval,-2)
						VOv5_max@xi     := VOv5_max@xi(ip)
						VOv5_max@yi     := VOv5_max@yi(ip)
						
						; Only retain Highs greater than 1012 hPa
						vals := ind(VOv5_max@maxval.ge.1012.)
						if(.not.ismissing(vals(0)))then
							VOv5_max@maxval := VOv5_max@maxval(vals)
							VOv5_max@xi     := VOv5_max@xi(vals)
							VOv5_max@yi     := VOv5_max@yi(vals)
							VOv5_max        = dimsizes(VOv5_max@maxval)
						else
							print("WARNING: No Highs >= 1012 hPa found.")
							VOv5_max@maxval := fval3
							VOv5_max@xi     := fval2
							VOv5_max@yi     := fval2
							VOv5_max        = dimsizes(VOv5_max@maxval)
						end if
						
						; Redefine maxH if there are fewer Highs
						if(VOv5_max.lt.maxH)then   maxH = VOv5_max   end if
						
						; Loop over all Highs to find best options for the graphic
						; m1 counts the valid Highs for the graphic
						m1 = 0
						do mmm = 0,VOv5_max-1
						
							; Skip if the current index is already set to missing
							if(VOv5_max@maxval(mmm).eq.fval3)then   continue   end if
							
							; Don't use if the High is too close to the boundary
							if(lon(VOv5_max@xi(mmm)).lt.(BOCO(2)+3.*pctLon) .or. \
							   lon(VOv5_max@xi(mmm)).gt.(BOCO(3)-3.*pctLon) .or. \
							   lat(VOv5_max@yi(mmm)).gt.(BOCO(0)-5.*pctLat) .or. \
							   lat(VOv5_max@yi(mmm)).lt.(BOCO(1)+3.*pctLat))then
								VOv5_max@maxval(mmm)    = fval3
								VOv5_max@xi(mmm)        = fval2
								VOv5_max@yi(mmm)        = fval2
								continue
							end if
							
							; Search in the x-direction for weaker Highs that are within
							; the minimum distance ''llbox'. ONly do this if more than one
							; High was found.
							if(mmm.lt.VOv5_max-1)then
								vals := ind(VOv5_max@xi(mmm+1:).ge.VOv5_max@xi(mmm)-dxi .and. \
									    VOv5_max@xi(mmm+1:).le.VOv5_max@xi(mmm)+dxi .and. \
									    VOv5_max@yi(mmm+1:).ge.VOv5_max@yi(mmm)-dxi .and. \
									    VOv5_max@yi(mmm+1:).le.VOv5_max@yi(mmm)+dxi)
								vals = mmm+1+vals
								if(.not.ismissing(vals(0)))then
									VOv5_max@maxval(vals)   = fval3
									VOv5_max@xi(vals)       = fval2
									VOv5_max@yi(vals)       = fval2
								end if
							end if
								
							; OK, made it this far so this is a good High for the graphic
							VOv5_max@maxval(mmm) = VOv5B(VOv5_max@yi(mmm),VOv5_max@xi(mmm))
							m1 = m1+1
								
							; If the limit maxH has been met, set all other values to missing
							; and break the loop.
							if(m1.ge.maxH .and. mmm.lt.VOv5_max-1)then
								VOv5_max@maxval(mmm+1:) = fval3
								VOv5_max@xi(mmm+1:)     = fval2
								VOv5_max@yi(mmm+1:)     = fval2
								break
							end if
						end do
							
						; Remove the missing values for easier graphic production
						vals := ind(VOv5_max@maxval.ne.fval3)
						if(.not.ismissing(vals(0)))then
							VOv5_max@maxval := VOv5_max@maxval(vals)
							VOv5_max@xi := VOv5_max@xi(vals)
							VOv5_max@yi := VOv5_max@yi(vals)
						end if
					end if
				else
					; No Highs allowed for the D03 graphic centered on a TC
					VOv5_max = 0
				end if

				; 2. Find the local Low Pressure locations.
				VOv5_min        = local_min(VOv5B, False, 0.0)
				
				if(VOv5_min.gt.0)then
					VOv5_min@_FillValue = fval2
					
					; Reorder Lows from lowest pressure to highest
					ip              := dim_pqsort(VOv5_min@minval,2)
					VOv5_min@xi     = VOv5_min@xi(ip)
					VOv5_min@yi     = VOv5_min@yi(ip)
					
					; Only retain Lows lower than 1012 hPa
					vals := ind(VOv5_min@minval.le.1012.)
					if(.not.ismissing(vals(0)))then
						VOv5_min@minval := VOv5_min@minval(vals)
						VOv5_min@xi     := VOv5_min@xi(vals)
						VOv5_min@yi     := VOv5_min@yi(vals)
						VOv5_min        = dimsizes(VOv5_min@minval)
					else
						print("WARNING: No Lows <= 1012 hPa found.")
						VOv5_min@minval := fval3
						VOv5_min@xi     := fval2
						VOv5_min@yi     := fval2
						VOv5_min        = dimsizes(VOv5_min@minval)
					end if
						
					; Redefine maxL if there are fewer Highs
					if(VOv5_min.lt.maxL)then   maxL = VOv5_min   end if
					
					; Loop over all Lows to find best options for the graphic
					; m1 counts the valid Lows for the graphic
					m1 = 0
					do mmm = 0,VOv5_min-1
					
						; Skip if the current index is already set to missing
						if(VOv5_min@minval(mmm).eq.fval3)then   continue   end if
						
						; Don't use if the Low is too close to the boundary
						if(lon(VOv5_min@xi(mmm)).lt.(BOCO(2)+3.*pctLon) .or. \
						   lon(VOv5_min@xi(mmm)).gt.(BOCO(3)-3.*pctLon) .or. \
						   lat(VOv5_min@yi(mmm)).gt.(BOCO(0)-5.*pctLat) .or. \
						   lat(VOv5_min@yi(mmm)).lt.(BOCO(1)+3.*pctLat))then
							VOv5_min@minval(mmm)    = fval3
							VOv5_min@xi(mmm)        = fval2
							VOv5_min@yi(mmm)        = fval2
							continue
						end if
						
						; Search in the x-direction for weaker Lows that are within
						; the minimum distance ''llbox'. Only do this if more than one
						; Low was found.
						if(mmm.lt.VOv5_min-1)then
							vals := ind(VOv5_min@xi(mmm+1:).ge.VOv5_min@xi(mmm)-dxi .and. \
								    VOv5_min@xi(mmm+1:).le.VOv5_min@xi(mmm)+dxi .and. \
								    VOv5_min@yi(mmm+1:).ge.VOv5_min@yi(mmm)-dxi .and. \
								    VOv5_min@yi(mmm+1:).le.VOv5_min@yi(mmm)+dxi)
							vals = mmm+1+vals
							if(.not.ismissing(vals(0)))then
								VOv5_min@minval(vals)   = fval3
								VOv5_min@xi(vals)       = fval2
								VOv5_min@yi(vals)       = fval2
							end if
						end if
						
						; OK, made it this far so this is a good Low for the graphic
						VOv5_min@minval(mmm) = VOv5B(VOv5_min@yi(mmm),VOv5_min@xi(mmm))
						m1 = m1+1
						
						; If the limit maxL has been met, set all other values to missing
						; and break the loop.
						if(m1.ge.maxL .and. mmm.lt.VOv5_min-1)then
							VOv5_min@minval(mmm+1:) = fval3
							VOv5_min@xi(mmm+1:)     = fval2
							VOv5_min@yi(mmm+1:)     = fval2
							break
						end if
					end do
					
					; Remove the missing values for easier graphic production
					vals := ind(VOv5_min@minval.ne.fval3)
					if(.not.ismissing(vals(0)))then
						VOv5_min@minval := VOv5_min@minval(vals)
						VOv5_min@xi := VOv5_min@xi(vals)
						VOv5_min@yi := VOv5_min@yi(vals)
					end if
				end if
                        
                        
				; 3. Acknowledge that MSLP Markers have been created for this input file and time.
				;    They will not need to be recreated for any future graphics with the same
				;    input file and time.
				MSLP_MARKERS = True
				
			end if
			
			;================================================================
                        ; lxiv. Enter Overlay #5 part five.
                        ;================================================================
                        if(DO_OV5)then
                        
                        	; 1. Define Text Resources
                        	txres                   := True
	                        txres@txJust            = "CenterCenter"
	                        txres@txPerimOn         = False
	                        txres@txPerimColor      = "black"
                        
	                        txres1                  := txres
	                        txres1@txFontHeightF    = 0.012
	                        txres1@txFontColor      = "blue"
	                        txres1@txFont           = 30
                        
	                        txres2                  := txres
	                        txres2@txFontHeightF    = 0.012
	                        txres2@txFontColor      = "white"       ;MKedit "black"
	                        txres2@txFont           = 130
	                        txres2@txFontThicknessF = 4.0           ;MKedit 5.0
                        
	                        txres3                  := txres
	                        txres3@txFontHeightF    = 0.008
	                        txres3@txFontColor      = "blue"
	                        txres3@txFont           = 26
                        
	                        txres4                  := txres
	                        txres4@txFontHeightF    = 0.008
	                        txres4@txFontColor      = "white"       ;MKedit "black"
	                        txres4@txFont           = 126
	                        txres4@txFontThicknessF = 4.0           ;MKedit 5.0
                                
	                        txres5                  := txres
	                        txres5@txFontHeightF    = 0.012
	                        txres5@txFontColor      = "red"
	                        txres5@txFont           = 30
                        
	                        txres6                  := txres
	                        txres6@txFontHeightF    = 0.012
	                        txres6@txFontColor      = "white"       ;MKedit "black"
	                        txres6@txFont           = 130
	                        txres6@txFontThicknessF = 4.0           ;MKedit 5.0
                        
	                        txres7                  := txres
	                        txres7@txFontHeightF    = 0.008
	                        txres7@txFontColor      = "red"
	                        txres7@txFont           = 26
                        
	                        txres8                  := txres
	                        txres8@txFontHeightF    = 0.008
	                        txres8@txFontColor      = "white"       ;MKedit "black"
	                        txres8@txFont           = 126
	                        txres8@txFontThicknessF = 4.0           ;MKedit 5.0
                        

	                        ; 2. THE TEXT OVERLAY FUNCTIONS
	                        if(pctLon/pctLat.gt.1.5)then            HLpct = 5.0
	                        else if(pctLon/pctLat.le.1.5\
	                          .and. pctLon/pctLat.ge.0.5)then       HLpct = 3.0
	                        else if(pctLon/pctLat.lt.0.5)then       HLpct = 2.0
	                        end if  end if  end if
			end if
			
			;================================================================
                        ; lxv. Enter Overlay #5 part six.
                        ;================================================================
                        if(DO_OV5)then
                                
				; 1. OVERLAY THE HIGH PRESSURE MARKERS
                        	if(VOv5_max.gt.0)then
                                	if(any(VOv5_max@xi.ne.fval2) .and. any(VOv5_max@yi.ne.fval2))then
	                                        TEXT01  := gsn_add_text(wks,plot,"H",lon(VOv5_max@xi(ind(VOv5_max@xi.ne.fval2))),\
	                                                                             lat(VOv5_max@yi(ind(VOv5_max@yi.ne.fval2))),txres1)
	                                        TEXT02  := gsn_add_text(wks,plot,"H",lon(VOv5_max@xi(ind(VOv5_max@xi.ne.fval2))),\
	                                                                             lat(VOv5_max@yi(ind(VOv5_max@yi.ne.fval2))),txres2)
	                                        TEXT03  := gsn_add_text(wks,plot,toint(VOv5_max@maxval(ind(VOv5_max@maxval.ne.fval2))),\
	                                                                         lon(VOv5_max@xi(ind(VOv5_max@xi.ne.fval2))),\
	                                                                         lat(VOv5_max@yi(ind(VOv5_max@yi.ne.fval2)))+HLpct*pctLat,txres3)
	                                        TEXT04  := gsn_add_text(wks,plot,toint(VOv5_max@maxval(ind(VOv5_max@maxval.ne.fval2))),\
	                                                                         lon(VOv5_max@xi(ind(VOv5_max@xi.ne.fval2))),\
	                                                                         lat(VOv5_max@yi(ind(VOv5_max@yi.ne.fval2)))+HLpct*pctLat,txres4)
	                                end if
	                        end if
				
				; 2. OVERLAY THE LOW PRESSURE MARKERS
	                        if(VOv5_min.gt.0)then
	                                if(any(VOv5_min@xi.ne.fval2) .and. any(VOv5_min@yi.ne.fval2))then
	                                        TEXT05  := gsn_add_text(wks,plot,"L",lon(VOv5_min@xi(ind(VOv5_min@xi.ne.fval2))),\
	                                                                            lat(VOv5_min@yi(ind(VOv5_min@yi.ne.fval2))),txres5)
	                                        TEXT06  := gsn_add_text(wks,plot,"L",lon(VOv5_min@xi(ind(VOv5_min@xi.ne.fval2))),\
	                                                                            lat(VOv5_min@yi(ind(VOv5_min@yi.ne.fval2))),txres6)
	                                        TEXT07  := gsn_add_text(wks,plot,toint(VOv5_min@minval(ind(VOv5_min@minval.ne.fval2))),\
	                                                                        lon(VOv5_min@xi(ind(VOv5_min@xi.ne.fval2))),\
	                                                                        lat(VOv5_min@yi(ind(VOv5_min@yi.ne.fval2)))+HLpct*pctLat,txres7)
	                                        TEXT08  := gsn_add_text(wks,plot,toint(VOv5_min@minval(ind(VOv5_min@minval.ne.fval2))),\
	                                                                        lon(VOv5_min@xi(ind(VOv5_min@xi.ne.fval2))),\
	                                                                        lat(VOv5_min@yi(ind(VOv5_min@yi.ne.fval2)))+HLpct*pctLat,txres8)
	                                end if
	                        end if
	                end if
			
			;================================================================
                        ;================================================================
                        ; END --> OVERLAY #5, MSLP MARKERS (H/L)
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #6, STORM LABELS
                        ;================================================================
                        ;================================================================
                
                	;================================================================
                        ; lxvi. Storm labels require at least one ATCF file and for the
			;       graphic to not be storm-centered.
			;================================================================
	                if(ATCF_FILES(0).ne."NONE" .and. .not.SC_GRAPHICS)then
	                ;if(isStrSubset(DSOURCE,"HWRF") .or. DSOURCE.eq."GFS" .or. isStrSubset(str_lower(DSOURCE),"fvgfs"))then
                        
				; Loop over all available storms
	                        do sss = 0,nAll-1
	                                ; Get TC Info for current time
	                                myInd2  = ind(tcFHR(sss,:).eq.toint(str_sub_str(sprinti(FMT_HR,FHR3(ttt)),"f","")))
	                                if(ismissing(myInd2(0)))then   continue   end if
	                                myLat1  := tcLats(sss,myInd2) - 6.*pctLat
	                                myLat2  := tcLats(sss,myInd2)
	                                myLon   := tcLons(sss,myInd2)
	        
	                                ; Print the Storm ID
					print("MSG: Adding Storm ID label for "+ALL_SID(sss)+" at "+myLat1+","+myLon)
	                                add_sid_label(wks,plot,str_upper(ALL_SID(sss)),myLon,myLat1,0.011)
	                                
	                                ; Print TC symbols for some plots
	                                if(any((/"SHDL","SHML","SHSL","SFDL","SFML","SFSL"/).eq.FNAME(ppp)))then
	                                        add_storm_marker(wks,plot,tofloat(tcINT(sss,ind(tcFHR(sss,:).eq.FHR3(ttt)))),ALL_SNUM(sss),myLon,myLat2,0.014)
	                                end if
	                        end do ;sss
	                end if
			
			;================================================================
                        ;================================================================
                        ; END --> OVERLAY #6, STORM LABELS
                        ;================================================================
                        ;================================================================
                        
                        
                        
                        
                        ;================================================================
                        ;================================================================
                        ; START --> OVERLAY #7, TITLES
                        ;================================================================
                        ;================================================================
                
                	;================================================================
                        ; lxvii. Graphic Title
			;================================================================
	                if(DOMAIN.eq."d03")then         add_graphic_title(wks,FNAME(ppp),vpLFT,vpTOP+0.023,0.012)
	                else                            add_graphic_title(wks,FNAME(ppp),vpLFT,vpTOP+0.023,0.010)
	                end if
                
	                ;================================================================
                        ; lxviii. Time Title
			;================================================================
	                TimeTitle       = "Init: "+changeTimeFmt(str_sub_str(IDATE,"gfs.",""),"YYYYMMDDHH","HHz DOW, Month DD YYYY",0)+\
	                                  "  Forecast Hour:["+str_sub_str(sprinti("%0.3i",FHR3(ttt)),"f","")+"]  valid at "+validDate
	                if(DOMAIN.eq."d03")then         add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.010,"left")
	                else                            add_time_title(wks,TimeTitle,vpLFT,vpTOP+0.008,0.008,"left")
	                end if
                
	                ;================================================================
                        ; lxix. Storm Title
			;================================================================
	                if(LONGSID_LIST.ne."")then
	                        if(DOMAIN.eq."d03")then         add_storm_title(wks,str_upper(LONGSID_LIST),vpRGT,vpTOP+0.008,0.010,"right")
	                        else                            add_storm_title(wks,str_upper(LONGSID_LIST),vpRGT,vpTOP+0.008,0.008,"right")
	                        end if
	                end if
                
			;================================================================
                        ; lxxx. Max Wind & Min MSLP information
			;================================================================
	                if(DOMAIN.eq."d03")then         add_vmax_label (wks,UV10max,vpLFT+vpWID,vpTOP+0.035,0.009,"right")
	                                                add_mslp_label (wks,MSLPmin,vpLFT+vpWID,vpTOP+0.022,0.009,"right")
	                end if
                
			;================================================================
                        ; lxxi. Model Title & Disclaimer
			;================================================================
	                if(DO_SRCLBL)then
	                        if(DOMAIN.eq."d03")then         add_disclaimer(wks,plot,EXPT,BOCO(2)+2.*pctLon,BOCO(1)+2.*pctLon,0.015,"left")
	                                                        add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.015)
	                        else                            add_disclaimer(wks,plot,EXPT,BOCO(2)+2.*pctLon,BOCO(1)+2.*pctLon,0.009,"left")
	                                                        add_model_title(wks,EXPT,ENSID,vpLFT,vpTOP+0.04,0.010)
	                        end if
	                end if
			
			;================================================================
                        ;================================================================
                        ; END --> OVERLAY #7, TITLES
                        ;================================================================
                        ;================================================================
			
			
			
			
			;****************************************************************
                        ;================================================================
                        ; START --> ACTUALLY DRAW THE PLOT WITH ALL OVERLAYS
                        ;================================================================
                        ;****************************************************************
			draw(plot)
                	frame(wks)
			;****************************************************************
                        ;================================================================
                        ; END --> ACTUALLY DRAW THE PLOT WITH ALL OVERLAYS
                        ;================================================================
                        ;****************************************************************


                
		
			;================================================================
                        ; lxxii. Remove white space on plot using the terminal
                        ; lxxiii. Convert to GIF and removed PNG on success
                        ; NOTE: These are handled together, so we can call
                        ;  'convert' only once per plot when both are True
			;================================================================
			;if(DO_RMWHITE)then   system("convert -trim "+OFILE+".png "+OFILE+".png")   end if
			if (DO_RMWHITE .and. DO_CONVERTGIF) then
			    system("convert -trim "+OFILE+".png +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
			else if (DO_CONVERTGIF) then
			    system("convert "+OFILE+".png +repage gif:"+OFILE+".gif && /bin/rm "+OFILE+".png")
			else if (DO_RMWHITE) then
			    system("convert -trim "+OFILE+".png "+OFILE+".png")
			end if  end if  end if

		end do ;ppp
        
	end do ;ttt
        

	;================================================================
        ; 10. Indicate that file has been processed.
        ;     Check that all variables for this graphic have been
        ;     produced. Loop over all expected graphics to check for
	;     missing graphics.
	;================================================================
        print("MSG: Checking that all graphics were produced for current forecast hour ("+sprinti("%0.3i", FHR(fff))+").")
        if(.not.isvar("FILE_DONE"))then   FILE_DONE = new(nFiles,logical)   end if
        FILE_DONE(fff) = True
        do ppp = 0,dimsizes(FNAME2)-1
                ; Skip variables that are not expected for current FHR
                if(FHR(fff).lt.3 .and. isStrSubset(FNAME2(ppp),"PRCP_"))then    continue   end if
                if(FHR(fff).lt.3 .and. isStrSubset(FNAME2(ppp),"REFL"))then     continue   end if
                if(FHR(fff).lt.24 .and. isStrSubset(FNAME2(ppp),"PRCP24_"))then continue   end if
                if(DSOURCE.eq."GFS" .and. \
                   isStrSubset(FNAME2(ppp),"REFL"))then                         continue   end if
                if(      DOMAIN.eq."d03"\
                   .and. isStrSubset(FNAME2(ppp),"PRCP"))then                   continue   end if
                                                        
                PNGname = FNAME2(ppp)+"."+IDATE+"."+DOMAIN+".f"+sprinti("%0.3i", FHR(fff))+".gif"
                ;print(PNGname)
                ;exit
                if(SC_DOMAIN .or. SC_DSOURCE)then
                        PNGname = "*"+str_lower(LONGSID_LIST)+"."+PNGname
                end if
                p := systemfunc("ls "+ODIR+PNGname+" 2>/dev/null")
                if(ismissing(p(0)))then
                        print("MSG: Graphics not found for "+FNAME2(ppp)+".")
                        FILE_DONE(fff) = False
                        break
                end if
        end do ;ppp
        
        
	;================================================================
        ; 11. Update PlottedFiles file if all files are done and:
        ;     --> ATCF is required and forecast hour is less than or
	;         equal to the minimum hour from ATCFs.
        ;     --> ATCF is not required.
	;================================================================
        if(FILE_DONE(fff))then
                print("MSG: All graphics found for the current forecast hour ("+sprinti("%0.3i", FHR(fff))+").")
                
                ;if(ATCF_REQD .and. ALL_ATCF)then
                if(ATCF_REQD .and. FHR(fff).le.min(tcMAXHR))then
                        if(fileexists(PLOTTED_FILES))then
                                system("echo "+iFiles(myInd(0))+" >> "+PLOTTED_FILES)
                        else
                                system("echo "+iFiles(myInd(0))+" > "+PLOTTED_FILES)
                        end if
                end if
                if(.not.ATCF_REQD)
                        if(fileexists(PLOTTED_FILES))then
                                system("echo "+iFiles(myInd(0))+" >> "+PLOTTED_FILES)
                        else
                                system("echo "+iFiles(myInd(0))+" > "+PLOTTED_FILES)
                        end if
                end if
                system("sort -u "+PLOTTED_FILES+" > "+PLOTTED_FILES+".TMP")
                system("mv "+PLOTTED_FILES+".TMP "+PLOTTED_FILES)
        end if
        
end do ; fff
end if ; DO_PLOT




;================================================================
;================================================================
;================================================================
; PART X. IF YOU GOT HERE, THEN THE SCRIPT HAS FINISHED RUNNING
;         WITHOUT FATAL, DEBILITATING ERRORS. WOW! I DIDN'T THINK
;         ANYONE WOULD ACTUALLY GET THIS FAR. LET'S REJOICE!
;         CHEERS!
;================================================================
;================================================================
;================================================================

if(ATCF_REQD .and. .not.ALL_ATCF)then
        print("")
        print("MSG: INCOMPLETE! Graphics were produced, but might be missing info from ATCFs.")
        print("MSG: Graphics will be reproduced once ATCFs become available.")
        system("echo 'incomplete' > "+STATUS_FILE)
else if(.not.all(FNL_IFILES))then
        print("")
        print("MSG: INCOMPLETE! Graphics were produced, but new input files might be available.")
        print("MSG: If new input files are found, a new session will be initiated.")
        system("echo 'incomplete' > "+STATUS_FILE)
else if(.not.all(FILE_DONE))then
        print("")
        print("MSG: INCOMPLETE! Some graphics are missing.")
        print("MSG: Will attempt to produce these missing graphics next time.")
        system("echo 'incomplete' > "+STATUS_FILE)
else
        print("")
        print("MSG: COMPLETE! Graphics were produced! Way to go! You rock!")
        ;print("MSG: This forecast will be double-checked for completeness.")
        system("echo 'complete' > "+STATUS_FILE)        
end if  end if  end if




;================================================================
;================================================================
;================================================================
; PART XI. Print some information to the terminal
;================================================================
;================================================================
;================================================================

print("")
print("MSG: Ran GPLOT with these settings:")
print("********************")
;if(IS_REAL)then         print("MSG:   Real-Time Forecast: ON")
;else                    print("MSG:   Real-Time Forecast: OFF")
;end if
if(IS_MSTORM)then        print("MSG:   Basin-Scale HWRF:   ON")
else                    print("MSG:   Basin-Scale HWRF:   OFF")
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
print("MSG:   System Environment: "+SYS_ENV)
print("MSG:   Input File Tag(s):  "+stringOut(ITAG))
print("MSG:   Initial/Final Hour: "+INIT_HR+"/"+FNL_HR)
print("MSG:   Hour Format:        "+FMT_HR)
print("MSG:   Time Step [h]:      "+DT)
if(ATCF_REQD)then       print("MSG:   ATCF Required:      ON")
else                    print("MSG:   ATCF Required:      OFF")
end if
print("MSG:   Final ATCF Path:    "+ATCF2_DIR)
print("MSG:   Final ATCF Tag:     "+ATCF2_TAG)
if(ATCF1_DIR.ne.ATCF2_DIR .and. ATCF1_TAG.ne.ATCF2_TAG)then
        print("MSG:   Temp. ATCF Path:    "+ATCF1_DIR)
        print("MSG:   Temp. ATCF Tag:     "+ATCF1_TAG)
end if
if(DO_RMWHITE)then      print("MSG:   Trim graphics:      ON")
else                    print("MSG:   Trim graphics:      OFF")
end if
if(DO_SRCLBL)then       print("MSG:   Source Labels:      ON")
else                    print("MSG:   Source Labels:      OFF")
end if
;if(DO_TOPOMASK)then    print("MSG:   Topography mask:    ON")
;else                   print("MSG:   Topography mask:    OFF")
if(DO_CONVERTGIF)then   print("MSG:   Convert to GIF:     ON")
else                    print("MSG:   Convert to GIF:     OFF")
end if
print("MSG:   Plotted Files file: "+PLOTTED_FILES)
print("MSG:   Status file:        "+STATUS_FILE)
;end if
print("********************")




;================================================================
;================================================================
;================================================================
; PART XII. End of the File!
;================================================================
;================================================================
;================================================================

print("")
print("MSG: GPLOT finished running at "+systemfunc("date")+".")

exit

end







