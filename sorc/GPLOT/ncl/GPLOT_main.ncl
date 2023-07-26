;================================================================
; GPLOT_main.ncl
;
; Higher-level Functions and procedures for GPLOT.ncl are stored
; in this file to stay organized. Note that more basic functions
; and procedures are located in GPLOT_utils.ncl
; Functions are given in alphabetical order.
;
; Written By:	  Ghassan Alaka, Jr.
;
; Created:	  April 22, 2017
; Last Modified:  April 22, 2017
;
; FUNCTION LIST
; 1)  findCenter
; 2)  Read_Master_Namelist


;load "/home/Ghassan.Alaka/GPLOT/ncl/GPLOT_func.v3.ncl"

external FILTER121 "$GPLOT_DIR/sorc/GPLOT/ncl/wrapit/filter121.so"
external CENTROID  "$GPLOT_DIR/sorc/GPLOT/ncl/wrapit/centroid.so"


;
;================================================================
; findCenter
; Dependencies:	level_convert
;		findVarName
;		getVar2d
;		getVar3d
;		filter121_2d
;
; type:		0=smoothing
;		1=centroid

undef("findCenter")
function findCenter (f:file,dSource[1]:string,var[1]:string,LEV[*]:float,LAT[*]:float,LONF[*]:float,flipFlag[1]:logical,Bds[*]:float,type[1]:integer)
local CNTR, levsz, masterList, kkk, kkk2, kkk3, V, levCode, mf, Vs, V1d, nDims, dNames, tmp, gcdist, \
      MaxMin, Vmax, Vmin, N, b, flipFlag, A, flipLev, ICTR, V2
begin
	; 1) Perform checks!
	; Check #1 --> Need units attribute
	if(.not.isatt(LEV,"units"))then	print("ERROR: findCenter: 'units' must be an attribute of pressure levels arrays")
					exit
	end if
	; Check #2 --> Need multiplicative factor (mf) attribute
	if(.not.isatt(LEV,"mf"))then	print("ERROR: findCenter: 'mf' must be an attribute of variable 'lev'")
					exit
	end if
	; Check #3 --> Variable must exist to compute TC center
	masterList	= (/"AVO",\		; Vorticity (absolute)
			    "RVO",\		; Vorticity (relative)
			    "HGT"/)		; Geopotential Height
	if(.not.any(masterList.eq.var))then
		print("ERROR: findCenter: Variable name not recognized")
		print("ERROR: findCenter: Cannot find TC center")
		exit
	end if
	; Check #4 --> 'type' must be supported
	if(.not.any((/0,1/).eq.type))then
		print("ERROR: findCenter: type not found")
		exit
	end if

	; 2) Get some info about level(s)
	LEVPA	= level_convert(LEV,LEV@units,"float","Pa")
	NZ	= dimsizes(LEV)
	if(NZ.gt.1)then
		levCode	= sprinti("%0.4i",level_convert(min(LEV),LEV@units,"integer","hPa"))+sprinti("%0.4i",level_convert(max(LEV),LEV@units,"integer","hPa"))
	else
		levCode	= level_convert(LEV(0),LEV@units,"string","hPa")
	end if

	; 3) Get info about dimensions
	nDims	:= dimsizes(getfilevardimsizes(f,findVarName(dSource,var,levCode)))
	dNames	:= getfilevardims(f,findVarName(dSource,var,levCode))
		nDims@dNames = dNames

	; 4) Set variables based on 'var'
	;    b = centroid type
	;    N = 1-2-1 filter iterations
	if(any((/"AVO","RVO"/).eq.var))then	MaxMin = "max"
						b	= 1
						if(type.eq.0)then N = 71 end if
						if(type.eq.1)then N = 25 end if
	end if
	if(any((/"HGT"/).eq.var))then		MaxMin = "min"
						b	= -1
						if(type.eq.0)then N = 51 end if
						if(type.eq.1)then N = 25 end if
	end if

	; 5) Actually read in the data
	if(NZ.gt.1)then
		V	= getVar3d(f,dSource,(/var,levCode/),nDims,Bds,LEV@mf,flipFlag,LONF)
		;V	:= V({totype(LEVPA,typeof(LEV))/k},:,:)
		flipLev	= False
		if(max(LEV).ne.LEV(0))then	LEV	= LEV(::-1)
						V	= V(::-1,:,:)
						flipLev	= True
		end if
	else
		tmp	:= getVar2d(f,dSource,(/var,levCode/),nDims,Bds,LEV@mf,flipFlag,LONF)
		V	:= new((/1,dimsizes(tmp&lat),dimsizes(tmp&lon)/),"float")
		V(0,:,:)= tmp
		V!0	= "lev"
		flipLev	= False
	end if
	
	; 6) Create the output variable "CNTR"
	;	Structure:  CNTR(:,0) = (/lat,lon/)
	; 	The 'use' attribute is important to determine if the center is associated the same vortex.
	; 	Right now, if the center at one level is within 50 km of the center in the level below. it
	; 	is considered a part of the same vortex. If not, 'use' is set to False.
	CNTR	= new((/2,NZ/),"float")
	ICTR	= new((/2,NZ/),"integer")
	CNTR@use= new(NZ,"logical")
	CNTR@lim= new(NZ,"float")
	CNTR@value=new(NZ,"float")
	
	; 7) Filter the input data to remove any irregularities
	V2	= V(:,{avg(Bds(:1))+2.:avg(Bds(:1))-2.},{avg(Bds(2:))+2.:avg(Bds(2:))-2.})
	nx2	= dimsizes(V2(0,0,:))
	ny2	= dimsizes(V2(0,:,0))
	Vs	= V2
	FILTER121::filter121(V2,Vs,nx2,ny2,NZ,N,False)
	
	; 8) FIND THE TC CENTERS
	; Type 0 --> Just use the smoothed field.
	; Type 1 --> Find the centroid of the smoothed field.
	if(type.eq.0)then
		do kkk = 0,NZ-1
			Vssz		= dimsizes(Vs(kkk,:,:))
			V1d		= ndtooned(Vs(kkk,:,:))
			if(MaxMin.eq."max")then	tmp = ind_resolve(maxind(V1d),Vssz)
						CNTR@value(kkk) = max(Vs(kkk,:,:))
			end if
			if(MaxMin.eq."min")then	tmp = ind_resolve(minind(V1d),Vssz)
						CNTR@value(kkk) = min(Vs(kkk,:,:))
			end if
			CNTR(0,kkk)	= V2&lat(tmp(0,0))
			CNTR(1,kkk)	= V2&lon(tmp(0,1))
		end do
	else if(type.eq.1)then
		if(MaxMin.eq."max")then
			A	= dim_max_n(Vs,(/1,2/)) - 0.20*abs(dim_max_n(Vs,(/1,2/))-dim_min_n(Vs,(/1,2/)))
			CENTROID::centroid(V,ICTR,A,toint(b),nx2,ny2,NZ)
			CNTR@value = dim_max_n(dim_max_n(Vs,2),1)
		else if(MaxMin.eq."min")then
			A	= dim_min_n(Vs,(/1,2/)) + 0.20*abs(dim_max_n(Vs,(/1,2/))-dim_min_n(Vs,(/1,2/)))
			CENTROID::centroid(V2,ICTR,A,toint(b),nx2,ny2,NZ)
			CNTR@value = dim_min_n(dim_min_n(Vs,2),1)
		end if  end if
		vals := ind(ICTR(1,:).ge.0 .and. .not.ismissing(ICTR(1,:)))
		if(.not.ismissing(vals(0)))then
			CNTR(0,vals) = (/V2&lat(ICTR(1,vals)-1)/)
			CNTR(1,vals) = (/V2&lon(ICTR(0,vals)-1)/)
		end if
			
	end if  end if
	
	; 9) Determine if CNTR at this level is within 1 km per 1 hPa of CNTR of two levels below
	do kkk = 0,NZ-1
	
		if(kkk.eq.1)then
			gcdist1	= gc_latlon(CNTR(0,kkk),CNTR(1,kkk),CNTR(0,kkk-1),CNTR(1,kkk-1),2,4)
			lim1	= gcdist1/(abs(LEV(kkk)-LEV(kkk-1))/LEV@mf)
			CNTR@lim(kkk) = lim1
			
			if(lim1.le.1.)then		CNTR@use(kkk) = True
			else				CNTR@use(kkk) = False
			end if
			if(.not.CNTR@use(kkk-1))then	CNTR@use(kkk) = False   end if
			
		else if(kkk.gt.1)then
			gcdist1	= gc_latlon(CNTR(0,kkk),CNTR(1,kkk),CNTR(0,kkk-1),CNTR(1,kkk-1),2,4)
			lim1	= gcdist1/(abs(LEV(kkk)-LEV(kkk-1))/LEV@mf)
			gcdist2	= gc_latlon(CNTR(0,kkk),CNTR(1,kkk),CNTR(0,kkk-2),CNTR(1,kkk-2),2,4)
			lim2	= gcdist2/(abs(LEV(kkk)-LEV(kkk-2))/LEV@mf)
			CNTR@lim(kkk) = lim1
			
			if(lim1.le.1.)then		CNTR@use(kkk) = True
			else if(lim2.le.1.)then		CNTR@use(kkk) = True
			else				CNTR@use(kkk) = False
			end if  end if
			if(.not.CNTR@use(kkk-1) .or.\
			   .not.CNTR@use(kkk-2))then	CNTR@use(kkk) = False	end if
		else
			CNTR@use(kkk) = True
			CNTR@lim(kkk) = 0.
		end if  end if
	end do
	
	
	; 10) Flip vertical dimension back
	if(flipLev)then   CNTR = CNTR(:,::-1)
			  CNTR@use = CNTR@use(::-1)
			  CNTR@lim = CNTR@lim(::-1)
			  CNTR@value = CNTR@value(::-1)
			  LEV = LEV(::-1)
	end if

	
	; 11) Return the list of TC centers
	delete([/V,V2,Vs/])

	return(CNTR)
end
; findCenter


undef("Read_Master_Namelist")
function Read_Master_Namelist (NFILE[1]:string)
local val, NML
begin

	; Read the master namelist into NCL
	NML = asciiread(NFILE,-1,"string")

	; a. DOMAIN --> the map domain with hard-coded boundaries.
	val = ind(isStrSubset2(NML,"DOMAIN ="))
        if(ismissing(val(0)))then	print("WARNING: DOMAIN should be supplied via command line or namelist.")
					print("WARNING: Assuming DOMAIN='atl'.")
					NML@DOMAIN  = "atl"
        else				NML@DOMAIN  = NML(val(0))
					NML@DOMAIN  := str_squeeze(str_split(NML@DOMAIN," = "))
					NML@DOMAIN  := NML@DOMAIN(1)
        end if

	; b. TIER --> the priority of current batch of graphics.
	val := ind(isStrSubset2(NML,"TIER ="))
        if(ismissing(val(0)))then	print("WARNING: TIER should be supplied via command line or namelist.")
					print("WARNING: Assuming TIER='Tier1'.")
					NML@TIER = "Tier1"
        else				NML@TIER = NML(val(0))
					NML@TIER := str_squeeze(str_split(NML@TIER," = "))
					NML@TIER := NML@TIER(1)
        end if

	; c. DSOURCE --> the data source (not the experiment!)
	val := ind(isStrSubset2(NML,"DSOURCE ="))
        if(ismissing(val(0)))then	print("WARNING: DSOURCE should be supplied via command line or namelist.")
					print("WARNING: Assuming DSOURCE='HAFS'.")
					NML@DSOURCE = "HAFS"
        else				NML@DSOURCE = NML(val(0))
					NML@DSOURCE := str_squeeze(str_split(NML@DSOURCE," = "))
					NML@DSOURCE := NML@DSOURCE(1)
        end if

	; d. MACHINE --> the system environment (e.g., JET, HERA, ORION)
	;    SYS_ENV is an old option that might still be used in some places.
	val := ind(isStrSubset2(NML,"MACHINE ="))
        if(ismissing(val(0)))then	val := ind(isStrSubset2(NML,"SYS_ENV ="))
					if(ismissing(val(0)))then	print("WARNING: MACHINE should be supplied via command line or namelist.")
									print("WARNING: Assuming MACHINE='JET'.")
									NML@MACHINE = "JET"
					else
									NML@MACHINE = NML(ind(isStrSubset2(NML,"SYS_ENV =")))
									NML@MACHINE := str_squeeze(str_split(NML@MACHINE," = "))
									NML@MACHINE := NML@MACHINE(1)
					end if
        else				NML@MACHINE = NML(ind(isStrSubset2(NML,"MACHINE =")))
					NML@MACHINE := str_squeeze(str_split(NML@MACHINE," = "))
					NML@MACHINE := NML@MACHINE(1)
        end if
	val := ind(isStrSubset2(NML,"SYS_ENV ="))
        if(ismissing(val(0)))then	print("WARNING: SYS_ENV should be supplied via command line or namelist.")
					print("WARNING: Assuming SYS_ENV='JET'.")
					NML@SYS_ENV = "JET"
        else				NML@SYS_ENV = NML(ind(isStrSubset2(NML,"SYS_ENV =")))
					NML@SYS_ENV := str_squeeze(str_split(NML@SYS_ENV," = "))
					NML@SYS_ENV := NML@SYS_ENV(1)
        end if

	; e. IDATE --> the forecast cycle (YYYYMMDDHH)
	val := ind(isStrSubset2(NML,"IDATE ="))
        if(ismissing(val(0)))then	print("ERROR: IDATE must be supplied via command line or namelist.")
					NML@IDATE = systemfunc("date +'%Y%m%d%H'")
					print("WARNING: Assuming IDATE="+NML@IDATE+" - Don't expect this to work.")
        else				NML@IDATE = NML(val(0))
					NML@IDATE := str_squeeze(str_split(NML@IDATE," = "))
					if(dimsizes(NML@IDATE).eq.2)then	NML@IDATE := NML@IDATE(1)
					else					NML@IDATE := ""
					end if
        end if

	; f. SID --> the Storm identifier (e.g., 00L)
	val := ind(isStrSubset2(NML,"SID ="))
        if(ismissing(val(0)))then	print("WARNING: SID should be supplied via command line or namelist.")
					print("WARNING: Assuming SID='NONE'.")
					NML@SID = "NONE"
        else				NML@SID = NML(val(0))
					NML@SID := str_squeeze(str_split(NML@SID," = "))
					if(dimsizes(NML@SID).eq.2)then	NML@SID := NML@SID(1)
					else				NML@SID := ""
					end if
        end if

	; g. ENSID --> the ensemble ID, if applicable
	val := ind(isStrSubset2(NML,"ENSID ="))
        if(ismissing(val(0)))then	NML@ENSID = ""
        else				NML@ENSID = NML(val(0))
					NML@ENSID := str_squeeze(str_split(NML@ENSID," = "))
					if(dimsizes(NML@ENSID).gt.2)then
						print("MSG: More than one ENSID found. I will take only the first one.")
						NML@ENSID := NML@ENSID(1)
					else if(dimsizes(NML@ENSID).eq.2)then
						NML@ENSID := NML@ENSID(1)
					else
						NML@ENSID=""
					end if  end if
        end if


	; h. MODELID --> the model identifier (e.g., HAFS, HWRF, AVNO, ,etc.)
	val := ind(isStrSubset2(NML,"MID ="))
        if(ismissing(val(0)))then	NML@MODELID = ""
        else				NML@MODELID = NML(val(0))
					NML@MODELID := str_squeeze(str_split(NML@MODELID," = "))
					if(dimsizes(NML@MODELID).gt.2)then
						print("MSG: More than one MODELID found. I will take only the first one.")
						NML@MODELID := NML@MODELID(1)
					else if(dimsizes(NML@MODELID).eq.2)then
						NML@MODELID := NML@MODELID(1)
					else
						NML@MODELID=""
					end if  end if
        end if

	; i. ATCF_REQD --> 'True' if the ATCF is required.
	val := ind(isStrSubset2(NML,"ATCF_REQD ="))
        if(ismissing(val(0)))then	print("WARNING: ATCF_REQD should be supplied via command line or namelist.")
					NML@ATCF_REQD := True
					print("WARNING: Assuming ATCF_REQD='"+tostring(NML@ATCF_REQD)+"'.")
        else				NML@ATCF_REQD = NML(val(0))
					NML@ATCF_REQD := str_squeeze(str_split(NML@ATCF_REQD," = "))
					NML@ATCF_REQD := where(NML@ATCF_REQD(1).eq."True",True,False)
        end if

	; j. EXPT --> the experiment name (not the data source!)
	val := ind(isStrSubset2(NML,"EXPT ="))
        if(ismissing(val(0)))then	print("ERROR: EXPT must be supplied via command line or namelist.")
					NML@EXPT = "HWRF_Forecast"
					print("WARNING: Assuming EXPT="+NML@EXPT+" - Don't expect this to work.")
        else				NML@EXPT = NML(val(0))
					NML@EXPT := str_squeeze(str_split(NML@EXPT," = "))
					NML@EXPT := NML@EXPT(1)
        end if

	; k. IDIR --> the top-level input data path
        val := ind(isStrSubset2(NML,"IDIR ="))
        if(ismissing(val(0)))then	print("ERROR: IDIR must be supplied via command line or namelist.")
					NML@IDIR = "."
					print("WARNING: Assuming IDIR="+NML@IDIR+" - Don't expect this to work.")
        else				NML@IDIR = NML(val(0))
					NML@IDIR := str_squeeze(str_split(NML@IDIR," = "))
					NML@IDIR := NML@IDIR(1)
        end if

	; l. ITAG --> the file name tag for input data.
	val := ind(isStrSubset2(NML,"ITAG ="))
        if(ismissing(val(0)))then	print("WARNING: ITAG should be supplied via command line or namelist.")
					print("WARNING: Assuming ITAG="+NML@ITAG)
					NML@ITAG = ""
        else				NML@ITAG = NML(val(0))
					NML@ITAG := str_squeeze(str_split(NML@ITAG," = "))
					if(dimsizes(NML@ITAG).eq.1)then	NML@ITAG = ""
					else				NML@ITAG := ITAG(1)
									NML@ITAG := str_squeeze(str_split(NML@ITAG," "))
					end if
        end if

	; m. EXT --> the extension of the input data files.
	val := ind(isStrSubset2(NML,"EXT ="))
        if(ismissing(val(0)))then	print("WARNING: EXT should be supplied via command line or namelist.")
					NML@EXT = ""
					print("WARNING: Assuming EXT="+NML@EXT)
        else				NML@EXT = NML(val(0))
					NML@EXT := str_squeeze(str_split(NML@EXT," = "))
					if(dimsizes(NML@EXT).eq.1)then	NML@EXT = ""
					else				NML@EXT := NML@EXT(1)
									NML@EXT := str_squeeze(str_split(NML@EXT," "))
					end if
        end if

	; n. ODIR --> the top-level output data path
	val := ind(isStrSubset2(NML,"ODIR ="))
        if(ismissing(val(0)))then	print("ERROR: ODIR must be supplied via command line or namelist.")
					NML@ODIR = "."
					print("WARNING: Assuming ODIR="+NML@ODIR+" - Don't expect this to work.")
        else				NML@ODIR = NML(val(0))
					NML@ODIR := str_squeeze(str_split(NML@ODIR," = "))
					NML@ODIR := NML@ODIR(1)
        end if

	; o. ODIR_TYPE --> special option to modify ODIR
	val := ind(isStrSubset2(NML,"ODIR_TYPE ="))
        if(ismissing(val(0)))then	print("WARNING: ODIR_TYPE should be supplied via command line or namelist.")
					NML@ODIR_TYPE = 0
        else				NML@ODIR_TYPE = NML(val(0))
					NML@ODIR_TYPE := str_squeeze(str_split(NML@ODIR_TYPE," = "))
					NML@ODIR_TYPE := toint(NML@ODIR_TYPE(1))
        end if
	;if(NML@ODIR_TYPE.eq.1)then 	NML@ODIR = str_sub_str(NML@ODIR+"/"+NML@DOMAIN+"/","//","/")
	;else				NML@ODIR = str_sub_str(NML@ODIR+"/"+NML@EXPT+"/"+NML@ENSID+"/"+NML@IDATE+"/"+NML@DOMAIN+"/","//","/")
	;end if
	;print("MSG: I will place graphics in this location --> "+NML@ODIR)
	NML@ODIR_ADECK = str_sub_str(NML@ODIR+"/adeck/","//","/")

	; p. INIT_HR --> the initial forecast hour for which graphics will be produced.
	val := ind(isStrSubset2(NML,"INIT_HR ="))
        if(ismissing(val(0)))then	print("WARNING: INIT_HR should be supplied via command line or namelist.")
					NML@INIT_HR = 0
					print("WARNING: Assuming INIT_HR="+NML@INIT_HR)
        else				NML@INIT_HR = NML(val(0))
					NML@INIT_HR := str_squeeze(str_split(NML@INIT_HR," = "))
					NML@INIT_HR := toint(tofloat(NML@INIT_HR(1)))
        end if

	; q. FNL_HR --> the final forecast hour for which graphics will be produced.
	;               value may change based on the availability of ATCF data.
	val := ind(isStrSubset2(NML,"FNL_HR ="))
        if(ismissing(val(0)))then	print("WARNING: FNL_HR should be supplied via command line or namelist.")
					NML@FNL_HR = 126
					print("WARNING: Assuming FNL_HR="+NML@FNL_HR)
        else				NML@FNL_HR = NML(val(0))
					NML@FNL_HR := str_squeeze(str_split(NML@FNL_HR," = "))
					NML@FNL_HR := toint(tofloat(NML@FNL_HR(1)))
        end if

	; r. FMT_HR --> the format code for the forecast hour used in the input file name
	val := ind(isStrSubset2(NML,"FMT_HR ="))
        if(ismissing(val(0)))then	print("WARNING: FMT_HR should be supplied via command line or namelist.")
					NML@FMT_HR = "%0.3i"
					print("WARNING: Assuming FMT_HR="+NML@FMT_HR)
        else				NML@FMT_HR = NML(val(0))
					NML@FMT_HR := str_squeeze(str_split(NML@FMT_HR," = "))
					NML@FMT_HR := "%0."+NML@FMT_HR(1)+"i"
        end if

	; s. DT --> the time step between forecast hours
	val := ind(isStrSubset2(NML,"DT ="))
        if(ismissing(val(0)))then	print("WARNING: DT should be supplied via command line or namelist.")
					NML@DT = 3
					print("WARNING: Assuming DT="+NML@DT)
        else				NML@DT = NML(val(0))
					NML@DT := str_squeeze(str_split(NML@DT," = "))
					NML@DT := toint(tofloat(NML@DT(1)))
        end if

	; t. IS_MSTORM --> 'True' if the data contains forecasts for multiple TCs
	;                  This variable determines if the case is from Basin-Scale HWRF.
	;                  It is important because that means multiple high-res domains
	;                  could exist. Domains that are not storm-centered should be not
	;                  be linked to a specific storm. The Operational HWRF is storm-
	;                  centered, so it is always linked to a specific storm.
	val := ind(isStrSubset2(NML,"IS_MSTORM ="))
        if(ismissing(val(0)))then	print("WARNING: IS_MSTORM should be supplied via command line or namelist.")
					NML@IS_MSTORM = False
					print("WARNING: Assuming IS_MSTORM="+tostring(IS_MSTORM))
        else				NML@IS_MSTORM = NML(val(0))
					NML@IS_MSTORM := str_squeeze(str_split(NML@IS_MSTORM," = "))
					NML@IS_MSTORM := where(NML@IS_MSTORM(1).eq."True",True,False)
        end if

	; u. DO_RMWHITE --> 'True' if the white space should be trimmed from graphics.
	val := ind(isStrSubset2(NML,"DO_RMWHITE ="))
        if(ismissing(val(0)))then	print("WARNING: DO_RMWHITE should be supplied via command line or namelist.")
					NML@DO_RMWHITE = False
					print("WARNING: Assuming DO_RMWHITE="+tostring(NML@DO_RMWHITE))
        else				NML@DO_RMWHITE = NML(val(0))
					NML@DO_RMWHITE := str_squeeze(str_split(NML@DO_RMWHITE," = "))
					NML@DO_RMWHITE := where(NML@DO_RMWHITE(1).eq."True",True,False)
        end if

	; v. DO_SRCLBL --> 'True' if the data source should be added to the graphic title.
	val := ind(isStrSubset2(NML,"DO_SRCLBL ="))
        if(ismissing(val(0)))then	print("WARNING: DO_SRCLBL should be supplied via command line or namelist.")
					NML@DO_SRCLBL = False
					print("WARNING: Assuming DO_SRCLBL="+tostring(NML@DO_SRCLBL))
        else				NML@DO_SRCLBL = NML(val(0))
					NML@DO_SRCLBL := str_squeeze(str_split(NML@DO_SRCLBL," = "))
					NML@DO_SRCLBL := where(NML@DO_SRCLBL(1).eq."True",True,False)
        end if

	; w. PIV --> the pivot longitude for regional domains.
	;            For strange regional domains (e.g., HWRF-B), the longitude array must be reordered.
	val := ind(isStrSubset2(NML,"PIV ="))
        if(ismissing(val(0)))then	print("WARNING: PIV should be supplied via the command line or namelist.")
					NML@MAP_PIV = 180
					print("WARNING: Assuming PIV="+NML@MAP_PIV)
        else				NML@MAP_PIV = NML(val(0))
					NML@MAP_PIV := str_squeeze(str_split(NML@MAP_PIV," = "))
					NML@MAP_PIV := tofloat(NML@MAP_PIV(1))
        end if

	; x. DO_CONVERTGIF --> 'True' if PNG files should be converted to GIF and remove PNG files.
	val := ind(isStrSubset2(NML,"DO_CONVERTGIF ="))
        if(ismissing(val(0)))then	print("WARNING: DO_CONVERTGIF should be supplied via the command line or namelist.")
					NML@DO_CONVERTGIF = False
					print("WARNING: Assuming DO_CONVERTGIF="+tostring(NML@DO_CONVERTGIF))
        else				NML@DO_CONVERTGIF = NML(val(0))
					NML@DO_CONVERTGIF := str_squeeze(str_split(NML@DO_CONVERTGIF," = "))
					NML@DO_CONVERTGIF := where(NML@DO_CONVERTGIF(1).eq."True",True,False)
	end if

	; y. NMAX_MAPS --> The maximum number of files to process.
	val := ind(isStrSubset2(NML,"NMAX_MAPS ="))
        if(ismissing(val(0)))then	print("WARNING: NMAX_MAPS should be supplied via the command line or namelist.")
					NML@MAP_NMAX = 100
					print("WARNING: Assuming NMAX_MAPS="+NML@MAP_NMAX)
        else				NML@MAP_NMAX = NML(val(0))
					NML@MAP_NMAX := str_squeeze(str_split(NML@MAP_NMAX," = "))
					NML@MAP_NMAX := toint(NML@MAP_NMAX(1))
        end if

	; z. DO_TITLES --> 'True' if the main title and subtitle should be added to the graphic.
	val := ind(isStrSubset2(NML,"DO_TITLES ="))
        if(ismissing(val(0)))then	print("WARNING: DO_TITLES should be supplied via command line or namelist.")
					NML@DO_TITLES = True
					print("WARNING: Assuming DO_TITLES="+NML@DO_TITLES)
        else				NML@DO_TITLES = NML(val(0))
					NML@DO_TITLES := str_squeeze(str_split(NML@DO_TITLES," = "))
					NML@DO_TITLES := where(NML@DO_TITLES(1).eq."True",True,False)
        end if

	; aa. DO_DISCLAIMER --> 'True' if the disclaimer should be added to the graphic.
	val := ind(isStrSubset2(NML,"DO_DISCLAIMER ="))
        if(ismissing(val(0)))then	print("WARNING: DO_DISCLAIMER should be supplied via command line or namelist.")
					NML@DO_DISCLAIMER = True
					print("WARNING: Assuming DO_DISCLAIMER="+NML@DO_DISCLAIMER)
        else				NML@DO_DISCLAIMER = NML(val(0))
					NML@DO_DISCLAIMER := str_squeeze(str_split(NML@DO_DISCLAIMER," = "))
					NML@DO_DISCLAIMER := where(NML@DO_DISCLAIMER(1).eq."True",True,False)
        end if

	; ***************************
	; OPTIONS SPECIFIC TO GPLOT MODULE STATS
	; ***************************

	; ab. ATCF1_DIR --> The ATCF 1 directory. This is typically where temporary ATCF files are stored.
	val := ind(isStrSubset2(NML,"ATCF1_DIR ="))
	if(ismissing(val(0)))then	NML@ATCF1_DIR = "MISSING"
	else				NML@ATCF1_DIR = NML(val(0))
					NML@ATCF1_DIR := str_squeeze(str_split(NML@ATCF1_DIR," = "))
					NML@ATCF1_DIR := NML@ATCF1_DIR(1)
	end if

	; ac. ATCF1_TAG --> The ATCF 1 file string.
	val := ind(isStrSubset2(NML,"ATCF1_TAG ="))
	if(ismissing(val(0)))then	NML@ATCF1_TAG = "MISSING"
	else				NML@ATCF1_TAG = NML(val(0))
					NML@ATCF1_TAG := str_squeeze(str_split(NML@ATCF1_TAG," = "))
					NML@ATCF1_TAG := NML@ATCF1_TAG(1)
	end if

	; ad. ATCF2_DIR --> The ATCF 2 directory. This is typically where final ATCF files are stored.
	val := ind(isStrSubset2(NML,"ATCF2_DIR ="))
	if(ismissing(val(0)))then	NML@ATCF2_DIR = "MISSING"
	else				NML@ATCF2_DIR = NML(val(0))
					NML@ATCF2_DIR := str_squeeze(str_split(NML@ATCF2_DIR," = "))
					NML@ATCF2_DIR := NML@ATCF2_DIR(1)
	end if

	; ae. ATCF2_TAG -->  The ATCF 2 file string.
	val := ind(isStrSubset2(NML,"ATCF2_TAG ="))
	if(ismissing(val(0)))then	NML@ATCF2_TAG = "MISSING"
	else				NML@ATCF2_TAG = NML(val(0))
					NML@ATCF2_TAG := str_squeeze(str_split(NML@ATCF2_TAG," = "))
					NML@ATCF2_TAG := NML@ATCF2_TAG(1)
	end if


	; af. ADECK_DIR --> The aid (A-Deck) directory.
	val := ind(isStrSubset2(NML,"ADECK_DIR ="))
	if(ismissing(val(0)))then	NML@ADECK_DIR = "MISSING"
	else				NML@ADECK_DIR = NML(val(0))
					NML@ADECK_DIR := str_squeeze(str_split(NML@ADECK_DIR," = "))
					NML@ADECK_DIR := NML@ADECK_DIR(1)
	end if

	; ag. BDECK_DIR --> The Best Track (B-Deck) directory. Fallback to the old 'BDECK2_DIR'
	val := ind(isStrSubset2(NML,"BDECK_DIR ="))
	if(ismissing(val(0)))then	val := ind(isStrSubset2(NML,"BDECK2_DIR ="))   end if 
	if(ismissing(val(0)))then	NML@BDECK_DIR = "MISSING"
	else				NML@BDECK_DIR = NML(val(0))
					NML@BDECK_DIR := str_squeeze(str_split(NML@BDECK_DIR," = "))
					NML@BDECK_DIR := NML@BDECK_DIR(1)
	end if

	; ah. MCODE --> The actual (late) ATCF identifier (i.e. mode code XXXX)
	val := ind(isStrSubset2(NML,"MCODE ="))
	if(ismissing(val(0)))then	print("ERROR: 'MCODE' must be defined in the namelist.")
					NML@MCODE = "HWRF"
					print("WARNING: Assuming MCODE="+NML@MCODE+" - Don't expect this to work.")
	else				NML@MCODE = NML(val(0))
					NML@MCODE := str_squeeze(str_split(NML@MCODE," = "))
					NML@MCODE := NML@MCODE(1)
	end if
	if(NML@MCODE.eq."AVNO")then	NML@MCODEV = "GFSO"
	else				NML@MCODEV = NML@MCODE
	end if

	; ai. MCODEI --> The 6-h early model code. If not set, interpolation will be turned off.
	val := ind(isStrSubset2(NML,"MCODEI ="))
	if(ismissing(val(0)))then	NML@MCODEI = "MISSING"
	else				NML@MCODEI = NML(val(0))
					NML@MCODEI  := str_squeeze(str_split(NML@MCODEI," = "))
					NML@MCODEI  := NML@MCODEI(1)
	end if
	if(NML@MCODEI.eq."AVNI")then	NML@MCODEVI = "GFSI"
	else				NML@MCODEVI = NML@MCODEI
	end if

	; aj. MCODE12 --> The 12-h early model code. If not set, interpolation will be turned off.
	val := ind(isStrSubset2(NML,"MCODE12 ="))
	if(ismissing(val(0)))then	NML@MCODE12 = "MISSING"
	else				NML@MCODE12 = NML(val(0))
					NML@MCODE12 := str_squeeze(str_split(NML@MCODE12," = "))
					NML@MCODE12 := NML@MCODE12(1)
	end if


	; ak. MORIG --> The original model code. Default is MCODE.
	val := ind(isStrSubset2(NML,"MORIG ="))
	if(ismissing(val(0)))then	NML@MORIG = NML@MCODE
	else				NML@MORIG = NML(val(0))
					NML@MORIG := str_squeeze(str_split(NML@MORIG," = "))
					NML@MORIG := NML@MORIG(1)
	end if

	; al. TRKmodels --> Late track models
	val := ind(isStrSubset2(NML,"TRKM ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"TRKM00 ="))
		if(ismissing(val(0)))then	NML@TRKmodels = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@TRKmodels = NML(val(0))
					NML@TRKmodels := str_squeeze(str_split(NML@TRKmodels," = "))
					NML@TRKmodels := NML@TRKmodels(1:)
	end if

	; al. TRKINTmodels --> Late track/intensity models
	val := ind(isStrSubset2(NML,"TRKIM ="))
	if(ismissing(val(0)))then	NML@TRKINTmodels = "MISSING"
	else				NML@TRKINTmodels = NML(val(0))
					NML@TRKINTmodels := str_squeeze(str_split(NML@TRKINTmodels," = "))
					NML@TRKINTmodels := NML@TRKINTmodels(1:)
	end if


	; am. INTmodels --> Late intensity models
	val := ind(isStrSubset2(NML,"INTM ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"INTM00 ="))
		if(ismissing(val(0)))then	NML@INTmodels = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@INTmodels = NML(val(0))
					NML@INTmodels := str_squeeze(str_split(NML@INTmodels," = "))
					NML@INTmodels := NML@INTmodels(1:)
	end if

	; an. PRSmodels --> Late pressure models
	val := ind(isStrSubset2(NML,"PRSM ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"PRSM00 ="))
		if(ismissing(val(0)))then	NML@PRSmodels = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@PRSmodels = NML(val(0))
					NML@PRSmodels := str_squeeze(str_split(NML@PRSmodels," = "))
					NML@PRSmodels := NML@PRSmodels(1:)
	end if

	; ao. TRKmodelsI --> Early track models
	val := ind(isStrSubset2(NML,"TRKMI ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"TRKMI00 ="))
		if(ismissing(val(0)))then	NML@TRKmodelsI = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@TRKmodelsI = NML(val(0))
					NML@TRKmodelsI := str_squeeze(str_split(NML@TRKmodelsI," = "))
					NML@TRKmodelsI := NML@TRKmodelsI(1:)
	end if

	; ap. INTmodelsI --> Early intensity models
	val := ind(isStrSubset2(NML,"INTMI ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"INTMI00 ="))
		if(ismissing(val(0)))then	NML@INTmodelsI = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@INTmodelsI = NML(val(0))
					NML@INTmodelsI := str_squeeze(str_split(NML@INTmodelsI," = "))
					NML@INTmodelsI := NML@INTmodelsI(1:)
	end if

	; aq. TRKmodelsT --> Late track trend models
	val := ind(isStrSubset2(NML,"TRKMT ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"TRKMT00 ="))
		if(ismissing(val(0)))then	NML@TRKmodelsT = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@TRKmodelsT = NML(val(0))
					NML@TRKmodelsT := str_squeeze(str_split(NML@TRKmodelsT," = "))
					NML@TRKmodelsT := NML@TRKmodelsT(1:)
	end if

	; ar. INTmodelsT --> Late intensity trend models
	val := ind(isStrSubset2(NML,"INTMT ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"INTMT00 ="))
		if(ismissing(val(0)))then	NML@INTmodelsT = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@INTmodelsT = NML(val(0))
					NML@INTmodelsT := str_squeeze(str_split(NML@INTmodelsT," = "))
					NML@INTmodelsT := NML@INTmodelsT(1:)
	end if

	; as. PRSmodelsT --> Late pressure trend models
	val := ind(isStrSubset2(NML,"PRSMT ="))
	if(ismissing(val(0)))then
		val := ind(isStrSubset2(NML,"PRSMT00 ="))
		if(ismissing(val(0)))then	NML@PRSmodelsT = "MISSING"	end if
	end if
	if(.not.ismissing(val(0)))then	NML@PRSmodelsT = NML(val(0))
					NML@PRSmodelsT := str_squeeze(str_split(NML@INTmodelsT," = "))
					NML@PRSmodelsT := NML@INTmodelsT(1:)
	end if

	; at. etModels --> The early track models for verification
	val := ind(isStrSubset2(NML,"ETM ="))
	if(ismissing(val(0)))then	NML@etModels = "MISSING"
	else				NML@etModels = NML(val(0))
					NML@etModels := str_squeeze(str_split(NML@etModels," = "))
					NML@etModels := NML@etModels(1:)
	end if

	; aw. eiModels --> The early intensity models for verification
	val := ind(isStrSubset2(NML,"EIM ="))
	if(ismissing(val(0)))then	NML@eiModels = "MISSING"
	else				NML@eiModels = NML(val(0))
					NML@eiModels := str_squeeze(str_split(NML@eiModels," = "))
					NML@eiModels := NML@eiModels(1:)
	end if

	; ax. ltModels --> The late track models for verification
	val := ind(isStrSubset2(NML,"LTM ="))
	if(ismissing(val(0)))then	NML@ltModels = "MISSING"
	else				NML@ltModels = NML(val(0))
					NML@ltModels := str_squeeze(str_split(NML@ltModels," = "))
					NML@ltModels := NML@ltModels(1:)
	end if

	; ay. liModels --> The late intensity models for verification
	val := ind(isStrSubset2(NML,"LIM ="))
	if(ismissing(val(0)))then	NML@liModels = "MISSING"
	else				NML@liModels = NML(val(0))
					NML@liModels := str_squeeze(str_split(NML@liModels," = "))
					NML@liModels := NML@liModels(1:)
	end if

	; az. LEAD_TIMES --> Lead times for X-axis tick marks and verification.
	val := ind(isStrSubset2(NML,"LEAD_TIMES ="))
	if(ismissing(val(0)))then	NML@LEAD_TIMES = ispan(0,168,12)
	else				NML@LEAD_TIMES = NML(val(0))
					NML@LEAD_TIMES := str_squeeze(str_split(NML@LEAD_TIMES,"="))
					NML@LEAD_TIMES := NML@LEAD_TIMES(1)
					NML@LEAD_TIMES := toint(str_split(NML@LEAD_TIMES," "))
	end if

	; ba. nTrend --> The number of forecasts to include in trend graphics.
	val := ind(isStrSubset2(NML,"NTREND ="))
	if(ismissing(val(0)))then	NML@nTrend = 6
	else				NML@nTrend = NML(val(0))
					NML@nTrend := str_squeeze(str_split(NML@nTrend,"="))
					NML@nTrend := toint(NML@nTrend(1))
	end if

	; bb. DO_INTERP --> Determine whether or not to do the interpolation.
	val := ind(isStrSubset2(NML,"DO_INTERP ="))
	if(ismissing(val(0)))then	NML@DO_INTERP = False
	else				NML@DO_INTERP = NML(val(0))
					NML@DO_INTERP := str_squeeze(str_split(NML@DO_INTERP,"="))
					NML@DO_INTERP := where(NML@DO_INTERP(1).eq."True",True,False)
	end if

	; bc. DO_MARKERS --> Determine whether or not to plot markers.
	val := ind(isStrSubset2(NML,"DO_MARKERS ="))
	if(ismissing(val(0)))then	NML@DO_MARKERS = True
	else				NML@DO_MARKERS = NML(val(0))
					NML@DO_MARKERS := str_squeeze(str_split(NML@DO_MARKERS,"="))
					NML@DO_MARKERS := where(NML@DO_MARKERS(1).eq."True",True,False)
	end if

	; bd. MAX_FHR --> Determine the maximum lead time for graphics.
	val := ind(isStrSubset2(NML,"MAX_FHR ="))
	if(ismissing(val(0)))then	NML@MAX_FHR = 180
	else				NML@MAX_FHR = NML(val(0))
					NML@MAX_FHR := str_squeeze(str_split(NML@MAX_FHR,"="))
					NML@MAX_FHR := toint(NML@MAX_FHR(1))
	end if

	; be. DO_FHRLABELS --> Determine whether or not to show forecast lead time labels.
	val := ind(isStrSubset2(NML,"DO_FHRLABELS ="))
	if(ismissing(val(0)))then	NML@DO_FHRLABELS = True
	else				NML@DO_FHRLABELS = NML(val(0))
					NML@DO_FHRLABELS := str_squeeze(str_split(NML@DO_FHRLABELS,"="))
					NML@DO_FHRLABELS := where(NML@DO_FHRLABELS(1).eq."True",True,False)
	end if


        ; ***************************
        ; MORE GENERAL OPTIONS
        ; ***************************

	; bf. DO_PDF --> If True, PDF files are created.
	;                Also, DO_CONVERTGIF and DO_RMWHITE are set to False.
	val := ind(isStrSubset2(NML,"DO_PDF ="))
	if(ismissing(val(0)))then	NML@DO_PDF = False
	else				NML@DO_PDF = NML(val)
					NML@DO_PDF := str_squeeze(str_split(NML@DO_PDF," = "))
					NML@DO_PDF := where(NML@DO_PDF(1).eq."True",True,False)
	end if
	if(NML@DO_PDF)then	NML@DO_CONVERTGIF = False
				NML@DO_RMWHITE = False
	end if

	; bg. FORCE --> If True, force certain actions
	val := ind(isStrSubset2(NML,"FORCE ="))
	if(.not.ismissing(val(0)))then	NML@FORCE = False
	else				NML@FORCE = NML(val)
					NML@FORCE := str_squeeze(str_split(NML@FORCE," = "))
					NML@FORCE := where(NML@FORCE(1).eq."True",True,False)
	end if



	; Return the namelist variable
	return(NML)

end
; Read_Master_Namelist
