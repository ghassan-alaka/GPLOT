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


;load "/home/Ghassan.Alaka/GPLOT/ncl/GPLOT_func.v3.ncl"

external FILTER121    "$GPLOT_DIR/wrapit/filter121.so"
external CENTROID "$GPLOT_DIR/wrapit/centroid.so"


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
function findCenter (f:file,dSource[1]:string,var[1]:string,LEV[*]:float,Bds[*]:float,type[1]:integer)
local CNTR, levsz, masterList, kkk, kkk2, kkk3, V, levCode, mf, Vs, V1d, nDims, dNames, tmp, gcdist, \
      MaxMin, Vmax, Vmin, N, b, flipFlag, A, flipLev, ICTR
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
	do ddd = 0,nDims-1
	
		; PRESSURE LEVELS
		; This is the input variable "LEV"
		
		; LATITUDE
		if(isStrSubset(dNames(ddd),"lat"))then
			y	= ddd
			;LAT	= fspan(60.,-10.,141)
			LAT	:= f->$dNames(ddd)$({Bds(0):Bds(1)})
			ny	= dimsizes(LAT)
			reso	= abs(LAT(0)-LAT(1))
		end if
		
		; LONGITUDE
		if(isStrSubset(dNames(ddd),"lon"))then
			x	= ddd
			;LON	= fspan(-100.,-20.,161)
			if(min(f->$dNames(ddd)$).eq.0.)then
				LONF	:= (/where(lonFlip(f->$dNames(ddd)$).ge.180.,lonFlip(f->$dNames(ddd)$)-360.,lonFlip(f->$dNames(ddd)$))/)
				flipFlag = True
			else if(max(f->$dNames(ddd)$).gt.360.)
				LONF	:= (/f->$dNames(ddd)$ - 360./)
				flipFlag = False
			else if(max(f->$dNames(ddd)$).gt.180. .and. \
				min(f->$dNames(ddd)$).gt.180.)then
				LONF	:= (/f->$dNames(ddd)$ - 360./)
				flipFlag = False
			else
				LONF	:= (/f->$dNames(ddd)$/)
				flipFlag = False
			end if
			end if
			end if
			LONF!0 = "lon"
			LONF&lon = LONF
			LONF@units = "degrees_east"
	
			LON	:= LONF({Bds(2):Bds(3)})
			nx	= dimsizes(LON)
		end if
	end do
	
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
		V	:= getVar3d(f,dSource,(/var,levCode/),nDims,Bds,LEV@mf,flipFlag,LONF)
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
		end if
		end if
		CNTR(0,:) = (/V2&lat(ICTR(1,:))/)
		CNTR(1,:) = (/V2&lon(ICTR(0,:))/)
	end if
	end if
	
	
	; 9) Determine if CNTR at this level is within 1 km per 1 hPa of CNTR of two levels below
	do kkk = 0,NZ-1
	
		if(kkk.eq.1)then
			gcdist1	= gc_latlon(CNTR(0,kkk),CNTR(1,kkk),CNTR(0,kkk-1),CNTR(1,kkk-1),2,4)
			lim1	= gcdist1/(abs(LEV(kkk)-LEV(kkk-1))/LEV@mf)
			CNTR@lim(kkk) = lim1
			
			;if(CNTR(0,kkk).eq.CNTR(0,kkk-1) .and. CNTR(1,kkk).eq.CNTR(1,kkk-1))then		CNTR@use(kkk) = True
			;										continue
			;end if
			
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
			
			; Check if this center is the same as the two centers below it. If so, set to True
			;if(      CNTR(0,kkk).eq.CNTR(0,kkk-1)\
			;   .and. CNTR(1,kkk).eq.CNTR(1,kkk-1))then	CNTR@use(kkk) = True
			;						continue
			;else if(      CNTR(0,kkk).eq.CNTR(0,kkk-2)\
			;        .and. CNTR(1,kkk).eq.CNTR(1,kkk-2))then	CNTR@use(kkk) = True
			;						continue
			;end if
			;end if
			
			if(lim1.le.1.)then		CNTR@use(kkk) = True
			else if(lim2.le.1.)then		CNTR@use(kkk) = True
			else				CNTR@use(kkk) = False
			end if
			end if
			if(.not.CNTR@use(kkk-1) .or.\
			   .not.CNTR@use(kkk-2))then	CNTR@use(kkk) = False	end if
		else
			CNTR@use(kkk) = True
			CNTR@lim(kkk) = 0.
		end if
		end if
		
		;print(LEV(kkk)+"  "+CNTR(0,kkk)+"  "+CNTR(1,kkk)+"  "+CNTR@lim(kkk)+"  "+CNTR@use(kkk))
		;exit
	end do
	;exit
	
	
	; 10) Flip vertical dimension back
	if(flipLev)then   CNTR = CNTR(:,::-1)
			  CNTR@use = CNTR@use(::-1)
			  CNTR@lim = CNTR@lim(::-1)
			  CNTR@value = CNTR@value(::-1)
			  LEV = LEV(::-1)
	end if

	
	; 11) Return the list of TC centers 
	return(CNTR)
end
