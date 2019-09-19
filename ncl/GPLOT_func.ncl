;===============================================================
; GPLOT_func.ncl
;
; Higher-level functions and procedures for GPLOT.ncl are stored
; in this file to stay organized. Note that more basic functions
; and procedures are located in GPLOT_util.ncl. Note that more
; advanced functions and procedures are located in GPLOT_main.ncl
; Functions are given in alphabetical order.
;
; Written By:	  Ghassan Alaka, Jr.
;
; Created:	  September 2, 2015
; Last Modified:  April 24, 2017
;
; FUNCTION LIST
; 1) atcfRead
; 2) getTopo
; 3) getVar2d
; 4) getVar3d
; 5) getVarXC
;
;
;
; Feel free to make changes to this file, but please remember
; that any changes to functions/procedures defined here will
; need to be updated in any parent NCL script that calls it.
;
; Any bugs/errors should be submitted to ghassan.alaka@noaa.gov
;
;================================================================

;================================================================
; Universal Constants
;================================================================

load "$GPLOT_DIR/ncl/GPLOT_util.ncl"

external MWAVG    "$GPLOT_DIR/ncl/wrapit/mwavg.so"
external HBFILTER "$GPLOT_DIR/ncl/wrapit/hbfilter.so"

g		= 9.80665
pi		= 3.14159265359
d2r		= pi/180.
r		= 6371000.
omega		= 0.0000727
ms2kts		= 1.94384449
fval		= 9.96921e+36
fval2		= -2147483647
fval3		= 1.0e+20



;
;================================================================
; add_disclaimer

undef("add_disclaimer")
procedure add_disclaimer (wks[1],plot[1]:graphic,EXPT[1]:string,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float,JUST[1]:string)
local txres, MSG, DISCLAIMER
begin

	; Define resources
	txres			:= True
	txres@txFontHeightF	= TXHGT
	txres@txFontColor	= "black"
	txres@txFont		= 25
	
	; Get the right justification
	if(str_lower(JUST).eq."left")then	txres@txJust	= "CenterLeft"
	else if(str_lower(JUST).eq."center")	txres@txJust	= "CenterCenter"
	else if(str_lower(JUST).eq."right")	txres@txJust	= "CenterRight"
	else					txres@txJust	= "CenterLeft"
	end if  end if  end if

	; Choose the disclaimer message
	if(EXPT.eq."fvGFS_ATL")then	MSG = "*Experimental Product of HRD & GFDL*"
	else				MSG = "*Experimental Product of NOAA/AOML/HRD*"
	end if
	
	; Add title to existing workstation
	DISCLAIMER = gsn_add_text(wks,plot,MSG,XLOC,YLOC,txres)

end



;
;================================================================
; add_graphic_title

undef("add_graphic_title")
procedure add_graphic_title (wks[1],VAR[1]:string,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float)
local txres, TITLE
begin

	; Define resources
	txres			:= True
	txres@txJust		= "CenterLeft"
	txres@txFontColor	= "black"
	txres@txFont		= 26
	txres@txFontHeightF	= TXHGT
	
	; Get the title
	TITLE = getPlotTitle(VAR)
	
	; Add title to the existing workstation
	gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)

end



;
;================================================================
; add_model_title

undef("add_model_title")
procedure add_model_title (wks[1],EXPT[1]:string,ENSID[1],XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float)
local txres, TITLE
begin

	; Define resources
	txres			:= True
	txres@txJust		= "CenterLeft"
	txres@txFontHeightF	= TXHGT
	txres@txFontColor	= "black"
	txres@txFont		= 26
	
	; Get the title
	TITLE = getExptInfo(EXPT,"title")
	if (ENSID.ne."") TITLE=TITLE+" Mem:"+ENSID end if
	
	; Add title to existing workstation
	gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)

end



;
;================================================================
; add_mslp_title

undef("add_mslp_label")
procedure add_mslp_label (wks[1],MSLP[1]:float,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float,JUST[1]:string)
local txres, TITLE
begin

	; Define resources
	txres			:= True
	txres@txFontColor	= "brown"
	txres@txFont		= 25
	txres@txFontHeightF	= TXHGT
	
	; Get the right justification
	if(str_lower(JUST).eq."left")then	txres@txJust	= "CenterLeft"
	else if(str_lower(JUST).eq."center")	txres@txJust	= "CenterCenter"
	else if(str_lower(JUST).eq."right")	txres@txJust	= "CenterRight"
	else					txres@txJust	= "CenterLeft"
	end if  end if  end if
	
	TITLE = "MSLP = "+sprintf("%7.2f",MSLP)+" mb"
	
	; Add title to the existing workstation
	gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)

end



;
;================================================================
; add_sid_label

undef("add_sid_label")
procedure add_sid_label (wks[1],plot[1]:graphic,SID[1]:string,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float)
local txres1, txres2, TEXT
begin

	TEXT = new(2,"graphic")

	; Define inner text resources
	txres1			:= True
	txres1@txJust		= "CenterCenter"
	txres1@txPerimOn	= False
	txres1@txPerimColor	= "black"
	txres1@txFontColor	= "grey10"
	txres1@txFont		= 26
	txres1@txFontHeightF	= TXHGT
	
	; Define ouline text resources
	txres2			:= True
	txres2@txJust		= "CenterCenter"
	txres2@txPerimOn	= False
	txres2@txPerimColor	= "black"
	txres2@txFontColor	= "white"
	txres2@txFont		= 126
	txres2@txFontThicknessF	= 4.0
	txres2@txFontHeightF	= TXHGT
	
	; Add title to the existing plot
	TEXT(0)	= gsn_add_text(wks,plot,SID,XLOC,YLOC,txres1)
	TEXT(1)	= gsn_add_text(wks,plot,SID,XLOC,YLOC,txres2)

end



;
;================================================================
; add_storm_marker

undef("add_storm_marker")
procedure add_storm_marker (wks[1],plot[1]:graphic,INT[1]:float,SID[1]:integer,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float)
local txres1, txres2, TEXT
begin

	TEXT = new(2,"graphic")

	; Define text resources (text serves as marker)
	txres1			= True
	txres1@txJust		= "CenterCenter"
	txres1@txPerimOn	= False
	txres1@txPerimColor	= "black"
	txres1@txFontHeightF	= TXHGT
	txres1@txFontColor	= "red"
				
	; Define text outline resources (text serves as marker)
	txres2			= txres1
	txres2@txFontColor	= "black"
	txres2@txFontThicknessF	= 5.0
				
	if(INT.ge.64.)then	txres1@txFont	= 37
				TEXT(0)		= gsn_add_text(wks,plot,"p",XLOC,YLOC,txres1)
				txres2@txFont	= 137
				TEXT(1)		= gsn_add_text(wks,plot,"p",XLOC,YLOC,txres2)
	else if(INT.lt.64. .and.  INT.ge.34.)then
				txres1@txFont	= 35
				TEXT(0)		= gsn_add_text(wks,plot,"m",XLOC,YLOC,txres1)
				txres2@txFont	= 135
				TEXT(1)		= gsn_add_text(wks,plot,"m",XLOC,YLOC,txres2)
	else if(SID.gt.50)then	txres1@txFont	= 22
				TEXT(0)	= gsn_add_text(wks,plot,"x",XLOC,YLOC,txres1)
				txres2@txFont	= 122
				TEXT(1)		= gsn_add_text(wks,plot,"x",XLOC,YLOC,txres2)
	else			txres1@txFont	= 22
				TEXT(0)		= gsn_add_text(wks,plot,"L",XLOC,YLOC,txres1)
				txres2@txFont	= 122
				TEXT(1)		= gsn_add_text(wks,plot,"L",XLOC,YLOC,txres2)
	end if  end if  end if
	
end



;
;================================================================
; add_storm_title

undef("add_storm_title")
procedure add_storm_title (wks[1],TITLE[1]:string,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float,JUST[1]:string)
local txres, TITLE, T_SPLT, T1, T2, T3
begin

	; Define resources
	txres			:= True
	txres@txFontColor	= "black"
	txres@txFont		= 25
	txres@txFontHeightF	= TXHGT
	
	; Get the correct justification
	if(str_lower(JUST).eq."left")then	txres@txJust	= "CenterLeft"
	else if(str_lower(JUST).eq."center")	txres@txJust	= "CenterCenter"
	else if(str_lower(JUST).eq."right")	txres@txJust	= "CenterRight"
	else					txres@txJust	= "CenterLeft"
	end if  end if  end if

	; Split the title string to get the number of items (e.g., storms)
	T_SPLT = str_split(TITLE,",")

	; Add title to the existing workstation
	if(dimsizes(T_SPLT).le.4)then
		gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)
	else if(dimsizes(T_SPLT).eq.5)then
		T1 = T_SPLT(0)+","+T_SPLT(1)+","+T_SPLT(2)+","+T_SPLT(3)
		T2 = T_SPLT(4)
		gsn_text_ndc(wks,T1,XLOC,YLOC,txres)
		gsn_text_ndc(wks,T2,XLOC,YLOC+0.010,txres)
	else if(dimsizes(T_SPLT).gt.5 .and. dimsizes(T_SPLT).le.8)then
		T1 = T_SPLT(0)+","+T_SPLT(1)+","+T_SPLT(2)+","+T_SPLT(3)
		T2 = ""
		do ttt = 4,dimsizes(T_SPLT)-2
			T2 = T2+T_SPLT(ttt)+","
		end do
		T2 = T2+T_SPLT(dimsizes(T_SPLT)-1)
		gsn_text_ndc(wks,T1,XLOC,YLOC,txres)
		gsn_text_ndc(wks,T2,XLOC,YLOC+0.010,txres)
	else if(dimsizes(T_SPLT).eq.9)then
		T1 = T_SPLT(0)+","+T_SPLT(1)+","+T_SPLT(2)+","+T_SPLT(3)
		T2 = T_SPLT(4)+","+T_SPLT(5)+","+T_SPLT(6)+","+T_SPLT(7)
		T3 = T_SPLT(8)
		gsn_text_ndc(wks,T1,XLOC,YLOC,txres)
		gsn_text_ndc(wks,T2,XLOC,YLOC+0.010,txres)
		gsn_text_ndc(wks,T3,XLOC,YLOC+0.020,txres)
	else if(dimsizes(T_SPLT).gt.9 .and. dimsizes(T_SPLT).le.12)then
		T1 = T_SPLT(0)+","+T_SPLT(1)+","+T_SPLT(2)+","+T_SPLT(3)
		T2 = T_SPLT(4)+","+T_SPLT(5)+","+T_SPLT(6)+","+T_SPLT(7)
		T3 = ""
		do ttt = 8,dimsizes(T_SPLT)-2
			T3 = T3+T_SPLT(ttt)+","
		end do
		T3 = T3+T_SPLT(dimsizes(T_SPLT)-1)
		gsn_text_ndc(wks,T1,XLOC,YLOC,txres)
		gsn_text_ndc(wks,T2,XLOC,YLOC+0.010,txres)
		gsn_text_ndc(wks,T3,XLOC,YLOC+0.020,txres)
	end if  end if  end if  end if  end if

end



;
;================================================================
; add_time_title

undef("add_time_title")
procedure add_time_title (wks[1],TITLE[1]:string,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float,JUST[1]:string)
local txres
begin

	; Define resources
	txres			:= True
	txres@txFontColor	= "black"
	txres@txFont		= 25
	txres@txFontHeightF	= TXHGT
	
	; Get the right justification
	if(str_lower(JUST).eq."left")then	txres@txJust	= "CenterLeft"
	else if(str_lower(JUST).eq."center")	txres@txJust	= "CenterCenter"
	else if(str_lower(JUST).eq."right")	txres@txJust	= "CenterRight"
	else					txres@txJust	= "CenterLeft"
	end if  end if  end if
	
	; Add title to the existing workstation
	gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)

end



;
;================================================================
; add_time_title

undef("add_vmax_label")
procedure add_vmax_label (wks[1],VMAX[1]:float,XLOC[1]:float,YLOC[1]:float,TXHGT[1]:float,JUST[1]:string)
local txres, TITLE
begin

	; Define resources
	txres			:= True
	txres@txFontColor	= "brown"
	txres@txFont		= 25
	txres@txFontHeightF	= TXHGT
	
	; Get the right justification
	if(str_lower(JUST).eq."left")then	txres@txJust	= "CenterLeft"
	else if(str_lower(JUST).eq."center")	txres@txJust	= "CenterCenter"
	else if(str_lower(JUST).eq."right")	txres@txJust	= "CenterRight"
	else					txres@txJust	= "CenterLeft"
	end if  end if  end if
	
	TITLE = "VMAX = "+sprintf("%7.2f",VMAX)+" kt"
	
	; Add title to the existing workstation
	gsn_text_ndc(wks,TITLE,XLOC,YLOC,txres)

end



;
;================================================================
; adeckRead

undef("adeckRead")
function adeckRead (atcfFile[1]:string,IMODEL[1]:string,IDATE[1]:string)
local fileChk, DATA, windRad, tmp, BLOCK, NStag, EWtag
begin

	; 1. Read the ATCF data
	fileChk		= systemfunc("ls "+atcfFile+" 2>/dev/null")
	if(ismissing(fileChk(0)))then	print("ERROR: atcfRead: ATCF file not found.")
					print("ERROR: atcfRead: "+atcfFile)
	end if
	DATA		:= asciiread(atcfFile,-1,"string")


	; 2. Get Wind Radii - only keep indices for 34 kts.
	windRad		:= toint(str_squeeze(str_get_field(DATA,12,",")))
	allDates	:= str_squeeze(str_get_field(DATA,3,","))
	allModels	:= str_squeeze(str_get_field(DATA,5,","))
	if(IDATE.eq."")then
		vals	= ind(windRad.le.34 .and. isStrSubset2(allModels,IMODEL))
	else
		vals	= ind(windRad.le.34 .and. allDates.eq.IDATE .and. isStrSubset2(allModels,IMODEL))
	end if
	N		= dimsizes(vals)
	BLOCK		= new((/4,N/),"float")
	if(ismissing(vals(0)))then	print("ERROR: atcfRead: No valid entries found.")
					return(BLOCK)
	end if
	
	if(IDATE.eq."")then
		IDATE2	= tostring(remove_duplicates(toint(allDates)))
	else
		IDATE2	= IDATE
	end if


	; 3. Parse out relevant data
	; Get Latitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),7,","))
	NStag		= where(isStrSubset2(tmp,"N"),"N","S")
	BLOCK(0,:)	= where(NStag.eq."N",1.*tofloat(str_sub_str(tmp,"N",""))/10.,-1.*tofloat(str_sub_str(tmp,"S",""))/10.)
	; Get Longitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),8,","))
	EWtag		= where(isStrSubset2(tmp,"W"),"W","E")
	BLOCK(1,:)	= where(EWtag.eq."W",-1.*tofloat(str_sub_str(tmp,"W",""))/10.,1.*tofloat(str_sub_str(tmp,"E",""))/10.)
	; Get Lead Time
	BLOCK(2,:)	= toint(str_squeeze(str_get_field(DATA(vals),6,",")))
	; Get Intensity
	BLOCK(3,:)	= toint(str_squeeze(str_get_field(DATA(vals),9,",")))
	; Data Labels
	BLOCK@data	= (/"latitude","longitude","lead_time","intensity"/)
	BLOCK@N		= N
	BLOCK@models	= str_squeeze(str_get_field(DATA(vals),5,","))
	BLOCK@init_date	= IDATE2
	BLOCK@SID = str_squeeze(str_get_field(DATA(vals(0)),1,",")) + str_squeeze(str_get_field(DATA(vals(0)),2,","))
	
	
	
	; 4. Return BLOCK to caller.
	return(BLOCK)
		
end
;adeckRead



;
;================================================================
; atcfRead

undef("atcfRead")
function atcfRead (atcfFile[1]:string)
local fileChk, DATA, windRad, tmp, BLOCK, NStag, EWtag, iii, NEQ, windInt
begin

	; 1. Read the ATCF data
	fileChk		= systemfunc("ls "+atcfFile+" 2>/dev/null")
	if(ismissing(fileChk(0)))then	print("ERROR: atcfRead: ATCF file not found.")
					print("ERROR: atcfRead: "+atcfFile)
	end if
	DATA		:= asciiread(atcfFile,-1,"string")


	; 2. Find NEQ entry to determine where the Wind Radii data are stored
    iii = 0
    windInt = -999
    do while (windInt.eq.-999)
        NEQ         := str_squeeze(str_get_field(DATA,iii,","))
        if(any(NEQ.eq."NEQ"))then   windInt := toint(iii-1)   end if
        iii = iii+1
    end do
        
    
        
    ; 2. Get Wind Radii - only keep indices for 34 kts.
	windRad		:= toint(str_squeeze(str_get_field(DATA,windInt,",")))
	vals		= ind(windRad.eq.34)
	N		= dimsizes(vals)
	BLOCK		= new((/4,100/),"float")
	if(ismissing(vals(0)))then	print("ERROR: atcfRead: No valid entries found.")
					exit
	end if


	; 3. Parse out relevant data
	; Get Latitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),windInt-5,","))
	NStag		= where(isStrSubset2(tmp,"N"),"N","S")
	BLOCK(0,:N-1)	= where(NStag.eq."N",1.*tofloat(str_sub_str(tmp,"N",""))/10.,-1.*tofloat(str_sub_str(tmp,"S",""))/10.)
	; Get Longitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),windInt-4,","))
	EWtag		= where(isStrSubset2(tmp,"W"),"W","E")
	BLOCK(1,:N-1)	= where(EWtag.eq."W",-1.*tofloat(str_sub_str(tmp,"W",""))/10.,1.*tofloat(str_sub_str(tmp,"E",""))/10.)
	; Get Lead Time
	BLOCK(2,:N-1)	= toint(str_squeeze(str_get_field(DATA(vals),windInt-6,",")))
	; Get Intensity
	BLOCK(3,:N-1)	= toint(str_squeeze(str_get_field(DATA(vals),windInt-3,",")))
	; Data Labels
	BLOCK@data = (/"latitude","longitude","lead_time","intensity"/)
	BLOCK@N	= N
	BLOCK@SID = str_squeeze(str_get_field(DATA(vals(0)),1,",")) + str_squeeze(str_get_field(DATA(vals(0)),2,","))
	
	
	
	; 4. Return BLOCK to caller.
	return(BLOCK)
		
end
;atcfRead



;================================================================
; bdeckRead

undef("bdeckRead")
function bdeckRead (bestFile[1]:string,IDATE[1]:string)
local fileChk, DATA, windRad, tmp, BLOCK, NStag, EWtag, tunits, utc0, utc, bbb
begin

	; 1. Read the ATCF data
	fileChk		= systemfunc("ls "+bestFile+" 2>/dev/null")
	if(ismissing(fileChk(0)))then	print("ERROR: bdeckRead: ATCF file not found.")
					print("ERROR: bdeckRead: "+atcfFile)
	end if
	DATA		:= asciiread(bestFile,-1,"string")


	; 2. Get Wind Radii - only keep indices for 34 kts.
	windRad		:= toint(str_squeeze(str_get_field(DATA,12,",")))
	vals		= ind(windRad.le.34)
	N		= dimsizes(vals)
	allDates	:= str_squeeze(str_get_field(DATA(vals),3,","))
	if(IDATE.eq."none")then	  IDATE = allDates(0)   end if
	BLOCK		= new((/5,N/),"float")
	if(ismissing(vals(0)))then	print("ERROR: bdeckRead: No valid entries found.")
					exit
	end if
	
	
	; Get Lead Times
	tunits	= "hours since 1970-01-01 00:00;00"
	tmp	:= tochar(IDATE)
	utc0	= cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
				  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0)
	do bbb = 0,N-1
		tmp	:= tochar(allDates(bbb))
		utc	= cd_inv_calendar(toint(tostring(tmp(0:3))),toint(tostring(tmp(4:5))),\
					  toint(tostring(tmp(6:7))),toint(tostring(tmp(8:9))),0,0,tunits,0)
		BLOCK(2,bbb)	= toint(utc-utc0)
	end do

	; 3. Parse out relevant data
	; Get Latitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),7,","))
	NStag		= where(isStrSubset2(tmp,"N"),"N","S")
	BLOCK(0,:)	= where(NStag.eq."N",1.*tofloat(str_sub_str(tmp,"N",""))/10.,-1.*tofloat(str_sub_str(tmp,"S",""))/10.)
	; Get Longitude
	tmp		:= str_squeeze(str_get_field(DATA(vals),8,","))
	EWtag		= where(isStrSubset2(tmp,"W"),"W","E")
	BLOCK(1,:)	= where(EWtag.eq."W",-1.*tofloat(str_sub_str(tmp,"W",""))/10.,1.*tofloat(str_sub_str(tmp,"E",""))/10.)
	; Get Intensity
	BLOCK(3,:)	= toint(str_squeeze(str_get_field(DATA(vals),9,",")))
	; Get Pressure
	BLOCK(4,:)	= toint(str_squeeze(str_get_field(DATA(vals),10,",")))
	; Data Labels
	BLOCK@data	= (/"latitude","longitude","lead_time","intensity","pressure"/)
	BLOCK@N		= N
	BLOCK@type	= str_squeeze(str_get_field(DATA(vals),11,","))
	BLOCK@valid_times = allDates
	BLOCK@init_date	= IDATE
	
	
	
	; 4. Return BLOCK to caller.
	return(BLOCK)
		
end
;adeckRead



;
;================================================================
; get_dim_lat

undef("get_dim_lat")
function get_dim_lat (f:file,DIM[1]:string,BOCO[4]:float,TCLATS[*]:float,TCLONS[*]:float,i[1]:integer,TYPE[1]:integer)
local LAT, BDS, rngLat, rngLon, LLratio
begin

	; New boundaries
	BDS = BOCO
	

	; TYPE 0: take the boundaries as provided
	; TYPE 1: Take the entire latitude dimension from the file
	if(TYPE.eq.1)then	LAT	= (/f->$DIM$/)
				BDS(0)	= max(LAT)
				BDS(1)	= min(LAT)
	
	; TYPE 2: Take +/- 15 degrees around a TC center
	else if(TYPE.eq.2)then	if(ismissing(TCLATS(i)))then	print("ERROR: get_dim_lat: TC latitude not found.")
								exit
				end if
				BDS(0) = TCLATS(i)+15.
				BDS(1) = TCLATS(i)-15.
	
	; TYPE 3: Take +/- 4 degrees around a TC center		
	else if(TYPE.eq.3)then	if(ismissing(TCLATS(i)))then	print("ERROR: get_dim_lat: TC latitude not found.")
								exit
				end if
				BDS(0) = TCLATS(i)+4.
				BDS(1) = TCLATS(i)-4.

	; TYPE 4: Encompass the entire TC forecast track, if possible.
	else if(TYPE.eq.4)then	if(all(ismissing(TCLATS)))then	print("ERROR: get_dim_lat: No TC latitudes found.")
								exit
				end if
				if(all(ismissing(TCLONS)))then	print("ERROR: get_dim_lat: No TC longitudes found.")
								exit
				end if
				rngLat	= abs((max(TCLATS)+5.) - (min(TCLATS)-5.))
				rngLon	= abs((max(TCLONS)+5.) - (min(TCLONS)-5.))
				LLratio	= rngLat/rngLon
				if(LLratio.lt.0.5)then		BDS(0)	= max(TCLATS) + 5. + 0.5*abs(0.5*rngLon - rngLat)
								BDS(1)	= min(TCLATS) - 5. - 0.5*abs(0.5*rngLon - rngLat)
								rngLat	= abs(BDS(0)-BDS(1))
				else if(LLratio.gt.0.5)then	BDS(3)	= max(TCLONS) + 5. + 0.5*abs(2.*rngLat - rngLon)
								BDS(2)	= min(TCLONS) - 5. - 0.5*abs(2.*rngLat - rngLon)
								rngLon	= abs(BDS(2)-BDS(3))
				end if  end if
				print("MSG: New Ratio = "+(rngLat/rngLon))
	
	; TYPE 5: Take +/- 40 degrees around the initial TC location (for HWRF D01)
	else if(TYPE.eq.5)then	if(ismissing(TCLATS(0)))then	print("ERROR: get_dim_lat: Initial TC latitude not found.")
								exit
				end if
				BDS(0) = TCLATS(0)+40.
				BDS(1) = TCLATS(0)-40.
	
	; TYPE 6: Take +/- 10 degrees around the TC center
	else if(TYPE.eq.6)then	if(ismissing(TCLATS(i)))then	print("ERROR: get_dim_lat: TC latitude not found.")
								exit
				end if
				BDS(0) = TCLATS(i)+10.
				BDS(1) = TCLATS(i)-10.
	
	; TYPE 7: Take +/- 20 degrees around the TC center (used for GPLOT_ships)
	else if(TYPE.eq.7)then	if(ismissing(TCLATS(i)))then	print("ERROR: get_dim_lat: TC latitude not found.")
								exit
				end if
				BDS(0) = TCLATS(i)+20.
				BDS(1) = TCLATS(i)-20.
	end if  end if  end if  end if  end if  end if  end if
	
	
	; Create LAT if not already defined using BDS
	; If the BDS coordinates are not found exactly in LAT, then adjust
	; to coordinates that are outside of BDS.
	if(.not.isvar("LAT"))then
		if(BDS(0).lt.max(f->$DIM$))then
			if(f->$DIM$({BDS(0)}).ne.BDS(0))then
				IBDS0 = BDS(0)+abs(f->$DIM$(0) - f->$DIM$(1))
			end if
		end if
		if(.not.isvar("IBDS0"))then   IBDS0 = BDS(0)   end if
		if(BDS(1).gt.min(f->$DIM$))then
			if(f->$DIM$({BDS(1)}).ne.BDS(1))then
				IBDS1 = BDS(1)-abs(f->$DIM$(0) - f->$DIM$(1))
			end if
		end if
		if(.not.isvar("IBDS1"))then   IBDS1 = BDS(1)   end if
		LAT = (/f->$DIM$({IBDS0:IBDS1})/)
	end if
	
	; Correct LAT to be oriented the same as the file dimension.
	;if(f->$DIM$(1)-f->$DIM$(0).gt.0 .and. LAT(1)-LAT(0).lt.0)then
	;	LAT = LAT(::-1)
	;else if(f->$DIM$(1)-f->$DIM$(0).lt.0 .and. LAT(1)-LAT(0).gt.0)then
	;	LAT = LAT(::-1)
	;end if  end if
	
	; Update metadata
	LAT@BOCO	= BDS
	LAT@long_name	= "Latitude"
	LAT@units	= "degrees_north"
	LAT!0		= "lat"
	LAT&lat		= LAT
	

	return(LAT)

end



;
;================================================================
; get_dim_lon

undef("get_dim_lon")
function get_dim_lon (f:file,DIM[1]:string,BOCO[4]:float,TCLONS[*]:float,TCLATS[*]:float,i[1]:integer,TYPE[1]:integer)
local LON, LONF, BDS, rngLat, rngLon, LLratio, flipFlag, PIV
begin

	; New boundaries
	BDS = BOCO
	
	; Get the pivot longitude, if applicable
	if(isatt(BOCO,"PivotLon"))then	PIV = BOCO@PivotLon
	else				PIV = 180.
	end if
	
	; Check if the longitude should be pivoted
	if(isatt(BOCO,"doPivot"))then	DO_PIVOT = BOCO@doPivot
	else				DO_PIVOT = True
	end if
	
	; Check if the longitude coordinate is as expected.
	; -180<->180 needs to be treated differently than 0<->360
	if(min(f->$DIM$).eq.0.)then
		if(DO_PIVOT)then		;LONF	= (/where(lonFlip(f->$DIM$).ge.180.,\
						;			   lonFlip(f->$DIM$)-360.,\
						;			   lonFlip(f->$DIM$))/)
						LONF	= lonPivot(f->$DIM$,PIV)
						LONF	:= (/where(LONF.ge.PIV,LONF-360.,LONF)/)
						flipFlag= True
		else				LONF 	= (/f->$DIM$/)
						flipFlag= False
		end if
	else if(max(f->$DIM$).gt.360.)then	LONF	= (/f->$DIM$ - 360./)
						flipFlag= False
	else if(max(f->$DIM$).gt.180. .and. \
		min(f->$DIM$).gt.180.)then	LONF	= (/f->$DIM$ - 360./)
						flipFlag= False
	else					LONF	:= (/f->$DIM$/)
						flipFlag= False
	end if  end if  end if
	LONF!0 = "lon"
	LONF&lon = LONF
	LONF@units = "degrees_east"

	; TYPE 0: take the boundaries as provided (no action needed)
	; TYPE 1: Take the entire latitude dimension from the file
	if(TYPE.eq.1)then	LON	= (/LONF/)
				BDS(3)	= max(LON)
				BDS(2)	= min(LON)
	
	; TYPE 2: Take +/- 15 degrees around a TC center
	else if(TYPE.eq.2)then	if(ismissing(TCLONS(i)))then	print("ERROR: get_dim_lon: TC longitude not found.")
								exit
				end if
				BDS(3) = TCLONS(i)+15.
				BDS(2) = TCLONS(i)-15.
	
	; TYPE 3: Take +/- 4 degrees around a TC center		
	else if(TYPE.eq.3)then	if(ismissing(TCLONS(i)))then	print("ERROR: get_dim_lon: TC longitude not found.")
								exit
				end if
				BDS(3) = TCLONS(i)+4.
				BDS(2) = TCLONS(i)-4.

	; TYPE 4: Encompass the entire TC forecast track, if possible.
	else if(TYPE.eq.4)then	if(all(ismissing(TCLONS)))then	print("ERROR: get_dim_lon: No TC longitudes found.")
								exit
				end if
				if(all(ismissing(TCLATS)))then	print("ERROR: get_dim_lon: No TC latitudes found.")
								exit
				end if
				rngLat	= abs((max(TCLATS)+5.) - (min(TCLATS)-5.))
				rngLon	= abs((max(TCLONS)+5.) - (min(TCLONS)-5.))
				LLratio	= rngLat/rngLon
				if(LLratio.lt.0.5)then		BDS(0)	= max(TCLATS) + 5. + 0.5*abs(0.5*rngLon - rngLat)
								BDS(1)	= min(TCLATS) - 5. - 0.5*abs(0.5*rngLon - rngLat)
								rngLat	= abs(BDS(0)-BDS(1))
				else if(LLratio.gt.0.5)then	BDS(3)	= max(TCLONS) + 5. + 0.5*abs(2.*rngLat - rngLon)
								BDS(2)	= min(TCLONS) - 5. - 0.5*abs(2.*rngLat - rngLon)
								rngLon	= abs(BDS(3)-BDS(2))
				end if  end if
				print("MSG: New Ratio = "+(rngLat/rngLon))
	
	; TYPE 5: Take +/- 40 degrees around the initial TC location (for HWRF D01)
	else if(TYPE.eq.5)then	if(ismissing(TCLONS(0)))then	print("ERROR: get_dim_lon: Initial TC longitude not found.")
								exit
				end if
				BDS(3) = TCLONS(0)+40.
				BDS(2) = TCLONS(0)-40.
	
	; TYPE 6: Take +/- 10 degrees around the TC center
	else if(TYPE.eq.6)then	if(ismissing(TCLONS(i)))then	print("ERROR: get_dim_lon: TC longitude not found.")
								exit
				end if
				BDS(3) = TCLONS(i)+10.
				BDS(2) = TCLONS(i)-10.
	
	; TYPE 7: Take +/- 20 degrees around the TC center (used for GPLOT_ships)
	else if(TYPE.eq.7)then	if(ismissing(TCLONS(i)))then	print("ERROR: get_dim_lon: TC longitude not found.")
								exit
				end if
				BDS(3) = TCLONS(i)+20.
				BDS(2) = TCLONS(i)-20.
	end if  end if  end if  end if  end if  end if  end if

	
	; Create LON if not already defined using BDS
	; If the BDS coordinates are not found exactly in LON, then adjust
	; to coordinates that are outside of BDS.
	if(.not.isvar("LON"))then
		if(BDS(3).lt.max(LONF))then
			if(LONF({BDS(3)}).ne.BDS(3))then
				IBDS3 = BDS(3)+abs(LONF(0) - LONF(1))
			end if
		end if
		if(.not.isvar("IBDS3"))then   IBDS3 = BDS(3)   end if
		if(BDS(2).gt.min(LONF))then
			if(LONF({BDS(2)}).ne.BDS(2))then
				IBDS2 = BDS(2)-abs(LONF(0) - LONF(1))
			end if
		end if
		if(.not.isvar("IBDS2"))then   IBDS2 = BDS(2)   end if
		LON = (/LONF({IBDS3:IBDS2})/)
	end if
	
	; Correct LON to be oriented the same as LONF
	if(LONF(1)-LONF(0).gt.0 .and. LON(1)-LON(0).lt.0)then
		LON = LON(::-1)
	else if(LONF(1)-LONF(0).lt.0 .and. LON(1)-LON(0).gt.0)then
		LON = LON(::-1)
	end if  end if
	
	; Update metadata
	LON@BOCO	= BDS
	LON@LONF	= (/LONF/)
	LON@flipFlag	= flipFlag
	LON@long_name	= "Longitude"
	LON@units	= "degrees_east"
	LON!0		= "lon"
	LON&lon		= LON
	

	return(LON)

end



;
;================================================================
; getTopo

function getTopo (lat[*]:float,lon[*]:float,flipFlag:logical)
local fi,latsz,lonsz,topoF,topo
begin

	latsz	= dimsizes(lat)
	lonsz	= dimsizes(lon)

	iFile	= systemfunc("echo $NCARG_ROOT")+"topo/all10/topo0.1.nc"

	if(.not.isfilepresent(iFile))then	print("ERROR: No topography data.")
						exit
	end if

	fi	= addfile(iFile,"r")

	topoF	= fi->TOPO({lat(0):lat(latsz-1)},{lon(0):lon(lonsz-1)})
	
	topo	= area_hi2lores_Wrap(topoF&lon,topoF&lat,topoF,False,1,lon,lat,False)
	
	return(topo)

end


;
;================================================================
; getVar2d
;
;

function getVar2d (f:file,dSource[1]:string,varInfo[*]:string,nDims[1]:integer,iBds[*]:float,mf[1]:float,flags[*]:logical,lon[*]:numeric)
local tmp, lev1, lev2, levFlag, flipFlag, rmVortex, lonsz, tmpU, tmpV, tmpU1, tmpU2, tmpV1, tmpV2, \
      V, V3d, wgt, wgtTot, nLevs, ppp, fff, allLevs, N, ISZ, JSZ, KSZ, DX, NP, PIV
begin

	; Set the minimum pressure difference between levels
	; Eventually should treat this as function input
	NP = 25.

	; Determine which dimensions are in this variable
	if(  any(isStrSubset2(nDims@dNames,"lat"))\
	.or. any(isStrSubset2(nDims@dNames,"grid_yt")))then	latDim = True
								latIND = ind(     isStrSubset2(nDims@dNames,"lat")\
									     .or. isStrSubset2(nDims@dNames,"grid_yt"))
								; Get resolution
								DX = abs(f->$nDims@dNames(latIND)$(1) - f->$nDims@dNames(latIND)$(0))
								latNAME = nDims@dNames(latIND)
	else							latDim = False
	end if
	if(  any(isStrSubset2(nDims@dNames,"lon"))\
	.or. any(isStrSubset2(nDims@dNames,"grid_xt")))then	lonDim = True
								lonIND = ind(     isStrSubset2(nDims@dNames,"lon")\
									     .or. isStrSubset2(nDims@dNames,"grid_xt"))
								lonNAME = nDims@dNames(lonIND)
	else							lonDim = False
	end if
	if(  any(isStrSubset2(nDims@dNames,"ISBL"))\
	.or. any(isStrSubset2(nDims@dNames,"HGT"))\
	.or. any(isStrSubset2(nDims@dNames,"lev"))\
	.or. any(isStrSubset2(nDims@dNames,"lv")))then		levDim = True
								levIND = ind(isStrSubset2(nDims@dNames,"ISBL") .or.\
									     isStrSubset2(nDims@dNames,"HGT")  .or.\
									     isStrSubset2(nDims@dNames,"lev")  .or.\
									     isStrSubset2(nDims@dNames,"lv")        )
								levNAME = nDims@dNames(levIND)
	else							levDim = False
	end if
	if(any(isStrSubset2(nDims@dNames,"time")))then		timeDim = True
								if(dimsizes(varInfo).lt.3)then	print("ERROR: getVar2d: Time index is missing. Check 'varInfo'.")
												exit
								end if
								tIND	 = toint(varInfo(2))
								tNAME	= nDims@dNames(tIND)
	else							timeDim = False
	end if
	
	
	; Need at least lat and lon for the function
	if(.not.latDim .or. .not.lonDim)then	print("ERROR: getVar2d: Must have Latitude and Longitude dimensions.")
						exit
	end if
	
	
	; Get the pivot longitude, if applicable
	if(isatt(iBds,"PivotLon"))then	PIV = iBds@PivotLon
	else				PIV = 180.
	end if
	
	
	; Check for flags to do extra work
	; flipFlag: reorders the x-direction from 0-365 --> -180-180
	; rmVortex: removes the TC vortex as in Kurihara et al. (1993)
	do fff = 0,dimsizes(flags)-1
		if(.not.isvar("flipFlag"))then
			if(fff.eq.0)then	flipFlag = flags(0)
			else			flipFlag = False
			end if
		else if(.not.isvar("rmVortex"))then
			if(fff.eq.1)then	rmVortex = flags(1)
			else			rmVortex = False
			end if
		end if
		end if
	end do
	if(.not.isvar("rmVortex"))then rmVortex = False end if
	if(.not.isvar("flipFlag"))then flipFlag = False end if
	if(rmVortex)then	if(DX.ge.0.30)then				N = 15
				else if(DX.lt.0.30 .and. DX.ge.0.15)then	N = 30
				else if(DX.lt.0.15 .and. DX.ge.0.05)then	N = 45
				else if(DX.lt.0.05)then				N = 60
				else						N = 15
				end if  end if  end if  end if
				;N = 60
	end if
	;print(rmVortex)


	; Longer string means we are dealing with multiple vertical levels
	; e.g., average, difference, etc.
	if(strlen(varInfo(1)).gt.4)then
		;if(nDims.eq.2)then	print("ERROR: getVar2d: No Vertical Dimension.")
		;			exit
		;end if
		if(strlen(varInfo(1)).ne.9)then
			print("ERROR: getVar2d: Level Info must have a length of 9: "+varInfo(1))
			exit
		end if
		
		tmp	= tochar(varInfo(1))
		lev1	= tofloat(tostring(tmp(:3)))
		lev2	= tofloat(tostring(tmp(4:7)))
		levFlag	= tostring(tmp(8))
		delete(tmp)

			
		if(levFlag.eq."d")then
			if(varInfo(0).eq."UV")then
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					tmpU1	= f->$findVarName(dSource,"U",tostring(toint(lev1)))$({mf*lev1},{iBds(0):iBds(1)},:)
					tmpU2	= f->$findVarName(dSource,"U",tostring(toint(lev2)))$({mf*lev2},{iBds(0):iBds(1)},:)
					tmpV1	= f->$findVarName(dSource,"V",tostring(toint(lev1)))$({mf*lev1},{iBds(0):iBds(1)},:)
					tmpV2	= f->$findVarName(dSource,"V",tostring(toint(lev2)))$({mf*lev2},{iBds(0):iBds(1)},:)
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					tmpU1	= f->$findVarName(dSource,"U",tostring(toint(lev1)))$(tIND,{iBds(0):iBds(1)},:)
					tmpU2	= f->$findVarName(dSource,"U",tostring(toint(lev2)))$(tIND,{iBds(0):iBds(1)},:)
					tmpV1	= f->$findVarName(dSource,"V",tostring(toint(lev1)))$(tIND,{iBds(0):iBds(1)},:)
					tmpV2	= f->$findVarName(dSource,"V",tostring(toint(lev2)))$(tIND,{iBds(0):iBds(1)},:)
				end if  end if
				ISZ	= dimsizes(tmpU1(0,:))
				JSZ	= dimsizes(tmpU1(:,0))
				if(isatt(tmpU1,"units"))then   units = tmpU1@units   end if
				
				if(flipFlag)then	;tmpU1	:= lonFlip(tmpU1)
							;tmpU2	:= lonFlip(tmpU2)
							;tmpV1	:= lonFlip(tmpV1)
							;tmpV2	:= lonFlip(tmpV2)
							tmpU1	:= lonPivot(tmpU1,PIV)
							tmpU2	:= lonPivot(tmpU2,PIV)
							tmpV1	:= lonPivot(tmpV1,PIV)
							tmpV2	:= lonPivot(tmpV2,PIV)
				end if
				
				if(rmVortex)then	tmp	:= new((/1,JSZ,ISZ/),"float")
							tmp(0,:,:) = tmpU1
							tmpU1	:= tmp
							HBFILTER::hbfilter(tmp,tmpU1,N,ISZ,JSZ,1)
							tmpU1	:= tmpU1(0,:,:)
							
							tmp(0,:,:) = tmpU2
							tmpU2	:= tmp
							HBFILTER::hbfilter(tmp,tmpU2,N,ISZ,JSZ,1)
							tmpU2	:= tmpU2(0,:,:)
							
							tmp(0,:,:) = tmpV1
							tmpV1	:= tmp
							HBFILTER::hbfilter(tmp,tmpV1,N,ISZ,JSZ,toint(1))
							tmpV1	:= tmpV1(0,:,:)
							
							tmp(0,:,:) = tmpV2
							tmpV2	:= tmp
							HBFILTER::hbfilter(tmp,tmpV2,N,ISZ,JSZ,toint(1))
							tmpV2	:= tmpV2(0,:,:)
				end if
				
				tmpU	= (/tmpU1 - tmpU2/)
				tmpV	= (/tmpV1 - tmpV2/)
				V1	= sqrt(tmpU1^2.+tmpV1^2.)
				V2	= sqrt(tmpU2^2.+tmpV2^2.)

				; Take the magnitude of the winds
				V	= sqrt(tmpU^2.+tmpV^2.)
				;V	= (/V1 - V2/)
				V@units = "m s-1"
				delete([/tmpU,tmpU1,tmpU2,tmpV,tmpV1,tmpV2/])
			else
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					tmpV1	= f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$({mf*lev1},{iBds(0):iBds(1)},:)
					tmpV2	= f->$findVarName(dSource,varInfo(0),tostring(toint(lev2)))$({mf*lev2},{iBds(0):iBds(1)},:)
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					tmpV1	= f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$(tIND,{iBds(0):iBds(1)},:)
					tmpV2	= f->$findVarName(dSource,varInfo(0),tostring(toint(lev2)))$(tIND,{iBds(0):iBds(1)},:)
				end if  end if
				ISZ	= dimsizes(tmpV1(0,:))
				JSZ	= dimsizes(tmpV1(:,0))
				
				if(flipFlag)then	;tmpV1	:= lonFlip(tmpV1)
							;tmpV2	:= lonFlip(tmpV2)
							tmpV1	:= lonPivot(tmpV1,PIV)
							tmpV2	:= lonPivot(tmpV2,PIV)
				end if
								
				if(rmVortex)then	tmp	:= new((/1,JSZ,ISZ/),"float")
							tmp(0,:,:) = tmpV1
							tmpV1	:= tmp
							HBFILTER::hbfilter(tmp,tmpV1,N,ISZ,JSZ,toint(1))
							tmpV1	:= tmpV1(0,:,:)
							
							tmp(0,:,:) = tmpV2
							tmpV2	:= tmp
							HBFILTER::hbfilter(tmp,tmpV2,N,ISZ,JSZ,toint(1))
							tmpV2	:= tmpV2(0,:,:)
				end if
								
				V = (/tmpV1 - tmpV2/)
				
				if(isfilevaratt(f,findVarName(dSource,varInfo(0),tostring(toint(lev1))),"units"))then
					V@units = f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$@units
				end if
			end if
		else if(levFlag.eq."a")then
			if(varInfo(0).eq."UV")then
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					tmpU	= f->$findVarName(dSource,"U",tostring(toint(lev1)))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
					tmpV	= f->$findVarName(dSource,"V",tostring(toint(lev1)))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
					if(isatt(tmpU,"units"))then   units = tmpU@units   end if
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					levChk	= fspan(lev1,lev2,toint(abs(lev1-lev2)/25.)+1)
					do kkk = 0,dimsizes(levChk)-1
						if(isfilevar(f,findVarName(dSource,"U",tostring(toint(levChk(kkk))))))then
							if(.not.isvar("tmpU"))then
								tmp	:= f->$findVarName(dSource,"U",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								if(isatt(tmp,"units"))then   units = tmp@units   end if
								sz	:= dimsizes(tmp)
								tmpU	= new((/1,sz(0),sz(1)/),"float")
								tmpU(0,:,:) = tmp
								copy_VarAtts(tmp,tmpU)
								
								tmp	:= f->$findVarName(dSource,"V",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								sz	:= dimsizes(tmp)
								tmpV	= new((/0,sz(0),sz(1)/),"float")
								tmpV(0,:,:) = tmp
								copy_VarAtts(tmp,tmpV)
							else
								sz	:= dimsizes(tmpU)
								tmp	:= new((/1,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,"U",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								tmpU	:= array_append_record(tmpU,tmp,0)
								
								sz	:= dimsizes(tmpV)
								tmp	:= new((/0,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,"V",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								tmpV	:= array_append_record(tmpV,tmp,0)
							end if
						end if
					end do
				end if  end if
				ISZ	= dimsizes(tmpU(0,0,:))
				JSZ	= dimsizes(tmpU(0,:,0))
				KSZ	= dimsizes(tmpU(:,0,0))
				
				if(flipFlag)then	;tmpU	:= lonFlip(tmpU)
							;tmpV	:= lonFlip(tmpV)
							tmpU	:= lonPivot(tmpU,PIV)
							tmpV	:= lonPivot(tmpV,PIV)
				end if
				
				if(rmVortex)then	tmp	:= tmpU
							HBFILTER::hbfilter(tmp,tmpU,N,ISZ,JSZ,KSZ)
							
							tmp	:= tmpV
							HBFILTER::hbfilter(tmp,tmpV,N,ISZ,JSZ,KSZ)
				end if
				
				tmpU	:= dim_avg_n((/tmpU/),0)
				tmpV	:= dim_avg_n((/tmpV/),0)
				
				V	= sqrt(tmpU^2.+tmpV^2.)
				if(isvar("units"))then  V@units = units
				else			V@units = "m s-1"
				end if
				delete([/tmpU,tmpV/])
			else
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					V	= f->$findVarName(dSource,varInfo(0),tostring(lev1))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					levChk	= fspan(lev1,lev2,toint(abs(lev1-lev2)/25.)+1)
					do kkk = 0,dimsizes(levChk)-1
						if(isfilevar(f,findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))))then
							if(.not.isvar("V"))then
								tmp	:= f->$findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								sz	:= dimsizes(tmp)
								V	= new((/1,sz(0),sz(1)/),"float")
								V(0,:,:) = tmp
								copy_VarAtts(tmp,V)
							else
								sz	:= dimsizes(V)
								tmp	:= new((/1,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								V	:= array_append_record(V,tmp,0)
							end if
						end if
					end do
				end if  end if
				ISZ	= dimsizes(V(0,0,:))
				JSZ	= dimsizes(V(0,:,0))
				KSZ	= dimsizes(V(:,0,0))
							
				if(flipFlag)then	;V	:= lonFlip(V)
							V	:= lonPivot(V,PIV)
				end if
				if(rmVortex)then	tmp	:= V
							HBFILTER::hbfilter(tmp,V,N,ISZ,JSZ,KSZ)
				end if
				V	:= (/dim_avg_n(V,0)/)
				
				if(isfilevaratt(f,findVarName(dSource,varInfo(0),tostring(toint(lev1))),"units"))then
					V@units = f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$@units
				end if
			end if
		else if(levFlag.eq."m")then
			if(varInfo(0).eq."UV")then
				; Traditionally, each output file has dimensions of LEV/LAT/LON
				; and each output file represents a single forecast time
				; Examples: HWRF, GFS
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					tmpU	= f->$findVarName(dSource,"U",tostring(toint(lev1)))$(ind(f->$nDims@dNames(levIND)$.ge.(mf*lev1) .and. \
													  f->$nDims@dNames(levIND)$.le.(mf*lev2) .and. \
													  toint(f->$nDims@dNames(levIND)$)%toint(mf*NP).eq.0) ,\
												      {iBds(0):iBds(1)},:)
					tmpV	= f->$findVarName(dSource,"V",tostring(toint(lev1)))$(ind(f->$nDims@dNames(levIND)$.ge.(mf*lev1) .and. \
													  f->$nDims@dNames(levIND)$.le.(mf*lev2) .and. \
													  toint(f->$nDims@dNames(levIND)$)%toint(mf*NP).eq.0) ,\
												      {iBds(0):iBds(1)},:)
					if(isatt(tmpU,"units"))then   units = tmpU@units   end if
				
				; Sometimes, each output file has dimensions of TIME/LAT/LON
				; and each output file represents a single model level
				; Examples: fvGFS
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					levChk	= fspan(lev1,lev2,toint(abs(lev1-lev2)/NP)+1)
					do kkk = 0,dimsizes(levChk)-1
						if(isfilevar(f,findVarName(dSource,"U",tostring(toint(levChk(kkk))))))then
							if(.not.isvar("tmpU"))then
								tmp	:= f->$findVarName(dSource,"U",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								if(isatt(tmp,"units"))then   units = tmp@units   end if
								sz	:= dimsizes(tmp)
								tmpU	= new((/1,sz(0),sz(1)/),"float")
								tmpU(0,:,:) = tmp
								copy_VarAtts(tmp,tmpU)
								
								tmp	:= f->$findVarName(dSource,"V",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								sz	:= dimsizes(tmp)
								tmpV	= new((/1,sz(0),sz(1)/),"float")
								tmpV(0,:,:) = tmp
								copy_VarAtts(tmp,tmpV)
							else
								sz	:= dimsizes(tmpU)
								tmp	:= new((/1,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,"U",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								tmpU	:= array_append_record(tmpU,tmp,0)
								
								sz	:= dimsizes(tmpV)
								tmp	:= new((/1,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,"V",tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								tmpV	:= array_append_record(tmpV,tmp,0)
							end if
						end if
					end do
				end if  end if
				ISZ	= dimsizes(tmpU(0,0,:))
				JSZ	= dimsizes(tmpU(0,:,0))
				KSZ	= dimsizes(tmpU(:,0,0))
				
				; No missing values allowed to remove the vortex
				; Fill them in using 'linmsg_n'
				if(rmVortex)then
					if(num(ismissing(tmpU)).gt.0 .or. num(ismissing(tmpV)).gt.0)then
						tmpU = linmsg_n(tmpU,-1,1)
						tmpV = linmsg_n(tmpV,-1,1)
						;rmVortex = False
					end if
				end if
				
				; Flip the longitude dimension, if flipFlag=True
				if(flipFlag)then	;tmpU	= lonFlip(tmpU)
							;tmpV	= lonFlip(tmpV)
							tmpU	:= lonPivot(tmpU,PIV)
							tmpV	:= lonPivot(tmpV,PIV)
							xAdd	= -360.
				else			xAdd	= 0.
				end if
				
				; Remove the TC vortex, if rmVortex=True
				if(rmVortex)then	tmp	:= tmpU
							HBFILTER::hbfilter(tmp,tmpU,N,ISZ,JSZ,KSZ)
							tmp	:= tmpV
							HBFILTER::hbfilter(tmp,tmpV,N,ISZ,JSZ,KSZ)
				end if
				;print(max(tmp)+"  "+max(tmpU))
				tmpU@_FillValue = fval3
				tmpV@_FillValue = fval3
				
				; Calculate the wind amplitude from 'tmpU' and 'tmpV'
				V	= (/sqrt(tmpU^2.+tmpV^2.)/)
				if(isvar("units"))then	V@units = units
				else			V@units = "m s-1"
				end if
				delete([/tmpU,tmpV/])
			else
				if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
					V	= f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$(ind(f->$nDims@dNames(levIND)$.ge.(mf*lev1) .and. \
														 f->$nDims@dNames(levIND)$.le.(mf*lev2) .and. \
														 toint(f->$nDims@dNames(levIND)$)%toint(mf*NP).eq.0) ,\
													     {iBds(0):iBds(1)},:)
				else if(latDim .and. lonDim .and. timeDim .and. .not.levDim)then
					levChk	= fspan(lev1,lev2,toint(abs(lev1-lev2)/25.)+1)
					do kkk = 0,dimsizes(levChk)-1
						if(isfilevar(f,findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))))then
							if(.not.isvar("V"))then
								tmp	:= f->$findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								sz	:= dimsizes(tmp)
								V	= new((/1,sz(0),sz(1)/),"float")
								V(0,:,:) = tmp
								copy_VarAtts(tmp,V)
							else
								sz	:= dimsizes(V)
								tmp	:= new((/1,sz(1),sz(2)/),"float")
								tmp(0,:,:) = f->$findVarName(dSource,varInfo(0),tostring(toint(levChk(kkk))))$(tIND,{iBds(0):iBds(1)},:)
								V	:= array_append_record(V,tmp,0)
							end if
						end if
					end do
				end if  end if
				ISZ	= dimsizes(V(0,0,:))
				JSZ	= dimsizes(V(0,:,0))
				KSZ	= dimsizes(V(:,0,0))
				
				; No missing values allowed to remove the vortex
				; Fill them in using 'linmsg_n'
				if(rmVortex)then
					if(num(ismissing(V)).gt.0)then
						V = linmsg_n(V,-1,1)
						;rmVortex = False
					end if
				end if
				
				; Flip the longitude dimension, if flipFlag=True
				if(flipFlag)then	;V	= lonFlip(V)
							V	:= lonPivot(V,PIV)
							xAdd	= -360.
				else			xAdd	= 0.
				end if
				
				; Remove the TC vortex, if rmVortex=True
				if(rmVortex)then	tmp	:= V
							HBFILTER::hbfilter(tmp,V,N,ISZ,JSZ,KSZ)
				end if

				; Copy attribute "units" to the NCL variable
				if(isfilevaratt(f,findVarName(dSource,varInfo(0),tostring(toint(lev1))),"units"))then
					V@units = f->$findVarName(dSource,varInfo(0),tostring(toint(lev1)))$@units
				end if
			end if
			
			; Get the vertical levels
			allLevs	= f->$nDims@dNames(0)$(ind(f->$nDims@dNames(levIND)$.ge.(mf*lev1) .and. \
							   f->$nDims@dNames(levIND)$.le.(mf*lev2) .and. \
							   toint(f->$nDims@dNames(levIND)$)%toint(mf*NP).eq.0))
			nLevs	= dimsizes(allLevs)
			
			; Define the 3D variable
			V3d	= V
				V3d!0 = "lev"
				V3d!1 = "lat"
				V3d!2 = "lon"
				V3d&lev	= f->$nDims@dNames(0)$(ind(f->$nDims@dNames(levIND)$.ge.(mf*lev1) .and. \
								   f->$nDims@dNames(levIND)$.le.(mf*lev2) .and. \
								   toint(f->$nDims@dNames(levIND)$)%toint(mf*NP).eq.0))
				V3d&lat = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lat")))$({iBds(0):iBds(1)})
				V3d&lon = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lon")))$
			
			; Redefine V to become the 2D mass-weighted average
			V	:= V3d(0,:,:)
			V	= 0.
			MWAVG::mwavg(V3d,V,allLevs,max(allLevs),ISZ,JSZ,nLevs)
			
			; If any values were missing at any levels, set those locations to missing in the 2D output
			V	= where(dim_avg_n_Wrap(tofloat(where(ismissing(V3d),1.,0.)),0).gt.0.,V@_FillValue,V)
		else
			print("ERROR: getVar2d: Unreadable Level Flag.")
			exit
		end if
		end if
		end if
		delete([/lev1,lev2,levFlag/])
	
	; Shorter string means that only 1 level is requested.
	; This simplifies things greatly.
	else
		if(varInfo(0).eq."UV")then
			if(latDim .and. lonDim .and. .not.levDim .and. .not.timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$({iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$({iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$({mf*tofloat(varInfo(1))},{iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$({mf*tofloat(varInfo(1))},{iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. .not.levDim .and. timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$(tIND,{iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$(tIND,{iBds(0):iBds(1)},:)
			end if  end if	end if
			units	= tmpU@units
			ISZ	= dimsizes(tmpU(0,:))
			JSZ	= dimsizes(tmpU(:,0))
				
			if(flipFlag)then	;tmpU	= lonFlip(tmpU)
						;tmpV	= lonFlip(tmpV)
						tmpU	:= lonPivot(tmpU,PIV)
						tmpV	:= lonPivot(tmpV,PIV)
						xAdd	= -360.
			else			xAdd	= 0.
			end if
			if(rmVortex)then	tmp	:= new((/1,JSZ,ISZ/),"float")
						tmp(0,:,:) = tmpU
						tmpU	:= tmp
						HBFILTER::hbfilter(tmp,tmpU,N,ISZ,JSZ,toint(1))
						tmpU	:= tmpU(0,:,:)
							
						tmp(0,:,:) = tmpV
						tmpV	:= tmp
						HBFILTER::hbfilter(tmp,tmpV,N,ISZ,JSZ,toint(1))
						tmpV	:= tmpV(0,:,:)
			end if
			V	= (/sqrt(tmpU^2.+tmpV^2.)/)
			if(isvar("units"))then	V@units = units
			else			V@units = "m s-1"
			end if
			delete([/tmpU,tmpV/])
		else if(any((/"RVO","AVO"/).eq.varInfo(0)) .and. .not.isfilevar(f,findVarName(dSource,"AVO",varInfo(1))))then
			if(latDim .and. lonDim .and. .not.levDim .and. .not.timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$({iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$({iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$({mf*tofloat(varInfo(1))},{iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$({mf*tofloat(varInfo(1))},{iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. .not.levDim .and. timeDim)then
				tmpU	= f->$findVarName(dSource,"U",varInfo(1))$(tIND,{iBds(0):iBds(1)},:)
				tmpV	= f->$findVarName(dSource,"V",varInfo(1))$(tIND,{iBds(0):iBds(1)},:)
			end if  end if	end if
			ISZ	= dimsizes(tmpU(0,:))
			JSZ	= dimsizes(tmpU(:,0))
				
			if(flipFlag)then	;tmpU	= lonFlip(tmpU)
						;tmpV	= lonFlip(tmpV)
						tmpU	:= lonPivot(tmpU,PIV)
						tmpV	:= lonPivot(tmpV,PIV)
						xAdd	= -360.
			else			xAdd	= 0.
			end if
			if(rmVortex)then	tmp	:= new((/1,JSZ,ISZ/),"float")
						tmp(0,:,:) = tmpU
						tmpU	:= tmp
						HBFILTER::hbfilter(tmp,tmpU,N,ISZ,JSZ,toint(1))
						tmpU	:= tmpU(0,:,:)
							
						tmp(0,:,:) = tmpV
						tmpV	:= tmp
						HBFILTER::hbfilter(tmp,tmpV,N,ISZ,JSZ,toint(1))
						tmpV	:= tmpV(0,:,:)
			end if
			
			tmpU!0 = "lat"
			tmpU!1 = "lon"
			tmpU&lat = f->$nDims@dNames(latIND)$({iBds(0):iBds(1)})
			tmpU&lon = f->$nDims@dNames(lonIND)$
			tmpV!0 = "lat"
			tmpV!1 = "lon"
			tmpV&lat = f->$nDims@dNames(latIND)$({iBds(0):iBds(1)})
			tmpV&lon = f->$nDims@dNames(lonIND)$
			V	= uv2vr_cfd(tmpU,tmpV,f->$nDims@dNames(latIND)$({iBds(0):iBds(1)}),f->$nDims@dNames(lonIND)$,2)
			V@units = "s-1"
			delete([/tmpU,tmpV/])
		else
			if(latDim .and. lonDim .and. .not.levDim .and. .not.timeDim)then
				V = f->$findVarName(dSource,varInfo(0),varInfo(1))$({iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. levDim .and. .not.timeDim)then
				V = f->$findVarName(dSource,varInfo(0),varInfo(1))$({mf*tofloat(varInfo(1))},{iBds(0):iBds(1)},:)
			else if(latDim .and. lonDim .and. .not.levDim .and. timeDim)then
				V = f->$findVarName(dSource,varInfo(0),varInfo(1))$(tIND,{iBds(0):iBds(1)},:)	
			end if  end if  end if
			ISZ = dimsizes(V(0,:))
			JSZ = dimsizes(V(:,0))
			
			if(flipFlag)then	;V	= lonFlip(V)
						V	:= lonPivot(V,PIV)
						xAdd	= -360.
			else			xAdd	= 0.
			end if
			if(rmVortex)then	tmp	:= new((/1,JSZ,ISZ/),"float")
						tmp(0,:,:) = V
						V	:= tmp
						HBFILTER::hbfilter(tmp,V,N,ISZ,JSZ,toint(1))
						V	:= V(0,:,:)
			end if
			if(isfilevaratt(f,findVarName(dSource,varInfo(0),varInfo(1)),"units"))then
				V@units = f->$findVarName(dSource,varInfo(0),varInfo(1))$@units
			end if
		end if  end if
	end if

	
	; Update Metadata
	lonsz	= dimsizes(lon)
	V!0 = "lat"
	V!1 = "lon"
	V&lat = f->$nDims@dNames(latIND)$({iBds(0):iBds(1)})
	if(lonsz.eq.1 .and. lon(0).eq.0)then
		V&lon = f->$nDims@dNames(lonIND)$
	else if(lonsz.gt.1)then
		V&lon = lon
		V	:= V(:,{iBds(2):iBds(3)})
		V&lon	:= lon({iBds(2):iBds(3)})
	end if  end if


	; Correct units
	if(.not.isatt(V,"units"))then   V@units = ""   end if
	if(varInfo(0).eq."RVO" .and. V@units.eq."s-1")then
		V	= 100000.*(V - conform(V,2.*omega*sin(d2r*V&lat),0))
		V@units	= "10^-5 s-1"
	else if(any((/"UV","U","V"/).eq.varInfo(0)) .and. any((/"m s-1","m/s"/).eq.V@units))then
		V	= ms2kts*V
		V@units	= "kts"
	else if(varInfo(0).eq."HGT" .and. any((/"m","gpm"/).eq.V@units))then
		V	= 0.1*V
		V@units	= "dam"
	else if(any((/"PRCP","PRATE"/).eq.varInfo(0)) .and. V@units.eq."kg m-2 s-1")then
		V	= V*3600.
		V@units	= "mm h-1"
	else if(varInfo(0).eq."TPRCP" .and. V@units.eq."kg m-2")then
		V	= V/21.2
		V@units	= "in"
	else if(varInfo(0).eq."MSLP" .and. V@units.eq."Pa")then
		V	= 0.01*V
		V@units	= "hPa"
	end if  end if  end if  end if  end if  end if
	
	
	; Add information about lonFlip and rmVortex
	if(flipFlag)then    V@flag1 = "longitude flipped"   end if
	if(rmVortex)then    V@flag2 = "vortex removed"      end if
	
	
	return(V)
	
end



;
;================================================================
; getVar3d

undef("getVar3d")
function getVar3d (f:file,dSource[1]:string,varInfo[2]:string,nDims[1]:integer,iBds[*]:float,mf[1]:float,flags[*]:logical,lon[*]:numeric)
local tmp, lev1, lev2, tmpU, tmpV, V, PIV
begin
	
	do fff = 0,dimsizes(flags)-1
		if(.not.isvar("flipFlag"))then
			if(fff.eq.0)then	flipFlag = flags(0)
			else			flipFlag = False
			end if
		else if(.not.isvar("rmVortex"))then
			if(fff.eq.1)then	rmVortex = flags(1)
			else			rmVortex = False
			end if
		end if
		end if
	end do
	if(.not.isvar("rmVortex"))then rmVortex = False end if
	if(.not.isvar("flipFlag"))then flipFlag = False end if
	if(rmVortex)then	if(.not.isatt(flags,"icen"))then	print("ERROR: getVar3d: The attribute 'icen' is necessary to remove vortex")
									exit
				end if
				if(.not.isatt(flags,"jcen"))then	print("ERROR: getVar3d: The attribute 'jcen' is necessary to remove vortex")
									exit
				end if
				if(.not.isatt(flags,"radius"))then	print("ERROR: getVar3d: The attribute 'radius' is necessary to remove vortex")
									exit
				end if
				if(dSource.eq."H3HW")then		N = 51
				else if(dSource.eq."H214")then		N = 35
				else if(dSource.eq."HWRF_Ens")then	N = 35
				else if(dSource.eq."GFS_an")then	N = 15
				else					N = 11
				end if
				end if
				end if
				end if
				;N = 15
	end if
	
	
	; Get the pivot longitude, if applicable
	if(isatt(iBds,"PivotLon"))then	PIV = iBds@PivotLon
	else				PIV = 180.
	end if


	if(strlen(varInfo(1)).ne.8)then
		print("ERROR: getVar3d: Must define two levels")
		exit
	else
		if(nDims.le.2)then
			print("ERROR: getVar3d: No Vertical Dimension.")
			exit
		end if
		
		tmp	= tochar(varInfo(1))
		lev1	= tofloat(tostring(tmp(:3)))
		lev2	= tofloat(tostring(tmp(4:7)))
		delete(tmp)

			
		if(varInfo(0).eq."UV")then			
			tmpU	= f->$findVarName(dSource,"U",tostring(lev1))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
			tmpV	= f->$findVarName(dSource,"V",tostring(lev1))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
				
			if(flipFlag)then	;tmpU	= lonFlip(tmpU)
						;tmpV	= lonFlip(tmpV)
						tmpU	:= lonPivot(tmpU,PIV)
						tmpV	:= lonPivot(tmpV,PIV)
						xAdd	= -360.
			else			xAdd	= 0.
			end if
			if(rmVortex)then	tmp := tmpU
						HBFILTER::hbfilter(tmp,tmpU,N,ISZ,JSZ,toint(1))
						
						tmp := tmpV
						HBFILTER::hbfilter(tmp,tmpV,N,ISZ,JSZ,toint(1))
			end if
			;print("Vortex removal COMPLETE")
			;tmpU	:= (/tmpU/)
			;tmpV	:= (/tmpV/)
			
			V	= (/sqrt(tmpU^2.+tmpV^2.)/)
			V@units = "m s-1"
			delete([/tmpU,tmpV/])
		else
			V	= f->$findVarName(dSource,varInfo(0),varInfo(1))$({mf*lev1:mf*lev2},{iBds(0):iBds(1)},:)
			
			if(flipFlag)then	;V	= lonFlip(V)
						V	:= lonPivot(V,PIV)
						xAdd	= -360.
			else			xAdd	= 0.
			end if
			if(rmVortex)then	tmp := V
						HBFILTER::hbfilter(tmp,V,N,ISZ,JSZ,toint(1))
			end if
			if(isfilevaratt(f,findVarName(dSource,varInfo(0),varInfo(1)),"units"))then
				V@units = f->$findVarName(dSource,varInfo(0),varInfo(1))$@units
			end if
		end if
	end if
	

	; Update Metadata
        lonsz   = dimsizes(lon)
        V!0 = "lev"
        V!1 = "lat"
        V!2 = "lon"
	V&lev = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"ISBL")))$({mf*lev1:mf*lev2})
        V&lat = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lat")))$({iBds(0):iBds(1)})
        if(lonsz.eq.1 .and. lon(0).eq.0)then
                V&lon = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lon")))$
        else if(lonsz.gt.1)then
                V&lon = lon
                V       := V(:,:,{iBds(2):iBds(3)})
                V&lon   := lon({iBds(2):iBds(3)})
        end if
        end if


	; Correct units
	if(.not.isatt(V,"units"))then   V@units = ""   end if
	if(varInfo(0).eq."RVO" .and. V@units.eq."s-1")then
		V	= 100000.*(V - conform(V,2.*omega*sin(d2r*V&lat),1))
		V@units	= "10^-5 s-1"
	end if
	if(any((/"UV","U","V"/).eq.varInfo(0)) .and. V@units.eq."m s-1")then
		V	= ms2kts*V
		V@units	= "kts"
	end if
	if(varInfo(0).eq."HGT" .and. any((/"m","gpm"/).eq.V@units))then
		V	= 0.1*V
		V@units	= "dam"
	end if
	if(varInfo(0).eq."TPRCP" .and. V@units.eq."kg m-2")then
		V	= V/21.2
		V@units	= "in"
	end if
	if(varInfo(0).eq."MSLP" .and. V@units.eq."Pa")then
		V	= 0.01*V
		V@units	= "hPa"
	end if
	
	
	return(V)
	
end



;
;================================================================
; getVarXC
;
; Use MSLP to find the storm center
; User must define N-S or E-W cross-section
; Upper and lower pressure bounds are specified by a string -- varInfo(1)

function getVarXC (f:file,dSource[1]:string,varInfo[2]:string,nDims[1]:integer,iBds[*]:float,mf[1]:float,flags[*]:logical,lon[*]:numeric)
local V,flipFlag, rmVortex, PIV
begin


        do fff = 0,dimsizes(flags)-1
                if(.not.isvar("flipFlag"))then
                        if(fff.eq.0)then        flipFlag = flags(0)
                        else                    flipFlag = False
                        end if
                else if(.not.isvar("rmVortex"))then
                        if(fff.eq.1)then        rmVortex = flags(1)
                                                if(.not.isatt(flags,"icen"))then        print("ERROR: getVarXC: The attribute 'icen' is necessary to remove vortex")
                                                                                        exit
                                                end if
                                                if(.not.isatt(flags,"jcen"))then        print("ERROR: getVarXC: The attribute 'jcen' is necessary to remove vortex")
                                                                                        exit
                                                end if
                                                if(.not.isatt(flags,"radius"))then      print("ERROR: getVarXC: The attribute 'radius' is necessary to remove vortex")
                                                                                        exit
                                                end if
                        else                    rmVortex = False
                        end if
                end if
                end if
        end do
        if(.not.isvar("rmVortex"))then rmVortex = False end if
        if(.not.isvar("flipFlag"))then flipFlag = False end if

	if(nDims.eq.2)then
		print("ERROR: getVarXC: No Vertical Dimension.")
		exit
	end if
	if(strlen(varInfo(1)).ne.9)then
		print("ERROR: getVarXC: Level Info must have a length of 9: "+varInfo(1))
		exit
	end if
	
	; Get the pivot longitude, if applicable
	if(isatt(iBds,"PivotLon"))then	PIV = iBds@PivotLon
	else				PIV = 180.
	end if

	tmp     = tochar(varInfo(1))
	lev1    = mf*tofloat(tostring(tmp(:3)))
	lev2    = mf*tofloat(tostring(tmp(4:7)))
	levFlag = tostring(tmp(8))
	delete(tmp)

	if(levFlag.eq."x")then
                if(varInfo(0).eq."UV")then
                        if(flipFlag)then
                                ;tmpU    = ms2kts*(/lonFlip(f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:))/)
                                ;tmpV    = ms2kts*(/lonFlip(f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:))/)
				tmpU    = ms2kts*(/lonPivot(f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:),PIV)/)
                                tmpV    = ms2kts*(/lonPivot(f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:),PIV)/)
                        else
                                tmpU    = ms2kts*(/f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:)/)
                                tmpV    = ms2kts*(/f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:)/)
                        end if
                        V       = sqrt(tmpU^2.+tmpV^2.)
                        V@units = "kts"
                        delete([/tmpU,tmpV/])

                else
                        if(flipFlag)then
                                ;V       = (/lonFlip(f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:))/)
				V       = (/lonPivot(f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:),PIV)/)
                        else
                                V       = (/f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{avg((/iBds(0),iBds(1)/))},:)/)
                        end if
                        if(isfilevaratt(f,findVarName(dSource,varInfo(0),tostring(lev1)),"units"))then
                                V@units = f->$findVarName(dSource,varInfo(0),tostring(lev1))$@units
                        end if
                end if

	else if(levFlag.eq."y")then
                if(varInfo(0).eq."UV")then
                        if(flipFlag)then
                                ;tmpU    = ms2kts*(/lonFlip(f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:))/)
                                ;tmpV    = ms2kts*(/lonFlip(f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:))/)
				tmpU    = ms2kts*(/lonPivot(f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:),PIV)/)
                                tmpV    = ms2kts*(/lonPivot(f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:),PIV)/)
                        else
                                tmpU    = ms2kts*(/f->$findVarName(dSource,"U",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:)/)
                                tmpV    = ms2kts*(/f->$findVarName(dSource,"V",tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:)/)
                        end if
                        V       = sqrt(tmpU^2.+tmpV^2.)
                        V@units = "kts"
                        delete([/tmpU,tmpV/])

                else
                        if(flipFlag)then
                                ;V       = (/lonFlip(f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:))/)
				V       = (/lonPivot(f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:),PIV)/)
                        else
                                V       = (/f->$findVarName(dSource,varInfo(0),tostring(lev1))$({lev1:lev2},{iBds(0):iBds(1)},:)/)
                        end if
                        if(isfilevaratt(f,findVarName(dSource,varInfo(0),tostring(lev1)),"units"))then
                                V@units = f->$findVarName(dSource,varInfo(0),tostring(lev1))$@units
                        end if
                end if

	end if
	end if

        ; Update Metadata
	lonsz   = dimsizes(lon)
        if(levFlag.eq."x")then
		V!0 = "lev"
		V!1 = "lon"
		V&lev = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"ISBL")))$({lev1:lev2})
		if(lonsz.eq.1 .and. lon(0).eq.0)then
			V&lon = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lon")))$
		else if(lonsz.gt.1)then
			V&lon	= lon
			V       := V(:,{iBds(2):iBds(3)})
			V&lon   := lon({iBds(2):iBds(3)})
		end if
		end if

	else if(levFlag.eq."y")then
		V!0 = "lev"
	        V!1 = "lat"
	        V!2 = "lon"
	        V&lev = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"ISBL")))$({lev1:lev2})
	        V&lat = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lat")))$({iBds(0):iBds(1)})
	        if(lonsz.eq.1 .and. lon(0).eq.0)then
	                V&lon = f->$nDims@dNames(ind(isStrSubset2(nDims@dNames,"lon")))$
	        else if(lonsz.gt.1)then
	                V&lon = lon
	        end if
	        end if
		V	:= dim_avg_n_Wrap(V(:,:,{iBds(2):iBds(3)}),2)

	end if
	end if


	; Correct units
	if(.not.isatt(V,"units"))then   V@units = ""   end if
	if(varInfo(0).eq."RVO" .and. V@units.eq."s-1")then
		if(levFlag.eq."x")then
			V	= 100000.*(V - (2.*omega*sin(d2r*avg((/iBds(0),iBds(1)/)))))
		else
			V	= 100000.*(V - conform(V,2.*omega*sin(d2r*V&lat),0))
		end if
		V@units	= "10^-5 s-1"
	end if
	if(any((/"UV","U","V"/).eq.varInfo(0)) .and. V@units.eq."m s-1")then
		V	= ms2kts*V
		V@units	= "kts"
	end if
	if(varInfo(0).eq."HGT" .and. any((/"m","gpm"/).eq.V@units))then
		V	= 0.1*V
		V@units	= "dam"
	end if
	if(varInfo(0).eq."TPRCP" .and. V@units.eq."kg m-2")then
		V	= V/21.2
		V@units	= "in"
	end if
	if(varInfo(0).eq."MSLP" .and. V@units.eq."Pa")then
		V	= 0.01*V
		V@units	= "hPa"
	end if


	return(V)

end

;
;================================================================
; nml_read
;
; extract certain variable from the GPLOT master input namelist

function nml_read (nml:string,var[1]:string)
local str,var1,var2,var3
begin

	str  = var+" ="
	var1 = nml(ind(isStrSubset2(nml,str)))
	var2 = str_squeeze(str_split(var1,"="))
	if (dimsizes(var2).eq.2) then var3=str_squeeze(var2(1))
	else var3=""
	end if
	return(var3)
end

;
;================================================================
; get_uniq_ind
;
; e.g. list=(/x,x,a,a,b/) --> get_uniq_ind --> 0,2,4
;

function get_uniq_ind (lst)
begin
	strlist=tostring(lst)
	index=new(100,integer) ; list max number is 100
	dd=dimsizes(strlist)
	index(0)=0
	do ddd=1,dd-1
		previous_values:=strlist(0:ddd-1)
		if (ismissing(str_match(previous_values,strlist(ddd)))) then
		index(ddd)=ddd
		end if
	end do
	index:=index(ind(.not.ismissing(index)))
	return(index)
end



