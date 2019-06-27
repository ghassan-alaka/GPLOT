;================================================================
; GPLOT_util.ncl
;
; Lower-level functions and procedures for GPLOT.ncl are stored
; in this file to stay organized. IMPORTANT -- No function should
; be added here that depends on another custom function. Higher-
; level functions and procedures are located in GPLOT_func.ncl,
; and may call functions from this file.
; Functions are given in alphabetical order.
;
; Written By:     Ghassan Alaka, Jr.
;
; Created:        September 2, 2015
; Last Modified:  August 28, 2017
;
; Last MK Modified: 13 Jun 2018 (getModelInfo and getExptInfo)
;
; FUNCTION LIST
; 1)  calcTheta
; 2)  changeTimeFmt
; 3)  chkCmdInputs
; 4)  filter121
; 5)  defineCMAP_fill
; 6)  defineCMAP_name
; 7)  defineLevels
; 8)  findVarName
; 9)  GenPlotRes
; 10) getDmnBds
; 11) getFileTag
; 12) getLatLonLbl
; 13) getPlotTitle
; 14) getStmThin
; 14) hbfilter
; 15) isStrSubset2
; 16) level_convert
; 17) printMaxMin
; 18) remove_duplicates
; 19) testPlot_2d_map
; 20) testPlot_map
; 21) testPlot_contour
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

;MKedit
GPLOT=systemfunc("echo $GPLOT_DIR")
;MKedit end

;================================================================
; Universal Constants
;================================================================

;g               = 9.80665
;pi              = 3.14159265359
;d2r             = pi/180.
;r               = 6371000.
;omega           = 0.0000727
;ms2kts          = 1.94384449
;fval            = 9.96921e+36
;fval2           = -2147483647
;fval3           = 1.0e+20



;================================================================
; Universal Constants
;================================================================
undef("load_constants")
function load_constants ()
local constant
begin
	constant	:= "Constants are set"
	constant@g	= tofloat(9.80665)			; Gravitational Acceleration [m s-2]
	constant@G	= tofloat(6.67e-11)			; Universal Gravitational Constant [N m-2 kg-2]
	constant@pi	= tofloat(acos(-1.0))			; Pi [n/a]
	constant@c	= tofloat(2.998e+08)			; Speed of Light [m s-1]
	constant@R	= tofloat(287.058)			; Specific Gas Constant for Dry Air [J K-1 kg-1]
	constant@c_p	= tofloat(1004.)			; Specific Heat at Constant Pressure [J K-1 kg-1]
	constant@c_v	= tofloat(717.)				; Specific Heat at Constant Volume [J K-1 kg-1]
	constant@dalr	= tofloat(constant@g/constant@c_p)	; Dry Adiabatic Lapse Rate [K m-1]
	constant@d2r	= tofloat(constant@pi/180)		; Degrees-to-Radians Conversion [rad deg-1]
	constant@r2d	= tofloat(180./constant@pi)		; Radians-to-Degrees Conversion [deg rad-1]
	constant@r	= tofloat(6.371e+06)			; Earth Radius [m]
	constant@omega	= tofloat(7.27e-05)			; Earth Angular Speed [rad s-1]
	constant@kappa	= tofloat(constant@R/constant@c_p)	; Poisson Constant [n/a]
	constant@sigma	= tofloat(5.67e-08)			; Stefan-Boltzmann Constant [J s-1 m-2 K-4]
	constant@rho_0	= tofloat(1.25)				; Air Density at Sea Level [kg m-3]
	constant@ms2kts	= tofloat(1.94384449)			; Meters-Per-Second-to-Knots Conversion [m s-1 kt-1]
	constant@fval	= tofloat(9.96921e+36)			; Missing Value for Float #1 [n/a]
	constant@fval2	= toint(-2147483647)			; Missing Value for Integer [n/a]
	constant@fval3	= tofloat(1.0e+20)			; Missing Value for Float #2 [n/a]
	constant@fval4	= tostring("missing")			; Missing value for String [n/a]
	constant@tunits = "hours since 1970-01-01 00:00:00"	; Solid baseline date used to convert/compare dates.
	
	return(constant)
end



;
;================================================================
; allMasters
;
; Store master lists in this function for easy access.
; Users should edit this function to build their own master lists.

undef("allMasters")
function allMasters (TYPE[1]:string)
local MASTER_OUT
begin

	ALL_MASTER_TYPE	= (/"basin"/)
	
	if(.not.any(ALL_MASTER_TYPE.eq.TYPE))then
		print("ERROR: allMasters: Master list type '"+TYPE+"' not recognized.")
		exit
	end if
	
	; These are hurricane basins
	if(TYPE.eq."basin")then		MASTER_OUT	= (/(/"Atlantic","AL","L"/),\
							    (/"East Pacific","EP","E"/),\
							    (/"West Pacific","WP","W"/)\
							  /)
	end if
	
	return(MASTER_OUT)

end



;
;================================================================
; arrow
;
; Draw an arrow on a plot using gsn_add_polyline
; draws an arrow on a plot. calling syntax is same as in gsn_add_polyline()
; Arindam Chakraborty <arch at caos.iisc.ernet.in>, May'10.

undef("arrow")
function arrow(wks,plt,xpts,ypts,res)
begin

	getvalues plt
		"trXMinF": x0
		"trXMaxF": x1
		"trYMinF": y0
		"trYMaxF": y1
	end getvalues

	plt@arr = gsn_add_polyline(wks,plt,xpts,ypts,res)

	; determines how broad should the arrow be
	asp = 0.4

	xp1 = xpts(0)
	xp2 = xpts(1)
	yp1 = ypts(0)
	yp2 = ypts(1)

	xp1n = (xp1 - x0)/(x1-x0)
	xp2n = (xp2 - x0)/(x1-x0)
	yp1n = (yp1 - y0)/(y1-y0)
	yp2n = (yp2 - y0)/(y1-y0)

	pi = acos(-1.0)
	rad2deg = 180.0/pi

	theta = atan2((yp2n-yp1n),(xp2n-xp1n))

	if(theta.lt.0 .and. yp2.gt.yp1)then
		theta = pi + theta
	else
		if(theta.gt.0 .and. yp2.lt.yp1)then   theta = pi + theta   end if
	end if

	; length of the line of arrowhead as a fraction of the length of the given line
	fhead = 0.3
	ahead = sqrt((xp2n-xp1n)*(xp2n-xp1n) + (yp2n-yp1n)*(yp2n-yp1n))*fhead
	phi = atan(asp)

	resp = True

	lambda1 = theta + phi
	xa1n = xp2n - ahead*cos(lambda1)
	ya1n = yp2n - ahead*sin(lambda1)
	xa1 = x0 + xa1n*(x1-x0)
	ya1 = y0 + ya1n*(y1-y0)

	xpts1 = (/xa1,xp2/)
	ypts1 = (/ya1,yp2/)
	plt@arr1 = gsn_add_polyline(wks,plt,xpts1,ypts1,res)

	lambda2 = theta - phi
	xa2n = xp2n - ahead*cos(lambda2)
	ya2n = yp2n - ahead*sin(lambda2)
	xa2 = x0 + xa2n*(x1-x0)
	ya2 = y0 + ya2n*(y1-y0)
	xpts2 = (/xa2,xp2/)
	ypts2 = (/ya2,yp2/)
	plt@arr2 = gsn_add_polyline(wks,plt,xpts2,ypts2,res)

	return (plt)
	
end



;
;================================================================
; basinCodes
;
; Get the basin code 

undef("basinCodes")
function basinCodes (IN:string,TYPE[1]:string)
local MASTER_BASIN, MASTER_TYPE, IN_ch, NOUT, NIN1, NIN2, OUT
begin

	; These are the recognized types
	MASTER_TYPE	= (/"long_name","XX","X"/)
	
	; Find the output type and check that it is acceptable
	if(.not.any(MASTER_TYPE.eq.TYPE))then
		print("ERROR: basinCodes: Type '"+TYPE+"' not recognized.")
		print("ERROR: basinCodes: Recognized types are: long_name, XX, X")
		exit
	end if
	NOUT		= ind(MASTER_TYPE.eq.TYPE)
	MASTER_BASIN	= allMasters("basin")
	;MASTER_BASIN	= (/(/"Atlantic","AL","L"/),\
	;		    (/"East Pacific","EP","E"/),\
	;		    (/"West Pacific","WP","W"/)\
	;		  /)
	
	; Find out the input type
	IN_ch = tochar(IN)
	if(dimsizes(IN_ch).eq.1)then		NIN1 = 2
	else if(dimsizes(IN_ch).eq.2)then	NIN1 = 1
	else if(dimsizes(IN_ch).ge.3)then	NIN1 = 0
	end if  end if  end if

	; Find the basin and check that something is recognized
	if(.not.any(MASTER_BASIN(:,NIN1).eq.IN))then
		print("ERROR: basinCodes: Input string '"+IN+"' not recognized.")
		print("ERROR: basinCodes: Recognized basins are: Atlantic, East Pacific, and West Pacific")
		exit
	end if
	NIN2	= ind(MASTER_BASIN(:,NIN1).eq.IN)
	
	; Get the output basin string
	OUT = MASTER_BASIN(NIN2,NOUT)
	return(OUT)

end




;
;================================================================
; calcTheta
;
; Calculate the potential temperature

undef("calcTheta")
function calcTheta (T:numeric,P[*]:numeric,Ps[1]:numeric)
local szT, szP, nDT, nDP, TH
begin
	C	= load_constants()

	szT	= dimsizes(T)
	nDT	= dimsizes(szT)
	szP	= dimsizes(P)
	if(szT(0).ne.szP)then	print("ERROR: calcTheta: Vertical dimension does not match.")
				exit
	end if
	if(nDT.gt.1 .or. szT(0).gt.1)then
		P2 = conform(T,P,0)
	else	P2 = P
	end if

	TH	= T*(Ps/P2)^C@kappa
	
	return(TH)

end ; calcTheta



;
;================================================================
; changeTimeFmt

undef("changeTimeFmt")
function changeTimeFmt (oldStr[*]:string,oldFormat[1]:string,newFormat[1]:string,fHr[*]:float)
local newStr, tmp, MM, DD, YYYY, HH, mm, ss, masterList, DOW, DOWStr, MMStr, time_hours,aaa,fHr2,nStr
begin

        masterList      = (/"MM/DD/YYYY (HH:mm)",   "YYYYMMDDHH",\
                            "YYYYMMDD_HH",          "YYYYMMDD",\
                            "YYYYMMDD_HHmmss",      "YYYY-MM-DD_HH",\
                            "HHz Month DD YYYY",    "HHz DOW, Month DD YYYY",\
			    "DD/HH"/)

	; Check for failures
        if(.not.any(masterList.eq.oldFormat))then
                print("ERROR: changeTimeFmt: 'oldFormat' not supported by 'changeTimeFmt' !")
                print("ERROR: changeTimeFmt: oldFormat = "+oldFormat)
                exit
        end if
        if(.not.any(masterList.eq.newFormat))then
                print("ERROR: changeTimeFmt: 'newFormat' not supported by 'changeTimeFmt' !")
                print("ERROR: changeTimeFmt: newFormat = "+newFormat)
                exit
        end if
	if(dimsizes(oldStr).ne.dimsizes(fHr) .and. dimsizes(fHr).ne.1)then
		print("ERROR: changeTimeFmt: 'fHr' length must match 'oldStr' length")
                exit
	end if
	
	
	if(oldFormat.ne.newFormat)then

	; Setup a few
	nStr	= dimsizes(oldStr)
	newStr	= new(nStr,"string","missing")
	if(dimsizes(fHr).eq.1 .and. \
	   dimsizes(oldStr).ne.1)then	fHr2 = new((/nStr/),"float",default_fillvalue("float"))
					fHr2 = fHr
	else				fHr2 = fHr
	end if
	do aaa = 0,nStr-1

		 ; 1) Disect the old string
		tmp     := tochar(oldStr(aaa))
                if(oldFormat.eq."MM/DD/YYYY (HH:mm)")then
                        MM      = tostring(tmp(0:1))
                        DD      = tostring(tmp(3:4))
                        YYYY    = tostring(tmp(6:9))
                        HH      = tostring(tmp(12:13))
                        mm      = tostring(tmp(15:16))
                        ss      = "00"
                else if(oldFormat.eq."YYYYMMDDHH")then
                        MM      = tostring(tmp(4:5))
                        DD      = tostring(tmp(6:7))
                        YYYY    = tostring(tmp(0:3))
                        HH      = tostring(tmp(8:9))
                        mm      = "00"
                        ss      = "00"
                else if(oldFormat.eq."YYYYMMDD_HH")then
                        MM      = tostring(tmp(4:5))
                        DD      = tostring(tmp(6:7))
                        YYYY    = tostring(tmp(0:3))
                        HH      = tostring(tmp(9:10))
                        mm      = "00"
                        ss      = "00"
                else if(oldFormat.eq."YYYYMMDD")then
                        MM      = tostring(tmp(4:5))
                        DD      = tostring(tmp(6:7))
                        YYYY    = tostring(tmp(0:3))
                        HH      = "00"
                        mm      = "00"
                        ss      = "00"
                else if(oldFormat.eq."YYYYMMDD_HHmmss")then
                        MM      = tostring(tmp(4:5))
                        DD      = tostring(tmp(6:7))
                        YYYY    = tostring(tmp(0:3))
                        HH      = tostring(tmp(9:10))
                        mm      = tostring(tmp(11:12))
                        ss      = tostring(tmp(13:14))
                else if(oldFormat.eq."YYYY-MM-DD_HH:mm:ss")then
                        MM      = tostring(tmp(5:6))
                        DD      = tostring(tmp(8:9))
                        YYYY    = tostring(tmp(0:3))
                        HH      = tostring(tmp(11:12))
                        mm      = tostring(tmp(14:15))
                        ss      = tostring(tmp(17:18))
                else if(oldFormat.eq."YYYY-MM-DD_HH")then
                        MM      = tostring(tmp(5:6))
                        DD      = tostring(tmp(8:9))
                        YYYY    = tostring(tmp(0:3))
                        HH      = tostring(tmp(11:12))
                        mm      = "00"
                        ss      = "00"
                end if  end if  end if  end if  end if
		end if  end if


                ; 2) Add in forecast hours to get new time
                if(fHr.ne.0.)then
                        time_hours      = cd_inv_calendar(toint(YYYY),toint(MM),toint(DD),\
                                                          toint(HH),toint(mm),toint(ss),\
                                                          "hours since 1970-01-01 00:00",0)
                        time_hours      = time_hours+fHr2(aaa)
                        tmp             := cd_calendar(time_hours,0)
                        YYYY            = sprinti("%0.4i",toint(tmp(0,0)))
                        MM              = sprinti("%0.2i",toint(tmp(0,1)))
                        DD              = sprinti("%0.2i",toint(tmp(0,2)))
                        HH              = sprinti("%0.2i",toint(tmp(0,3)))
                        mm              = sprinti("%0.2i",toint(tmp(0,4)))
                        ss              = sprinti("%0.2i",toint(tmp(0,5)))
                        delete([/tmp,time_hours/])
                end if


                ; 3) Reconstruct the new string
                if(newFormat.eq."MM/DD/YYYY (HH:mm)")then
                        newStr(aaa)  = ""+MM+"/"+DD+"/"+YYYY+" ("+HH+":"+mm+")"
                else if(newFormat.eq."YYYYMMDDHH")then
                        newStr(aaa)  = ""+YYYY+MM+DD+HH
                else if(newFormat.eq."YYYYMMDD_HH")then
                        newStr(aaa) = ""+YYYY+MM+DD+"_"+HH
                else if(newFormat.eq."YYYYMMDD")then
                        newStr(aaa) = ""+YYYY+MM+DD
                else if(newFormat.eq."YYYYMMDD_HHmmss")then
                        newStr(aaa) = ""+YYYY+MM+DD+"_"+HH+mm+ss
                else if(newFormat.eq."YYYY-MM-DD_HH:mm:ss")then
                        newStr(aaa) = ""+YYYY+"-"+MM+"-"+DD+"_"+HH+":"+mm+":"+ss
                else if(newFormat.eq."YYYY-MM-DD_HH")then
                        newStr(aaa) = ""+YYYY+"-"+MM+"-"+DD+"_"+HH
                else if(newFormat.eq."HHz Month DD YYYY")then
                        MMStr   = (/"Jan","Feb","Mar","Apr","May","Jun",\
                                    "Jul","Aug","Sep","Oct","Nov","Dec"/)
                        newStr(aaa)  = ""+HH+"z "+MMStr(toint(MM)-1)+" "+DD+" "+YYYY
                        delete(MMStr)
                else if(newFormat.eq."HHz DOW, Month DD YYYY")then
                        MMStr   = (/"Jan","Feb","Mar","Apr","May","Jun",\
                                    "Jul","Aug","Sep","Oct","Nov","Dec"/)
                        DOWStr  = (/"Sun","Mon","Tue","Wed","Thu","Fri","Sat"/)
                        DOW     = day_of_week(toint(YYYY),toint(MM),toint(DD))
                        newStr(aaa)  = ""+HH+"z "+DOWStr(DOW)+", "+MMStr(toint(MM)-1)+" "+DD+" "+YYYY
                        delete([/MMStr,DOWStr,DOW/])
		else if(newFormat.eq."DD/HH")then
			newStr(aaa) = DD+"/"+HH
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if


                delete([/YYYY,MM,DD,HH,mm,ss/])
		
	end do
	
        else
                newStr = oldStr
        end if


        return(newStr)

end



;
;================================================================
; chkCmdInputs

undef("chkCmdInputs")
procedure chkCmdInputs (vars2chk[*]:string,nameBds[1]:string)
local masterChk, nChk
begin

	masterChk	= (/"nameBds", "tier", "dSource",\
			    "iDate", "IDtag"/)

	nChk	= dimsizes(vars2chk)

	do ccc = 0,nChk-1

		if(.not.any(masterChk.eq.vars2chk(ccc)))then   continue   end if

		if(vars2chk(ccc).eq."nameBds")then; .and. .not.isvar("nameBds"))then
			print("MSG:  'nameBds' is not defined.")
			print("MSG:  Assuming nameBds='basin'.")
			nameBds = "basin"
			;nameBds = "BoB"
		end if

		if(vars2chk(ccc).eq."tier" .and. .not.isvar("tier"))then
			print("MSG:  'tier' is not defined.")
			print("MSG:  Assuming tier='Tier1'.")
			tier = "Tier1"
		end if

		if(vars2chk(ccc).eq."dSource" .and. .not.isvar("dSource"))then
			print("MSG:  'dSource' is not defined.")
			print("MSG:  Assuming dSource='HWRF16'.")
			dSource = "HWRF"
		end if

		if(vars2chk(ccc).eq."iDate" .and. .not.isvar("iDate"))then
			iDate = ""
		end if

		if(vars2chk(ccc).eq."IDtag" .and. .not.isvar("IDtag"))then
			IDtag = ""
		end if

	end do
end





;
;================================================================
; circle_ll

undef("circle_ll")
procedure circle_ll(wks:graphic,plt:graphic,x0:numeric,y0:numeric,r:numeric,res:logical)
; Draws a circle (or ellipse) or radius (or x-axis length) r centered at (x0,y0). Optionally it can draw an ellipse and axes can be rotated as desired.
; Input Parameters:
; wks: graphics workstation
; plt: the plot graphics
; (x0,y0): center of the circle in terms of plot coordinate
; r: radius (or x-direction length) of the circle in terms of plot coordinate
; res: resources. If true can take any line graphic parameter valid to gsn_add_polyline. In addition, there are two special resources for this procedure:
;      Scale: The scale by which the y-axis length should be modified to the x-axis length. This can be used to plot an ellipse. (default=1).
;      Rotation: The rotation angle (in degrees) by which the axes are rotated (positive anticlockwhise). (default=0).
;
; Example:
;...
; arr=random_uniform(1,10,(/15,20/))
; wks = gsn_open_wks("x11","circle");
; res = True
; res@vpHeightF=0.5
; res@vpWidthF=0.8
; res@gsnDraw = False
; res@gsnFrame = False
; plt=gsn_csm_contour(wks,arr,res)

; resc=True
; resc@gsLineColor="red"
; resc@gsLineThicknessF=2
; resc@Scale=1.5
; resc@Rotation=-45
; circle_ll(wks,plt,10,7,3,resc)
; draw(plt)
; frame(wks)
;...
;
; Author: Arindam Chakraborty <arch at caos.iisc.ernet.in>
;

begin
	rot = 0.0
	scale = 1.0
	th = ispan(0,360,1)
	pi = acos(-1.0)

	res1 = res
	if(res)then
		if(isatt(res,"Scale"))then
			scale = res@Scale
			delete(res1@Scale)
		end if
		if(isatt(res,"Rotation"))then
			rot = res@Rotation
			delete(res1@Rotation)
		end if
	end if

	getvalues plt
		"trXMinF":xmin
		"trXMaxF":xmax
		"trYMinF":ymin
		"trYMaxF":ymax
		"vpXF":vpx0
		"vpYF":vpy1
		"vpWidthF":vpw
		"vpHeightF":vph
	end getvalues

	rx = r/(xmax-xmin)
	ry = rx*vpw/vph

	xpts1 = cos(th*pi/180)
	ypts1 = sin(th*pi/180)*scale

	xpts2 = xpts1*cos(rot*pi/180) - ypts1*sin(rot*pi/180)
	ypts2 = xpts1*sin(rot*pi/180) + ypts1*cos(rot*pi/180)

	xpts = rx*xpts2*(xmax-xmin) + x0
	ypts = ry*ypts2*(ymax-ymin) + y0
	str = unique_string("circle_ll")
	plt@$str$ = gsn_add_polyline(wks,plt,xpts,ypts,res1)
	delete([/str,xpts1,ypts1,xpts2,ypts2,th,res1/])

end




;
;================================================================
; defineCMAP_fill

undef("defineCMAP_fill")
function defineCMAP_fill (var[1]:string,pLev[1]:string,ColorMap[1]:string)
local tmp, cmap
begin

		; For Relative Vorticity
		if(var.eq."RVO")then
			if(any((/"850","700","500","200"/).eq.pLev))then
				cmap	= read_colormap_file(ColorMap)
				cmap	:= cmap(8:dimsizes(cmap(:,0))-10:-1,:)
				cmap(0,:)	= (/0.,0.,0.,0./)
			end if
		
		else if(var.eq."REFL".or.var.eq."REFD")then  ;MKedit
			cmap	= read_colormap_file(ColorMap)
			;cmap(0,:)	= (/0.,0.,0.,0./)
		
		else if(var.eq."TPRCP")then
			cmap	= read_colormap_file(ColorMap)
			;cmap	:= cmap(65:,:)
			;cmap(0,:)	= (/0.,0.,0.,0./)
			
		else if(var.eq."MSLP")then
			cmap	= read_colormap_file(ColorMap)
			;cmap	:= cmap(10:175,:)
			;cmap(0,:)	= (/0.,0.,0.,0./)
		else
			cmap	= read_colormap_file(ColorMap)
		
		end if
		end if
		end if
		end if
		
	return(cmap)

end



;
;================================================================
; defineCMAP_name
undef("defineCMAP_name")
function defineCMAP_name (var[1]:string,lev[1]:string)
local cmap
begin

		; For Relative Vorticity
		if(var.eq."RVO")then
			if(any((/"850","700","500","200","02001000"/).eq.lev))then
				cmap = "matlab_hot"
			end if
		
		; For Total Wind
		else if(var.eq."UV")then
			if(any((/"02000850d","05000850d","07000850d"/).eq.lev))then
                cmap = "shear"
			else if(any((/"02500850m","05000850m","02500500m","07000850m"/).eq.lev))then
				cmap = "WhiteBlueGreenYellowRed"
			else if(any((/"850","10","02001000","02001000y","02001000x"/).eq.lev))then
				;cmap = "WhiteBlueGreenYellowRed"
				cmap = "tcwinds1"
			else if (lev.eq."750") then
				cmap = "TDRwind"
			end if  end if  end if  end if

		; For Relative Humidity
		else if(var.eq."RH")then
			;cmap	= "GMT_drywet"
			cmap	= "MPL_BrBG"
		
		; For Precipitation Rate
		else if(any((/"PRCP","PRATE"/).eq.var))then
			cmap	= "precip3_16lev"
		
		; For Total Precipitation
		else if(var.eq."TPRCP")then
			;cmap	= "prcp_1"
			;cmap	= "GMT_wysiwygcont"
			;cmap	= "tprcp"
			cmap	= "tprcp2"
			;cmap    = "ncl_default"
		
		; For Total Precipitable Water
		else if(var.eq."TPW")then
			;cmap	= "MPL_rainbow"
			cmap	= "MPL_BrBG"
		
		; For Geopotential Height
		else if(var.eq."HGT")then
			cmap	= "BlAqGrYeOrRe"
		
		; For Convective Available Potential Energy
		else if(var.eq."CAPE")then
			cmap	= "WhiteBlueGreenYellowRed"
		
		; For Mean Sea-Level Pressure
		else if(var.eq."MSLP")then
			cmap	= "mslp"
			;cmap	= "ncl_default"
		
		; For Air Temperature
		else if(var.eq."T")then
			;if(lev.eq."2")then
				cmap = "ncl_default"
			;end if			
		
		; For Steering Flow
		else if(var.eq."SFLO")then
			cmap	= "WhViBlGrYeOrRe"
		
		; For Vertical Wind Shear
		else if(var.eq."VWS")then
			cmap	= "WhViBlGrYeOrRe"
		
		; For Composite Reflectivity
		else if(var.eq."REFL")then  
			cmap	= "dbz"

		else if(var.eq."REFD")then ; MKedit
			cmap    = "REFD"

		else
			cmap	= "BlAqGrYeOrReVi200"
			
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if

	return(cmap)

end



;
;================================================================
; defineLevels

undef("defineLevels")
function defineLevels (var[1]:string,lev[1]:string)
local levels
begin

		; For Relative Vorticity
		if(var.eq."RVO")then
			if(any((/"850","700","500","200"/).eq.lev))then
				levels	= fspan(2.,70.,35)
				levels@stride = 2
			end if
		end if
		
		; For Total Wind
		if(var.eq."UV")then
			;if(lev.eq."850")then
			;	levels = new(41,float)
			;	levels(:22)	= fspan(10.,65.,23)
			;	levels(23:)	= fspan(70.,155.,18)
			;	levels@stride	= 2
			;end if
			if(lev.eq."02000850d")then
				;levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
				;		    17.5, 20.0, 22.5, 25.0, 27.5,\
				;		    30.0, 35.0, 40.0, 45.0, 50.0,\
				;		    55.0, 60.0, 70.0, 80.0, 90.0/)
                levels      = fspan(5.0,50.0,19)
				levels@stride	= 2
			end if
			if(lev.eq."05000850d")then
				;levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
				;		    17.5, 20.0, 22.5, 25.0, 27.5,\
				;		    30.0, 35.0, 40.0, 45.0, 50.0,\
				;		    55.0, 60.0/)
                levels      = fspan(5.0,50.0,19)
				levels@stride	= 2
			end if
			if(lev.eq."07000850d")then
				;levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
				;		    17.5, 20.0, 22.5, 25.0, 27.5,\
				;		    30.0, 35.0, 40.0, 45.0, 50.0/)
                levels      = fspan(5.0,50.0,19)
				levels@stride	= 2
			end if
			if(lev.eq."02500850m")then
				levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
						    17.5, 20.0, 22.5, 25.0, 27.5,\
						    30.0, 35.0, 40.0, 45.0, 50.0,\
						    55.0, 60.0, 70.0, 80.0, 90.0/)
				levels@stride	= 1
			end if
			if(lev.eq."05000850m")then
				levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
						    17.5, 20.0, 22.5, 25.0, 27.5,\
						    30.0, 35.0, 40.0, 45.0, 50.0,\
						    55.0, 60.0/)
				levels@stride	= 1
			end if
			if(lev.eq."07000850m")then
				levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
						    17.5, 20.0, 22.5, 25.0, 27.5,\
						    30.0, 35.0, 40.0/)
				levels@stride	= 1
			end if
			if(lev.eq."02500500m")then
				levels		= (/ 5.0,  7.5, 10.0, 12.5, 15.0,\
						    17.5, 20.0, 22.5, 25.0, 27.5,\
						    30.0, 35.0, 40.0, 45.0, 50.0,\
						    55.0, 60.0, 70.0, 80.0, 90.0/)
				levels@stride	= 1
			end if
			if (lev.eq."750") then
				levels     = (/  7.0  ,  10.0  ,  13.0  ,  16.0  ,   \
						 19.0  ,  22.0  ,  25.0  ,  28.0  ,  31.0  ,   \
						 34.0  ,  36.0  ,  38.0  ,  40.0  ,  42.0  ,   \
						 44.0  ,  46.0  ,  48.0  ,  50.0  ,  52.0  ,   \
						 54.0  ,  56.0  ,  58.0  ,  60.0  ,  62.0  ,   \
						 64.0  ,  69.333,  74.666,  80.0  ,  85.333,   \
					 	 90.666,  96.0  , 100.666, 105.333, 110.0  ,   \
						115.0  , 120.0  , 125.0  , 130.0  , 135.0  ,   \
						140.0  , 145.0  , 150.0  , 155.0        /)
				levels@stride   = 3
			end if
			if(any((/"10","850","0850","02001000","02001000y","02001000x"/).eq.lev))then
				;levels		= (/ 10.00,  13.00,  16.00,  19.00,  22.00, \
				;		     25.00,  28.00,  31.00,  34.00,  36.00, \
				;		     38.00,  40.00,  42.00,  44.00,  46.00, \
				;		     48.00,  50.00,  52.00,  54.00,  56.00, \
				;		     58.00,  60.00,  62.00,  64.00,  68.75, \
				;		     73.50,  78.25,  83.00,  86.25,  89.50, \
				;		     92.75,  96.00, 100.50, 105.00, 109.50, \
				;		    114.00, 119.25, 124.50, 129.75, 135.00, \
				;		    140.00, 145.00, 150.00, 155.00/)
				levels		= (/10., 20., 34., 50., 64., 83., 96., 114., 137./)
				levels@stride = 1
			end if
		end if
		
		; For Relative Humidity
		if(var.eq."RH")then
			levels		= fspan(5.,95.,19)
			levels@stride	= 2
		end if
		
		; For Precipitation
		if(any((/"PRCP","PRATE"/).eq.var))then
			levels			= (/ 0.1,  0.25, 0.5,  1.0,  1.5,\
						     2.0,  3.0,  4.0,  5.0,  7.5,\
						    10.0, 15.0, 20.0, 25.0, 30.0,\
						    40.0/)
			levels@stride		= 1
		end if
		
		; For Total Precipitation
		if(var.eq."TPRCP")then
			;levels			= (/ 0.05, 0.1,  0.25, 0.5,  0.75,\
			;			     1.0,  1.25, 1.5,  2.0,  2.5, \
			;			     3.0,  3.5,  4.0,  5.0,  6.0, \
			;			     8.0, 10.0, 12.0, 14.0, 16.0, \
			;			    20.0, 24.0/)
			;levels			= (/  0.01,  0.10,  0.25,  0.50,  0.75, \
			;			      1.00,  1.25,  1.50,  1.75,  2.00, \
			;			      3.00,  4.00,  6.00,  8.00, 10.00, \
			;			     15.00, 20.00/)
			levels          = (/ 0.01,  0.10,  0.25,  0.50,  0.75,  1.00, \
					     1.25,  1.50,  1.75,  2.00,  2.50,  3.00, \
					     4.00,  5.00,  7.00, 10.00, 15.00, 20.00, \
					    25.00, 30.00, 25.00, 40.00, 50.00, 60.00/)
			levels@stride		= 1
		end if
		
		; For Total Precipitable Water
		if(var.eq."TPW")then
			levels	= fspan(5.,65.,31)
			levels@stride	= 2
		end if
		
		; For Geopotential Height
		if(var.eq."HGT")then
			if(any((/"02500500d"/).eq.lev))then
				levels	= fspan(490.,510.,21)
				levels@stride = 2
			else if(any((/"500","0500"/).eq.lev))then
				levels	= fspan(468.,540.,13)
				levels	:= array_append_record(levels,fspan(542.,600.,30),0)
				levels@stride	= 2
			else if(lev.eq."200")then
				levels	= fspan(1170.,1260.,31)
				levels@stride	= 2
			end if  end if  end if
		end if
		
		; For Reflectivity
		if(var.eq."REFL")then
			levels	= fspan(5.,75.,15)
			levels@stride	= 2
		end if
	
		; For Reflectivity ay 750 hpa (MK)
		if (var.eq."REFD") then
			levels  = fspan(2.,78.,39)
                        levels@stride   = 2
		end if
	
		; For Storm_Relative Helicity
		if(var.eq."HLCY")then
			levels	= fspan(100.,600.,11)
			levels@stride	= 2
		end if
		
		; For Storm_Relative Helicity
		if(var.eq."CAPE")then
			levels	= fspan(500.,4500.,17)
			levels@stride	= 2
		end if
		
		; For Mean Sea-Level Pressure
		if(var.eq."MSLP")then
			levels	= fspan(980.,1040.,16)
			levels@stride	= 1
		end if

		; For Air Temperature
                if(var.eq."T")then
                        if(lev.eq."2")then
				levels  = fspan(280.,320.,21)
                        	levels@stride   = 1
			else if(lev.eq."850")then
				levels  = fspan(270.,310.,21)
                        	levels@stride   = 1
			end if  end if
                end if
		
		; For Steering Flow
		if(var.eq."SFLO")then
			levels	= fspan(10.,60.,11)
			levels@stride	= 1
		end if
		
	return(levels)

end



;================================================================
; filter121
;
; If cyclic = True, then opt only applies to the y-direction

undef("filter121")
function filter121 (V:float,N[1]:integer,cyclic[1]:logical)
local nDims, wgts, V2
begin

        nDims   = dimsizes(dimsizes(V))
	V2	= V
        wgts    = (/0.25,0.5,0.25/)

        if(nDims.lt.2 .or. nDims.gt.4)then
                print("ERROR: Data has "+nDims+" dimensions.")
                print("ERROR: Unsupported number of dimensions.")
                exit
        end if

        do lll = 1,1
                if(nDims.eq.2)then	i = 1
		else if(nDims.eq.3)then i = 2
		else if(nDims.eq.4)then i = 3
		end if
		end if
		end if

		; filter in the x-direction
		if(cyclic)then
			V2 = wgt_runave_n_Wrap(V2,wgts,-1,i)
		else
			V2 = wgt_runave_n_Wrap(V2,wgts,1,i)
		end if

		; filter in the y-direction
		V2 = wgt_runave_n_Wrap(V2,wgts,1,i-1)
        end do ;lll

        return(V2)

end



;
;================================================================
; findVarName

undef("findVarName")
function findVarName (dSource:string,var[1]:string,lev[1]:string)
local oldVar, newVar, masterList, lev2, catNum, levch
begin

	levch	= tochar(lev)
	if(lev.ne."" .and. dimsizes(levch).le.5)then		lev2 = tostring(toint(lev))
	else if(lev.ne."" .and. dimsizes(levch).gt.5)then	lev2 = tostring(toint(tostring(levch(:3))))
	else		   					lev2 = ""
	end if  end if
	
	masterList	= (/(/"T",	"101"/),\	; 101.  3D Air Temperature
			    (/"HGT",	"102"/),\	; 102.  3D Geopotential Height
			    (/"U",	"103"/),\	; 103.  3D Zonal Wind
			    (/"V",	"104"/),\	; 104.  3D Meridional Wind
			    (/"UV",	"105"/),\	; 105.  3D Total Wind
			    (/"TH",	"106"/),\	; 106.  3D Potential Temperature
			    (/"RH",	"107"/),\	; 107.  3D Relative Humidity
			    (/"SPH",	"108"/),\	; 108.  3D Specific Humidity
			    (/"W",	"109"/),\	; 109.  3D Vertical Velocity
			    (/"AVO",	"110"/),\	; 110.  3D Vorticity (absolute)
			    (/"RVO",	"111"/),\	; 111.  3D Vorticity (relative)
			    (/"DPT",	"112"/),\	; 112.  3D Dewpoint Temperature
			    (/"U10",	"201"/),\	; 201.  10-m Zonal Wind
			    (/"V10",	"202"/),\	; 202.  10-m Meridional Wind
			    (/"UV10",	"203"/),\	; 203.  10-m Total Wind
			    (/"T2",	"301"/),\	; 301.  2-m Air Temperature
			    (/"DPT2",	"302"/),\	; 302.  2-m Dew Point Temperature
			    (/"SPH2",	"303"/),\	; 303.  2-m Specific Humidity
			    (/"RH2",	"304"/),\	; 304.  2-m Relative Humidity
			    (/"MSLP",	"401"/),\	; 401.  Mean Sea Level Pressure
			    (/"LHFLX",	"402"/),\	; 402.  Surface Latent Heat Flux
			    (/"SHFLX",	"403"/),\	; 403.  Surface Sensible Heat Flux
			    (/"TSFC",	"404"/),\	; 404.  Surface Temperature
			    (/"SST",	"405"/),\	; 405.  Sea Surface Temperature
			    (/"PSFC",	"406"/),\	; 406.  Surface Pressure
			    (/"TTROP",	"501"/),\	; 501.  Tropopause Temperature
			    (/"PRCP",	"601"/),\	; 601.  Bulk Precipitation Rate
			    (/"PRATE",	"602"/),\	; 602.  Instantaneous Precipitation Rate
			    (/"TPRCP",	"603"/),\	; 603.  Total Precipitation
			    (/"TPW",	"604"/),\	; 604.  Total Precipitable Water
			    (/"REFL",	"605"/),\	; 605.  Composite Reflectivity
			    (/"HLCY",	"606"/),\	; 606.  Storm-Relative Helicity
			    (/"CAPE",	"607"/),\	; 607.  Convective Available Potential Energy
			    (/"REFD",	"608"/),\	; 608.  Reflectivity  (200-100000pa) ; MKadd
			    (/"LAND",  "1001"/)\	; 1001. Land-Sea Mask
			    /)


	
	
	
	; If the level is non-pressure, we need to add it to the variable name
	; Otherwise, oldVar = var
	; MKnote - UV lev=10 -> U10,V10 ; T lev=2 -> T2
	if(any((/"10","2"/).eq.lev))then
		oldVar = var+lev
	else
		oldVar = var
	end if
	if(.not.any(masterList(:,0).eq.oldVar))then	print("ERROR: findVarName: Variable '"+oldVar+"' not recognized")
							exit
	end if
	catNum	= masterList(ind(masterList(:,0).eq.oldVar),1)
	

	; VARIABLE TABLE FOR:  GFS, HWRF
	if(any((/"GFS","GFS_an","GFS_fcst","GFS_d03"/).eq.dSource))then
		varList	= (/(/"TMP_P0_L100_GLL0",	"101"/),\	; 101.  3D Air Temperature
			    (/"HGT_P0_L100_GLL0",	"102"/),\	; 102.  3D Geopotential Height
			    (/"UGRD_P0_L100_GLL0",	"103"/),\	; 103.  3D Zonal Wind
		 	    (/"VGRD_P0_L100_GLL0",	"104"/),\	; 104.  3D Meridional Wind
			    (/"UGRD_P0_L100_GLL0",	"105"/),\	; 105.  3D Total Wind
			    (/"RH_P0_L100_GLL0",	"107"/),\	; 107.  3D Relative Humidity
			    (/"SPFH_P0_L100_GLL0",	"108"/),\	; 108.  3D Specific Humidity
			    (/"VVEL_P0_L100_GLL0",	"109"/),\	; 109.  3D Vertical Velocity
			    (/"ABSV_P0_L100_GLL0",	"110"/),\	; 110.  3D Vorticity (absolute)
			    (/"ABSV_P0_L100_GLL0",	"111"/),\	; 111.  3D Vorticity (relative)
			    (/"DPT_P0_L100_GLL0",	"112"/),\	; 112.  3D Dewpoint Temperature
			    (/"UGRD_P0_L103_GLL0",	"201"/),\	; 201.  10-m Zonal Wind
			    (/"VGRD_P0_L103_GLL0",	"202"/),\	; 202.  10-m Meridional Wind
			    (/"UGRD_P0_L103_GLL0",	"203"/),\	; 203.  10-m Total Wind
			    (/"TMP_P0_L103_GLL0",	"301"/),\	; 301.  2-m Air Temperature
			    (/"DPT_P0_L103_GLL0",	"302"/),\	; 302.  2-m Dew Point Temperature
			    (/"SPFH_P0_L103_GLL0",	"303"/),\	; 303.  2-m Specific Humidity
			    (/"RH_P0_L103_GLL0",	"304"/),\	; 304.  2-m Relative Humidity
			    (/"MSLET_P0_L101_GLL0",	"401"/),\	; 401.  Mean Sea Level Pressure
			    (/"LHTFL_P0_L1_GLL0",	"402"/),\	; 402.  Surface Latent Heat Flux
			    (/"SHTFL_P0_L1_GLL0",	"403"/),\	; 403.  Surface Sensible Heat Flux
			    (/"TMP_P0_L1_GLL0",		"404"/),\	; 404.  Surface Temperature
			    (/"WTMP_P0_L1_GLL0",	"405"/),\	; 405.  Sea Surface Temperature
			    (/"PRES_P0_L1_GLL0",	"406"/),\	; 406.  Surface Pressure
			    (/"TMP_P0_L7_GLL0",		"501"/),\	; 501.  Tropopause Temperature
			    (/"APCP_P8_L1_GLL0_acc",	"601"/),\	; 601.  Bulk Precipitation Rate
			    (/"PRATE_P0_L1_GLL0",	"602"/),\	; 602.  Instantaneous Precipitation Rate
			    (/"APCP_P8_L1_GLL0_acc",	"603"/),\	; 603.  Total Precipitation
			    (/"PWAT_P0_L200_GLL0",	"604"/),\	; 604.  Total Precipitable Water
			    (/"REFC_P0_L200_GLL0",	"605"/),\	; 605.  Composite Reflectivity
			    (/"HLCY_P0_2L103_GLL0",	"606"/),\	; 606.  Storm-Relative Helicity
			    (/"CAPE_P0_L1_GLL0",	"607"/),\	; 607.  Convective Available Potential Energy
			    (/"LAND_P0_L1_GLL0",       "1001"/)\	; 1001. Land-Sea Mask
			    /)
	
	else if(isStrSubset(dSource,"HWRF") .or. isStrSubset(dSource,"HAFS"))then
		varList	= (/	(/"TMP_P0_L100_GLL0",	"101"/),\	; 101.  3D Air Temperature
				(/"HGT_P0_L100_GLL0",	"102"/),\	; 102.  3D Geopotential Height
				(/"UGRD_P0_L100_GLL0",	"103"/),\	; 103.  3D Zonal Wind
		 		(/"VGRD_P0_L100_GLL0",	"104"/),\	; 104.  3D Meridional Wind
				(/"UGRD_P0_L100_GLL0",	"105"/),\	; 105.  3D Total Wind
				(/"RH_P0_L100_GLL0",	"107"/),\	; 107.  3D Relative Humidity
				(/"SPFH_P0_L100_GLL0",	"108"/),\	; 108.  3D Specific Humidity
				(/"VVEL_P0_L100_GLL0",	"109"/),\	; 109.  3D Vertical Velocity
				(/"ABSV_P0_L100_GLL0",	"110"/),\	; 110.  3D Vorticity (absolute)
				(/"ABSV_P0_L100_GLL0",	"111"/),\	; 111.  3D Vorticity (relative)
				(/"DPT_P0_L100_GLL0",   "112"/),\	; 112.  3D Dewpoint Temperature
				(/"UGRD_P0_L103_GLL0",  "201"/),\	; 201.  10-m Zonal Wind
				(/"VGRD_P0_L103_GLL0",  "202"/),\	; 202.  10-m Meridional Wind
				(/"UGRD_P0_L103_GLL0",  "203"/),\	; 203.  10-m Total Wind
				(/"TMP_P0_L103_GLL0",   "301"/),\	; 301.  2-m Air Temperature
				(/"DPT_P0_L103_GLL0",   "302"/),\	; 302.  2-m Dew Point Temperature
				(/"SPFH_P0_L103_GLL0",  "303"/),\	; 303.  2-m Specific Humidity
				(/"RH_P0_L103_GLL0",    "304"/),\	; 304.  2-m Relative Humidity
				(/"PRMSL_P0_L101_GLL0",	"401"/),\	; 401.  Mean Sea Level Pressure
				(/"LHTFL_P0_L1_GLL0",	"402"/),\	; 402.  Surface Latent Heat Flux
				(/"SHTFL_P0_L1_GLL0",   "403"/),\	; 403.  Surface Sensible Heat Flux
				(/"TMP_P0_L1_GLL0",     "404"/),\	; 404.  Surface Temperature
				(/"WTMP_P0_L1_GLL0",    "405"/),\	; 405.  Sea Surface Temperature
				(/"PRES_P0_L1_GLL0",	"406"/),\	; 406.  Surface Pressure
				(/"TMP_P0_L7_GLL0",     "501"/),\	; 501.  Tropopause Temperature
				(/"APCP_P8_L1_GLL0_acc","601"/),\	; 601.  Bulk Precipitation Rate
				(/"PRATE_P0_L1_GLL0",   "602"/),\	; 602.  Instantaneous Precipitation Rate
				(/"APCP_P8_L1_GLL0_acc","603"/),\	; 603.  Total Precipitation
				(/"PWAT_P0_L200_GLL0",	"604"/),\	; 604.  Total Precipitable Water
				(/"REFC_P0_L200_GLL0",	"605"/),\	; 605.  Composite Reflectivity
				(/"HLCY_P0_2L103_GLL0",	"606"/),\	; 606.  Storm-Relative Helicity
				(/"CAPE_P0_L1_GLL0",	"607"/),\	; 607.  Convective Available Potential Energy
				(/"REFD_P0_L100_GLL0",	"608"/),\	; 608.  Reflectivity  ;MK
				(/"LAND_P0_L1_GLL0",	"1001"/)\	; 1001. Land-Sea Mask
			    /)
	
	; VARIABLE TABLE FOR: ERA-INTERIM
	else if(dSource.eq."ERAi")then
		varList	= (/(/"TMP_P0_L100_GLL0",	"101"/),\	; 101.  3D Air Temperature
			    (/"Z_GDS0_ISBL",		"102"/),\	; 102.  3D Geopotential Height
		 	    (/"VGRD_P0_L100_GLL0",	"104"/),\	; 104.  3D Meridional Wind
			    (/"RH_P0_L100_GLL0",	"107"/),\	; 107.  3D Relative Humidity
			    (/"VVEL_P0_L100_GLL0",	"109"/),\	; 109.  3D Vertical Velocity
			    (/"ABSV_P0_L100_GLL0",	"110"/),\	; 110.  3D Vorticity (absolute)
			    (/"ABSV_P0_L100_GLL0",	"111"/),\	; 111.  3D Vorticity (relative)
			    (/"UGRD_P0_L100_GLL0",	"103"/),\	; 103.  3D Zonal Wind
			    (/"UGRD_P0_L100_GLL0",	"105"/)\	; 105.  3D Total Wind
			    /)
	
	; VARIABLE TABLE FOR: 2013 BASIN-SCALE HWRF
	else if(any((/"H3HW","HEDAS"/).eq.dSource))then
		varList	= (/	(/"TMP_GDS0_ISBL",	"101"/),\	; 101.  3D Air Temperature
				(/"HGT_GDS0_ISBL",	"102"/),\	; 102.  3D Geopotential Height
				(/"U_GRD_GDS0_ISBL",    "103"/),\	; 103.  3D Zonal Wind
				(/"V_GRD_GDS0_ISBL",	"104"/),\	; 104.  3D Meridional Wind
				(/"U_GRD_GDS0_ISBL",    "105"/),\	; 105.  3D Total Wind
				(/"R_H_GDS0_ISBL",	"107"/),\	; 107.  3D Relative Humidity
				(/"SPF_H_GDS0_ISBL",	"108"/),\	; 108.  3D Specific Humidity
				(/"V_VEL_GDS0_ISBL",	"109"/),\	; 109.  3D Vertical Velocity
				(/"ABS_V_GDS0_ISBL",	"110"/),\	; 110.  3D Vorticity (absolute)
				(/"ABS_V_GDS0_ISBL",	"111"/),\	; 111.  3D Vorticity (relative)
				(/"P_WAT_GDS0_EATM",	"604"/),\	; 604.  Total Precipitable Water
				(/"REFC_GDS0_EATM",	"605"/)\	; 605.  Composite Radar Reflectivity
			    /)
	
	; VARIABLE TABLE FOR:  FVGFS_ATL
	else if(isStrSubset(dSource,"fvGFS_ATL"))then
		varList = (/(/"TMP"+lev2,		"101"/),\	; 101.  3D Air Temperature
			    (/"HGT"+lev2,		"102"/),\	; 102.  3D Geopotential Height
			    (/"VGRD"+lev2,		"104"/),\	; 104.  3D Meridional Wind
			    (/"RH"+lev2,		"107"/),\	; 107.  3D Relative Humidity
			    (/"VVEL"+lev2,		"109"/),\	; 109.  3D Vertical Velocity
			    (/"UGRD"+lev2,		"103"/),\	; 103.  3D Zonal Wind
			    (/"PRMSL",			"401"/),\	; 401.  Mean Sea Level Pressure
			    (/"UGRD"+lev2,		"105"/),\	; 105.  3D Total Wind
			    (/"UGRD"+lev2,		"110"/),\	; 110.  3D Vorticity (absolute)
			    (/"UGRD"+lev2,		"111"/),\	; 111.  3D Vorticity (relative)
			    (/"PRESsfc",		"406"/),\	; 406.  Surface Pressure
			    (/"PWAT",			"604"/),\	; 604.  Total Precipitable Water
			    (/"REFC",			"605"/),\	; 605.  Composite Reflectivity
			    (/"CAPEsfc",		"607"/),\	; 607.  Convective Available Potential Energy
			    (/"UGRD10m",		"201"/),\	; 201.  10-m Zonal Wind
			    (/"VGRD10m",		"202"/),\	; 202.  10-m Meridional Wind
			    (/"UGRD10m",		"203"/),\	; 203.  10-m Total Wind
			    (/"TMP2m",			"301"/),\	; 301.  2-m Air Temperature
			    (/"TMPsfc",			"404"/),\	; 404.  Surface Temperature
			    (/"DPT2m",			"302"/),\	; 302.  2-m Dew Point Temperature
			    (/"DPT"+lev2,		"112"/),\	; 112.  3D Dewpoint Temperature
			    (/"SPFH2m",			"303"/),\	; 303.  2-m Specific Humidity
			    (/"PRATEsfc",		"602"/)\	; 602.  Instantaneous Precipitation Rate
			    /)

	; VARIABLE TABLE FOR:  GEFS
	else if(any((/"GEFS"/).eq.dSource))then
		varList	= (/(/"TMP_P1_L100_GLL0",	"101"/),\	; 101.  3D Air Temperature
			    (/"HGT_P1_L100_GLL0",	"102"/),\	; 102.  3D Geopotential Height
			    (/"UGRD_P1_L100_GLL0",	"103"/),\	; 103.  3D Zonal Wind
		 	    (/"VGRD_P1_L100_GLL0",	"104"/),\	; 104.  3D Meridional Wind
			    (/"UGRD_P1_L100_GLL0",	"105"/),\	; 105.  3D Total Wind
			    (/"RH_P1_L100_GLL0",	"107"/),\	; 107.  3D Relative Humidity
			    (/"SPFH_P0_L100_GLL0",	"108"/),\	; 108.  3D Specific Humidity
			    (/"ABSV_P0_L100_GLL0",	"110"/),\	; 110.  3D Vorticity (absolute)
			    (/"ABSV_P0_L100_GLL0",	"111"/),\	; 111.  3D Vorticity (relative)
			    (/"DPT_P0_L100_GLL0",	"112"/),\	; 112.  3D Dewpoint Temperature
			    (/"UGRD_P1_L103_GLL0",	"201"/),\	; 201.  10-m Zonal Wind
			    (/"VGRD_P1_L103_GLL0",	"202"/),\	; 202.  10-m Meridional Wind
			    (/"UGRD_P0_L103_GLL0",	"203"/),\	; 203.  10-m Total Wind
			    (/"TMP_P1_L103_GLL0",	"301"/),\	; 301.  2-m Air Temperature
			    (/"DPT_P0_L103_GLL0",	"302"/),\	; 302.  2-m Dew Point Temperature
			    (/"SPFH_P0_L103_GLL0",	"303"/),\	; 303.  2-m Specific Humidity
			    (/"RH_P1_L103_GLL0",	"304"/),\	; 304.  2-m Relative Humidity
			    (/"PRMSL_P1_L101_GLL0",	"401"/),\	; 401.  Mean Sea Level Pressure
			    (/"LHTFL_P11_L1_GLL0_avg",	"402"/),\	; 402.  Surface Latent Heat Flux
			    (/"SHTFL_P11_L1_GLL0_avg",	"403"/),\	; 403.  Surface Sensible Heat Flux
			    (/"WTMP_P0_L1_GLL0",	"405"/),\	; 405.  Sea Surface Temperature
			    (/"PRES_P1_L1_GLL0",	"406"/),\	; 406.  Surface Pressure
			    (/"HGT_P1_L1_GLL0",		"407"/),\	; 407.  Surface Geopotential Height
			    (/"TMP_P0_L7_GLL0",		"501"/),\	; 501.  Tropopause Temperature
			    (/"ULWRF_P11_L8_GLL0_avg",	"502"/),\	; 502.  Upward Long-Wave Radiation Flux (TOA)
			    (/"APCP_P8_L1_GLL0_acc",	"601"/),\	; 601.  Bulk Precipitation Rate
			    (/"PRATE_P0_L1_GLL0",	"602"/),\	; 602.  Instantaneous Precipitation Rate
			    (/"APCP_P8_L1_GLL0_acc",	"603"/),\	; 603.  Total Precipitation
			    (/"PWAT_P1_L200_GLL0",	"604"/),\	; 604.  Total Precipitable Water
			    (/"REFC_P0_L200_GLL0",	"605"/),\	; 605.  Composite Reflectivity
			    (/"HLCY_P0_2L103_GLL0",	"606"/),\	; 606.  Storm-Relative Helicity
			    (/"CAPE_P1_2L108_GLL0",	"607"/),\	; 607.  Convective Available Potential Energy (Surface to 180 hPa)
			    (/"CIN_P1_2L108_GLL0",	"609"/),\	; 607.  Convective Inhibition (Surface to 180 hPa)
			    (/"VVEL_P1_L100_GLL0",	"701"/),\	; 701.  850 hPa Vertical Velocity
			    (/"LAND_P0_L1_GLL0",       "1001"/)\	; 1001. Land-Sea Mask	    
			    /)

	else if(any((/"ECMWF"/).eq.dSource))then
		varList = (/(/"TMP_P0_L100_GLL0",	"101"/),\	; 101.  3D Air Temperature
			    (/"HGT_P0_L100_GLL0",	"102"/),\	; 102.  3D Geopotential Height
			    (/"UGRD_P0_L100_GLL0",	"103"/),\	; 103.  3D Zonal Wind
		 	    (/"VGRD_P0_L100_GLL0",	"104"/),\	; 104.  3D Meridional Wind
			    (/"UGRD_P0_L100_GLL0",	"105"/),\	; 105.  3D Total Wind
			    (/"PRES_P0_L101_GLL0",	"401"/)\	; 401.  Mean Sea Level Pressure
			    /)
	
	end if  end if  end if  end if  end if  end if  end if
	
	match	= ind(varList(:,1).eq.catNum)
	if(ismissing(match(0)))then	print("WARNING: findVarName: Variable '"+oldVar+"' not found in table for "+dSource+".")
					newVar = "missing"
					return(newVar)
	else				newVar	= varList(match,0)
	end if
	
	
	; This is a temp. fix for Precipitation variables that were updated to have time interval information.
	; This should be integrated into the actual variable name, as with UV10, T2, etc.
	if(isStrSubset(var,"PRCP") .and. isStrSubset(lev,"h"))then	newVar = newVar+lev   end if
	
	
	return(newVar)

end



;
;================================================================
; GenPlotRes
;
undef("GenPlotRes")
function GenPlotRes (type:integer,OVLAY:logical)
local res, type2
begin
	
	type2		= tochar(tostring(type))
	type2		:= toint(tostring(type2(dimsizes(type2)-1)))


	; These resources are common to every option.
	res		= True
	res@gsnDraw	= False
	res@gsnFrame	= False
	res@gsnMaximize	= False
	
	
	; If not an overlay, add these resources for basic map funcitonality
	; These options strongly depend on map type (e.g., cylindrical equidistant, polar cylindrical)
	if(.not.OVLAY)then
	
		; CYLINDRICAL EQUIDISTANT PLOTTING RESOURCES
		if(type.lt.10)then	res@mpFillOn			= False
					;res@mpOutlineBoundarySets	= "AllBoundaries"
					res@mpGeophysicalLineColor	= "gray10"	;MKedit - was gray20
					res@mpGeophysicalLineThicknessF	= 2.5		;MKedit - was 5.0
					res@mpNationalLineColor		= "gray10"	;MKedit - was gray20
					res@mpNationalLineThicknessF	= 2.5		;MKedit - was 5.0
					res@mpUSStateLineColor		= "gray10"	;MKedit - was gray20
					res@mpUSStateLineThicknessF	= 1.5		;MKedit - was 2.5
					res@mpProvincialLineColor	= "gray10"	;MKedit - was gray20
					res@mpProvincialLineThicknessF	= 1.5		;MKedit - was 2.5
					res@mpGridAndLimbOn		= True
					res@mpGridLatSpacingF		= 10.
					res@mpGridLonSpacingF		= 10.
					res@mpGridLineDashPattern	= 2
					res@mpGridLineThicknessF	= 1.5
					res@mpDataBaseVersion		= "MediumRes"
					res@mpDataSetName		= "Earth..4"
					res@mpDataResolution		= "Coarsest"
					;res@mpOutlineBoundarySets	= "National"
					res@mpOutlineBoundarySets	= "AllBoundaries"
					res@mpOutlineSpecifiers		= "United States : States"
					;res@mpGridAndLimbDrawOrder	= "PreDraw"
					;res@mpOutlineDrawOrder		= "PreDraw"
					if(any((/0,1,2/).eq.type))then
						res@mpOutlineDrawOrder	= "Draw"
						res@cnFillDrawOrder	= "PreDraw"
					else if(any((/3,4,5/).eq.type))then
						res@mpOutlineDrawOrder	= "PreDraw"
						res@vcVectorDrawOrder	= "Draw"        ; draw vectors last
					end if  end if
					;res@tmXBMajorOutwardLengthF	= 0.0
					;res@tmXBMinorOutwardLengthF	= 0.0
					;res@tmYLMajorOutwardLengthF	= 0.0
					;res@tmYLMinorOutwardLengthF	= 0.0
					res@tmXBMode			= "Explicit"
					res@tmXBValues			= fspan(-280.,460.,75)
					res@tmXBLabels			= (/ "80E", "90E","100E","110E","120E","130E","140E","150E",\
									    "160E","170E", "180","170W","160W","150W","140W","130W",\
									    "120W","110W","100W", "90W", "80W", "70W", "60W", "50W",\
									     "40W", "30W", "20W", "10W",   "0", "10E", "20E", "30E",\
									     "40E", "50E", "60E", "70E", "80E", "90E","100E","110E",\
									    "120E","130E","140E","150E","160E","170E", "180","170W",\
									    "160W","150W","140W","130W","120W","110W","100W", "90W",\
									     "80W", "70W", "60W", "50W", "40W", "30W", "20W", "10W",\
									       "0", "10E", "20E", "30E", "40E", "50E", "60E", "70E",\
									     "80E", "90E","100E"/)
					res@tmXBLabelFontHeightF	= 0.009
					res@tmXBMajorLengthF		= 0.005
					res@tmXBMajorOutwardLengthF	= 0.005
					res@tmXBMinorLengthF		= 0.0
					res@tmYLMode			= "Explicit"
					res@tmYLValues			= fspan(-90.,90.,19)
					res@tmYLLabels			= (/"90S","80S","70S","60S","50S","40S",\
									    "30S","20S","10S", "EQ","10N","20N","30N",\
									    "40N","50N","60N","70N","80N","90N"/)
					res@tmYLLabelFontHeightF	= 0.009
					res@tmYLMajorLengthF		= 0.005
					res@tmYLMajorOutwardLengthF	= 0.005
					res@tmYLMinorLengthF		= 0.0
					res@tmXUseBottom		= False
					res@tmXTMajorLengthF		= 0.0
					res@tmXTMajorOutwardLengthF	= 0.0
					res@tmYUseLeft			= False
					res@tmYRMajorLengthF		= 0.0
					res@tmYRMajorOutwardLengthF	= 0.0
					;res@lbAutoManage		= False
					res@lbLabelFontHeightF		= 0.008
					res@lbLabelAutoStride		= False
					res@lbLabelStride		= 4
					res@lbOrientation		= "vertical"
					res@lbLabelOffsetF		= 0.02
					;res@pmLabelBarHeightF		= 0.1
					res@pmLabelBarWidthF		= 0.03
					res@pmLabelBarOrthogonalPosF	= 0.005
					;res@pmLabelBarParallelPosF	= -0.5
					res@tiMainFontHeightF		= 0.012
					res@tiMainOffsetYF		= -0.004
					res@gsnLeftStringFontHeightF	= 0.010
					;res@gsnLeftStringOrthogonalPosF	= -0.0020
					res@gsnRightStringFontHeightF	= 0.010
					;res@gsnRightStringOrthogonalPosF= -0.0000

		; CROSS SECTION PLOTTING RESOURCES
		else if(type.ge.10 .and. type.lt.20)then
					res@tmXBMode                    = "Explicit"
					res@tmXBLabelFontHeightF        = 0.012
                                        res@tmXBMajorLengthF            = 0.005
                                        res@tmXBMajorOutwardLengthF     = 0.005
                                        res@tmXBMinorLengthF            = 0.0
					res@tmXBLabelDeltaF		= -0.5
					;res@trYAxisType			= "LogAxis"
					res@trYLog			= True
					res@trYReverse			= True
                                        res@tmYLMode                    = "Explicit"
                                        res@tmYLValues                  = fspan(1000.,100.,19)
                                        res@tmYLLabels                  = (/"1000","","","850","","","700","","",\
                                                                            "","500","", "400","","300","","200","","100"/)
                                        res@tmYLLabelFontHeightF        = 0.012
                                        res@tmYLMajorLengthF            = 0.005
                                        res@tmYLMajorOutwardLengthF     = 0.005
                                        res@tmYLMinorLengthF            = 0.0
					res@tmYLLabelDeltaF             = -0.5
                                        res@tmXUseBottom                = False
                                        res@tmXTMajorLengthF            = 0.0
                                        res@tmXTMajorOutwardLengthF     = 0.0
                                        res@tmYUseLeft                  = False
                                        res@tmYRMajorLengthF            = 0.0
                                        res@tmYRMajorOutwardLengthF     = 0.0
                                        ;res@lbAutoManage               = False
                                        res@lbLabelFontHeightF         = 0.012
                                        res@lbLabelAutoStride           = False
                                        res@lbLabelStride               = 4
                                        res@lbOrientation               = "vertical"
                                        res@lbLabelOffsetF              = 0.02
                                        ;res@pmLabelBarHeightF          = 0.1
                                        res@pmLabelBarWidthF            = 0.03
                                        res@pmLabelBarOrthogonalPosF    = -0.018
                                        ;res@pmLabelBarParallelPosF     = -0.5
                                        res@tiMainFontHeightF           = 0.012
                                        res@tiMainOffsetYF              = -0.004
                                        res@gsnLeftStringFontHeightF    = 0.009
                                        res@gsnLeftStringOrthogonalPosF = -0.0025
                                        res@gsnRightStringFontHeightF   = 0.009
                                        res@gsnRightStringOrthogonalPosF= -0.001

		
		; POLAR CYLINDRICAL PLOTTING RESOURCES
		else if(type.ge.20 .and. type.lt.30)then
					res@gsnPolar			= "SH"                 ; North hemisphere
					res@gsnTickMarksOn		= False
					;res@gsnAddCyclic		= True
					res@mpFillOn			= False                ; Turn off map fill
					res@mpOutlineOn			= False                ; Turn off map outlines
					res@mpMaxLatF			= -70.                 ; specify min lat
					res@lbOrientation		= "vertical"
					res@lbLabelOffsetF		= 0.02
					;res@pmLabelBarHeightF		= 0.1
					res@pmLabelBarWidthF		= 0.03
					res@pmLabelBarOrthogonalPosF	= 0.05
					;res@pmLabelBarParallelPosF	= -0.5
					res@tiMainFontHeightF		= 0.012
					res@tiMainOffsetYF		= -0.004
					res@gsnLeftStringFontHeightF	= 0.009
					res@gsnLeftStringOrthogonalPosF	= -0.0025
					res@gsnRightStringFontHeightF	= 0.009
					res@gsnRightStringOrthogonalPosF= -0.001
		
		; R-Z PLOTTING RESOURCES
		else if(type.ge.30 .and. type.lt.40)then
					res@trYReverse			= True
					res@trYAxisType			= "LogAxis"
					res@tiXAxisString		= "Radius [km]"
					res@tiYAxisString		= "Pressure [hPa]"
					res@lbOrientation		= "vertical"
					res@lbLabelOffsetF		= 0.02
					;res@tiMainFontHeightF		= 0.012
					res@tiMainOffsetYF		= -0.004
					;res@pmLabelBarHeightF		= 0.1
					res@pmLabelBarWidthF		= 0.03
					res@pmLabelBarOrthogonalPosF	= 0.00
					;res@pmLabelBarParallelPosF	= -0.5
					res@tiMainFontHeightF		= 0.012
					res@tiMainOffsetYF		= -0.004
					res@gsnLeftStringFontHeightF	= 0.009
					res@gsnLeftStringOrthogonalPosF	= -0.0025
					res@gsnRightStringFontHeightF	= 0.009
					res@gsnRightStringOrthogonalPosF= -0.001
		
		; CARTESIAN (X,Y) PLOTTING RESOURCES
		else if(type.ge.40 .and. type.lt.50)then
					res@trGridType			= "TriangularMesh"
		
		; HODOGRAPH PLOTTING RESOURCES
		else if(type.ge.50 .and. type.lt.60)then
					res@tmXTOn			= False
					res@tmXBOn			= False
					res@tmYLOn			= False
					res@tmYROn			= False
					res@tmXBMajorLengthF		= 0.0
					res@tmXBMajorOutwardLengthF	= 0.0
					res@tmXBMinorLengthF		= 0.0
					res@tmXBMinorOutwardLengthF	= 0.0
					res@tmYLMajorLengthF		= 0.0
					res@tmYLMajorOutwardLengthF	= 0.0
					res@tmYLMinorLengthF		= 0.0
					res@tmYLMinorOutwardLengthF	= 0.0
					;res@tmXBLabelDeltaF		= -0.80
					;res@tmYLLabelDeltaF		= -0.80
					;res@tiXAxisString		= "Wind Speed [kts]"
					;res@tiYAxisString		= "Wind Speed [kts]"
					;res@tiMainFontHeightF		= 0.012
					res@tiMainOffsetYF		= -0.004
					res@tiMainFontHeightF		= 0.012
		end if  end if  end if  end if  end if
		end if
	end if

	
	; GENERAL PLOTTING RESOURCES FOR THE DATA ITSELF.
	; Should be independent of plot type except in rare cases
	
	; Contour Fill Only
	if(type2.eq.0)then		res@cnLinesOn			= False
					res@cnFillOn			= True
					res@cnFillMode			= "RasterFill"
					res@cnRasterSmoothingOn		= True
		

	; Contour Lines Only
	else if(type2.eq.1)then		;res@gsnAddCyclic		= OvCyc
					;res@gsnContourNegLineDashPattern= 1
					;res@gsnContourZeroLineThicknessF= 3.0
					res@cnLinesOn			= True
					res@cnFillOn			= False                 ; turn on color fill
					res@cnLineLabelInterval 	= 1
					res@cnLineThicknessF		= 5.5		;MKedit was 2.0
					res@cnLineLabelFontColor        = "saddlebrown"	;MKedit - add this to match cnLineColor
					res@cnLineColor			= "saddlebrown"
					res@cnInfoLabelOn		= False
					res@gsnRightString		= ""
					res@gsnLeftString		= ""
					res@cnLineLabelsOn		= True                 ; turn on line labels
					;res@cnLineLabelDensityF	= 0.5
					res@cnLineLabelBackgroundColor	= "transparent"
					res@lbLabelAutoStride		= True                 ; optimal labels
					res@cnLineLabelPlacementMode	= "constant"
					res@cnLineLabelFontHeightF	= .006 ;MKedit - .005
					;res@cnExplicitLineLabelsOn	= True
					;res@cnLineLabelStrings		= OVLabelBarStrings
	
	
	; Contour Fill & Lines
	else if(type2.eq.2)then		;res@gsnContourNegLineDashPattern= 1
					;res@gsnContourZeroLineThicknessF= 3.0
					res@cnLinesOn			= True
					res@cnFillOn			= True                 ; turn on color fill
					res@cnLineLabelInterval 	= 1
					res@cnLineThicknessF		= 1.
					res@cnLineColor			= "black"
					res@cnInfoLabelOn		= False
					res@gsnRightString		= ""
					res@gsnLeftString		= ""
					res@cnLineLabelsOn		= True                 ; turn on line labels
					;res@cnLineLabelDensityF	= 0.5
					res@cnLineLabelBackgroundColor	= "transparent"
					res@lbLabelAutoStride		= True                 ; optimal labels
					res@cnLineLabelPlacementMode	= "constant"
					res@cnLineLabelFontHeightF	= .007
					;res@cnExplicitLineLabelsOn	= True
					;res@cnLineLabelStrings		= OVLabelBarStrings
					res@cnFillMode			= "RasterFill"
					res@cnRasterSmoothingOn		= True
	
	
	; Wind Vectors
	else if(type2.eq.3)then		;res@vcRefMagnitudeF		= arrowMag	; define vector ref mag
					res@vcRefLengthF		= 0.05           ; define length of vec ref
					;res@vcRefAnnoOrthogonalPosF	= -1.0            ; move ref vector vertically
					;res@vcRefAnnoParallelPosF	= 0.08			; move ref vector horizontally
					res@vcRefAnnoArrowLineColor	= "black"         ; change ref vector color
					res@vcRefAnnoArrowUseVecColor	= False           ; don't use vec color for ref
					res@vcGlyphStyle		= "LineArrow"	; turn on curley vectors
					res@vcLineArrowThicknessF	= 1.5               ; change vector thickness
					res@vcMinDistanceF		= 0.02
					;res@vcMinFracLengthF		= 0.2
					;res@vcLineArrowHeadMaxSizeF	= 0.015
					res@vcLineArrowColor		= "black"
	
	
	; Curley Wind Vectors
	else if(type2.eq.4)then		;res@vcRefMagnitudeF		= arrowMag	; define vector ref mag
					res@vcRefLengthF		= 0.05           ; define length of vec ref
					;res@vcRefAnnoOrthogonalPosF	= -1.0            ; move ref vector vertically
					;res@vcRefAnnoParallelPosF	= 0.08			; move ref vector horizontally
					res@vcRefAnnoArrowLineColor	= "black"         ; change ref vector color
					res@vcRefAnnoArrowUseVecColor	= False           ; don't use vec color for ref
					res@vcRefAnnoOn			= False
					res@vcGlyphStyle		= "CurlyVector"	; turn on curley vectors
					res@vcLineArrowThicknessF	= 2.5               ; change vector thickness
					res@vcMinDistanceF		= 0.025
					;res@vcMinFracLengthF		= 0.2
					res@vcLineArrowHeadMaxSizeF	= 0.015
					res@vcLineArrowColor		= "black"
		
	
	; Wind Barbs
	else if(type2.eq.5)then		res@vcGlyphStyle		= "WindBarb"         ; select wind barbs 
					;res@vcRefMagnitudeF		= 10.                ; make vectors larger
					res@vcRefLengthF		= 0.018              ; ref vec length
					res@vcRefAnnoOn			= False
					res@vcMinDistanceF		= 0.02              ; thin out windbarbs
					res@vcWindBarbColor		= "black" ;"gray30"
					res@vcWindBarbTickLengthF	= 0.4                ; length of the barb tick
					res@vcWindBarbTickSpacingF	= 0.12                ; distance between ticks
					res@vcWindBarbLineThicknessF    = 2.5   ;MKedit 2.0 to 3.0
	
	; Streamlines
	else if(type2.eq.6)then		res@stLineThicknessF		= 2.0
					res@stLineColor			= "saddlebrown"
					res@stArrowLengthF		= 0.003
					res@stArrowStride		= 1
					res@stLineStartStride		= 1
					res@stStepSizeF			= 0.001
					res@stMinArrowSpacingF		= 0.01             
					res@stMinDistanceF		= 0.005
					res@stMinLineSpacingF		= 0.003
					;res@stMinStepFactorF		= 50.0
					res@stLengthCheckCount		= 50


	; Blank Contour Plot (No Map)
	else if(type2.eq.7)then		type = -1
	
	; Map Only
	else if(type2.eq.9)then		type			= -1
					res@mpOutlineDrawOrder		= "Draw"
					res@mpGridAndLimbDrawOrder	= "Draw"
					if(isatt(res,"lbLabelAutoStride"))then   delete(res@lbLabelAutoStride)   end if
					if(isatt(res,"lbLabelStride"))then       delete(res@lbLabelStride)       end if
					if(isatt(res,"lbLabelOffsetF"))then      delete(res@lbLabelOffsetF)      end if
		
	end if  end if  end if  end if  end if
	end if  end if  end if	end if

	
	return(res)
end



;
;================================================================
; getAutoDir

undef("getAutoDir")
function getAutoDir (SYS_ENV[1]:string,DSOURCE[1]:string,type[1]:string)
local AA
begin

	masterType	= (/"EXPT","IDIR","ATCFDIR"/)
	if(.not.any(masterType.eq.type))then
		print("ERROR: getAutoDir: Request for '"+type+"' not recognized.")
		exit
	end if
	
	
	masterSYS	= (/"JET","MAC"/)
	if(.not.any(masterSYS.eq.SYS_ENV))then
		print("ERROR: getAutoDir: System '"+SYS_ENV+"' not recognized.")
		exit
	end if
	if(SYS_ENV.eq."MAC")then
		print("ERROR: getAutoDir: Mac Environment not functional yet")
		exit
	end if
	
	
	masterSRC	= (/"HWRF","HWRF_d03","HWRF15","HWRF15_d03",\
			    "HWRF16","HWRF16_d03","HWRF16r1","HWRF16r1_d03",\
			    "HWRF16r2","HWRF16r2_d03","HWRF17","HWRF17_d03",\
			    "HWRF17EPS","HWRF17EPS_d03",\
			    "HWRF18","HWRF18_d03","HWRF18L","HWRF18L_d03","HWRF18LG","HWRF18LG_d03",\
			    "HWRF18LS","HWRF18LS_d03","HWRF18W","HWRF18W_d03",\
			    "GFS","GFS_d03","fvGFS_ATL","fvGFS_ATL_d03",\
			    "H18L","HB17","HB16","HB16r1","HB16r2","HB15","H8LG","H8LS","H18W","H7BE"/)
	if(.not.any(masterSRC.eq.DSOURCE))then
		print("ERROR: getAutoDir: Data Source '"+DSOURCE+"' not recognized.")
		exit
	end if
	
	
	if(SYS_ENV.eq."JET")then	
		if(any((/"HWRF","HWRF_d03"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "hwrf-basinscale_multistorms-rev4_2015_realtime"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/"
			end if  end if  end if
		else if(any((/"HWRF15","HWRF15_d03","HB15"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "hwrf-basinscale_multistorms-rev4_2015_realtime"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2015-rev4_realtime/hwrf-basinscale_multistorms-rev4_2015_realtime/"
			end if  end if  end if
		else if(any((/"HWRF16","HWRF16_d03","HB16"/).eq.DSOURCE))then
		        if(type.eq."EXPT")then
				AA           = "hwrf-basinscale_multistorms-rev1_2016_realtime"
			else if(type.eq."IDIR")then
				AA           = "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/"
			end if  end if  end if
		else if(any((/"HWRF16r1","HWRF16r1_d03","HB16r1"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "hwrf-basinscale_multistorms-rev1_2016_history"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/pytmp_2016-rev1_history/hwrf-basinscale_multistorms-rev1_2016_history/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/rthr-aoml/noscrub_2016-rev1_realtime/hwrf-basinscale_multistorms-rev1_2016_realtime/"
			end if  end if  end if
		else if(any((/"HWRF16r2","HWRF16r2_d03","HB16r2"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "hwrf-basinscale_multistorms-rev2_2016_history"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/Mu-Chieh.Ko/pytmp_2016-rev2_history/hwrf-basinscale_multistorms-rev2_2016_history/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Mu-Chieh.Ko/noscrub_2016-rev2_realtime/hwrf-basinscale_multistorms-rev2_2016_realtime/"
			end if  end if  end if
		else if(any((/"HWRF17","HWRF17_d03","HB17"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				;AA	= "H217_Test01"
				AA	= "HB17_v1_forecast"
				;AA	= "HB17_v1_history"
			else if(type.eq."IDIR")then
				AA	= "/lfs3/projects/hur-aoml/rthr-aoml/pytmp/HB17_v1_forecast/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs3/projects/hur-aoml/rthr-aoml/noscrub/HB17_v1_forecast/"
			end if  end if  end if
		else if(any((/"HWRF17EPS","HWRF17EPS_d03","H7BE"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H217_EPS_v1"
			else if(type.eq."IDIR")then
				AA	= "/lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/H217_EPS_v1/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/H217_EPS_v1/"
			end if  end if  end if
		else if(any((/"HWRF18","HWRF18_d03","H18L"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H18L"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H18L/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H18L/"
			end if  end if  end if
		else if(any((/"HWRF18L","HWRF18L_d03","H18L"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H18L"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H18L/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H18L/"
			end if  end if  end if
		else if(any((/"HWRF18LG","HWRF18LG_d03","H8LG"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H8LG"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H8LG/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H8LG/"
			end if  end if  end if
		else if(any((/"HWRF18LS","HWRF18LS_d03","H8LS"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H8LS"
			else if(type.eq."IDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/H8LS/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/H8LS/"
			end if  end if  end if
		else if(any((/"HWRF18W","HWRF18W_d03","H18W"/).eq.DSOURCE))then
			if(type.eq."EXPT")then
				AA	= "H18W"
			else if(type.eq."IDIR")then
				AA	= "/lfs1/projects/hur-aoml/Ghassan.Alaka/pytmp/H18W/com/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs1/projects/hur-aoml/Ghassan.Alaka/noscrub/H18W/"
			end if  end if  end if
		else if(any((/"GFS","GFS_d03"/).eq.DSOURCE))then
		        if(type.eq."EXPT")then
				AA    = "GFS_Forecast"
			else if(type.eq."IDIR")then
			        AA    = "/lfs2/projects/hur-aoml/Ghassan.Alaka/GPLOT/GFSdata/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/GFS_Forecast/"
			end if  end if  end if
		else if(any((/"fvGFS_ATL","fvGFS_ATL_d03"/).eq.DSOURCE))then
		        if(type.eq."EXPT")then
				AA    = "fvGFS_ATL"
			else if(type.eq."IDIR")then
			        AA    = "/lfs2/projects/hur-aoml/Ghassan.Alaka/pytmp/fvGFS_ATL/"
			else if(type.eq."ATCFDIR")then
				AA	= "/lfs2/projects/hur-aoml/Ghassan.Alaka/noscrub/fvGFS_ATL/"
			end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if
	end if
	
	return(AA)

end



;
;================================================================
; getDmnBds

undef("getDmnBds")
function getDmnBds (nameBds[*]:string,dSource[1]:string)
local allBds, nBds, masterBds, bbb
begin

	nBds	= dimsizes(nameBds)
	allBds	= new((/nBds,4/),"float")

	nameBds	= str_lower(nameBds)

	masterBds	= (/"hwrf",    "basin",\
			    "atl",     "wlant",\
			    "epac",    "cpac",\
			    "gom",\
			    "d03",     "bob",\
			    "nio",     "data",\
			    "eus",     "fvgfs",\
			    "elant",   "bigd01",\
			    "d01",     "d02",\
			    "tkfull",  "ships",\
                	    "carolina","global"/)

	do bbb = 0,nBds-1
		if(.not.any(masterBds.eq.nameBds(bbb)))then   continue   end if

		if(nameBds(bbb).eq."basin")then		allBds(bbb,:) = (/50.,-15.,-160.,   0./) end if
		if(nameBds(bbb).eq."fvgfs")then		allBds(bbb,:) = (/55.,  5.,-100.,   0./) end if
		if(nameBds(bbb).eq."atl")then		allBds(bbb,:) = (/50.,  0.,-110.,   0./) end if
		if(nameBds(bbb).eq."wlant")then		allBds(bbb,:) = (/40.,  5.,-110., -50./) end if
		if(nameBds(bbb).eq."elant")then		allBds(bbb,:) = (/35.,  5., -60.,   0./) end if
		if(nameBds(bbb).eq."epac")then		allBds(bbb,:) = (/35.,  0.,-140., -80./) end if
		if(nameBds(bbb).eq."cpac")then		allBds(bbb,:) = (/35.,  0.,-180.,-140./) end if
		if(nameBds(bbb).eq."gom")then		allBds(bbb,:) = (/35., 10.,-105., -70./) end if
		if(nameBds(bbb).eq."eus")then		allBds(bbb,:) = (/45., 20., -85., -60./) end if
		if(nameBds(bbb).eq."bob")then		allBds(bbb,:) = (/40.,  5.,  65., 100./) end if
		if(nameBds(bbb).eq."nio")then		allBds(bbb,:) = (/30.,  0.,  50., 100./) end if
		if(nameBds(bbb).eq."bigd01")then	allBds(bbb,:) = (/70.,-40.,-200.,  40./) end if
		if(nameBds(bbb).eq."carolina")then	allBds(bbb,:) = (/41., 31., -84., -74./) end if
		
		; Set these to zero, which means take the entire domain given
		if(nameBds(bbb).eq."hwrf")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."d01")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."d02")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."d03")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."tkfull")then	allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."data")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."ships")then		allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
		if(nameBds(bbb).eq."global")then	allBds(bbb,:) = (/ 0.,  0.,   0.,  0./)   end if
	end do

	if(nBds.eq.1)then   allBds := allBds(0,:)   end if
	
	return(allBds)

end



;
;================================================================
; getDmnInfo

undef("getDmnInfo")
function getDmnInfo (DOMAIN[1]:string,INFO[1]:string,DO_VALUE[1]:logical)
local MASTER_DOMAIN, MASTER_INFO, OUT
begin


	MASTER_INFO	= (/"bdstype","maxH","maxL","llbox"/)
	
	MASTER_DOMAIN	= (/"hwrf",  "basin",\
			    "atl",   "wlant",\
			    "epac",  "cpac",\
			    "gom",\
			    "d03",   "bob",\
			    "nio",   "data",\
			    "eus",   "fvgfs",\
			    "elant", "bigd01",\
			    "d01",   "d02",\
			    "tkfull", "ships",\
			    "global"/)
	if(.not.any(MASTER_DOMAIN.eq.DOMAIN))then
		print("ERROR: getDmnInfo: DOMAIN '"+DOMAIN+"' not recognized. Assigning defaults.")
	end if


	; Set the boundary type to properly find the domain
	; in which the data is extracted.
	if(INFO.eq."bdstype")then
		if(     DOMAIN.eq."basin")then	OUT = 0
		else if(DOMAIN.eq."fvgfs")then	OUT = 0
		else if(DOMAIN.eq."atl")then	OUT = 0
		else if(DOMAIN.eq."wlant")then	OUT = 0
		else if(DOMAIN.eq."elant")then	OUT = 0
		else if(DOMAIN.eq."epac")then	OUT = 0
		else if(DOMAIN.eq."cpac")then	OUT = 0
		else if(DOMAIN.eq."gom")then	OUT = 0
		else if(DOMAIN.eq."eus")then	OUT = 0
		else if(DOMAIN.eq."bob")then	OUT = 0
		else if(DOMAIN.eq."nio")then	OUT = 0
		else if(DOMAIN.eq."bigd01")then	OUT = 0
		else if(DOMAIN.eq."hwrf")then	OUT = 0
		else if(DOMAIN.eq."d01")then	OUT = 0
		else if(DOMAIN.eq."d02")then	OUT = 2
		else if(DOMAIN.eq."d03")then	OUT = 3
		else if(DOMAIN.eq."tkfull")then	OUT = 4
		else if(DOMAIN.eq."ships")then	OUT = 7
		else if(DOMAIN.eq."global")then OUT = 1
		else if(DO_VALUE)then		OUT = 6
		else				OUT = 0
		end if  end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if  end if
		end if  end if
	; Set the maximum number of High pressure markers
	else if(INFO.eq."maxH")then
		if(     DOMAIN.eq."basin")then	OUT = 10
		else if(DOMAIN.eq."bigd01")then	OUT = 10
		else if(DOMAIN.eq."d01")then	OUT = 10
		else if(DOMAIN.eq."atl")then	OUT = 10
		else if(DOMAIN.eq."wlant")then	OUT = 10
		else if(DOMAIN.eq."elant")then	OUT = 10
		else if(DOMAIN.eq."epac")then	OUT = 10
		else if(DOMAIN.eq."cpac")then	OUT = 10
		else if(DOMAIN.eq."tkfull")then	OUT = 10
		else if(DOMAIN.eq."d02")then	OUT = 10
		else if(DOMAIN.eq."d03")then	OUT = 2
		else if(DOMAIN.eq."global")then OUT = 25
		else				OUT = 10
		end if  end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if  end if
	
	; Set the maximum number of Low pressure markers
	else if(INFO.eq."maxL")then
		if(     DOMAIN.eq."basin")then	OUT = 12
		else if(DOMAIN.eq."bigd01")then	OUT = 12
		else if(DOMAIN.eq."d01")then	OUT = 12
		else if(DOMAIN.eq."atl")then	OUT = 12
		else if(DOMAIN.eq."wlant")then	OUT = 12
		else if(DOMAIN.eq."elant")then	OUT = 12
		else if(DOMAIN.eq."epac")then	OUT = 12
		else if(DOMAIN.eq."cpac")then	OUT = 12
		else if(DOMAIN.eq."tkfull")then	OUT = 12
		else if(DOMAIN.eq."d02")then	OUT = 12
		else if(DOMAIN.eq."d03")then	OUT = 2
		else if(DOMAIN.eq."global")then OUT = 25
		else				OUT = 12
		end if  end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if  end if
	
	; Set the minimum lat/lon box for each High/Low MSLP marker
	; This "thins" the markers
	else if(INFO.eq."llbox")then
		if(     DOMAIN.eq."basin")then	OUT = 15.
		else if(DOMAIN.eq."bigd01")then	OUT = 15.
		else if(DOMAIN.eq."d01")then	OUT = 10.
		else if(DOMAIN.eq."atl")then	OUT = 10.
		else if(DOMAIN.eq."wlant")then	OUT = 7.5
		else if(DOMAIN.eq."elant")then	OUT = 7.5
		else if(DOMAIN.eq."epac")then	OUT = 7.5
		else if(DOMAIN.eq."cpac")then	OUT = 7.5
		else if(DOMAIN.eq."tkfull")then	OUT = 7.5
		else if(DOMAIN.eq."d02")then	OUT = 7.5
		else if(DOMAIN.eq."d03")then	OUT = 2.5
		else if(DOMAIN.eq."global")then OUT = 20.
		else				OUT = 10.
		end if  end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if  end if

	end if  end if  end if  end if

	return(OUT)

end




;================================================================
; getExptInfo
;
; res - Input is existing NCL resources variable
; dydx - 2 element array giving DY and DX, in degrees
undef("getExptInfo")
function getExptInfo (EXPTS[*]:string, infoType[1]:string)
begin
	masterInfo=asciiread(GPLOT+"/tbl/ExptInfo.dat",-1,"string")
	masterExpt=str_squeeze(str_get_field(masterInfo(:),1,","))
	masterType=(/"expt","title"/)

	if(.not.any(masterType.eq.infoType))then
		print("ERROR: getExptInfo: infoType '"+infoType+"' not recognized.")
		exit
	else
		typeind=str_match_ind(masterType,infoType)+1 ;str_get_field starts at 1
	end if

	N = dimsizes(EXPTS)
	vartype="string"  ;Only string for now. If modification needed in the future, check getModelInfo.
	EXPTINFO=new(N,vartype)

	do aaa = 0, N-1
		EXPT=EXPTS(aaa)
		if(.not.any(masterExpt.eq.EXPT))then

		print("WARNING: getExptInfo: EXPT '"+EXPT+"' not recognized. Assigning missing value.")

		else

		exptind=str_match_ind(masterExpt,EXPT)
		EXPTINFO(aaa)=str_squeeze(str_get_field(masterInfo(exptind),typeind,","))	
		
		end if
	end do
	return(EXPTINFO)
end

undef("getExptInfo2")
function getExptInfo2 (EXPTS[*]:string, infoType[1]:string)
local master_Type, master_EXPT, N, aaa, EXPT, EXPTINFO
begin

	; All possible values of "infoType"
	master_Type	= (/"color","marker","title"/)
	if(.not.any(master_Type.eq.infoType))then
		print("ERROR: getExptInfo: infoType '"+infoType+"' not recognized.")
		exit
	end if
	
	; Not currently used
	if(isatt(infoType,"IS_MSTORM"))then	IS_MSTORM = infoType@IS_MSTORM
	else					IS_MSTORM = True
	end if
	
	; All possible values of "MODELS"
	master_EXPT	= (/"HB17_v1_forecast","HB17_v1_history",\
					"H217_sensitivity","H217_sensitivity_irma",\
					"H217_sensitivity_irma_jose","H217_sensitivity_irma_katia",\
			    "GFS_Forecast",\
			    "H18W","HP2H","HP2F","HP3H","H218","Test.MK"/)
	
	; How many models?
	N = dimsizes(EXPTS)
	if(infoType.eq."color")then		EXPTINFO = new(N,"string")
	else if(infoType.eq."marker")then	EXPTINFO = new(N,"integer")
	else if(infoType.eq."title")then	EXPTINFO = new(N,"string")
	end if  end if  end if
	
	; Loop over all models (Could be 1)
	do aaa = 0,N-1
		
		EXPT = EXPTS(aaa)
		
		; Check if this particular MODEL is supported
		if(.not.any(master_EXPT.eq.EXPT))then
			print("WARNING: getExptInfo: EXPT '"+EXPT+"' not recognized. Assigning missing value.")
		end if
		
		
		; This is a table of the relevant Model Info, separated by Data Source
		if(     EXPT.eq."HB17_v1_forecast")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2017 Basin-Scale HWRF"
			end if  end if  end if
		else if(EXPT.eq."HB17_v1_history")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2017 Basin-Scale HWRF"
			end if  end if  end if
		else if(EXPT.eq."Test.MK")then
                        if(infoType.eq."color")then             EXPTINFO(aaa) = "brown"
                        else if(infoType.eq."marker")then       EXPTINFO(aaa) = 7
                        else if(infoType.eq."title")then        EXPTINFO(aaa) = "2017 Basin-Scale HWRF"
                        end if  end if  end if
		else if(EXPT.eq."GFS_Forecast")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "blue"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 4
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "Global Forecast System"
			end if  end if  end if
		else if(EXPT.eq."H218")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2018 Oper. HWRF"
			end if  end if  end if
		else if(EXPT.eq."H18W")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2018 HWRF Test-W"
			end if  end if  end if
		else if(EXPT.eq."HP2H")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2018 HWRF Test-2H"
			end if  end if  end if
		else if(EXPT.eq."HP3H")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2018 HWRF Test-3H"
			end if  end if  end if
		else if(EXPT.eq."HP2F")then
			if(infoType.eq."color")then		EXPTINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	EXPTINFO(aaa) = 7
			else if(infoType.eq."title")then	EXPTINFO(aaa) = "2018 HWRF Test-2F"
			end if  end if  end if
        else if(EXPT.eq."H217_sensitivity")then
            if(infoType.eq."color")then     EXPTINFO(aaa) = "brown"
            else if(infoType.eq."marker")then   EXPTINFO(aaa) = 7
            else if(infoType.eq."title")then    EXPTINFO(aaa) = "2017 Basin-Scale HWRF Test-A"
            end if  end if  end if
        else if(EXPT.eq."H217_sensitivity_irma")then
            if(infoType.eq."color")then     EXPTINFO(aaa) = "brown"
            else if(infoType.eq."marker")then   EXPTINFO(aaa) = 7
            else if(infoType.eq."title")then    EXPTINFO(aaa) = "2017 Basin-Scale HWRF Test-B"
            end if  end if  end if
        else if(EXPT.eq."H217_sensitivity_irma_jose")then
            if(infoType.eq."color")then     EXPTINFO(aaa) = "brown"
            else if(infoType.eq."marker")then   EXPTINFO(aaa) = 7
            else if(infoType.eq."title")then    EXPTINFO(aaa) = "2017 Basin-Scale HWRF Test-C"
            end if  end if  end if
        else if(EXPT.eq."H217_sensitivity_irma_katia")then
            if(infoType.eq."color")then     EXPTINFO(aaa) = "brown"
            else if(infoType.eq."marker")then   EXPTINFO(aaa) = 7
            else if(infoType.eq."title")then    EXPTINFO(aaa) = "2017 Basin-Scale HWRF Test-D"
            end if  end if  end if
		end if  end if  end if  end if
		end if  end if  end if  end if
		end if  end if  end if  end if end if
		
	end do
	
	; Return Requested Model Info
	return(EXPTINFO)
end

;
;================================================================
; getIDIR
; Due to varies possibility of Input Directory(IDIR)
undef("getIDIR")
function getIDIR (IDIR[1]:string,EXPT[1]:string,IDATE[1]:string,SID[1]:string,ENSID[1]:string)
local DIR
begin

	DIR=IDIR

	Resources=(/EXPT,ENSID,"com",IDATE,SID/)
	Resources:=Resources(ind(Resources.ne.""))
	D=dimsizes(Resources) 


	times=0
	maxtime=D
	do while (D.gt.0.and.times.lt.maxtime)
;	print("MKMSG: getIDIR: Resources # = "+D+" Tried Times = "+times)
	do i=0,D-1
		temp:=systemfunc("ls "+DIR) 
		ires=Resources(i)

		if (.not.ismissing(ind(temp.eq.ires))) then 
			DIR=DIR+ires+"/"
;			print("MKMSG: getIDIR: Find "+ires+" Current DIR= "+DIR)

			if (D.gt.1) then
				Resources:=Resources(ind(Resources.ne.ires))
				D=dimsizes(Resources)
;				print("MKMSG: getIDIR: remained resources "+Resources)
			else
				Resources:=""
				D=0
;				print("MKMSG: getIDIR: No remained resources. Function End.")
			end if

			break 
		end if
	end do
	times=times+1
	end do
	
	returns=DIR
;	print("MKMSG: getIDIR: Unused Resources - "+Resources)
	return(returns)

end


;
;================================================================
; getFileTag

undef("getFileTag")
function getFileTag (IDIR[1]:string,DSOURCE[1]:string,EXPT[1]:string,DOMAIN[1]:string,\
                      IDATE[1]:string,SID[1]:string,FHR[1]:string,ITAG[1]:string,\
                      OCFLAG[1]:integer,EXT[1]:string,ENSID[1]:string)
local MASTER_SRC, IFILE, IDIR2, ITAG2, ENS_tag
begin

        MASTER_SRC      = (/    "HWRF",\
                              "HWRF15",\
                              "HWRF16",\
                              "HWRF17",\
                              "HWRF18",\
                                "NMMB",\
                                 "GFS",\
                           "fvGFS_ATL"/)

        if(.not.any(MASTER_SRC.eq.DSOURCE))then
                print("ERROR: getFileTag: Data Source "+DSOURCE+" not found.")
                exit
        end if

        if(ENSID.eq."")then  ENS_tag = ""
        else                 ENS_tag = ENSID+"/"
        end if


        ; Options for the input directory structure
        ; It may include IDATE, SID, both, or neither

;	MASTERIDIR= (/	IDIR  ,\   ; MK: IDIR is where we have iFiles
;			IDIR+"com/"+ENS_tag+IDATE+"/"+SID+"/",\ ; MK: IDIR is DATA/EXPT/
;			IDIR+ENS_tag+IDATE+"/"+SID+"/",  \ ; MK: IDIR is DATA/EXPT/com/
;			IDIR+ENS_tag+SID+"/"+IDATE+"/", \
;			IDIR+ENS_tag+SID+"/" , \
;			IDIR+ENS_tag+"com/"+IDATE+"/" ,\
;			IDIR+ENS_tag+"com/"+IDATE+"/"+SID+"/" ,\
;			IDIR+ENS_tag+"com/"+SID+"/"+IDATE+"/" ,\
;			IDIR+ENS_tag+"com/"+SID+"/" ,\
;			IDIR+ENS_tag+"com/" ,\
;			IDIR+EXPT+"/" ,\
;			IDIR+EXPT+"/"+ENS_tag+"com/"+IDATE+"/" ,\
;			IDIR+EXPT+"/"+ENS_tag+"com/"+IDATE+"/"+SID+"/" ,\
;			IDIR+EXPT+"/"+ENS_tag+"com/"+SID+"/"+IDATE+"/" ,\
;			IDIR+EXPT+"/"+ENS_tag+"com/"+SID+"/" ,\
;			IDIR+EXPT+"/"+ENS_tag+"com/" ,\
;			IDIR+EXPT+"/"+ENS_tag+EXPT+"/" ,\
;			IDIR+ENS_tag /)

	IDIR2=IDIR ;MASTERIDIR(LEVEL)

                ;DOMAIN:   		else			,			d02			,			d03			   ;MASTER_SRC
        MASTERTAG=(/	(/"*hwrfprs.global.0p25."+FHR+".grb2"	,	"*hwrfprs.synoptic.0p125."+FHR+".grb2"	,	"*hwrfprs.storm.0p02."+FHR+".grb2"	/),\  ;HWRF
			(/"*hwrfprs.d1.0p25."+FHR+".grb2"	,	"*hwrfprs.d1.0p25."+FHR+".grb2"		,	"*hwrfprs.d3.0p03."+FHR+".grb2"		/),\  ;HWRF15
			(/"*hwrfprs.global.0p25."+FHR+".grb2"	,	"*hwrfprs.synoptic.0p125."+FHR+".grb2"	,	"*hwrfprs.storm.0p02."+FHR+".grb2"	/),\  ;HWRF16
			(/"*hwrfprs.global.0p25."+FHR+".grb2"	,	"*hwrfprs.synoptic.0p125."+FHR+".grb2"	,	"*hwrfprs.storm.0p02."+FHR+".grb2"	/),\  ;HWRF17
			(/"*hwrfprs.global.0p25."+FHR+".grb2"	,	"*hwrfprs.synoptic.0p125."+FHR+".grb2"	,	"*hwrfprs.storm.0p015."+FHR+".grb2"	/),\  ;HWRF18
			(/"*nmbprs_d01."+FHR+".nc"		,	"*nmbprs_d01."+FHR+".nc"		,	"*nmbprs_d01."+FHR+".nc"		/),\  ;NMMB
			(/"gfs.t[0-9][0-9]z.pgrb2.0p25."+FHR+".grb2",	"gfs.t[0-9][0-9]z.pgrb2.0p25."+FHR+".grb2",	"gfs.t[0-9][0-9]z.pgrb2.0p25."+FHR+".grb2"/), \  ;GFS
			(/"atmos_sos.nest02_nested_ltd.nc.nc"	,	"atmos_sos.nest02_nested_ltd.nc.nc"	,	"atmos_sos.nest02_nested_ltd.nc.nc"	/)	/)  ;fvGFS_ATL

        if(ITAG.eq."")then
		IND0=ind(DSOURCE.eq.MASTER_SRC)
		if (DOMAIN.eq."d03") then
			IND1=2
		else if (DOMAIN.eq."d02") then
			IND1=1
		else 
			IND1=0
		end if end if
		
		ITAG2=MASTERTAG(IND0,IND1)

        else if(ITAG.ne."" .and. EXT.eq."")
                ITAG2 = "*"+ITAG+"*"+FHR+"*.grb2"
        else
                ITAG2 = "*"+ITAG+"*"+FHR+"*"+EXT
        end if  end if

        IFILE = IDIR2+ITAG2

        return(IFILE)

end


;
;================================================================
; getFileTag1

undef("getFileTag1")
function getFileTag1 (dSource[1]:string,iDir[1]:string,iDate[1]:string,iStorm[1]:string,ITAG[1]:string)
local masterSrc, iFile
begin

	masterSrc	= (/    "HWRF",\
			      "HWRF15",\
			      "HWRF16",\
			    "HWRF16r1",\
			    "HWRF16r2",\
			    "HWRF_d03",\
			  "HWRF15_d03",\
			  "HWRF16_d03",\
			"HWRF16r1_d03",\
			"HWRF16r2_d03",\
			      "HWRF17",\
			  "HWRF17_d03",\
			      "HWRF18",\
			  "HWRF18_d03",\
			     "HWRF18L",\
			 "HWRF18L_d03",\
			    "HWRF18LG",\
			"HWRF18LG_d03",\
			    "HWRF18LS",\
			"HWRF18LS_d03",\
			     "HWRF18W",\
			 "HWRF18W_d03",\
			        "NMMB",\
				 "GFS",\
			     "GFS_d03",\
			   "fvGFS_ATL",\
		       "fvGFS_ATL_d03"/)

	if(.not.any(masterSrc.eq.dSource))then
		print("ERROR: getFileTag: Data Source "+dSource+" not found.")
		exit
	end if

	if(dSource.eq."HWRF")then
		if(HWRFtag.eq."")then
			;iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d123.0p25.*.grb2"
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d1.0p25.*.grb2"
			;iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d1.0p20.*.grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"*.grb2"
		end if
	else if(any((/"HWRF16","HWRF16r1","HWRF16r2","HWRF17","HWRF18","HWRF18L","HWRF18LG","HWRF18LS","HWRF18W"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.global.0p25.*.grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"*.grb2"
		end if
	else if(dSource.eq."HWRF_d03")then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d3.0p03.*.grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"*.grb2"
		end if
	else if(any((/"HWRF16_d03","HWRF16r1_d03","HWRF16r2_d03","HWRF17_d03","HWRF18_d03","HWRF18L_d03","HWRF18LG_d03","HWRF18LS_d03","HWRF18W_d03"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.storm.0p02.*.grb2"
			;iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.core.0p02.*.grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"*.grb2"
		end if
	else if(dSource.eq."NMMB")then
		iFile		= iDir+"nmbprs_d01.*"
	else if(any((/"GFS","GFS_d03"/).eq.dSource))then
		iFile		= iDir+iDate+"/gfs*0p25*grb2"
	else if(any((/"fvGFS_ATL","fvGFS_ATL_d03"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/atmos_sos.nest02_nested_ltd.nc"
		else
			iFile		= iDir+iDate+"/"+ITAG+".nc"
		end if
	end if  end if  end if  end if  end if  end if  end if

	return(iFile)

end



;
;================================================================
; getFileTag2

undef("getFileTag2")
function getFileTag2 (dSource[1]:string,iDir[1]:string,iDate[1]:string,iStorm[1]:string,FHR[1]:string,ITAG[1]:string)
local masterSrc, iFile
begin

	masterSrc	= (/    "HWRF",\
			      "HWRF15",\
			      "HWRF16",\
			    "HWRF16r1",\
			    "HWRF16r2",\
			    "HWRF_d03",\
			  "HWRF15_d03",\
			  "HWRF16_d03",\
			"HWRF16r1_d03",\
			"HWRF16r2_d03",\
			      "HWRF17",\
			  "HWRF17_d03",\
			      "HWRF18",\
			  "HWRF18_d03",\
			     "HWRF18L",\
			 "HWRF18L_d03",\
			    "HWRF18LG",\
			"HWRF18LG_d03",\
			    "HWRF18LS",\
			"HWRF18LS_d03",\
			     "HWRF18W",\
			 "HWRF18W_d03",\
			        "NMMB",\
				 "GFS",\
			   "fvGFS_ATL",\
		       "fvGFS_ATL_d03"/)

	if(.not.any(masterSrc.eq.dSource))then
		print("ERROR: getFileTag: Data Source "+dSource+" not found.")
		exit
	end if

	if(dSource.eq."HWRF")then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d1.0p25."+FHR+".grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"."+FHR+".grb2"
		end if
	else if(any((/"HWRF16","HWRF16r1","HWRF16r2","HWRF17","HWRF18","HWRF18L","HWRF18LG","HWRF18LS","HWRF18W"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.global.0p25."+FHR+".grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"*"+FHR+".grb2"
		end if
	else if(dSource.eq."HWRF_d03")then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.d3.0p03."+FHR+".grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"."+FHR+".grb2"
		end if
	else if(any((/"HWRF16_d03","HWRF16r1_d03","HWRF16r2_d03","HWRF17_d03","HWRF18_d03","HWRF18L_d03","HWRF18LG_d03","HWRF18LS_d03","HWRF18W_d03"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/"+iStorm+"/*hwrfprs.storm.0p02."+FHR+".grb2"
		else
			iFile		= iDir+iDate+"/"+iStorm+"/*"+ITAG+"."+FHR+".grb2"
		end if
	else if(dSource.eq."NMMB")then
		iFile		= iDir+"nmbprs_d01.*"
	else if(dSource.eq."GFS")then
		iFile		= iDir+iDate+"/gfs*0p25."+FHR+".grb2"
	else if(any((/"fvGFS_ATL","fvGFS_ATL_d03"/).eq.dSource))then
		if(ITAG.eq."")then
			iFile		= iDir+iDate+"/atmos_sos.nest02_nested_ltd.nc.nc"
		else
			iFile		= iDir+iDate+"/"+ITAG+".nc"
		end if
	end if  end if  end if  end if  end if  end if  end if

	return(iFile)

end



;
;================================================================
; getFileTag3

undef("getFileTag3")
function getFileTag3 (IDIR[1]:string,DSOURCE[1]:string,EXPT[1]:string,DOMAIN[1]:string,\
		      IDATE[1]:string,SID[1]:string,FHR[1]:string,ITAG[1]:string,\
		      LEVEL[1]:integer,EXT[1]:string,ENSID[1]:string)
local MASTER_SRC, IFILE, IDIR2, ITAG2, ENS_tag
begin

	MASTER_SRC	= (/    "HWRF",\
			      "HWRF15",\
			      "HWRF16",\
			      "HWRF17",\
			      "HWRF18",\
			       "HEDAS",\
			        "NMMB",\
				 "GFS",\
			   "fvGFS_ATL"/)

	if(.not.any(MASTER_SRC.eq.DSOURCE))then
		print("ERROR: getFileTag: Data Source "+DSOURCE+" not found.")
		exit
	end if
	
	if(ENSID.eq."")then  ENS_tag = ""
	else		     ENS_tag = ENSID+"/"
	end if
	
	
	; Options for the input directory structure
	; It may include IDATE, SID, both, or neither
	if(LEVEL.eq.17)then		IDIR2 = IDIR+ENS_tag
	else if(LEVEL.eq.16)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+EXPT+"/"
	else if(LEVEL.eq.15)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+"com/"
	else if(LEVEL.eq.14)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+"com/"+SID+"/"
	else if(LEVEL.eq.13)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+"com/"+SID+"/"+IDATE+"/"
	else if(LEVEL.eq.12)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+"com/"+IDATE+"/"+SID+"/"
	else if(LEVEL.eq.11)then	IDIR2 = IDIR+EXPT+"/"+ENS_tag+"com/"+IDATE+"/"
	else if(LEVEL.eq.10)then	IDIR2 = IDIR+EXPT+"/"
	else if(LEVEL.eq.9)then		IDIR2 = IDIR+ENS_tag+"com/"
	else if(LEVEL.ge.8)then		IDIR2 = IDIR+ENS_tag+"com/"+SID+"/"
	else if(LEVEL.eq.7)then		IDIR2 = IDIR+ENS_tag+"com/"+SID+"/"+IDATE+"/"
	else if(LEVEL.eq.6)then		IDIR2 = IDIR+ENS_tag+"com/"+IDATE+"/"+SID+"/"
	else if(LEVEL.eq.5)then		IDIR2 = IDIR+ENS_tag+"com/"+IDATE+"/"
	else if(LEVEL.ge.4)then		IDIR2 = IDIR+ENS_tag+SID+"/"
	else if(LEVEL.eq.3)then		IDIR2 = IDIR+ENS_tag+SID+"/"+IDATE+"/"
	else if(LEVEL.eq.2)then		IDIR2 = IDIR+IDATE+"/"+ENS_tag+SID+"/"   ;MK 
	else if(LEVEL.eq.1)then		IDIR2 = IDIR+IDATE+"/"+ENS_tag		 ;MK
	else if(LEVEL.eq.0)then		IDIR2 = IDIR
	end if  end if  end if  end if  end if
	end if  end if  end if  end if  end if
	end if  end if  end if  end if  end if
	end if  end if  end if


	if(ITAG.eq."")then
		if(DSOURCE.eq."HWRF")then
			if(DOMAIN.eq."d03")then		ITAG2 = "*hwrfprs.storm.0p02."+FHR+".grb2"
			else if(DOMAIN.eq."d02")then	ITAG2 = "*hwrfprs.synoptic.0p125."+FHR+".grb2"
			else				ITAG2 = "*hwrfprs.global.0p25."+FHR+".grb2"
			end if  end if
		else if(any((/"HWRF18"/).eq.DSOURCE))then
			if(DOMAIN.eq."d03")then		ITAG2 = "*hwrfprs.storm.0p015."+FHR+".grb2"
			else if(DOMAIN.eq."d02")then	ITAG2 = "*hwrfprs.synoptic.0p125."+FHR+".grb2"
			else				ITAG2 = "*hwrfprs.global.0p25."+FHR+".grb2"
			end if  end if
		else if(any((/"HWRF16","HWRF17"/).eq.DSOURCE))then
			if(DOMAIN.eq."d03")then		ITAG2 = "*hwrfprs.storm.0p02."+FHR+".grb2"
			else if(DOMAIN.eq."d02")then	ITAG2 = "*hwrfprs.synoptic.0p125."+FHR+".grb2"
			else				ITAG2 = "*hwrfprs.global.0p25."+FHR+".grb2"
			end if  end if
		else if(DSOURCE.eq."HWRF15")then
			if(DOMAIN.eq."d03")then		ITAG2 = "*hwrfprs.d3.0p03."+FHR+".grb2"
			else				ITAG2 = "*hwrfprs.d1.0p25."+FHR+".grb2"
			end if
		else if(DSOURCE.eq."NMMB")then
			ITAG2 = "*nmbprs_d01."+FHR+".nc"
		else if(DSOURCE.eq."GFS")then
			ITAG2 = "gfs.t[0-9][0-9]z.pgrb2.0p25."+FHR+".grb2"
		else if(DSOURCE.eq."fvGFS_ATL")then
			ITAG2 = "atmos_sos.nest02_nested_ltd.nc.nc"
		else if(DSOURCE.eq."HEDAS")then
			print("ERROR: getFiletag: ITAG must be specified for HEDAS output")
			exit
		end if  end if  end if  end if  end if
		end if  end if  end if
	else if(ITAG.ne."" .and. EXT.eq."")
		ITAG2 = "*"+ITAG+"*"+FHR+"*.grb2"
	else
		ITAG2 = "*"+ITAG+"*"+FHR+"*"+EXT
	end if  end if
	
	IFILE = IDIR2+ITAG2

	return(IFILE)

end



;
;================================================================
; getInvestSID

undef("getInvestSID")
function getInvestSID (iYYYY[1]:string,iSID[1]:string)
local invSID, data, dFile, SIDlong, YYYY, tcSID, invSID
begin

	dFile = "/home/Ghassan.Alaka/GPLOT/tbl/SIDs_Old_New.dat"
	oSID	= "missing"
	oSID@_FillValue = "missing"
	
	if(.not.fileexists(dFile))then	oSID	= "missing"
					;print("WARNING:  getInvestSID:  SID table not found.")
	end if

	data	= asciiread(dFile,-1,"string")
	SIDlong	= str_squeeze(str_get_field(data,1," "))
	YYYY	= str_squeeze(str_get_field(data,2," "))
	tcSID	= str_squeeze(str_get_field(data,3," "))
	invSID	= str_squeeze(str_get_field(data,4," "))
	
	val	= ind(YYYY.eq.iYYYY .and. tcSID.eq.iSID)
	
	if(dimsizes(val).gt.1)then	oSID	= "missing"
					;print("WARNING: getInvestSID: Too many entries in table.")
	else if(ismissing(val))then	oSID	= "missing"
					;print("WARNING: getInvestSID: SID not found in table.")
	else				oSID = invSID(val)
	end if  end if
		

	return(oSID)

end



;================================================================
; getLatLonLbl
;
; res - Input is existing NCL resources variable
; dydx - 2 element array giving DY and DX, in degrees

undef("getLatLonLbl")
procedure getLatLonLbl (res[1]:logical, LON[*]:float, LAT[*]:float)
local NX, NY, XLBL, YLBL
begin
	; Checks
	if(	max(LAT).gt. 90.\
	   .or. min(LAT).lt.-90.)then	print("ERROR: getLatLonLbl: Latitude array out of bounds")
	   				exit
	end if
	if(	max(LON).gt. 540.\
	   .or. min(LON).lt.-540.)then	print("ERROR: getLatLonLbl: Longitude array out of bounds")
	   				exit
	end if

	; Set up variables
	;LON		= fspan(-280.,460.,149)
	NX		= dimsizes(LON)
	XLBL		= new(NX,"string")
	;LAT		= fspan(-90.,90.,37)
	NY		= dimsizes(LAT)
	YLBL		= new(NY,"string")
	
	; Get the LONGITUDE labels
	do xxx = 0,NX-1
		if(	LON(xxx).gt.-540.\
		   .and.LON(xxx).lt.-360.)then	XLBL(xxx) = ""+tostring(toint(abs(360.+LON(xxx))))+"W"
		else if(LON(xxx).eq.-360.)then	XLBL(xxx) = "0"
		else if(LON(xxx).lt.-180.\
		   .and.LON(xxx).gt.-360.)then	XLBL(xxx) = ""+tostring(toint(LON(xxx)+360.))+"E"
		else if(LON(xxx).eq.-180.)then	XLBL(xxx) = "180"
		else if(LON(xxx).gt.-180.\
		   .and.LON(xxx).lt.0.)then	XLBL(xxx) = tostring(toint(abs(LON(xxx))))+"W"
		else if(LON(xxx).eq.0.)then	XLBL(xxx) = "0"
		else if(LON(xxx).gt.0.\
		   .and.LON(xxx).lt.180.)then	XLBL(xxx) = tostring(toint(LON(xxx)))+"E"
		else if(LON(xxx).eq.180.)then	XLBL(xxx) = "180"
		else if(LON(xxx).gt.180.\
		   .and.LON(xxx).lt.360.)then	XLBL(xxx) = tostring(toint(360.-LON(xxx)))+"W"
		else if(LON(xxx).eq.360.)then	XLBL(xxx) = "0"
		else if(LON(xxx).gt.360.\
		   .and.LON(xxx).lt.540.)then	XLBL(xxx) = tostring(toint(LON(xxx)-360.))+"E"
		end if
		end if
		end if
		end if
		end if
		end if
		end if
		end if
		end if
		end if
		end if
	end do
	
	; Get the LATITUDE labels
	do yyy = 0,NY-1
		if(	LAT(yyy).lt.0.)then	YLBL(yyy) = tostring(toint(abs(LAT(yyy))))+"S"
		else if(LAT(yyy).eq.0.)then	YLBL(yyy) = "EQ"
		else if(LAT(yyy).gt.0.)then	YLBL(yyy) = tostring(toint(LAT(yyy)))+"N"
		end if
		end if
		end if
	end do
	
	; Assign Attributed
	res@tmXBMode	= "Explicit"
	res@tmXBValues	:= toint(LON)
	res@tmXBLabels	:= XLBL
	res@tmYLValues	:= toint(LAT)
	res@tmYLLabels	:= YLBL

end



;================================================================
; getLatLonLbl2
;
; res - Input is existing NCL resources variable
; LON - array of longitudes
; LAT - array of latitudes
; SMTH - smooth? True or False

undef("getLatLonLbl2")
procedure getLatLonLbl2 (res[1]:logical, LON[*]:float, LAT[*]:float, SMTH[1]:logical)
local NX, NY, XLBL, YLBL
begin
	; Checks
	if(	max(LAT).gt. 90.\
	   .or. min(LAT).lt.-90.)then	print("ERROR: getLatLonLbl: Latitude array out of bounds")
	   				exit
	end if
	if(	max(LON).gt. 540.\
	   .or. min(LON).lt.-540.)then	print("ERROR: getLatLonLbl: Longitude array out of bounds")
	   				exit
	end if

	; Set up variables
	;LON		= fspan(-280.,460.,149)
	NX		= dimsizes(LON)
	XLBL		= new(NX,"string")
	;LAT		= fspan(-90.,90.,37)
	NY		= dimsizes(LAT)
	YLBL		= new(NY,"string")
	
	; SMOOTHING IS ON
	if(SMTH)then
		; Get the LONGITUDE labels
		do xxx = 0,NX-1
			if(	LON(xxx).gt.-540.\
			   .and.LON(xxx).lt.-360.)then	XLBL(xxx) = ""+tostring(toint(abs(360.+LON(xxx))))+"W"
			else if(LON(xxx).eq.-360.)then	XLBL(xxx) = "0"
			else if(LON(xxx).lt.-180.\
			   .and.LON(xxx).gt.-360.)then	XLBL(xxx) = ""+tostring(toint(LON(xxx)+360.))+"E"
			else if(LON(xxx).eq.-180.)then	XLBL(xxx) = "180"
			else if(LON(xxx).gt.-180.\
			   .and.LON(xxx).lt.0.)then	XLBL(xxx) = tostring(toint(abs(LON(xxx))))+"W"
			else if(LON(xxx).eq.0.)then	XLBL(xxx) = "0"
			else if(LON(xxx).gt.0.\
			   .and.LON(xxx).lt.180.)then	XLBL(xxx) = tostring(toint(LON(xxx)))+"E"
			else if(LON(xxx).eq.180.)then	XLBL(xxx) = "180"
			else if(LON(xxx).gt.180.\
			   .and.LON(xxx).lt.360.)then	XLBL(xxx) = tostring(toint(360.-LON(xxx)))+"W"
			else if(LON(xxx).eq.360.)then	XLBL(xxx) = "0"
			else if(LON(xxx).gt.360.\
			   .and.LON(xxx).lt.540.)then	XLBL(xxx) = tostring(toint(LON(xxx)-360.))+"E"
			end if  end if  end if  end if  end if
			end if  end if  end if  end if  end if
			end if
		end do
	
		; Get the LATITUDE labels
		do yyy = 0,NY-1
			if(	LAT(yyy).lt.0.)then	YLBL(yyy) = tostring(toint(abs(LAT(yyy))))+"S"
			else if(LAT(yyy).eq.0.)then	YLBL(yyy) = "EQ"
			else if(LAT(yyy).gt.0.)then	YLBL(yyy) = tostring(toint(LAT(yyy)))+"N"
			end if  end if  end if
		end do
	
		; Assign Attributed
		res@tmXBMode	= "Explicit"
		res@tmXBValues	:= toint(LON)
		res@tmXBLabels	:= XLBL
		res@tmYLValues	:= toint(LAT)
		res@tmYLLabels	:= YLBL
	
	; SMOOTHING IS OFF
	else
		; Get the LONGITUDE labels
		do xxx = 0,NX-1
			if(	LON(xxx).gt.-540.\
			   .and.LON(xxx).lt.-360.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",abs(LON(xxx)+360.))),".00","")+"W"
			else if(LON(xxx).eq.-360.)then	XLBL(xxx) = "0"
			else if(LON(xxx).lt.-180.\
			   .and.LON(xxx).gt.-360.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",LON(xxx)+360.)),".00","")+"E"
			else if(LON(xxx).eq.-180.)then	XLBL(xxx) = "180"
			else if(LON(xxx).gt.-180.\
			   .and.LON(xxx).lt.0.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",abs(LON(xxx)))),".00","")+"W"
			else if(LON(xxx).eq.0.)then	XLBL(xxx) = "0"
			else if(LON(xxx).gt.0.\
			   .and.LON(xxx).lt.180.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",LON(xxx))),".00","")+"E"
			else if(LON(xxx).eq.180.)then	XLBL(xxx) = "180"
			else if(LON(xxx).gt.180.\
			   .and.LON(xxx).lt.360.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",360.-LON(xxx))),".00","")+"W"
			else if(LON(xxx).eq.360.)then	XLBL(xxx) = "0"
			else if(LON(xxx).gt.360.\
			   .and.LON(xxx).lt.540.)then	XLBL(xxx) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",LON(xxx)-360.)),".00","")+"E"
			end if  end if  end if  end if  end if
			end if  end if  end if  end if  end if
			end if
		end do
	
		; Get the LATITUDE labels
		do yyy = 0,NY-1
			if(	LAT(yyy).lt.0.)then	YLBL(yyy) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",abs(LAT(yyy)))),".00","")+"S"
			else if(LAT(yyy).eq.0.)then	YLBL(yyy) = "EQ"
			else if(LAT(yyy).gt.0.)then	YLBL(yyy) = ""+str_sub_str(str_squeeze(sprintf("%5.2f",LAT(yyy))),".00","")+"N"
			end if  end if  end if
		end do
	
		; Assign Attributed
		res@tmXBMode	= "Explicit"
		res@tmXBValues	:= LON
		res@tmXBLabels	:= XLBL
		res@tmYLValues	:= LAT
		res@tmYLLabels	:= YLBL
	end if

end



;================================================================
; getModelInfo
;
; res - Input is existing NCL resources variable
; dydx - 2 element array giving DY and DX, in degrees

undef("getModelInfo")
function getModelInfo (MODELS[*]:string, infoType[1]:string)
begin

	masterInfo    = asciiread(GPLOT+"/tbl/ModelInfo.dat",-1,"string")
	masterModel   = str_get_cols(masterInfo,0,3)
	masterType    = (/"model","long_name","marker","color","title"/)

	if(.not.any(masterType.eq.infoType))then
		print("ERROR: getModelInfo: infoType '"+infoType+"' not recognized.")
		exit
	else
		typeind =str_match_ind(masterType,infoType)+1 ;str_get_field starts at 1
	end if

	if(isatt(infoType,"IS_MSTORM"))then      IS_MSTORM = infoType@IS_MSTORM
	else                                    IS_MSTORM = True
	end if

	N = dimsizes(MODELS)
	if(infoType.eq."marker") vartype="integer" else vartype="string" end if
	MODELINFO = new(N,vartype)
	do aaa = 0,N-1
	
                ; Auto-correct a few non-4-letter model identifiers
                if(IS_MSTORM .and. isStrSubset(MODELS(aaa),"HWRF17"))then
                        MODEL = "HB17"
                else if(IS_MSTORM .and. isStrSubset(MODELS(aaa),"HWRF16"))then
                        MODEL = "HB16"
                else if(isStrSubset(MODELS(aaa),"fvGFS"))then
                        MODEL = "FGFS"
                else if(isStrSubset(MODELS(aaa),"GFS"))then
                        MODEL = "GFSO"
                else
                        MODEL = MODELS(aaa)
                end if  end if  end if  end if

                ; Check if this particular MODEL is supported
                if(.not.any(masterModel.eq.MODEL))then
                        print("WARNING: getModelInfo: MODEL '"+MODEL+"' not recognized. Assigning missing value.")
		else
		modelind=str_match_ind(masterModel,MODEL)
			if (vartype.eq."string") then
			MODELINFO(aaa)=str_squeeze(str_get_field(masterInfo(modelind),typeind,","))
			else if (vartype.eq."integer") then
			;print(str_get_field(masterInfo(modelind),typeind,", "))
			MODELINFO(aaa)=toint(str_squeeze(str_get_field(masterInfo(modelind),typeind,",")))
			end if
			end if

                end if

	end do

	return(MODELINFO)
end



undef("getModelInfo2")
function getModelInfo2 (MODELS[*]:string, infoType[1]:string)
local masterType, masterModel, N, aaa, MODEL, MODELINFO
begin

	; All possible values of "infoType"
	masterType	= (/"color","marker","long_name","title","interp_code"/)
	if(.not.any(masterType.eq.infoType))then
		print("ERROR: getModelInfo: infoType '"+infoType+"' not recognized.")
		exit
	end if
	
	if(isatt(infoType,"IS_MSTORM"))then	IS_MSTORM = infoType@IS_MSTORM
	else					IS_MSTORM = True
	end if
	
	; All possible values of "MODELS"
	masterModel	= (/"HB17","HB16","HB15","HWRF","HWFI","AVNO",\
			    "AVNI","GFSO","GFSI","HMON","HMNI","NVGM",\
			    "NVGI","CTCX","CTCI","OFCL","OFCI","DSHP",\
			    "LGEM","IVCN","TVCN","HB7I","FGFS","EGRR",\
			    "EGRI","H18L","H8LI","H8LG","H8LS","H18W",\
			    "HP2H","H2HI","HP2F","H2FI","HP3H","H3HI",\
			    "H218","H18I"/)
	
	; How many models?
	N = dimsizes(MODELS)
	if(infoType.eq."color")then		MODELINFO = new(N,"string")
	else if(infoType.eq."marker")then	MODELINFO = new(N,"integer")
	else if(infoType.eq."long_name")then	MODELINFO = new(N,"string")
	else if(infoType.eq."title")then	MODELINFO = new(N,"string")
	else if(infoType.eq."interp_code")then	MODELINFO = new(N,"string")
	end if  end if  end if  end if  end if
	
	; Loop over all models (Could be 1)
	do aaa = 0,N-1
		; Auto-correct a few non-4-letter model identifiers
		if(IS_MSTORM .and. isStrSubset(MODELS(aaa),"HWRF17"))then
			MODEL = "HB17"
		else if(IS_MSTORM .and. isStrSubset(MODELS(aaa),"HWRF16"))then
			MODEL = "HB16"
		else if(isStrSubset(MODELS(aaa),"fvGFS"))then
			MODEL = "FGFS"
		else if(isStrSubset(MODELS(aaa),"GFS"))then
			MODEL = "GFSO"
		else
			MODEL = MODELS(aaa)
		end if  end if  end if  end if
		
		; Check if this particular MODEL is supported
		if(.not.any(masterModel.eq.MODEL))then
			print("WARNING: getModelInfo: MODEL '"+MODEL+"' not recognized. Assigning missing value.")
		end if
		
		
		; This is a table of the relevant Model Info, separated by Data Source
		if(     MODEL.eq."H18L")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-L"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-L"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H8LI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H8LG")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-LG"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-LG"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HLGI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H8LS")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-LS"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-LS"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HLSI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H18W")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-W"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-W"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H8WI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HP2H")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-2H"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-2H"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H2HI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H2HI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HP2H"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-2H (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H2HI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HP3H")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-3H"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-3H"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H3HI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H3HI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HP3H"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-3H (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H3HI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HP2F")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 HWRF-Test-2F"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-2F"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H2FI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H2FI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HP2F"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 HWRF Test-2F (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H2FI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H218")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2018 Oper. HWRF"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 Oper. HWRF"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H18I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."H18I")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. H218"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2018 Oper. HWRF (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "H18I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HB17")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2017 HWRF-Basin"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2017 Basin-Scale HWRF"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HB7I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HB7I")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "brown"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HB17"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2017 Basin-Scale HWRF (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HB7I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HB16")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "coral"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2016 HWRF-Basin"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2016 Basin-Scale HWRF"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HB6I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HB15")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "deepskyblue"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 7
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": 2015 HWRF-Basin"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "2015 Basin-Scale HWRF"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HB5I"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HWRF")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "purple"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 9
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Oper. HWRF"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational HWRF"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HWFI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HWFI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "purple"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 9
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HWRF"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational HWRF (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HWFI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."OFCL")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "red"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 12
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": NHC Official"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "NHC Official Forecast"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "OFCI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."OFCI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "red"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 12
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. OFCL"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "NHC Official Forecast (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "OFCI"
			end if  end if  end if  end if  end if
		else if(any((/"AVNO","GFSO"/).eq.MODEL))then
			if(infoType.eq."color")then		MODELINFO(aaa) = "blue"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 6
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Oper. GFS"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Global Forecast System"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = str_sub_str(MODEL,"O","I")
			end if  end if  end if  end if  end if
		else if(any((/"AVNI","GFSI"/).eq.MODEL))then
			if(infoType.eq."color")then		MODELINFO(aaa) = "blue"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 6
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. GFS"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Global Forecast System (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = MODEL
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HMON")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "green"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 4
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Oper. HMON"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational HMON"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "HMNI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."HMNI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "green"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 4
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. HMON"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational HMON (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = MODEL
			end if  end if  end if  end if  end if
		else if(MODEL.eq."NVGM")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "cyan"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 11
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Oper. NAVGEM"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational NAVGEM"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "NVGI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."NVGI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "cyan"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 11
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. NVGM"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Operational NAVGEM (Early)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = MODEL
			end if  end if  end if  end if  end if
		else if(MODEL.eq."DSHP")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "darkgreen"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 6
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Decay SHIPS"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Decay SHIPS"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = ""
			end if  end if  end if  end if  end if
		else if(MODEL.eq."LGEM")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "cadetblue"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 4
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": SHIPS LGEM"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Logistic Growth Equation Model (LGEM)"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = ""
			end if  end if  end if  end if  end if
		else if(MODEL.eq."CTCX")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "darkorange"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 8
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": COAMPS-TC"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "COAMPS-TC w/ GFS"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "CTCI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."CTCI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "darkorange"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 8
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. CTCX"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Early COAMPS-TC"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = MODEL
			end if  end if  end if  end if  end if
		else if(MODEL.eq."TVCN")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "darkslategray"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 10
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": NHC Consensus"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "NHC Track Consensus"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = ""
			end if  end if  end if  end if  end if
		else if(MODEL.eq."IVCN")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "darkslategray"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 10
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": NHC Consensus"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "NHC Intensity Consensus"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = ""
			end if  end if  end if  end if  end if
		else if(MODEL.eq."FGFS")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "seagreen3"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 15
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": fvGFS ATL"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "fvGFS Atlantic 3km"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = ""
			end if  end if  end if  end if  end if
		else if(MODEL.eq."EGRR")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "violetred"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 3
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": UKMet"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "UK Met Office"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = "EGRI"
			end if  end if  end if  end if  end if
		else if(MODEL.eq."EGRI")then
			if(infoType.eq."color")then		MODELINFO(aaa) = "violetred"
			else if(infoType.eq."marker")then	MODELINFO(aaa) = 3
			else if(infoType.eq."long_name")then	MODELINFO(aaa) = MODEL+": Interp. UKMet"
			else if(infoType.eq."title")then	MODELINFO(aaa) = "Early UK Met Office"
			else if(infoType.eq."interp_code")	MODELINFO(aaa) = MODEL
			end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
	end do
	
	; Return Requested Model Info
	return(MODELINFO)
end




;
;================================================================
; getPlotTitle

undef("getPlotTitle")
function getPlotTitle (short_name[1]:string)
local List_Title
begin

                ; Choose the correct title
                if(short_name.eq."RVO850_Z850_UV850")then
                        List_Title	= "850mb Rel. Vorticity (10~S~-5~N~ s~S~-1~N~, shaded), GPH (dam; lines), and Wind (kt; barbs)"
                else if(short_name.eq."RVO500_Z500_UV500")then
                        List_Title	= "500mb Rel. Vorticity (10~S~-5~N~ s~S~-1~N~, shaded), GPH (dam; lines), and Wind (kt; barbs)"
                else if(short_name.eq."RVO200_Z200_UV200")then
                        List_Title	= "200mb Rel. Vorticity (10~S~-5~N~ s~S~-1~N~, shaded), GPH (dam; lines), and Wind (kt; barbs)"
                else if(short_name.eq."HGT500_MSLP")then
                        List_Title	= "500mb Geopotential Height (dam; shaded), and MSLP (mb; lines, centers)"
		else if(short_name.eq."HGT200_UV200_MSLP")then
                        List_Title	= "200mb Geopotential Height (dam; shaded), Wind (kt; barbs), and MSLP (mb; centers)"
                else if(short_name.eq."RH700400_UV700400_MSLP")then
                        List_Title	= "700-400mb Rel. Humidity (%; shaded), Wind (kt; barbs), and MSLP (mb; lines, centers)"
                else if(short_name.eq."RVO850_UV200")then
                        List_Title	= "850mb Rel. Vorticity (10~S~-5~N~ s~S~-1~N~, shaded), and 200mb Wind (kt; barbs)"
                else if(short_name.eq."UV850_MSLP")then
                        List_Title	= "850mb Wind (kt; shaded, lines), Streamlines, MSLP (mb; centers)"
                else if(short_name.eq."UV750_UV550")then
                        List_Title	= "750mb Wind (kt; shaded, streamlines), 550mb (kt, grey streamlines)"
                else if(short_name.eq."RVO700_Z700_UV700")then
                        List_Title	= "700mb  Rel. Vorticity (10~S~-5~N~ s~S~-1~N~, shaded), GPH (dam; contours), and Wind (kt; barbs)"
                else if(short_name.eq."RH700_UV700_MSLP")then
                        List_Title	= "700mb Relative Humidity (%; shaded), Wind (kt), and MSLP (mb; lines and centers)"
                else if(short_name.eq."PRCP_MSLP_Z1000500")then
                        List_Title	= "Precip. Rate (mm/hr; shaded), MSLP (mb; lines, centers), 1000-500mb Z (dam; dashed lines)"
                else if(short_name.eq."PRCP_MSLP")then
                        List_Title	= "Precip. Rate (mm/hr; shaded), MSLP (mb; centers)"
                else if(short_name.eq."TPRCP_MSLP")then
                        List_Title	= "Total Precip. (inches; shaded), MSLP (mb; centers)"
                else if(short_name.eq."TPW_MSLP")then
                        List_Title	= "Total Precipitable Water (mm; shaded), MSLP (mb; centers)"
                else if(short_name.eq."REFL_MSLP")then
                        List_Title	= "Composite Reflectivity (dBz; shaded), MSLP (mb; centers)"
                else if(short_name.eq."REFD_UV750")then   ;MKedit
                        List_Title	= "Reflectivity at 750mb (dBz; shaded), Wind at 750mb (kt; barbs)"
                else if(short_name.eq."CAPE_HLCY_MSLP")then
                        List_Title	= "CAPE (J/kg; shaded), Storm-Rel. Helicity (m~S~2~N~ s~S~-2~N~; lines), MSLP (mb; centers)"
                else if(short_name.eq."SHDL")then
                        List_Title	= "Deep-Layer (200-850mb) Wind Shear (kt; shaded, lines)"
                else if(short_name.eq."SHML")then
                        List_Title	= "Mid-Layer (500-850mb) Wind Shear (kt; shaded, lines)"
                else if(short_name.eq."SHSL")then
                        List_Title	= "Shallow-Layer (700-850mb) Wind Shear (kt; shaded, lines)"
                else if(short_name.eq."MSLP")then
                        List_Title	= "Mean Sea-Level Pressure (mb; shaded, lines)"
                else if(short_name.eq."UV10_MSLP")then
                        List_Title	= "10m Wind (kt; shaded, lines), MSLP (mb; centers)"
                else if(short_name.eq."T2")then
                        List_Title	= "2m Temperature (K; shaded)"
                else if(short_name.eq."PRCP24_MSLP")then
                        List_Title	= "24-h Precip. Rate (mm/hr; shaded), MSLP (mb; centers)"
		else if(short_name.eq."UV_NS")then
                        List_Title	= "Meridional Cross Section: Wind (kt; shaded)"
		else if(short_name.eq."UV_EW")then
                        List_Title	= "Zonal Cross Section:  Wind (kt; shaded)"
                else if(short_name.eq."SFDL")then
                        List_Title	= "Deep-Layer (250-850mb) Steering Flow (kt; shaded, lines)"
		else if(short_name.eq."SFML")then
                        List_Title	= "Mid-Layer (500-850mb) Steering Flow (kt; shaded, lines)"
		else if(short_name.eq."SFSL")then
                        List_Title	= "Shallow-Layer (700-850mb) Steering Flow (kt; shaded, lines)"
		else if(short_name.eq."TCCEN")then
			List_Title	= "Center Fixes [geopotential height centroid]"
		else if(short_name.eq."TCCEN_zoom")then
			List_Title	= "Center Fixes [geopotential height centroid] (zoom)"
		else if(short_name.eq."TCHODO")then
			List_Title	= "Near-Storm Hodograph [kts]"
		else if(short_name.eq."TCHODO_zoom")then
			List_Title	= "Near-Storm Hodograph [kts] (zoom)"
		else if(short_name.eq."T850_UV850_MSLP")then
			List_Title	= "850mb Temperature (K; shaded), 850mb Wind (kt; barbs), and MSLP (mb; centers)"
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if
		end if  end if  end if  end if  end if


		; If nothing from this above matches, then return a default title
		;if(.not.isvar("List_Title"))then   List_Title = "PLEASE ADD TITLE"   end if
		if(.not.isvar("List_Title"))then   List_Title = short_name   end if ;MK

		return(List_Title)

end




;
; ----------------------------------------------------------
; getStmThin
;
; Purpose:  Stores the integer by which to properly thin
;		streamlines in NCL. Later, a function may
;		do this automatically based on resolution
;		and domain size.
;
; Written in NCL by Ghassan Alaka.

undef("getStmThin")
function getStmThin (DSOURCE[1]:string,DMN[1]:string)
local z
begin

	; Define GPLOT_DIR from the environment.
	GPLOT_DIR = systemfunc("echo $GPLOT_DIR")
	
	
	; Find the thinning value from the table
	z = systemfunc("awk '$1 ~ /^"+DSOURCE+"/ && $2 ~ /^"+DMN+"/ { print $3 }' "+GPLOT_DIR+"/tbl/StreamlineThin.dat")
	
	
	; Find the default thinning value from the table
	if(ismissing(z))then
		z = systemfunc("awk '$1 ~ /^DEFAULT/ && $2 ~ /^"+DMN+"/ { print $3 }' "+GPLOT_DIR+"/tbl/StreamlineThin.dat")
	end if
	
	
	; Set the thinning value to 1.
	if(ismissing(z))then
		z = "1"
	end if
	
	z := toint(z)
	
	
	return(z)
	
end ;getStmThin




;
; ----------------------------------------------------------
; hbfilter
;
; Purpose:  This routine filters and removes the hurricane
;	    vortex (Kurihara et al., 1993, MWR)
;
; Written in NCL by Ghassan Alaka.
;
; Adapted from code originally written by Gopal (HRD) and
; Qingfu Liu (EMC)

undef("hbfilter")
function hbfilter (input:float, nLoops[1]:integer)
local output, M, nM, sz, nDims, dimNames, XTU, YTU, K
begin

	; Import constants
	C = load_constants()

	; Set pre-defined parameter M
	M	= (/2.,3.,4.,2.,5.,6.,7.,2.,8.,9.,2./)
	nM	= dimsizes(M)

	; Check the size of the input
	sz	= dimsizes(input)
	nDims	= dimsizes(dimsizes(input))
	
	; Certain # of dimensions are unacceptable
	if(nDims.eq.1 .or. nDims.ge.4)then
		print("ERROR: HBFILTER: Data is "+tostring(nDims)+"D.")
		print("ERROR: HBFILTER: Data must be 2D or 3D.")
		print("ERROR: HBFILTER: Exiting")
		exit
	end if
	
	
	; If 'input' is only 2D, change to 3D
	; Copy attributes, dimensions, and coordinates from 'input'..
	if(nDims.eq.2)then
		output	= new((/1,sz(0),sz(1)/),"float")
		copy_VarAtts(input,output)
		output(0,:,:) = (/input/)
		sz	:= (/1,sz(0),sz(1)/)
	else if(nDims.eq.3)then
		output = (/input/)
		copy_VarAtts(input,output)
	end if
	end if	
	
	; Define XTU and YTU
	XTU	= new((/sz(0),sz(2),nM/),"float",C@fval3)
	YTU	= new((/sz(0),sz(1),nM/),"float",C@fval3)
	
	; Set the filtering parameter K
	K	= new(nM,"float")
	do nnn = 0,nM-1
		K(nnn) = 0.5*(1 - cos(2.*C@pi/M(nnn)))
	end do
	
	
	; Filter in zonal direction
	do aaa = 0,nLoops-1
		do jjj = 0,sz(1)-2
			XTU(:,0,:)	 = conform(XTU(:,0,:),      output(:,jjj,0),      0)
			XTU(:,sz(2)-1,:) = conform(XTU(:,sz(2)-1,:),output(:,jjj,sz(2)-1),0)
			XTU(:,1:sz(2)-2,0) = output(:,jjj,1:sz(2)-2) + K(0)*(output(:,jjj,:sz(2)-3)+output(:,jjj,2:sz(2)-1)-2.*output(:,jjj,1:sz(2)-2))
			do nnn = 1,nM-1
				XTU(:,1:sz(2)-2,nnn) = XTU(:,1:sz(2)-2,nnn-1) + K(nnn)*(XTU(:,:sz(2)-3,nnn-1)+XTU(:,2:sz(2)-1,nnn-1)-2.*XTU(:,1:sz(2)-2,nnn-1))
			end do
			output(:,jjj,:sz(2)-2) = XTU(:,:sz(2)-2,nM-1)
		end do ;jjj
		;exit
	end do ;aaa
		
		
	; Filter in meridional direction
	do aaa = 0,nLoops-1
		do iii = 0,sz(2)-1
			YTU(:,0,:)	 = conform(YTU(:,0,:),      output(:,0,iii),      0)
			YTU(:,sz(1)-1,:) = conform(YTU(:,sz(1)-1,:),output(:,sz(1)-1,iii),0)
			YTU(:,1:sz(1)-2,0) = output(:,1:sz(1)-2,iii) + K(0)*(output(:,:sz(1)-3,iii)+output(:,2:sz(1)-1,iii)-2.*output(:,1:sz(1)-2,iii))
			do nnn = 1,nM-1
				YTU(:,1:sz(1)-2,nnn) = YTU(:,1:sz(1)-2,nnn-1) + K(nnn)*(YTU(:,:sz(1)-3,nnn-1)+YTU(:,2:sz(1)-1,nnn-1)-2.*YTU(:,1:sz(1)-2,nnn-1))
			end do
			output(:,:sz(1)-2,iii) = YTU(:,:sz(1)-2,nM-1)
		end do ;iii
	end do ;aaa
	
	
	; Convert from 3D to 2D if originally 2D data
	if(nDims.eq.2)then	tmp	= output
				output	:= input
				output	= (/tmp(0,:,:)/)
	end if
	
	
	return(output)	

end



;
;================================================================
; isStrSubset2

undef("isStrSubset2")
function isStrSubset2 (myStr[*]:string,sub[1]:string)
local nStr, isStr, sss

begin

        nStr    = dimsizes(myStr)

        isStr   = new(nStr,"logical")

	if(sub.eq."")then
		isStr = True
	else
	        do sss = 0,nStr-1
			isStr(sss) = isStrSubset(myStr(sss),sub)
		end do
	end if

        return(isStr)
end



;
;================================================================
; level_convert

undef("level_convert")
function level_convert (old_lev[*]:numeric,old_units:string,new_type:string,new_units:string)
local new_lev, old_lev_fl, supported, nlev, lll
begin

	myTypes = (/"string","float","integer","string4"/)
	myUnits = (/"hPa","Pa"/)
	nlev	= dimsizes(old_lev)
	
	
	; Check for errors
	if(.not.any(myTypes.eq.typeof(old_lev)))then
		print("ERROR: level_convert: Old data type not supported")
		exit
	end if
	if(.not.any(myTypes.eq.new_type))then
		print("ERROR: level_convert: New data type not supported")
		exit
	end if
	if(.not.any(myUnits.eq.old_units))then
		print("ERROR: level_convert: Old unit type not supported")
		exit
	end if
	if(.not.any(myUnits.eq.new_units))then
		print("ERROR: level_convert: New unit type not supported")
		exit
	end if


	; Set the multiplicative factor to get correct units
	if(old_units.eq.new_units)then
		mfact = 1.
	else
		if(old_units.eq."Pa" .and. new_units.eq."hPa")then
			mfact = 0.01
		end if
		if(old_units.eq."hPa" .and. new_units.eq."Pa")then
			mfact = 100.
		end if
	end if
	if(new_type.eq."string4")then 	new_lev	= new(nlev,"string")
	else				new_lev	= new(nlev,new_type)
	end if
	new_lev@units	= ""+new_units
	do lll = 0,nlev-1
		if(new_type.eq."string")then		new_lev(lll)	= sprintf("%3.0f",mfact*tofloat(old_lev(lll)))
		else if(new_type.eq."string4")then	new_lev(lll)	= sprinti("%0.4i",toint(mfact*tofloat(old_lev(lll))))
		else if(new_type.eq."float")then	new_lev		= mfact*tofloat(old_lev)
							break
		else if(new_type.eq."integer")then	new_lev		= tointeger(mfact*tofloat(old_lev))
							break
		end if
		end if
		end if
		end if
	end do

	
	; Return the new levels
	return(new_lev)

end




;
;================================================================
; printMaxMin
; opt = 0  No exit
;     = 1  Exit

undef("printMaxMin")
procedure printMaxMin (V:numeric,opt[1]:integer)

begin

	print("Max: "+max(V))
	print("Min: "+min(V))

	if(opt.eq.1)then   exit   end if

end



;
;================================================================
; remove_duplicates
; Description: Take an array of data, sort it, remove duplicates, and 
;	       return
; Courtesy of Carl Schreck.

undef("remove_duplicates") 
function remove_duplicates(IN:numeric)
begin 
	data = ndtooned(IN) 
	qsort(data) 
	duplicate = new( dimsizes(data), logical ) 
	duplicate(0) = False 
	do i = 1, dimsizes(data)-1 
		duplicate(i) = ( data(i).eq.data(i-1) ) 
	end do 
	uniqueInd = ind(.not.duplicate) 
	retVal = data(uniqueInd) 
	return(retVal) 
end ; remove_duplicates



;
;===============================================================
; stringOut
; Description: Take an array of strings and write them out in
;              a single line. Great for terminal display.

undef("stringOut")
function stringOut(V)
local Vsz, V1d, out, sss, FMT

begin

	Vsz = dimsizes(V)
	type    = typeof(V)
	if(dimsizes(Vsz).gt.1)then  V1d = ndtooned(V)
				    Vsz := dimsizes(V)
	else			    V1d = V
	end if
	
	; Get the optional format for float/integer types
	if(isatt(V,"format"))then
		FMT = V@format
	else
		if(type.eq."float")then		FMT = "%6.3f"
		else if(type.eq."integer")then	FMT = "%0.4i"
		else				FMT = ""
		end if  end if
	end if
			

	out = ""
	do sss = 0,Vsz-1
		if(sss.ge.100)then   break   end if
		if(type.eq."float")then		out = out+sprintf(FMT,V1d(sss))
		else if(type.eq."integer")then	out = out+sprinti(FMT,V1d(sss))
		else				out = out+tostring(V1d(sss))
		end if  end if
		if(sss.ne.Vsz-1)then out = out+"," end if
	end do

	return(out)

end





;
;================================================================
; testPlot

undef("testPlot_2d_map")
procedure testPlot_2d_map (V[*][*]:float,cnLevels[*]:float)
local wks, dims, res, plot
begin

	wks	= gsn_open_wks("x11","TEST")
	gsn_define_colormap(wks,"ncl_default")
	
	dims	= getvardims(V)
	if(ismissing(dims(0)))then
		print("ERROR: testPlot_2d_map: Dimensions not named")
		exit
	end if

	res			= True
	res@cnLinesOn		= False
	res@cnFillOn		= True
	res@cnFillMode		= "RasterFill"
	res@cnRasterSmoothingOn	= True
	res@cnLevels		= cnLevels
	res@mpMaxLatF		= max(V&$dims(0)$)
	res@mpMinLatF		= min(V&$dims(0)$)
	res@mpMaxLonF		= max(V&$dims(1)$)
	res@mpMinLonF		= min(V&$dims(1)$)
	
	if(max(V&$dims(1)$)-min(V&$dims(1)$).ge.360.)then
		res@gsnAddCyclic = True
	else
		res@gsnAddCyclic = False
	end if
	
	plot = gsn_csm_contour_map(wks,V,res)

end



;
;================================================================
; testPlot_map

undef("testPlot_map")
procedure testPlot_map (V[*][*]:float,cnLevels[*]:float,fileName[1]:string)
local wks, dims, res, plot, odir
begin

	; Get the plot type
	if(.not.isatt(fileName,"type"))then	print("ERROR: testPlot_map: Plot type not found.")
						exit
	end if
	plotType	= fileName@type
	if(.not.any((/"x11","pdf"/).eq.plotType))then	print("ERROR: testPlot_map: Plot type not recognized.")
							exit
	end if
	
	; Open the Workstation
	wks	= gsn_open_wks(plotType,fileName)
	gsn_define_colormap(wks,"ncl_default")
	
	dims	= getvardims(V)
	if(ismissing(dims(0)))then	print("ERROR: testPlot_map: Dimensions not named")
					exit
	end if

	res			= True
	res@cnLinesOn		= False
	res@cnFillOn		= True
	res@cnFillMode		= "RasterFill"
	res@cnRasterSmoothingOn	= True
	res@cnLevelSelectionMode= "ExplicitLevels"
	res@cnLevels		= cnLevels
	res@mpMaxLatF		= max(V&$dims(0)$)
	res@mpMinLatF		= min(V&$dims(0)$)
	res@mpMaxLonF		= max(V&$dims(1)$)
	res@mpMinLonF		= min(V&$dims(1)$)
	
	if(max(V&$dims(1)$)-min(V&$dims(1)$).ge.360.)then
		res@gsnAddCyclic = True
	else
		res@gsnAddCyclic = False
	end if
	
	plot = gsn_csm_contour_map(wks,V,res)

end




;
;================================================================
; testPlot_xy

undef("testPlot_contour")
procedure testPlot_contour (V[*][*]:float,cnLevels[*]:float)
local wks, res, plot
begin

	wks	= gsn_open_wks("x11","TEST")
	gsn_define_colormap(wks,"ncl_default")


	res			= True
	res@cnLinesOn		= False
	res@cnFillOn		= True
	res@cnFillMode		= "RasterFill"
	res@cnRasterSmoothingOn	= True
	if(dimsizes(cnLevels).gt.1)then res@cnLevels = cnLevels end if
	
	plot = gsn_csm_contour(wks,V,res)

end

