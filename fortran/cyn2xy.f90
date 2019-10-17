!-------------------------------------------------------------------------
!   SUBROUTINE cyn2xy
!   -------------------
!   V_XYZ		- Input, 3D & in cartesian coordinates
!   V_CYN		- Output, 3D & in polar-cylindrical coordinates
!   ISZ			- Size of the x-dimension of V_XYZ
!   JSZ			- Size of the y-dimension of V_XYZ
!   KSZ			- Size of the z-dimension of V_XYZ, could be 0
!   RAD			- Radius dimension
!   RSZ			- Size of the r-dimension of V_CYN
!   THETA		- Azimuthal Angle dimension
!   THSZ		- Size of the theta-dimension of V_CYN
!   ICEN		- Index of TC center in X
!   JCEN		- Index of TC center in Y
!   DX			- Resoulution in XY units
!   FVAL		- Missing Value
!
!   DESCRIPTION	Coordinate transformation is achieved through a 
!			4 nearest neigbor technique. The closest 4 (X,Y)
!			points are weighted based on proximity to the
!			(R,TH) point of interest. These weight are
!			represented by aaa,bbb,ccc,ddd below. RADIUS 
!			starts from 0 (i.e., TC center) and has the same
!			resolution as the input data (i.e., DX). THETA
!			starts pointing East and rotates counterclockwise.
!			The 1st and last THETA values are identical.


 SUBROUTINE xy2cyn (V_XYZ,V_CYN,ISZ,JSZ,KSZ,RAD,RSZ,THETA,THSZ,ICEN,JCEN,DX,FVAL)

 IMPLICIT NONE

! Input/Ouput Variables
 INTEGER, INTENT(IN)	:: ISZ, JSZ, KSZ, RSZ, THSZ
 INTEGER, INTENT(IN)	:: ICEN(KSZ), JCEN(KSZ)
 REAL,	  INTENT(IN)	:: V_XYZ(ISZ,JSZ,KSZ), DX, FVAL
 REAL,	  INTENT(INOUT)	:: V_CYN(THSZ,RSZ,KSZ)
 REAL,	  INTENT(INOUT)	:: RAD(RSZ), THETA(THSZ)
 
! Local Variables
 INTEGER	:: ibd, i1, i2, jbd, j1, j2
 INTEGER	:: i, iii, j, jjj, k, iloc, jloc
 INTEGER	:: m, m1, n, n1, xsign, ysign
 LOGICAL	:: defined
 REAL		:: pi_cnst = 3.141592653589793238462643383279502884
 REAL		:: dTH, RCOS(THSZ), RSIN(THSZ)
 REAL		:: xxx, yyy, aaa, bbb, ccc, ddd, eee









;
;================================================================
; cyn2xy
;
; Description:	NCL function that converts POLAR CYLINDRICAL
;		(r,theta) coordinates to CARTESIAN (x,y)
;		coordinates by weighting the four (4) nearest
;		neighbors.
;
; Input:	Vtmp	Input (r,theta) data
;			2D (r,theta) or 3D (lev,r,theta)
;		xsz	# of points for the x dimension
;		ysz	# of points for the y dimension
;		xcen	Center index for the X dimension
;			May provide one value, or a different
;				value for each level
;		ycen	Center index for the Y dimension
;			May provide one value, or a different
;				value for each level
;		dr	Unused at this time.
;
; NOTES:	Units of input data will be the same as units
;			of output data.
;		Metadata (e.g., Lat/Lon coordinates) must be
;			added seperately.

undef("cyn2xy")
function cyn2xy (Vtmp:float, xsz[1]:integer, ysz[1]:integer, xcen[*]:integer, ycen[*]:integer, dr[1]:float)
local Vxyz, Vcyn, sz, dNames, nDims, ksz, xsz, ysz, rsz, thsz, dLambda, \
      iii, jjj, kkk, x, y, aaa, bbb, ccc, ddd, m, m1,\
      n, n1, xcen, ycen, r, rloc, th, thloc, eee, tmp
begin
	; Load constants
	C = load_constants()
	
	; Get size of Vtmp
	sz	= dimsizes(Vtmp)
	dNames	= getvardims(Vtmp)
	nDims	= dimsizes(dNames)

	
	; Sort out dimensions and dimension sizes
	if(nDims.lt.2 .or. nDims.gt.3)then	print("ERROR: cyn2xy: Number of dimensions should be 2 or 3.")
						exit
	end if
	do dd = 0,nDims-1
		if(     isStrSubset(dNames(dd),"lev")\
		   .or. isStrSubset(dNames(dd),"lv" )) then	ksz = sz(dd)
		else						if(nDims-3.ge.0)then ksz = sz(nDims-3) end if
		end if
		if(isStrSubset(dNames(dd),"radius"))   then	rsz = sz(dd)
		else						rsz = (sz(nDims-2))
		end if
		if(isStrSubset(dNames(dd),"theta"))    then	thsz = sz(dd)
		else						thsz = sz(nDims-1)
		end if
	end do
	
	
	; Create output array
	if(.not.isvar("ksz"))then	ksz	= 1
					Vcyn	:= new((/1,rsz,thsz/),float)
					Vcyn(0,:,:)	= Vtmp
	else
					Vcyn	= Vtmp
	end if
	
	
	; Make sure number of center fixes matches vertical dimension
	if(dimsizes(ycen).ne.ksz)then
		if(dimsizes(ycen).eq.1)then	print("MSG: xy2cyn: (1) J center used for all levels")
						tmp	:= new(ksz,"integer")
						tmp	= ycen
						ycen	:= tmp
		else				print("ERROR: xy2cyn: Too many J center pts")
						exit
		end if
	end if
	if(dimsizes(xcen).ne.ksz)then
		if(dimsizes(xcen).eq.1)then	print("MSG: xy2cyn: (1) I center used for all levels")
						tmp	:= new(ksz,"integer")
						tmp	= xcen
						xcen	:= tmp
		else				print("ERROR: xy2cyn: Too many I center pts")
						exit
		end if
	end if

	Vxyz = new((/ksz,ysz,xsz/),float)
	
	dLambda = 2.*C@pi/tofloat(thsz)
	
	
	do jjj = 0,ysz-1
	do iii = 0,xsz-1
	do kkk = 0,ksz-1
		
		if(ismissing(xcen(kkk)) .or. ismissing(ycen(kkk)))then   continue   end if
		x	= tofloat(ispan(0,xsz-1,1)-xcen(kkk))
		y	= tofloat(ispan(0,ysz-1,1)-ycen(kkk))
		if(x(iii).eq.0. .and. y(jjj).eq.0.)then	Vxyz(kkk,jjj,iii) = Vcyn(kkk,0,0)
							continue
		end if
		
				
		th	= (0.5*C@pi - -1.*atan2(y(jjj),x(iii)))/dLambda - 1
		;th	= -1.*atan2(y(jjj),x(iii))/dLambda
		thloc	= toint(th)
		aaa	= abs(th - tofloat(thloc))
		bbb	= 1. - aaa
		if(thloc.ge.thsz)then	thloc = thloc - thsz - 1
		else if(th.lt.0.)then	thloc = thloc + thsz - 1
		end if
		end if
		
		
		if(x(iii).eq.0)then
			r 	= abs(y(jjj)/sin(atan2(y(jjj),x(iii))))
		else
			r 	= abs(x(iii)/cos(atan2(y(jjj),x(iii))))
		end if
		rloc	= toint(r)
		ccc	= abs(r - tofloat(rloc))
		ddd	= 1. - ccc
		if(x(iii).lt.0 .and. y(jjj).gt.0)then ;continue
		end if
		
		
		m = rloc
		m1 = m+1
		n = thloc
		n1 = n+toint(sign_matlab(th))
		if(any((/n,m,n1,m1/).lt.0))then		eee = C@fval3
							eee@_FillValue = C@fval3
		else if(any((/n,n1/).gt.thsz-1))then	eee = C@fval3
							eee@_FillValue = C@fval3
		else if(any((/m,m1/).gt.rsz-1))then	eee = C@fval3
							eee@_FillValue = C@fval3
		else					eee = min((/Vcyn(kkk,m,n), Vcyn(kkk,m,n1),\
								    Vcyn(kkk,m1,n),Vcyn(kkk,m1,n1)/))
		end if
		end if
		end if


		if(.not.ismissing(eee))then
			Vxyz(kkk,jjj,iii)	= ddd*(bbb*Vcyn(kkk,m,n) +  aaa*Vcyn(kkk,m,n1)) +\
						  ccc*(bbb*Vcyn(kkk,m1,n) + aaa*Vcyn(kkk,m1,n1))
		;else
		;	print("MSG: xy2cyn: Problems with horizontal interpolation.")
		;	print("MSG: xy2cyn: (kkk,nnn,mmm,n1,m1)=("+kkk+","+nnn+","+mmm+","+n1+","+m1+")")
		end if
	end do
	end do
	end do
	
	
	if(num(ismissing(Vxyz)).gt.0)then
		print("MSG: cyn2xy: Problems with horizontal interpolation.")
		print("MSG: cyn2xy: "+num(ismissing(Vxyz))+" points are missing.")
	end if
	
	
	; Create output array
	if(ksz.eq.1)then   Vxyz := Vxyz(0,:,:)   end if
	
	
	return(Vxyz)

end
