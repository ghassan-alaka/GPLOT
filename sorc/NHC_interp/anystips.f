      program anystips
c
c ....................... start prolog ..........................
c
c  program name: anystips
c
c  description:  Main program to run STIPS on ANY consensus member
c		 Statistical Typhoon Intensity Prediction System
c                intensity forecasts for a storm in the western
c		 North Pacific.
c
c  current programmer:        buck sampson, nrl
c
c  usage:                     anystips wp0104 20 NGPI >dostip.txt
c
c  input files:
c    b????.dat - tropical cyclone present and past locations
c
c  output files:
c    anystips.txt - textual output of dostip run.
c
c**************************************************************
c   ******* stips routine has several configuration files, ****
c   ******* field data files, and output files.            ****
c   ******* see stips.f routine for explanations           ****
c**************************************************************
c
c  error conditions: no stips forecast if storm is not within
c                    the western North Pacific basin.
c
c                    abort program if input file not in b deck
c                    format
c
c ............. maintenance section ................................
c
c  brief description of program modules:
c     icrdtg   - creates new dtg given old one and time difference
c     locase   - puts character string in lower case
c     rhdst    - computes distance and direction given lats and lons
c                (rhumb-line)
c     rltlg    - computes lat and lon given lat,lon,direction and distance
c                by moving along a rhumb line
c
c  principal variables and arrays:
c     strmid   - 6 character stormid (wp0196)
c     strmid8  - 8 character stormid (wp011996)
c     filename - location of the output file
c     int12 ...- 12 hr stifor forecast intensity
c     la0      - current latitude
c     lo0      - current longitude
c     lam06 ...- 6 hr old latitude
c     lom06 ...- 6 hr old longitude
c     lam12 ...- 12 hr old latitude
c     lom12 ...- 12 hr old longitude
c     strmid   - storm id (e.g., wp0190)
c     storms   - location of the storms directory
c     wind     - current intensity
c     wind12   - 12 hr old intensity
c     xp       - extrapolation forecast data
c     ymdh     - date (YYMMDDHH)
c     idate    - date (YYYYMMDD)
c     itime    - time (hh)
c     rftrack  - array of forecast lats and lons (official forecast)
c
c
c  references:
C     STIPS - John Knaff, Buck Sampson and Mark DeMaria)
c
c  method:
c     The working best track is read in and stips is run.
c     Results of the models are then written to a file in the "adeck" format.
c
c  language: FORTRAN 77
c
c  record of changes:
c
      include 'dataformats.inc'
      include 'dataioparms.inc'

      common /rawdf/ rawdfn, aidid
      character *40 rawdfn
      character *4  aidid
      character *4  aidid12
      character *4  altid

      dimension rftrack( 10, 2 )
      character ewtrack( 10 )
      character nstrack( 10 )
      dimension ifstip ( 10, 3 )
      dimension ifstpd ( 10, 3 )
      dimension ifstfp ( 10, 3 )
      dimension ifstfd ( 10, 3 )
      dimension ifstf2 ( 10, 3 )
      dimension ifstop ( 10, 3 )
      dimension ifstod ( 10, 3 )
      integer ymdh,wind,wind06,wind12,wind18,wind24
      integer ltlnwnd(numtau,llw)
      integer ltlnwn1(numtau,llw)
      integer ii, iarg
      integer ibtwind, ios
      integer yymmdd
      integer result
      real la0,lo0,lam06,lom06,lam12,lom12,lam18,lom18,lam24,lom24
      real int12, int24, int36, int48, int60, int72
      real btlat, btlon
      character*8 idtg,fstdtg,dtgm06,dtgm12,dtgm18,dtgm24
      character*8 tdtg
      character*8 btdtg
      character*10 dtg10
      character*10 cstname
      character*6 strmid
      character*8 strmid8
      character*4 aidid1, aidid2
      character*2 century
      character*2 cent
      character*1 ns,ew
      character*30 bname
      character*100 storms,atcfinc
      character*200 btfile,fcstfile
      character*200 stfile
      character*25 line
      logical fststip
      logical fststsh
      logical use_alt
      type ( BIG_AID_DATA ) aidsData
      type ( AID_DATA )     aidData, tauData

c
      data fststip /.true./
      data fststsh /.true./
      data use_alt /.false./
      data aidid   /"    "/
      data aidid12 /"    "/
      data altid   /"    "/
      data cstname   /"          "/
      data igt0,igtm12,igtm24 /3*0/
c
c  open files
c
c   2 - best track
c   7 - stips forecast track
c  12 - objective aids
c
      call getenv("ATCFSTRMS",storms)
      ind=index(storms," ")-1
cajs  Use the following starting arg # when compiling with f77
          iarg = 1
cajs  Use the following starting arg # when compiling with f90
cx    iarg = 2
      call getarg(iarg,strmid)
      iarg = iarg + 1
      call locase (strmid,6)
      call getarg(iarg,century)
      strmid8=strmid(1:4)//century//strmid(5:6)
      iarg = iarg + 1
      call getarg(iarg,aidid)
      iarg = iarg + 1
      call getarg(iarg,altid)

      if (strmid(1:1).eq.' ' .or. century(1:1).eq.' '.or.
     1    aidid(1:1) .eq.' ') then
	   print *, 'aidid:',aidid
	   print *, 'useage: anystips wp0101 20 NGPI <JNGI>'
           stop
      endif

      print *,' '
      print *,' '
      print *,' '
      print *,'*************************************************** '
      print *,'          STIPS and Decay STIPS for ', aidid, altid

      btfile=storms(1:ind)//"/b"//strmid8//".dat"
      fcstfile=storms(1:ind)//"/a"//strmid8//".dat"
      stfile=storms(1:ind)//"/"//strmid8//"_anystips.fst"

      print *, ' Best Track File   = ', btfile
      print *, ' STIPS Output File   = ', stfile
      print *, ' Objective Aids File  = ', fcstfile

      open (2,file=btfile,status='old',err=9001)
      open (7,file=stfile,status='unknown',err=9002)
      open (12,file=fcstfile,status='old',iostat=ios,err=9003 )

      rewind 2
      rewind 7
      rewind 12
c
c  make the basin of the storm id upper case
c
      call upcase (strmid,6)
cx    if (ichar(strmid(1:1)) .gt. 96) strmid(1:1) =
cx   &         char(ichar(strmid(1:1))-32)
cx    if (ichar(strmid(2:2)) .gt. 96) strmid(2:2) =
cx   &         char(ichar(strmid(2:2))-32)

      print *,'*************************************************** '
      write (6,6000) strmid8
c
c  the technique numbers
c
      istip=3
      istid=3
c
c  find the last dtg in the best track file
c
      ios = 0
      do while ( ios .eq. 0 )
         call readBTrk( 2,cent,tdtg,btlat,ns,btlon,ew,ibtwind,ios )
         if (tdtg.ne.'        ') fstdtg=tdtg
      enddo
c
c
c  now find the current, -6, -12, -18, and -24 hr positions
c
      call icrdtg (fstdtg,dtgm06,-06)
      call icrdtg (fstdtg,dtgm12,-12)
      call icrdtg (fstdtg,dtgm18,-18)
      call icrdtg (fstdtg,dtgm24,-24)
      rewind 2
      ios = 0
      do while ( ios .eq. 0 )
         call readBTrk( 2,century,btdtg,btlat,ns,btlon,ew,ibtwind,ios )
         if( ios .eq. 0 ) then
            if( btdtg .eq. dtgm24 ) then
               lam24 = btlat
               lom24 = btlon
               wind24 = ibtwind
               if (ns .eq. 'S') lam24 = -lam24
               if (ew .eq. 'W') lom24 = 360.0 - lom24
               igtm24 = 1
            else if( btdtg .eq. dtgm18 ) then
               lam18 = btlat
               lom18 = btlon
               wind18 = ibtwind
               if (ns .eq. 'S') lam18 = -lam18
               if (ew .eq. 'W') lom18 = 360.0 - lom18
               igtm18 = 1
            else if( btdtg .eq. dtgm12 ) then
               lam12 = btlat
               lom12 = btlon
               wind12 = ibtwind
               if (ns .eq. 'S') lam12 = -lam12
               if (ew .eq. 'W') lom12 = 360.0 - lom12
               igtm12 = 1
            else if( btdtg .eq. dtgm06 ) then
               lam06 = btlat
               lom06 = btlon
               wind06 = ibtwind
               if (ns .eq. 'S') lam06 = -lam06
               if (ew .eq. 'W') lom06 = 360.0 - lom06
               igtm06 = 1
            else if( btdtg .eq. fstdtg ) then
               read (century, '(i2)' )icentury
               read( btdtg, '(i8)' ) ymdh
               read( btdtg, '(i6,i2)' ) yymmdd,itime
	       idate = icentury*1000000 + yymmdd
               la0 = btlat
               lo0 = btlon
               wind = ibtwind
               if (ns .eq. 'S') la0 = -la0
               if (ew .eq. 'W') lo0 = 360.0 - lo0
               igt0 = 1
            endif
         endif
      enddo
      close (2)
c
c  we must have at least a current and -12 position.
c
      if (igt0 .eq. 0) then
        write (6,6006)
        stop 'anystips: MUST HAVE VALID CURRENT AND -12 POSITS'
      endif
      if (igtm12 .eq. 0) then
        write (6,6007)
        stop 'anystips: MUST HAVE VALID CURRENT AND -12 POSITS'
      endif
      if (igtm24 .eq. 0) then
        write (6,6008)
        call rhdst (la0,lo0,lam12,lom12,dir,dst)
        call rltlg (lam12,lom12,lam24,lom24,dir,dst)
      endif
c
c  check to make sure intensity is ok
c
      if (wind .le. 0) then
         write (6,6009)
         fststip =  .false.
         fststid =  .false.
      endif
c
c  snag the official forecast
c
      dtg10 = century//fstdtg
      call getBigAidDTG ( 12, dtg10, aidsData, result )
      if ( result .eq. 0 ) then
          print *, "Can't find dtg:",dtg10, "in aids file:",fcstfile
          stop
      endif
      call getTech( aidsData, aidid, aidData, result )
      if ( result .eq. 0 ) then
cx     use the 12-h old interpolation
       aidid12=aidid
       aidid12(4:4)='2'
       print *, aidid," track missing:",fcstfile,", using ", aidid12
       call getTech( aidsData, aidid12, aidData, result )
      endif
      if ( result .eq. 0 ) then
          use_alt = .true.
	  print *, aidid," track missing:",fcstfile,", using ", altid
          call getTech( aidsData, altid, aidData, result )
          if ( result .eq. 0 ) then
	     print *, altid," track missing:",fcstfile
             stop
          endif
      endif
      do 30 j = 1, 10
	  itau = 12*j
	  call getSingleTAU ( aidData, itau, tauData, result )
cx
cx        missing forecast, try again
cx
          if ( result .eq. 0 ) then
	       rftrack(j,1) = 0.0
	       rftrack(j,2) = 0.0
	       ewtrack(j) = "E"
	       nstrack(j) = "N"
	       go to 30
          endif
	  rftrack(j,1) = tauData%aRecord(1)%lat
	  rftrack(j,2) = tauData%aRecord(1)%lon
	  if ( tauData%aRecord(1)%ns .eq. "S" )
     &               rftrack(j,1) = - rftrack(j,1)
	  if ( tauData%aRecord(1)%ew .eq. "W" )
     &               rftrack(j,2) = 360.0 - rftrack(j,2)
cx
cx        fill gaps in forecast, if possible
cx
          if ( j.gt.3            .and. rftrack(j-1,1).eq.0 .and.
     &	       rftrack(j,1).gt.0 .and. rftrack(j-2,1).gt.0) then
                   rftrack(j-1,1) = (rftrack(j-2,1) + rftrack(j,1))/2.0
                   rftrack(j-1,2) = (rftrack(j-2,2) + rftrack(j,2))/2.0
          endif
	  ewtrack(j) = "E"
	  nstrack(j) = "N"
   30 continue

c
c  new stips can run between 0n and 55n and between 30e and 180e.
c
      if (la0 .lt. 0. .or. la0 .gt. 55.) then
         write (6,6003) la0,lo0
         fststip = .false.
         fststid = .false.
      else if (lo0 .gt. 180. .or. lo0 .lt. 30.) then
         write (6,6003) la0,lo0
         fststip = .false.
         fststid = .false.
      endif
c
c  sh stips can run between 0s and 55s and between 30e and 100w.
c
      if (la0 .lt. -55. .or. la0 .gt. 0.) then
         write (6,6004) la0,lo0
         fststsh = .false.
      else if (lo0 .gt. 280. .or. lo0 .lt. 30.) then
         write (6,6004) la0,lo0
         fststsh = .false.
      endif
c
c  write input to screen
c
        print*,'   date:',ymdh
        print*,'   -24 hr  pos:',lam24,'N',lom24,'E',wind24,'kts'
        print*,'   -18 hr  pos:',lam18,'N',lom18,'E',wind18,'kts'
        print*,'   -12 hr  pos:',lam12,'N',lom12,'E',wind12,'kts'
        print*,'   -06 hr  pos:',lam06,'N',lom06,'E',wind06,'kts'
        print*,'   current pos:',la0,'N',lo0,'E',wind,'kts'
        print*,'   ***********model track*****************'

	do 10 i=1,10
         print*, i*12,"hr lat=",rftrack(i,1),"N lon=",rftrack(i,2),"E"
   10   continue


c
c  compute stips forecasts
c
      if (fststip .or. fststsh) then
        read (strmid(3:4),'(i2)') istnum
        call locase (strmid8,8)
        if (fststip) then
c23456789012345678921234567893123456789412345678951234567896123456789712
cx       call stips (strmid8,istnum,cstname,idate,itime,la0,lo0,
cx   &	    wind,lam12,lom12,wind12,rftrack,ifstip,ifstpd,ifstfp,ifstfd)
         call stips (strmid8,istnum,cstname,idate,itime,la0,lo0,wind,
     &      lam12,lom12,wind12,rftrack,
     &      ifstip,ifstpd,
     &      ifstfp,ifstfd,
     &      ifstop,ifstod)
        else if (fststsh) then
         call stfps_sh(strmid8,istnum,cstname,idate,itime,la0,lo0,
     &      wind,lam12,lom12,wind12,rftrack,
     &      ifstfp,ifstfd,ifstop,ifstod)
        endif
c
  	do 45 i = 1, 10
c*********************************************************************72
c  put the STID longitudes into 0-360 degrees for writeAid
	 if (ifstip(i,3) .eq. 0 .or. ifstip(i,1) .eq. 0) then
	    ifstip(i,1) = 0
	    ifstip(i,2) = 0
	    ifstip(i,3) = 0
	    ifstpd(i,1) = 0
	    ifstpd(i,2) = 0
	    ifstpd(i,3) = 0
	 else
	    ifstip(i,2) = 3600 - ifstip(i,2)
	    ifstpd(i,2) = 3600 - ifstpd(i,2)
         endif
c*********************************************************************72
c  put the STFD longitudes into 0-360 degrees for writeAid for new stips
	 if (ifstfd(i,3) .eq. 0 .or. ifstfd(i,1) .eq. 0) then
	    ifstfd(i,1) = 0
	    ifstfd(i,2) = 0
	    ifstfd(i,3) = 0
	 else
  	    ifstfp(i,2) = 3600 - ifstfp(i,2)
  	    ifstfd(i,2) = 3600 - ifstfd(i,2)
         endif
c*********************************************************************72
c  put the STOD longitudes into 0-360 degrees for writeAid for ohc stips
         if (ifstod(i,3) .eq. 0 .or. ifstod(i,1) .eq. 0) then
            ifstop(i,1) = 0
            ifstop(i,2) = 0
            ifstop(i,3) = 0
            ifstod(i,1) = 0
            ifstod(i,2) = 0
            ifstod(i,3) = 0
         else
            ifstop(i,2) = 3600 - ifstop(i,2)
            ifstod(i,2) = 3600 - ifstod(i,2)
         endif
  45    continue
c  print the output
        print *,' FCST is:'
        print *,' '
        if (aidid .eq. 'CONW') then
            aidid1 = 'STWP'
            aidid2 = 'STWD'
        elseif (use_alt) then
            aidid1=altid
            aidid2=altid
            aidid1(3:4)='SP'
            aidid2(3:4)='S1'
        else
            aidid1=aidid
            aidid2=aidid
            aidid1(3:4)='SP'
            aidid2(3:4)='S1'
        endif
cx      do 50 i = 1, 10
cx       itau = i*12
cx       print *,aidid1,itau,'h :',
cx   1           ifstfp(i,1),'N ',ifstfp(i,2),'W ',ifstfp(i,3),'kts'
cx50    continue
        print *,' '
        do 60 i = 1, 10
         itau = i*12
         print *,aidid2,itau,'h :',
     1           ifstfd(i,1),'N ',ifstfd(i,2),'W ',ifstfd(i,3),'kts'
  60    continue

cx      call writeAid( 7, strmid, cent, fstdtg, aidid1, ifstip )
        call writeAid( 7, strmid, cent, fstdtg, aidid2, ifstfd )

c******************************************************************
cx  STOD (ocean heat content)
        if (aidid .eq. 'CONW') then
            aidid1 = 'S2WP'
            aidid2 = 'S2WD'
        elseif (use_alt) then
            aidid1=altid
            aidid2=altid
            aidid1(3:4)='SO'
            aidid2(3:4)='S5'
        else
            aidid1=aidid
            aidid2=aidid
            aidid1(3:4)='S0'
            aidid2(3:4)='S5'
        endif

cx      do 70 i = 1, 10
cx       itau = i*12
cx       print *,aidid1,itau,'h :',
cx   1           ifstop(i,1),'N ',ifstop(i,2),'W ',ifstop(i,3),'kts'
cx70    continue
        print *,' '
        do 80 i = 1, 10
         itau = i*12
         print *,aidid2,itau,'h :',
     1           ifstod(i,1),'N ',ifstod(i,2),'W ',ifstod(i,3),'kts'
cx       noted that sometimes STIPS-OHC blows up near the end of a storm!
cx       sh132009 (2 cases), sh152009 and sh202009
cx       in that case, just give up on forecast.
         if (ifstod(i,3) .gt. 150) ifstod(i,3) = 0
  80    continue

cx      call writeAid( 7, strmid, cent, fstdtg, aidid1, ifstop )
        call writeAid( 7, strmid, cent, fstdtg, aidid2, ifstod )

c******************************************************************
cx  testing pdf mapping
        aidid2(3:4)='S3'
        call fitpdf(wind,ifstfd,10,ifstf2)
        do i = 1, 10
         itau = i*12
cx       print *,' '
cx       print *,aidid2,itau,'h :',
cx   1           ifstf2(i,1),'N ',ifstf2(i,2),'W ',ifstf2(i,3),'kts'
        enddo
cx      call writeAid( 7, strmid, cent, fstdtg, aidid2, ifstf2 )
c******************************************************************
      endif

      goto 9999
c
c
 5000 format (6x,i8,6f4.1,i3,10x,a6)
6000  format(10x,'anystips forecasts for ',a8//)
6001  format(' ',' warning dtg (year = ',i2,') (month = ',i2,') (day =',
     1  i3,') (hour = ',i2,')'/)
6002  format(' ',10x,'tau = ',f5.1,'    lat = ',f5.1,'n',
     1  '   long = ',f6.1,'e'/)
6003  format(' ** no stips wp forecast **    current position:',2f6.1,
     1 '  is outside domain (0n-45n,180e-100e).')
6004  format(' ** no stips sh forecast **    current position:',2f6.1,
     1 '  is outside domain (0n-45s,30e-270w).')
6006  format(' **** anystips stop **** must have valid current posit.')
6007  format(' **** anystips stop **** -12hr bad or missing.')
6008  format(' -24hr posit does not exist.  will extrapolate backward',
     &    ' from 00 and -12')
6009  format(' ** no stips forecast ** storm intensity <= 0.')
c
 9001 continue
      print*,' error opening best track data file:',filename
      stop 'anystips: error opening best track data file'
 9002 continue
      print*,' error opening stips forecast file:',stfile
      stop 'anystips: error opening stips forecast file:'
 9003 continue
      print*,' error opening objective aid file:',fcstfile
      stop 'anystips: error opening forecast file:'

 9999 continue
      stop 'anystips: GOOD STOP'
      end
      subroutine fitpdf(wind,intin,ni,intout)
c
c     program maps STID intensity change forecasts to the pdf of observed
c     intensity change.
c
      COMMON /pdf1/ vmax(60)
      COMMON /pdf2/ob012(60),ob024(60),ob036(60),
     &     ob048(60),ob060(60),ob072(60),ob084(60),
     &     ob096(60),ob108(60),ob120(60)
      COMMON /pdf3/st012(60),st024(60),st036(60),
     &     st048(60),st060(60),st072(60),st084(60),
     &     st096(60),st108(60),st120(60)

      dimension  intin(10,3),intout(10,3)
      real fordI(ni),fixdI(ni)
      integer wind
cx
      do i=1,ni
           fordI(i) = intin(i,3) - wind
           intout(i,1) = intin(i,1)
           intout(i,2) = intin(i,2)
      enddo
cx

      do i=1,10
         di=0.0
         fi=0.0
         ier=0
         di=fordi(i)
         if (i.eq. 1) then
            call interp(di,vmax,st012,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob012,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.2)then
            call interp(di,vmax,st024,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob024,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.3)then
            call interp(di,vmax,st024,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob024,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.3)then
            call interp(di,vmax,st036,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob036,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.4)then
            call interp(di,vmax,st048,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob048,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.5)then
            call interp(di,vmax,st060,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob060,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.6)then
            call interp(di,vmax,st072,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob072,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.7)then
            call interp(di,vmax,st084,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob084,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.8)then
            call interp(di,vmax,st096,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob096,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.9)then
            call interp(di,vmax,st108,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob108,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         elseif(i.eq.10)then
            call interp(di,vmax,st120,rprob,60,ier)
            if(ier.ne.0)print*,'error'
            if (rprob.gt.0.95)rprob=0.95
            if (rprob.lt.0.05)rprob=0.05
            call interp(rprob,ob120,vmax,fi,60,ier)
            if(ier.ne.0)print*,'error'
            fixdI(i)=fi
         endif
      enddo
cx
      do i=1,ni
           intout(i,3) = nint (fixdI(i)) + wind
           if (intin(i,3) .le. 0) intout(i,3) = 0
      enddo
cx
      return
      end
c


C
      SUBROUTINE INTERP(X,ARG,VAL,Y,N,IER)
C
C-------------------------------------------------------------------------------
C
C     THIS SUBROUTINE LINEARLY INTERPOLATES THE VALUE OF Y FOR THE
C     ARGUMENT X FROM THE TABULATED FUNCTION STORED IN ARG (X) AND
C     VAL (Y).  N = NUMBER OF POINTS TABULATED IN ARG AND VAL
C     (MINIMUM = 2).  THE VALUE OF IER IS AS FOLLOWS
C          IER = 0 -  NO ERROR
C          IER = 1 - X.LT.ARG(1).OR.X.GT.ARG(N)
C          IER = 2 - N.LT.2
C          IER = 3 - X(M).GE.X(M+1)
C     IF IER IS NOT ZERO, Y IS SET TO ZERO.
C
C-------------------------------------------------------------------------------
C
      DIMENSION ARG(N),VAL(N)
      IER=0
      IF(N.GE.2) GO TO 20
      IER=2
      GO TO 90
   20 IF(X.GE.ARG(1).AND.X.LE.ARG(N)) GO TO 30
      IER=1
      GO TO 90
   30 IF(X.NE.ARG(N)) GO TO 40
      Y=VAL(N)
      RETURN
   40 DO 50 I=2,N
      IF(X.LT.ARG(I)) GO TO 60
   50 CONTINUE
      STOP 12
   60 IF(X.NE.ARG(I-1)) GO TO 70
      Y=VAL(I-1)
      RETURN
   70 S=ARG(I)-ARG(I-1)
      IF(S.GT.0.) GO TO 80
      IER=3
      GO TO 90
   80 S=(VAL(I)-VAL(I-1))/S
      Y=S*(X-ARG(I-1))+VAL(I-1)
      IF(VAL(I).EQ.99999..OR.VAL(I-1).EQ.99999.) Y=99999.
      RETURN
   90 Y=0.
      PRINT *, 'ERROR ',IER,' IN SUBROUTINE INTERP.'
      RETURN
      END
       BLOCK DATA
       COMMON /pdf1/ vmax(60)
       COMMON /pdf2/ob012(60),ob024(60),ob036(60),
     & ob048(60),ob060(60),ob072(60),ob084(60),
     & ob096(60),ob108(60),ob120(60)
       COMMON /pdf3/st012(60),st024(60),st036(60),
     & st048(60),st060(60),st072(60),st084(60),
     & st096(60),st108(60),st120(60)
c       COMMON /pdf4/omax(10),omin(10)

c       DATA omax /22.5,37.5,52.5,57.5,72.5,77.5,87.5,77.5,77.5,77.5/
c       DATA omin /-22.5,-32.5,-47.5,-62.5,-72.5,-87.5,-87.5,-92.5,
c     .      -97.5,-102.5/
       DATA vmax /-147.500,-142.500,-137.500,-132.500,-127.500,
     , -122.500,-117.500,-112.500,-107.500,-102.500,
     , -97.500, -92.500, -87.500, -82.500, -77.500,
     , -72.500, -67.500, -62.500, -57.500, -52.500,
     , -47.500, -42.500, -37.500, -32.500, -27.500,
     , -22.500, -17.500, -12.500,  -7.500,  -2.500,
     ,   2.500,   7.500,  12.500,  17.500,  22.500,
     ,  27.500,  32.500,  37.500,  42.500,  47.500,
     ,  52.500,  57.500,  62.500,  67.500,  72.500,
     ,  77.500,  82.500,  87.500,  92.500,  97.500,
     , 102.500, 107.500, 112.500, 117.500, 122.500,
     , 127.500, 132.500, 137.500, 142.500, 147.500/

       DATA ob012 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,     0.000,   0.000,   0.000,   0.000,   0.000,
     , 0.000,   0.000,   0.000,   0.000,   0.000,
     , 0.000,   0.000,   0.000,   0.000,   0.000,
     , 0.000,   0.000,   0.000,   0.009,   0.016,
     , 0.029,   0.057,   0.080,   0.190,   0.275,
     , 0.515,   0.702,   0.858,   0.926,   0.967,
     , 0.978,   0.989,   0.989,   0.996,   0.996,
     , 1.000,   1.000,   1.000,   1.000,   1.000,
     , 1.000,   1.000,   1.000,   1.000,   1.000,
     , 1.000,   1.000,   1.000,   1.000,   1.000,
     , 1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob024 /   0.000,   0.000,   0.000,   0.000,   0.000,
     . 0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.005,
     ,  0.009,   0.009,   0.009,   0.010,   0.011,
     ,  0.013,   0.025,   0.038,   0.058,   0.093,
     ,  0.139,   0.180,   0.241,   0.297,   0.374,
     ,  0.497,   0.570,   0.661,   0.768,   0.844,
     ,  0.907,   0.940,   0.952,   0.971,   0.981,
     ,  0.988,   0.988,   0.992,   0.996,   0.996,
     ,  0.996,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob036 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,   0.000,   0.000,   0.000,   0.000,   0.004,
     ,   0.004,   0.004,   0.004,   0.004,   0.007,
     ,   0.012,   0.016,   0.033,   0.034,   0.049,
     ,   0.068,   0.090,   0.112,   0.143,   0.173,
     ,   0.215,   0.284,   0.334,   0.374,   0.439,
     ,   0.498,   0.548,   0.585,   0.682,   0.743,
     ,   0.807,   0.859,   0.903,   0.929,   0.933,
     ,   0.944,   0.956,   0.972,   0.983,   0.984,
     ,   0.991,   0.992,   1.000,   1.000,   1.000,
     ,   1.000,   1.000,   1.000,   1.000,   1.000,
     ,   1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob048 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.004,   0.004,   0.007,   0.007,   0.011,
     ,  0.011,   0.011,   0.015,   0.019,   0.024,
     ,  0.040,   0.044,   0.059,   0.066,   0.071,
     ,  0.108,   0.138,   0.156,   0.187,   0.226,
     ,  0.277,   0.334,   0.370,   0.397,   0.431,
     ,  0.506,   0.551,   0.595,   0.646,   0.689,
     ,  0.726,   0.773,   0.838,   0.884,   0.907,
     ,  0.929,   0.943,   0.953,   0.968,   0.969,
     ,  0.975,   0.990,   0.994,   0.995,   0.995,
     ,  0.995,   0.999,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob060 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.004,   0.004,   0.008,   0.008,   0.012,
     ,  0.013,   0.013,   0.024,   0.036,   0.038,
     ,  0.057,   0.060,   0.078,   0.105,   0.121,
     ,  0.150,   0.178,   0.216,   0.247,   0.274,
     ,  0.304,   0.345,   0.367,   0.404,   0.457,
     ,  0.520,   0.575,   0.614,   0.646,   0.676,
     ,   0.700,   0.739,   0.763,   0.818,   0.854,
     ,  0.871,   0.898,   0.915,   0.941,   0.947,
     ,  0.958,   0.981,   0.984,   0.993,   0.993,
     ,  0.993,   0.995,   0.999,   0.999,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob072 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.004,   0.004,   0.008,   0.012,   0.022,
     ,  0.023,   0.023,   0.050,   0.070,   0.080,
     ,  0.110,   0.114,   0.130,   0.157,   0.165,
     ,  0.186,   0.226,   0.265,   0.282,   0.331,
     ,  0.361,   0.394,   0.420,   0.479,   0.521,
     ,  0.581,   0.603,   0.633,   0.666,   0.698,
     ,  0.721,   0.743,   0.754,   0.776,   0.814,
     ,  0.853,   0.886,   0.913,   0.931,   0.936,
     ,  0.948,   0.963,   0.969,   0.980,   0.986,
     ,  0.986,   0.994,   0.994,   0.996,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob084 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.006,   0.006,   0.020,
     ,  0.026,   0.031,   0.054,   0.066,   0.083,
     ,  0.117,   0.119,   0.141,   0.164,   0.180,
     ,  0.216,   0.273,   0.304,   0.334,   0.384,
     ,  0.430,   0.459,   0.476,   0.527,   0.537,
     ,  0.587,   0.609,   0.641,   0.689,   0.711,
     ,  0.739,   0.764,   0.771,   0.780,   0.799,
     ,  0.833,   0.860,   0.867,   0.896,   0.913,
     ,  0.926,   0.937,   0.947,   0.979,   0.990,
     ,  0.990,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob096 /   0.000,   0.000,   0.000,   0.006,   0.006,
     ,  0.006,   0.006,   0.011,   0.017,   0.037,
     ,  0.049,   0.071,   0.106,   0.119,   0.130,
     ,  0.153,   0.166,   0.183,   0.220,   0.251,
     ,  0.294,   0.333,   0.364,   0.399,   0.447,
     ,  0.466,   0.500,   0.546,   0.586,   0.609,
     ,  0.627,   0.656,   0.690,   0.717,   0.723,
     ,  0.750,   0.770,   0.777,   0.797,   0.824,
     ,  0.849,   0.861,   0.884,   0.907,   0.933,
     ,  0.947,   0.964,   0.983,   0.990,   0.990,
     ,  0.990,   0.997,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob108 /   0.000,   0.000,   0.000,   0.007,   0.007,
     ,  0.007,   0.014,   0.014,   0.028,   0.035,
     ,  0.061,   0.075,   0.110,   0.122,   0.136,
     ,  0.151,   0.160,   0.195,   0.240,   0.247,
     ,  0.270,   0.301,   0.363,   0.390,   0.459,
     ,  0.483,   0.511,   0.539,   0.548,   0.567,
     ,  0.605,   0.619,   0.663,   0.678,   0.713,
     ,  0.736,   0.757,   0.784,   0.828,   0.843,
     ,  0.868,   0.871,   0.871,   0.894,   0.915,
     ,  0.950,   0.979,   0.981,   0.990,   0.991,
     ,  0.997,   0.997,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA ob120 /   0.000,   0.000,   0.000,   0.014,   0.014,
     ,  0.014,   0.021,   0.035,   0.042,   0.056,
     ,  0.076,   0.111,   0.132,   0.159,   0.168,
     ,  0.183,   0.192,   0.220,   0.266,   0.280,
     ,  0.323,   0.356,   0.420,   0.432,   0.483,
     ,  0.515,   0.550,   0.557,   0.578,   0.630,
     ,  0.647,   0.675,   0.709,   0.725,   0.753,
     ,  0.772,   0.797,   0.815,   0.836,   0.866,
     ,  0.875,   0.905,   0.912,   0.935,   0.947,
     ,  0.947,   0.975,   0.979,   0.993,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st012 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.004,   0.004,   0.004,
     ,  0.004,   0.004,   0.008,   0.011,   0.016,
     ,  0.025,   0.030,   0.057,   0.123,   0.236,
     ,  0.557,   0.874,   0.982,   0.998,   0.998,
     ,  0.998,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st024 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.004,   0.011,   0.015,
     ,  0.015,   0.023,   0.028,   0.042,   0.061,
     ,  0.088,   0.138,   0.197,   0.246,   0.291,
     ,  0.472,   0.586,   0.765,   0.915,   0.967,
     ,  0.998,   0.998,   0.998,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st036 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.011,
     ,  0.014,   0.014,   0.021,   0.022,   0.026,
     ,  0.034,   0.060,   0.078,   0.096,   0.140,
     ,  0.177,   0.206,   0.255,   0.284,   0.329,
     ,  0.478,   0.530,   0.622,   0.729,   0.852,
     ,  0.956,   0.978,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st048 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.004,   0.007,   0.011,   0.011,   0.011,
     ,  0.011,   0.019,   0.030,   0.039,   0.058,
     ,  0.062,   0.089,   0.110,   0.150,   0.188,
     ,  0.229,   0.265,   0.275,   0.325,   0.349,
     ,  0.459,   0.510,   0.575,   0.651,   0.754,
     ,  0.845,   0.916,   0.972,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st060 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.004,   0.004,
     ,  0.004,   0.008,   0.012,   0.016,   0.019,
     ,  0.027,   0.034,   0.050,   0.062,   0.076,
     ,  0.089,   0.132,   0.137,   0.176,   0.228,
     ,  0.248,   0.278,   0.318,   0.355,   0.396,
     ,  0.479,   0.507,   0.539,   0.620,   0.700,
     ,  0.778,   0.858,   0.934,   0.970,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st072 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.004,   0.004,   0.008,
     ,  0.012,   0.016,   0.016,   0.020,   0.039,
     ,  0.050,   0.062,   0.074,   0.084,   0.105,
     ,  0.137,   0.162,   0.194,   0.248,   0.289,
     ,  0.308,   0.320,   0.376,   0.396,   0.432,
     ,  0.495,   0.537,   0.583,   0.630,   0.670,
     ,  0.735,   0.827,   0.896,   0.950,   0.979,
     ,  0.999,   1.000,   1.000,   1.000,   1.000,
     , 1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st084 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.000,   0.000,   0.006,
     ,  0.006,   0.011,   0.023,   0.037,   0.049,
     ,  0.049,   0.060,   0.071,   0.093,   0.139,
     ,  0.156,   0.184,   0.240,   0.281,   0.300,
     ,  0.319,   0.359,   0.399,   0.449,   0.471,
     ,  0.540,   0.560,   0.600,   0.634,   0.676,
     ,  0.717,   0.809,   0.859,   0.913,   0.957,
     ,  0.984,   0.994,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st096 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.000,   0.006,   0.011,   0.011,
     ,  0.017,   0.026,   0.031,   0.054,   0.054,
     ,  0.066,   0.100,   0.127,   0.139,   0.161,
     ,  0.201,   0.231,   0.287,   0.306,   0.346,
     ,  0.384,   0.407,   0.464,   0.483,   0.507,
     ,  0.561,   0.594,   0.621,   0.650,   0.680,
     ,  0.719,   0.797,   0.854,   0.904,   0.939,
     ,  0.967,   0.987,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st108 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.000,   0.007,   0.007,   0.014,   0.021,
     ,  0.021,   0.028,   0.042,   0.042,   0.056,
     ,  0.077,   0.104,   0.118,   0.139,   0.153,
     ,  0.203,   0.254,   0.278,   0.292,   0.334,
     ,  0.353,   0.367,   0.390,   0.423,   0.459,
     ,  0.530,   0.551,   0.597,   0.642,   0.687,
     ,  0.720,   0.755,   0.857,   0.901,   0.925,
     ,  0.946,   0.970,   0.984,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/

       DATA st120 /   0.000,   0.000,   0.000,   0.000,   0.000,
     ,  0.007,   0.007,   0.021,   0.021,   0.021,
     ,  0.028,   0.042,   0.042,   0.049,   0.085,
     ,  0.113,   0.134,   0.155,   0.190,   0.220,
     ,  0.261,   0.275,   0.316,   0.344,   0.351,
     ,  0.379,   0.397,   0.418,   0.474,   0.501,
     ,  0.575,   0.582,   0.612,   0.684,   0.730,
     ,  0.746,   0.788,   0.885,   0.907,   0.924,
     ,  0.945,   0.956,   0.977,   0.993,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000,
     ,  1.000,   1.000,   1.000,   1.000,   1.000/
       end


