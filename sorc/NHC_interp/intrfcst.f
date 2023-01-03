      program nintrfcst
c
c**   This program interpolates and extrapolates the official and model
c**      in time and space
c**           Original programmer:                   Jim Gross, NHC 1997-2001
c**           New interpolator:                      Jim Gross, NHC 2001
c**           Applied to JTWC aids, consensus, AOR:  B. Sampson  6/01
c**           QC routines added                   :  B. Sampson  8/01
c**           Added configuration file:              B. Sampson  1/02
c**           Added alternate aids for consensus     B. Sampson  9/02
c**           Added Goerss error radii for WP        B. Sampson  5/04
c**           Added Current models to consensus      B. Sampson  6/04
c**           Added Goerss error radii for SH        B. Sampson  6/05
c**           Updated Goerss error radii for WP      B. Sampson  6/05
c**           Rewritten NHC interpolator             J. Gross    8/05
c**           Combined NHC/JTWC interpolator         B. Sampson 11/05
c**           Second and third alternates            B. Sampson  8/06
c**           Allow selection of input file          B. Sampson  3/07
c**           Allow selection of dtg                 B. Sampson  3/07
c**           Allow unlimited current models         B. Sampson  4/07
c**           GPCE and corrected consensus for AL,EP B. Sampson  4/07
c**           Consensus information file             B. Sampson  12/07
c**           Specify "track" or "intensity" in input B. Sampson 07/08
c**           GPCE-AX                                 B. Sampson 03/09
c**           6-7 day forecasting for track,int, rad  B. Sampson 04/10
c**           intensity adjustment options            B. Sampson 01/12
c**           initial and final intensity adj times   B. Sampson 04/23/12
c**           intensity GPCE                          B. Sampson 02/27/13
c**           track/intensity GPCE updates            B. Sampson/J. Goerss 04/07/14
c**           track phase out capability              B. Sampson/J. Goerss 11/19/14
c
c
c     ncmax is the maximum number of consensus forecasts allowed
c     incmax is the maximum number of input models per consensus
      parameter ( ncmax=100, incmax=40 )

      parameter ( nmodmax=1000 )
c
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /adjust/iadj(nmodmax),jadj(nmodmax)
      common /files/iaids_file, istd_out,iconsensus_in
c
      real    blat, blon
      integer iwind
      integer iaids_file, istd_out
c
      character*1   ns, ew
      character*2   bttype
      character*4   fst_tech, tech_12, tech_06
      character*4   conname, cona(incmax)
      character*4   alt(incmax),alt2(incmax),alt3(incmax)
      character*8   strmid
      character*10  doall, dtg_request
      character*10  ymdh(150), dtgcur, output_ext
      character*500 home_path, stm_path, out_path
      character*500 bdeck_file, adeck_file, adeck_temp, edeck_temp
      character*500 con_in_file
      character*50  infile
      character*50  defile

c
c     File ids
c
      iaids_file = 31
      istd_out = 6
      iconsensus_in = 35

c
c**   Get the strmid from the command line
c
      strmid = '        '
      doall  = '          '
      infile = '          '
      defile = 'intrfcst.input'
cx   set iarg=1 for some compilers, 2 for others
      iarg=1
      call getarg ( iarg, strmid )

      if (strmid .ne. '        ' ) then
         print *, 'intrfcst: Storm ID = ', strmid
      else
         print *, 'USAGE: intrfcst wp012000 [doall,yyyymmddhh] [infile]'
         stop
      endif
c
c**   Get the doall option from the command line
c
      call getarg ( iarg+1, doall )
      if ( doall .eq. 'doall     ' ) then
         print *, 'Doing whole file'
         dtg_request = '          '
         call getarg ( iarg+2, infile )
      else if ( doall(1:1) .gt. '0' .and. doall(1:1) .le. '9'  ) then
         print *, 'Doing selected dtg:', doall
         dtg_request = doall
         call getarg ( iarg+2, infile )
      else
         dtg_request = '          '
         call getarg ( iarg+1, infile )
      endif
c
cx  read the input file for which aids to interpolate
cx  and which consensus to compute
      if ( infile(1:1).eq.' ' ) then
       infile = defile
      endif
      call read_mod(infile,nmodmax,nmodel,fst_tech,
     &              tech_12,tech_06,iadj,jadj)
c
c**   Output filename extension
c
      output_ext = 'gun'
c
c**   Create the bdeck, adeck, edeck output and adeck output file names and
c**     open the files
c
      call getenv ( "ATCFSTRMS", stm_path )           ! c-ops
      out_path = stm_path                             ! c-ops
c
c
      bdeck_file = trim( stm_path )//"/b"//strmid//".dat"
      adeck_file = trim( stm_path )//"/a"//strmid//".dat"
      adeck_temp = trim( out_path )//"/a"//strmid//"."//output_ext
      edeck_temp = trim( out_path )//"/e"//strmid//"."//output_ext
      con_in_file =trim( out_path )//"/"//strmid//"consensus_input.gun"
c
      print *, ' *************************************************** '
      print *, ' '
      print *, ' interpolator for ',strmid
      print *, ' Best track file    = ', bdeck_file
      print *, ' Aids file          = ', adeck_file
      print *, ' Aids output file   = ', adeck_temp
      print *, ' Error output file  = ', edeck_temp
      print *, ' Consensus input used  = ', con_in_file
c
c**   Open input files
c
      open ( 21, file=bdeck_file, status='old', iostat=ios, err=1010 )
      open ( 22, file=adeck_file, status='old', iostat=ios, err=1020 )
c
c**   Open the output files
c
      open ( 31, file=adeck_temp, status='unknown',iostat=ios,err=1030 )
      open ( 32, file=edeck_temp, status='unknown',iostat=ios,err=1040 )
      open ( 35, file=con_in_file, status='unknown',iostat=ios,err=1050)
c
C**   Read the best track file and store the date/time group in an array
C
      isave = 1
      ifirst= 0
 10   call doReadBT ( 21, ymdh(isave),blat,ns,blon,ew,iwind,bttype,ios )
      if ( ios .lt. 0 ) goto 20
c     Save this one if it matches the requested dtg
      if (ymdh(isave) .eq. dtg_request) then
          ifirst = isave
      endif
      isave = isave + 1
      go to 10
c
   20 close ( 21 )
      last = isave - 1

cx    no best track for dtg requested
      print *, 'ifirst=', ifirst
      if (ifirst .le. 0 .and. dtg_request.ne. '          ') then
          print *, ' no best track point for dtg=', dtg_request
          stop
      endif

cx
cx set loop start depending on whether to do all dtgs, selected dtg, or last dtg
cx
      if ( doall.eq.'doall' ) then
         ifirst = 1
      else if ( dtg_request(1:1) .gt. '0' ) then
         last = ifirst
      else
         ifirst = last
      endif

c
      do 200 loop = ifirst, last
c
         rewind ( 22 )
c
c**   Find the current, past 6-hour and past 12-hour forecast times
c
         dtgcur = ymdh(loop)                      !c-loop
c
c**   Read in the forecast data
c
         call reader ( dtgcur, nmodel, istat )

         if ( istat .eq. 1 ) goto 200
c
c**   Create the interpolated forecasts
c

         call intrfcst ( strmid, dtgcur, nmodel )
cx
cx   add current model runs (STIP, STID, STWP, STWD ...) to aids used in consensus
cx
      call get_cur(nmodel)
c
c   consensus loop
c
  150 continue
         call read_con(infile,ncmax,incmax,conname,contype,
     &                 naids,nmin,cona,alt,alt2,alt3,idone)
         if (idone.eq.1) then
           idone = 0
           rewind (23)
           go to 200
         endif
c
c**   Compute and write out the consensus forecast
c
         call comcon(strmid,dtgcur,dir,nmodel,
     &               conname,contype,naids,nmin,cona,alt,alt2,alt3)
cx       call comconm(strmid,dtgcur,dir,nmodel,
cx   &               conname,contype,naids,nmin,cona,alt,alt2,alt3)
         go to 150

  200 continue
c
      close ( 22 )
      close ( 23 )
      close ( 31 )
      close ( 32 )
      close ( 35 )
c
      stop ' Interpolation of forecasts are finished'
c
c**   Error messages
c
 1010 print *, ' Error opening b-deck of current storm - ios = ',ios
      print *, ' Filename:', bdeck_file
      stop
c
 1020 print *, ' Error opening a-deck of current storm - ios = ',ios
      print *, ' Filename:', adeck_file
      stop
c
 1030 print *, ' Error opening temporary adeck for output - ios = ',ios
      print *, ' Filename:', adeck_temp
      stop
c
 1040 print *, ' Error opening temporary error radius file - ios = ',ios
      print *, ' Filename:', edeck_temp
      stop
 1050 print *, ' Error opening consensus input aids used - ios = ',ios
      print *, ' Filename:', con_in_file
      stop
c
      end
c***************************************************************************
      subroutine reader ( dtgcur, nmodel, istat )
c
c**   Reads in the initial and forecast data
c
      include 'dataformats.inc'
c
      parameter ( nmodmax=1000 )
c
      parameter ( ntau=31 )
      parameter ( taurecmax=20 )
      parameter ( ltau=15 )
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /adjust/iadj(nmodmax),jadj(nmodmax)
c
      common /forecast/ fst00lat(ntau,nmodmax), fst00lon(ntau,nmodmax),
     &                  fst00spd(ntau,nmodmax), fst00tau(ntau,nmodmax),
     &                  fst00rad(ntau,4,taurecmax,nmodmax),
     &                  fst06lat(ntau,nmodmax), fst06lon(ntau,nmodmax),
     &                  fst06spd(ntau,nmodmax), fst06tau(ntau,nmodmax),
     &                  fst06rad(ntau,4,taurecmax,nmodmax),
     &                  fst12lat(ntau,nmodmax), fst12lon(ntau,nmodmax),
     &                  fst12spd(ntau,nmodmax), fst12tau(ntau,nmodmax),
     &                  fst12rad(ntau,4,taurecmax,nmodmax),
     &                  last_tau00(nmodmax),
     &                  last_tau06(nmodmax), last_tau12(nmodmax)
      common /initial/ latcur, loncur, spdcur, radcur
      common /minus12/ latm12, lonm12, spdm12
cx  development type for OFCI
      common /devel/ ty(ltau)
c
      real fst06lat, fst06lon, fst06spd, fst06tau
      real fst12lat, fst12lon, fst12spd, fst12tau
      integer tau(ntau), last_tau00, last_tau06, last_tau12
      real latcur, loncur, spdcur, radcur( 4, 3 )
      real latm06, lonm06, spdm06, radm06( 4, 3 )
      real latm12, lonm12, spdm12, radm12( 4, 3 )
      integer result
c
      character*2   ty
      character*4   fst_tech, tech_12, tech_06
      character*10  dtgcur, dtgm06, dtgm12
c
      type ( BIG_AID_DATA ) aidsData
      type ( AID_DATA )     aidData, tauData
c
      data tau /   0,   6,  12,  18,  24,  30,  36,  42,  48,
     &                 54,  60,  66,  72,  78,  84,  90,  96,
     &                102, 108, 114, 120, 126, 132, 138, 144,
     &                150, 156, 162, 168, 174, 180 /
c
      latcur = 0.0
      loncur = 0.0
      spdcur = 0.0
      latm06 = 0.0
      lonm06 = 0.0
      spdm06 = 0.0
      latm12 = 0.0
      lonm12 = 0.0
      spdm12 = 0.0
      do m = 1, 3
         do n = 1, 4
            radcur( n, m ) = 0.0
            radm06( n, m ) = 0.0
            radm12( n, m ) = 0.0
         enddo
      enddo
c
      do i = 1, nmodel
         last_tau00(i) = 0
         last_tau06(i) = 0
         last_tau12(i) = 0
c
         do j = 1, ntau
            fst00lat( j, i ) = 0.0
            fst00lon( j, i ) = 0.0
            fst00spd( j, i ) = 0.0
            fst00tau( j, i ) = 0.0
            fst06lat( j, i ) = 0.0
            fst06lon( j, i ) = 0.0
            fst06spd( j, i ) = 0.0
            fst06tau( j, i ) = 0.0
            fst12lat( j, i ) = 0.0
            fst12lon( j, i ) = 0.0
            fst12spd( j, i ) = 0.0
            fst12tau( j, i ) = 0.0
c
            do m = 1, 3
               do n = 1, 4
                  fst00rad( j, n, m, i ) = 0.0
                  fst06rad( j, n, m, i ) = 0.0
                  fst12rad( j, n, m, i ) = 0.0
               enddo
            enddo
c
         enddo
      enddo
c
      call dtgmod ( dtgcur,  -6, dtgm06 , result )
      call dtgmod ( dtgcur, -12, dtgm12 , result )

c
cc      rewind ( 22 )                         !c-loop
c
c**   Get the minus 12-hour initial data from the CARQ
c
      call getBigAidDTG ( 22, dtgm12, aidsData, result )
      if ( result .eq. 0 ) then
         rewind ( 22 )
         goto 50
      endif
c
      call getTech ( aidsData, "CARQ", aidData, result )
      if ( result .eq. 0) then
         rewind ( 22)
         goto 50
      endif
c
      call getSingleTAU ( aidData, 0, tauData, result )
      if ( result .eq. 0 ) then
         rewind ( 22 )
         goto 50
      endif
c
      latm12 = alat180(tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
      lonm12 = alon360(tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
      spdm12 = tauData%aRecord(1)%vmax
c
c**   Read the 34, 50 and 64-knot wind radii
c
      num12rcrds = tauData%numrcrds
c
      do m = 1, num12rcrds
c
         if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
            radm12( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radm12( 2, m ) = float( tauData%aRecord(m)%radii(1))
            radm12( 3, m ) = float( tauData%aRecord(m)%radii(1))
            radm12( 4, m ) = float( tauData%aRecord(m)%radii(1))
c
         elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
            radm12( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radm12( 2, m ) = float( tauData%aRecord(m)%radii(2))
            radm12( 3, m ) = float( tauData%aRecord(m)%radii(3))
            radm12( 4, m ) = float( tauData%aRecord(m)%radii(4))
c
         endif
      enddo
c
c**   Get the forecast data for desired techinques for all TAUs
c
      do 40 i = 1, nmodel
c
         call getTech ( aidsData, fst_tech(i), aidData, result )
         if ( result .eq. 0 ) goto 40
c
         do 30 j = 1, ntau
c
            call getSingleTAU ( aidData, tau(j), tauData, result )
            if ( result .eq. 0 ) then
c
c**   If there is no initial 0 TAU for the technique, use the CARQ data
c
               if ( tau(j) .eq. 0 ) then
                  last_tau12(i) = last_tau12(i) + 1
                  fst12lat( last_tau12(i), i ) = latm12
                  fst12lon( last_tau12(i), i ) = lonm12
                  fst12spd( last_tau12(i), i ) = spdm12
                  fst12tau( last_tau12(i), i ) = float( tau(j) )
                  do m = 1, num12rcrds
                     do n = 1, 4
                        fst12rad( last_tau12(i), n, m, i ) = radm12(n,m)
                     enddo
                  enddo
                  go to 30
               else
                  go to 30
               endif
c
            endif
c
            last_tau12(i) = last_tau12(i) + 1
            fst12lat( last_tau12(i), i ) =
     &       alat180( tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
            fst12lon( last_tau12(i), i ) =
     &       alon360( tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
            fst12spd( last_tau12(i), i ) = tauData%aRecord(1)%vmax
            fst12tau( last_tau12(i), i ) = float( tau(j) )
c
c**   Read the 34, 50 and 64-knot wind radii
c
            do m = 1, tauData%numrcrds
c
               if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
                  fst12rad( last_tau12(i), 1, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst12rad( last_tau12(i), 2, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst12rad( last_tau12(i), 3, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst12rad( last_tau12(i), 4, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
c
               elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
                  fst12rad( last_tau12(i), 1, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst12rad( last_tau12(i), 2, m, i ) =
     &                 float( tauData%aRecord(m)%radii(2))
                  fst12rad( last_tau12(i), 3, m, i ) =
     &                 float( tauData%aRecord(m)%radii(3))
                  fst12rad( last_tau12(i), 4, m, i ) =
     &                 float( tauData%aRecord(m)%radii(4))
c
               endif
            enddo
c
 30      continue
c
c**   The OFCL forecast requires special handling
c
         if ( fst_tech(i) .eq. 'OFCL' ) then
c
c**   Because intensifying OFCL forecast intensities can cross radii
c**     thresholds, add backward persistence to the OFCL radii forecasts
c**     by extending them one TAU backwards.
c
            do j = 2, last_tau12(i)
               if(fst12spd(j-1,i).lt.64.0.and.fst12spd(j,i).gt.64.0)then
                  do n = 1, 4
                     fst12rad(j-1,n,3,i) = fst12rad(j,n,3,i)
                  enddo
               endif
               if(fst12spd(j-1,i).lt.49.0.and.fst12spd(j,i).gt.49.0)then
                  do n = 1, 4
                     fst12rad(j-1,n,2,i) = fst12rad(j,n,2,i)
                  enddo
               endif
               if(fst12spd(j-1,i).lt.34.0.and.fst12spd(j,i).gt.34.0)then
                  do n = 1, 4
                     fst12rad(j-1,n,1,i) = fst12rad(j,n,1,i)
                  enddo
               endif
            enddo
c
c**   Because OFCL radii forecasts do not extend 5 days, add persistence
c**     to the OFCL radii forecasts by extending them one TAU
c
            if ( fst12spd(5,i).gt.64.0.and.fst12spd(4,i).gt.64.0 )then
               do n = 1, 4
                  fst12rad(5,n,3,i) = fst12rad(4,n,3,i)
               enddo
            endif
            if ( fst12spd(7,i).gt.49.0.and.fst12spd(6,i).gt.49.0 )then
               do n = 1, 4
                  fst12rad(7,n,2,i) = fst12rad(6,n,2,i)
               enddo
            endif
            if ( fst12spd(7,i).gt.34.0.and.fst12spd(6,i).gt.34.0 )then
               do n = 1, 4
                  fst12rad(7,n,1,i) = fst12rad(6,n,1,i)
               enddo
            endif
         endif
c
 40   continue
c
 50   continue
c
c**   Get the minus 6-hour initial data from the CARQ
c
      call getBigAidDTG ( 22, dtgm06, aidsData, result )
      if ( result .eq. 0 ) then
         rewind ( 22 )
         goto 80
      endif
c
      call getTech ( aidsData, "CARQ", aidData, result )
      if ( result .eq. 0 ) then
         rewind ( 22 )
         goto 80
      endif
c
      call getSingleTAU ( aidData, 0, tauData, result )
      if ( result .eq. 0 ) then
         rewind ( 22 )
         goto 80
      endif
c
      latm06 = alat180(tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
      lonm06 = alon360(tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
      spdm06 = tauData%aRecord(1)%vmax
c
c**   Read the 34, 50 and 64-knot wind radii
c
      num06rcrds = tauData%numrcrds
c
      do m = 1, num06rcrds
c
         if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
            radm06( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radm06( 2, m ) = float( tauData%aRecord(m)%radii(1))
            radm06( 3, m ) = float( tauData%aRecord(m)%radii(1))
            radm06( 4, m ) = float( tauData%aRecord(m)%radii(1))
c
         elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
            radm06( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radm06( 2, m ) = float( tauData%aRecord(m)%radii(2))
            radm06( 3, m ) = float( tauData%aRecord(m)%radii(3))
            radm06( 4, m ) = float( tauData%aRecord(m)%radii(4))
c
         endif
      enddo
c
c**   Get the forecast data for desired techinques all TAUs
c
      do 70 i = 1, nmodel
c
         call getTech ( aidsData, fst_tech(i), aidData, result )
         if ( result .eq. 0 ) goto 70
c
         do 60 j = 1, ntau
            call getSingleTAU ( aidData, tau(j), tauData, result )
            if ( result .eq. 0 ) then
c
c**   If there is no initial 0 TAU for the technique, use the CARQ data
c
               if ( tau(j) .eq. 0 ) then
                  last_tau06(i) = last_tau06(i) + 1
                  fst06lat( last_tau06(i), i ) = latm06
                  fst06lon( last_tau06(i), i ) = lonm06
                  fst06spd( last_tau06(i), i ) = spdm06
                  fst06tau( last_tau06(i), i ) = float( tau(j) )
                  do m = 1, num06rcrds
                     do n = 1, 4
cx                     found an array overwrite here ... sampson jun 06
                       if (last_tau12(i) .gt. 0) then
                        fst06rad( last_tau12(i), n, m, i ) = radm06(n,m)
                       endif
                     enddo
                  enddo
                  go to 60
               else
                  go to 60
               endif
            endif
c
            last_tau06(i) = last_tau06(i) + 1
            fst06lat( last_tau06(i), i ) =
     &       alat180( tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
            fst06lon( last_tau06(i), i ) =
     &       alon360( tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
            fst06spd( last_tau06(i), i ) = tauData%aRecord(1)%vmax
            fst06tau( last_tau06(i), i ) = float( tau(j) )
c
c**   Read the 34, 50 and 64-knot wind radii
c
            do m = 1, tauData%numrcrds
c
               if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
                  fst06rad( last_tau06(i), 1, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst06rad( last_tau06(i), 2, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst06rad( last_tau06(i), 3, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst06rad( last_tau06(i), 4, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
c
               elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
                  fst06rad( last_tau06(i), 1, m, i ) =
     &                 float( tauData%aRecord(m)%radii(1))
                  fst06rad( last_tau06(i), 2, m, i ) =
     &                 float( tauData%aRecord(m)%radii(2))
                  fst06rad( last_tau06(i), 3, m, i ) =
     &                 float( tauData%aRecord(m)%radii(3))
                  fst06rad( last_tau06(i), 4, m, i ) =
     &                 float( tauData%aRecord(m)%radii(4))
c
               endif
            enddo
c
 60      continue
c
c**   The OFCL forecast requires special handling
c
         if ( fst_tech(i) .eq. 'OFCL' ) then

cx    First capture development type for OFCL
         do j = 1, ltau
            call getSingleTAU ( aidData, (j-1)*12, tauData, result )
            if (result) then
               ty( j ) = tauData%aRecord(1)%ty
            else
               ty( j ) = '  '
            endif
         enddo
c
c**   Because intensifying OFCL forecast intensities can cross radii
c**     thresholds, add backward persistence to the OFCL radii forecasts
c**     by extending them one TAU backwards.
c
            do j = 2, last_tau06(i)
               if(fst06spd(j-1,i).lt.64.0.and.fst06spd(j,i).gt.64.0)then
                  do n = 1, 4
                     fst06rad(j-1,n,3,i) = fst06rad(j,n,3,i)
                  enddo
               endif
               if(fst06spd(j-1,i).lt.49.0.and.fst06spd(j,i).gt.49.0)then
                  do n = 1, 4
                     fst06rad(j-1,n,2,i) = fst06rad(j,n,2,i)
                  enddo
               endif
               if(fst06spd(j-1,i).lt.34.0.and.fst06spd(j,i).gt.34.0)then
                  do n = 1, 4
                     fst06rad(j-1,n,1,i) = fst06rad(j,n,1,i)
                  enddo
               endif
            enddo
c
c**   Because OFCL radii forecasts do not extend 5 days, add forward
c**     persistence to the OFCL radii forecasts by extending them
c**     one TAU forwards.
c
            if ( fst06spd(5,i).gt.64.0.and.fst06spd(4,i).gt.64.0 )then
               do n = 1, 4
                  fst06rad(5,n,3,i) = fst06rad(4,n,3,i)
               enddo
            endif
            if ( fst06spd(7,i).gt.49.0.and.fst06spd(6,i).gt.49.0 )then
               do n = 1, 4
                  fst06rad(7,n,2,i) = fst06rad(6,n,2,i)
               enddo
            endif
            if ( fst06spd(7,i).gt.34.0.and.fst06spd(6,i).gt.34.0 )then
               do n = 1, 4
                  fst06rad(7,n,1,i) = fst06rad(6,n,1,i)
               enddo
            endif

         endif
c
 70   continue
c
 80   continue
c
      istat = 0
c
c**   Get the current initial data from the CARQs
c
      call getBigAidDTG ( 22, dtgcur, aidsData, result )
      if ( result .eq. 0 ) then
         print *, ' No initial objective aid data for: ', dtgcur
         istat = 1
         return
      endif
c
      call getTech ( aidsData, "CARQ", aidData, result )
      if ( result .eq. 0 ) then
         print *, ' No CARQ in initial objective aid data: ', dtgcur
         istat = 1
         return
      endif
c
      call getSingleTAU ( aidData, 0, tauData, result )
      if ( result .eq. 0 ) then
         print *, ' No initial position from CARQ for: ', dtgcur
         istat = 1
         return
      endif
c
      latcur = alat180(tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
      loncur = alon360(tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
      spdcur = tauData%aRecord(1)%vmax
cxxxxxx   insert ... sampson aug 08
c
c**   Read the 34, 50 and 64-knot wind radii
c
      num00rcrds = tauData%numrcrds
c
      do m = 1, num00rcrds
c
         if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
            radcur( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 2, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 3, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 4, m ) = float( tauData%aRecord(m)%radii(1))
c
         elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
            radcur( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 2, m ) = float( tauData%aRecord(m)%radii(2))
            radcur( 3, m ) = float( tauData%aRecord(m)%radii(3))
            radcur( 4, m ) = float( tauData%aRecord(m)%radii(4))
c
         endif
      enddo
cxxxxxx   end of insert ... sampson aug 08
c
c**   Get the forecast data for desired techinques all TAUs
c
         do 100 i = 1, nmodel
c
            call getTech ( aidsData, fst_tech(i), aidData, result )
            if ( result .eq. 0 ) goto 100
c
            do 90 j = 1, ntau
               call getSingleTAU ( aidData, tau(j), tauData, result )
               if ( result .eq. 0 ) then
c
c**   If there is no initial 0 TAU for the technique, use the CARQ data
c
                  if ( tau(j) .eq. 0 ) then
                     last_tau00(i) = last_tau00(i) + 1
                     fst00lat( last_tau00(i), i ) = latcur
                     fst00lon( last_tau00(i), i ) = loncur
                     fst00spd( last_tau00(i), i ) = spdcur
                     fst00tau( last_tau00(i), i ) = float( tau(j) )
                     go to 90
                  else
                     go to 90
                  endif
               endif
c
               last_tau00(i) = last_tau00(i) + 1
               fst00lat( last_tau00(i), i ) =
     &           alat180( tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
               fst00lon( last_tau00(i), i ) =
     &           alon360( tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
               fst00spd( last_tau00(i), i ) = tauData%aRecord(1)%vmax
               fst00tau( last_tau00(i), i ) = float( tau(j) )
c
 90         continue
 100     continue

c
c**   Read the current 34, 50 and 64-knot wind radii
c
      do m = 1, tauData%numrcrds
c
         if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
            radcur( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 2, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 3, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 4, m ) = float( tauData%aRecord(m)%radii(1))
c
         elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
            radcur( 1, m ) = float( tauData%aRecord(m)%radii(1))
            radcur( 2, m ) = float( tauData%aRecord(m)%radii(2))
            radcur( 3, m ) = float( tauData%aRecord(m)%radii(3))
            radcur( 4, m ) = float( tauData%aRecord(m)%radii(4))
c
         endif
      enddo
c
c**   For diagnostics, write out the input for interpolation
c
cx    nowrite = 0
      nowrite = 1
      if ( nowrite .eq. 1 ) goto 110
c
      write (*,'('' '')')
      write (*,'(''CARQ   '', a10)') dtgcur
      write (*,'(9f6.1)') latm12, lonm12, spdm12, latm06, lonm06,
     &                       spdm06, latcur, loncur, spdcur
      write (*,'(12f6.1)') (( radm12( n, m ), n = 1, 4 ), m = 1, 3 )
      write (*,'(12f6.1)') (( radm06( n, m ), n = 1, 4 ), m = 1, 3 )
      write (*,'(12f6.1)') (( radcur( n, m ), n = 1, 4 ), m = 1, 3 )
      write (*,'('' '')')
      do i = 1, nmodel
         write (*,'(a4,2i10,5x,a10)') fst_tech(i), last_tau12(i),
     &                                last_tau06(i), dtgcur
         write (*,'('' '')')
      write (*,'(''M12   '', a10)') dtgm12
         write (*,'(a4,3x, a10)') fst_tech(i), dtgm12
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst12lat(j,i), j = 1, last_tau12(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst12lon(j,i), j = 1, last_tau12(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst12spd(j,i), j = 1, last_tau12(i) )
         do j = 1, last_tau12(i)
            write (*,'(4x, i5, 12f6.1)') j,
     &           ((fst12rad(j,n,m,i), n = 1, 4), m = 1, 3)
         enddo
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst12tau(j,i), j = 1, last_tau12(i) )
         write (*,'('' '')')
         write (*,'(''M06   '', a10)') dtgm06
         write (*,'(a4, 3x, a10)') fst_tech(i), dtgm06
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst06lat(j,i), j = 1, last_tau06(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst06lon(j,i), j = 1, last_tau06(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst06spd(j,i), j = 1, last_tau06(i) )
         do j = 1, last_tau06(i)
            write (*,'(4x, i5, 12f6.1)') j,
     &           ((fst06rad(j,n,m,i), n = 1, 4), m = 1, 3)
         enddo
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst06tau(j,i), j = 1, last_tau06(i) )
         write (*,'('' '')')
         write (*,'(''M00   '', a10)') dtgm00
         write (*,'(a4, 3x, a10)') fst_tech(i), dtgm00
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst00lat(j,i), j = 1, last_tau00(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst00lon(j,i), j = 1, last_tau00(i) )
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst00spd(j,i), j = 1, last_tau00(i) )
         do j = 1, last_tau00(i)
            write (*,'(4x, i5, 12f6.1)') j,
     &           ((fst00rad(j,n,m,i), n = 1, 4), m = 1, 3)
         enddo
         write (*,'(a4, 31f6.1)') fst_tech(i),
     &           (fst00tau(j,i), j = 1, last_tau00(i) )
         write (*,'('' '')')
      enddo
c
 110  continue
c
      return
      end
c***************************************************************************
      subroutine intrfcst ( strmid, dtgcur, nmodel )
c
c**   Interpolate the non-blank forecasts
c
      parameter ( nmodmax=1000, ntau=31, nits = ntau*2 - 1, ltau=15 )
      parameter ( taurecmax=20 )
c
c
      common /techs/ fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /adjust/iadj(nmodmax),jadj(nmodmax)
      common /forecast/ fst00lat(ntau,nmodmax), fst00lon(ntau,nmodmax),
     &                  fst00spd(ntau,nmodmax), fst00tau(ntau,nmodmax),
     &                  fst00rad(ntau,4,taurecmax,nmodmax),
     &                  fst06lat(ntau,nmodmax), fst06lon(ntau,nmodmax),
     &                  fst06spd(ntau,nmodmax), fst06tau(ntau,nmodmax),
     &                  fst06rad(ntau,4,taurecmax,nmodmax),
     &                  fst12lat(ntau,nmodmax), fst12lon(ntau,nmodmax),
     &                  fst12spd(ntau,nmodmax), fst12tau(ntau,nmodmax),
     &                  fst12rad(ntau,4,taurecmax,nmodmax),
     &                  last_tau00(nmodmax),
     &                  last_tau06(nmodmax), last_tau12(nmodmax)
      common /initial/  latcur, loncur, spdcur, radcur
      common /intrp/    intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &                  intspd(ltau,nmodmax), intrad(ltau,4,3,nmodmax)
      common /files/iaids_file, istd_out, iconsensus_in
c
      real intlat, intlon, intspd, intrad
      real fst00lat, fst00lon, fst00spd, fst00tau
      real fst06lat, fst06lon, fst06spd, fst06tau
      real fst12lat, fst12lon, fst12spd, fst12tau
      real fstne34(ntau,nmodmax),fstse34(ntau,nmodmax),
     &     fstsw34(ntau,nmodmax),fstnw34(ntau,nmodmax)
      real fstne50(ntau,nmodmax),fstse50(ntau,nmodmax),
     &     fstsw50(ntau,nmodmax),fstnw50(ntau,nmodmax)
      real fstne64(ntau,nmodmax),fstse64(ntau,nmodmax),
     &     fstsw64(ntau,nmodmax),fstnw64(ntau,nmodmax)
c
      integer last_tau00, last_tau06, last_tau12
      real latcur, loncur, spdcur, radcur( 4, 3 )
      real offset_lat, offset_lon, offset_spd, offset_rad( 4, 3 )
      real intrlat(nits), intrlon(nits), intrspd(nits),intrrad(nits,4,3)
c
      real intrne34(nits),intrse34(nits),
     &     intrsw34(nits),intrnw34(nits)
      real intrne50(nits),intrse50(nits),
     &     intrsw50(nits),intrnw50(nits)
      real intrne64(nits),intrse64(nits),
     &     intrsw64(nits),intrnw64(nits)
c
      real atau(ltau)
      integer iaids_file, istd_out, iconsensus_in
      real dst(nmodmax)
c
      character*4   fst_tech, tech_12, tech_06
      character*8   strmid
      character*10  dtgcur
c
      data atau /  0.0, 12.0, 24.0, 36.0,  48.0,  60.0,
     &                  72.0, 84.0, 96.0, 108.0, 120.0,
     &                 132.0,144.0,156.0, 168.0/
c
C**   DEFINE INTERNAL FUNCTION for extrapolation
C
cx    FI(T,A,B,C) = (2.0*A + T*(4.0*B - C - 3.0*A +
cx   & T*(C - 2.0*B + A)))/2.0
cx    simplified to linear extrapolation
      FI(T,A,B,C) = C + (C - B)
c
c**   Define interpolation flags
c
      iflag  =  1   !  Determine interpolation coefficients everytime
      lflag  =  0   !  0 = linear interpolation,  1 = spline interpolation
      lfilter=  1   !  0 = no filtering,          1 = filtering
      ntimes = 10   !  Number of 3-point center weighted filter passes
c
c**   Zero interpolation arrays
c

      do i = 1, nmodel
         do k = 1, ltau
            intlat( k, i ) = 0.0
            intlon( k, i ) = 0.0
            intspd( k, i ) = 0.0
c
            do m = 1, 3
               do n = 1, 4
                  intrad( k, n, m, i ) = 0.0
               enddo
            enddo
c
         enddo
      enddo
c
c**   Do the 12-hour interpolation, first
c
      do i = 1, nmodel
c
cx      if ( last_tau12(i) .gt. 3 ) then
cx      if ( last_tau12(i) .gt. 1 ) then
cx      if ( last_tau12(i) .gt. 1 .and. tech_12(i).ne. 'XXXX') then
        if ( tech_12(i).eq. 'XXXX' .or. tech_12(i).eq.fst_tech(i) ) then
cx          write (*,*) 'no 12-hour interpolation for ', fst_tech(i)
        elseif ( last_tau12(i) .gt. 1 ) then
            last_tau = int( fst12tau( last_tau12(i), i ) )
c
c**   Make input minus 12 forecast wind radii arrays easier to handle
c
         do k = 1, ntau
            fstne34( k, i ) = fst12rad( k, 1, 1, i )
            fstse34( k, i ) = fst12rad( k, 2, 1, i )
            fstsw34( k, i ) = fst12rad( k, 3, 1, i )
            fstnw34( k, i ) = fst12rad( k, 4, 1, i )
            fstne50( k, i ) = fst12rad( k, 1, 2, i )
            fstse50( k, i ) = fst12rad( k, 2, 2, i )
            fstsw50( k, i ) = fst12rad( k, 3, 2, i )
            fstnw50( k, i ) = fst12rad( k, 4, 2, i )
            fstne64( k, i ) = fst12rad( k, 1, 3, i )
            fstse64( k, i ) = fst12rad( k, 2, 3, i )
            fstsw64( k, i ) = fst12rad( k, 3, 3, i )
            fstnw64( k, i ) = fst12rad( k, 4, 3, i )
         enddo
c
         j = 0
c
         do k = 0, last_tau, 3
c
            j = j + 1
c
            call mspline ( fst12tau( 1, i ), fst12lat( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrlat(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lat12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fst12lon( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrlon(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lon12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fst12spd( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrspd(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..spd12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstne34( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrne34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstse34( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrse34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstsw34( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrsw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstnw34( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrnw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstne50( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrne50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstse50( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrse50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstsw50( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrsw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstnw50( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrnw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstne64( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrne64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstse64( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrse64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstsw64( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrsw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst12tau( 1, i ), fstnw64( 1, i ),
     &           last_tau12(i), iflag, lflag, float(k),
     &           intrnw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw64 rad12..',
     &           dtgcur, fst_tech(i), k
c
          enddo
c
          jlast = j
c
c**   Filter interpolated points
c
          if (lfilter .eq. 1) then
            call filter ( ntimes, jlast, intrlat )
            call filter ( ntimes, jlast, intrlon )
            call filter ( ntimes, jlast, intrspd )
            call filter ( ntimes, jlast, intrne34 )
            call filter ( ntimes, jlast, intrse34 )
            call filter ( ntimes, jlast, intrsw34 )
            call filter ( ntimes, jlast, intrnw34 )
            call filter ( ntimes, jlast, intrne50 )
            call filter ( ntimes, jlast, intrse50 )
            call filter ( ntimes, jlast, intrsw50 )
            call filter ( ntimes, jlast, intrnw50 )
            call filter ( ntimes, jlast, intrne64 )
            call filter ( ntimes, jlast, intrse64 )
            call filter ( ntimes, jlast, intrsw64 )
            call filter ( ntimes, jlast, intrnw64 )
          endif
c
c**   Put the radii back in a more convenient form
c
          do k = 1, nits
             intrrad( k, 1, 1 ) = intrne34( k )
             intrrad( k, 2, 1 ) = intrse34( k )
             intrrad( k, 3, 1 ) = intrsw34( k )
             intrrad( k, 4, 1 ) = intrnw34( k )
             intrrad( k, 1, 2 ) = intrne50( k )
             intrrad( k, 2, 2 ) = intrse50( k )
             intrrad( k, 3, 2 ) = intrsw50( k )
             intrrad( k, 4, 2 ) = intrnw50( k )
             intrrad( k, 1, 3 ) = intrne64( k )
             intrrad( k, 2, 3 ) = intrse64( k )
             intrrad( k, 3, 3 ) = intrsw64( k )
             intrrad( k, 4, 3 ) = intrnw64( k )
          enddo
c
c
c**   Offset the filtered interpolated forecast for the current
c*       initial values
c
          k = 0
c
          do j = 5, jlast, 4
c
             if ( j .eq. 5 ) then
                offset_lat = latcur - intrlat(j)
                offset_lon = loncur - intrlon(j)
                offset_spd = spdcur - intrspd(j)
c
c  added for sanity check of model lat/lon near best track lat/lon *********
                call dirdst(latcur,loncur,intrlat(j),intrlon(j),
     &                      dir,dst(i))
c
                do m = 1, 3
                   do n = 1, 4
                      offset_rad( n, m ) = radcur(n,m) - intrrad(j,n,m)
                   enddo
                enddo
c
             endif
c
             k = k + 1
c

cx       intensity adjustment section
cx
cx       adjustment for aids values left blank in input file
            if    (jadj(i) .le. 0 ) then
               spdfac=1.0
cx       adjustment for aids with improper input in input file
            elseif(jadj(k) .lt. iadj(i)) then
               spdfac=1.0
cx       full adjustment for tau prior to initial phase out
            elseif(atau(k) .le. iadj(i)) then
               spdfac=1.0
cx       partial adjustment for tau after initial phase out
            elseif( atau(k).gt.iadj(i) .and. atau(k).lt.jadj(i) ) then
               spdfac = 1.0 - (atau(k) - iadj(i)) / (jadj(i) - iadj(i))
cx       no adjustment for tau at or after final phase out
            elseif(atau(k) .ge. jadj(i)) then
               spdfac=0.0
cx       full adjustment for strange cases
            else
               spdfac=1.0
            endif
cx
cx       end of intensity adjustment section

c
            intlat ( k, i ) = offset_lat        + intrlat(j)
            intlon ( k, i ) = offset_lon        + intrlon(j)
cx          intlat ( k, i ) = offset_lat*spdfac + intrlat(j)
cx          intlon ( k, i ) = offset_lon*spdfac + intrlon(j)
            intspd ( k, i ) = offset_spd*spdfac + intrspd(j)
c
            if ( intspd ( k, i ) .lt. 0.0 ) intspd ( k, i ) = 0.0
c
             do m = 1, 3
                do n = 1, 4
cx                 if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) =
cx   &                  offset_rad(n,m) + intrrad(j,n,m)
                   if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) =
     &                  offset_rad(n,m)*spdfac + intrrad(j,n,m)
c
                   if ( intspd ( k, i ) .lt. 34.0 .and. m .eq. 1)
     &                  intrad ( k, n, m, i ) = 0.0
                   if ( intspd ( k, i ) .lt. 50.0 .and. m .eq. 2)
     &                  intrad ( k, n, m, i ) = 0.0
                   if ( intspd ( k, i ) .lt. 64.0 .and. m .eq. 3)
     &                  intrad ( k, n, m, i ) = 0.0
c
                   if ( m.eq.2 .and. intrad(k,n,m,i).gt.intrad(k,n,1,i))
     &                  intrad( k,n,m,i ) = 0.6*intrad( k,n,1,i )
                   if ( m.eq.3 .and. intrad(k,n,m,i).gt.intrad(k,n,2,i))
     &                  intrad( k,n,m,i ) = 0.6*intrad( k,n,2,i )
c
                   if ( intrad(k,n,m,i) .lt. 0.0 ) intrad(k,n,m,i) = 0.0
c
                enddo
             enddo
c
          enddo
c
          klast = k
c
c**   If the initial 12-hour forecast is zero (i.e. no intensity
c**     forecasts for this model), zero them out since the interpolated
c**      intensities were generated only from the offset.
c
          if ( fst12spd( 1, i ) .lt. 0.01 ) then
             do m = 1, klast
                intspd( m, i ) = 0.0
             enddo
          endif
c
c**   Extrapolate the offset interpolated forecast 12 hours
c
          if ( klast .gt. 2 .and. klast .lt. ltau ) then
             intlat( klast + 1, i) = fi( 3.0, intlat( klast - 2, i ),
     &            intlat( klast - 1, i ), intlat( klast, i ) )
             intlon( klast + 1, i) = fi( 3.0, intlon( klast - 2, i ),
     &            intlon( klast - 1, i ), intlon( klast, i ) )
cx    this was modified to be a persistence value due to unrealistic extrapolation
             intspd( klast + 1, i) = intspd( klast - 1, i )
c
             if ( intspd( klast+1,i ) .lt. 0.0 ) intspd(klast+1,i) = 0.0
c
             if ( i .gt. 1 ) then
c
                do m = 1, 3
                   do n = 1, 4
c
                      intrad ( klast + 1, n, m, i ) = fi( 3.0,
     &                     intrad( klast - 2, n, m, i ),
     &                     intrad( klast - 1, n, m, i ),
     &                     intrad( klast,     n, m, i ))
c
                      if ( intspd ( klast + 1, i ).lt.34.0 .and. m.eq.1)
     &                     intrad ( klast + 1, n, m, i ) = 0.0
                      if ( intspd ( klast + 1, i ).lt.50.0 .and. m.eq.2)
     &                     intrad ( klast + 1, n, m, i ) = 0.0
                      if ( intspd ( klast + 1, i ).lt.64.0 .and. m.eq.3)
     &                     intrad ( klast + 1, n, m, i ) = 0.0
c
                      if ( m.eq.2 .and. intrad( klast + 1, n, m, i ).gt.
     &                     intrad( klast + 1, n, 1, i ))
     &                     intrad( klast + 1, n, 2, i ) =
     &                     0.6*intrad( klast + 1, n, 1, i )
                      if ( m.eq.3 .and. intrad( klast + 1, n, m, i ).gt.
     &                     intrad( klast + 1, n, 2, i ))
     &                     intrad( klast + 1, n, 3, i ) =
     &                     0.6*intrad( klast + 1, n, 2, i )
c
                      if ( intrad( klast + 1, n, m, i ) .lt. 0.0 )
     &                     intrad( klast + 1, n, m, i ) = 0.0
c
                   enddo
                enddo
             endif
          endif
c
c**   For diagnostics, write out the 12-hour interpolated forecast
c
cx        nowrite = 0
          nowrite = 1
          if ( nowrite .eq. 1 ) goto 10
c
          write (*,'('' 12-hour interpolated forecast '')')
      write (*,'('' '')')
          write (*,'(a4, 3x, a10, 3f6.1)') tech_12(i), dtgcur,
     &                              offset_lat, offset_lon, offset_spd
      write (*,'('' '')')
      write (*,'(a4, 11f6.1)') tech_12(i), (intlat(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrlat(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') tech_12(i), (intlon(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrlon(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') tech_12(i), (intspd(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrspd(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') fst_tech(i), (atau(j),     j = 1, ltau )
      write (*,'('' '')')
          write (*,'(a4, 12f6.1)')
     &        tech_12(i), ((offset_rad( n, m ), n = 1, 4), m = 1, 3 )
      do j = 1, ltau
         write (*,'(f6.1, 4x, a4, 12f6.1)') atau(j),
     &        tech_12(i), ((intrad( j, n, m, i ), n = 1, 4), m = 1, 3 )
      enddo
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw34(j), j = 1, 45 )
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw50(j), j = 1, 45 )
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw64(j), j = 1, 45 )
c
 10       continue
c
        endif
      enddo
c
c**   Do the 6-hour interpolation, second
c
      do i = 1, nmodel
c
cx      if (last_tau06(i) .gt. 3 ) then
cx      if (last_tau06(i) .gt. 1 ) then
        if ( tech_06(i).eq. 'XXXX' .or. tech_06(i).eq.fst_tech(i) ) then
cx          write (*,*) 'no 6-hour interpolation for ', fst_tech(i)
        elseif ( last_tau06(i) .gt. 1 ) then
c
c**   There maybe a 12-hour interpolation for this technique, so zero it
c
           do kk = 1, ltau
             intlat( kk, i ) = 0.0
             intlon( kk, i ) = 0.0
             intspd( kk, i ) = 0.0
c
             do m = 1, 3
                do n = 1, 4
                   intrad( kk, n, m, i ) = 0.0
                enddo
             enddo
c
           enddo
c
c
c**   Make input minus 06 forecast wind radii arrays easier to handle
c
         do k = 1, ntau
            fstne34( k, i ) = fst06rad( k, 1, 1, i )
            fstse34( k, i ) = fst06rad( k, 2, 1, i )
            fstsw34( k, i ) = fst06rad( k, 3, 1, i )
            fstnw34( k, i ) = fst06rad( k, 4, 1, i )
            fstne50( k, i ) = fst06rad( k, 1, 2, i )
            fstse50( k, i ) = fst06rad( k, 2, 2, i )
            fstsw50( k, i ) = fst06rad( k, 3, 2, i )
            fstnw50( k, i ) = fst06rad( k, 4, 2, i )
            fstne64( k, i ) = fst06rad( k, 1, 3, i )
            fstse64( k, i ) = fst06rad( k, 2, 3, i )
            fstsw64( k, i ) = fst06rad( k, 3, 3, i )
            fstnw64( k, i ) = fst06rad( k, 4, 3, i )
         enddo
c
         last_tau = int( fst06tau( last_tau06(i), i ) )
         j = 0
c
         do k = 0, last_tau, 3
c
            j = j + 1
c
            call mspline ( fst06tau( 1, i ), fst06lat( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrlat(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lat06..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fst06lon( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &                 intrlon(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lon06..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fst06spd( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrspd(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..spd06..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstne34( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrne34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstse34( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrse34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstsw34( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrsw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstnw34( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrnw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw34 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstne50( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrne50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstse50( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrse50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstsw50( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrsw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstnw50( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrnw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw50 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstne64( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrne64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstse64( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrse64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstsw64( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrsw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw64 rad12..',
     &           dtgcur, fst_tech(i), k
c
            call mspline ( fst06tau( 1, i ), fstnw64( 1, i ),
     &           last_tau06(i), iflag, lflag, float(k),
     &           intrnw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw64 rad12..',
     &           dtgcur, fst_tech(i), k
c
         enddo
c
         jlast = j
c
c**   Filter interpolated points
c
         if (lfilter .eq. 1) then
           call filter ( ntimes, jlast, intrlat )
           call filter ( ntimes, jlast, intrlon )
           call filter ( ntimes, jlast, intrspd )
           call filter ( ntimes, jlast, intrne34 )
           call filter ( ntimes, jlast, intrse34 )
           call filter ( ntimes, jlast, intrsw34 )
           call filter ( ntimes, jlast, intrnw34 )
           call filter ( ntimes, jlast, intrne50 )
           call filter ( ntimes, jlast, intrse50 )
           call filter ( ntimes, jlast, intrsw50 )
           call filter ( ntimes, jlast, intrnw50 )
           call filter ( ntimes, jlast, intrne64 )
           call filter ( ntimes, jlast, intrse64 )
           call filter ( ntimes, jlast, intrsw64 )
           call filter ( ntimes, jlast, intrnw64 )
         endif
c
c**   Put the radii back in a more convenient form
c
          do k = 1, nits
             intrrad( k, 1, 1 ) = intrne34( k )
             intrrad( k, 2, 1 ) = intrse34( k )
             intrrad( k, 3, 1 ) = intrsw34( k )
             intrrad( k, 4, 1 ) = intrnw34( k )
             intrrad( k, 1, 2 ) = intrne50( k )
             intrrad( k, 2, 2 ) = intrse50( k )
             intrrad( k, 3, 2 ) = intrsw50( k )
             intrrad( k, 4, 2 ) = intrnw50( k )
             intrrad( k, 1, 3 ) = intrne64( k )
             intrrad( k, 2, 3 ) = intrse64( k )
             intrrad( k, 3, 3 ) = intrsw64( k )
             intrrad( k, 4, 3 ) = intrnw64( k )
          enddo
c
c**   Offset the filtered interpolated forecast for the current
c**      initial values
c
         k = 0
c
         do j = 3, jlast, 4

            if ( j .eq. 3 ) then
               offset_lat = latcur - intrlat(j)
               offset_lon = loncur - intrlon(j)
               offset_spd = spdcur - intrspd(j)
c
c  added for sanity check of model lat/lon near best track lat/lon *********
                call dirdst(latcur,loncur,intrlat(j),intrlon(j),
     &                      dir,dst(i))
c
                do m = 1, 3
                   do n = 1, 4
                      offset_rad( n, m ) = radcur(n,m) - intrrad(j,n,m)
                   enddo
                enddo
c
            endif
c
            k = k + 1
c

cx       intensity adjustment section
cx
cx       adjustment for aids values left blank in input file
            if    (jadj(i) .le. 0 ) then
               spdfac=1.0
cx       adjustment for aids with improper input in input file
            elseif(jadj(i) .lt. iadj(i)) then
               spdfac=1.0
cx       full adjustment for tau prior to initial phase out
            elseif(atau(k) .le. iadj(i)) then
               spdfac=1.0
cx       partial adjustment for tau prior to initial phase out
            elseif( atau(k).gt.iadj(i) .and. atau(k).lt.jadj(i) ) then
               spdfac = 1.0 - (atau(k) - iadj(i)) / (jadj(i) - iadj(i))
cx       no adjustment for tau after final phase out
            elseif(atau(k) .ge. jadj(i)) then
               spdfac=0.0
cx       full adjustment for strange cases
            else
               spdfac=1.0
            endif
cx       end of intensity adjustment section
cx

c
            intlat ( k, i ) = offset_lat        + intrlat(j)
            intlon ( k, i ) = offset_lon        + intrlon(j)
cx          intlat ( k, i ) = offset_lat*spdfac + intrlat(j)
cx          intlon ( k, i ) = offset_lon*spdfac + intrlon(j)
            intspd ( k, i ) = offset_spd*spdfac + intrspd(j)
c
            if ( intspd ( k, i ) .lt. 0.0 ) intspd ( k, i ) = 0.0
c
             do m = 1, 3
                do n = 1, 4
cx                 if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) =
cx   &                  offset_rad(n,m) + intrrad(j,n,m)
                   if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) =
     &                  offset_rad(n,m)*spdfac + intrrad(j,n,m)
c
                   if ( intspd ( k, i ) .lt. 34.0 .and. m .eq. 1)
     &                  intrad ( k, n, m, i ) = 0.0
                   if ( intspd ( k, i ) .lt. 50.0 .and. m .eq. 2)
     &                  intrad ( k, n, m, i ) = 0.0
                   if ( intspd ( k, i ) .lt. 64.0 .and. m .eq. 3)
     &                  intrad ( k, n, m, i ) = 0.0
c
                   if ( m.eq.2 .and. intrad(k,n,m,i).gt.intrad(k,n,1,i))
     &                  intrad( k,n,m,i ) = 0.6*intrad( k,n,1,i )
                   if ( m.eq.3 .and. intrad(k,n,m,i).gt.intrad(k,n,2,i))
     &                  intrad( k,n,m,i ) = 0.6*intrad( k,n,2,i )
c
                   if ( intrad(k,n,m,i) .lt. 0.0 ) intrad(k,n,m,i) = 0.0
c
                enddo
             enddo
c
         enddo
c
         klast = k
c
c**   If the initial 6-hour forecast is zero (i.e. no intensity
c**     forecasts for this model), zero them out since the interpolated
c**      intensities were generated only from the offset.
c
         if ( fst06spd( 1, i ) .lt. 0.01 ) then
            do m = 1, klast
               intspd( m, i ) = 0.0
            enddo
         endif
c
c**   Extrapolate the offset interpolated forecast 6 hours
c
         if ( klast .gt. 2 .and. klast .lt. ltau ) then
            intlat( klast + 1, i) = fi( 3.0, intlat( klast - 2, i ),
     &           intlat( klast - 1, i ), intlat( klast, i ) )
            intlon( klast + 1, i) = fi( 3.0, intlon( klast - 2, i ),
     &           intlon( klast - 1, i ), intlon( klast, i ) )
            intspd( klast + 1, i) = fi( 3.0, intspd( klast - 2, i ),
     &           intspd( klast - 1, i ), intspd( klast, i ) )
c
            if ( intspd( klast + 1, i ) .lt. 0.0 )
     &           intspd( klast + 1, i ) = 0.0
c
            if ( i .gt. 1 ) then
c
               do m = 1, 3
                  do n = 1, 4
c
                     intrad( klast + 1, n, m, i ) = fi( 3.0,
     &                    intrad( klast - 2, n, m, i ),
     &                    intrad( klast - 1, n, m, i ),
     &                    intrad( klast,     n, m, i ))
c
                     if ( intspd( klast + 1, i ).lt.34.0 .and. m.eq.1)
     &                    intrad( klast + 1, n, m, i ) = 0.0
                     if ( intspd( klast + 1, i ).lt.50.0 .and. m.eq.2)
     &                    intrad( klast + 1, n, m, i ) = 0.0
                     if ( intspd( klast + 1, i ).lt.64.0 .and. m.eq.3)
     &                    intrad( klast + 1, n, m, i ) = 0.0
c
                     if ( m.eq.2 .and. intrad( klast + 1, n, m, i ) .gt.
     &                    intrad( klast + 1, n, 1, i ))
     &                    intrad( klast + 1, n, 2, i ) =
     &                    0.6*intrad( klast + 1, n, 1, i )
                     if ( m.eq.3 .and. intrad (klast + 1, n, m, i ) .gt.
     &                    intrad( klast + 1, n, 2, i ))
     &                    intrad( klast + 1, n, 3, i ) =
     &                    0.6*intrad( klast + 1, n, 2, i )
c
                     if ( intrad( klast + 1, n, m, i ).lt. 0.0 )
     &                    intrad( klast + 1, n, m, i ) = 0.0
c
                  enddo
               enddo
            endif
         endif
c
c**   For diagnostics, write out the 6-hour interpolated forecast
c
cx       nowrite = 0
         nowrite = 1
         if ( nowrite .eq. 1 ) goto 20
c
         write (*,'('' 6-hour interpolated forecast '')')
      write (*,'('' '')')
         write (*,'(a4, 3x, a10, 3f6.1)') tech_06(i), dtgcur,
     &                             offset_lat, offset_lon, offset_spd
      write (*,'('' '')')
      write (*,'(a4, 11f6.1)') tech_06(i), (intlat(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrlat(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') tech_06(i), (intlon(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrlon(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') tech_06(i), (intspd(j,i), j = 1, ltau )
      write (*,'(45f6.1)') (intrspd(j), j = 1, 45 )
      write (*,'(a4, 11f6.1)') fst_tech(i), (atau(j),     j = 1, ltau )
      write (*,'('' '')')
         write (*,'(a4, 12f6.1)')
     &        tech_06(i), ((offset_rad( n, m ), n = 1, 4), m = 1, 3 )
      do j = 1, ltau
         write (*,'(f6.1, 4x, a4, 12f6.1)') atau(j),
     &        tech_06(i), ((intrad( j, n, m, i ), n = 1, 4), m = 1, 3 )
      enddo
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw34(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw34(j), j = 1, 45 )
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw50(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw50(j), j = 1, 45 )
c
      write (*,'('' '')')
      write (*,'(45f6.1)') (intrne64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrse64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrsw64(j), j = 1, 45 )
      write (*,'(45f6.1)') (intrnw64(j), j = 1, 45 )
c
 20      continue
c
c**   06-hour interpolated forecasts check/write
c

cx     QC the resultant objective aid speeds.
          call speedck ( intlat(1,i), intlon(1,i), intspd(1,i) ,
     &                ltau, tech_06(i) )

cx     QC the resultant objective aid for extra-tropical transition.
cx     only for NGPS at the moment.  FNMOC is supposed to do this internally,
cx     but I haven't seen evidence of it yet.
cx        if (tech_06(i) .eq. 'NGPI' .or. tech_06(i) .eq. 'JNGI' ) then
cx           call excheck ( intlat(1,i), intlon(1,i), intspd(1,i) ,
cx   &                      ltau, tech_06(i) )
cx        endif
cx     check that the initial posit and 12 hour forecast are close
          if ( dst(i) < 180. ) then
c
c**   Write the 6-hour interpolated forecasts to the output file
c
           call new_write_out ( iaids_file, strmid, dtgcur, tech_06(i),
     &         intlat(1,i), intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )
           call new_write_out ( istd_out, strmid, dtgcur, tech_06(i),
     &         intlat(1,i), intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )
          else
           call zero1 ( intlat(1,i), ltau )
           call zero1 ( intlon(1,i), ltau )
           call zero1 ( intspd(1,i), ltau )
           write ( *, *)'Skipping interpolation for ', strmid
           write ( *, *)'DTG: ', dtgcur, ' tech:', tech_06(i)
           write ( *, *)'Init pos - 6hr fcst (',dst(i),'nm) too large'
           dst(i) = 0.0
          endif
c
        else
c
c**   12-hour interpolated forecasts check/write
c
cx   QC the resultant objective aid speeds.
         call speedck ( intlat(1,i), intlon(1,i), intspd(1,i) ,
     &                ltau, tech_12(i) )
cx     check that the initial posit and 12 hour forecast are close
         if ( dst(i) < 180. ) then
c
c**   Write the 12-hour interpolated forecasts to the output file
c
           call new_write_out ( iaids_file, strmid, dtgcur, tech_12(i),
     &         intlat(1,i), intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )
           call new_write_out ( istd_out, strmid, dtgcur, tech_12(i),
     &         intlat(1,i), intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )
         else
          call zero1 ( intlat(1,i), ltau )
          call zero1 ( intlon(1,i), ltau )
          call zero1 ( intspd(1,i), ltau )
          write ( *, *)'Skipping interpolation for ', strmid
          write ( *, *)'DTG: ', dtgcur, ' tech:', tech_12(i)
          write ( *, *)'Init pos - 12 hr fcst (',dst(i),'nm) too large'
          dst(i) = 0.0
         endif
c
        endif
c
      enddo
c
      return
      end
c********1*********2*********3*********4*********5*********6*********7**
      subroutine zero1( array, n )
c
c**   Zeros out a one dimensional array of size n
c
c     Programmer:  Sampson
c
      real array( n )
c
      do  i = 1, n
c
         array( i ) = 0.0
      end do
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine speedck( alat, alon, aspd, n, model )
c
c     Programmer:  Sampson
c**   Check the speeds of individual forecasts.  Can't be
c     greater than 60 knots.
c
      character*4 model
      real        alat(n), alon(n), aspd( n )
c
      do  i = 2, n-1
         call dirdst (alat(i),alon(i),alat(i+1),alon(i+1),dir,dst)
cx  forecasts every 12 hours, compute the allowable distance traveled
         discheck=60*12
         if ( dst .gt. discheck ) then
            if( abs(alat(i+1)) .gt. 0.01 ) then
              print *, model, ' tau= ', (i+1)*12,
     &            ' lat=',alat(i+1),' lon=',alon(i+1)
              print *, 'speed (', dst/12,') too large'
            endif
            call zero1 ( alat(i+1), n-i )
            call zero1 ( alon(i+1), n-i )
            call zero1 ( aspd(i+1), n-i )
         endif
c
      end do
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine excheck( alat, alon, aspd, n, model )
c
c     Programmer:  Sampson
c**   Check if the storm is going extra tropical.  Can't be
c     greater than 40 knots and 40.0 lat.
c
      character*4 model
      real        alat(n), alon(n), aspd( n )
c
      do  i = 2, n-1
         call dirdst (alat(i),alon(i),alat(i+1),alon(i+1),dir,dst)
cx  forecasts every 12 hours, compute the allowable distance traveled
         discheck=30*12
         if ( dst .gt. discheck .and. abs(alat(i+1)) .gt. 25.0) then
              print *, model, ' tau= ', (i+1)*12,
     &            ' lat=',alat(i+1),' lon=',alon(i+1)
              print *, 'extra-tropical, speed(', dst/12,') > 40'
              print *, 'and lat (', alat(i+1),') > 40'
            call zero1 ( alat(i+1), n-i )
            call zero1 ( alon(i+1), n-i )
            call zero1 ( aspd(i+1), n-i )
         endif
c
      end do
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      function alat180 ( alat90, ns )
c
c**   Transforms given latitude from 0 to 90 degrees to -90 to 90 degrees
c
      real alat90
c
      character*1 ns
c
      alat180 = alat90
c
      if ( ns .eq. "S" ) alat180 = - alat90
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      function alon360 ( alon180, ew )
c
c**   Transforms given longitude from 0 to 180 degrees to 0 to 360 degrees
c**       ( East > 180 )
c
      real alon180
c
      character*1 ew
c
      alon360 = alon180
c
      if ( ew .eq. "E" ) alon360 = 360.0 - alon180
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine filter ( ntimes, last, array )
c
c**   Use a three-point center weighted filter on an array, any
c**      number of times
c
      parameter ( ntau=31, nits = ntau*2 - 1 )
c
      real array(nits), filter_array(nits)
c
c**   Fix the end points
c
      filter_array(1) = array(1)
      filter_array(last) = array(last)
c
c**   Filter the array the number if times specified
c
      do ntime = 1, ntimes
c
c**   Do the filtering
c
         do n = 2, last - 1
            filter_array(n) = 0.25*array(n - 1) + 0.5*array(n) +
     &                        0.25*array(n + 1)
         enddo
c
c**   Replace the original array with the filtered array
c
         do n = 2, last - 1
            array(n)= filter_array(n)
         enddo
c
      enddo
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine write_out ( ifile, strmid, dtgcur, new_tech,
     &     intlat, intlon, intspd )
c
c**   Put the new objective aid in a writeable format
c
c     input variables
c                ifile = file id
c                strmid = storm id
c                dtgcur = current dtg
c                new_tech = technique name
c                intlat = array of latitudes in 10ths of degrees
c                intlon = array of longitudes in 10ths of degrees
c
c     sampson, nrl dec 15 2007

      parameter ( ltau=15 )
c
      real     intlat(ltau), intlon(ltau), intspd(ltau)
      integer  newfst(ltau,3)
      integer  ifile
c
      character*2  tech
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
      character*76 chfcst
c
c**   Zero the output array
c
      do ii = 1, ltau
         do jj = 1, 3
            newfst( ii, jj ) = 0
         enddo
      enddo
c
      do j = 1, ltau
c
         newfst( j, 1 ) = nint ( intlat( j )*10.0 )
         newfst( j, 2 ) = nint ( intlon( j )*10.0 )
         newfst( j, 3 ) = nint ( intspd( j ) )

      enddo
c
      call newWriteAidRcd ( ifile,  strmid, dtgcur, new_tech, newfst )
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine new_write_out ( ifile, strmid, dtgcur, new_tech,

     &     intlat, intlon, intspd, intrad )
c
c**   Put the new objective aid in a writeable format
c
c
c     input variables
c                ifile = file id
c                strmid = storm id
c                dtgcur = current dtg
c                new_tech = technique name
c                intlat = array of latitudes in 10ths of degrees
c                intlon = array of longitudes in 10ths of degrees
c
cx   had to put in development type for OFCI

      parameter ( ltau=15 )
c
cx   development type, only for OFCI
      common /devel/ ty(ltau)

      real     intlat(ltau), intlon(ltau), intspd(ltau)
      real     intrad(ltau,4,3)
      integer  result, newfst(ltau,3), newrad(ltau,4,4)
      integer  ifile
c
      character*2  ty
      character*2  tech
      character*3  wquad(ltau,4)
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
      character*76 chfcst
c
c**   Zero the output array
c
      do ii = 1, ltau
c
         if ( new_tech .ne. 'OFCI' ) then
            ty(ii)="  "
         endif
         do jj = 1, 3
            newfst( ii, jj ) = 0
         enddo
c
         do mm = 1, 4
            do nn = 1, 4
               wquad( ii, nn ) = 'NEQ'
               newrad( ii, nn, mm ) = 0
            enddo
         enddo
c
      enddo
c
c**   Fill output array with the interpolated latitudes,
c**     longitudes, intensities and wind radii values
c
      do j = 1, ltau
c
         newfst( j, 1 ) = int ( intlat( j )*10.0 + 0.5 )
         newfst( j, 2 ) = int ( intlon( j )*10.0 + 0.5 )
c
         newfst( j, 3 ) = int ( intspd( j ) )
c
         do m = 1, 3
            do n = 1, 4
c
c**   For wind radii diagnostics,
c
cx             nowrite = 0
               nowrite = 1
               if (nowrite .eq. 1 ) goto 10

c
c**   Check that the wind speed supports quadrant wind radii
c
               if ( intspd(j).lt.34.0 .and. intrad( j,n,1) .gt. 0.0 )
     &              print *, 'vmaxl34 at dtgcur = ', dtgcur,
     &              intspd(j), intrad( j,n,1 )
               if ( intspd(j).lt.50.0 .and. intrad( j,n,2) .gt. 0.0 )
     &              print *, 'vmaxl50 at dtgcur = ', dtgcur,
     &              intspd(j), intrad( j,n,2 )
               if ( intspd(j).lt.64.0 .and. intrad( j,n,3) .gt. 0.0 )
     &              print *, 'vmaxl64 at dtgcur = ', dtgcur,
     &              intspd(j), intrad( j,n,3 )
c
c**   Check that there are no negative wind radii for each quadrant
c
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.1 .and. n.eq.1 )
     &  print *, 'n34rne at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.1 .and. n.eq.2 )
     &  print *, 'n34rse at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.1 .and. n.eq.3 )
     &  print *, 'n34rsw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.1 .and. n.eq.4 )
     &  print *, 'n34rnw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
c
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.2 .and. n.eq.1 )
     &  print *, 'n50rne at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.2 .and. n.eq.2 )
     &  print *, 'n50rse at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.2 .and. n.eq.3 )
     &  print *, 'n50rsw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.2 .and. n.eq.4 )
     &  print *, 'n50rnw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
c
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.3 .and. n.eq.1 )
     &  print *, 'n64rne at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.3 .and. n.eq.2 )
     &  print *, 'n64rse at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.3 .and. n.eq.3 )
     &  print *, 'n64rsw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
               if ( intrad( j,n,m ).lt.0.0 .and. m.eq.3 .and. n.eq.4 )
     &  print *, 'n64rnw at dtgcur and tau = ', dtgcur, j, intrad(j,n,m)
c
c**   Check that wind radii are within their thresholds for each quadrant
c
       if ( intrad (j, 1, 2).gt.intrad( j, 1, 1 ).and.m.eq.2.and.n.eq.1)
     & write ( *, '(''ne50rg34r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 1, 2 ), intrad( j, 1, 1 )
       if ( intrad (j, 2, 2).gt.intrad( j, 2, 1 ).and.m.eq.2.and.n.eq.2)
     & write ( *, '(''se50rg34r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 2, 2 ), intrad( j, 2, 1 )
       if ( intrad (j, 3, 2).gt.intrad( j, 3, 1 ).and.m.eq.2.and.n.eq.3)
     & write ( *, '(''sw50rg34r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 3, 2 ), intrad( j, 3, 1 )
       if ( intrad (j, 4, 2).gt.intrad( j, 4, 1 ).and.m.eq.2.and.n.eq.4)
     & write ( *, '(''nw50rg34r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 4, 2 ), intrad( j, 4, 1 )
c
       if ( intrad (j, 1, 3).gt.intrad( j, 1, 2 ).and.m.eq.3.and.n.eq.1)
     & write ( *, '(''ne64rg50r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 1, 3 ), intrad( j, 1, 2 )
       if ( intrad (j, 2, 3).gt.intrad( j, 2, 2 ).and.m.eq.3.and.n.eq.2)
     & write ( *, '(''ne64rg50r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 2, 3 ), intrad( j, 2, 2 )
       if ( intrad (j, 3, 3).gt.intrad( j, 3, 2 ).and.m.eq.3.and.n.eq.3)
     & write ( *, '(''ne64rg50r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 3, 3 ), intrad( j, 3, 2 )
       if ( intrad (j, 4, 3).gt.intrad( j, 4, 2 ).and.m.eq.3.and.n.eq.4)
     & write ( *, '(''ne64rg50r at dtgcur and tau = '',a10,i3,2f8.2)')
     &              dtgcur, j, intrad ( j, 4, 3 ), intrad( j, 4, 2 )
c
 10            continue
c
               newrad( j, m, n ) = int ( intrad( j, n, m ) )
c
            enddo
         enddo
      enddo
c
c**   Remove the effects of using persistence in the OFCL forecast
c**     from the OFCI forecast
c
      if ( new_tech .eq. 'OFCI' ) then
c
         do j = 5, ltau
            do n = 1, 4
               newrad( j, 3, n ) = 0
c
               if ( j .gt. 7 ) then
                  newrad( j, 1, n ) = 0
                  newrad( j, 2, n ) = 0
c
               endif
            enddo
         enddo
      endif
c
c**   Write the interpolated forecasts in the proper format
c
cx  only write aids for which new_tech is non-blank, not XXXX
cx    if(      new_tech.ne.'    '
cx   &  .and.  new_tech.ne.'XXXX'
cx   &  .and. (new_tech(4:4).eq.'I' .or. new_tech(4:4).eq.'2'
cx   &  .or.   new_tech(4:4).eq.'3' .or. new_tech(4:4).eq.'4'))then
      if(      new_tech.ne.'    ' .and.  new_tech.ne.'XXXX' ) then
         call convWriteAidData ( ifile,  strmid, dtgcur, new_tech,
     &     newfst, wquad, newrad, ty, result )
      endif
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine err_rad_out ( strmid, dtgcur, new_tech,
     &     intlat, intlon, err_rad )
c
c
      include 'dataioparms.inc'
      parameter ( ltau=15 )
c
      real     intlat(ltau), intlon(ltau), err_rad(ltau)
      integer  newfst(ltau,llw)
c
      character*2  tech
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
      character*76 chfcst
c
c**   Zero the output array
c
      do ii = 1, ltau
         do jj = 1, 3
            newfst( ii, jj ) = 0
         enddo
      enddo
c
      do j = 1, ltau
c
      if (j.ge.2.and.j.le.5.or.j.eq.7.or.j.eq.9.or.j.eq.11) then
         newfst( j, 1 ) = int ( intlat( j )*10.0 + 0.5 )
         newfst( j, 2 ) = int ( intlon( j )*10.0 + 0.5 )
         if ( err_rad( j ) .lt. 0 ) err_rad( j ) = 0
         newfst( j, 3 ) = int ( err_rad( j ) )
      endif

      enddo
c
      call newWriteErrRadRcd ( 32,  strmid, dtgcur, new_tech, newfst )
C      New routine for writing edeck records.  ajs  08/2010
C      call putEProbTR_GPCE (32,strmid,dtgcur,new_tech,newfst,iresult)
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine err_rad_int_out ( strmid, dtgcur, new_tech,
     &     intlat, intlon, err_rad, v_adj )
c
c
      include 'dataioparms.inc'
      parameter ( ltau=15 )
c
      real     intlat(ltau), intlon(ltau), err_rad(ltau), v_adj(ltau)
      integer  newfst(ltau,llw)
c
      character*2  tech
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
      character*76 chfcst
c
c**   Zero the output array
c
      do ii = 1, ltau
         do jj = 1, 3
            newfst( ii, jj ) = 0
         enddo
      enddo
c
      do j = 1, ltau
c
      if (j.ge.2.and.j.le.5.or.j.eq.7.or.j.eq.9.or.j.eq.11) then
         newfst( j, 1 ) = int ( intlat( j )*10.0 + 0.5 )
         newfst( j, 2 ) = int ( intlon( j )*10.0 + 0.5 )
         newfst( j, 3 ) = nint ( v_adj( j ) )
      endif

      enddo
c
       call putEProbIN_GPCE (32,strmid,dtgcur,new_tech,newfst,err_rad,
     &                       iresult)
c
      return
      end


c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine err_radx_out ( strmid, dtgcur, new_tech, gxlat, gxlon,
     &                    theta, gxcross, gxlong, bias_x, bias_a)
c
c
      include 'dataioparms.inc'
      parameter ( ltau=11 )
c
      real     gxlat(ltau),  gxlon(ltau), theta(ltau),  err_rad(ltau)
      real     gxcross(ltau), gxlong(ltau)
      real     bias_x(ltau), bias_a(ltau)
      integer  newfst(ltau,llwx)
      integer  pi
c
      character*2  tech
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
      character*76 chfcst
c
c**   Zero the output array
c
      do ii = 1, ltau
         do jj = 1, 3
            newfst( ii, jj ) = 0
         enddo
      enddo
c
      do j = 1, ltau
c
cx    if (j.ge.2.and.j.le.5.or.j.eq.7.or.j.eq.9.or.j.eq.11) then
cx       biased lat and lon
         newfst( j, 1 ) = int ( gxlat( j )*10.0 + 0.5 )
         newfst( j, 2 ) = int ( gxlon( j )*10.0 + 0.5 )
cx       probability percentile
         newfst( j, 3 ) = 70
cx       equivalent error radius (circle)
         newfst( j, 4 ) = sqrt ( gxcross ( j ) * gxlong ( j ) )
cx       direction of cross track error (math degrees)
         newfst( j, 5 ) = nint ( theta ( j ) )
cx       error radii (ellipse)
         newfst( j, 6 ) = nint ( gxcross( j ) )
         newfst( j, 7 ) = nint ( gxlong ( j ) )
cx       biases
         newfst( j, 8 ) = nint ( bias_x( j ) )
         newfst( j, 9 ) = nint ( bias_a( j ) )
cx    endif

      enddo
c
cx    call newWriteErrRadRcdx ( 32,  strmid, dtgcur, new_tech, newfst )
C      New routine for writing edeck records.  ajs  08/2010
       call putEProbTR_AX (32,strmid,dtgcur,new_tech,newfst,iresult)
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine mspline ( x, f, n, iflag, lflag, xi, fi, ierr )
c
c     This routine applies a quadratic interpolation procedure
c     to f(x) between x(1) and x(n). f(x) is assumed to be
c     represented by quadratic polynomials between the points
c     x(i). The polynomials are chosen so that they equal f(i)
c     at the points x(i), the first derviatives on either
c     side of the interior x(i) match at x(i), and the second
c     derivative of the approximated function integrated
c     over the domain is minimized.
c
c     This version is for interpolating longitude
c
c     Input:  x(1),x(2) ... x(n)      The x values (must be sequential)
c             f(1),f(2) ... f(n)      The function values
c             n                       The number of x,f pairs
c             iflag                   Flag for initialization
c                                      =1 for coefficient calculation
c                                      =0 to use previous coefficients
c             lflag                   Flag for linear interpolation
c                                      =0 to perform linear interpolation
c                                      =1 to perform quadratic interpolation
c             xi                      The x value at which to interpolate f
c
c     Output: fi                      The interpolated function value
c             ierr                    Error flag
c                                      =0  Normal return
c                                      =1  Parameter nmax is too small or n<2
c                                      =2  The x values are not sequential
c                                      =3  Coefficient iteration did not
c                                          converge
c                                      =4  Mix-up finding coefficients
c                                      =5  if xi .gt. x(n) or .lt. x(1),
c                                          xi is set to nearest endpoint
c                                          before the interpolation
c
c                                     Note: fi is set to -99.9 if
c                                           ierr=1,2,3 or 4
c
      parameter (nmax=100)
c
      dimension x(n),f(n)
c
c     Save variables
      dimension ax(nmax),bx(nmax),cx(nmax)
c
c     Temporary local variables
      dimension df(nmax),dx(nmax),gm(nmax),ct(nmax)
c
      save ax,bx,cx
c
c     Specify unit number for debug write statements
c     and debug flag
      idbug  = 0
      lutest = 6
      ierr   = 0
c
c     Specify minimum reduction in cost function for convergence
      thresh = 1.0e-10
c
c     Check to make sure nmax is large enough, and n is .gt. 1
      if (n .gt. nmax .or. n .lt. 2) then
         ierr=1
         fi = -99.9
         return
      endif
c
      if (iflag .eq. 1) then
c        Perform the initialization for later interpolation
c
c        Check to make sure x is sequential
         do 10 i=1,n-1
            if (x(i) .ge. x(i+1)) then
               ierr=2
               fi = -99.9
               return
            endif
   10    continue
c
c        Check for special case where n=2. Only linear interpolation
c        is possible.
         if (n .eq. 2) then
            cx(1) = 0.0
            bx(1) = (f(2)-f(1))/(x(2)-x(1))
            ax(1) = f(1) - bx(1)*x(1)
            go to 1500
         endif
c
c        Calculate x and f differences
         do 15 i=1,n-1
            df(i) = f(i+1)-f(i)
            dx(i) = x(i+1)-x(i)
   15    continue
c
c        Calculate domain size
         d = x(n) - x(1)
c
c        Check for linearity of input points
         eps = 1.0e-10
         bb = (f(2)-f(1))/(x(2)-x(1))
         aa = f(1) - bb*x(1)
         dev = 0.0
         do 12 i=3,n
            dev = dev + abs(aa + bb*x(i) - f(i))
   12    continue
c
         if (dev .lt. eps .or. lflag .eq. 0) then
            do 13 i=1,n-1
               cx(i) = 0.0
   13       continue
            go to 1000
         endif
c
c        Iterate to find the c-coefficients
         cx(1) = 0.0
         nit  = 100
         slt  = 0.01
         cfsave = 1.0e+10
c
         do 20 k=1,nit
c           Calculate c values
            do 25 i=2,n-1
               cx(i) = -cx(i-1)*dx(i-1)/dx(i)
     +                -df(i-1)/(dx(i)*dx(i-1))
     +                +df(i  )/(dx(i)*dx(i  ))
   25       continue
c
c           Calculate current value of cost function
            cf0 = 0.0
            do 26 i=1,n-1
               cf0 = cf0 + cx(i)*cx(i)*dx(i)
   26       continue
            cf0 = 0.5*cf0/d
c
            if (idbug .ne. 0) then
               write(lutest,101) cf0
  101          format(/,' cf0=',e13.6)
            endif
c
c           Check for convergence
            rel = abs(cf0 - cfsave)/abs(cfsave)
            if (rel .lt. thresh) go to 1000
            cfsave = cf0
c
c           Calculate values of Lagrange multipliers
            gm(n-1) = cx(n-1)*dx(n-1)/d
c
            if (n .gt. 3) then
               do 30 i=n-2,2,-1
                  gm(i) = cx(i)*dx(i)/d - gm(i+1)*dx(i)/dx(i+1)
   30          continue
            endif
c
c           Calculate gradient of cost function with respect to c1
            dsdc1 =  dx(1)*(cx(1)/d - gm(2)/dx(2))
c
c           Adjust cx(1) using trial step
            ct(1) = cx(1) - slt*dsdc1
c
c           Calculate remaining c values at trial step
            do 33 i=2,n-1
               ct(i) = -ct(i-1)*dx(i-1)/dx(i)
     +                 -df(i-1)/(dx(i)*dx(i-1))
     +                 +df(i  )/(dx(i)*dx(i  ))
   33       continue
c
c           Calculate cost function at trial step
            cft = 0.0
            do 31 i=1,n-1
               cft = cft + ct(i)*ct(i)*dx(i)
   31       continue
            cft = 0.5*cft/d
c
c            write(6,*) 'dsdc1,cft,cf0',dsdc1,cft,cf0
c           Calculate optimal step length and re-adjust cx(1)
            den = 2.0*((cft-cf0) + slt*dsdc1*dsdc1)
            if (den .ne. 0.0) then
               slo = dsdc1*dsdc1*slt*slt/den
            else
               slo =0.0
            endif
c
c           Adjust slo if desired
            slo = 1.0*slo
c
            cx(1) = cx(1) - slo*dsdc1
c
            if (idbug .ne. 0) then
               write(lutest,100) k,cft,slt,slo
  100          format(' Iteration=',i4,'  cf1=',e11.4,' slt=',e11.4,
     +                                                ' slo=',e11.4)
c
               do 99 j=1,n-1
                  write(lutest,102) j,cx(j)
  102             format('    i=',i2,' c=',f8.4)
   99          continue
            endif
c
c           Calculate trial step for next time step
            slt = 0.5*slo
   20    continue
c
c        Iteration did not converge
         ierr=3
         fi=-99.9
         return
c
c        Iteration converged
 1000    continue
c
         if (idbug .ne. 0) then
            write(lutest,104)
  104       format(/,' Iteration converged')
         endif
c
c        Calculate b and a coefficients
         do 40 i=1,n-1
            bx(i) = df(i)/dx(i) - cx(i)*(x(i+1) + x(i))
            ax(i) = f(i) - bx(i)*x(i) - cx(i)*x(i)*x(i)
   40    continue
      endif
c
 1500 continue
c     Interpolate the function
c
c     Check for xi out of bounds
      if (xi .lt. x(1)) then
         xi = x(1)
         ierr = 5
      endif
c
      if (xi .gt. x(n)) then
         xi = x(n)
         ierr = 5
      endif
c
c     Find the interval for the interpolation
      ii = 1
      do 50 i=2,n
         if (xi .le. x(i)) then
            ii = i-1
            go to 2000
         endif
   50 continue
c
      fi = -99.9
      ierr=4
      return
c
 2000 continue
      fi = ax(ii) + bx(ii)*xi + cx(ii)*xi*xi
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine comcon(strmid,dtgcur,dir,nmodel,
     &                  conname,contype,naids,nmin,cona,alt,alt2,alt3)
c
c**   Do a generic consenus forecast and write result to file
c**   This version does position, intensity and wind radii
c     Programmer:  Sampson
c
c     input variables:
c               strmid  - storm identifier (char, bbnnyyyy)
c               dtgcur  - current date-time-group (char, yyyymmddhh)
c               dir     - current direction of motion
c               nmodel  - number of input models interpolated (integer)
c               ntau    - number of taus (integer)
c               conname - 4 char consensus name
c               contype - 4 char consensus type
c               naids   - number of aids in consensus (integer)
c               nmin    - minimum number of aids in consensus (integer)
c               cona    - 4 char array of models making up consensus
c               alt     - 4 char array of alternate models
c               alt2    - 4 char array of second alternate models
c               alt3    - 4 char array of third alternate models
c     output variables:
c               none
c
      parameter ( nmodmax=1000, ltau=15 , ltauax=11, intrcnt=45 )
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /intrp/    intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &                  intspd(ltau,nmodmax), intrad(ltau,4,3,nmodmax)
      common /files/iaids_file, istd_out, iconsensus_in
c
      real intlat, intlon, intspd
      real intlag(ltau), intlog(ltau), intspg(ltau)
      real intrad
      real intrag(ltau,4,3), intspr(ltau)
      real spread(ltau), err_rad(ltau)
      real spreadi(ltau), err_radi(ltau), v_add(ltau)
      real clim_rad(ltau)
      real dir
      real corlat(ltau), corlon(ltau)
      real corr_lat(ltau), corr_lon(ltau)
      real sumdist, sumdisti
      integer ididrad, ididclimrad, ididelipse, ididradint

c   These are GPCE-AX cross track angles
      real theta(ltauax)
c   These are saved for GPCE-AX cross and along track probabilities
      real savlat(ltau,naids), savlon(ltau,naids)
c   These are the same arrays as above, only out to 120 h
      real savlatx(ltauax,naids), savlonx(ltauax,naids)
c   These are intlag, intlog and intspg for GPCE-AX (only out to 120 h)
      real intlagx(ltauax), intlogx(ltauax), intspgx(ltauax)
c   These are output from cross and along track probabilities
      real gxlat(ltauax),gxlon(ltauax),gxcross(ltauax),gxalong(ltauax)
      real bias_x(ltauax), bias_a(ltauax)
c
      integer ntot(ltau)
      integer ntoti(ltau)
      integer ntotr(ltau,4,3)
      integer numod(ltau)
      integer numodi(ltau)
      integer k,kk,kka,j
      integer is
      integer iaids_file, istd_out, iconsensus_in
c
      character*2  basin
      character*4  fst_tech, tech_12, tech_06
      character*4  conname, cona(nmodel)
      character*4  contype
      character*4  cor_tech, connamex
      character*4  alt(nmodel), alt2(nmodel), alt3(nmodel)
      character*8  strmid
      character*10 dtgcur
      character*200 aidstring, stringdash
      character*200 string0,stringresult,string1,string2,string3,string4
      character*200 pristring, altstring, al2string, al3string

	print*,'size of savlatx =',size(savlatx)
	print*,'size of savlonx =',size(savlonx)
	print*,'size of savlat =',size(savlat)
	print*,'size of savlon =',size(savlon)
        print*,'naids = ',naids
        print*,'ltau = ',ltau
        print*,'ltauax = ',ltauax

cx    zero out savlat and savlon
      do k = 1, naids
      do j = 1, ltau
            savlat(j,k)=-999.0
            savlon(j,k)=-999.0
      enddo
      enddo

cx    zero out savlatx and savlonx
      do k = 1, naids
      do j = 1, ltauax
            savlatx(j,k)=-999.0
            savlonx(j,k)=-999.0
      enddo
      enddo

c
c     run for all taus
c
      write(stringdash,'(a)')' ---------------------------------------'
      write(string0,'(a4,a,a10)')conname,' Aid Availability For ',dtgcur
      do j = 1, ltau
c
         ntot(j)    = 0
         ntoti(j)   = 0
         intlag(j)  = 0.0
         intlog(j)  = 0.0
         intspg(j)  = 0.0
         intspr(j)  = 99.0
         do m = 1, 4
         do n = 1, 3
            intrag(j,m,n)  = 0.0
            ntotr(j,m,n)   = 0
         enddo
         enddo
         spread(j)  = 0.0
         spreadi(j) = 0.0
         numod(j)   = 0
         numodi(j)  = 0
         is=1
cx     blank out strings
         do i = 1, 200
            aidstring(i:i)=' '
            string0(i:i)=' '
            string1(i:i)=' '
            string2(i:i)=' '
            string3(i:i)=' '
            string4(i:i)=' '
            pristring(i:i)=' '
            altstring(i:i)=' '
            al2string(i:i)=' '
            al3string(i:i)=' '
         enddo

         write (pristring(is:is+8),'(a9)')'primary: '
         write (altstring(is:is+8),'(a9)')'alt aid: '
         write (al2string(is:is+8),'(a9)')'2nd alt: '
         write (al3string(is:is+8),'(a9)')'3rd alt: '
         write (stringresult,'(a)')'Resultant Forecast '
         write (string1,'(a,a)')'Construct a Consensus: ', conname
         write (string2,'(a)')'1. XXXX means aid not available.      '
         write (string3,'(a)')'2. Aids could be 12-h interpolations. '
         write (string4,'(a)')'3. Use aids in following order:       '
         write (aidstring(is:is+8),'(a4,i4,1x)') 'tau=',(j-1)*12
         is=is+9
c
c     get input model and add to consensus
c
         do k = 1, naids

           call find_ind(cona(k),tech_06,nmodmax,kk)
           call find_ind( alt(k),tech_06,nmodmax,kka)
           call find_ind(alt2(k),tech_06,nmodmax,kk2)
           call find_ind(alt3(k),tech_06,nmodmax,kk3)
           write (pristring(is:is+4),'(a4,1x)') cona(k)
           write (altstring(is:is+4),'(a4,1x)') alt(k)
           write (al2string(is:is+4),'(a4,1x)') alt2(k)
           write (al3string(is:is+4),'(a4,1x)') alt3(k)
           write (aidstring(is:is+4),'(a5)') 'XXXX '
           if (kk .eq. 0) then
                   continue
           elseif(intlat(j,kk).ne.0.0 .and. intlon(j,kk).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   intlag(j) = intlag(j) + intlat(j,kk)
                   intlog(j) = intlog(j) + intlon(j,kk)
                   write (aidstring(is:is+4),'(a4,1x)') cona(k)
           elseif (kka .eq. 0) then
                   continue
           elseif(intlat(j,kka).ne.0.0 .and. intlon(j,kka).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   intlag(j) = intlag(j) + intlat(j,kka)
                   intlog(j) = intlog(j) + intlon(j,kka)
                   write (aidstring(is:is+4),'(a4,1x)') alt(k)
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intlat(j,kk2).ne.0.0 .and. intlon(j,kk2).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   intlag(j) = intlag(j) + intlat(j,kk2)
                   intlog(j) = intlog(j) + intlon(j,kk2)
                   write (aidstring(is:is+4),'(a4,1x)') alt2(k)
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intlat(j,kk3).ne.0.0 .and. intlon(j,kk3).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   intlag(j) = intlag(j) + intlat(j,kk3)
                   intlog(j) = intlog(j) + intlon(j,kk3)
                   write (aidstring(is:is+4),'(a4,1x)') alt2(k)
           endif

cx         intensity assignment
           if (kk .eq. 0) then
                   continue
           elseif(intspd(j,kk).gt.0.01)then
                   ntoti(j) = ntoti(j) + 1
                   intspg(j) = intspg(j) + intspd(j,kk)
           elseif (kka .eq. 0) then
                   continue
           elseif(intspd(j,kka).gt.0.01)then
                   ntoti(j) = ntoti(j) + 1
                   intspg(j) = intspg(j) + intspd(j,kka)
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intspd(j,kk2).gt.0.01)then
                   ntoti(j) = ntoti(j) + 1
                   intspg(j) = intspg(j) + intspd(j,kk2)
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intspd(j,kk3).gt.0.01)then
                   ntoti(j) = ntoti(j) + 1
                   intspg(j) = intspg(j) + intspd(j,kk3)
           endif
           is=is+5

cx         wind radii assignment, could be done several ways
           if (kk .eq. 0) then
                   continue
           elseif(intspd(j,kk).gt.35.0)then
                   do m = 1, 4
                   do n = 1, 3
                     if(intrad(j,m,n,kk).gt.0.0)then
                       ntotr(j,m,n)  = ntotr(j,m,n) + 1
                       intrag(j,m,n) = intrag(j,m,n) + intrad(j,m,n,kk)
                     endif
                   enddo
                   enddo
           elseif (kka .eq. 0) then
                   continue
           elseif(intspd(j,kka).gt.35.0)then
                   do m = 1, 4
                   do n = 1, 3
                     if(intrad(j,m,n,kka).gt.0.0)then
                       ntotr(j,m,n)  = ntotr(j,m,n) + 1
                       intrag(j,m,n) = intrag(j,m,n) + intrad(j,m,n,kka)
                     endif
                   enddo
                   enddo
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intspd(j,kk2).gt.35.0)then
                   do m = 1, 4
                   do n = 1, 3
                     if(intrad(j,m,n,kk2).gt.0.0)then
                       ntotr(j,m,n)  = ntotr(j,m,n) + 1
                       intrag(j,m,n) = intrag(j,m,n) + intrad(j,m,n,kk2)
                     endif
                   enddo
                   enddo
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intspd(j,kk3).gt.35.0)then
                   do m = 1, 4
                   do n = 1, 3
                     if(intrad(j,m,n,kk3).gt.0.0)then
                       ntotr(j,m,n)  = ntotr(j,m,n) + 1
                       intrag(j,m,n) = intrag(j,m,n) + intrad(j,m,n,kk3)
                     endif
                   enddo
                   enddo
           endif

         enddo
         if (j .eq. 1) then
            write (istd_out,*) '  '
            write (istd_out,*) '  '
            write (istd_out,*) '  '
            write (istd_out,*) '  '
            write (istd_out,'(a)')stringdash
            write (istd_out,'(a)')string0
            write (istd_out,'(a)')string1
            write (istd_out,'(a)')string2
            write (istd_out,'(a)')string3
            write (istd_out,'(a)')string4
            write (istd_out,'(a)')pristring
            write (istd_out,'(a)')altstring
            write (istd_out,'(a)')al2string
            write (istd_out,'(a)')al3string
            write (iconsensus_in,*) '  '
            write (iconsensus_in,*) '  '
            write (iconsensus_in,*) '  '
            write (iconsensus_in,*) '  '
            write (iconsensus_in,'(a)')stringdash
            write (iconsensus_in,'(a)')string0
            write (iconsensus_in,'(a)')string1
            write (iconsensus_in,'(a)')string2
            write (iconsensus_in,'(a)')string3
            write (iconsensus_in,'(a)')string4
            write (iconsensus_in,'(a)')pristring
            write (iconsensus_in,'(a)')altstring
            write (iconsensus_in,'(a)')al2string
            write (iconsensus_in,'(a)')al3string
         endif
c
c     compute resultant position consensus for a given tau
c
         if ( ntot(j) .ge. nmin .and. contype .ne. "INTE" .and.
     &        contype .ne. "RADI" ) then
               intlag(j) = intlag(j)/ntot(j)
               intlog(j) = intlog(j)/ntot(j)
         else
               ntot(j)   = 0
               intlag(j) = 0.0
               intlog(j) = 0.0
         endif
c
c     compute resultant intensity consensus for a given tau
c
         if ( ntoti(j) .ge. nmin .and. contype .ne. "TRAC" .and.
     &        contype .ne. "RADI" ) then
               intspg(j) = intspg(j)/ntoti(j)
         else
               ntoti(j)  = 0
               intspg(j) = 0.0
         endif
c
c     compute resultant radii consensus for a given tau
c
         if ( contype .eq. "RADI" ) then
               do m = 1, 4
               do n = 1, 3
                 if ( ntotr(j,m,n) .ge. nmin ) then
                   intrag(j,m,n) = intrag(j,m,n)/ntotr(j,m,n)
                 else
                   intrag(j,m,n) = 0.0
                 endif
               enddo
               enddo
         else
               do m = 1, 4
               do n = 1, 3
                  ntotr(j,m,n) = 0
                  intrag(j,m,n) = 0.0
               enddo
               enddo
         endif
c
c  accumulate track distance of each member from consensus forecast
c
        sumdist = 0.0
        dist    = 0.0
        ntot(j) = 0
        do k = 1,naids
           call find_ind(cona(k),tech_06,nmodmax,kk)
           call find_ind( alt(k),tech_06,nmodmax,kka)
           call find_ind(alt2(k),tech_06,nmodmax,kk2)
           call find_ind(alt3(k),tech_06,nmodmax,kk3)
           if (kk .eq. 0) then
                   continue
           elseif(intlat(j,kk).ne.0.0 .and. intlon(j,kk).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   savlat(j, ntot(j)) = intlat(j,kk)
                   savlon(j, ntot(j)) = intlon(j,kk)
                   avglat = 0.5*(intlat(j,kk)+intlag(j))
                   coslat= cos(avglat*pi/180.)
                   dlat = 60.*(intlat(j,kk)-intlag(j))
                   dlon = 60.*coslat*(intlon(j,kk)-intlog(j))
                   dist = sqrt(dlat**2 + dlon**2)
                   sumdist = sumdist + dist
           elseif (kka .eq. 0) then
                   continue
           elseif(intlat(j,kka).ne.0.0 .and. intlon(j,kka).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   savlat(j, ntot(j)) = intlat(j,kka)
                   savlon(j, ntot(j)) = intlon(j,kka)
                   avglat = 0.5*(intlat(j,kka)+intlag(j))
                   coslat= cos(avglat*pi/180.)
                   dlat = 60.*(intlat(j,kka)-intlag(j))
                   dlon = 60.*coslat*(intlon(j,kka)-intlog(j))
                   dist = sqrt(dlat**2 + dlon**2)
                   sumdist = sumdist + dist
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intlat(j,kk2).ne.0.0 .and. intlon(j,kk2).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   savlat(j, ntot(j)) = intlat(j,kk2)
                   savlon(j, ntot(j)) = intlon(j,kk2)
                   avglat = 0.5*(intlat(j,kk2)+intlag(j))
                   coslat= cos(avglat*pi/180.)
                   dlat = 60.*(intlat(j,kk2)-intlag(j))
                   dlon = 60.*coslat*(intlon(j,kk2)-intlog(j))
                   dist = sqrt(dlat**2 + dlon**2)
                   sumdist = sumdist + dist
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intlat(j,kk3).ne.0.0 .and. intlon(j,kk3).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   savlat(j, ntot(j)) = intlat(j,kk3)
                   savlon(j, ntot(j)) = intlon(j,kk3)
                   avglat = 0.5*(intlat(j,kk3)+intlag(j))
                   coslat= cos(avglat*pi/180.)
                   dlat = 60.*(intlat(j,kk3)-intlag(j))
                   dlon = 60.*coslat*(intlon(j,kk3)-intlog(j))
                   dist = sqrt(dlat**2 + dlon**2)
                   sumdist = sumdist + dist
           endif
c
        enddo
c
c  accumulate intensity delta of each member from consensus forecast
c
        sumdisti = 0.0
        dist     = 0.0
        ntoti(j) = 0
        do k = 1,naids
           call find_ind(cona(k),tech_06,nmodmax,kk)
           call find_ind( alt(k),tech_06,nmodmax,kka)
           call find_ind(alt2(k),tech_06,nmodmax,kk2)
           call find_ind(alt3(k),tech_06,nmodmax,kk3)
           if (kk .eq. 0) then
                   continue
           elseif(intspd(j,kk).gt.0.0 )then
                   ntoti(j) = ntoti(j) + 1
                   dist = abs (intspd(j,kk) - intspg(j))
                   sumdisti = sumdisti + dist
           elseif (kka .eq. 0) then
                   continue
           elseif(intspd(j,kka).gt.0.0 )then
                   ntoti(j) = ntoti(j) + 1
                   dist = abs (intspd(j,kka) - intspg(j))
                   sumdisti = sumdisti + dist
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intspd(j,kk2).gt.0.0 )then
                   ntoti(j) = ntoti(j) + 1
                   dist = abs (intspd(j,kk2) - intspg(j))
                   sumdisti = sumdisti + dist
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intspd(j,kk3).gt.0.0 )then
                   ntoti(j) = ntoti(j) + 1
                   dist = abs (intspd(j,kk3) - intspg(j))
                   sumdisti = sumdisti + dist
           endif
c
        enddo
c
c  Now compute track spread
c
        write (aidstring(is:is+14),'(a15)')
     &            't spread= n/a  '
        if ( ntot(j) .ge. nmin .and. intlag(j) .ne. 0.0) then
          spread(j) = sumdist / float(ntot(j))
          numod(j) = ntot(j)
          if (spread(j) .gt. 0.0) then
            write (aidstring(is:is+14),'(a11,f4.0)')
     &            't spread=', spread(j)
          endif
        endif
        is=is+16

c
c  Now compute intensity spread
c
        write (aidstring(is:is+14),'(a15)')
     &            'i spread= n/a  '
        if ( ntoti(j) .ge. nmin .and. intspg(j) .gt. 0.0 ) then
          spreadi(j) = sumdisti / float(ntoti(j))
          numodi(j) = ntoti(j)
          if (spreadi(j) .gt. 0.0) then
            write (aidstring(is:is+14),'(a11,f4.0)')
     &            'i spread=', spreadi(j)
          endif
        endif
        is=is+16
        write (istd_out,'(a)')aidstring
        write (iconsensus_in,'(a)')aidstring


      enddo
cx  end big aid loop

      ididrad    = -1
      ididclimrad= -1
      ididelipse = -1
      ididradint = -1
      basin = strmid(1:2)

cx    GPCE-AX is still out to 120 h, so need these shortened arrays
      do i = 1, ltauax
            intlagx(i) = intlag(i)
            intlogx(i) = intlog(i)
            intspgx(i) = intspg(i)
      enddo
c
cx      GPCE-AX is still out to 120 h, so need these shortened arrays
      do k = 1, naids
           do i = 1, ltauax
                 savlatx(i, k) = savlat(i, k)
                 savlonx(i, k) = savlon(i, k)
           enddo
      enddo

c  track GPCE
      if ( (basin .eq. 'al' .or. basin .eq. 'ep')
     & .and. (conname .eq. 'TCON' .or. conname .eq. 'TVCN') ) then

        call do_err_rad     ( conname,strmid,intlag,intlog,spread
     &                       ,numod,ididrad,err_rad)
        call do_err_ellipse(conname,strmid,intlagx,intlogx,intspgx,
     &                   naids,savlatx,savlonx,numod,
     &                   ididelipse, theta, gxlat, gxlon,
     &                   gxcross, gxalong, bias_x, bias_a
     &                       ,numod,ididrad,err_rad)
        call do_err_rad_clim( 'GPCC',strmid,ididclimrad,clim_rad)

      elseif ( (basin .eq. 'cp' ) .and.
     &         (conname .eq. 'CONW' .or. conname .eq. 'TESW' .or.
     &          conname .eq. 'TVCN') ) then
        call do_err_rad   ( conname,strmid,intlag,intlog,spread
     &                       ,numod,ididrad,err_rad)
        call do_err_rad_clim( 'GPCC',strmid,ididclimrad,clim_rad)

      elseif ( (basin .eq. 'wp' .or. basin .eq. 'sh') .and.
     &         (conname .eq. 'CONW' .or. conname .eq. 'TESW') ) then

        call do_err_rad_wp( conname,strmid,intlag,intlog,spread
     &                       ,numod,ididrad,err_rad)
        call do_err_ellipse(conname,strmid,intlagx,intlogx,intspgx,
     &                   naids,savlatx,savlonx,numod,
     &                   ididelipse, theta, gxlat, gxlon,
     &                   gxcross, gxalong, bias_x, bias_a
     &                       ,numod,ididrad,err_rad)
        call do_err_rad_clim( 'GPCC',strmid,ididclimrad,clim_rad)
      endif

c  intensity GPCE
      if ( basin .eq. 'wp' .and. conname .eq. 'S5YY'  ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'wp' .and. conname .eq. 'S5RI' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'al' .and. conname .eq. 'ICON' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'al' .and. conname .eq. 'IVCN' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'al' .and. conname .eq. 'IVRI' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'ep' .and. conname .eq. 'ICON' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'ep' .and. conname .eq. 'IVCN' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)
      else if ( basin .eq. 'ep' .and. conname .eq. 'IVRI' ) then

        call do_err_rad_int ( conname,strmid,intlag,intlog,intspg,
     &                        spreadi,numod,ididradint,err_radi, v_add)

      endif

c
c     Write out the new CON forecast
c
         write (istd_out,'(a)')stringresult
         call write_out ( iaids_file, strmid, dtgcur, conname,
     &                    intlag, intlog, intspg )
         call write_out ( iconsensus_in, strmid, dtgcur, conname,
     &                    intlag, intlog, intspg )
         call write_out ( istd_out, strmid, dtgcur, conname,
     &                    intlag, intlog, intspg )
         if ( contype .eq. "RADI" ) then
           write (istd_out,'(a)')"radii consensus"
           call new_write_out ( istd_out, strmid, dtgcur, conname,
     &                    intlag, intlog, intspr, intrag )
           call new_write_out ( iaids_file, strmid, dtgcur, conname,
     &                    intlag, intlog, intspr, intrag )
         endif
c
c     Write out the new CON error record
c
      if ( ididrad .eq. 1 ) then
         call err_rad_out ( strmid, dtgcur, conname,
     &                     intlag, intlog, err_rad )
      endif
      if ( ididclimrad .eq. 1 ) then
         call err_rad_out ( strmid, dtgcur, 'GPCC',
     &                     intlag, intlog, clim_rad )
      endif
      if ( ididelipse .eq. 1 ) then
         connamex = conname(1:3)//'X'
         call err_radx_out ( strmid, dtgcur, connamex, gxlat, gxlon,
     &                       theta, gxcross, gxalong, bias_x, bias_a)
      endif
      if ( ididradint .eq. 1 ) then
         call err_rad_int_out ( strmid, dtgcur, conname,
     &                     intlag, intlog, err_radi, v_add )
      endif

c
c**  Switch for corrected forecasts
c
      icorrect = 0
c
c**  Get correctors for the CONU forecasts
c
      if( (basin .eq. 'al' .or. basin .eq. 'ep') .and.
     &     (conname .eq. 'CONU' .or. conname .eq. 'TVCN') ) then
        call do_corr_con( conname, strmid, intlag, intlog, spread,
     &                  numod,corr_lat, corr_lon )
        icorrect = 1
c
c**  Get correctors for the GUNA forecasts
c
      else if( basin .eq. 'al'  .and.
     &         (conname .eq. 'GUNA' .or. conname .eq. 'TCON') ) then
        call do_corr_con( conname, strmid, intlag, intlog, spread,
     &                  numod,corr_lat, corr_lon )
        icorrect = 1
c
c**  Get correctors for the CONW forecasts
c
      else if( basin .eq. 'wp' .and. conname .eq. 'CONW') then
        call do_corr_con( conname, strmid, intlag, intlog, spread,
     &                  numod,corr_lat, corr_lon )
        icorrect = 1
c
c**  Get correctors for the CONW forecasts
c
      else if( basin .eq. 'wp' .and. conname .eq. 'TESW') then
        call do_corr_con( conname, strmid, intlag, intlog, spread,
     &                  numod,corr_lat, corr_lon )
        icorrect = 1
      endif
c
c** Apply correctors and write out corrected forecast
c
      if (icorrect .eq. 1) then
        do j=1,ltau
          corlat(j)=intlag(j)-corr_lat(j)
          corlon(j)=intlog(j)-corr_lon(j)
        enddo
        if (conname .eq. 'GUNA') cor_tech='CGUN'
        if (conname .eq. 'TCON') cor_tech='TCCN'
        if (conname .eq. 'CONU') cor_tech='CCON'
        if (conname .eq. 'TVCN') cor_tech='TVCC'
        if (conname .eq. 'CONW') cor_tech='CCON'
        if (conname .eq. 'TESW') cor_tech='CCON'
        call write_out ( iaids_file, strmid, dtgcur, cor_tech,
     &                   corlat, corlon, newspd )
        call write_out ( istd_out, strmid, dtgcur, cor_tech,
     &                   corlat, corlon, newspd )
      endif

      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine comconm(strmid,dtgcur,dir,nmodel,
     &                  conname,contype,naids,nmin,cona,alt,alt2,alt3)
c
c**   Do a generic consenus forecast and write result to file
c**   This version is a motion consensus
c     Programmer:  Sampson
c
c     input variables:
c               strmid  - storm identifier (char, bbnnyyyy)
c               dtgcur  - current date-time-group (char, yyyymmddhh)
c               dir     - current direction of motion
c               nmodel  - number of input models interpolated (integer)
c               ntau    - number of taus (integer)
c               conname - 4 char consensus name
c               contype - 4 char consensus type
c               naids   - number of aids in consensus (integer)
c               nmin    - minimum number of aids in consensus (integer)
c               cona    - 4 char array of models making up consensus
c               alt     - 4 char array of alternate models
c               alt2    - 4 char array of second alternate models
c               alt3    - 4 char array of third alternate models
c     output variables:
c               none
c
      parameter ( nmodmax=1000, ltau=15 , intrcnt=45 )
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /intrp/    intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &                  intspd(ltau,nmodmax), intrad(ltau,4,3,nmodmax)
      common /files/iaids_file, istd_out, iconsensus_in
c
      real intlat, intlon, intspd
      real intlag(ltau), intlog(ltau), intspg(ltau)
      real dlat(ltau), dlon(ltau), dspd(ltau)
      real dir

      integer ntot(ltau)
      integer ntoti(ltau)
      integer k,kk,kka,kk2,kk3,j
      integer iaids_file, istd_out, iconsensus_in
c
      character*2  basin
      character*4  fst_tech, tech_12, tech_06
      character*4  conname, cona(nmodel)
      character*4  contype
      character*4  cor_tech
      character*4  alt(nmodel), alt2(nmodel), alt3(nmodel)
      character*8  strmid
      character*10 dtgcur

c
c  starting point
c
      ntot(1)   = 0
      ntoti(1)  = 0
      intlag(1) = 0.0
      intlog(1) = 0.0
      intspg(1) = 0.0
      do k = 1, naids
        call find_ind(cona(k),tech_06,nmodmax,kk)
        call find_ind( alt(k),tech_06,nmodmax,kka)
        call find_ind(alt2(k),tech_06,nmodmax,kk2)
        call find_ind(alt3(k),tech_06,nmodmax,kk3)
        if (kk .eq. 0) then
             continue
        else if(intlat(1,kk).ne.0.0 .and. intlon(1,kk).gt.0.01)then
            ntot(1) = ntot(1) + 1
            intlag(1) = intlag(1) + intlat(1,kk)
            intlog(1) = intlog(1) + intlon(1,kk)
            if ( intspd(1,kk) .gt. 0.01 ) then
              ntoti(1)  = ntoti(1) + 1
              intspg(1) = intspg(1) + intspd(1,kk)
            endif
        else if (kka .eq. 0) then
            continue
        else if(intlat(1,kka).ne.0.0 .and. intlon(1,kka).gt.0.01)then
            ntot(1) = ntot(1) + 1
            intlag(1) = intlag(1) + intlat(1,kka)
            intlog(1) = intlog(1) + intlon(1,kka)
            if ( intspd(1,kka) .gt. 0.01 ) then
              ntoti(1)  = ntoti(1) + 1
              intspg(1) = intspg(1) + intspd(1,kka)
            endif
        else if (kk2 .eq. 0) then
            continue
        else if(intlat(1,kk2).ne.0.0 .and. intlon(1,kk2).gt.0.01)then
            ntot(1) = ntot(1) + 1
            intlag(1) = intlag(1) + intlat(1,kk2)
            intlog(1) = intlog(1) + intlon(1,kk2)
            if ( intspd(1,kk2) .gt. 0.01 ) then
              ntoti(1)  = ntoti(1) + 1
              intspg(1) = intspg(1) + intspd(1,kk2)
            endif
        else if (kk3 .eq. 0) then
            continue
        else if(intlat(1,kk3).ne.0.0 .and. intlon(1,kk3).gt.0.01)then
            ntot(1) = ntot(1) + 1
            intlag(1) = intlag(1) + intlat(1,kk3)
            intlog(1) = intlog(1) + intlon(1,kk3)
            if ( intspd(1,kk3) .gt. 0.01 ) then
              ntoti(1)  = ntoti(1) + 1
              intspg(1) = intspg(1) + intspd(1,kk3)
            endif
        endif
      enddo
      intlag(1) = intlag(1)/ntot(1)
      intlog(1) = intlog(1)/ntot(1)
      intspg(1) = intspg(1)/ntoti(1)
c
c     run for all taus > 0
c
      do j = 2, ltau
c
         ntot(j)   = 0
         ntoti(j)  = 0
         dlat(j) = 0.0
         dlon(j) = 0.0
         dspd(j) = 0.0
         intlag(j) = 0.0
         intlog(j) = 0.0
         intspg(j) = 0.0

c
c     get input model and add to consensus
c
         do k = 1, naids

           call find_ind(cona(k),tech_06,nmodmax,kk)
           call find_ind( alt(k),tech_06,nmodmax,kka)
           call find_ind(alt2(k),tech_06,nmodmax,kk2)
           call find_ind(alt3(k),tech_06,nmodmax,kk3)
           if (kk .eq. 0) then
                   continue
           elseif(intlat(j,kk).ne.0.0 .and. intlon(j,kk).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   dlat(j) = dlat(j) + (intlat(j,kk) - intlat(j-1,kk))
                   dlon(j) = dlon(j) + (intlon(j,kk) - intlon(j-1,kk))
               if ( intspd(j,kk) .gt. 0.01 ) then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk) - intspd(j-1,kk))
               endif
           elseif (kka .eq. 0) then
                   continue
           elseif(intlat(j,kka).ne.0.0 .and. intlon(j,kka).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   dlat(j) = dlat(j) + (intlat(j,kka) - intlat(j-1,kka))
                   dlon(j) = dlon(j) + (intlon(j,kka) - intlon(j-1,kka))
               if ( intspd(j,kka) .gt. 0.01 ) then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kka) - intspd(j-1,kka))
               endif
           elseif (kk2 .eq. 0) then
                   continue
           elseif(intlat(j,kk2).ne.0.0 .and. intlon(j,kk2).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   dlat(j) = dlat(j) + (intlat(j,kk2) - intlat(j-1,kk2))
                   dlon(j) = dlon(j) + (intlon(j,kk2) - intlon(j-1,kk2))
               if ( intspd(j,kk2) .gt. 0.01 ) then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk2) - intspd(j-1,kk2))
               endif
           elseif (kk3 .eq. 0) then
                   continue
           elseif(intlat(j,kk3).ne.0.0 .and. intlon(j,kk3).gt.0.01)then
                   ntot(j) = ntot(j) + 1
                   dlat(j) = dlat(j) + (intlat(j,kk3) - intlat(j-1,kk3))
                   dlon(j) = dlon(j) + (intlon(j,kk3) - intlon(j-1,kk3))
               if ( intspd(j,kk3) .gt. 0.01 ) then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk3) - intspd(j-1,kk3))
               endif
           elseif(intspd(j,kk).ne.0.0)then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk) - intspd(j-1,kk))
           elseif(intspd(j,kka).ne.0.0)then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kka) - intspd(j-1,kka))
           elseif(intspd(j,kk2).ne.0.0)then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk2) - intspd(j-1,kk2))
           elseif(intspd(j,kk3).ne.0.0)then
                   ntoti(j) = ntoti(j) + 1
                   dspd(j) = dspd(j) + (intspd(j,kk3) - intspd(j-1,kk3))
           endif

         enddo
c
c     compute resultant position consensus for a given tau
c
          if ( ntot(j) .ge. nmin ) then
               intlag(j) = intlag(j-1) + dlat(j)/ntot(j)
               intlog(j) = intlog(j-1) + dlon(j)/ntot(j)
         else
               ntot(j)   = 0
               intlag(j) = 0.0
               intlog(j) = 0.0
         endif
c
c     compute resultant intensity consensus for a given tau
c
         if ( ntoti(j) .ge. nmin) then
               intspg(j) = intspg(j-1) +dspd(j)/ntoti(j)
         else
               intspg(j) = 0.0
         endif

      enddo

c
c     Write out the new CON forecast
c
         call write_out ( iaids_file, strmid, dtgcur, conname,
     &                    intlag, intlog, intspg )
         call write_out ( istd_out, strmid, dtgcur, conname,
     &                    intlag, intlog, intspg )

      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine find_ind(atech,new_tech,nmodel,index)
c
c**   Do a search through new_tech array for atech model
c     Programmer:  Sampson
c
c     input variables:
c               atech   - 4 char model identifiers
c               new_tech- 4 char array of possible models
c               nmodel -  number of aids to search in new_tech
c     output variables:
c               index   - the index of the model, zero if not found
      character*4 atech,new_tech(nmodel)
      do i = 1, nmodel
         if (atech .eq. new_tech(i)) then
            index=i
            return
         endif
      enddo
      index=0
      return
      end

c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine read_con(infile,ncmax,incmax,conname,contype,
     &           naids,min,cona,alt,alt2,alt3,idone)
c
c  reads the configuration file (intrfcst.input or other) for consensus input
c     Programmer:  Sampson
c
c     input variables:
c               infile  - input filename
c               ncmax   - max number of consensus allowed
c               incmax  - max number of models per consensus
c     output variables:
c               conname - consensus name (4 chars)
c               contype - consensus type (4 chars)
c               naids   - number of aids in consensus (integer)
c               min    - minimum number of aids in consensus (integer)
c               cona    - 4 char aid names from which to form consensus (20 max)
c               alt     - 4 char aid names of alternative aid (UKMI instead of EGRI)
c               alt2    - 4 char aid names of second alternative aid (JUKI instead of EGRI)
c               alt3    - 4 char aid names of third alternative aid
c               idone   - integer (1 if done reading consensus file)
c
c
c     input files:
c               intrfcst.input or other name  - configuration file
c     output files: none
c
      character*200 line
      character*200 blankline
      character*500 inc_path
      character*50  infile
      character*600 filename
      character*4   cona(incmax),alt(incmax),alt2(incmax),alt3(incmax)
      character*4   conname
      character*4   contype
      integer       naids,min
      integer       ind
      common /inopen/ iopen
      data iopen/0/
      idone = 0
      inmax = incmax
      contype = "    "
c
c   preliminary stuff for first time through the routine
c
      if (iopen .eq. 0) then
c
c**      Create the best track and adeck file names and open the files
c
         call getenv ( "ATCFINC", inc_path )
         ind = index( inc_path, " " ) - 1
c
         filename = inc_path(1:ind)//"/"//infile
         open ( 23, file=filename,  status='old', iostat=ios, err=1010 )
c
c    set the file pointer to the first consensus
c
   70    continue
         read( 23, '(a200)', end=1040 ) line
         if (line(1:19) .ne. '#START CONSENSUS IN') then
            go to 70
         endif
         iopen = 1
      endif
c
c   end of preliminary stuff for first time through the routine
c

c
c    read one consensus specification
c
c    three lines total
c            Consensus name
c            Require
c            Input models
c
c
   80      continue
           read( 23, '(a200)', end=1030 ) line
           if (line(1:19) .ne. 'Consensus name     ') then
                go to 80
           endif
           read(line, '(30x,a4)') conname
           read( 23, '(a200)', end=1030 ) line
cx  optional consensus type
           if (line(1:19) .eq. 'Consensus type     ') then
              read(line, '(30x,a4)') contype
              read( 23, '(a200)', end=1030 ) line
           elseif (line(1:19) .ne. 'Require at least ..') then
                go to 80
           endif
           if (line(1:19) .ne. 'Require at least ..') then
                go to 80
           endif
           read(line, '(30x,i4)') min
           read( 23, '(a200)', end=1030 ) line
           if (line(1:19) .ne. 'Input models (max o') then
                go to 80
           endif
           read(line, '(30x,40(a4,1x))') cona
           naids=0
           do i = 1, incmax
            if (cona(i) .gt. '    ') naids=i
           enddo

           read( 23, '(a200)', end=1030 ) line
           if (line(1:19) .ne. 'Alt   models (max o') then
                do i = 1, incmax
                   alt(i)="    "
                enddo
                backspace (23)
           else
                read(line, '(30x,40(a4,1x))') alt
           endif

           read( 23, '(a200)', end=1030 ) line
           if (line(1:19) .ne. 'Alt2  models (max o') then
                do i = 1, incmax
                   alt2(i)="    "
                enddo
                backspace (23)
           else
                read(line, '(30x,40(a4,1x))') alt2
           endif

           read( 23, '(a200)', end=1030 ) line
           if (line(1:19) .ne. 'Alt3  models (max o') then
                do i = 1, incmax
                   alt3(i)="    "
                enddo
                backspace (23)
           else
                read(line, '(30x,40(a4,1x))') alt3
           endif
c          successful consensus read, return
      idone = 0
      return
c
c**   Error messages
c
 1010 print *, ' Error opening interpolator input - ios = ',ios
      print *, ' Filename:', filename
      stop
 1030 print *, ' Graceful end to consensus config file'
      idone = 1
      return
 1040 print *, ' Cant find beginning of consensus input'
      print *, ' Consensus input should start with:#START CONSENSUS IN'
      idone = 1
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine get_cur(nmodel)
c
c**   Get current model runs, add to the "interpolated" aids
c     Programmer:  Sampson
c
c     input variables:
c               none
c     output variables:
c               none
c
      parameter ( nmodmax=1000, ltau=15 , intrcnt=45 )
      parameter ( ntau=31 )
      parameter ( taurecmax=20 )
      common /techs/ fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      common /initial/ latcur, loncur, spdcur, radcur
      common /intrp/    intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &                  intspd(ltau,nmodmax), intrad(ltau,4,3,nmodmax)
      common /forecast/ fst00lat(ntau,nmodmax), fst00lon(ntau,nmodmax),
     &                  fst00spd(ntau,nmodmax), fst00tau(ntau,nmodmax),
     &                  fst00rad(ntau,4,taurecmax,nmodmax),
     &                  fst06lat(ntau,nmodmax), fst06lon(ntau,nmodmax),
     &                  fst06spd(ntau,nmodmax), fst06tau(ntau,nmodmax),
     &                  fst06rad(ntau,4,taurecmax,nmodmax),
     &                  fst12lat(ntau,nmodmax), fst12lon(ntau,nmodmax),
     &                  fst12spd(ntau,nmodmax), fst12tau(ntau,nmodmax),
     &                  fst12rad(ntau,4,taurecmax,nmodmax),
     &                  last_tau00(nmodmax),
     &                  last_tau06(nmodmax), last_tau12(nmodmax)

c
      real intlat, intlon, intspd, intrad
      real fst00lat, fst00lon, fst00spd, fst00tau
      real fst06lat, fst06lon, fst06spd, fst06tau
      real fst12lat, fst12lon, fst12spd, fst12tau
      real latcur, loncur, spdcur
      integer kk
      integer last_tau00, last_tau06, last_tau12
      character*4  fst_tech, tech_12, tech_06

      iflag = 1 ! Determine interpolation coefficients every time
      lflag = 0 ! 0= linear interpolation 1 = spline interpolation
c
cx   start with clean arrays
      do i = 1,nmodel
         if (tech_06(i) .eq. 'XXXX' .or.
     &       tech_06(i) .eq. fst_tech(i)) then
           do j = 1, ltau
            intlat(j,i) = 0.0
            intlon(j,i) = 0.0
            intspd(j,i) = 0.0
            do m = 1, 3
               do n = 1, 4
                 do k = 1, ltau
                   intrad( k, n, m, i ) = 0.0
                 enddo
               enddo
            enddo
           enddo
         endif
      enddo

      do i = 1,nmodel
         if (tech_06(i) .eq. 'XXXX' .or.
     &       tech_06(i) .eq. fst_tech(i)) then
           do j = 1, ltau
            intlat(j,i) = 0.0
            intlon(j,i) = 0.0
            intspd(j,i) = 0.0
            call mspline ( fst00tau( 1, i ), fst00lat( 1, i ),
     &           last_tau00(i), iflag, lflag, float((j-1)*12),
     &           intlat(j,i), ierror )
            if ( ierror .ne. 0 ) then
                 intlat(j,i) = 0.0
            endif
c
            call mspline ( fst00tau( 1, i ), fst00lon( 1, i ),
     &           last_tau00(i), iflag, lflag, float((j-1)*12),
     &           intlon(j,i), ierror )
            if ( ierror .ne. 0 ) then
                 intlon(j,i) = 0.0
            endif
            call mspline ( fst00tau( 1, i ), fst00spd( 1, i ),
     &           last_tau00(i), iflag, lflag, float((j-1)*12),
     &           intspd(j,i), ierror )
            if ( ierror .ne. 0 ) then
                 intspd(j,i) = 0.0
            endif
cx  special case rapid, which starts at 0 and has no lat or lon
cx          if (fst_tech(i) .eq. 'RI25' .or. fst_tech(i) .eq. 'RI30'
cx   &                                .or. fst_tech(i) .eq. 'RI35') then
cx             if (j .lt. 4 .and. intspd(j,i) .gt. 0.0 ) then
cx             if ( intspd(j,i) .gt. 0.0 ) then
cx                      intspd(j,i) = intspd(j,i) + spdcur
cx                      print *, fst_tech(i), 'intensity:', intspd(j,i)
cx             endif
cx          endif
cx  end of special case rapid, which starts at 0 and has no lat or lon
           enddo
           tech_06(i) = fst_tech(i)
           tech_12(i) = fst_tech(i)
         endif
      enddo

      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine read_mod (inf,nmodmax,nmodel,fst_tech,tech_12,tech_06,
     &               iadj,jadj)
c
c  reads the configuration file (intrfcst.input) for interpolator input
c     Programmer:  Sampson
c
c     input variables:
c               inf - the configuration file name
c               nmodmax - max number of models to interpolate
c     output variables:
c               nmodel - number of models to interpolate
c               fst_tech - array of models ids to interpolate (4 chars)
c               tech_12  - array of models ids 12h interpolated (4 chars)
c               tech_06  - array of models ids 06h interpolated (4 chars)
c               iadj     - array of model intensity interpolation initial phase out times (integer)
c               jadj     - array of model intensity interpolation final phase out times (integer)
c
c     input files:
c               intrfcst.input or other name  - configuration file
c     output files: none
c
      character*4   fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)
      character*50  inf
      character*80  line
      character*500 inc_path
      character*600 filename
      integer       naids
      integer       ind
      integer       iadj(nmodmax)
      integer       jadj(nmodmax)

      nmodel = 0
c
c**   Create the best track and adeck file names and open the files
c
      call getenv ( "ATCFINC", inc_path )
      ind = index( inc_path, " " ) - 1
c
      filename = inc_path(1:ind)//"/"//inf
      open ( 23, file=filename,  status='old', iostat=ios, err=1010 )

   10 continue
c    interpolator data read
      read ( 23, '(a80)', end=1020 ) line
      if (line(1:19) .ne.    '#START INTERPOLATOR') then
           go to 10
      endif
   20 continue
      read( 23, '(a80)', end=1030 ) line
      if (line(1:19) .eq. '#END INTERPOLATOR I') then
             go to 50
      elseif(nmodel .gt. nmodmax) then
             go to 50
      else
             nmodel = nmodel + 1
             iadj(nmodel) = 0
             jadj(nmodel) = 0
             read(line, '(a4,1x,a4,1x,a4,i4,i4)')
     &       fst_tech(nmodel), tech_12(nmodel), tech_06(nmodel),
     &       iadj(nmodel), jadj(nmodel)
             go to 20
      endif
   50 continue
      close (23)
      return
c
c**   Error messages
c
 1010 print *, ' Error opening interpolator input - ios = ',ios
      print *, ' Filename:', filename
      stop
 1020 print *, ' End of file reading interpolator data '
      print *, ' Filename:', filename
      print *, ' line:', line
      stop
 1030 print *, ' Unnexpected end of file reading interpolator data '
      print *, ' Filename:', filename
      print *, ' line:', line
      stop
      end
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine do_err_rad_clim( conname,strmid,ididclimrad,clim_rad)
c
c  computes the GPCE climatological radii as a baseline for estat, also
c  may be of use to the forecaster (larger than, smaller than climatology)
c     Programmer:  Sampson
c
c     input variables:
c               conname - consensus name
c               strmid  - the storm id (wp012009)
c     output variables:
c               ididclimrad - whether or not this routine found radii
c               climrad - climatological GPCE radii (nm)

c     Sampson   Mar 2009    added Atlantic and EP (2005-2008)
c     Sampson   Mar 2009    added CP

      character*4  conname
      character*8  strmid
      parameter ( ltau=15 )
      real clim_rad(ltau)
      real wp_clim (ltau)
      data wp_clim/0.0,40.0,80.0,111.5,143.0,170.5,
     &             198.0,238.5,279.0,322.5,366.0,4*0.0/
      real ep_clim (ltau)
      data ep_clim/0.0,32.0,64.0,92.0,119.0,146.0,
     &             173.0,200.0,227.0,281.0,335.0,4*0.0/
      real al_clim (ltau)
      data al_clim/0.0,36.0,72.0,100.0,129.0,146.0,
     &             192.0,229.0,266.0,311.0,356.0,4*0.0/

      do n = 1,ltau
        if (strmid(1:2) .eq. 'wp') then
           clim_rad(n)=wp_clim(n)
           ididclimrad=1
        else if (strmid(1:2) .eq. 'ep') then
           clim_rad(n)=ep_clim(n)
           ididclimrad=1
        else if (strmid(1:2) .eq. 'cp') then
           clim_rad(n)=wp_clim(n)
           ididclimrad=1
        else if (strmid(1:2) .eq. 'al') then
           clim_rad(n)=al_clim(n)
           ididclimrad=1
        else
           clim_rad(n)=0.0
           ididclimrad=0
        endif
      enddo
      return
      end

c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine do_err_rad ( new_tech, strmid, newlat, newlon, spread,
     &                        numod, ireturn, err_rad )
c
c    added CP basin ... sampson nov 2009
c    168 h arrays ... sampson feb 2010
      parameter ( nmodmax=1000, ltau=15, intrcnt=45 )
c
      common /initial/ latcur, loncur, spdcur
      common /intrp/ intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &               intspd(ltau,nmodmax)
c
      real intlat, intlon, intspd
      real newlat(ltau), newlon(ltau)
      real spread(ltau),err_rad(ltau)
      integer numod(ltau)
      real al_conu_spread(ltau),al_conu_init_inten(ltau)
     &,    al_conu_fcst_inten(ltau),al_conu_init_lat(ltau)
     &,    al_conu_lond(ltau),al_conu_const(ltau)
      real al_guna_spread(ltau),al_guna_init_inten(ltau)
     &,    al_guna_fcst_inten(ltau)
     &,    al_guna_lond(ltau),al_guna_const(ltau)
      real ep_conu_spread(ltau),ep_conu_numem(ltau)
     $,    ep_conu_init_inten(ltau),ep_conu_fcst_inten(ltau)
     $,    ep_conu_init_lon(ltau),ep_conu_lond(ltau)
     $,    ep_conu_init_lat(ltau),ep_conu_const(ltau)
      real ep_guna_spread(ltau),ep_guna_init_inten(ltau)
     &,    ep_guna_fcst_inten(ltau),ep_guna_latd(ltau)
     &,    ep_guna_lond(ltau),ep_guna_const(ltau)
     &,    ep_guna_init_lat(ltau),ep_guna_init_lon(ltau)
      real siglvl_const(ltau)
      real siglvl_const_ep(ltau)
c
c**   Prediction coefficients for TVCN(CONU) (Atlantic)
c
c    Derived from 2001-2003 Seasons
c
c     data al_conu_spread/0.,.524,.564,.559,.457,0.,.527,0.,.829,0.,.73/
c     data al_conu_init_inten/0.,-.267,-.447,-.644,6*0.,-1.738/
c     data al_conu_fcst_inten/4*0.,-0.967,0.,-1.725,0.,-1.726,2*0./
c     data al_conu_init_lat/10*0.,13.42/
c     data al_conu_const/0.,41.,66.,94.,144.,0.,221.,0.,198.,0.,-51./
c
c    Derived from 2001-2004 Seasons
c
c     data al_conu_spread/0.,.565,.609,.556,.5,0.,.541,0.,.775,0.,.803/
c     data al_conu_init_inten/0.,-.236,-.359,-.486,-.666,6*0./
c     data al_conu_fcst_inten/6*0.,-1.269,0.,-1.405,0.,-1.719/
c     data al_conu_init_lat/11*0./
c     data al_conu_const/0.,38.,59.,84.,115.,0.,187.,0.,189.,0.,237./
c
c    Derived from 2002-2005 Seasons
c
c     data al_conu_spread/0.,.581,.531,.561,.622,0.,.610,0.,.782,0.
c    x,                   .789/
c     data al_conu_init_inten/0.,-.231,-.325,-.357,-.400,6*0./
c     data al_conu_fcst_inten/6*0.,-.86,0.,-1.29,0.,-1.86/
c     data al_conu_init_lat/11*0./
c     data al_conu_const/0.,36.,58.,71.,83.,0.,142.,0.,181.,0.,255./
c
c    Derived from 2002-2006 Seasons
c
c     data al_conu_spread/0.,.603,.576,.573,.629,0.,.560,0.,.744,0.
c    x,                   .791/
c     data al_conu_init_inten/0.,-.216,-.296,-.327,-.364,6*0./
c     data al_conu_fcst_inten/6*0.,-.86,0.,-1.4,0.,-1.8/
c     data al_conu_init_lat/11*0./
c     data al_conu_const/0.,34.,53.,67.,78.,0.,146.,0.,194.,0.,244./
c
c    Derived from 2002-2007 Seasons
c
c     data al_conu_spread/0.,.607,.567,.558,.608,0.,.552,0.,.738,0.
c    x,                   .785/
c     data al_conu_init_inten/0.,-.215,-.295,-.326,7*0./
c     data al_conu_fcst_inten/4*0.,-.446,0.,-.858,0.,-1.34,0.,-1.69/
c     data al_conu_init_lat/11*0./
c     data al_conu_const/0.,34.,53.,68.,86.,0.,146.,0.,189.,0.,237./
c
c    Derived from 2003-2008 Seasons
c
c     data al_conu_spread/0.,.613,.581,.543,.603,0.,.541,0.,.643,0.
c    x,                   .701,4*0./
c     data al_conu_init_inten/0.,-.204,-.255,-.261,7*0.,4*0./
c     data al_conu_fcst_inten/4*0.,-.306,0.,-.569,0.,-.876,0.,-.856,
c    x                    4*0./
c     data al_conu_init_lat/11*0.,4*0.0/
c     data al_conu_const/0.,32.,48.,61.,73.,0.,121.,0.,160.,0.,176.,
c    x                    4*0./
c
c    Derived from 2003-2009 Seasons
c
c     data al_conu_spread/0.,.598,.568,.530,.591,0.,.535,0.,.638,0.
c    x                   ,.700,4*0./
c     data al_conu_init_inten/0.,-.202,-.253,-.255,7*0.,4*0./
c     data al_conu_fcst_inten/4*0.,-.289,0.,-.531,0.,-.863,0.,-.858,
c    x                    4*0./
c     data al_conu_init_lat/11*0.,4*0.0/
c     data al_conu_const/0.,32.,48.,61.,71.,0.,118.,0.,160.,0.,177.,
c    x                    4*0./
c
c    Derived from 2004-2010 Seasons
c
c     data al_conu_spread/0.,.661,.603,.568,.617,0.,.446,0.,.507,0.
c    x                   ,.584,4*0./
c     data al_conu_init_inten/0.,-.192,-.243,-.239,-.217,6*0.,4*0./
c     data al_conu_fcst_inten/11*0.,4*0./
c     data al_conu_lond/6*0.,-2.05,0.,-2.68,0.,-3.32,4*0./
c     data al_conu_init_lat/11*0.,4*0.0/
c     data al_conu_const/0.,31.,46.,58.,63.,0.,103.,0.,144.,0.,182.,
c    x                    4*0./
c
c    Derived from 2005-2011 Seasons
c
c     data al_conu_spread/0.,.621,.543,.541,.631,0.,.395,0.,.420,0.
c    x                   ,.497,4*0./
c     data al_conu_init_inten/0.,-.206,-.275,-.276,-.245,6*0.,4*0./
c     data al_conu_fcst_inten/11*0.,4*0./
c     data al_conu_lond/6*0.,-1.81,0.,-2.86,0.,-3.65,4*0./
c     data al_conu_init_lat/11*0.,4*0.0/
c     data al_conu_const/0.,31.,48.,58.,61.,0.,104.,0.,154.,0.,198.,
c    x                    4*0./
c
c    Derived from 2008-2013 Seasons
c
      data al_conu_spread/0.,.62,.64,.584,.486,0.,.362,0.,.579,0.
     x                   ,.707,4*0./
      data al_conu_init_inten/0.,-.181,-.26,-.316,-.379,0.,-.512,8*0./
      data al_conu_fcst_inten/15*0./
      data al_conu_lond/15*0./
      data al_conu_init_lat/15*0./
      data al_conu_const/0.,28.,41.,55.,73.,0.,114.,0.,90.,0.,99.,
     x                    4*0./
c
c**   Prediction coefficients for TCON(GUNA) (Atlantic)
c
c
c    Derived from 2001-2003 Seasons
c
c     data al_guna_spread/0.,.565,.645,.677,.692,0.,.752,0.,.899,0.
c    &                   ,.685/
c     data al_guna_init_inten/0.,-.26,-.403,-.572,-.701,0.,-.876,4*0./
c     data al_guna_lond/10*0.,-7.07/
c     data al_guna_const/0.,40.,59.,81.,100.,0.,120.,0.,36.,0.,157./
c
c    Derived from 2001-2004 Seasons
c
c     data al_guna_spread/0.,.646,.646,.616,.593,0.,.699,0.,.803,0.
c    &                   ,.659/
c     data al_guna_init_inten/0.,-.224,-.328,-.426,-.55,0.,-.684,4*0./
c     data al_guna_lond/10*0.,-4.04/
c     data al_guna_const/0.,35.,54.,73.,96.,0.,114.,0.,61.,0.,162./
c
c    Derived from 2002-2005 Seasons
c
c     data al_guna_spread/0.,.654,.596,.649,.651,0.,.551,0.,.583,0.
c    &                   ,.599/
c     data al_guna_init_inten/0.,-.224,-.282,-.296,7*0./
c     data al_guna_lond/4*0.,-1.72,0.,-2.7,0.,-3.25,0.,-5.74/
c     data al_guna_const/0.,34.,51.,60.,54.,0.,92.,0.,128.,0.,190./
c
c    Derived from 2002-2006 Seasons
c
c     data al_guna_spread/0.,.655,.596,.646,.661,0.,.538,0.,.564,0.
c    &                   ,.605/
c     data al_guna_init_inten/0.,-.209,-.264,-.264,7*0./
c     data al_guna_lond/4*0.,-1.43,0.,-2.99,0.,-3.80,0.,-5.82/
c     data al_guna_const/0.,32.,48.,56.,50.,0.,93.,0.,131.,0.,182./
c
c    Derived from 2002-2007 Seasons
c
c     data al_guna_spread/0.,.634,.549,.605,.655,0.,.634,0.,.674,0.
c    &                   ,.827/
c     data al_guna_init_inten/0.,-.206,-.266,-.272,7*0./
c     data al_guna_fcst_inten/4*0.,-.331,0.,-.645,0.,-.868,0.,-1.11/
c     data al_guna_lond/11*0./
c     data al_guna_const/0.,33.,50.,59.,70.,0.,116.,0.,151.,0.,166./
c
c    Derived from 2003-2008 Seasons
c
c     data al_guna_spread/0.,.646,.639,.537,.619,0.,.607,0.,.657,0.
c    &                   ,.865,4*0./
c     data al_guna_init_inten/0.,-.191,-.193,8*0.,4*0./
c     data al_guna_fcst_inten/3*0.,-.262,-.302,0.,-.607,0.,-.782,0.
c    &                   ,-.653,4*0./
c     data al_guna_lond/11*0.,4*0./
c     data al_guna_const/0.,30.,31.,61.,70.,0.,115.,0.,146.,0.,119.
c    &                   ,4*0./
c
c
c    Derived from 2003-2009 Seasons
c
      data al_guna_spread/0.,.631,.535,.545,.574,0.,.511,0.,.555,0.
     &                   ,.722,4*0./
      data al_guna_init_inten/0.,-.193,-.231,-.237,7*0.,4*0./
      data al_guna_fcst_inten/11*0.,4*0./
      data al_guna_lond/4*0.,-1.59,0.,-2.69,0.,-3.01,0.,-3.7,4*0./
      data al_guna_const/0.,31.,46.,58.,55.,0.,95.,0.,128.,0.,140.,
     &                   4*0./
c
c**   Prediction coefficients for TVCN(CONU) (Eastern North Pacific)
c
c
c    Derived from 2001-2003 Seasons
c
c     data ep_conu_spread/0.,.402,.425,.423,.252,0.,.415,0.,.447,0.
c    &                   ,.742/
c     data ep_conu_numem/0.,-3.99,-7.32,-9.66,-12.09,0.,-19.88,0.
c    &                  ,-33.05,0.,-36.03/
c     data ep_conu_init_inten/0.,-.157,9*0./
c     data ep_conu_fcst_inten/8*0.,-.891,0.,-1.452/
c     data ep_conu_init_lat/10*0.,-10.43/
c     data ep_conu_const/0.,52.,74.,99.,137.,0.,179.,0.,288.,0.,463./
c
c    Derived from 2001-2004 Seasons
c
c     data ep_conu_spread/0.,.387,.409,.409,.246,0.,.429,0.,.454,0.
c    &                   ,.544/
c     data ep_conu_numem/0.,-4.63,-8.8,-11.87,-15.1,0.,-20.95,0.
c    &                  ,-26.45,2*0./
c     data ep_conu_init_inten/0.,-.147,9*0./
c     data ep_conu_fcst_inten/8*0.,-.907,0.,-1.645/
c     data ep_conu_init_lat/11*0./
c     data ep_conu_const/0.,54.,80.,108.,148.,0.,181.,0.,271.,0.,248./
c
c    Derived from 2001-2005 Seasons
c
c     data ep_conu_spread/0.,.386,.396,.401,.250,0.,.382,0.,.336,0.
c    &                   ,.446/
c     data ep_conu_numem/11*0./
c     data ep_conu_init_inten/0.,-.222,-.316,-.375,-.472,0.,-.620,0.
c    &                       ,-.863,0.,-1.01/
c     data ep_conu_fcst_inten/11*0./
c     data ep_conu_init_lat/11*0./
c     data ep_conu_const/0.,37.,48.,75.,106.,0.,131.,0.,186.,0.,212./
c
c    Derived from 2002-2006 Seasons
c
c     data ep_conu_spread/0.,.303,.329,.341,.177,0.,.353,0.,.296,0.
c    &                   ,.346/
c     data ep_conu_numem/11*0./
c     data ep_conu_init_inten/0.,-.215,-.335,-.420,-.556,0.,-.659,0.
c    &                       ,-.619,0.,-.82/
c     data ep_conu_fcst_inten/11*0./
c     data ep_conu_init_lat/11*0./
c     data ep_conu_const/0.,37.,59.,78.,114.,0.,137.,0.,178.,0.,224./
c
c    Derived from 2003-2007 Seasons
c
c     data ep_conu_spread/0.,.255,.356,.518,.511,0.,.346,0.,.334,0.
c    &                   ,.363/
c     data ep_conu_numem/0.,-4.1,-6.34,-11.4,-14.,0.,-19.7,0.,-28.4
c    &                  ,0.,-22.9/
c     data ep_conu_init_inten/0.,-.158,-.24,8*0./
c     data ep_conu_fcst_inten/11*0./
c     data ep_conu_init_lat/11*0./
c     data ep_conu_init_lon/10*0.,-2.66/
c     data ep_conu_lond/10*0.,5.21/
c     data ep_conu_const/0.,52.,78.,90.,113.,0.,176.,0.,240.,0.,491./
c
c    Derived from 2004-2008 Seasons
c
c     data ep_conu_spread/0.,.237,.264,.349,.449,0.,.300,0.,.340,0.
c    &                   ,.240,4*0./
c     data ep_conu_numem/0.,-2.95,-4.79,-6.2,-9.1,0.,-14.3,0.,-17.8
c    &                  ,0.,-22.3,4*0./
c     data ep_conu_init_inten/0.,-.180,-.258,-.310,-.339,6*0.,4*0./
c     data ep_conu_fcst_inten/11*0.,4*0./
c     data ep_conu_init_lat/11*0.,4*0./
c     data ep_conu_init_lon/11*0.,4*0./
c     data ep_conu_lond/11*0.,4*0./
c     data ep_conu_const/0.,49.,76.,95.,114.,0.,153.,0.,188.,0.,252.
c    &                   ,4*0./
c
c    Derived from 2004-2009 Seasons
c
c     data ep_conu_spread/0.,.278,.252,.357,.442,0.,.274,0.,.252,0.
c    &                   ,.205,4*0./
c     data ep_conu_numem/0.,-2.28,-3.91,-4.14,-6.25,0.,-13.2,0.,-15.8
c    &                  ,0.,-13.7,4*0./
c     data ep_conu_init_inten/0.,-.190,-.276,-.336,-.360,6*0.,4*0./
c     data ep_conu_fcst_inten/11*0.,4*0./
c     data ep_conu_init_lat/11*0.,4*0./
c     data ep_conu_init_lon/11*0.,4*0./
c     data ep_conu_lond/11*0.,4*0./
c     data ep_conu_const/0.,46.,75.,87.,104.,0.,153.,0.,194.,0.,230.
c    &                   ,4*0./
c
c    Derived from 2004-2010 Seasons
c
c     data ep_conu_spread/0.,.289,.242,.340,.433,0.,.293,0.,.274,0.
c    &                   ,.235,4*0./
c     data ep_conu_numem/0.,-2.15,-4.02,-4.83,-7.39,0.,-14.3,0.,-17.9
c    &                  ,0.,-18.2,4*0./
c     data ep_conu_init_inten/0.,-.192,-.271,-.312,-.303,6*0.,4*0./
c     data ep_conu_fcst_inten/11*0.,4*0./
c     data ep_conu_init_lat/11*0.,4*0./
c     data ep_conu_init_lon/11*0.,4*0./
c     data ep_conu_lond/11*0.,4*0./
c     data ep_conu_const/0.,45.,75.,90.,106.,0.,155.,0.,199.,0.,240.
c    &                   ,4*0./
c
c    Derived from 2005-2011 Seasons
c
c     data ep_conu_spread/0.,.306,.256,.356,.461,0.,.275,0.,.262,0.
c    &                   ,.268,4*0./
c     data ep_conu_numem/0.,-1.85,-3.42,-4.63,-8.87,0.,-12.4,0.,-13.9
c    &                  ,0.,-10.7,4*0./
c     data ep_conu_init_inten/0.,-.197,-.280,-.280,7*0.,4*0./
c     data ep_conu_fcst_inten/11*0.,4*0./
c     data ep_conu_init_lat/11*0.,4*0./
c     data ep_conu_init_lon/11*0.,4*0./
c     data ep_conu_lond/11*0.,4*0./
c     data ep_conu_const/0.,44.,72.,86.,94.,0.,150.,0.,187.,0.,199.
c    &                   ,4*0./
c
c    Derived from 2007-2013 Seasons
c
      data ep_conu_spread/0.,.487,.382,.499,.661,0.,.39,0.,.311,0.
     &                   ,.379,4*0./
      data ep_conu_numem/4*0.,-8.74,0.,-10.4,0.,-12.0,6*0./
      data ep_conu_init_inten/0.,-.198,-.301,-.325,11*0./
      data ep_conu_fcst_inten/15*0./
      data ep_conu_init_lat/15*0./
      data ep_conu_init_lon/15*0./
      data ep_conu_lond/15*0./
      data ep_conu_const/0.,28.,46.,50.,78.,0.,122.,0.,163.,0.,110.
     &                   ,4*0./

c
c**   Prediction coefficients for TCON(GUNA) (Eastern North Pacific)
c
c
c    Derived from 2001-2003 Seasons
c
c     data ep_guna_spread/0.,.407,.331,.452,.444,0.,.539,0.,.685,0.
c    &                   ,.823/
c     data ep_guna_init_inten/0.,-.174,-.266,0.,-.296,6*0./
c     data ep_guna_fcst_inten/6*0.,-.472,0.,-1.073,2*0./
c     data ep_guna_latd/6*0.,-7.3,4*0./
c     data ep_guna_lond/3*0.,-2.48,7*0./
c     data ep_guna_const/0.,34.,60.,62.,81.,0.,129.,0.,140.,0.,77./
c
c    Derived from 2001-2004 Seasons
c
c     data ep_guna_spread/0.,.348,.292,.408,.356,0.,.492,0.,.59,0.
c    &                   ,.723/
c     data ep_guna_init_inten/0.,-.156,-.254,-.29,-.381,0.,-.536,4*0./
c     data ep_guna_fcst_inten/8*0.,-1.155,2*0./
c     data ep_guna_latd/11*0./
c     data ep_guna_lond/8*0.,3.83,0.,9.82/
c     data ep_guna_const/0.,33.,58.,69.,90.,0.,109.,0.,108.,0.,-44./
c
c    Derived from 2001-2005 Seasons
c
c     data ep_guna_spread/0.,.340,.258,.333,.222,0.,.281,0.,.226,0.
c    &                   ,.472/
c     data ep_guna_init_inten/0.,-.156,-.261,-.305,-.395,0.,-.523,4*0./
c     data ep_guna_fcst_inten/8*0.,-.827,2*0./
c     data ep_guna_latd/11*0./
c     data ep_guna_lond/8*0.,4.83,0.,10.75/
c     data ep_guna_const/0.,32.,58.,71.,96.,0.,123.,0.,115.,0.,-37./
c
c    Derived from 2002-2006 Seasons
c
c     data ep_guna_spread/0.,.298,.259,.303,.164,0.,.236,0.,.238,0.
c    &                   ,.389/
c     data ep_guna_init_inten/0.,-.142,-.263,-.356,-.482,0.,-.433,4*0./
c     data ep_guna_fcst_inten/11*0./
c     data ep_guna_init_lon/8*0.,-1.38,2*0./
c     data ep_guna_latd/11*0./
c     data ep_guna_lond/8*0.,4.65,0.,10.4/
c     data ep_guna_const/0.,32.,58.,71.,96.,0.,123.,0.,115.,0.,-37./
c
c    Derived from 2003-2007 Seasons
c
c     data ep_guna_spread/0.,.294,.283,.319,.231,0.,.194,3*0.,.284/
c     data ep_guna_init_inten/0.,-.15,-.269,-.393,-.514,6*0./
c     data ep_guna_fcst_inten/6*0.,-.75,0.,-.895,2*0./
c     data ep_guna_init_lat/10*0.,9.46/
c     data ep_guna_init_lon/6*0.,-1.23,0.,-1.84,0.,-2.78/
c     data ep_guna_latd/11*0./
c     data ep_guna_lond/8*0.,4.37,0.,9.0/
c     data ep_guna_const/0.,31.,52.,72.,100.,0.,289.,0.,377.,0.,224./
c
c    Derived from 2004-2008 Seasons
c
c     data ep_guna_spread/0.,.249,.181,.174,.141,0.,.161,4*0.,4*0./
c     data ep_guna_init_inten/0.,-.154,-.265,-.387,-.368,6*0.,4*0./
c     data ep_guna_fcst_inten/6*0.,-.806,0.,-.863,2*0.,4*0./
c     data ep_guna_init_lat/10*0.,18.6,4*0./
c     data ep_guna_init_lon/6*0.,-.884,0.,-1.11,2*0.,4*0./
c     data ep_guna_latd/11*0.,4*0./
c     data ep_guna_lond/8*0.,2.67,0.,8.75,4*0./
c     data ep_guna_const/0.,31.,54.,77.,80.,0.,250.,0.,301.,0.,-189.,
c    &                   4*0./
c
c    Derived from 2004-2009 Seasons
c
      data ep_guna_spread/0.,.239,.159,.164,.223,0.,.161,3*0.,.269,
     &                    4*0./
      data ep_guna_init_inten/0.,-.174,-.283,-.39,-.423,0.,-.405,4*0.,
     &                    4*0./
      data ep_guna_fcst_inten/11*0.,4*0./
      data ep_guna_init_lat/11*0.,4*0./
      data ep_guna_init_lon/8*0.,-1.35,0.,-2.14,4*0./
      data ep_guna_latd/8*0.,-5.15,2*0.,4*0./
      data ep_guna_lond/8*0.,3.64,0.,10.9,4*0./
      data ep_guna_const/0.,33.,57.,80.,94.,0.,126.,0.,295.,0.,254.,
     &                   4*0./

c
c**   Significance level adjustment
c
c  For 2004 and 2005
c     data siglvl_const/0.,10.,15.,22.5,30.,0.,45.,0.,60.,0.,75./
c  For 2006 Atlantic
c     data siglvl_const/0.,8.,12.,16.,20.,0.,30.,0.,45.,0.,60./
c  For 2008 Atlantic
c     data siglvl_const/0.,8.,12.,16.,20.,0.,30.,0.,40.,0.,60./
c  For 2009-2010 Atlantic
c     data siglvl_const/0.,5.,8.,12.,16.,0.,24.,0.,32.,0.,40.,4*0./
c  For 2012 Atlantic
c     data siglvl_const/0.,5.,8.,11.,15.,0.,21.,0.,28.,0.,42.,4*0./
c  For 2014 Atlantic
      data siglvl_const/0.,4.,6.,10.,12.,0.,17.,0.,22.,0.,33.,4*0./
c  For 2006 Eastern North Pacific
c     data siglvl_const_ep/0.,10.,15.,22.5,30.,0.,45.,0.,60.,0.,75./
c  For 2008 Eastern North Pacific
c     data siglvl_const_ep/0.,8.,12.,18.,24.,0.,35.,0.,45.,0.,60./
c  For 2009-2010 Eastern North Pacific
c     data siglvl_const_ep/0.,5.,8.,12.,16.,0.,24.,0.,32.,0.,40.,4*0./
c  For 2011 Eastern North Pacific
c     data siglvl_const_ep/0.,5.,8.,12.,14.,0.,18.,0.,24.,0.,32.,4*0./
c  For 2012 Eastern North Pacific
c     data siglvl_const_ep/0.,4.,8.,12.,15.,0.,15.,0.,18.,0.,21.,4*0./
c  For 2014 Eastern North Pacific
      data siglvl_const_ep/0.,4.,7.,9.,13.,0.,10.,0.,16.,0.,21.,4*0./
c
      real latcur, loncur, spdcur
c
      character*4  new_tech
      character*8  strmid
      character*2  basin
c
      basin = strmid (1:2)
      ireturn = -1
c
      do n = 1, ltau

         if( intspd(n,1) .gt. 0.0)  then
            fcst_inten = intspd(n,1)
         else
            fcst_inten = 65.0
         endif
c
         if ( (new_tech .eq. 'CONU' .or. new_tech .eq. 'TVCN')
     &        .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               err_rad(n) = al_conu_spread(n)*spread(n)
     &              + al_conu_init_inten(n)*spdcur
     &              + al_conu_fcst_inten(n)*fcst_inten
     &              + al_conu_lond(n)*xlond
     &              + al_conu_init_lat(n)*latcur
     &              + al_conu_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) )
     &              err_rad(n) = siglvl_const(n)
               ireturn = 1
c
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( (new_tech .eq. 'GUNA' .or. new_tech .eq. 'TCON')
     &        .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlond = newlon(n) - loncur
               err_rad(n) = al_guna_spread(n)*spread(n)
     &              + al_guna_init_inten(n)*spdcur
     &              + al_guna_fcst_inten(n)*fcst_inten
     &              + al_guna_lond(n)*xlond
     &              + al_guna_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) )
     &              err_rad(n) = siglvl_const(n)
               ireturn = 1
c
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( (new_tech .eq. 'CONU' .or. new_tech .eq. 'TVCN')
     &       .and. (basin .eq. 'ep' .or. basin .eq. 'cp' )   ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlond = newlon(n) - loncur
               xnumod=float(numod(n))
               err_rad(n)=ep_conu_spread(n)*spread(n)
     &              + ep_conu_numem(n)*xnumod
     &              + ep_conu_init_inten(n)*spdcur
     &              + ep_conu_fcst_inten(n)*fcst_inten
     &              + ep_conu_init_lat(n)*latcur
     &              + ep_conu_init_lon(n)*loncur
     &              + ep_conu_lond(n)*xlond
     &              + ep_conu_const(n) + siglvl_const_ep(n)
c
               if ( err_rad(n) .lt. siglvl_const_ep(n) )
     &              err_rad(n) = siglvl_const_ep(n)
               ireturn = 1
c
c
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( (new_tech .eq. 'GUNA' .or. new_tech .eq. 'TCON')
     &       .and. (basin .eq. 'ep' .or. basin .eq. 'cp' )   ) then
            if( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = newlon(n) - loncur
               err_rad(n) = ep_guna_spread(n)*spread(n)
     &              + ep_guna_init_inten(n)*spdcur
     &              + ep_guna_fcst_inten(n)*fcst_inten
     &              + ep_guna_latd(n)*xlatd
     &              + ep_guna_lond(n)*xlond
     &              + ep_guna_init_lat(n)*latcur
     &              + ep_guna_init_lon(n)*loncur
     &              + ep_guna_const(n) + siglvl_const_ep(n)
c
               if ( err_rad(n) .lt. siglvl_const_ep(n) )
     &              err_rad(n) = siglvl_const_ep(n)
               ireturn = 1
c
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
      enddo
c
      return
      end

c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine do_err_rad_int (new_tech,strmid,newlat,newlon,newint,
     &                          spreadi, numod, ireturn, err_rad, v_add)
c
c    sampson nrl dec 2012
c    sampson nrl oct 2013 - updates with spread bug corrected
c    sampson nrl apr 2014 - updates Atlantic (2008-2012), EP (2008-20013) and WP (2012-2013)
c
c
      parameter ( nmodmax=1000, ltau=15, intrcnt=45 )
c
      common /initial/ latcur, loncur, spdcur
      common /intrp/ intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &               intspd(ltau,nmodmax)
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)

c
      real intlat, intlon, intspd
      real newlat(ltau), newlon(ltau), newint(ltau)
      real spreadi(ltau), err_rad(ltau), v_add(ltau)
      real intf(ltau), intc(ltau)
      integer numod(ltau)
c
      real latcur, loncur, spdcur
c
c
      character*4  fst_tech, tech_12, tech_06
      character*4  new_tech
      character*8  strmid
      character*2  basin
c
      basin = strmid (1:2)
      ireturn = -1

      call find_ind('JTWI',tech_06,nmodmax,kk)
      if ( kk .eq. 0 ) then
         call find_ind('OFCI',tech_06,nmodmax,kk)
      endif
      if ( kk .eq. 0 ) then
        print *, "in do_err_rad_int: Can't find JTWI or OFCI, aborting"
        print *, "in do_err_rad_int: No Intensity GPCE computed"
        return
      endif

c     Get the forecast intensity for JTWI or OFCI
      do i = 1, ltau
         if( intspd(i,1) .gt. 0.0)  then
           intf(i) = intspd(i,kk)
           intc(i) = intspd(i,kk) - spdcur
         else
           intf(i) = 65.0
           intc(i) =  5.0
         endif
      end do

c
      if ( (new_tech .eq. 'IVCN' .or. new_tech .eq. 'ICON'
     &      .or. new_tech .eq. 'IVRI') .and.
     &     (basin    .eq. 'al'   ) .and.
     &     (newint(1).gt. 0.01   ))then
c
cx      2013         ...spread/lat----fcstv/change  --const --add
cx      err_rad(1) = 0.
cx      err_rad(2) =                 + 0.061*intf(2) + 3.5  +1.2
cx      err_rad(3) =   -0.194*latcur + 0.077*intf(3) + 9.6  +1.6
cx      err_rad(4) =   -0.267*latcur + 0.081*intf(4) +12.6  +1.7
cx      err_rad(5) =                 + 0.133*intc(5) +12.6  +3.0
cx      err_rad(6) = 0.
cx      err_rad(7) =                 + 0.120*intc(7) +14.1  +3.4
cx      err_rad(8) = 0.
cx      err_rad(9) =                   0.098*intc(9) +13.90 +3.6
cx      err_rad(10)= 0.
cx      err_rad(11)=                  +0.091*intc(11)+14.0  +3.1
cx      err_rad(12)= 0.
cx      err_rad(13)= 0.
cx      err_rad(14)= 0.
cx      err_rad(15)= 0.

cx      2014         ...intens/lat----fcstv/change  --const --add
        err_rad(1) = 0.
        err_rad(2) =    0.069*spdcur - 0.119*latcur   + 5.5  +1.2
        err_rad(3) =   -0.149*latcur + 0.078*intf(3)  + 8.3  +1.5
        err_rad(4) =   -0.194*latcur + 0.080*intf(4)  +10.7  +1.5
        err_rad(5) =                   0.118*intc(5)  +12.0  +2.6
        err_rad(6) = 0.
        err_rad(7) =                   0.126*intc(7)  +13.3  +2.6
        err_rad(8) = 0.
        err_rad(9) =                   0.096*intc(9)  +13.4  +3.3
        err_rad(10)= 0.
        err_rad(11)=                   0.102*intc(11) +13.7  +3.1
        err_rad(12)= 0.
        err_rad(13)= 0.
        err_rad(14)= 0.
        err_rad(15)= 0.
cx                    no bias corrections for GPCE
        v_add(1) = 0.
        v_add(2) = newint(2)
        v_add(3) = newint(3)
        v_add(4) = newint(4)
        v_add(5) = newint(5)
        v_add(6) = 0.
        v_add(7) = newint(7)
        v_add(8) = 0.
        v_add(9) = newint(9)
        v_add(10)= 0.
        v_add(11)= newint(11)
        v_add(12)= 0.
        v_add(13)= 0.
        v_add(14)= 0.
        v_add(15)= 0.
        ireturn = 1
c
      else if ( (new_tech .eq. 'IVCN' .or. new_tech .eq. 'ICON'
     &      .or. new_tech .eq. 'IVRI') .and.
     &     (basin    .eq. 'ep'   ) .and.
     &     (newint(1).gt. 0.01   ))then
c
cx      2013         spread, lat, lon, intf                   --const--add
cx      err_rad(1) =0.
cx      err_rad(2) =                -0.318*latcur+0.070*intf(2)+7.8 +1.6
cx      err_rad(3) =                -0.793*latcur+0.113*intf(3)+16.4+1.2
cx      err_rad(4) =                -1.280*latcur+0.111*intf(4)+26.6+2.8
cx      err_rad(5) =                -1.660*latcur+0.109*intf(5)+34.3+2.8
cx      err_rad(6) =0.
cx      err_rad(7) =                -1.340*latcur+0.138*intf(7)+29.1+4.0
cx      err_rad(8) =0.
cx      err_rad(9) =0.501*spreadi(9)-0.146*intc(9)             +14.5+3.1
cx      err_rad(10)=0.
cx      err_rad(11)=0.611*spreadi(11)+0.212*intc(11)           +12.8+3.7
cx      err_rad(12)=0.
cx      err_rad(13)=0.
cx      err_rad(14)=0.
cx      err_rad(15)=0.
cx      2013         spread, lat, lon, intf                   --const--add
        err_rad(1) =0.
        err_rad(2) =                -0.217*latcur+0.075*intf(2)+5.8 +1.3
        err_rad(3) =                -0.586*latcur+0.131*intf(3)+11.9+1.6
        err_rad(4) =                -1.060*latcur+0.144*intf(4)+20.8+2.4
        err_rad(5) =                -1.450*latcur+0.150*intf(5)+27.8+3.0
        err_rad(6) =0.
        err_rad(7) =                -1.010*latcur+0.164*intf(7)+21.3+3.0
        err_rad(8) =0.
        err_rad(9) =0.376*spreadi(9)+0.141*intc(9)             +13.7+2.1
        err_rad(10)=0.
        err_rad(11)=0.            +0.150*intc(11)+0.201*intf(11)+6.6+3.4
        err_rad(12)=0.
        err_rad(13)=0.
        err_rad(14)=0.
        err_rad(15)=0.
cx                    no bias corrections for GPCE
        v_add(1) = 0.
        v_add(2) = newint(2)
        v_add(3) = newint(3)
        v_add(4) = newint(4)
        v_add(5) = newint(5)
        v_add(6) = 0.
        v_add(7) = newint(7)
        v_add(8) = 0.
        v_add(9) = newint(9)
        v_add(10)= 0.
        v_add(11)= newint(11)
        v_add(12)= 0.
        v_add(13)= 0.
        v_add(14)= 0.
        v_add(15)= 0.
        ireturn = 1
c
      else if ( (new_tech .eq. 'S5YY' .or. new_tech .eq. 'S5RI' ) .and.
     &     (basin    .eq. 'wp'   ) .and.
     &     (newint(1).gt. 0.01   ))then
c
cx      the WP GPCE wants all lons 0-180
        lon = loncur
        if ( loncur .gt. 180.0 ) lon = 360.0 - loncur
cx      2013         spread, lat, lon, intf                   --const--add
cx      err_rad(1)=0.
cx      err_rad(2)=-0.163*latcur   -0.125*lon                  +26.9 +0.5
cx      err_rad(3)=-0.377*latcur   -0.170*lon                  +40.2 +1.5
cx      err_rad(4)=-0.568*latcur   -0.209*lon                  +51.1 +1.4
cx      err_rad(5)=-0.633*latcur   -0.167*lon                  +47.6 +2.4
cx      err_rad(6)=0.
cx      err_rad(7)=-0.631*latcur+0.68*spreadi(7) -0.068*spdcur +24.1 +2.9
cx      err_rad(8)=0.
cx      err_rad(9)=-0.390*latcur+0.637*spreadi(9)              +15.0 +3.6
cx      err_rad(10)=0.
cx      err_rad(11)=            0.692*spreadi(11)              + 6.1 +1.4
cx      err_rad(12)=0.
cx      err_rad(13)=0.
cx      err_rad(14)=0.
cx      err_rad(1)=0.
cx      2014         spread, lat, lon, intf                   --const--add
        err_rad(1)=0.
        err_rad(2)=-0.116*latcur   +0.044*intf(2)              +6.3  +0.6
        err_rad(3)=-0.310*latcur   +0.073*intf(3)              +11.4 +1.0
        err_rad(4)=-0.487*latcur   +0.104*intc(4)              +21.5 +1.6
        err_rad(5)=-0.554*latcur   +0.134*intc(5)              +23.7 +1.8
        err_rad(6)=0.
        err_rad(7)=-0.507*latcur   +0.123*intc(7)              +23.1 +3.6
        err_rad(8)=0.
        err_rad(9)= 0.082*intc(9)  +0.511*spreadi(9)           +8.3  +2.6
        err_rad(10)=0.
        err_rad(11)=                0.764*spreadi(11)          +3.5  +1.6
        err_rad(12)=0.
        err_rad(13)=0.
        err_rad(14)=0.
        err_rad(1)=0.

cx                    no bias corrections for GPCE
        v_add(1) = 0.
        v_add(2) = newint(2)
        v_add(3) = newint(3)
        v_add(4) = newint(4)
        v_add(5) = newint(5)
        v_add(6) = 0.
        v_add(7) = newint(7)
        v_add(8) = 0.
        v_add(9) = newint(9)
        v_add(10)= 0.
        v_add(11)= newint(11)
        v_add(12)= 0.
        v_add(13)= 0.
        v_add(14)= 0.
        v_add(15)= 0.
        ireturn = 1
c
      else
           do i = 1, ltau
               err_rad(i) = 0.0
               v_add(i)   = newint(i)
           end do
         ireturn =-1
c
      endif

c     post-processing to get rid of negative err_rad
c     we saw this in wp132012 (e.g., 2012080712)
      do i = 1, ltau
         if ( err_rad(i) .le. 3.0 ) then
             print *, 'aid=', newtech
             print *, 'lat, lon, spread=', latcur, loncur, spreadi(i)
             print *, 'GPCE err_rad < 3, resetting to 3'
             err_rad(i) = 3.0
         endif
      end do

c     post-processing to get rid of non-zero err_rad and v_rad
c     when the actual intensity is zero
      do i = 1, ltau
         if ( newint(i) .le. 0.0 ) then
             err_rad(i) = 0.0
             v_add(i)   = 0.0
         endif
      end do

c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine do_err_ellipse(new_tech,strmid,newlat,newlon,newspd,
     &  naids,
     &  glat,glon,nens,
     &  idid, theta, gxlat, gxlon, gxcross, gxalong, bias_x, bias_a)
c
c  makes the GPCE-AX forecasts
c     Programmer:  Sampson
c
c     input variables:
c               new_tech - tech name for GPCE-AX
c               strmid   - storm id (e.g. wp012009)
c               newlat   - latitudes of consensus forecast (0-120 by 12)
c               newlon   - longitudes of consensus forecast (0-120 by 12)
c               newspd   - intensities of consensus forecast (0-120 by 12)
c               naids    - number of aids in consensus
c               glat     - latitudes of member forecasts (0-120 by 12)
c               glon     - longitudes of member forecasts (0-120 by 12)
c               nens     - number of consensus members found
c     output variables:
c               idid     - boolean for whether this routine was successful
c               theta   -  direction of motion
c               gxlat   -  GPCE-AX lat
c               gxlon   -  GPCE-AX lon
c               gxcross -  GPCE-AX cross track radius
c               gxalong -  GPCE-AX along track radius
c               bias_x  -  GPCE-AX cross bias
c               bias_a  -  GPCE-AX along bias
c

      parameter ( nmodmax=1000, nmodel=15, ltau=11, nth=121 )
      parameter ( npreds=2 )
c
      common /initial/ latcur, loncur, spdcur
c
      real newlat(ltau), newlon(ltau), newspd(ltau)
      real theta(ltau), asprd(ltau), xsprd(ltau)
      real gxlat(ltau), gxlon(ltau), gxcross(ltau), gxalong(ltau)
      integer naids
      integer idid
      integer ltauf
      real glat(ltau,naids), glon(ltau,naids)
      integer nens(ltau)
      real latcur, loncur, spdcur
c
      real bias_x(ltau), bias_a(ltau)
      real predictors_x(npreds), predictors_a(npreds)
      real coefs_x(npreds+1), coefs_a(npreds+1)
      real boost_x, boost_a
      real forecast_x, forecast_a
c
      character*4  new_tech
      character*8  strmid
      character*2  basin
      real atau(ltau)

c***  Interploted lat and lon (lath, lonh) and associated time
c***  values
      real th(nth), lath(nth), lonh(nth)
      data atau /  0.0, 12.0, 24.0, 36.0,  48.0,  60.0,
     &                  72.0, 84.0, 96.0, 108.0, 120.0/
      real hdir(ltau)
      integer iprint
c***  Debugging print
c***
      iprint = 0
c
      basin = strmid (1:2)
      idid = -1
cx
cx    start with a clean slate
cx
      do n = 1, ltau
          theta(n) = 0.0
          asprd(n) = 0.0
          xsprd(n) = 0.0
          gxlat(n) = 0.0
          gxlon(n) = 0.0
          gxcross(n) = 0.0
          gxalong(n) = 0.0
          bias_x(n)  = 0.0
          bias_a(n)  = 0.0
      enddo

c***  get length of forecast
      do n = 1, ltau
          if (nens(n) .gt. 1) ltauf = n
      enddo

      call hermite( ltauf,ltau,atau,newlat,newlon,nth,th,lath,lonh )
      call direction_and_spread( ltau, nth, naids, atau, th,
     &                           newlat, newlon, glat, glon,
     &                           lath, lonh,
     &                           theta, asprd, xsprd )
      do n = 1, ltau
       if (loncur .lt. 180.0) then
cx       al version
         call assign_ax(n,latcur,loncur,spdcur,newlat(n),newlon(n),
     &                  asprd(n),xsprd(n),theta(n),
     &                  bias_x(n),bias_a(n),
     &                  gxlat(n),gxlon(n),predictors_x,coefs_x,
     &                  predictors_a,coefs_a, boost_x)
      else
cx       wp version
         call assign_wp(n,latcur,loncur,spdcur,newlat(n),newlon(n),
     &                  newspd(n),asprd(n),xsprd(n),theta(n),
     &                  bias_x(n),bias_a(n),
     &                  gxlat(n),gxlon(n),predictors_x,coefs_x,
     &                  predictors_a,coefs_a, boost_x)
      endif
         call GPCE_AX(predictors_x,coefs_x,predictors_a,
     &                   coefs_a,boost_x,npreds,gxcross(n),gxalong(n))
      enddo

cx
cx   a little clean up for missing taus
cx
      if (ltauf .lt. ltau) then
         do i=ltauf+1,ltau
             bias_x(i)  = 0.0
             bias_a(i)  = 0.0
             gxlat(i)   = 0.0
             gxlon(i)   = 0.0
             gxcross(i) = 0.0
             gxalong(i) = 0.0
             asprd(i)   = 0.0
             xsprd(i)   = 0.0
         enddo
      endif

      if (iprint) then
        write(6,*) ' Tau  Members Theta  Along sprd Across sprd'
        do i=1,ltau
          write(6,'(f4.0,i8,3f9.0)')
     &               atau(i),nens(i),theta(i),asprd(i),xsprd(i)
        enddo
      endif

      do n = 1, ltau
c
         if ( (new_tech .eq. 'TVCN' .or. new_tech .eq. 'TCON' .or.
     &         new_tech .eq. 'CONW' .or. new_tech .eq. 'TESW')
cx   &        .and. basin .eq. 'al' .or. basin .eq. 'ep' ) then
     &        .and. basin .eq. 'al' .or. basin .eq. 'wp' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               idid = 1
c
c
            else
               theta(n) = 0.0
               asprd(n) = 0.0
               xsprd(n) = 0.0
               gxlat(n) = 0.0
               gxlon(n) = 0.0
               gxcross(n) = 0.0
               gxalong(n) = 0.0
               bias_x(n)  = 0.0
               bias_a(n)  = 0.0
c
            endif
         endif
c
c
      enddo
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine do_err_rad_wp (conname,strmid,intlag,intlog,spread
     &                          ,numod,ireturn, err_rad)
c
c
c    added CP basin ... sampson nov 2009
c    added 168 h arrays ... sampson may 2010
c
      parameter ( nmodmax=1000, ltau=15, intrcnt=45 )
      common /initial/ latcur, loncur, spdcur, radcur
      common /intrp/ intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &               intspd(ltau,nmodmax)
      common /minus12/ latm12, lonm12, spdm12
      common /techs/fst_tech(nmodmax),tech_12(nmodmax),tech_06(nmodmax)

c
      character*4  fst_tech, tech_12, tech_06
      character*4  conname
      character*8  strmid
      character*2 basin
      real intlat, intlon, intspd
      real latcur, loncur, spdcur
      real latm12, lonm12, spdm12
      real intlag(ltau), intlog(ltau)
      real spread(ltau),err_rad(ltau)
      real xlati,xloni,xlatd,xlond
      integer numod(ltau)
      real wp_con_spread(ltau),wp_con_init_inten(ltau)
     &,    wp_con_fcst_inten(ltau),wp_con_init_lat(ltau)
     &,    wp_con_lond(ltau),wp_con_const(ltau)
     &,    wp_con_numod(ltau)
      real wp_siglvl_const(ltau)
      real shs_con_spread(ltau),shs_con_init_inten(ltau)
     &,    shs_con_init_lat(ltau),shs_con_init_lon(ltau)
     &,    shs_con_numod(ltau),shs_con_const(ltau)
      real shs_siglvl_const(ltau)
      real shp_con_spread(ltau),shp_con_init_inten(ltau)
cx   &,    shp_con_init_lat(ltau),shp_con_lond(ltau)
     &,    shp_con_init_lat(ltau),shp_con_init_lon(ltau)
cx   &,    shp_con_latd(ltau),shp_con_speed(ltau)
     &,    shp_con_numod(ltau),shp_con_const(ltau)
      real shp_siglvl_const(ltau)
c
c 2004 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.575,.653,.716,.592,0.,.350,4*0./
c     data wp_con_init_inten/0.,-.247,4*0.,-.608,0.,-1.075,2*0./
c     data wp_con_fcst_inten/2*0.,-.398,-.491,-.547,6*0./
c     data wp_con_init_lat/8*0.,8.89,0.,12.07/
c     data wp_con_lond/6*0.,4.28,0.,4.05,0.,5.54/
c     data wp_con_const/0.,40.,61.,75.,100.,0.,185.,0.,212.,0.,185./
c
c significance level adjustment
c
c     data wp_siglvl_const/0.,10.,15.,22.5,30.,0.,45.,0.,60.,0.,75./
c
c 2005 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.604,.555,.46,.488,0.,.447,4*0./
c     data wp_con_init_inten/11*0./
c     data wp_con_fcst_inten/0.,-.272,-.388,-.545,-.635,0.,-.868,
c    x                       0.,-1.263,0.,-1.174/
c     data wp_con_init_lat/8*0.,5.433,0.,10.07/
c     data wp_con_lond/11*0./
c     data wp_con_const/0.,43.,65.,96.,115.,0.,166.,0.,252.,0.,241./
c
c 2006 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.586,.485,.398,.444,0.,.382,4*0./
c     data wp_con_init_inten/11*0./
c     data wp_con_fcst_inten/0.,-.275,-.376,-.485,-.585,0.,-.807,
c    x                       0.,-1.161,0.,-1.044/
c     data wp_con_init_lat/8*0.,4.15,0.,6.02/
c     data wp_con_lond/11*0./
c     data wp_con_const/0.,42.,65.,92.,110.,0.,164.,0.,249.,0.,266./
c
c 2007 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.565,.437,.415,.432,0.,.408,4*0./
c     data wp_con_init_inten/11*0./
c     data wp_con_fcst_inten/0.,-.29,-.41,-.471,-.509,0.,-.604,
c    x                       0.,-1.16,0.,-1.03/
c     data wp_con_init_lat/8*0.,4.26,0.,5.23/
c     data wp_con_lond/11*0./
c     data wp_con_const/0.,43.,69.,87.,101.,0.,138.,0.,246.,0.,288./
c
c 2008 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.587,.519,.501,.472,0.,.445,0.,
c    x                   .291,0.,.387/
c     data wp_con_init_inten/11*0./
c     data wp_con_fcst_inten/0.,-.289,-.376,-.388,-.459,0.,-.521,
c    x                       0.,-.798,2*0./
c     data wp_con_init_lat/11*0./
c     data wp_con_lond/11*0./
c     data wp_con_const/0.,42.,61.,72.,90.,0.,121.,0.,223.,0.,182./
c
c 2009 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.597,.527,.499,.488,0.,.498,0.,
c    x                   .367,0.,.399/
c     data wp_con_init_inten/11*0./
c     data wp_con_fcst_inten/0.,-.278,-.385,-.392,-.441,0.,-.505,
c    x                       0.,-.787,2*0./
c     data wp_con_init_lat/11*0./
c     data wp_con_lond/11*0./
c     data wp_con_const/0.,41.,61.,73.,89.,0.,120.,0.,226.,0.,201./
c
c 2010 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.595,.518,.456,.453,0.,.431,0.,
c    x                   .565,0.,.408,4*0./
c     data wp_con_init_inten/0.,-.247,-.335,-.383,-.381,6*0.,4*0./
c     data wp_con_fcst_inten/6*0.,-.473,4*0.,4*0./
c     data wp_con_init_lat/11*0.,4*0./
c     data wp_con_lond/11*0.,4*0./
c     data wp_con_const/0.,37.,55.,72.,82.,0.,121.,0.,132.,0.,205.,
c    x                  4*0./
c
c 2011 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.582,.497,.455,.452,0.,.422,0.,
c    x                   .554,0.,.385,4*0./
c     data wp_con_init_inten/0.,-.243,-.323,-.362,-.356,6*0.,4*0./
c     data wp_con_fcst_inten/6*0.,-.521,4*0.,4*0./
c     data wp_con_init_lat/11*0.,4*0./
c     data wp_con_lond/11*0.,4*0./
c     data wp_con_const/0.,36.,55.,70.,80.,0.,126.,0.,135.,0.,208.,
c    x                  4*0./
c
c 2012 prediction coefficients for consensus (Western North Pacific)
c
c     data wp_con_spread/0.,.553,.606,.597,.491,0.,.450,0.,
c    x                   .714,0.,.485,4*0./
c     data wp_con_init_inten/0.,-.236,-.298,-.333,-.373,6*0.,4*0./
c     data wp_con_fcst_inten/6*0.,-.546,4*0.,4*0./
c     data wp_con_init_lat/11*0.,4*0./
c     data wp_con_lond/11*0.,4*0./
c     data wp_con_const/0.,37.,49.,57.,78.,0.,125.,0.,105.,0.,183.,
c    x                  4*0./
c
c 2014 prediction coefficients for consensus (Western North Pacific)
c
      data wp_con_spread/0.,.586,.505,.58,.57,0.,.402,0.,
     x                   .633,0.,.426,4*0./
      data wp_con_init_inten/0.,-.22,-.289,-.309,-.344,10*0./
      data wp_con_fcst_inten/6*0.,-.689,8*0./
      data wp_con_init_lat/15*0./
      data wp_con_lond/15*0./
      data wp_con_numod/8*0.,-24.8,0.,-27.6,4*0./
      data wp_con_const/0.,34.,49.,53.,63.,0.,128.,0.,207.,0.,290.,
     x                  4*0./

c
c 2009 significance level adjustment
c
c     data wp_siglvl_const/0.,8.,12.,16.,20.,0.,30.,0.,40.,0.,60.,4*0./
c
c 2010 significance level adjustment
c
c     data wp_siglvl_const/0.,6.,10.,15.,20.,0.,30.,0.,40.,0.,50.,4*0./
c
c 2011 significance level adjustment
c
c     data wp_siglvl_const/0.,5.,9.,12.,17.,0.,24.,0.,30.,0.,50.,4*0./
c
c 2012 significance level adjustment
c
c     data wp_siglvl_const/0.,5.,9.,12.,19.,0.,24.,0.,32.,0.,40.,4*0./
c
c 2014 significance level adjustment
c
      data wp_siglvl_const/0.,5.,9.,12.,16.,0.,21.,0.,32.,0.,40.,4*0./
c
c 2005 prediction coefficients for consensus (Southern Hemisphere < 140E)
c
c     data shs_con_spread/0.,.344,.432,.379,.421,0.,.307,0.,.296,
c    x                   0.,.347/
c     data shs_con_init_inten/0.,-.309,-.385,-.447,7*0./
c     data shs_con_init_lat/4*0.,-3.96,0.,-6.99,0.,-9.48,0.,
c    x                      -8.9/
c     data shs_con_init_lon/2*0.,-.21,-.312,-.534,0.,-.748,0.,
c    x                      -1.1,0.,-1.16/
c     data shs_con_numod/4*0.,-12.3,0.,-17.6,4*0./
c     data shs_con_const/0.,53.,91.,125.,230.,0.,348.,0.,386.,0.,
c    x                     417./
c
c 2007 prediction coefficients for consensus (Southern Hemisphere < 140E)
c
c     data shs_con_spread/0.,.359,.398,.411,.453,0.,.373,0.,.407,
c    x                    0.,.509,4*0./
c     data shs_con_init_inten/0.,-.283,-.303,-.324,-.255,6*0.,4*0./
c     data shs_con_init_lat/6*0.,-6.13,0.,-5.02,2*0.,4*0./
c     data shs_con_init_lon/6*0.,-.804,0.,-.825,0.,-1.05,4*0./
c     data shs_con_numod/11*0.,4*0./
c     data shs_con_const/0.,51.,69.,86.,100.,0.,266.,0.,301.,0.,
c    x                   262.,4*0./
c
c 2011 prediction coefficients for consensus (Southern Hemisphere < 140E)
c
c     data shs_con_spread/0.,.544,.625,.583,.472,0.,.410,0.,.351,
c    x                    0.,.510,4*0./
c     data shs_con_init_inten/0.,-.186,9*0.,4*0./
c     data shs_con_init_lat/11*0.,4*0./
c     data shs_con_init_lon/11*0.,4*0./
cx    data shs_con_numod/6*0.,-8.49,0.,-20.2,0.,-28.2,0.,4*0./
c     data shs_con_numod/6*0.,-8.49,0.,-20.2,0.,-28.2,4*0./
c     data shs_con_const/0.,35.,31.,43.,66.,0.,146.,0.,234.,0.,
c    x                   268.,4*0./
c
c 2012 prediction coefficients for consensus (Southern Hemisphere < 140E)
c
      data shs_con_spread/0.,.609,.664,.617,.493,0.,.452,0.,.328,
     x                    0.,.451,4*0./
      data shs_con_init_inten/0.,-.214,-.226,8*0.,4*0./
      data shs_con_init_lat/11*0.,4*0./
      data shs_con_init_lon/11*0.,4*0./
      data shs_con_numod/3*0.,-6.14,-7.3,0.,-9.15,0.,-20.7,0.,
     &                  -29.4,4*0./
      data shs_con_const/0.,33.,40.,73.,100.,0.,139.,0.,236.,0.,
     x                   282.,4*0./
c
c 2011 significance level adjustment
c
c     data shs_siglvl_const/0.,5.,9.,13.,18.,0.,24.,0.,28.,0.,45.,
c    x                      4*0./
c
c 2012 significance level adjustment
c
      data shs_siglvl_const/0.,5.,8.,12.,16.,0.,22.,0.,26.,0.,45.,
     x                      4*0./
c
c 2005 prediction coefficients for consensus (Southern Hemisphere > 140E)
c
c     data shp_con_spread/2*0.,.332,.403,2*0.,.266,4*0./
c     data shp_con_init_inten/0.,-.278,2*0.,-1.03,0.,-1.05,4*0./
c     data shp_con_init_lat/3*0.,4.04,7.02,0.,12.8,0.,19.1,0.,
c    x                      41.4/
c     data shp_con_lond/0.,5.26,0.,6.88,7.74,0.,4.41,0.,
c    x                      8.41,0.,9.11/
c     data shp_con_latd/3*0.,-8.56,4*0.,-8.92,2*0./
c     data shp_con_numod/0.,-7.14,-16.3,-20.9,7*0./
c     data shp_con_const/0.,96.,127.,118.,112.,0.,45.,0.,57.,0.,
c    x                   -234./
c
c 2007 prediction coefficients for consensus (Southern Hemisphere > 140E)
c
c     data shp_con_spread/0.,.409,.458,.579,.327,0.,.341,0.,.241,2*0.,
c    x                    4*0./
c     data shp_con_init_inten/11*0.,4*0./
c     data shp_con_init_lat/4*0.,4.9,0.,6.98,0.,12.8,0.,19.8,4*0./
c     data shp_con_lond/8*0.,3.02,2*0.,4*0./
c     data shp_con_latd/11*0.,4*0./
c     data shp_con_numod/0.,-9.3,-12.4,-14.4,-19.2,0.,-21.2,4*0.,4*0./
c     data shp_con_speed/10*0.,18.5,4*0./
c     data shp_con_const/0.,73.,104.,122.,111.,0.,123.,0.,57.,0.,
c    x                   -100.,4*0./
c
c significance level adjustment
c
c     data shp_siglvl_const/0.,10.,15.,20.,25.,0.,30.,0.,45.,0.,60.,
c    x                      4*0./
c
c 2011 prediction coefficients for consensus (Southern Hemisphere > 140E)
c
c     data shp_con_spread/0.,.544,.625,.583,.472,0.,.410,0.,.351,
c    x                    0.,.510,4*0./
c     data shp_con_init_inten/0.,-.186,9*0.,4*0./
c     data shp_con_init_lat/11*0.,4*0./
c     data shp_con_init_lon/11*0.,4*0./
cx    data shp_con_numod/6*0.,-8.49,0.,-20.2,0.,-28.2,0.,4*0./
c     data shp_con_numod/6*0.,-8.49,0.,-20.2,0.,-28.2,4*0./
c     data shp_con_const/0.,35.,31.,43.,66.,0.,146.,0.,234.,0.,
c    x                   268.,4*0./
c
c 2012 prediction coefficients for consensus (Southern Hemisphere > 140E)
c
      data shp_con_spread/0.,.609,.664,.617,.493,0.,.452,0.,.328,
     x                    0.,.451,4*0./
      data shp_con_init_inten/0.,-.214,-.226,8*0.,4*0./
      data shp_con_init_lat/11*0.,4*0./
      data shp_con_init_lon/11*0.,4*0./
      data shp_con_numod/3*0.,-6.14,-7.3,0.,-9.15,0.,-20.7,0.,
     &                  -29.4,4*0./
      data shp_con_const/0.,33.,40.,73.,100.,0.,139.,0.,236.,0.,
     x                   282.,4*0./
c
c 2011 significance level adjustment
c
c     data shp_siglvl_const/0.,5.,9.,13.,18.,0.,24.,0.,28.,0.,45.,
c    x                      4*0./
c
c 2012 significance level adjustment
c
      data shp_siglvl_const/0.,5.,8.,12.,16.,0.,22.,0.,26.,0.,45.,
     x                      4*0./
c
      basin = strmid (1:2)
      ireturn = 1
c
c only apply this to CONW or TVCN in WP, IO, CP and SH basins
      if ( conname .ne. 'CONW' .and. conname .ne. 'TESW' .and.
     &     conname .ne. 'TVCN' .or.
     &     basin   .eq. 'al'   .or.  basin   .eq. 'ep' ) then
         ireturn = -1
         return
      endif

      call find_ind('JTWI',tech_06,nmodmax,kk)
      if ( kk .eq. 0 ) then
         call find_ind('OFCI',tech_06,nmodmax,kk)
      endif

      do n = 1,ltau
cx    check the intensity, if not filled then use 65
        if ( kk .gt. 0 .and. intspd(n,kk).gt.0. ) then
          fcst_inten = intspd(n,kk)
        else
          fcst_inten = 65.
        endif
      if(fcst_inten.lt.20.) fcst_inten=20.
cx    end of intensity setting
c
      if( latcur.ne.0.0.and.loncur.ne.0.0.and.latm12.ne.0.0
     &       .and.lonm12.ne.0.0) then
            avglat = 0.5 * (latcur + latm12)
            coslat  = cos( avglat*pi/180.0 )
            ydist = 60. * (latcur - latm12)
            xdist = 60. * (loncur - lonm12) * coslat
            speed = sqrt(xdist**2 + ydist**2)/12.
      else
            speed = 8.5
      endif
c
      if (basin.eq.'wp' .or. basin.eq.'cp') then
        if(intlag(n).ne.0.0.and.intlog(n).gt.0.01) then
          xlond = -(intlog(n) - loncur)
          xlati = latcur
          xnumod=float(numod(n))
          err_rad(n)=wp_con_spread(n)*spread(n)
     &        +wp_con_init_inten(n)*spdcur
     &        +wp_con_fcst_inten(n)*fcst_inten
     &        +wp_con_init_lat(n)*xlati
     &        +wp_con_lond(n)*xlond
     &        +wp_con_numod(n)*xnumod
     &        +wp_con_const(n)+wp_siglvl_const(n)
          if(err_rad(n).lt.wp_siglvl_const(n))
     x         err_rad(n)=wp_siglvl_const(n)
        else
          err_rad(n)=0.
        endif
      endif
      if (basin.eq.'sh') then
        if(intlag(n).ne.0.0.and.intlog(n).gt.0.01) then
          xlond = -(intlog(n) - loncur)
          xlatd = -(intlag(n) - latcur)
          xlati = -latcur
          xloni = 360.-loncur
          xnumod=float(numod(n))
         if(xloni.lt.140.) then
          err_rad(n)=shs_con_spread(n)*spread(n)
     &        +shs_con_init_inten(n)*spdcur
     &        +shs_con_init_lat(n)*xlati
     &        +shs_con_init_lon(n)*xloni
     &        +shs_con_numod(n)*xnumod
     &        +shs_con_const(n)+shs_siglvl_const(n)
          if(err_rad(n).lt.shs_siglvl_const(n))
     x         err_rad(n)=shs_siglvl_const(n)
         else
          err_rad(n)=shp_con_spread(n)*spread(n)
     &        +shp_con_init_inten(n)*spdcur
     &        +shp_con_init_lat(n)*xlati
     &        +shp_con_init_lon(n)*xloni
     &        +shp_con_numod(n)*xnumod
cx  equation now the same as other SH... sampson
cx   &        +shp_con_latd(n)*xlatd
cx   &        +shp_con_lond(n)*xlond
cx   &        +shp_con_speed(n)*speed
     &        +shp_con_const(n)+shp_siglvl_const(n)
          if(err_rad(n).lt.shp_siglvl_const(n))
     x        err_rad(n)=shp_siglvl_const(n)
         endif
        else
          err_rad(n)=0.
        endif
      endif
      enddo
      return
      end
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine GPCE_AX(predictors_x,coefs_x,predictors_a,
     >                   coefs_a,boost_x,npreds,gxcross,gxalong)

c***  This routine pulls in the across-track and along-track
c***  GPCE-AX predictors and coefficients as well as the
c***  across-track boost factor in order to produce the GPCE-AX
c***  across-track and along-track 70% radii.
c***
c***  The radii are the across-track and along-track semi-major
c***  axis that define the elliptical 70% isopleth centered on
c***  the across-track/along-track bias corrected consensus
c***  forecast.
c***
c***  Input variables:
c***     predictors_x  - A vector of length npreds that contains
c***                     the values of the across-track predictors
c***     coefs_x       - A vector of length npreds+1 that contains
c***                     the GPCE-AX across-track coefficients.
c***     predictors_a  - A vector of length npreds that contains
c***                     the values of the along-track predictors
c***     coefs_a       - A vector of length npreds+1 that contains
c***                     the GPCE-AX along-track coefficients.
c***     boost_x       - A scalar that contains the across-track
c***                     boost factor that transforms the across-track
c***                     predicted error to the across-track radius.
c***     npreds        - The number of GPCE-AX predictors.
c***
c***  Internal variables:
c***     forecast_x    - The predicted across-track error.
c***     forecast_a    - The predicted along-track error.
c***     boost_a       - A scalar that is calculated as a function
c***                     of boost_x, forecast_x and forecast_a that
c***                     contains the along-track boost factor that
c***                     transforms the along-track predicted error
c***                     to the along-track radius.
c***
c***  Output variables:
c***     gxcross       - GPCE-AX across-track radius.
c***     gxalong       - GPCE-AX along-track radius.
c***
c***  Written by: Jim Hansen
c***  Modified by Sampson to take care of 60, 84 and 108
c***  Last modified: March 2009

      implicit none

      integer npreds

      real predictors_x(npreds), predictors_a(npreds)
      real coefs_x(npreds+1), coefs_a(npreds+1)
      real boost_x, boost_a
      real forecast_x, forecast_a

      real gxcross, gxalong

c***  Perform across-track and along-track predictions
      forecast_x = coefs_x(1)+coefs_x(2)*predictors_x(1)+
     >                        coefs_x(3)*predictors_x(2)
      forecast_a = coefs_a(1)+coefs_a(2)*predictors_a(1)+
     >                        coefs_a(3)*predictors_a(2)

cx
cx    Capture missing taus (60, 84, 108)
cx
      if (forecast_x .eq. 0.0 .or. forecast_a .eq. 0.0) then
        gxcross = 0.0
        gxalong = 0.0
        return
      endif

c***  Boost the across-track forecast
      gxcross = forecast_x+boost_x

c***  Calculate the along-track boost value (note, this
c***  step is included for clarity.  Simply remove the
c***  '-forecast_a' on the RHS and you have an expression
c***  for gxalong).
      boost_a = ((forecast_a*gxcross)/forecast_x)-forecast_a

c***  Boost the along-track forecast
      gxalong = forecast_a+boost_a


      return
      end

c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine assign_ax(intau,latcur,loncur,spdcur,fstlat,fstlon,
     &                  asprd,xsprd,theta,
     &                  bias_cross, bias_along,
     &                  biaslat,biaslon,predictors_x,coefs_x,
     &                  predictors_a,coefs_a, boost_x)
c***  Input  variables:
c***     intau         - The forecast tau index for which to produce
c***                      predictors and the coefficients
c***     latcur        - The current latitude
c***     loncur        - The current longitude
c***     spdcur        - The current intensity
c***     fstlat        - The forecast latitude
c***     fstlon        - The forecast longitude
c***     asprd         - The forecast ensemble along track spread
c***     xsprd         - The forecast ensemble cross track spread
c***     theta         - The cross track direction (math coords)
c***  Output variables:
c***     bias_cross    - The cross track bias (nm)
c***     bias_along    - The along track bias (nm)
c***     biaslat       - The forecast latitude (bias corrected)
c***     biaslon       - The forecast longitude (bias corrected)
c***     predictors_x  - A vector of length npreds that contains
c***                     the values of the across-track predictors
c***     coefs_x       - A vector of length npreds+1 that contains
c***                     the GPCE-AX across-track coefficients.
c***     predictors_a  - A vector of length npreds that contains
c***                     the values of the along-track predictors
c***     coefs_a       - A vector of length npreds+1 that contains
c***                     the GPCE-AX along-track coefficients.
c***     boost_x       - A scalar that contains the across-track
c***                     boost factor that transforms the across-track
c***                     predicted error to the across-track radius.
c***
c***  Written by: Sampson
c***  Last modified: August 2009
c***
c***  Modifications:
c***                 Aug 2009 - Hansen updated predictors for atl 2009
c***                            season

      implicit none

      integer intau, itau
      integer npreds
c***     ltau          - The number of taus.
      integer ltau
      parameter ( ltau=11, npreds = 2 )
      real latcur, loncur, spdcur
      real fstlat, fstlon
      real theta
      real dir, xlat, xlon
      real bias_cross, bias_along
      real biaslat, biaslon
      real asprd, xsprd
      real predictors_x(npreds), predictors_a(npreds)
      real bias_x (ltau), bias_a (ltau)
      real const_x(ltau), const_a(ltau)
      real coef1_x(ltau), coef1_a(ltau)
      real coef2_x(ltau), coef2_a(ltau)
      real boosa_x(ltau)
      real coefs_x(npreds+1), coefs_a(npreds+1)
      real boost_x

c***  All the bias, coefficient, and boost values (output of
c***  make_across_GPCE, make_along_GPCE, and tune_across_along_GPCE_2d)
c
c      0 - 120 by 12
      data bias_x  / 0.0,  2.0118,   5.0073,   7.9294,   12.6399, 0.0,
     &           20.0772,     0.0,  18.4405,      0.0,  -0.1804/
      data bias_a  / 0.0, -0.1706, -1.2201,   -4.7781, -10.038, 0.0,
     &         -19.346,     0.0,  -32.1786,      0.0, -64.5993/
      data const_x / 0.0, 20.9453,  28.71,  44.2487,   53.4229, 0.0,
     &           99.1342,     0.0,  115.4063,      0.0,  8.5005/
      data coef1_x / 0.0,-0.11891,  0.51663,  0.47906,   0.50204, 0.0,
     &           0.37783,     0.0,  0.61283,      0.0,   0.78439/
      data coef2_x / 0.0, 0.45505, -0.12985, -0.26871,  -0.3301, 0.0,
     &          -0.67464,     0.0,   -0.83217,      0.0,    2.756/
      data const_a / 0.0, 23.909,  34.6322,  43.4422,   50.3711, 0.0,
     &           62.7908,     0.0,  78.5819,      0.0,  125.6122/
      data coef1_a / 0.0,-0.14753,  0.52938,  0.5691,   0.59069, 0.0,
     &           0.53391,     0.0,  0.61938,      0.0,   0.52929/
      data coef2_a / 0.0, 0.48827, -0.18243, -0.20443,  -0.2143, 0.0,
     &          -1.64,     0.0, -1.8179,      0.0,  -2.7814/
      data boosa_x / 0.0,     17.,      28.,      36.,       45., 0.0,
     &               72.,     0.0,     97.,      0.0,      131./

      itau = (intau-1)*12
      coefs_x(1)      = const_x(intau)
      coefs_x(2)      = coef1_x(intau)
      coefs_x(3)      = coef2_x(intau)
      boost_x         = boosa_x(intau)
      coefs_a(1)      = const_a(intau)
      coefs_a(2)      = coef1_a(intau)
      coefs_a(3)      = coef2_a(intau)

c***  Across and along predictors for each lead (output of
c***  make_across_GPCE and make_along_GPCE)
      if (itau .eq. 12) then
         predictors_x(1) = spdcur
         predictors_x(2) = xsprd
         predictors_a(1) = spdcur
         predictors_a(2) = asprd
      elseif (itau .eq. 24) then
         predictors_x(1) = xsprd
         predictors_x(2) = spdcur
         predictors_a(1) = asprd
         predictors_a(2) = spdcur
      elseif (itau .eq. 36) then
         predictors_x(1) = xsprd
         predictors_x(2) = loncur
         predictors_a(1) = asprd
         predictors_a(2) = spdcur
      elseif (itau .eq. 48) then
         predictors_x(1) = xsprd
         predictors_x(2) = loncur
         predictors_a(1) = asprd
         predictors_a(2) = spdcur
      elseif (itau .eq. 72) then
         predictors_x(1) = xsprd
         predictors_x(2) = loncur
         predictors_a(1) = asprd
         predictors_a(2) = abs(fstlon - loncur)
      elseif (itau .eq. 96) then
         predictors_x(1) = xsprd
         predictors_x(2) = loncur
         predictors_a(1) = asprd
         predictors_a(2) = abs(fstlon - loncur)
      elseif (itau .eq. 120) then
         predictors_x(1) = xsprd
         predictors_x(2) = latcur
         predictors_a(1) = asprd
         predictors_a(2) = abs(fstlon - loncur)
      endif

cx
cx    get new biased lat and lon
cx
cx    cross track
      dir = 90.0 - theta
      if (dir .lt. 0.0) dir = dir + 360.0
      call rltlg(fstlat,fstlon,xlat,xlon,dir,bias_x(intau))
cx    along track
      dir = dir - 180.0
      if (dir .lt. 0.0) dir = dir + 360.0
      call rltlg(xlat,xlon,biaslat,biaslon,dir,bias_a(intau))
      bias_cross = bias_x(intau)
      bias_along = bias_a(intau)

      return
      end
c********1*********2*********3*********4*********5*********6*********7**
c
      subroutine assign_wp(intau,latcur,loncur,spdcur,fstlat,fstlon,
     &                  fstspd,asprd,xsprd,theta,
     &                  bias_cross, bias_along,
     &                  biaslat,biaslon,predictors_x,coefs_x,
     &                  predictors_a,coefs_a, boost_x)
c***  Input  variables:
c***     intau         - The forecast tau index for which to produce
c***                      predictors and the coefficients
c***     latcur        - The current latitude
c***     loncur        - The current longitude
c***     spdcur        - The current intensity
c***     fstlat        - The forecast latitude
c***     fstlon        - The forecast longitude
c***     fstspd        - The forecast intensity
c***     asprd         - The forecast ensemble along track spread
c***     xsprd         - The forecast ensemble cross track spread
c***     theta         - The cross track direction (math coords)
c***  Output variables:
c***     bias_cross    - The cross track bias (nm)
c***     bias_along    - The along track bias (nm)
c***     biaslat       - The forecast latitude (bias corrected)
c***     biaslon       - The forecast longitude (bias corrected)
c***     predictors_x  - A vector of length npreds that contains
c***                     the values of the across-track predictors
c***     coefs_x       - A vector of length npreds+1 that contains
c***                     the GPCE-AX across-track coefficients.
c***     predictors_a  - A vector of length npreds that contains
c***                     the values of the along-track predictors
c***     coefs_a       - A vector of length npreds+1 that contains
c***                     the GPCE-AX along-track coefficients.
c***     boost_x       - A scalar that contains the across-track
c***                     boost factor that transforms the across-track
c***                     predicted error to the across-track radius.
c***
c***  Written by: Sampson
c***  Last modified: August 2009
c***
c***  Modifications:
c***                 Aug 2009 - Hansen updated predictors for wpac 2009
c***                            season

      implicit none

      integer intau, itau
      integer npreds
c***     ltau          - The number of taus.
      integer ltau
      parameter ( ltau=11, npreds = 2 )
      real latcur, loncur, spdcur
      real fstlat, fstlon, fstspd
      real theta
      real dir, xlat, xlon
      real bias_cross, bias_along
      real biaslat, biaslon
      real asprd, xsprd
      real predictors_x(npreds), predictors_a(npreds)
      real bias_x (ltau), bias_a (ltau)
      real const_x(ltau), const_a(ltau)
      real coef1_x(ltau), coef1_a(ltau)
      real coef2_x(ltau), coef2_a(ltau)
      real boosa_x(ltau)
      real coefs_x(npreds+1), coefs_a(npreds+1)
      real boost_x

c***  All the bias, coefficient, and boost values (output of
c***  make_across_GPCE, make_along_GPCE, and tune_across_along_GPCE_2d)
c
c      0 - 120 by 12
      data bias_x  / 0.0,-1.251,  -0.27427, -0.46631,-0.83852,0.0,
     &           -4.5465,     0.0,  -8.0966,      0.0,  -15.1331/
      data bias_a  / 0.0, 3.9807, 10.5846,  15.4554, 22.9429, 0.0,
     &         33.9558,     0.0,  -18.0599,      0.0, -17.6565/
      data const_x / 0.0, 18.3315,  43.4878,  55.0277,   59.9466, 0.0,
     &           77.1236,     0.0,  113.9329,      0.0,  85.7679/
      data coef1_x / 0.0, 0.66836,  0.46903,  0.44327,  0.48396, 0.0,
     &           0.51903,     0.0,  0.42675,      0.0,   0.41732/
      data coef2_x / 0.0, -0.1142, -0.27296, -0.30845,  -0.31008, 0.0,
     &          -0.33296,     0.0,   -0.24094,      0.0,    0.42888/
      data const_a / 0.0, 23.2252,  35.3675,  41.4909,   49.7764, 0.0,
     &           61.3209,     0.0,  146.0932,      0.0,  72.9709/
      data coef1_a / 0.0, 0.52375,  0.55139,  0.48669,   0.54693, 0.0,
     &           0.51863,     0.0,  0.50663,      0.0,   0.5604/
      data coef2_a / 0.0, -0.1501, -0.21839, -0.19794,  -0.24416, 0.0,
     &          2.1493,     0.0, -0.61365,      0.0,  4.6518/
      data boosa_x / 0.0,     19.,      33.,      47.,       57., 0.0,
     &               82.,     0.0,     108.,      0.0,      139./

      itau = (intau-1)*12
      coefs_x(1)      = const_x(intau)
      coefs_x(2)      = coef1_x(intau)
      coefs_x(3)      = coef2_x(intau)
      boost_x         = boosa_x(intau)
      coefs_a(1)      = const_a(intau)
      coefs_a(2)      = coef1_a(intau)
      coefs_a(3)      = coef2_a(intau)

c***  Across and along predictors for each lead (output of
c***  make_across_GPCE and make_along_GPCE)
      if (itau .eq. 12) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = spdcur
      elseif (itau .eq. 24) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = fstspd
      elseif (itau .eq. 36) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = fstspd
      elseif (itau .eq. 48) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = fstspd
      elseif (itau .eq. 72) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = abs(fstlat - latcur)
      elseif (itau .eq. 96) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = fstspd
      elseif (itau .eq. 120) then
         predictors_x(1) = xsprd
         predictors_x(2) = fstspd
         predictors_a(1) = asprd
         predictors_a(2) = abs(fstlat - latcur)
      endif

cx
cx    get new biased lat and lon
cx
cx    cross track
      dir = 90.0 - theta
      if (dir .lt. 0.0) dir = dir + 360.0
      call rltlg(fstlat,fstlon,xlat,xlon,dir,bias_x(intau))
cx    along track
      dir = dir - 180.0
      if (dir .lt. 0.0) dir = dir + 360.0
      call rltlg(xlat,xlon,biaslat,biaslon,dir,bias_a(intau))
      bias_cross = bias_x(intau)
      bias_along = bias_a(intau)

      return
      end
      subroutine do_corr_con ( new_tech, strmid, newlat, newlon, spread,
     &                        numod, corr_lat, corr_lon )
      parameter ( nmodmax=1000, ltau=11 , intrcnt=45 )
c
      common /initial/ latcur, loncur, spdcur
      common /minus12/ latm12, lonm12, spdm12
      common /intrp/ intlat(ltau,nmodmax), intlon(ltau,nmodmax),
     &               intspd(ltau,nmodmax)
c
      real intlat, intlon, intspd
      real latcur, loncur, spdcur
      real latm12, lonm12, spdm12
      integer numod(ltau)
      real newlat(ltau), newlon(ltau)
      real spread(ltau),corr_lat(ltau),corr_lon(ltau)
      real al_conulat_bias(ltau),al_conulon_bias(ltau)
      real al_conulat_spread(ltau),al_conulat_latd(ltau)
     &,    al_conulat_lond(ltau),al_conulat_const(ltau)
      real al_gunalat_bias(ltau),al_gunalon_bias(ltau)
      real al_gunalat_spread(ltau),al_gunalat_latd(ltau)
     &,    al_gunalat_lond(ltau),al_gunalat_const(ltau)
      real ep_conulat_bias(ltau),ep_conulon_bias(ltau)
      real ep_conulat_latd(ltau),ep_conulat_init_lon(ltau)
     &,    ep_conulat_lond(ltau),ep_conulat_numem(ltau)
     &,    ep_conulat_fcst_inten(ltau),ep_conulat_init_inten(ltau)
     &,    ep_conulat_speed(ltau),ep_conulat_init_lat(ltau)
      real ep_conulon_init_lat(ltau),ep_conulon_speed(ltau)
     &,    ep_conulon_latd(ltau),ep_conulon_lond(ltau)
     &,    ep_conulon_fcst_inten(ltau)
     &,    ep_conulon_spread(ltau)
      real ep_conulat_const(ltau),ep_conulon_const(ltau)
      real wp_conwlat_bias(ltau),wp_conwlon_bias(ltau)
     &,    wp_conwlat_latd(ltau),wp_conwlat_init_lat(ltau)
     &,    wp_conwlat_fcst_inten(ltau)
     &,    wp_conwlon_init_lat(ltau),wp_conwlon_latd(ltau)
     &,    wp_conwlon_lond(ltau)
     &,    wp_conwlon_fcst_inten(ltau),wp_conwlon_init_inten(ltau)
     &,    wp_conwlat_const(ltau),wp_conwlon_const(ltau)
c
c**   Correction coefficients for TVCN and/or CONU (Atlantic)
c
c    Derived from 2004-2010 Seasons
c
      data al_conulat_bias/0.,.05,.10,.14,.15,0.,.17,4*0./
      data al_conulon_bias/0.,.03,.07,.09,.04,0.,-.04,4*0./
      data al_conulat_spread/11*0./
      data al_conulat_latd/10*0.,.149/
      data al_conulat_lond/8*0.,-.051,0.,-.09/
      data al_conulat_const/8*0.,.995,0.,-.036/
c
c    Derived from 2003-2008 Seasons
c
c     data al_conulat_bias/0.,.06,.14,.22,.32,0.,.48,4*0./
c     data al_conulon_bias/0.,.10,.20,.30,.37,0.,.34,4*0./
c     data al_conulat_spread/8*0.,.0031,0.,.0050/
c     data al_conulat_latd/8*0.,.110,0.,.145/
c     data al_conulat_lond/8*0.,-.035,0.,-.038/
c     data al_conulat_const/8*0.,-.33,0.,-.82/
c
c    Derived from 2002-2007 Seasons
c
c     data al_conulat_bias/0.,.07,.16,.27,.36,0.,.50,4*0./
c     data al_conulon_bias/0.,.07,.13,.18,.19,0.,.03,4*0./
c     data al_conulat_spread/8*0.,.0078,0.,.0086/
c     data al_conulat_latd/8*0.,.167,0.,.226/
c     data al_conulat_const/8*0.,-1.76,0.,-2.81/
c
c    Derived from 2001-2006 Seasons
c
c     data al_conulat_bias/0.,.05,.12,.20,.27,0.,.38,4*0./
c     data al_conulon_bias/0.,.10,.20,.28,.34,0.,.30,4*0./
c     data al_conulat_spread/8*0.,.0076,0.,.0074/
c     data al_conulat_latd/8*0.,.148,0.,.199/
c     data al_conulat_const/8*0.,-1.71,0.,-2.49/
c
c    Derived from 2001-2005 Seasons
c
c     data al_conulat_bias/0.,.05,.12,.21,.27,0.,.37,4*0./
c     data al_conulon_bias/0.,.10,.20,.28,.34,0.,.32,4*0./
c     data al_conulat_spread/8*0.,.0074,0.,.0069/
c     data al_conulat_latd/8*0.,.149,0.,.2/
c     data al_conulat_const/8*0.,-1.61,0.,-2.25/
c
c    Derived from 2001-2004 Seasons
c
c     data al_conulat_bias/0.,.06,.12,.17,.21,0.,.30,4*0./
c     data al_conulon_bias/0.,.10,.19,.25,.29,0.,.26,4*0./
c     data al_conulat_spread/8*0.,.0084,0.,.0062/
c     data al_conulat_latd/8*0.,.132,0.,.187/
c     data al_conulat_const/8*0.,-1.75,0.,-2.11/
c
c**   Correction coefficients for TCON and/or GUNA (Atlantic)
c
c    Derived from 2004-2010 Seasons
c
      data al_gunalat_bias/0.,.05,.10,.14,.15,0.,.17,4*0./
      data al_gunalon_bias/0.,.03,.07,.09,.04,0.,-.04,0.,-.23
     &,                    0.,-.73/
      data al_gunalat_spread/11*0./
      data al_gunalat_latd/10*0.,.149/
      data al_gunalat_lond/8*0.,-.051,0.,-.09/
      data al_gunalat_const/8*0.,.995,0.,-.036/
c
c    Derived from 2003-2008 Seasons
c
c     data al_gunalat_bias/0.,.08,.18,.27,.37,0.,.51,4*0./
c     data al_gunalon_bias/0.,.09,.18,.26,.29,0.,.16,0.,-.26
c    &,                    0.,-.93/
c     data al_gunalat_spread/10*0.,.0052/
c     data al_gunalat_latd/8*0.,.091,0.,.175/
c     data al_gunalat_lond/8*0.,-.037,2*0./
c     data al_gunalat_const/8*0.,.41,0.,-1.34/
c
c    Derived from 2002-2007 Seasons
c
c     data al_gunalat_bias/0.,.10,.21,.31,.41,0.,.55,4*0./
c     data al_gunalon_bias/0.,.06,.11,.12,.11,0.,-.12,0.,-.54
c    &,                    0.,-1.2/
c     data al_gunalat_spread/8*0.,.0058,0.,.0072/
c     data al_gunalat_latd/8*0.,.12,0.,.194/
c     data al_gunalat_const/8*0.,-.98,0.,-2.02/
c
c    Derived from 2001-2006 Seasons
c
c     data al_gunalat_bias/0.,.08,.17,.24,.32,0.,.45,4*0./
c     data al_gunalon_bias/0.,.10,.19,.24,.30,0.,.17,0.,-.30
c    &,                    0.,-.83/
c     data al_gunalat_spread/8*0.,.0056,0.,.0054/
c     data al_gunalat_latd/8*0.,.110,0.,.178/
c     data al_gunalat_const/8*0.,-.99,0.,-1.64/
c
c    Derived from 2001-2005 Seasons
c
c     data al_gunalat_bias/0.,.06,.14,.21,.26,0.,.36,4*0./
c     data al_gunalon_bias/0.,.11,.22,.30,.42,0.,.37,0.,-.11
c    &,                    0.,-.83/
c     data al_gunalat_spread/8*0.,.0048,0.,.0049/
c     data al_gunalat_latd/8*0.,.105,0.,.189/
c     data al_gunalat_const/8*0.,-.81,0.,-1.46/
c
c    Derived from 2001-2004 Seasons
c
c     data al_gunalat_bias/0.,.06,.12,.16,.17,0.,.32,4*0./
c     data al_gunalon_bias/0.,.12,.22,.28,.40,0.,.31,0.,-.16
c    &,                    0.,-.53/
c     data al_gunalat_spread/8*0.,.0047,2*0./
c     data al_gunalat_latd/10*0.,.141/
c     data al_gunalat_const/8*0.,.05,0.,.10/
c
c**   Correction coefficients for TVCN and/or CONU (Eastern North Pacific)
c
c    Derived from 2005-2010 Seasons
c
      data ep_conulat_bias/11*0./
      data ep_conulon_bias/11*0./
      data ep_conulat_latd/0.,.152,9*0./
      data ep_conulat_init_lon/0.,.004,.008,.013,.02,0.,.031,0.
     &,                        .051,0.,.066/
      data ep_conulat_init_lat/8*0.,-.171,0.,-.272/
      data ep_conulat_lond/4*0.,-.05,0.,-.082,0.,-.118,0.,-.118/
      data ep_conulat_numem/11*0./
      data ep_conulat_init_inten/11*0./
      data ep_conulat_fcst_inten/11*0./
      data ep_conulat_speed/11*0./
      data ep_conulon_spread/3*0.,-.005,2*0.,-.007,0.,-.004,2*0./
      data ep_conulon_init_lat/2*0.,.042,.055,2*0.,.188,0.,.285,0.,.269/
      data ep_conulon_speed/0.,-.063,-.077,-.044,-.061,6*0./
      data ep_conulon_latd/10*0.,.157/
      data ep_conulon_fcst_inten/6*0.,.017,0.,.031,0.,.041/
      data ep_conulon_lond/0.,.211,.119,7*0.,.073/
      data ep_conulat_const/0.,-.59,-.98,-1.60,-2.13,0.,-2.91,0.
     &,                     -2.02,0.,-2.12/
      data ep_conulon_const/0.,.25,-.37,-.20,.43,0.,-3.18,0.
     &,                     -5.58,0.,-8.33/
c
c    Derived from 2004-2008 Seasons
c
c     data ep_conulat_bias/11*0./
c     data ep_conulon_bias/11*0./
c     data ep_conulat_latd/0.,.197,.101,3*0.,.146,3*0.,.195/
c     data ep_conulat_init_lon/0.,.005,.011,.017,.022,0.,.05,0.
c    &,                        .058,0.,.084/
c     data ep_conulat_lond/6*0.,-.057,0.,-.082,2*0./
c     data ep_conulat_numem/11*0./
c     data ep_conulat_init_inten/6*0.,-.011,3*0.,-.019/
c     data ep_conulat_fcst_inten/11*0./
c     data ep_conulat_speed/10*0.,-.114/
c     data ep_conulon_spread/6*0.,-.010,0.,-.007,2*0./
c     data ep_conulon_init_lat/4*0.,.089,0.,.28,0.
c    &,                        .60,0.,1.06/
c     data ep_conulon_speed/0.,-.073,-.095,-.111,-.127,0.,-.092,3*0.
c    &,                     -.249/
c     data ep_conulon_latd/11*0./
c     data ep_conulon_fcst_inten/10*0.,.044/
c     data ep_conulon_lond/0.,.265,.154,.111,.093,5*0.,.218/
c     data ep_conulat_const/0.,-.75,-1.19,-2.04,-2.61,0.,-5.11,0.
c    &,                     -5.62,0.,-8.29/
c     data ep_conulon_const/0.,.24,.33,.41,-.89,0.,-2.46,0.
c    &,                     -8.0,0.,-18.5/
c
c    Derived from 2003-2007 Seasons
c
c     data ep_conulat_bias/11*0./
c     data ep_conulon_bias/11*0./
c     data ep_conulat_latd/0.,.159,4*0.,.132,3*0.,.191/
c     data ep_conulat_init_lon/2*0.,.009,.014,.019,0.,.03,0.
c    &,                        .035,0.,.054/
c     data ep_conulat_lond/11*0./
c     data ep_conulat_numem/11*0./
c     data ep_conulat_fcst_inten/6*0.,-.018,0.,-.029,0.,-.034/
c     data ep_conulon_spread/6*0.,-.008,4*0./
c     data ep_conulon_init_lat/4*0.,.107,0.,.32,0.
c    &,                        .695,0.,1.38/
c     data ep_conulon_speed/0.,-.068,-.102,-.144,-.173,0.,-.111,4*0./
c     data ep_conulon_latd/11*0./
c     data ep_conulon_lond/0.,.243,.173,.169,.15,6*0./
c     data ep_conulat_const/0.,-.09,-1.08,-1.71,-2.21,0.,-2.8,0.
c    &,                     -2.08,0.,-4.91/
c     data ep_conulon_const/0.,.24,.34,.45,-1.11,0.,-3.04,0.
c    &,                     -10.36,0.,-19.84/
c
c    Derived from 2002-2006 Seasons
c
c     data ep_conulat_bias/11*0./
c     data ep_conulon_bias/11*0./
c     data ep_conulat_latd/0.,.133,9*0./
c     data ep_conulat_init_lon/2*0.,.0069,.0108,.0145,0.,.03,4*0./
c     data ep_conulat_lond/6*0.,-.067,0.,-.0815,0.,-.1055/
c     data ep_conulat_numem/2*0.,.1,.17,.219,6*0./
c     data ep_conulat_fcst_inten/8*0.,-.026,0.,-.034/
c     data ep_conulon_init_lat/2*0.,.035,.082,.128,0.,.288,0.
c    &,                        .595,0.,1.14/
c     data ep_conulon_speed/0.,-.07,9*0./
c     data ep_conulon_latd/11*0./
c     data ep_conulon_lond/0.,.25,9*0./
c     data ep_conulat_const/0.,-.1,-1.3,-2.1,-2.7,0.,-2.8,0.
c    &,                     2.6,0.,3.5/
c     data ep_conulon_const/0.,.2,-.7,-1.5,-2.3,0.,-4.8,0.
c    &,                     -9.2,0.,-17./
c
c    Derived from 2002-2005 Seasons
c
c     data ep_conulat_bias/11*0./
c     data ep_conulon_bias/11*0./
c     data ep_conulat_latd/0.,.132,9*0./
c     data ep_conulat_init_lon/2*0.,.0086,.0112,.0139,0.,.026,4*0./
c     data ep_conulat_lond/10*0.,-.0704/
c     data ep_conulat_numem/3*0.,.140,.194,6*0./
c     data ep_conulat_fcst_inten/8*0.,-.022,0.,-.032/
c     data ep_conulon_init_lat/2*0.,.036,.092,.146,0.,.312,0.
c    &,                        .617,0.,1.132/
c     data ep_conulon_speed/0.,-.084,9*0./
c     data ep_conulon_latd/0.,.15,9*0./
c     data ep_conulon_lond/0.,.301,9*0./
c     data ep_conulat_const/0.,-.1,-1.05,-2.,-2.5,0.,-2.9,0.
c    &,                     1.53,0.,3./
c     data ep_conulon_const/0.,.17,-.74,-1.7,-2.6,0.,-5.05,0.
c    &,                     -9.6,0.,-16.9/
c
c**   Correction coefficients for CONW (Western North Pacific)
c
c
c    Derived from 2004-2008 Seasons
c
      data wp_conwlat_bias/11*0./
      data wp_conwlon_bias/8*0.,.37,0.,.83/
      data wp_conwlat_latd/8*0.,.091,0.,.191/
      data wp_conwlat_init_lat/0.,-.014,-.021,-.026,-.032,0.,-.045
     x,                        4*0./
      data wp_conwlat_fcst_inten/10*0.,-.019/
      data wp_conwlon_init_lat/4*0.,-.049,0.,-.071,4*0./
      data wp_conwlon_latd/11*0./
      data wp_conwlon_lond/0.,-.077,-.099,-.098,-.077,6*0./
      data wp_conwlon_fcst_inten/6*0.,.027,4*0./
      data wp_conwlon_init_inten/6*0.,-.019,4*0./
      data wp_conwlat_const/0.,.23,.37,.43,.50,0.,.62,0.
     &,                     -.97,0.,-.23/
      data wp_conwlon_const/0.,-.14,-.29,-.38,.48,0.,.49,4*0./
c
c    Derived from 2004-2007 Seasons
c
c     data wp_conwlat_bias/11*0./
c     data wp_conwlon_bias/8*0.,-.03,0.,.27/
c     data wp_conwlat_latd/10*0.,.139/
c     data wp_conwlat_init_lat/0.,-.014,-.024,-.032,-.04,0.,-.059
c    x,                        0.,-.113,0.,-.143/
c     data wp_conwlon_init_lat/4*0.,-.044,0.,-.084,4*0./
c     data wp_conwlon_latd/11*0./
c     data wp_conwlon_lond/0.,-.075,-.09,-.1,-.09,0.,-.079,4*0./
c     data wp_conwlat_const/0.,.25,.42,.52,.62,0.,.83,0.
c    &,                     1.42,0.,.79/
c     data wp_conwlon_const/0.,-.16,-.32,-.5,.18,0.,.68,4*0./
c
c    Derived from 2003-2006 Seasons
c
c     data wp_conwlat_bias/11*0./
c     data wp_conwlon_bias/8*0.,.09,0.,.30/
c     data wp_conwlat_latd/10*0.,.164/
c     data wp_conwlat_init_lat/0.,-.016,-.027,-.036,-.047,0.,-.074
c    x,                        0.,-.163,0.,-.195/
c     data wp_conwlon_init_lat/3*0.,-.036,-.06,0.,-.111,4*0./
c     data wp_conwlon_latd/11*0./
c     data wp_conwlon_lond/0.,-.075,-.1,-.087,-.092,0.,-.075,4*0./
c     data wp_conwlat_const/0.,.27,.46,.61,.74,0.,1.05,0.
c    &,                     2.15,0.,1.29/
c     data wp_conwlon_const/0.,-.17,-.37,.15,.39,0.,1.1,4*0./
      character*4  new_tech
      character*8  strmid
      character*2  basin
c
      basin = strmid (1:2)
c
      pi = 3.14159
      do n = 1, ltau

         if( intspd(n,1) .gt. 0.0)  then
            fcst_inten = intspd(n,1)
         else
            fcst_inten = 65.0
         endif
         if( latcur.gt.0.0.and.loncur.gt.0.0.and.latm12.gt.0.0
     &       .and.lonm12.gt.0.0) then
            avglat = 0.5 * (latcur + latm12)
            coslat  = cos( avglat*pi/180.0 )
            ydist = 60. * (latcur - latm12)
            xdist = 60. * (loncur - lonm12) * coslat
            speed = sqrt(xdist**2 + ydist**2)/12.
         else
            speed = 8.5
         endif
c
         if ( (new_tech .eq. 'CONW' .or. new_tech .eq. 'TESW' ) .and.
     &        ( basin .eq. 'wp' ) ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = -(newlon(n) - loncur)
               corr_lat(n) = wp_conwlat_init_lat(n)*latcur
     &              + wp_conwlat_latd(n)*xlatd
     &              + wp_conwlat_fcst_inten(n)*fcst_inten
     &              + wp_conwlat_const(n) + wp_conwlat_bias(n)
               corr_lon(n) = wp_conwlon_init_lat(n)*latcur
     &              + wp_conwlon_latd(n)*xlatd
     &              + wp_conwlon_lond(n)*xlond
     &              + wp_conwlon_fcst_inten(n)*fcst_inten
     &              + wp_conwlon_init_inten(n)*spdcur
     &              + wp_conwlon_const(n) + wp_conwlon_bias(n)
               corr_lon(n)= - corr_lon(n)
            else
               corr_lat(n) = 0.0
               corr_lon(n) = 0.0
            endif
         endif
         if ( (new_tech .eq. 'CONU' .or. new_tech .eq. 'TVCN')
     &       .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = newlon(n) - loncur
               corr_lat(n) = al_conulat_spread(n)*spread(n)
     &              + al_conulat_latd(n)*xlatd
     &              + al_conulat_lond(n)*xlond
     &              + al_conulat_const(n) + al_conulat_bias(n)
               corr_lon(n) = al_conulon_bias(n)
            else
               corr_lat(n) = 0.0
               corr_lon(n) = 0.0
            endif
         endif
         if ( (new_tech .eq. 'CONU' .or. new_tech .eq. 'TVCN')
     &       .and. basin .eq. 'ep' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = newlon(n) - loncur
               xnumod=float(numod(n))
               corr_lat(n) = ep_conulat_init_lon(n)*loncur
     &              + ep_conulat_latd(n)*xlatd
     &              + ep_conulat_lond(n)*xlond
     &              + ep_conulat_speed(n)*speed
     &              + ep_conulat_numem(n)*xnumod
     &              + ep_conulat_init_inten(n)*spdcur
     &              + ep_conulat_fcst_inten(n)*fcst_inten
     &              + ep_conulat_const(n) + ep_conulat_bias(n)
               corr_lon(n) = ep_conulon_speed(n)*speed
     &              + ep_conulon_spread(n)*spread(n)
     &              + ep_conulon_latd(n)*xlatd
     &              + ep_conulon_lond(n)*xlond
     &              + ep_conulon_init_lat(n)*latcur
     &              + ep_conulon_fcst_inten(n)*fcst_inten
     &              + ep_conulon_const(n) + ep_conulon_bias(n)
            else
               corr_lat(n) = 0.0
               corr_lon(n) = 0.0
            endif
         endif
c
         if ( (new_tech .eq. 'GUNA' .or. new_tech .eq. 'TCON')
     &       .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = newlon(n) - loncur
               corr_lat(n) = al_gunalat_spread(n)*spread(n)
     &              + al_gunalat_latd(n)*xlatd
     &              + al_gunalat_lond(n)*xlond
     &              + al_gunalat_const(n) + al_gunalat_bias(n)
               corr_lon(n) = al_gunalon_bias(n)
            else
               corr_lat(n) = 0.0
               corr_lon(n) = 0.0
            endif
         endif
c
      enddo
c
      return
      end
