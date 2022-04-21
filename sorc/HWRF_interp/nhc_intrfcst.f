      program nhc_intrfcst
c
c**   This program interpolates and extrapolates the official and model
c**      in time and space
c
c**   Remove the "cloop" comments to allow the program to interpolate
c**     an entire adeck
c
c**   Remove the "clocal" comments to allow the program to work in 
c**      local directory
c
      parameter ( nmodel=11 )
c
      common /techs/ fst_tech(nmodel), tech_12(nmodel), tech_06(nmodel)
c
      real    blat, blon
      integer iwind
c
      character*1   ns, ew
      character*2   bttype
      character*4   fst_tech, tech_12, tech_06
      character*8   strmid
      character*10  ymdh(150), dtgcur, output_ext, adeck_ext
      character*200 home_path, stm_path, out_path
      character*150 bdeck_file, adeck_file, adeck_temp, edeck_temp
CC start UPDATE 1 
      PARAMETER ( NMODMAX=1000 )
      CHARACTER*150 AIDSFILE2, LINE
      INTEGER IAIDS2_FILE, NVOFF
      CHARACTER*4     TECH_VOFF(NMODMAX)
      INTEGER         IOFF1(NMODMAX),IOFF2(NMODMAX)
      COMMON /VAR_OFFSET/NVOFF,TECH_VOFF,
     *                   IOFF1,IOFF2

      namelist /tier1/ 
     & fst_tech, tech_06, tech_12, strmid,
     & output_ext, stm_path, adeck_ext

CC end   UPDATE 1 
c
c      data fst_tech /'UWNB'/
c
c      data tech_12 /'UNB2'/
c
c      data tech_06 /'UNBI'/
c
c**   Get the model-ids from a namelist input file... no command-line args or envars left!
c** Fortran unit=7 (fort.7)
c
      fst_tech = ' '
      tech_12 = ' '
      tech_06 = ' '
      stm_path= '/d1'
      strmid= 'aal00'
      output_ext= 'test'
      adeck_ext= ' '
      read (7, nml=tier1)
      out_path = stm_path
c
c**   Create the bdeck, adeck, edeck output and adeck output file names and 
c**     open the files
c
c
cc      home_path = "/home/atcfdev/pc_archive/storms/intrfcst_2005"  ! c-loop
cc      home_path = "/net/tpm/hur/trak/ver/atx/2005"  ! c-loop
cc      home_path = "/net/tpm/hur/trak/ver/atx/interp/2009/decks"  ! c-loop
cc      home_path = "/net/tpm/hur/trak/ver/alpert/multiyeari"        ! c-loop
cc      home_path = "/d1/src/HFIP/nhc_intrfcst/knew"
cc      stm_path = trim( home_path )//"/orig_2004"                   ! c-loop
cc      stm_path = trim( home_path )//"/orig_2005"                   ! c-loop
cc      stm_path = trim( home_path )                                 ! c-loop
cc      out_path = trim ( home_path )//"/"//output_ext               ! c-loop
cc      out_path = trim ( home_path )                                ! c-loop
c
c
      bdeck_file = trim( stm_path )//"/b"//strmid//".dat"
      adeck_file = trim(stm_path)//"/a"//strmid//trim(adeck_ext)//".dat"
      adeck_temp = trim( out_path )//"/a"//strmid//"."//output_ext
      edeck_temp = trim( out_path )//"/e"//strmid//"."//output_ext

CC start UPDATE 2 
c     Add read:  a list of aids that will use variable (in time) adjustment for winds
      AIDSFILE2 = 'interp_offset.dat'
      IAIDS2_FILE = 28
CC end   UPDATE 2

c
      print *, ' Best track file    = ', bdeck_file
      print *, ' Aids file          = ', adeck_file
      print *, ' Aids output file   = ', adeck_temp
      print *, ' Error output file  = ', edeck_temp
      print *, ' Aids-adjust  file  = ', AIDSFILE2
CC start UPDATE 3 
C
C
C     Get list of aids that will use variable adjustment
C     for the wind interpolation
C     ---------------------------------------------------------
      OPEN(IAIDS2_FILE,FILE=AIDSFILE2,STATUS='OLD',ERR=9000)
90    READ(IAIDS2_FILE,'(A)',ERR=9000) LINE
!MJM      print *, LINE
      IF (LINE(1:1).EQ.'#') GOTO 90
      IF (LINE(1:4) .EQ. 'STOP') GOTO 91
      NVOFF = NVOFF+1
      READ(LINE,*)
     *   TECH_VOFF(NVOFF),IOFF1(NVOFF),IOFF2(NVOFF)
      GO TO 90
C
91    CLOSE(IAIDS2_FILE)
C
CC end   UPDATE 3 

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
c
C**   Read the best track file and store the date/time group in an array
C
      isave = 1
 10   call doReadBT ( 21, ymdh(isave),blat,ns,blon,ew,iwind,bttype,ios )
      if ( ios .lt. 0 ) goto 20
      isave = isave + 1
      go to 10
c
   20 close ( 21 )
      last = isave - 1
c
      do 200 loop = 1, last                       !c-loop
c
         rewind ( 22 )
c
c**   Find the current, past 6-hour and past 12-hour forecast times
c
cc         dtgcur = '2004090712'
cc         dtgcur = '2004091518'
cc         dtgcur = '2004091318'
cc         dtgcur = ymdh(last)                 !c-ops
         dtgcur = ymdh(loop)                      !c-loop
c
c**   Read in the forecast data
c
         call reader ( dtgcur, istat )
         if ( istat .eq. 1 ) goto 200 
c
c**   Create the interpolated forecasts
c
         call intrfcst ( strmid, dtgcur )
c
c**   Compute and write out the GUNS forecast
c
!MJM:testing         call guns ( strmid, dtgcur, 'GUNS' )
c
c**   Compute and write out the GUNA forecast
c
!MJM:testing         call guna ( strmid, dtgcur, 'GUNA' )
c
c**   Compute and write out the CONU forecast
c
!MJM:testing         call conu ( strmid, dtgcur, 'CONU' )
c
  200 continue
c
      close ( 22 )
      close ( 31 )
      close ( 32 )
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
c
9000  WRITE(LUT,'("*** FATAL ERROR: FILE I/O ERROR ***")')
      STOP
      end
c***************************************************************************
      subroutine reader ( dtgcur, istat )
c
c**   Reads in the initial and forecast data
c 
      include 'dataformats.inc'
c
      parameter ( nmodel=11, ntau=23 )
c
      common /techs/ fst_tech(nmodel), tech_12(nmodel), tech_06(nmodel)
c
      common /forecast/ fst06lat(ntau,nmodel), fst06lon(ntau,nmodel), 
     &                  fst06spd(ntau,nmodel), fst06tau(ntau,nmodel),
     &                  fst06rad(ntau,4,3,nmodel),
     &                  fst12lat(ntau,nmodel), fst12lon(ntau,nmodel), 
     &                  fst12spd(ntau,nmodel), fst12tau(ntau,nmodel),
     &                  fst12rad(ntau,4,3,nmodel),
     &                  last_tau06(nmodel), last_tau12(nmodel)
      common /initial/ latcur, loncur, spdcur, radcur
c
      real fst06lat, fst06lon, fst06spd, fst06tau
      real fst12lat, fst12lon, fst12spd, fst12tau
      integer tau(ntau), last_tau06, last_tau12
      real latcur, loncur, spdcur, radcur( 4, 3 )
      real latm06, lonm06, spdm06, radm06( 4, 3 )
      real latm12, lonm12, spdm12, radm12( 4, 3 )
      integer result
c
      character*4   fst_tech, tech_12, tech_06
      character*10  dtgcur, dtgm06, dtgm12
c
      type ( BIG_AID_DATA ) aidsData
      type ( AID_DATA )     aidData, tauData
c
      data tau /   0,   6,  12,  18,  24,  30,  36,  42,  48,
     &                 54,  60,  66,  72,  78,  84,  90,  96,
     &                102, 108, 114, 120, 126, 132 /
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
         last_tau06(i) = 0
         last_tau12(i) = 0
c
         do j = 1, ntau
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
     &	     alon360( tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
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
                        fst06rad( last_tau12(i), n, m, i ) = radm06(n,m)
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
     &	     alat180( tauData%aRecord(1)%lat, tauData%aRecord(1)%NS)
            fst06lon( last_tau06(i), i ) = 
     &	     alon360( tauData%aRecord(1)%lon, tauData%aRecord(1)%EW)
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
c
c**   Read the 34, 50 and 64-knot wind radii
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
!!      nowrite = 0  
      nowrite = 1
      if ( nowrite .eq. 1 ) goto 90
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
         write (*,'(a4, 23f6.1)') fst_tech(i), 
     &           (fst12lat(j,i), j = 1, last_tau12(i) )
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst12lon(j,i), j = 1, last_tau12(i) )
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst12spd(j,i), j = 1, last_tau12(i) )
         do j = 1, last_tau12(i)
            write (*,'(4x, i5, 12f6.1)') j,
     &           ((fst12rad(j,n,m,i), n = 1, 4), m = 1, 3)
         enddo
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst12tau(j,i), j = 1, last_tau12(i) )
         write (*,'('' '')')
         write (*,'(''M06   '', a10)') dtgm06
         write (*,'(a4, 3x, a10)') fst_tech(i), dtgm06
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst06lat(j,i), j = 1, last_tau06(i) )
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst06lon(j,i), j = 1, last_tau06(i) )
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst06spd(j,i), j = 1, last_tau06(i) )
         do j = 1, last_tau06(i)
            write (*,'(4x, i5, 12f6.1)') j, 
     &           ((fst06rad(j,n,m,i), n = 1, 4), m = 1, 3)
         enddo
         write (*,'(a4, 23f6.1)') fst_tech(i),
     &           (fst06tau(j,i), j = 1, last_tau06(i) )
         write (*,'('' '')')
      enddo
c
 90   continue
c
      return
      end
c***************************************************************************
      subroutine intrfcst ( strmid, dtgcur )
c
c**   Interpolate the non-blank forecasts
c
      parameter ( nmodel=11, ntau=23, nits = ntau*2 - 1, ltau=22 )
c
c
      common /techs/  fst_tech(nmodel), tech_12(nmodel), tech_06(nmodel)
      common /forecast/ fst06lat(ntau,nmodel), fst06lon(ntau,nmodel), 
     &                  fst06spd(ntau,nmodel), fst06tau(ntau,nmodel),
     &                  fst06rad(ntau,4,3,nmodel),
     &                  fst12lat(ntau,nmodel), fst12lon(ntau,nmodel), 
     &                  fst12spd(ntau,nmodel), fst12tau(ntau,nmodel),
     &                  fst12rad(ntau,4,3,nmodel),
     &                  last_tau06(nmodel), last_tau12(nmodel)
      common /initial/  latcur, loncur, spdcur, radcur
      common /intrp/    intlat(ltau,nmodel), intlon(ltau,nmodel),
     &                  intspd(ltau,nmodel), intrad(ltau,4,3,nmodel)
c
      real intlat, intlon, intspd, intrad
      real fst06lat, fst06lon, fst06spd, fst06tau
      real fst12lat, fst12lon, fst12spd, fst12tau
      real fstne34(ntau,nmodel),fstse34(ntau,nmodel),
     &     fstsw34(ntau,nmodel),fstnw34(ntau,nmodel)
      real fstne50(ntau,nmodel),fstse50(ntau,nmodel),
     &     fstsw50(ntau,nmodel),fstnw50(ntau,nmodel)
      real fstne64(ntau,nmodel),fstse64(ntau,nmodel),
     &     fstsw64(ntau,nmodel),fstnw64(ntau,nmodel)
c
      integer last_tau06, last_tau12
      real latcur, loncur, spdcur, radcur( 4, 3 )
      real offset_lat, offset_lon, offset_spd, offset_rad( 4, 3 )
      real intrlat(nits), intrlon(nits), intrspd(nits),intrrad(nits,4,3)
c
CC start UPDATE 7
      real intrne34(nits),intrse34(nits),
     &     intrsw34(nits),intrnw34(nits)
      real intrne50(nits),intrse50(nits),
     &     intrsw50(nits),intrnw50(nits)
      real intrne64(nits),intrse64(nits),
     &     intrsw64(nits),intrnw64(nits)
c      real intrne34(nits,nmodel),intrse34(nits,nmodel),
c     &     intrsw34(nits,nmodel),intrnw34(nits,nmodel)
c      real intrne50(nits,nmodel),intrse50(nits,nmodel),
c     &     intrsw50(nits,nmodel),intrnw50(nits,nmodel)
c      real intrne64(nits,nmodel),intrse64(nits,nmodel),
c     &     intrsw64(nits,nmodel),intrnw64(nits,nmodel)
CC end   UPDATE 7
c
      real atau(ltau)
c
      character*4   fst_tech, tech_12, tech_06
      character*8   strmid
      character*10  dtgcur
      character*1   prevent_6h_interp
      character*1   force_12h_interp
c
      data atau /  0.0, 6.0, 12.0, 18.0, 24.0, 30.0, 36.0,
     &            42.0, 48.0, 54.0, 60.0, 66.0, 72.0, 78.0,
     &            84.0, 90.0, 96.0, 102.0, 108.0, 114.0, 120.0, 126.0 /
c
C**   DEFINE INTERNAL FUNCTION for extrapolation
C
      FI(T,A,B,C) = (2.0*A + T*(4.0*B - C - 3.0*A +
     & T*(C - 2.0*B + A)))/2.0
c
c**   Define interpolation flags
c
      iflag  =  1   !  Determine interpolation coefficients everytime
      lflag  =  0   !  0 = linear interpolation,  1 = spline interp
      ntimes = 10   !  Number of 3-point center weighted filter passes

      prevent_6h_interp = 'n'
      force_12h_interp = 'n'
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

c     -----------------------------------
c**   Do the 12-hour interpolation, first
c     -----------------------------------

      do i = 1, nmodel

        if ( last_tau12(i) .gt. 3 ) then

          last_tau = int( fst12tau( last_tau12(i), i ) )
  
c         Make input minus 12 forecast wind radii arrays 
c         easier to handle
 
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

          j = 0
     
          do k = 0, last_tau, 3

            j = j + 1

            call mspline ( fst12tau( 1, i ), fst12lat( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrlat(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lat12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fst12lon( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrlon(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lon12..',
     &         dtgcur, fst_tech(i), k
    
            call mspline ( fst12tau( 1, i ), fst12spd( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrspd(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..spd12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstne34( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrne34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne34 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstse34( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrse34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se34 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstsw34( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrsw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw34 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstnw34( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrnw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw34 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstne50( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrne50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne50 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstse50( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrse50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se50 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstsw50( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrsw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw50 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstnw50( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrnw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw50 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstne64( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrne64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne64 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstse64( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrse64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se64 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstsw64( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrsw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw64 rad12..',
     &         dtgcur, fst_tech(i), k

            call mspline ( fst12tau( 1, i ), fstnw64( 1, i ), 
     &         last_tau12(i), iflag, lflag, float(k), 
     &         intrnw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw64 rad12..',
     &         dtgcur, fst_tech(i), k

          enddo

          jlast = j

c         Filter interpolated points

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

c         Put the radii back in a more convenient form

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

c         Offset the filtered interpolated forecast for the current
c         initial values

          k = 0

          !do j = 5, jlast, 4
          do j = 5, jlast, 2 !MJM: Enable 6-hrly output instead of 12-hrly
            if ( j .eq. 5 ) then
              offset_lat = latcur - intrlat(j)
              offset_lon = loncur - intrlon(j)
              offset_spd = spdcur - intrspd(j)

              do m = 1, 3
                 do n = 1, 4
                    offset_rad( n, m ) = radcur(n,m) - intrrad(j,n,m)
                 enddo
              enddo

            endif

            k = k + 1
CC start UPDATE 4
            CALL SPD_INTP_FCTR(TECH_12(I),ATAU(K),SPDFAC)
c   
            intlat ( k, i ) = offset_lat + intrlat(j)
            intlon ( k, i ) = offset_lon + intrlon(j)
            intspd ( k, i ) = offset_spd*spdfac + intrspd(j)

c            intlat ( k, i ) = offset_lat + intrlat(j)
c            intlon ( k, i ) = offset_lon + intrlon(j)
c            intspd ( k, i ) = offset_spd + intrspd(j)
CC end   UPDATE 4

            if ( intspd ( k, i ) .lt. 0.0 ) intspd ( k, i ) = 0.0

            do m = 1, 3
              do n = 1, 4
                if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) = 
     &                offset_rad(n,m) + intrrad(j,n,m)

                if ( intspd ( k, i ) .lt. 34.0 .and. m .eq. 1) 
     &               intrad ( k, n, m, i ) = 0.0
                if ( intspd ( k, i ) .lt. 50.0 .and. m .eq. 2) 
     &               intrad ( k, n, m, i ) = 0.0
                if ( intspd ( k, i ) .lt. 64.0 .and. m .eq. 3) 
     &               intrad ( k, n, m, i ) = 0.0

                if ( m.eq.2 .and. intrad(k,n,m,i).gt.intrad(k,n,1,i))
     &               intrad( k,n,m,i ) = 0.6*intrad( k,n,1,i )
                if ( m.eq.3 .and. intrad(k,n,m,i).gt.intrad(k,n,2,i))
     &               intrad( k,n,m,i ) = 0.6*intrad( k,n,2,i )

                if ( intrad(k,n,m,i) .lt. 0.0 ) intrad(k,n,m,i) = 0.0

              enddo
            enddo

          enddo

          klast = k

c         If the initial 12-hour forecast is zero (i.e. no intensity
c         forecasts for this model), zero them out since the 
c         interpolated intensities were generated only from the offset

          if ( fst12spd( 1, i ) .lt. 0.01 ) then 
            do m = 1, klast
              intspd( m, i ) = 0.0
            enddo
          endif

c         Extrapolate the offset interpolated forecast 12 hours

          if ( klast .gt. 2 .and. klast .lt. ltau ) then

            intlat( klast + 1, i) = fi( 3.0, intlat( klast - 2, i ),
     &          intlat( klast - 1, i ), intlat( klast, i ) )
            intlon( klast + 1, i) = fi( 3.0, intlon( klast - 2, i ),
     &          intlon( klast - 1, i ), intlon( klast, i ) )
            intspd( klast + 1, i) = fi( 3.0, intspd( klast - 2, i ),
     &          intspd( klast - 1, i ), intspd( klast, i ) )

            if ( intspd( klast+1,i ) .lt. 0.0 ) then
              intspd(klast+1,i) = 0.0
            endif
 
            if ( i .gt. 1 ) then

              do m = 1, 3
                do n = 1, 4

                  intrad ( klast + 1, n, m, i ) = fi( 3.0,
     &                   intrad( klast - 2, n, m, i ),
     &                   intrad( klast - 1, n, m, i ),
     &                   intrad( klast,     n, m, i ))

                  if ( intspd ( klast + 1, i ).lt.34.0 .and. m.eq.1) 
     &                 intrad ( klast + 1, n, m, i ) = 0.0
                  if ( intspd ( klast + 1, i ).lt.50.0 .and. m.eq.2) 
     &                 intrad ( klast + 1, n, m, i ) = 0.0
                  if ( intspd ( klast + 1, i ).lt.64.0 .and. m.eq.3) 
     &                 intrad ( klast + 1, n, m, i ) = 0.0

                  if ( m.eq.2 .and. intrad( klast + 1, n, m, i ).gt. 
     &                 intrad( klast + 1, n, 1, i )) 
     &                 intrad( klast + 1, n, 2, i ) = 
     &                 0.6*intrad( klast + 1, n, 1, i )
                  if ( m.eq.3 .and. intrad( klast + 1, n, m, i ).gt.
     &                 intrad( klast + 1, n, 2, i ))
     &                 intrad( klast + 1, n, 3, i ) = 
     &                 0.6*intrad( klast + 1, n, 2, i )

                  if ( intrad( klast + 1, n, m, i ) .lt. 0.0 )
     &                 intrad( klast + 1, n, m, i ) = 0.0

                enddo
              enddo
            endif
          endif

c         For diagnostics, write out the 12-hour interpolated forecast

!!         nowrite = 0  
          nowrite = 1
          if ( nowrite .eq. 1 ) goto 10

          write (*,'('' 12-hour interpolated forecast '')')
          write (*,'('' '')')
          write (*,'(a4, 3x, a10, 3f6.1)') tech_12(i), dtgcur, 
     &                            offset_lat, offset_lon, offset_spd
          write (*,'('' '')')
          write (*,'(a4, 11f6.1)') tech_12(i), (intlat(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrlat(j), j = 1, 45 )
          write (*,'(a4, 11f6.1)') tech_12(i), (intlon(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrlon(j), j = 1, 45 )
          write (*,'(a4, 11f6.1)') tech_12(i), (intspd(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrspd(j), j = 1, 45 )
          write (*,'(a4, 11f6.1)') fst_tech(i), (atau(j),    j = 1,ltau)
          write (*,'('' '')')
          write (*,'(a4, 12f6.1)') 
     &      tech_12(i), ((offset_rad( n, m ), n = 1, 4), m = 1, 3 )
          do j = 1, ltau
            write (*,'(f6.1, 4x, a4, 12f6.1)') atau(j), 
     &      tech_12(i), ((intrad( j, n, m, i ), n = 1, 4), m = 1, 3 )
          enddo

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw34(j), j = 1, 45 )

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw50(j), j = 1, 45 )

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw64(j), j = 1, 45 )

 10       continue

        endif

      enddo

c     -------------------------------------
c     Do the 6-hour interpolation, second
c     -------------------------------------

      do i = 1, nmodel

c        print *,' '
c        print *,'Top of 6-h nmodel loop, i= ',i
c        print *,'prevent_6h_interp= ',prevent_6h_interp

        if (last_tau06(i) .gt. 3 .and. prevent_6h_interp /= 'y') then
 
c          print *,'!!! IN 6-H LOOP !!!, i= ',i

c         There maybe a 12-hour interpolation for this technique, 
c         so zero it

          do kk = 1, ltau
            intlat( kk, i ) = 0.0
            intlon( kk, i ) = 0.0
            intspd( kk, i ) = 0.0

            do m = 1, 3
              do n = 1, 4
                intrad( kk, n, m, i ) = 0.0
              enddo
            enddo

          enddo   


c         Make input minus 06 forecast wind radii arrays easier 
c         to handle

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
  
          last_tau = int( fst06tau( last_tau06(i), i ) ) 
          !print*,'MJM: last_tau = ',last_tau
          j = 0

          do k = 0, last_tau, 3

            j = j + 1

            call mspline ( fst06tau( 1, i ), fst06lat( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrlat(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lat06..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fst06lon( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &                 intrlon(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..lon06..',
     &           dtgcur, fst_tech(i), k

            !print*,'  '
            !print*,fst_tech(i)
            !print*,dtgcur
            !print*,'MJM: i = ',i
            !print*,'MJM: j = ',j
            !print*,'MJM: k = ',k
            !print*,'MJM: fst06spd( 1, i ) = ',fst06spd( 1, i )
            !print*,'MJM: fst06lat( 1, i ) = ',fst06lat( 1, i )
            !print*,'MJM: fst06lon( 1, i ) = ',fst06lon( 1, i )
            !print*,'  '

            call mspline ( fst06tau( 1, i ), fst06spd( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrspd(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..spd06..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstne34( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrne34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne34 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstse34( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrse34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se34 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstsw34( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrsw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw34 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstnw34( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrnw34(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw34 rad12..',
     &           dtgcur, fst_tech(i), k
  
            call mspline ( fst06tau( 1, i ), fstne50( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrne50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne50 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstse50( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrse50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se50 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstsw50( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrsw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw50 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstnw50( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrnw50(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw50 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstne64( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrne64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..ne64 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstse64( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrse64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..se64 rad12..',
     &           dtgcur, fst_tech(i), k
  
            call mspline ( fst06tau( 1, i ), fstsw64( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrsw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..sw64 rad12..',
     &           dtgcur, fst_tech(i), k

            call mspline ( fst06tau( 1, i ), fstnw64( 1, i ), 
     &           last_tau06(i), iflag, lflag, float(k), 
     &           intrnw64(j), ierror )
            if ( ierror .ne. 0 ) print *, 'ERROR..nw64 rad12..',
     &           dtgcur, fst_tech(i), k

          enddo

          jlast = j

c         Filter interpolated points

          call filter ( ntimes, jlast, intrlat )
          call filter ( ntimes, jlast, intrlon )
          call filter ( ntimes, jlast, intrspd )
!          print*,'  '
!          print*,fst_tech(i)
!          print*,dtgcur
!!          print*,'MJM: intrspd = ',intrspd
!          print*,'MJM: intrlat = ',intrlat
!          print*,'MJM: intrlon = ',intrlon
!          print*,'  '
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

c         Put the radii back in a more convenient form

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

c         Offset the filtered interpolated forecast for the
c         current initial values

          k = 0

          !do j = 3, jlast, 4
          do j = 3, jlast, 2 !MJM: Enable 6-hrly output instead of 12-hrly

            !print*,'MJM: intrspd = ',intrspd

            if ( j .eq. 3 ) then

              offset_lat = latcur - intrlat(j)
              offset_lon = loncur - intrlon(j)
              offset_spd = spdcur - intrspd(j)

              do m = 1, 3
                do n = 1, 4
                  offset_rad( n, m ) = radcur(n,m) - intrrad(j,n,m)
                enddo
              enddo

            endif

            k = k + 1
  
CC start UPDATE 5
            CALL SPD_INTP_FCTR(TECH_06(I),ATAU(K),SPDFAC)

            !print*,'  '
            !print*,TECH_06(I)
            !print*,'MJM: intrspd = ',intrspd
            !print*,'  '
C
            intlat ( k, i ) = offset_lat + intrlat(j)
            intlon ( k, i ) = offset_lon + intrlon(j)
            intspd ( k, i ) = offset_spd*spdfac + intrspd(j)
c
c            intlat ( k, i ) = offset_lat + intrlat(j)
c            intlon ( k, i ) = offset_lon + intrlon(j)
c            intspd ( k, i ) = offset_spd + intrspd(j)
CC end   UPDATE 5

            if ( intspd ( k, i ) .lt. 0.0 ) intspd ( k, i ) = 0.0

            do m = 1, 3
              do n = 1, 4

                if (intrrad( j,n,m ) .gt. 0.0 ) intrad( k,n,m,i ) = 
     &              offset_rad(n,m) + intrrad(j,n,m)

                if ( intspd ( k, i ) .lt. 34.0 .and. m .eq. 1) 
     &               intrad ( k, n, m, i ) = 0.0
                if ( intspd ( k, i ) .lt. 50.0 .and. m .eq. 2) 
     &               intrad ( k, n, m, i ) = 0.0
                if ( intspd ( k, i ) .lt. 64.0 .and. m .eq. 3) 
     &               intrad ( k, n, m, i ) = 0.0

                if ( m.eq.2 .and. intrad(k,n,m,i).gt.intrad(k,n,1,i))
     &               intrad( k,n,m,i ) = 0.6*intrad( k,n,1,i )
                if ( m.eq.3 .and. intrad(k,n,m,i).gt.intrad(k,n,2,i))
     &               intrad( k,n,m,i ) = 0.6*intrad( k,n,2,i )
  
                if ( intrad(k,n,m,i) .lt. 0.0 ) intrad(k,n,m,i) = 0.0

              enddo
            enddo
  
          enddo

          klast = k

c         If the initial 6-hour forecast is zero (i.e. no intensity
c         forecasts for this model), zero them out since the 
c         interpolated intensities were generated only from the offset.

          if ( fst06spd( 1, i ) .lt. 0.01 ) then 
            do m = 1, klast
              intspd( m, i ) = 0.0
            enddo
          endif

c         Extrapolate the offset interpolated forecast 6 hours

          if ( klast .gt. 2 .and. klast .lt. ltau ) then

            intlat( klast + 1, i) = fi( 3.0, intlat( klast - 2, i ),
     &           intlat( klast - 1, i ), intlat( klast, i ) )
            intlon( klast + 1, i) = fi( 3.0, intlon( klast - 2, i ),
     &           intlon( klast - 1, i ), intlon( klast, i ) )
            intspd( klast + 1, i) = fi( 3.0, intspd( klast - 2, i ),
     &           intspd( klast - 1, i ), intspd( klast, i ) )

            if ( intspd( klast + 1, i ) .lt. 0.0 ) 
     &           intspd( klast + 1, i ) = 0.0

            if ( i .gt. 1 ) then
  
              do m = 1, 3
                do n = 1, 4
  
                  intrad( klast + 1, n, m, i ) = fi( 3.0, 
     &                    intrad( klast - 2, n, m, i ),
     &                    intrad( klast - 1, n, m, i ), 
     &                    intrad( klast,     n, m, i ))

                  if ( intspd( klast + 1, i ).lt.34.0 .and. m.eq.1) 
     &                 intrad( klast + 1, n, m, i ) = 0.0
                  if ( intspd( klast + 1, i ).lt.50.0 .and. m.eq.2) 
     &                 intrad( klast + 1, n, m, i ) = 0.0
                  if ( intspd( klast + 1, i ).lt.64.0 .and. m.eq.3) 
     &                 intrad( klast + 1, n, m, i ) = 0.0

                  if ( m.eq.2 .and. intrad( klast + 1, n, m, i ) .gt.
     &                 intrad( klast + 1, n, 1, i ))
     &                 intrad( klast + 1, n, 2, i ) = 
     &                 0.6*intrad( klast + 1, n, 1, i )
                  if ( m.eq.3 .and. intrad (klast + 1, n, m, i ) .gt.
     &                 intrad( klast + 1, n, 2, i ))
     &                 intrad( klast + 1, n, 3, i ) = 
     &                 0.6*intrad( klast + 1, n, 2, i )
  
                  if ( intrad( klast + 1, n, m, i ).lt. 0.0 ) 
     &                 intrad( klast + 1, n, m, i ) = 0.0

                enddo
              enddo

            endif

          endif

c         For diagnostics, write out the 6-hour interpolated forecast

!!         nowrite = 0  
          nowrite = 1

          if ( nowrite .eq. 1 ) goto 20

          write (*,'('' 6-hour interpolated forecast '')')
          write (*,'('' '')')
          write (*,'(a4, 3x, a10, 3f6.1)') tech_06(i), dtgcur, 
     &                             offset_lat, offset_lon, offset_spd
          write (*,'('' '')')
          write (*,'(a4, 23f6.1)') tech_06(i), (intlat(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrlat(j), j = 1, 45 )
          write (*,'(a4, 23f6.1)') tech_06(i), (intlon(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrlon(j), j = 1, 45 )
          write (*,'(a4, 23f6.1)') tech_06(i), (intspd(j,i), j = 1,ltau)
          write (*,'(45f6.1)') (intrspd(j), j = 1, 45 )
          write (*,'(a4, 23f6.1)') fst_tech(i), (atau(j),   j = 1,ltau)
          write (*,'('' '')')
          write (*,'(a4, 23f6.1)') 
     &        tech_06(i), ((offset_rad( n, m ), n = 1, 4), m = 1, 3 )
          do j = 1, ltau
            write (*,'(f6.1, 4x, a4, 23f6.1)') atau(j), 
     &       tech_06(i), ((intrad( j, n, m, i ), n = 1, 4), m = 1, 3 )
          enddo

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw34(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw34(j), j = 1, 45 )

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw50(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw50(j), j = 1, 45 )

          write (*,'('' '')')
          write (*,'(45f6.1)') (intrne64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrse64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrsw64(j), j = 1, 45 )
          write (*,'(45f6.1)') (intrnw64(j), j = 1, 45 )

 20       continue

c         Write the 6-hour interpolated forecasts to the output file

c          write ( *, '(''This is the 6-hour interpolation.'',
c     &                   a10, 4x, a4 )') dtgcur, tech_06(i)
          call new_write_out ( strmid, dtgcur, tech_06(i), intlat(1,i), 
     &         intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )

        else

c         Write the 12-hour interpolated forecasts to the output file

c          write ( *, '(''This is the 12-hour interpolation.'',
c     &                   a10, 4x, a4 )') dtgcur, tech_12(i)
          call new_write_out ( strmid, dtgcur, tech_12(i), intlat(1,i),
     &         intlon(1,i), intspd(1,i), intrad(1,1,1,i ) )

        endif

      enddo
c
      return
      end
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
      parameter ( ntau=23, nits = ntau*2 - 1 )
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
      subroutine write_out ( strmid, dtgcur, new_tech, 
     &     intlat, intlon, intspd )
c
c**   Put the new objective aid in a writeable format
c
      parameter ( ltau=22 )
c
      real     intlat(ltau), intlon(ltau), intspd(ltau)
      integer  newfst(ltau,3)
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
         newfst( j, 1 ) = int ( intlat( j )*10.0 + 0.5 )
         newfst( j, 2 ) = int ( intlon( j )*10.0 + 0.5 )
         newfst( j, 3 ) = int ( intspd( j ) )

      enddo
c
      call newWriteAidRcd ( 31,  strmid, dtgcur, new_tech, newfst )
cc      call newWriteAidRcd (  6,  strmid, dtgcur, new_tech, newfst )
c
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine new_write_out ( strmid, dtgcur, new_tech, 
     &     intlat, intlon, intspd, intrad )
c
c**   Put the new objective aid in a writeable format
c
      parameter ( ltau=22 )
c
      real     intlat(ltau), intlon(ltau), intspd(ltau) 
      real     intrad(ltau,4,3)
      integer  result, newfst(ltau,3), newrad(ltau,4,4)
c
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
         !print *,'MJM: newfst( j, 3 ) = ',newfst( j, 3 )
c
         do m = 1, 3
            do n = 1, 4
c
c**   For wind radii diagnostics,
c
!!               nowrite = 0
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
      call convWriteAidData ( 31,  strmid, dtgcur, new_tech, 
     &     newfst, wquad, newrad, result )
cc      call convWriteAidData ( 6,  strmid, dtgcur, new_tech, 
cc     &     newfst, wquad, newrad, result )
cc      call newWriteAidRcd ( 31,  strmid, dtgcur, new_tech, newfst )
cc      call newWriteAidRcd (  6,  strmid, dtgcur, new_tech, newfst )
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
      parameter ( ltau=22 )
c
      real     intlat(ltau), intlon(ltau), err_rad(ltau)
      integer  newfst(ltau,3)
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
      subroutine guns ( strmid, dtgcur, new_tech )
c
c**   Do the GUNS - GFDI, NGPI, and UKMI, if possible
c
      parameter ( nmodel=11, ltau=22 )
c
      common /intrp/ intlat(ltau,nmodel), intlon(ltau,nmodel),
     &               intspd(ltau,nmodel), intrad(ltau,4,3,nmodel)
c
      real intlat, intlon, intspd, intrad
      real newlat(ltau), newlon(ltau), newspd(ltau)
c
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
c      
      do j = 1, ltau
c
         newlat(j) = 0.0
         newlon(j) = 0.0
         newspd(j) = 0.0
c
         if ( intlat(j,3) .gt. 0.01 .and. intlon(j,3) .gt. 0.01 .and.
     &        intlat(j,4) .gt. 0.01 .and. intlon(j,4) .gt. 0.01 .and.
     &        intlat(j,5) .gt. 0.01 .and. intlon(j,5) .gt. 0.01 ) 
     &        then
c
            newlat(j) = (intlat(j,3) + intlat(j,4) + intlat(j,5))/3.0
            newlon(j) = (intlon(j,3) + intlon(j,4) + intlon(j,5))/3.0
c
         endif
c
      enddo
c
c**   Write out the new forecast
c
      call write_out ( strmid, dtgcur, new_tech, 
     &                 newlat, newlon, newspd )
c 
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine guna ( strmid, dtgcur, new_tech )
c
c**   Do the GUNA - AVNI, GFDI, NGPI, and UKMI, if possible
c
      parameter ( nmodel=11, ltau=22 )
c
      common /intrp/ intlat(ltau,nmodel), intlon(ltau,nmodel),
     &               intspd(ltau,nmodel), intrad(ltau,4,3,nmodel)
c
      real intlat, intlon, intspd, intrad
      real newlat(ltau), newlon(ltau), newspd(ltau)
      real spread(ltau), err_rad(ltau)
      integer numod(ltau)
c
c**   Define the indices of the 4 GUNA models
c
      integer mod_indx(4)
      data mod_indx/ 2, 3, 4, 5 /
c
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
c      
      pi = 3.14159
c
      do j = 1, ltau
c
         newlat(j) = 0.0
         newlon(j) = 0.0
         newspd(j) = 0.0
         spread(j) = 0.0
          numod(j) = 0
c
         if ( intlat(j,2) .gt. 0.01 .and. intlon(j,2) .gt. 0.01 .and.
     &        intlat(j,3) .gt. 0.01 .and. intlon(j,3) .gt. 0.01 .and.
     &        intlat(j,4) .gt. 0.01 .and. intlon(j,4) .gt. 0.01 .and.
     &        intlat(j,5) .gt. 0.01 .and. intlon(j,5) .gt. 0.01 ) 
     &        then
c
            newlat(j) = (intlat(j,2) + intlat(j,3) + 
     &                   intlat(j,4) + intlat(j,5))/4.0
            newlon(j) = (intlon(j,2) + intlon(j,3) + 
     &                   intlon(j,4) + intlon(j,5))/4.0
            newspd(j) = 0.0
c
c**   Calculate spread of GUNA member forecasts
c
            sumdist = 0.0
c
 	    do m = 1, 4
c
	       avglat  =  0.5*( intlat( j, mod_indx(m) ) + newlat( j ))
               coslat  = cos( avglat*pi/180.0 )
               dlat    = 60.0*( intlat( j, mod_indx(m) ) - newlat( j ))
               dlon    = 60.0*coslat*
     &                        ( intlon( j, mod_indx(m) ) - newlon( j ))
               dist    = sqrt(dlat**2 + dlon**2)
               sumdist = sumdist + dist
            enddo
c
            spread(j) = sumdist/4.0
             numod(j) = 4
c
         endif
c
      enddo
c
      call do_err_rad( new_tech, strmid, newlat, newlon, spread, numod,
     &                 err_rad )
c
c**   Write out the new forecast
c
        call write_out ( strmid, dtgcur, new_tech,
     &                   newlat, newlon, newspd )
      call err_rad_out ( strmid, dtgcur, new_tech,
     &                   newlat, newlon, err_rad )
c 
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine conu ( strmid, dtgcur, new_tech )
c
c**   Do the CONU - Two or more of AVNI, GFDI, GFNI, NGPI, and UKMI,
c**     if possible
c
      parameter ( nmodel=11, ltau=22 )
c
      common /intrp/ intlat(ltau,nmodel), intlon(ltau,nmodel),
     &               intspd(ltau,nmodel), intrad(ltau,4,3,nmodel)
c
      real    intlat, intlon, intspd, intrad
      real    newlat(ltau), newlon(ltau), newspd(ltau)
      real    spread(ltau), err_rad(ltau)
      integer numod(ltau)
c
c**   Indices of the 5 CONU models
c
      integer mod_indx(5)
      data mod_indx/ 2, 3, 4, 5, 10 /
c
      character*4  new_tech
      character*8  strmid
      character*10 dtgcur
c      
      pi = 3.14159
c
      do j = 1, ltau
c
         newlat(j) = 0.0
         newlon(j) = 0.0
         newspd(j) = 0.0
         spread(j) = 0.0
          numod(j) = 0
            sumlat = 0.0
            sumlon = 0.0
             nmods = 0
c
        do m = 1,5
          if ( intlat(j,mod_indx(m)) .gt. 0.01 .and.
     &         intlon(j,mod_indx(m)) .gt. 0.01 )
     &         then
c
             nmods = nmods + 1
             sumlat = sumlat + intlat(j,mod_indx(m))
             sumlon = sumlon + intlon(j,mod_indx(m))
c
          endif
        enddo
c
        if (nmods.ge.2) then
          newlat(j) = sumlat / float(nmods)
          newlon(j) = sumlon / float(nmods)
          newspd(j) = 0.0
        endif
c
c**   Calculate spread of CONU member forecasts
c
        sumdist = 0.0
          nmods = 0
c
        do m = 1,5
          if ( intlat(j,mod_indx(m)) .gt. 0.01 .and.
     &         intlon(j,mod_indx(m)) .gt. 0.01 )
     &         then
c
             nmods   = nmods + 1
             avglat  =  0.5*( intlat( j, mod_indx(m) ) + newlat(j) )
             coslat  = cos(avglat*pi/180.0)
             dlat    = 60.0*( intlat( j, mod_indx(m) ) - newlat(j) )
             dlon    = 60.0*coslat*
     &                      ( intlon( j, mod_indx(m) ) - newlon(j) )
             dist    = sqrt(dlat**2 + dlon**2)
             sumdist = sumdist + dist
c
          endif
        enddo
c
        if ( nmods .ge. 2 ) then
          spread(j) = sumdist/float(nmods)
           numod(j) = nmods
        endif
c
      enddo
c
      call do_err_rad ( new_tech, strmid, newlat, newlon, spread, numod,
     &                  err_rad )
c
c**   Write out the new forecast and the spread
c
        call write_out ( strmid, dtgcur, new_tech,
     &                   newlat, newlon, newspd )
      call err_rad_out ( strmid, dtgcur, new_tech,
     &                   newlat, newlon, err_rad )
c 
      return
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      subroutine do_err_rad ( new_tech, strmid, newlat, newlon, spread,
     &                        numod, err_rad )
cc      parameter ( nmodel=1, ltau=11 )
      parameter ( nmodel=11, ltau=22 )
c
      common /initial/ latcur, loncur, spdcur
      common /intrp/ intlat(ltau,nmodel), intlon(ltau,nmodel),
     &               intspd(ltau,nmodel), intrad(ltau,4,3,nmodel)
c
      real intlat, intlon, intspd, intrad
      real newlat(ltau), newlon(ltau)
      real spread(ltau),err_rad(ltau)
      integer numod(ltau)
      real al_conu_spread(ltau),al_conu_init_inten(ltau)
     &,    al_conu_fcst_inten(ltau),al_conu_init_lat(ltau)
     &,    al_conu_const(ltau)
      real al_guna_spread(ltau),al_guna_init_inten(ltau)
     &,    al_guna_lond(ltau),al_guna_const(ltau)
      real ep_conu_spread(ltau),ep_conu_numem(ltau)
     $,    ep_conu_init_inten(ltau),ep_conu_fcst_inten(ltau)
     $,    ep_conu_init_lat(ltau),ep_conu_const(ltau)
      real ep_guna_spread(ltau),ep_guna_init_inten(ltau)
     &,    ep_guna_fcst_inten(ltau),ep_guna_latd(ltau)
     &,    ep_guna_lond(ltau),ep_guna_const(ltau)
      real siglvl_const(ltau)
c
c**   Prediction coefficients for CONU (Atlantic)
c
c**   Derived from 2001-2003 Seasons
c
c      data al_conu_spread/0.,.524,.564,.559,.457,0.,.527,0.,.829,0.,.73/
c      data al_conu_init_inten/0.,-.267,-.447,-.644,6*0.,-1.738/
c      data al_conu_fcst_inten/4*0.,-0.967,0.,-1.725,0.,-1.726,2*0./
c      data al_conu_init_lat/10*0.,13.42/
c      data al_conu_const/0.,41.,66.,94.,144.,0.,221.,0.,198.,0.,-51./
c
c**   Derived from 2001-2004 Seasons
c
      data al_conu_spread/0.,.565,.609,.556,.5,0.,.541,0.,.775,0.,.803/
      data al_conu_init_inten/0.,-.236,-.359,-.486,-.666,6*0./
      data al_conu_fcst_inten/6*0.,-1.269,0.,-1.405,0.,-1.719/
      data al_conu_init_lat/11*0./
      data al_conu_const/0.,38.,59.,84.,115.,0.,187.,0.,189.,0.,237./
c
c**   Prediction coefficients for GUNA (Atlantic)
c
c**   Derived from 2001-2003 Seasons
c
c      data al_guna_spread/0.,.565,.645,.677,.692,0.,.752,0.,.899,0.
c     &                   ,.685/
c      data al_guna_init_inten/0.,-.26,-.403,-.572,-.701,0.,-.876,4*0./
c      data al_guna_lond/10*0.,-7.07/
c      data al_guna_const/0.,40.,59.,81.,100.,0.,120.,0.,36.,0.,157./
c
c**   Derived from 2001-2004 Seasons
c
      data al_guna_spread/0.,.646,.646,.616,.593,0.,.699,0.,.803,0.
     &                   ,.659/
      data al_guna_init_inten/0.,-.224,-.328,-.426,-.55,0.,-.684,4*0./
      data al_guna_lond/10*0.,-4.04/
      data al_guna_const/0.,35.,54.,73.,96.,0.,114.,0.,61.,0.,162./
c
c**   Prediction coefficients for CONU (Eastern North Pacific)
c
c**   Derived from 2001-2003 Seasons
c
c      data ep_conu_spread/0.,.402,.425,.423,.252,0.,.415,0.,.447,0.
c     &                   ,.742/
c      data ep_conu_numem/0.,-3.99,-7.32,-9.66,-12.09,0.,-19.88,0.
c     &                  ,-33.05,0.,-36.03/
c      data ep_conu_init_inten/0.,-.157,9*0./
c      data ep_conu_fcst_inten/8*0.,-.891,0.,-1.452/
c      data ep_conu_init_lat/10*0.,-10.43/
c      data ep_conu_const/0.,52.,74.,99.,137.,0.,179.,0.,288.,0.,463./
c
c**    Derived from 2001-2004 Seasons
c
      data ep_conu_spread/0.,.387,.409,.409,.246,0.,.429,0.,.454,0.
     &                   ,.544/
      data ep_conu_numem/0.,-4.63,-8.8,-11.87,-15.1,0.,-20.95,0.
     &                  ,-26.45,2*0./
      data ep_conu_init_inten/0.,-.147,9*0./
      data ep_conu_fcst_inten/8*0.,-.907,0.,-1.645/
      data ep_conu_init_lat/11*0./
      data ep_conu_const/0.,54.,80.,108.,148.,0.,181.,0.,271.,0.,248./
c
c**   Prediction coefficients for GUNA (Eastern North Pacific)
c
c**   Derived from 2001-2003 Seasons
c
c      data ep_guna_spread/0.,.407,.331,.452,.444,0.,.539,0.,.685,0.
c     &                   ,.823/
c      data ep_guna_init_inten/0.,-.174,-.266,0.,-.296,6*0./
c      data ep_guna_fcst_inten/6*0.,-.472,0.,-1.073,2*0./
c      data ep_guna_latd/6*0.,-7.3,4*0./
c      data ep_guna_lond/3*0.,-2.48,7*0./
c      data ep_guna_const/0.,34.,60.,62.,81.,0.,129.,0.,140.,0.,77./
c
c**    Derived from 2001-2004 Seasons
c
      data ep_guna_spread/0.,.348,.292,.408,.356,0.,.492,0.,.59,0.
     &                   ,.723/
      data ep_guna_init_inten/0.,-.156,-.254,-.29,-.381,0.,-.536,4*0./
      data ep_guna_fcst_inten/8*0.,-1.155,2*0./
      data ep_guna_latd/11*0./
      data ep_guna_lond/8*0.,3.83,0.,9.82/
      data ep_guna_const/0.,33.,58.,69.,90.,0.,109.,0.,108.,0.,-44./
c
c**   Significance level adjustment
c
      data siglvl_const/0.,10.,15.,22.5,30.,0.,45.,0.,60.,0.,75./
c
      real latcur, loncur, spdcur
c
      character*4  new_tech
      character*8  strmid
      character*2  basin
c
      basin = strmid (1:2)
c
      do n = 1, ltau

         if( intspd(n,1) .gt. 0.0)  then
            fcst_inten = intspd(n,1)
         else
            fcst_inten = 65.0
         endif
c
         if ( new_tech .eq. 'CONU' .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               err_rad(n) = al_conu_spread(n)*spread(n)
     &              + al_conu_init_inten(n)*spdcur
     &              + al_conu_fcst_inten(n)*fcst_inten
     &              + al_conu_init_lat(n)*latcur
     &              + al_conu_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) ) 
     &              err_rad(n) = siglvl_const(n)
c 
cc         if(n.eq.3) then
cc           print *,spread(n),spdcur,intspd(n,1),latcur,err_rad(n)
cc         endif
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( new_tech .eq. 'GUNA' .and. basin .eq. 'al' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlond = newlon(n) - loncur
               err_rad(n) = al_guna_spread(n)*spread(n)
     &              + al_guna_init_inten(n)*spdcur
     &              + al_guna_lond(n)*xlond
     &              + al_guna_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) ) 
     &              err_rad(n) = siglvl_const(n)
c 
cc         if ( n .eq. 7 ) then
cc           print *,spread(n),spdcur,intspd(n,1),latcur,err_rad(n)
cc         endif
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( new_tech .eq. 'CONU' .and. basin .eq. 'ep' ) then
            if ( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               err_rad(n)=ep_conu_spread(n)*spread(n)
     &              + ep_conu_numem(n)*float(numod(n))
     &              + ep_conu_init_inten(n)*spdcur
     &              + ep_conu_fcst_inten(n)*fcst_inten
     &              + ep_conu_init_lat(n)*latcur
     &              + ep_conu_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) ) 
     &              err_rad(n) = siglvl_const(n)
c 
cc         if ( n .eq. 7 ) then
cc           print *,spread(n),spdcur,intspd(n,1),latcur,err_rad(n)
cc         endif
c
            else
               err_rad(n) = 0.0
c
            endif
         endif
c
         if ( new_tech .eq. 'GUNA' .and. basin .eq. 'ep' ) then
            if( newlat(n) .gt. 0.01 .and. newlon(n) .gt. 0.01 ) then
c
               xlatd = newlat(n) - latcur
               xlond = newlon(n) - loncur
               err_rad(n) = ep_guna_spread(n)*spread(n)
     &              + ep_guna_init_inten(n)*spdcur
     &              + ep_guna_fcst_inten(n)*fcst_inten
     &              + ep_guna_latd(n)*xlatd
     &              + ep_guna_lond(n)*xlond
     &              + ep_guna_const(n) + siglvl_const(n)
c
               if ( err_rad(n) .lt. siglvl_const(n) ) 
     &              err_rad(n) = siglvl_const(n)
c 
cc         if(n.eq.7) then
cc           print *,spread(n),spdcur,intspd(n,1),latcur,err_rad(n)
cc         endif
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
C     --------------------------------------------------
      SUBROUTINE SPD_INTP_FCTR(TECH,TAU,FACTOR)
C
C     Determines how much of the wind offset will be
C     applied.
C     --------------------------------------------------
C
      PARAMETER ( NMODMAX=1000 )
      COMMON /VAR_OFFSET/NVOFF,TECH_VOFF(NMODMAX),
     *                   IOFF1(NMODMAX),IOFF2(NMODMAX)
C
      CHARACTER*4 TECH, TECH_VOFF
C
      FACTOR = 1.0
      DO 100 L=1,NVOFF
         IF (TECH.EQ.TECH_VOFF(L)) THEN
            WINDOW = FLOAT(IOFF2(L) - IOFF1(L))
            PROGRESS = TAU - FLOAT(IOFF1(L))
            FACTOR = 1.0 - PROGRESS/WINDOW
            IF (FACTOR.GT.1.0) FACTOR = 1.0
            IF (FACTOR.LT.0.0) FACTOR = 0.0
            GOTO 200
            ENDIF
100      CONTINUE
C
200   CONTINUE
      RETURN
      END


