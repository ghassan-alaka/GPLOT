      program site_cor
c
c**   This program reads the realization files and computes site specific TC-CORs.
c
c
      implicit none
      real    blat, blon
      integer iwind
      integer naids, ii
      integer itau, ios, isave, last, l, last_tau
      integer ind_stm, ind_msg, ind_nhc, ind_inc

      integer maxtau, alltau
      parameter ( maxtau = 10, alltau = 120 )
      integer nomonum, nomonumTY, nomonumTS
      integer nomoint(1000),nomointTY(1000),nomointTS(1000)
      integer hit (0:maxtau)
      integer hittot (0:maxtau)
      integer iprob  (0:maxtau)
      integer icprob (0:maxtau)
      integer hits(0:999,0:maxtau)
      real    nomolat(1000), nomolon(1000)
      real    nomolatTY(1000), nomolonTY(1000)
      real    nomolatTS(1000), nomolonTS(1000)
      real    site_lat, site_lon
c
      character*1   ns, ew
      character*2   bttype
      character*4   aidid
      character*4   aidsfile
      character*8   strmid
      character*10  ymdh(150), cur_ymdh, cur_ymdh06
      character*30  station
      character*68  message(18)
      character*100 stm_path, msg_path, nhc_path, inc_path
      character*150 brkpt_file, msg_file
      character*150 bdeck_file, adeck_file
      character*150 nomo_file, nomo_fileTY, nomo_fileTS
      character*150 prob_file, determ_file, oprob_file
c
c**   Get the command line arguments
c
      strmid = ' '
      call getarg ( 1, strmid )
      call getarg ( 2, cur_ymdh )
      call getarg ( 3, station )
      call getarg ( 4, aidid )
      call getarg ( 5, aidsfile )

c
c**   If missing aidid, assign something in realization file
c
      if (aidid(1:4) .eq. '    ') then
              aidid='W000'
              naids=1000
              aidsfile='real'
      else
              naids=1
      endif

c
c**   If missing aidsfile, assign to realization file
c
      if (aidsfile(1:4) .eq. '    ') then
              aidsfile(1:4)='real'
      endif

c
c**   If missing storm id, give examples of use:
c
      if ( strmid .ne. '        ' ) then
         print *, ' Storm ID =', strmid
      else
         print*,'USAGE: site_cor stormid dtg site <aidid> <aidsfile>'
         print*,'For probabilities:'
         print*,'site_cor wp182011 2011092000 Yokota'
         print*,'For single aid in realization file:'
         print*,'EXAMPLE: site_cor wp182011 2011092000 Yokota W001 real'
         print*,'For single aid in aids file:'
         print*,'EXAMPLE: site_cor wp182011 2011092000 Yokota JTWC aids'
         stop
      endif
c
c**   Create the b-deck, a-deck and winds_files
c**     file names and open the files
c
      call getenv (  "ATCFSTRMS", stm_path )
      call getenv ( "ATCFOUTPUT", msg_path )
      call getenv ( "NHCMESSAGES", nhc_path )
      call getenv ( "ATCFINC", inc_path )
c
c
      ind_stm = index( stm_path, " " ) - 1
      ind_msg = index( msg_path, " " ) - 1
      ind_nhc = index( nhc_path, " " ) - 1
      ind_inc = index( inc_path, " " ) - 1
c
c      Filename assignments
c
      bdeck_file=stm_path(1:ind_stm)//"/b"//strmid//".dat"
      if ( aidsfile(1:4) .eq. 'real' ) then
         adeck_file=nhc_path(1:ind_nhc)//"/"//strmid//
     &              ".realizations."//cur_ymdh
      else
         adeck_file=stm_path(1:ind_stm)//"/a"//strmid//".dat"
      endif
      nomo_file=inc_path(1:ind_inc)//"/problty_dat/nomo."//station
      nomo_fileTY=
     &     inc_path(1:ind_inc)//"/problty_dat/nomo.TY."//station
      nomo_fileTS=
     &     inc_path(1:ind_inc)//"/problty_dat/nomo.TS."//station
      prob_file=
     &     nhc_path(1:ind_nhc)//"/"//strmid//"."//cur_ymdh//
     &     ".wndprb.nomo."//station
      call dtgmod (cur_ymdh, -6, cur_ymdh06, ios)
      oprob_file=
     &     nhc_path(1:ind_nhc)//"/"//strmid//"."//cur_ymdh06//
     &     ".wndprb.nomo."//station
      determ_file=
     &     nhc_path(1:ind_nhc)//"/"//strmid//"."//cur_ymdh//
     &     ".JTWC.nomo."//station

c
      print *, ' Best track file              = ', bdeck_file
      print *, ' Aids file                    = ', adeck_file
c
c**   Open input files
c
      open ( 21, file=bdeck_file, status='old', iostat=ios, err=1010 )
      open ( 22, file=adeck_file, status='old', iostat=ios, err=1020 )

      if ( naids .gt. 1 ) then
        print *, ' Previous site-specific prob file = ', oprob_file
        open( 28, file=oprob_file, status='unknown', iostat=ios, err=5 )
      endif
    5 continue
c
c**   Read the nomogram file into arrays of lat, lon, minimum intensity
c
      if ( station .eq. 'Yokota' ) then
        print *, ' Nomogram file                = ', nomo_file
        open( 23, file=nomo_file,  status='old', iostat=ios, err=1030 )
        call readNomo ( nomonum, nomolat, nomolon, nomoint )
      else
        print *, ' Nomogram file for Typhoons   = ', nomo_fileTY
        open( 23, file=nomo_fileTY,  status='old', iostat=ios, err=1030)
        call readNomoTY( site_lat, site_lon,
     &                   nomonumTY, nomolatTY, nomolonTY, nomointTY )
        close ( 23 )
        print *, ' Nomogram file for TS = ', nomo_fileTS
        open( 23, file=nomo_fileTS,  status='old', iostat=ios, err=1030)
        call readNomoTY( site_lat, site_lon,
     &                   nomonumTS, nomolatTS, nomolonTS, nomointTS )
      endif
      close ( 23 )
      print *, ' Probability output file      = ', prob_file
c
C**   Read the entire best track file and store the date/time group
c**     in an array
C
      isave = 1
c      stop
   10 call doReadBT ( 21, ymdh(isave), blat, ns, blon, ew, iwind,
     &                bttype ,ios )
      if ( ios .lt. 0 ) goto 20
      isave = isave + 1
      go to 10
c
   20 close ( 21 )
      last = isave - 1
c
c**   Read the official forecast data for the current date/time
c
       do 200 l = 1, naids
         rewind (22)
c
c**      construct the aid name
c
         if (l .ne. 1) then
             write (aidid, '(a1,i3.3)') "W", l-1
         endif

         call read_adeck ( cur_ymdh, last_tau, aidid )
c
c**   If no forecast for the current date/time then
c
         if ( last_tau .eq. -1 ) then
            print *,'There is no ', aidid,' forecast for : ', cur_ymdh
            go to 200
         endif
c
c**   Found forecast
c
cx       print *, 'success!'
         call fillfst ( cur_ymdh, last_tau )

c
c     Open deterministic nomogram output file
         if (naids .eq. 1) then
           open ( 24,file=determ_file, status='unknown', err=1040 )
           write( 24, *) '                                            '
           write( 24, *) '                                            '
           write( 24, *) '  ------- Winds at ', station(1:15), '-----'
           write( 24, *) '  These are computed by running the official '
           write( 24, *) '  forecast through nomograms for each site. '
           write( 24, *) '  Forecasters may need to adjust winds for'
           write( 24, *) '  factors such as approaching troughs,'
           write( 24, *) '  TC sized, speed and direction of approach.'
           write( 24, *) ,'                                        '
           write( 24, *) ,'       lat:', site_lat,' lon:',site_lon
           write( 24, *) '     Using ',aidid, ' forecast from:',cur_ymdh
           write( 24, *) '  ------------------------------------------'
         endif
c
c     Nomogram search
c
c     Yokota type nomogram
         if ( station .eq. 'Yokota' ) then
           if (naids .eq. 1) then
             call findVatSite
     &         (cur_ymdh,aidid,nomonum,nomolat,nomolon,nomoint,last_tau)
           else
             call findHiti
     &         (aidid,nomonum,nomolat,nomolon,nomoint,last_tau,hit)
           endif
c     Standard NEPRF nomogram
         else
           call findHitTY (cur_ymdh, site_lat, site_lon, naids, aidid,
     &        nomonumTY, nomolatTY, nomolonTY, nomointTY,
     &        nomonumTS, nomolatTS, nomolonTS, nomointTS, last_tau, hit)
         endif
         close (24)
cx    Sum the hits
         do ii=0, maxtau
            hits(l-1,ii)=hit(ii)
            if (l .eq. 1) hittot(ii)=0
            hittot(ii)=hittot(ii) + hit(ii)
         enddo
c
  200 continue

cx    Compute probs
         do ii=0, maxtau
            iprob(ii)=nint (float(hittot(ii))/10.0)
            if (ii .eq. 0) then
               icprob(ii) = iprob(ii)
            else
               icprob(ii)= icprob(ii-1) + iprob(ii)
            endif
         enddo

      close ( 22 )


      if ( naids .gt. 1 ) then
         write( *, *) "                                       "
         write( *, *) "                                       "
         write( *, *) "                                       "
         write( *, *) '*************   ', station(1:15), '***********'
         write( *, *) ,'       lat:', site_lat,' lon:',site_lon
         write( *, *) '        Forecast for dtg:',cur_ymdh
         write (*,*) "                                       "
         write (*,*) " These site specific wind probabilities are"
         write (*,*) " generated by running 1000 realizations through "
         write (*,*) " site nomograms based on position and intensity."
         write (*,*) "                                       "
         write (*,*) "------  Incremental 50-kt Probabilities  -------"
         write (*,*) "***  Incremental Probabilities  ********"
         write (*,*) "T=0  12  24  36  48  60  72  84  96  108 120"
         write (*,'(11i4)') ((iprob(ii)), ii=0,10)
         write (*,*) "------  Cumulative 50-kt Probabilities  -------"
         write (*,*) "T=0  12  24  36  48  60  72  84  96  108 120"
         write (*,'(11i4)') ((icprob(ii)), ii=0,10)
         write (*,*) "***************************************"

         open(25,file=prob_file, status='unknown', iostat=ios, err=1050)
         write( 25, *) "                                       "
         write( 25, *) "                                       "
         write( 25, *) "                                       "
         write( 25, *) "                                       "
         write( 25, *) '***********************************************'
         write( 25, *) '***************   ', station(1:18),'***********'
         write( 25, *) '***********************************************'
         write( 25, *) '    Wind Probabilities and TC-COR Guidance for '
         write( 25, *) '                   ', station(1:15)
         write( 25, *) ,'  lat:', site_lat,'     lon:',site_lon
         write( 25, *) '         Warning DTG:',cur_ymdh
         write (25,*) "                                       "
cx       write (25,*) " These site specific wind probabilities are "
cx       write (25,*) " generated by running 1000 realizations through "
cx       write (25,*) " site nomograms based on position and intensity."
cx       write (25,*) "                                       "
cx       write (25,*) "------  Incremental 50-kt Probabilities  ------"
cx       write (25,*) "T=0  12  24  36  48  60  72  84  96  108 120"
cx       write (25,'(11i4)') ((iprob(ii)), ii=0,10)
         write (25,*) "------  Cumulative 50-kt Probabilities  -------"
         write (25,*) "T=0  12  24  36  48  60  72  84  96  108 120"
         write (25,'(11i4)') ((icprob(ii)), ii=0,10)
cx       write (25,*) "-----------------------------------------------"

      endif
      call get_tccor (icprob)
      stop 'site_cor complete.'
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
 1030 print *, ' Error opening nomogram file - ios =', ios
      print *, ' Filename:', nomo_file
      stop
c
 1040 print *, ' Error opening probability file - ios =', ios
      print *, ' Filename:', determ_file
      stop
 1050 print *, ' Error opening probability file - ios =', ios
      print *, ' Filename:', prob_file
      stop
c
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE readNomo ( nomonum, nomolat, nomolon, nomoint )
C
C**   THIS SUBROUTINE reads the nomogram file
c
      implicit none
      integer nomonum, nomoint(1000)
      real    nomolat(1000), nomolon(1000)
      integer i
      character*100 line

      do 100 i=1, 1010
          read (23, '(a)', end=1010) line
          if (line(1:1) .ne. '#') then
            read (line, *, err=1020)
     &            nomolat(i), nomolon(i), nomoint(i)
            nomonum = i
          endif
  100 continue
 1010 continue
      print *, 'nomogram file read complete, ', nomonum, ' points'
      return
 1020 continue
      print *, 'error reading nomogram file, aborting.'
      stop
      end
c
c********1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE readNomoTY( site_lat, site_lon,
     &                       nomonum, nomolat, nomolon, nomoint )
C
C**   THIS SUBROUTINE reads the standard Navy nomogram file
c
      implicit none
      integer nomonum, nomoint(1000)
      real    nomolat(1000), nomolon(1000)
      real    site_lat, site_lon
      integer i
      character*100 line

c    read until we get to the CIRC line, then real data
      do 50 i=1, 1000
        read (23, '(a)', end=1010) line
        if (line(1:4) .ne. 'CIRC') then
            continue
        else
            read (line(6:10), '(f5.2)', err=1020) site_lat
            read (line(12:17),'(f6.2)', err=1020) site_lon
            go to 60
        endif
   50 continue

   60 continue
      do 100 i=1, 1010
          read (23, '(a)', end=1010) line
          if (line(1:1) .eq. '0' .and. line(5:5) .eq. ',' ) then
            read (line, '(2x,i2,1x,f5.2,1x,f6.2)', err=1020)
     &            nomoint(i), nomolat(i), nomolon(i)
            nomonum = i
          elseif (line(1:1) .eq. '0' .and. line(6:6) .eq. ',' ) then
            read (line, '(2x,i3,1x,f5.2,1x,f6.2)', err=1020)
     &            nomoint(i), nomolat(i), nomolon(i)
            nomonum = i
          endif
  100 continue
 1010 continue
      print *, 'nomogram file read complete, ', nomonum, ' points'
      return
 1020 continue
      print *, 'error reading nomogram file, aborting.'
      stop
      end
c********1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE read_adeck ( cur_ymdh, last_tau, aidid )
C
C**   THIS SUBROUTINE reads the forecast data
C
      INCLUDE  'dataformats.inc'
c
      integer maxtau, alltau
      parameter ( maxtau = 10, alltau = 120 )
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)
c
      common /big_aid_data/ aidsData, aidsDataresult
      character*2  basin, padding
c
      integer result, tau, aidsDataresult
      integer radp, rrp, mrd
c
      character*2  cynum
      character*4  aidid, aididi
      character*8  strmid
      character*10 fst_ymdh, cur_ymdh
      character*16 latlon
c
      type ( BIG_AID_DATA ) aidsData
      type ( AID_DATA ) fstRcd, tauData
      data aidsDataresult /0/
c
c**   Zero the arrays
c
      do i = 0, maxtau
c
         tau( i )    = 0
         flat( i )   = 0.0
         flon( i )   = 0.0
         vmax( i )   = 0.0
         latlon( i ) = ' '
c
         do j = 1, 4
            do k = 1, 3
               radius( i, k, j ) = 0.0
            enddo
         enddo
c
      enddo
c
c**   Get the guidance for the current date/time
c
      if ( aidsDataresult .eq. 0) then
        call getBigAidDTG ( 22, cur_ymdh, aidsData, result )
        if ( result .ne. 0) then
           aidsDataresult = 1
        elseif ( result .eq. 0 ) then
           last_tau = -1
           return
        endif
      endif
c
c**   Get the CARQ for the current date/time
c
      call getTech ( aidsData, 'CARQ', fstRcd, result )
      call getSingleTAU ( fstRcd, 0, tauData, result )
      radp(0) =  tauData%aRecord(1)%radp
      rrp(0)  =  tauData%aRecord(1)%rrp
      mrd(0)  =  tauData%aRecord(1)%mrd
c
c**   Get the forecast for the current date/time
c
      if ( aidid .eq. 'OFCL' ) then
           aididi(1:4) = 'JTWC'
      else
           aididi = aidid
      endif
      call getTech ( aidsData, aidid, fstRcd, result )
      if ( result .eq. 0 ) then
         call getTech ( aidsData, aididi, fstRcd, result )
cx  last ditch effort, grab the CARQ
         if ( result .eq. 0 ) then
            call getTech ( aidsData, 'CARQ', fstRcd, result )
            if ( result .eq. 0 ) then
                  last_tau = -1
                  return
            endif
         endif
      endif
c
c**   Get the forecast data by searching for all
c**     possible TAUs
c
      do 10 itau = 0, alltau, 12
c
         call getSingleTAU ( fstRcd, itau, tauData, istat )
         if ( istat .ne. 1) go to 10
c
c**   Read the basin, storm ID and current date/time
c
         if ( itau .eq. 0 ) then
            basin = tauData%atcfRcd(1)%basin
            cynum = tauData%atcfRcd(1)%cyNum
            numcy = tauData%aRecord(1)%cyNum
c
            strmid = basin//cynum//cur_ymdh(1:4)
c
            fst_ymdh = tauData%aRecord(1)%DTG
c
cx          print *, ' '
cx          print *, ' '
cx          print *, 'Storm ID       = ', strmid
cx          print *, 'Forecast ymdh  = ', fst_ymdh
cx          print *, 'Current ymdh   = ', cur_ymdh
c
            if ( fst_ymdh .ne. cur_ymdh ) then
               last_tau = 0
               print *, 'Forecast ymdh not equal the current ymdh!'
               print *, 'Forecast file not processed!'
               return
            endif
         endif
c
         num = (tauData%aRecord(1)%tau)/12
         tau(num) = tauData%aRecord(1)%tau
c
C**   Read the position and maximum wind
C
         flat(num)=alat180(tauData%aRecord(1)%lat,tauData%aRecord(1)%NS)
         flon(num)=alon360(tauData%aRecord(1)%lon,tauData%aRecord(1)%EW)
c
         latlon(num) = tauData%atcfRcd(1)%tau//' '//
     &                 tauData%atcfRcd(1)%latns(1:2)//
cc     &                 tauData%atcfRcd(1)%latns(1:2)//'.'//
     &                 tauData%atcfRcd(1)%latns(3:4)//' '//
     &                 tauData%atcfRcd(1)%lonew(1:3)//
cc     &                 tauData%atcfRcd(1)%lonew(1:3)//'.'//
     &                 tauData%atcfRcd(1)%lonew(4:5)
c
         vmax(num)= float( tauData%aRecord(1)%vmax )
c
c**   Read the 34, 50 and 64-knot wind radii
c
         do m = 1, tauData%numrcrds
c
            if ( tauData%aRecord(m)%windcode .eq. 'AAA' ) then
c
               radius( num, m, 1 ) = float( tauData%aRecord(m)%radii(1))
               radius( num, m, 2 ) = float( tauData%aRecord(m)%radii(1))
               radius( num, m, 3 ) = float( tauData%aRecord(m)%radii(1))
               radius( num, m, 4 ) = float( tauData%aRecord(m)%radii(1))
c
            elseif ( tauData%aRecord(m)%windcode .eq. 'NEQ' ) then
c
               radius( num, m, 1 ) = float( tauData%aRecord(m)%radii(1))
               radius( num, m, 2 ) = float( tauData%aRecord(m)%radii(2))
               radius( num, m, 3 ) = float( tauData%aRecord(m)%radii(3))
               radius( num, m, 4 ) = float( tauData%aRecord(m)%radii(4))
c
            endif
c           just assign the max wind rad, rad of outermost closed isobar to initial value on CARQ
            radp(num) =  radp(0)
            rrp(num)  =  rrp(0)
            mrd(num)  =  mrd(0)
         enddo
 10   continue
c
      last_tau = num
c
c**   Print out the read forecast values
c
cx    print *, ' '
cx    print *, ' last tau = ', last_tau
cx    print *, ' tau(last tau) = ', tau(last_tau)
cx    print *, ' '

cx    do num = 0, last_tau
cx
cx       write ( *, '( '' i = '', i3, '' tau = '', i3 ) ') num, tau(num)
cx       write ( *, '( '' lat = '', f5.1, '' lon = '', f5.1,
cx   &                '' vmax = '',f4.0 ) ')
cx   &                flat(num), flon(num), vmax(num)
cx       write ( *, '(''  34kt radii = '', 4f6.0 )' ) radius(num,1,1),
cx   &            radius(num,1,2), radius(num,1,3), radius(num,1,4)
cx       write ( *, '(''  50kt radii = '', 4f6.0 )' ) radius(num,2,1),
cx   &            radius(num,2,2), radius(num,2,3), radius(num,2,4)
cx       write ( *, '(''  64kt radii = '', 4f6.0 )' ) radius(num,3,1),
cx   &            radius(num,3,2), radius(num,3,3), radius(num,3,4)
cx
cx    enddo

      RETURN
      END
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
c***********************************************************************
      subroutine findHit
     &        (aidid, nomonum, nomolat, nomolon, nomoint, last_tau, hit)
c
c**     This routine finds 50-kt hits for a given forecast and site
c       input:
c               aidid: 4 char aid name
c               nomolat:  array of nomogram latitudes
c               nomolon:  array of nomogram longitudes
c               nomoint:  array of nomogram minimum intensities for 50 kt on site
c               last_tau: last tau in the forecast
c               hit: array of hits (1) or misses (0) for given forecast
c
      character*4 aidid
      integer maxtau, totpts
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
      integer nomonum, nomoint(1000), last_tau
      integer hit (0:maxtau)
      real    nomolat(1000), nomolon(1000)
      real    flatb, flonb
      real    dir, dis, keepdis, topdis
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)

      do i=0, last_tau
              hit(i) = 0
              dir =  0.0
              dis = 999.0
              keepdis = 999.0
              topdis = 20.0
              flatb = flat(i)
              flonb = flon(i)
              if (flon(i) .gt. 180.0) flonb = 360.0 - flon(i)
      do j=1, nomonum
                call dirdst( flatb,flonb,nomolat(j),nomolon(j),dir,dis )
                if ( dis .lt. topdis .and. dis .lt. keepdis) then
                     keepdis = dis
                     if ( vmax(i) .ge. nomoint(j) ) then
                        hit(i) = 1
                     else
                        hit(i) = 0
                     endif
                endif
      enddo
               if (hit(i) .eq. 1) then
                   print *,"hit: ",aidid," tau=",i*12
     &                    ," dis=", keepdis, " int=", vmax(i)
               else
cx                 print *,"miss: ",aidid," tau=",i*12
cx   &                    ," dis=", keepdis, " int=", vmax(i)
               endif
      enddo

      return
      end
c
c***********************************************************************
      subroutine findHiti
     &        (aidid, nomonum, nomolat, nomolon, nomoint, last_tau, hit)
c
c**     This routine finds 50-kt hits for a given forecast and site
c       In this case we are trying intermediate points.
c       input:
c               aidid: 4 char aid name
c               nomolat:  array of nomogram latitudes
c               nomolon:  array of nomogram longitudes
c               nomoint:  array of nomogram minimum intensities for 50 kt on site
c               last_tau: last tau in the forecast
c               hit: array of hits (1) or misses (0) for given forecast
c
      character*4 aidid
      integer maxtau, totpts
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
      integer nomonum, nomoint(1000), last_tau
      integer hit (0:maxtau)
      real    nomolat(1000), nomolon(1000)
      real    flata, flatb, flati, flona, flonb, floni
      real    vmaxa, vmaxb, vmaxi
      real    dir, dis, keepdis, topdis
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)

      do i=1, last_tau
          hit(i) = 0
          dir =  0.0
          dis = 999.0
          keepdis = 999.0
          topdis = 6.5
          flata = flat(i-1)
          flona = flon(i-1)
          flatb = flat(i)
          flonb = flon(i)
          if (flon(i-1) .gt. 180.0) flona = 360.0 - flon(i-1)
          if (flon(i) .gt. 180.0)   flonb = 360.0 - flon(i)
          vmaxa = vmax(i-1)
          vmaxb = vmax(i)
cx       get some intermediate points.
cx       must be within topdis nm of the nomogram grid point.
         do ii=0, 11
            flati=flata + (flatb-flata)*(float(ii)/12.)
            floni=flona + (flonb-flona)*(float(ii)/12.)
            vmaxi=vmaxa + (vmaxb-vmaxa)*(float(ii)/12.)
            do j=1, nomonum
c
              if ( hit(i) .eq. 0 ) then
                call dirdst( flati,floni,nomolat(j),nomolon(j),dir,dis )
                if ( dis.lt.topdis .and. dis.lt.keepdis ) then
                     keepdis = dis
                     if ( vmaxi   .ge. nomoint(j) ) then
                          hit(i) = 1
                          print *,"hit: ",aidid," t=",(i-1)*12+ii,
     &                            " vmax=", vmaxi, " d=", keepdis
                     else
                          hit(i) = 0
                     endif
                endif
              endif
c
            enddo
         enddo
               if (hit(i) .eq. 1) then
cx                 print *,"hit: ",aidid," tau=",i*12
cx   &                    ," dis=", keepdis, " int=", vmax(i)
               else
cx                 print *,"miss: ",aidid," tau=",i*12
cx   &                    ," dis=", keepdis, " int=", vmax(i)
               endif
      enddo

      return
      end

c
c***********************************************************************
      subroutine findHitTY (cur_ymdh, site_lat,site_lon,naids,aidid,
     &            nomonum,nomolat,nomolon,nomoint,
     &            nomonumTS,nomolatTS,nomolonTS,nomointTS,last_tau, hit)
c
c**     This routine finds 50-kt hits for a given forecast and site
c       In this case we are trying intermediate points.
c       This is for the Navy nomograms, Typhoon strength
c       input:
c               cur_ymdh: current date time group
c               site_lat: station latitude (deg)
c               site_lon: station longitude (deg)
c               naids:    number of aids (used to output hourly winds)
c               aidid:    4 char aid name
c               nomonum:  number of points in TY nomogram
c               nomolat:  array of TY nomogram latitudes
c               nomolon:  array of TY nomogram longitudes
c               nomoint:  array of TY nomogram minimum intensities for 50 kt on site
c               nomonumTS:number of points in TS nomogram
c               nomolatTS:array of nomogram latitudes for TS
c               nomolonTS:array of nomogram longitudes for TS
c               nomointTS:array of nomogram minimum intensities for 50 kt on site for TS
c               last_tau: last tau in the forecast
c               hit:      array of hits (1) or misses (0) for given forecast
c
      character*4 aidid
      character*10 cur_ymdh, fst_ymdh
      integer maxtau, totpts
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
      integer nomonum, nomonumTS, nomocount
      integer nomoint(1000), nomointTS(1000),last_tau
      integer hit (0:maxtau)
      integer it_hit
      real    nomolat(1000), nomolon(1000)
      real    nomolatTS(1000), nomolonTS(1000)
      real    flata, flatb, flati, flona, flonb, floni
      real    vmaxa, vmaxb, vmaxi, vmaxmod(0:maxtau), vmaxmodG(0:maxtau)
      real    dir, dis, keepdis, topdis, circ_dis, site_dis
      real    site_lat, site_lon
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)

cx    Yoksuka
cx    site_lat=35.3
cx    site_lon=139.65
cx    The maximum distance is 360 nm from site
      circ_dis = 360.0

      do i=1, last_tau
          hit(i) = 0
          dir =  0.0
          dis = 999.0
          keepdis = 999.0
cx        The top distance from a nomogram point.
cx        More contours and you can reduce this.
          topdis = 20.0
          flata = flat(i-1)
          flona = flon(i-1)
          flatb = flat(i)
          flonb = flon(i)
          if (flon(i-1) .gt. 180.0) flona = 360.0 - flon(i-1)
          if (flon(i) .gt. 180.0) flonb = 360.0 - flon(i)
          vmaxa = vmax(i-1)
          vmaxb = vmax(i)
cx       get some intermediate points, pretty lenient about hits.
cx       if we find one in all the intermediate points, we use it.
cx       we only care that it is within topdis nm of the hit location.
         do ii=0, 11
            flati=flata + (flatb-flata)*(float(ii)/12.)
            floni=flona + (flonb-flona)*(float(ii)/12.)
            vmaxi=vmaxa + (vmaxb-vmaxa)*(float(ii)/12.)
            call dirdst( flati,floni,site_lat,site_lon,dir,site_dis )

            if ( site_dis .lt. circ_dis ) then

c            assign nomocount
             if ( vmaxi .ge. 65. ) nomocount = nomonum
             if ( vmaxi .lt. 65. ) nomocount = nomonumTS

             do j=1, nomocount
c
c  ---    single forecast, just print out all the values

cx             Typhoon nomogram
               if ( naids .eq. 1 .and. vmaxi .ge. 65. ) then
                 call dirdst(flati,floni,nomolat(j),nomolon(j),
     &                       dir,dis)
                 if ( dis .lt. keepdis ) then
                       vmaxmod(i) = .67*vmaxi*nomoint(j)/100.
                       vmaxmodG(i) =    vmaxi*nomoint(j)/100.
                       keepdis = dis
                 endif
cx             TS nomogram
               elseif ( naids .eq. 1 .and. vmaxi .lt. 65. ) then
                 call dirdst(flati,floni,nomolatTS(j),nomolonTS(j),
     &                        dir,dis)
                 if ( dis .lt. keepdis ) then
                       vmaxmod(i) = .67*vmaxi*nomointTS(j)/100.
                       vmaxmodG(i) =    vmaxi*nomointTS(j)/100.
                       keepdis = dis
                 endif
c
c  ---    probabilities for groups of forecasts.

cx             Typhoon nomogram
               elseif ( hit(i) .eq. 0 .and. vmaxi .ge. 65. ) then
                 call dirdst(flati,floni,nomolat(j),nomolon(j),
     &                       dir,dis)
                 if ( dis .lt. topdis ) then
                       vmaxmod(i) = .67*vmaxi*nomoint(j)/100.
                       vmaxmodG(i) =    vmaxi*nomoint(j)/100.
cx                     if ( vmaxmod(i) .ge. 50. .or. naids .eq. 1 ) then
                       if ( vmaxmod(i) .ge. 50. .or.
     &                      naids .eq. 1 ) then
                          hit(i) = 1
                          keepdis = dis
                          print *,"TY hit: ",aidid," t=",(i-1)*12+ii,
     &                    " vmax=", vmaxmod(i), "G=", vmaxmodG(i),
     &                    " d=", keepdis
                       endif
                 endif
cx             TS nomogram
               elseif ( hit(i) .eq. 0 .and. vmaxi .lt. 65. ) then
                 call dirdst(flati,floni,nomolatTS(j),nomolonTS(j),
     &                       dir,dis)
                 if ( dis .lt. topdis ) then
                       vmaxmod(i) = .67*vmaxi*nomointTS(j)/100.
                       vmaxmodG(i) =    vmaxi*nomointTS(j)/100.
cx                     if ( vmaxmod(i) .ge. 50. .or. naids .eq. 1 ) then
                       if ( vmaxmod(i) .ge. 50. .or.
     &                      naids .eq. 1 ) then
                          hit(i) = 1
                          keepdis = dis
                          print *,"TS hit: ",aidid," t=",(i-1)*12+ii,
     &                    " vmax=", vmaxmod(i), "G=", vmaxmodG(i),
     &                    " d=", keepdis
                       endif
                 endif

               endif

             enddo
cx
cx           print the v at site for a single aid
             call dtgmod( cur_ymdh, (i-1)*12+ii, fst_ymdh, istat )
             if ( naids .eq. 1 .and. vmaxmod(i) .gt. 0.0) then
                print *,(i-1)*12+ii," h ", fst_ymdh,
     &                 " vmax=", vmaxmod(i), " gust=", vmaxmodG(i)
                write(24, *)(i-1)*12+ii," h ", fst_ymdh,
     &                 " vmax=", vmaxmod(i), " gust=", vmaxmodG(i)
             endif
            elseif ( naids .eq. 1) then
               call dtgmod( cur_ymdh, (i-1)*12+ii, fst_ymdh, istat )
               print *,    (i-1)*12+ii," h ", fst_ymdh," off nomogram"
               write(24, *)(i-1)*12+ii," h ", fst_ymdh," off nomogram"
            endif
         enddo

      enddo

cx    final clean up, only count initial hit
      it_hit=0
      do i=1, last_tau
        if (it_hit .eq. 1) hit(i)=0
        if (hit(i) .eq. 1) it_hit=1
      enddo

      return
      end

c
c***********************************************************************
      subroutine findVatSite
     &   (cur_ymdh, aidid, nomonum, nomolat, nomolon, nomoint, last_tau)
c
c**     This routine finds V at the site for the hourly posits of the forecast
c       passing through Yokota nomogram.  Until further notice, we use a simple
c       multiplicative factor determined from the threshold required to get 50 kt on site.
c       input:
c               cur_ymdh: dtg
c               aidid: 4 char aid name
c               nomolat:  array of nomogram latitudes
c               nomolon:  array of nomogram longitudes
c               nomoint:  array of nomogram minimum intensities for 50 kt on site
c               last_tau: last tau in the forecast
c
      character*10 cur_ymdh, fst_ymdh
      character*4 aidid
      integer maxtau, totpts
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
      integer istat
      integer iaidtau
      integer nomonum, nomoint(1000), last_tau
      real    nomolat(1000), nomolon(1000)
      real    flata, flatb, flati, flona, flonb, floni
      real    vmaxa, vmaxb, vmaxi
      real    vmaxsite
      real    dir, dis, keepdis, topdis
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)

      print *, "  "
      print *, "single aid run"
      do i=1, last_tau
          dir =  0.0
          dis = 999.0
          keepdis = 999.0
          topdis = 8.0
          vmaxsite = 0.0
          flata = flat(i-1)
          flona = flon(i-1)
          flatb = flat(i)
          flonb = flon(i)
          if (flon(i) .gt. 180.0) flona = 360.0 - flon(i-1)
          if (flon(i) .gt. 180.0) flonb = 360.0 - flon(i)
          vmaxa = vmax(i-1)
          vmaxb = vmax(i)
cx       get intermediate points
         do ii=0, 11
            flati=flata + (flatb-flata)*(float(ii)/12.)
            floni=flona + (flonb-flona)*(float(ii)/12.)
            vmaxi=vmaxa + (vmaxb-vmaxa)*(float(ii)/12.)
            do j=1, nomonum
c
                call dirdst( flati,floni,nomolat(j),nomolon(j),dir,dis )
                if ( dis .lt. topdis ) then
                          keepdis = dis
                          vmaxsite =  vmaxi**2/float(nomoint(j))
                endif
c
            enddo
            iaidtau = (i-1)*12+ii
            call dtgmod( cur_ymdh, iaidtau, fst_ymdh, istat )
            if (vmaxsite .eq. 0.0) then
             print *,iaidtau,"h  ",aidid," ",fst_ymdh," off nomogram"
             write(24,*, err = 1040)
     &          iaidtau,"h  ",fst_ymdh," off nomogram"
            else
             print *,iaidtau,"h  ",aidid," ",fst_ymdh," vmax=", vmaxsite
             write(24,*, err = 1040)
     &          iaidtau,"h  ",fst_ymdh," vmax=", vmaxsite
            endif
         enddo
      enddo

      return
 1040 continue
      print *, 'Error writing to deterministic Yokota wind file'
      return
      end

c
c
c
c*************************************************************************
      subroutine fillfst( cur_ymdh, last_tau )
c
c**     This routine fills missing taus (if there are any)
c       input:
c               cur_ymdh: current dtg
c               last_tau: last tau in the forecast
c
      integer maxtau, totpts
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)
c
      character*2  basin, padding
c
      real    flatmin, flatmax, flonmin, flonmax, radmax
      integer tau
      integer radp, rrp, mrd
      integer rmid
c
c
      character*2   cprob(0:7)
      character*2   cprob_34( totpts, 0:7 ), cprob_50( totpts, 0:7 )
      character*2   cprob_64( totpts, 0:7 )
      character*10  cur_ymdh
      character*16  latlon
c
cx    write ( *, *) "---- Before Filling The Missing TAUs ----"
cx    do num = 0, last_tau
c
cx       write ( *, '( '' i = '', i3, '' tau = '', i3 ) ') num, tau(num)
cx       write ( *, '( '' lat = '', f5.1, '' lon = '', f5.1,
cx   &                '' vmax = '',f4.0 ) ')
cx   &                flat(num), flon(num), vmax(num)
cx       write ( *, '(''  34kt radii = '', 4f6.0 )' ) radius(num,1,1),
cx   &            radius(num,1,2), radius(num,1,3), radius(num,1,4)
cx       write ( *, '(''  50kt radii = '', 4f6.0 )' ) radius(num,2,1),
cx   &            radius(num,2,2), radius(num,2,3), radius(num,2,4)
cx       write ( *, '(''  64kt radii = '', 4f6.0 )' ) radius(num,3,1),
cx   &            radius(num,3,2), radius(num,3,3), radius(num,3,4)
c
cx    enddo
c
c   fill in the missing with average of before and after
c
      do num = 1, last_tau-1
c     fill lat and lon
         if ( flat(num) .eq. 0.0 .and. flat(num+1) .ne. 0.0) then
              tau (num) = (tau (num-1) + tau (num+1))/2
              flat(num) = (flat(num-1) + flat(num+1))/2.
              flon(num) = (flon(num-1) + flon(num+1))/2.
c     fill wind radii
              do i = 1,3
              do j = 1,4
                if ( radius(num+1,i,j) .gt. 0 ) then
                 radius(num,i,j)=(radius(num-1,i,j)+radius(num+1,i,j))/2
                else
                 radius(num,i,j)=radius(num-1,i,j)
                endif
              enddo
              enddo
         endif
c     fill intensity
         if ( vmax(num) .eq. 0.0 .and. flat(num-1) .ne. 0.0) then
              vmax(num) = (vmax(num-1) + vmax(num+1) )/2.
         endif
c     attempt to fill wind radii for extratropical
         do j = 1,4
           if ( vmax(num) .gt. 34. .and. radius(num,1,j) .eq. 0) then
                radius(num,1,j) = radius(num-1,1,j)
           endif
           if ( vmax(num) .gt. 50. .and. radius(num,2,j) .eq. 0) then
                radius(num,2,j) = radius(num-1,2,j)
           endif
           if ( vmax(num) .gt. 63. .and. radius(num,3,j) .eq. 0) then
                radius(num,3,j) = radius(num-1,3,j)
           endif
         enddo
      enddo


cx    write ( *, *) "---- After Filling The Missing TAUs ----"
cx    do num = 0, last_tau
c
cx       write ( *, '( '' i = '', i3, '' tau = '', i3 ) ') num, tau(num)
cx       write ( *, '( '' lat = '', f5.1, '' lon = '', f5.1,
cx   &                '' vmax = '',f4.0 ) ')
cx   &                flat(num), flon(num), vmax(num)
cx       write ( *, '(''  34kt radii = '', 4f6.0 )' ) radius(num,1,1),
cx   &            radius(num,1,2), radius(num,1,3), radius(num,1,4)
cx       write ( *, '(''  50kt radii = '', 4f6.0 )' ) radius(num,2,1),
cx   &            radius(num,2,2), radius(num,2,3), radius(num,2,4)
cx       write ( *, '(''  64kt radii = '', 4f6.0 )' ) radius(num,3,1),
cx   &            radius(num,3,2), radius(num,3,3), radius(num,3,4)
c
cx    enddo

c
      return
      end

c
c********1*********2*********compute intermediate points***6*********7**
c
      subroutine latlon_hr(last_tau,iprint_tau,iprint_hr,hr_lat,hr_lon)
      integer maxtau
      parameter ( maxtau=10, ntsave=7, totpts=200, maxgrid=20 )
c
      common /fcst_data/ tau(0:maxtau), flat(0:maxtau), flon(0:maxtau),
     &        vmax(0:maxtau), radius(0:maxtau,4,4), latlon(0:maxtau),
     &        basin, padding, radp(0:maxtau), rrp(0:maxtau),
     &        mrd(0:maxtau)
c
      character*2  basin, padding
      character*16 latlon

      if (iprint_tau .eq. last_tau) then
        alatinc = (flat(iprint_tau) - flat(iprint_tau-1)) *iprint_hr/12.
        aloninc = (flon(iprint_tau) - flon(iprint_tau-1)) *iprint_hr/12.
      else
        alatinc = (flat(iprint_tau+1) - flat(iprint_tau)) *iprint_hr/12.
        aloninc = (flon(iprint_tau+1) - flon(iprint_tau)) *iprint_hr/12.
      endif
      hr_lat = flat(iprint_tau) + alatinc
      hr_lon = flon(iprint_tau) + aloninc
      return
      end
c
c********1*********2*********write wind radii to ww3 file**6*********7**
c
c     reformat the atcf output files for the grid interpolation
c     the atcf lon is 0-360, positive to west
c     interpolation routine is -180 --> +180, positive to the east
c     changed!  now interpolation routine is 0 --> +360, positive to the east

      subroutine writewnd (crad,oblat,oblon,vmax,wdir,cur_ymdh)
      character*4  crad
      character*10 cur_ymdh
      real         vmax10
      RPD = 0.0174532
cx    10-minute mean winds vice 1-minute mean winds
cx    vmax10 = vmax*0.88
cx    changed 12/14/2010 to new WMO standard
cx    vmax10 = vmax*0.93
cx    changed 1/18/2011 to just use 1-minute mean winds since evaluation showed negative bias.
      vmax10 = vmax
      if (vmax10 .eq. 0.0) then
               ucmp= 0.0
               vcmp = 0.0
      else
               chi = ((wdir + 180.)* RPD)
               vcmp = COS(chi) * vmax10
               ucmp = SIN(chi) * vmax10
      endif
c
c the atcf lon convention is 0 -> 360 towards the west, grid interp
c is 0 -> 360 towards the east.
c
      if (oblon .lt. 0.0) then
          oblon=360.-oblon
      else
          oblon=-oblon
      end if
      write (32,'(3f15.3)') oblon, oblat, ucmp
      write (33,'(3f15.3)') oblon, oblat, vcmp
cx  this is the atcf format file (a/c format)
cx  there is a header record and an ENDAT record in this format too.
      iwdir = nint(wdir)
      ivmax10 = nint(vmax10)
      write (38,'(a6,2f7.1,1x,i3.3,1x,i3,1x,a10,a2)')
     & ' SCT  ',oblat, oblon, iwdir, ivmax10, cur_ymdh, '00'
cx    write (38,'(a8,2f7.1,2i4,1x,a10,a2)')
cx   & ' OFCL   ',oblat, oblon, nint(wdir), nint(vmax10), cur_ymdh, '00'
      return
      end




C******************************************************************
C
C  BRIEF DESCRIPTION OF PROGRAM MODULES:
C
C   backspaceFile - Repositions the specified file back "numRcrds" records.
C   buildDvorak - Builds a long or short term trend dvorak code.
C   convToAiddata - Create an AID_DATA record using data passed in.
C   convToARecord - Create an A_RECORD record using data passed in.
C   convWriteAidData - Convert the passed data to an AID_DATA record and
C              write it to the specified output file.
C   doReadBT - Read one record from the best track file.  Same as readBT
C              except it can read a multi-line best track record, reads
C              10 digit dtg and reads the storm-type.
C   doWriteAidRcd - Write the aid record to the output data file.
C   doWriteErrRadRcd - Write the error radius record to the output data file.
C   getAidDTG - Gets the first aid data for the specified DTG from
C              the input file.
C   getAidTAU - Gets the first A_RECORD record for the specified tau
C              from the supplied AID_DATA.
C   getARecord - Reads one record of specified type from the input file.
C   getBigAidDTG - Gets all the aid data for the specified DTG from
C                  the input file
C   getSingleTAU - Gets the aid data for the specified tau from the
C              supplied AID_DATA.
C   getTech - Gets data for a specified aid technique from the supplied
C             BIG_AID_DATA structure and returns an AID_DATA structure
C   newWriteAidRcd - Write the aid record to the output data file.
C                    NHC version
C   newWriteErrRadRcd - Write the error radius record to the output data file.
C                    NHC version
C   outputAircraft - Build the aircraft section of the fix record
C   outputAnalysis - Build the analysis section of the fix record
C   outputArcd - write the A_RECORD to the specified file stream.
C   outputDropsonde - Build the dropsonde section of the fix record
C   outputDVTO - Build the objective dvorak section of the fix record
C   outputDVTS - Build the subjective dvorak section of the fix record
C   outputFrcd - Write the F_RECORD to the spectified file.
C   outputMicrowave - Build the microwave section of the fix record
C   outputRadar - Build the radar section of the fix record.
C   processAircraft - Assigns the data for an aircraft fix record.
C   processAnalysis - Assigns the data for an analysis fix record.
C   processArcd - Assigns the data for a A_RECORD structure.
C   processDropsonde - Assigns the data for a dropsonde fix record.
C   processDvorak - Assigns the data for a DVORAK fix structure.
C   processDVTO - Assigns the data for an objective dvorak fix record
C   processDVTS - Assigns the data for a subjective dvorak fix record.
C   processFrcd - Assigns the data for an F_RECORD structure.
C   processMicrowave - Assigns the data for a microwave fix record.
C   processRadar - Assigns the data for a radar fix record.
C   readARecord - Reads one AID_DATA data info from the input file.
C   readBest - Read one record from the best track file,
C              including the basin,cyclone number, century and development level.
C   readBestTrk - Read one record from the best track file.
C              Same as readBest except it can read multi-line best track files.
C   readBT   - Read one record from the best track file.
C   readBTrk - Read one record from the best track file.
C              Same as readBT except it can read multi-line best track files.
C   readFRecord - Reads one FIX_DATA data info from the input file.
C   readNext - Reads the next ATCF_RECORD record from the input file.
C   readNextFix - Reads the next FIX_RECORD record from the input file.
C   writeAid - Write the aid record to the output data file.
C   writeAidData - Write an AID_DATA record to the specified file stream.
C   writeAidRcd - Write the aid record to the output data file.
C                JTWC, NPMOC version
C   writeFix - Write a FIX_DATA record to the specified file.
C
C******************************************************************

C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBT
C
C  DESCRIPTION:  Read one record from the best track file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
C
C  INPUT PARAMETERS:
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     cent - century of posit (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C
C........................END PROLOGUE..................................
C
      subroutine readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
c
c         formal parameters
      integer           datFile
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios

      read( datFile, '(8x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3)',
     1     iostat=ios ) cent, dtg, flat, ns, flon, ew, iwind
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBest
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except also reads the basin and
C                cyclone number.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBest (datFile, basin, cycnum, cent, dtg, flat, ns,
C                           flon, ew, iwind, dev, ios )
C
C  INPUT PARAMETERS:
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     basin - basin read from the best track file
C     cycnum - cyclone num read from the best track file
C     cent - century (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     dev - development type
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C   sampson, nrl    jan 4 07   added dev
C
C........................END PROLOGUE..................................
C
      subroutine readBest (datFile, basin, cycnum, cent, dtg, flat, ns,
     &     flon, ew, iwind, dev, ios )
c
c         formal parameters
      integer           datFile
      character*2       basin
      character*2       cycnum
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ip
      character*2       dev
      integer           ios

      read( datFile,
     1 '(a2,2x,a2,2x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3,2x,i4,2x,a2)',
     2     iostat=ios )
     3  basin,cycnum,cent,dtg,flat,ns,flon,ew,iwind,ip,dev
C
      RETURN
      END


C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doReadBT
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except it can read a multi-line
C                best track record and it reads a 10 digit dtg
C                and it reads the storm-type.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call doReadBT (datFile,dtg,flat,ns,flon,ew,iwind,bttype,ios)
C
C  INPUT PARAMETERS:
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     dtg - YYYYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     bttype - storm development (TD,TS,TY,ST,TC,HU,SD,SS,EX,LO,WV,ET,XX)
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doReadBT (datFile,dtg,flat,ns,flon,ew,iwind,bttype,
     &     ios)

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*10       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      character*2       bttype
      integer           ios
c
c         local variables
      type (AID_DATA) btRcd
      integer         readStat

c     Read the next record in the data file.
      call readARecord( datFile, btRcd, readStat )
      if( readStat .eq. 1 ) then
         dtg = btRcd%aRecord(1)%DTG
         flat = btRcd%aRecord(1)%lat
         ns = btRcd%aRecord(1)%NS
         flon = btRcd%aRecord(1)%lon
         ew = btRcd%aRecord(1)%EW
         iwind = btRcd%aRecord(1)%vmax
         bttype = btRcd%aRecord(1)%ty
      endif
C     Switch the returned results from readARecord to results as defined by
C     FORTRAN read
      if( readStat .lt. 0 ) then
         ios = readStat
      elseif( readStat .eq. 1 ) then
         ios = 0
      elseif( readStat .eq. 0 ) then
         ios = 1
      endif
C
      END


C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBTrk
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except it can read multi-line
C                best track files.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBTrk (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
C
C  INPUT PARAMETERS:
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     cent - century of posit (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readBTrk (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios
c
c         local variables
      character*10      dtgnew
      character*2       bttype

      call doReadBT( datFile,dtgnew,flat,ns,flon,ew,iwind,bttype,
     &     ios )
      cent = dtgnew(1:2)
      dtg = dtgnew(3:10)
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBestTrk
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except also reads the basin and
C                cyclone number.
C                Same as readBest except it can read multi-line
C                best track files.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBestTrk (datFile, basin, cycnum, cent, dtg, flat, ns,
C                           flon, ew, iwind, dev, ios )
C
C  INPUT PARAMETERS:
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     basin - basin read from the best track file
C     cycnum - cyclone num read from the best track file
C     cent - century (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     dev - development level
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  sampson added development level   jan 07
C........................END PROLOGUE..................................
C
      subroutine readBestTrk (datFile, basin, cycnum, cent, dtg, flat,
     &     ns, flon, ew, iwind, dev, ios )

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*2       basin
      character*2       cycnum
      character*8       dtg
      character*2       cent
      character*2       dev
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios
c
c         local variables
      type (AID_DATA) btRcd
      integer         readStat

c     Read the next record in the data file.
      call readARecord( datFile, btRcd, readStat )
      if( readStat .eq. 1 ) then
         basin = btRcd%aRecord(1)%basin
         write( cycnum, '(i2.2)') btRcd%aRecord(1)%cyNum
         cent = btRcd%aRecord(1)%DTG(1:2)
         dtg = btRcd%aRecord(1)%DTG(3:10)
         flat = btRcd%aRecord(1)%lat
         ns = btRcd%aRecord(1)%NS
         flon = btRcd%aRecord(1)%lon
         ew = btRcd%aRecord(1)%EW
         iwind = btRcd%aRecord(1)%vmax
         dev  = btRcd%aRecord(1)%ty
      endif
C     Switch the returned results from readARecord to results as defined by
C     FORTRAN read
      if( readStat .lt. 0 ) then
         ios = readStat
      elseif( readStat .eq. 1 ) then
         ios = 0
      elseif( readStat .eq. 0 ) then
         ios = 1
      endif

C
      RETURN
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call doWriteAidRcd (datFile,stormID,cdtg,techname,itau,llwnd)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     itau - forecast period
C     llwnd - array of integers dimensioned (3) where
C             and the three components are lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doWriteAidRcd (datFile, stormID, cdtg, techname,
     1     itau, llwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           itau
      integer           llwnd(llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif

cx    Check for model runs where lat/lon out of range - convert to 0's
cx    sampson nrl oct 26, 1998
cx    sampson nrl aug 19, 2000  changed lon check for less than zero
cx    Handle cases of forecasts crossing 0 longitude,  ajs 1/17/01
      if( llwnd(2) .lt. 0 ) then
          llwnd(2) = llwnd(2) + 3600
      endif

      if( llwnd(1) .lt. -900  .or.
     1    llwnd(1) .gt.  900  .or.
     2    llwnd(2) .lt.    0  .or.
     3    llwnd(2) .gt.  3600 ) then
         llwnd(1) = 0
         llwnd(2) = 0
      endif
cx    Check for model runs where wind out of range - convert to 0's
cx    sampson nrl oct 26, 1998
      if( llwnd(3) .lt. 0 .or. llwnd(3) .gt. 300)
     1     llwnd(3) = 0

      if( llwnd(1) .ne. 0 .or. llwnd(2) .ne. 0
     1     .or. llwnd(3) .ne. 0) then
c     Convert from -900 thru 900 to 900S thru 900N
         ns = 'N'
         ilat = llwnd(1)
         if( ilat .lt. 0 ) then
            ilat = -ilat
            ns = 'S'
         endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
         ew = 'W'
         ilon = llwnd(2)
         if( ilon .gt. 1800 ) then
            ilon = 3600 - ilon
            ew = 'E'
         endif
c     Write the aid record...
cx    only if the lat and lon are meaningful
         if ( ilon .le. 1800 .and. ilat .lt. 900 ) then
            write(datFile,9080) basin, stormnum, cdtg,
     1           technum, techname, itau, ilat, ns, ilon, ew,
     1           llwnd(3)
 9080       format( A2,", ",A2,", ",A10,", ",A2,", ",A4,", ",
     1           I3,", ",I3,A1,", ",I4,A1,", ",I3 )
         endif
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  newWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                NHC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call newWriteAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine newWriteAidRcd (datFile, stormID, cdtg, techname,
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llwnd(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = newnumtau
c     For CARQ and WRNG loop on the taus: -24, -18, -12, -6 and 0
      if (techname.eq.'CARQ' .or. techname.eq.'WRNG') loopend = 5

c     For all other aids loop on the taus:
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       do all taus in 12 hr increments
         else
	    itau = (ii-1) * 12
         endif
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo
         call doWriteAidRcd(datFile, stormID, cdtg, techname, itau,
     &                      llwnd )
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                JTWC, NPMOC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (10,3) where
C               the first dimension is the TAUs 12, 24, 36, 48, 72, ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeAidRcd (datFile, stormID, cdtg, techname,
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      character*6       oldstormID;
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      oldstormID = stormID(1:4)//stormID(7:8)
      call writeAid( datFile, oldstormID, stormID(5:6), cdtg(3:10), techname,
     &     ltlnwnd )
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAid
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAid (datFile,stormID,century,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp0297
C     century - 1st two digits of the storm year, eg. 19
C     cdtg - current dtg, eg. 98060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (numtau,llw) where
C               the first dimension is the TAUs 12, 24, 36, 48, 60, 72 ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  sampson, nrl  Apr 99     R120 - has only 72, 96 and 120 hr fcsts
C  sampson, nrl  Jan 00     C120 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Apr 00     XTRP - 120 hour forecast, 12 hr interval
C  sampson, nrl  May 01     ST5D - 120 hour forecast, 12 hr interval
C  sampson, nrl  May 01     STIP, STID - 120 hour forecast, 12 hr interval
C  sampson, nrl  Oct 02     R120 - is 120 hour forecast, 12 hr interval
C  sampson, nrl  May 04     STWP, STWD - 120 hour forecast, 12 hr interval
C  sampson, nrl  Jun 04     xxSP, xxSD - 120 hour forecast, 12 hr interval
C  sampson, nrl  Jun 04     xxS1 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Jun 05     xxS3 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Sep 05     STFP, STFD - 120 hour forecast, 12 hr interval
C  sampson, nrl  Sep 05     STOP, STOD - 120 hour forecast, 12 hr interval
C........................END PROLOGUE..................................
C
      subroutine writeAid (datFile, stormID, century, cdtg, techname,
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*6       stormID
      character*2       century
      character*8       cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ii, itau
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif
cx
      loopend = 5
      if (techname.eq.'C120' .or. techname.eq.'XTRP' .or.
     &    techname.eq.'C121' .or. techname.eq.'C12X' .or.
     &    techname.eq.'C12C' .or. techname.eq.'C12X' .or.
     &    techname.eq.'STIP' .or. techname.eq.'STID' .or.
     &    techname.eq.'STWP' .or. techname.eq.'STWD' .or.
     &    techname.eq.'STFP' .or. techname.eq.'STFD' .or.
     &    techname.eq.'STOP' .or. techname.eq.'STOD' .or.
     &    techname(3:4).eq.'SD' .or. techname(3:4).eq.'SP' .or.
     &    techname(3:4).eq.'S1' .or. techname(3:4).eq.'S3' .or.
     &    techname.eq.'ST5D' .or. techname.eq.'R120' ) loopend = numtau

c     Loop on the taus: 12, 24, 36, 48 and 72
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       long-range forecasts (e.g., C120) ... sampson NRL Jan 00
cx       does all taus in 12 hr increments
         else if ( techname.eq.'C120' .or. techname.eq.'XTRP' .or.
     &             techname.eq.'C121' .or. techname.eq.'C12X' .or.
     &             techname.eq.'C12C' .or. techname.eq.'C12X' .or.
     &             techname.eq.'STIP' .or. techname.eq.'STID' .or.
     &             techname.eq.'STWP' .or. techname.eq.'STWD' .or.
     &             techname.eq.'STFP' .or. techname.eq.'STFD' .or.
     &             techname.eq.'STOP' .or. techname.eq.'STOD' .or.
     &             techname(3:4).eq.'SD' .or. techname(3:4).eq.'SP' .or.
     &             techname(3:4).eq.'S1' .or. techname(3:4).eq.'S3' .or.
     &             techname.eq.'ST5D' .or. techname.eq.'R120' ) then
	    itau = ii * 12
cx       special case for extended forecasts (R120) ... sampson NRL Apr 99
cx       else if ( techname .eq. 'R120' ) then
cx          itau = ii * 12 + 60
cx       old case for 72 hour forecasts ...
         else
            itau = ii * 12
            if( itau .eq. 60 ) itau = 72
         endif
cx       Check for model runs where lat/lon out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,1) .lt. -900  .or.
     1	     ltlnwnd(ii,1) .gt.  900  .or.
     2       ltlnwnd(ii,2) .lt. -1800 .or.
     3       ltlnwnd(ii,2) .gt.  3600 ) then
      	       ltlnwnd(ii,1) = 0
      	       ltlnwnd(ii,2) = 0
         endif
cx       Check for model runs where wind out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,3) .lt. 0 .or. ltlnwnd(ii,3) .gt. 300)
     1	       ltlnwnd(ii,3) = 0

         if( ltlnwnd(ii,1) .ne. 0 .or. ltlnwnd(ii,2) .ne. 0
     1        .or. ltlnwnd(ii,3) .ne. 0) then
c           Convert from -900 thru 900 to 900S thru 900N
            ns = 'N'
            ilat = ltlnwnd(ii,1)
            if( ilat .lt. 0 ) then
               ilat = -ilat
               ns = 'S'
            endif
c           Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
            ew = 'W'
            ilon = ltlnwnd(ii,2)
            if( ilon .gt. 1800 ) then
               ilon = 3600 - ilon
               ew = 'E'
            endif
c           Write the aid record...
cx          only if the lat and lon are meaningful
	    if ( ilon .lt. 1800 .and. ilat .lt. 900 ) then
               write(datFile,9080) basin, stormnum, century, cdtg,
     1              technum, techname, itau, ilat, ns, ilon, ew,
     1              ltlnwnd(ii,3)
 9080         format( A2,", ",A2,", ",A2,A8,", ",A2,", ",A4,", ",
     1              I3,", ",I3,A1,", ",I4,A1,", ",I3 )
            endif
         endif
      enddo
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readNext
C
C  DESCRIPTION:  Reads the next ATCF_RECORD record from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readNext (datFile,record,ios)
C
C  INPUT PARAMETERS:
C     datFile - unit number of input data file
C
C  OUTPUT PARAMETERS:
C     rcd - structure to read record into
C     ios - return 0 success, neg for end of file, pos for error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readNext (datFile, rcd, ios )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      type (ATCF_RECORD) rcd
      integer            ios
c
c         local variables
      character line*200
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Zero out the dtg, tech, lat, lon and vmax just in case
      rcd%DTG = '          '
      rcd%tech = '    '
      rcd%latns = '    '
      rcd%lonew = '     '
      rcd%vmax = '   '
c
c     Read one data record.
      read( datFile, '(a200)', iostat=ios ) line
c     Skip over blank lines.
      do while( ios .eq. 0 .and. len_trim(line) .eq. 0 )
         read( datFile, '(a200)', iostat=ios ) line
      enddo
      if( ios .eq. 0 ) then
c
c     Get the individual fields from the data record.
         read( line, '(a2,2x,a2,2x,a10,2x,a2,2x,a4,2x,a3)' )
     &        rcd%basin, rcd%cyNum, rcd%DTG, rcd%technum, rcd%tech,
     &        rcd%tau
         read( line, '(35x,a4,2x,a5,2x,a3)' )
     &        rcd%latns, rcd%lonew, rcd%vmax
         read( line, '(53x,a4,2x,a2,2x,a3,2x,a3,4(2x,a4))' )
     &        rcd%mslp, rcd%ty, rcd%rad, rcd%windcode, rcd%radii(1),
     &        rcd%radii(2), rcd%radii(3), rcd%radii(4)
         read( line, '(97x,a4,2x,a4,5(2x,a3),2x,a3,2x,a3,2x,a3)' )
     &        rcd%radp, rcd%rrp, rcd%mrd, rcd%gusts, rcd%eye,
     &        rcd%subregion, rcd%maxseas, rcd%initials, rcd%dir,
     &        rcd%speed
         read( line, '(149x,a10,2x,a1)' )
     &        rcd%stormname, rcd%depth
         read( line, '(164x,a2,2x,a3,4(2x,a3))' )
     &        rcd%seas, rcd%seascode, rcd%seasrad(1), rcd%seasrad(2),
     &        rcd%seasrad(3), rcd%seasrad(4)
      endif
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readNextFix
C
C  DESCRIPTION:  Reads the next FIX_RECORD record from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 2004
C
C  USAGE:  call readNextFix (datFile,record,ios)
C
C  INPUT PARAMETERS:
C     datFile - unit number of input data file
C
C  OUTPUT PARAMETERS:
C     rcd - structure to read record into
C     ios - return 0 success, neg for end of file, pos for error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readNextFix (datFile, rcd, ios )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      type (FIX_RECORD)  rcd
      integer            ios
c
c         local variables
      character line*400
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Zero out the dtg, fixtype, lat, lon and vmax
      rcd%DTG = '            '
      rcd%fixtype = '    '
      rcd%latns = '     '
      rcd%lonew = '      '
      rcd%v = '   '
c
c     Read one data record.
      read( datFile, '(a400)', iostat=ios ) line
c     Skip over blank lines.
      do while( ios .eq. 0 .and. len_trim(line) .eq. 0 )
         read( datFile, '(a400)', iostat=ios ) line
      enddo
      if( ios .eq. 0 ) then
c
c     Get the individual fields from the data record.
         read( line, '(a2,2x,a2,2x,a12,2x,a3,2x,a4,2x,a10,2x,a1)' )
     &        rcd%basin, rcd%cyNum, rcd%DTG, rcd%fixformat, rcd%fixtype,
     &        rcd%cicode, rcd%flagged
         read( line, '(48x,a5,2x,a6,2x,a5,2x,a1,2x,a3)' )
     &        rcd%latns, rcd%lonew, rcd%height, rcd%positConf, rcd%v
         read( line, '(78x,a1,2x,a4,2x,a1,2x,a4)' )
     &        rcd%vConf, rcd%pressure, rcd%presConf, rcd%presDeriv
         read( line, '(96x,a3,2x,a4,4(2x,a4),4(2x,a1))' )
     &        rcd%rad, rcd%windcode, rcd%radii(1), rcd%radii(2),
     &        rcd%radii(3), rcd%radii(4), rcd%radMod(1),
     &        rcd%radMod(2), rcd%radMod(3), rcd%radMod(4)
         read( line, '(143x,a1,2x,a3,2x,a3,2x,a1,2x,a5,2x,a3)' )
     &        rcd%radConf, rcd%mrd, rcd%eye, rcd%subregion,
     &        rcd%fixsite, rcd%initials
         read( line, '(171x,a200)' ) rcd%remainder
      endif
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getARecord
C
C  DESCRIPTION:  Reads one record of specified type from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getARecord (datFile,"CARQ",aidRcd,result)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     technique - "CARQ", "WRNG", "JTWC" ...
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getARecord (datFile, technique, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      character*4     technique
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      logical*2   found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     of the type specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If tech name matches specified record type then process.
         if( technique .eq. aidRcd%aRecord(1)%tech ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readARecord
C
C  DESCRIPTION:  Reads one AID_DATA data info from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readARecord (datFile,aidRcd,result)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for unexpected end-of-file,
C              1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readARecord (datFile, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer            ii
      integer            readStat
      type (ATCF_RECORD) recrd
      character          savDTG*10
      character          savtech*4
      logical*2          done
      integer            savtau
      integer            newtau
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 0
c
c     Read the next record in the data file
      call readNext( datFile, recrd, readStat )
c
c     Save the date-time-group and the technique
      if( readStat .eq. 0 ) then
         result = 1
         savDTG = recrd%DTG
         savtech = recrd%tech
      endif
c
c     Read all of the tau's for this DTG and tech
      ii=0
      aidRcd%numrcrds = 0
      done = .false.
      do while (result.eq.1 .and. readStat.eq.0
     &          .and. ii.lt.AidTauMax*AidRadMax .and. .not.done)
         ii = ii + 1
         read( recrd%tau, '(i3)' ) savtau
c
c        Process the A_RECORD
         call processArcd( aidRcd%aRecord(ii), recrd, result )
c        Copy the ATCF_RECORD
         aidRcd%atcfRcd(ii) = recrd
         aidRcd%numrcrds = ii
c
c        Read the next record in the data file
         call readNext( datFile, recrd, readStat )
         read( recrd%tau, '(i3)' ) newtau
c
c        If new dtg or tech or tau is less than previous tau then
c        flag done and backup one record.
         if( readStat.eq.0 .and. (savDTG.ne.recrd%DTG .or.
     &        savtech.ne.recrd%tech .or. newtau.lt.savtau) ) then
            done = .true.
            backspace( datFile )
         endif
      enddo

      if( readStat .gt. 0 ) result = 0
      if( readStat .lt. 0 .and. aidRcd%numrcrds .eq. 0 )
     &     result = readStat
c     GDN - added this, else sometimes get into infinite loop
      if ( readStat .lt. 0 ) result = readStat
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getSingleTAU
C
C  DESCRIPTION:  Gets the aid data for the specified tau from the
C                supplied AID_DATA.  Returns the data for the tau
C                in tauData.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getSingleTAU (aidRcd, 72, tauData, result)
C
C  INPUT PARAMETERS:
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     tauData - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getSingleTAU ( aidRcd, tau, tauData, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd, tauData
      integer         tau
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      tauData%numrcrds = 0
      do while( ii.le.aidRcd%numrcrds )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            tauData%aRecord(jj) = aidRcd%aRecord(ii)
            tauData%atcfRcd(jj) = aidRcd%atcfRcd(ii)
            jj = jj + 1
            tauData%numrcrds = tauData%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidTAU
C
C  DESCRIPTION:  Gets the first A_RECORD record for the specified tau from
C                the supplied AID_DATA.
C                Note: this only get the first RAD (34, 50, 64, 100 kt),
C                for the requested tau.  If all the records for the tau
C                are needed then use getSingleTAU().
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidTAU (aidRcd, 72, aRecord, result)
C
C  INPUT PARAMETERS:
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     aRecord - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidTAU ( aidRcd, tau, aRecord, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd
      integer         tau
      type (A_RECORD) aRecord
      integer         result
c
c         local variables
      integer   ii
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      do while( ii.le.aidRcd%numrcrds .and. .not.found )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            aRecord = aidRcd%aRecord(ii)
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidDTG
C
C  DESCRIPTION:  Gets the first aid data for the specified DTG
C                from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidDTG (datFile, dtg, aidRcd, result)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidDTG ( datFile, dtg, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       dtg
      type (AID_DATA)   aidRcd
      integer           result
c
c         local variables
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG(3:10) ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getTech
C
C  DESCRIPTION:  Gets the data for a specified aid technique
C                from the supplied BIG_AID_DATA structure and
C                returns an AID_DATA structure
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getTech (bigAidRcd, tech, aidRcd, result)
C
C  INPUT PARAMETERS:
C     bigAidRcd - BIG_AID_DATA structure containing all records for a dtg
C     tech - requested obj aid technique
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getTech ( bigAidRcd, tech, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (BIG_AID_DATA) bigAidRcd
      character*4     tech
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tech in the BIG_AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      aidRcd%numrcrds = 0
      do while( ii .le. bigAidRcd%numrcrds )
         if( bigAidRcd%aRecord(ii)%tech .eq. tech ) then
            found = .true.
            aidRcd%aRecord(jj) = bigAidRcd%aRecord(ii)
            aidRcd%atcfRcd(jj) = bigAidRcd%atcfRcd(ii)
            jj = jj + 1
            aidRcd%numrcrds = aidRcd%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getBigAidDTG
C
C  DESCRIPTION:  Gets all the aid data for the specified DTG
C                from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getBigAidDTG (datFile, dtg, bigAidRcd, result)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     bigAidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  Sampson, NRL 8/01 If dtg of file is larger than searched dtg, quit search
C
C........................END PROLOGUE..................................
C
      subroutine getBigAidDTG ( datFile, dtg, bigAidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
c      character*8       dtg
      character*10       dtg
      type (BIG_AID_DATA)   bigAidRcd
      type (AID_DATA)   aidRcd
      integer           readStat
      integer           result
      integer           ii, jj
c
c         local variables
      logical*2 found
      logical*2 dtgmatch
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      readStat = 1
      found = .false.
      dtgmatch = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG ) found = .true.
cx
cx       If dtg is greater than specified dtg then stop and backspace.
         if( result.eq.1 .and. dtg .lt. aidRcd%aRecord(1)%DTG ) then
	     result = 0
             call backspaceFile( datFile, aidRcd%numrcrds )
	     return
         endif
c
      enddo

      if( found ) dtgmatch = .true.
c     If found assign the aidRcd read into bigAidRcd
      if( dtgmatch ) then
         do ii=1, aidRcd%numrcrds
            bigAidRcd%aRecord(ii) = aidRcd%aRecord(ii)
            bigAidRcd%atcfRcd(ii) = aidRcd%atcfRcd(ii)
         enddo
         bigAidRcd%numrcrds = aidRcd%numrcrds
      endif

c     Loop on reading records as long as dtg matches specified dtg
      do while( readStat.eq.1 .and. dtgmatch .and.
     &     bigAidRcd%numrcrds .lt. (BigAidMax*AidTauMax*AidRadMax) )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .ne. aidRcd%aRecord(1)%DTG ) dtgmatch = .false.
c        If matching dtg then assign aidRcd into bigAidRcd
         if( readStat.ne.0 .and. dtgmatch ) then
            jj = bigAidRcd%numrcrds + 1
            do ii=1, aidRcd%numrcrds
               bigAidRcd%aRecord(jj) = aidRcd%aRecord(ii)
               bigAidRcd%atcfRcd(jj) = aidRcd%atcfRcd(ii)
               jj = jj + 1
            enddo
            bigAidRcd%numrcrds = bigAidRcd%numrcrds + aidRcd%numrcrds
         endif
c
      enddo

c     Backup the file to just after the last matching dtg.
      if( found .and. .not. dtgmatch .and. aidRcd%numrcrds .gt. 0 )
     &   call backspaceFile( datFile, aidRcd%numrcrds )
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  backspaceFile
C
C  DESCRIPTION:  Repositions the specified file back "numRcrds" records.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call backspaceFile ( datFile, numRcrds )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the objective aids file
C     numRcrds - number of records to back up
C
C  OUTPUT PARAMETERS:
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine backspaceFile (datFile, numRcrds)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      integer            numRcrds
      integer            ii
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      do ii=1, numRcrds
         backspace datFile
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processArcd
C
C  DESCRIPTION:  Assigns the data for a A_RECORD structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call processArcd ( aidRcd%aRecord(ii), recrd, result )
C
C  INPUT PARAMETERS:
C     atcfRcd - ATCF_RECORD struct containing data
C
C  OUTPUT PARAMETERS:
C     aidRcd - A_RECORD struct to receive data
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processArcd (aidRcd, atcfRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (A_RECORD)    aidRcd
      type (ATCF_RECORD) atcfRcd
      integer            result
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1

      aidRcd%basin = atcfRcd%basin
      read( atcfRcd%cyNum, '(i2)' ) aidRcd%cyNum
      aidRcd%DTG = atcfRcd%DTG
      read( atcfRcd%technum, '(i2)' ) aidRcd%technum
      aidRcd%tech = atcfRcd%tech
      read( atcfRcd%tau, '(i3)' ) aidRcd%tau
      read( atcfRcd%latns, '(f3.1,a1)' ) aidRcd%lat, aidRcd%NS
      read( atcfRcd%lonew, '(f4.1,a1)' ) aidRcd%lon, aidRcd%EW
      read( atcfRcd%vmax, '(i3)' ) aidRcd%vmax
      read( atcfRcd%mslp, '(i4)' ) aidRcd%mslp
      aidRcd%ty = atcfRcd%ty
      read( atcfRcd%rad, '(i3)' ) aidRcd%rad
      aidRcd%windcode = atcfRcd%windcode
      read( atcfRcd%radii(1), '(i4)' ) aidRcd%radii(1)
      read( atcfRcd%radii(2), '(i4)' ) aidRcd%radii(2)
      read( atcfRcd%radii(3), '(i4)' ) aidRcd%radii(3)
      read( atcfRcd%radii(4), '(i4)' ) aidRcd%radii(4)
      read( atcfRcd%radp, '(i4)' ) aidRcd%radp
      read( atcfRcd%rrp, '(i4)' ) aidRcd%rrp
      read( atcfRcd%mrd, '(i3)' ) aidRcd%mrd
      read( atcfRcd%gusts, '(i3)' ) aidRcd%gusts
      read( atcfRcd%eye, '(i3)' ) aidRcd%eye
      read( atcfRcd%subregion, '(a3)' ) aidRcd%subregion
      read( atcfRcd%maxseas, '(i3)' ) aidRcd%maxseas
      aidRcd%initials = atcfRcd%initials
      read( atcfRcd%dir, '(i3)' ) aidRcd%dir
      read( atcfRcd%speed, '(i3)' ) aidRcd%speed
      aidRcd%stormname = atcfRcd%stormname
      aidRcd%depth = atcfRcd%depth
      read( atcfRcd%seas, '(i2)' ) aidRcd%seas
      aidRcd%seascode = atcfRcd%seascode
      read( atcfRcd%seasrad(1), '(i3)' ) aidRcd%seasrad(1)
      read( atcfRcd%seasrad(2), '(i3)' ) aidRcd%seasrad(2)
      read( atcfRcd%seasrad(3), '(i3)' ) aidRcd%seasrad(3)
      read( atcfRcd%seasrad(4), '(i3)' ) aidRcd%seasrad(4)
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convWriteAidData
C
C  DESCRIPTION:  Convert the passed data to an AID_DATA record and write
C                the AID_DATA record to the output data file, NHC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convWriteAidData (datFile,stormID,cdtg,techname,ltlnwnd,
C                                 windcode,radii,result)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C     windcode - array of strings, dimensioned (newnumtau,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the 2nd dimension is the wind intensities: 34, 50, 64 and 100.
C                Values for the windcode are:
C                     AAA - all quadrants or
C                     NEQ-northeast quadrant.
C     radii - array of wind radii values, dimension (newnumtau,4,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               the second dimension is the wind intensity: 34,50,64,100
C               and the third dimension is the 4 possible radii quadrants.
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine convWriteAidData (datFile, stormID, cdtg, techname,
     1     ltlnwnd, windcode, radii, result )

      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
      character*3       windcode(newnumtau,4)
      integer           radii(newnumtau,4,4)
      integer           result
c
c         local variables
      integer           ii
      type (AID_DATA)   aiddata
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
C     Create the AID_DATA record from the individual variables and arrays.
      call convToAiddata( stormID, cdtg, techname, ltlnwnd, windcode,
     &     radii, aiddata )

C     Write the AID_DATA record out to the specified file (datFile).
      call writeAidData( datFile, aiddata, result )
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convToAiddata
C
C  DESCRIPTION:  Creates an AID_DATA record using data passed in.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convToAiddata ( stormID, cdtg, techname, ltlnwnd, windcode,
C                               radii, aiddata )
C
C  INPUT PARAMETERS:
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C     windcode - array of strings, dimensioned (newnumtau,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the 2nd dimension is the wind intensities: 34, 50, 64 and 100.
C                Values for the windcode are:
C                     AAA - all quadrants or
C                     NEQ-northeast quadrant.
C     radii - array of wind radii values, dimension (newnumtau,4,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               the second dimension is the wind intensity: 34,50,64,100
C               and the third dimension is the 4 possible radii quadrants.
C
C
C  OUTPUT PARAMETERS:
C     aiddata - resulting AID_DATA struct
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C  Minor change to records written - don't include 360 longitudes
C  Added due to extra lines written for wind radii cliper  ... sampson 3/24/04
C
C........................END PROLOGUE..................................
C
      subroutine convToAiddata (stormID, cdtg, techname, ltlnwnd,
     &     windcode, radii, aiddata )
c
      implicit none
c
      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
      character*3       windcode(newnumtau,4)
      integer           radii(newnumtau,4,4)
      type (AID_DATA)   aiddata
c
c         local variables
      type (A_RECORD)   rcrd
      type (A_RECORD)   aRcrd
      integer           ii, jj, kk, mm, itau
      integer           loopend
      integer           numrcrd
      integer           llwnd(llw)
      integer           wndrad(4)
      integer           rad(4)
      data rad/34,50,64,100/
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      numrcrd = 1
      loopend = newnumtau

c     For CARQ and WRNG loop on the taus: -24, -18, -12, -6 and 0
      if (techname.eq.'CARQ' .or. techname.eq.'WRNG') loopend = 5

c     For all other aids loop on the taus:
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120

      do ii = 1, loopend

c        Get the tau.
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
c        else all taus in 12 hr increments
         else
            itau = (ii-1) * 12
         endif

c        Get the lat/lon/wind for this tau
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo

c        Check for model runs where lat/lon out of range - convert to 0's
c        Handle cases of forecasts crossing 0 longitude,  ajs 1/17/01
         if( llwnd(2) .lt. 0 ) then
            llwnd(2) = llwnd(2) + 3600
         endif

         if( llwnd(1) .lt. -900  .or. llwnd(1) .gt.  900  .or.
     &        llwnd(2) .lt. 0  .or. llwnd(2) .ge.  3600 ) then
            llwnd(1) = 0
            llwnd(2) = 0
         endif
c        Check for model runs where wind out of range - convert to 0's
         if( llwnd(3) .lt. 0 .or. llwnd(3) .gt. 300)
     &        llwnd(3) = 0

         if( llwnd(1) .ne. 0 .or. llwnd(2) .ne. 0
     1        .or. llwnd(3) .ne. 0) then

            do jj = 1, 4
c              Loop thru the four radii, 34,50,64,100 kt.  Always write
c              out the 34 kt radii.  Write out the others if radii have
c              been defined for them.
               if( jj .eq. 1 .or.
     &              radii(ii,jj,1).gt.0 .or. radii(ii,jj,2).gt.0 .or.
     &              radii(ii,jj,3).gt.0 .or. radii(ii,jj,4).gt.0 ) then
c                 Get the wind radii for this wind intensity.
                  do kk = 1, 4
                     wndrad(kk) = radii(ii,jj,kk)
                  enddo

                  call convToARecord( stormID, cdtg, itau, techname,
     &                 llwnd, rad(jj), windcode(ii,jj), wndrad, aRcrd )

                  aiddata%aRecord(numrcrd) = aRcrd
                  aiddata%numrcrds = numrcrd
                  numrcrd = numrcrd + 1

               endif            ! if 34 kt radii .or. radii .gt. 0
            enddo               ! do jj = 1, 4
         endif                  ! if llwnd(1) .or. llwnd(20 .or. llwnd(3)
      enddo                     ! do ii = 1, loopend
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convToARecord
C
C  DESCRIPTION:  Creates an A_RECORD record using data passed in.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convToARecord ( stormID, cdtg, techname, llwnd, rad,
C                               windcode, wndrad, aRcrd )
C
C  INPUT PARAMETERS:
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     llwnd - array of integers dimensioned (llw) where
C               the llw dimension is lat, lon and wind
C     rad - specifies which wind radii, 34, 50, 64 or 100
C     windcode - wind radii code string
C                Values for the windcode are:
C                     AAA - all quadrants or
C                     NEQ-northeast quadrant.
C     wndrad - array of wind radii values, dimension (4)
C               representing the 4 possible radii quadrants.
C
C
C  OUTPUT PARAMETERS:
C     aRcrd - resulting A_RECORD struct
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C  Added initialization as per Colin McCadie, NHC ... sampson Apr 04
C........................END PROLOGUE..................................
C
      subroutine convToARecord (stormID, cdtg, itau, techname, llwnd,
     &     rad, windcode, wndrad, aRcrd )
c
      implicit none
c
      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*8       stormID
      character*10      cdtg
      integer           itau
      character*4       techname
      integer           llwnd(llw)
      integer           rad
      character*3       windcode
      integer           wndrad(4)
      type (A_RECORD)   aRcrd
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

c     Assign the data to the A_RECORD
      aRcrd%basin = stormID(1:2)
      call upcase( aRcrd%basin, 2 )
      read( stormID(3:4), '(i2)' ) aRcrd%cyNum
      aRcrd%DTG = cdtg
      if( techname .eq. 'CARQ' ) then
         aRcrd%technum = 1
      else if( techname .eq. 'WRNG' ) then
         aRcrd%technum = 2
      else
         aRcrd%technum = 3
      endif
      aRcrd%tech = techname
      aRcrd%tau = itau

c     Convert from -900 thru 900 to 90.0S thru 90.0N
      aRcrd%NS = 'N'
      aRcrd%lat = llwnd(1) / 10.
      if( aRcrd%lat .lt. 0. ) then
         aRcrd%lat = -aRcrd%lat
         aRcrd%NS = 'S'
      endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 180.0W thru 180.0E
      aRcrd%EW = 'W'
      aRcrd%lon = llwnd(2) / 10.
      if( aRcrd%lon .gt. 180.0 ) then
         aRcrd%lon = 360.0 - aRcrd%lon
         aRcrd%EW = 'E'
      endif
      aRcrd%vmax = llwnd(3)

      aRcrd%mslp = 0
      aRcrd%ty = " "

c     Assign the wind radii values.
      aRcrd%rad = rad;
      aRcrd%windcode = windcode
      do ii = 1, 4
         aRcrd%radii(ii) = wndrad(ii)
      enddo

      aRcrd%subregion = " "
      aRcrd%initials = " "
      aRcrd%stormname = " "
      aRcrd%depth = " "
      aRcrd%seascode = " "
C
cx
cx from Colin McCadie = had to explicitly set these variables to zero (hp compiler)
      aRcrd%radp = 0
      aRcrd%rrp  = 0
      aRcrd%mrd  = 0
      aRcrd%gusts= 0
      aRcrd%eye  = 0

      aRcrd%maxseas = 0
      aRcrd%dir     = 0
      aRcrd%speed   = 0
      aRcrd%seas    = 0
      aRcrd%seasrad = 0

      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAidData
C
C  DESCRIPTION:  Writes an AID_DATA record to the specified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call writeAidData ( datFile, aiddata, result )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the objective aids file
C     aiddata - AID_DATA struct containing data to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeAidData (datFile, aiddata, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (AID_DATA)   aiddata
      integer           result
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
C
      do ii = 1, aiddata%numrcrds
         call outputArcd( datFile, aiddata%aRecord(ii), result )
      enddo
C
      END



C......................................................................
c
c     This function was added because the NCEP IBM mainframe compiler
c     wouldn't allow testing integers as logicals.   03/2004  A. Schrader
c

      logical function nonzero (ii)
c
      integer ii
c
      if( ii .ne. 0 ) then
         nonzero = .TRUE.
      else
         nonzero = .FALSE.
      endif

      end
C......................................................................


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputArcd
C
C  DESCRIPTION:  Writes the A_RECORD to the spectified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call outputArcd ( datFile, aidRcd%aRecord(ii), result )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the objective aids file
C     aidRcd - A_RECORD struct containing record to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputArcd (datFile, aidRcd, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (A_RECORD)   aidRcd
      integer           result
c
c         local variables
      integer           ii
      character*200     line
      character*50      temp
      integer           writeline
      logical           nonzero
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Build the output objective aid line. */
c
c     Start with the basin, cyclone number and date-time-group.
      write( line,8000 ) aidRcd%basin, aidRcd%cyNum, aidRcd%DTG
 8000 format( a2,", ",i2.2,", ",a10,',')

c     Add on the tech number, tech name and tau.
      if( aidRcd%tech .eq. "BEST" ) then
         write( temp, 8010 ) aidRcd%tech, aidRcd%tau
 8010    format( ' ','  , ',a4,", ",i3,',')
      else
         write( temp,8020 ) aidRcd%technum, aidRcd%tech, aidRcd%tau
 8020    format( ' ',i2.2,", ",a4,", ",i3,',')
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the lat, lon and vmax.
      write( temp,8030 ) NINT(aidRcd%lat*10.), aidRcd%NS,
     &     NINT(aidRcd%lon*10.), aidRcd%EW, aidRcd%vmax
 8030 format( ' ',i3,a1,", ",i4,a1,", ",i3,',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the mslp and ty.
      write( temp,8040 ) aidRcd%mslp, aidRcd%ty
 8040 format( ' ',i4,", ",a2,',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the rad, windcode and radii data.
      write( temp,8050 ) aidRcd%rad, aidRcd%windcode,
     &     (aidRcd%radii(ii),ii=1,4)
 8050 format( ' ',i3,", ",a3,4(", ",i4),',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

      writeline = 1
c     Add on the last closed isobar(radp), it's radius(rrp),
c     radius of max winds(mrd), gusts and eye diameter.
      if( nonzero(aidRcd%radp) .or. nonzero(aidRcd%rrp) .or.
     &     nonzero(aidRcd%mrd) .or. nonzero(aidRcd%gusts) .or.
     &     nonzero(aidRcd%eye) .or. nonzero(aidRcd%maxseas) .or.
     &     nonzero(aidRcd%dir) .or. nonzero(aidRcd%speed) .or.
     &     nonzero(LEN_TRIM(aidRcd%subregion)) .or.
     &     nonzero(LEN_TRIM(aidRcd%initials)) .or.
     &     nonzero(LEN_TRIM(aidRcd%stormname)) .or.
     &     nonzero(LEN(aidRcd%depth)) .or.
     &     nonzero(aidRcd%seasrad(1)) .or.
     &     nonzero(aidRcd%seasrad(2)) .or.
     &     nonzero(aidRcd%seasrad(3)) .or.
     &     nonzero(aidRcd%seasrad(4)) ) then
         write( temp,8060 ) aidRcd%radp, aidRcd%rrp, aidRcd%mrd,
     &        aidRcd%gusts, aidRcd%eye
 8060    format( ' ',i4,", ",i4,", ",i3,", ",i3,", ",i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

      if( nonzero(aidRcd%maxseas) .or. nonzero(aidRcd%dir) .or.
     &     nonzero(aidRcd%speed) .or.
     &     nonzero(LEN_TRIM(aidRcd%initials)) .or.
     &     nonzero(LEN_TRIM(aidRcd%subregion)) .or.
     &     nonzero(LEN_TRIM(aidRcd%stormname)) .or.
     &     nonzero(LEN(aidRcd%depth)) .or.
     &     nonzero(aidRcd%seasrad(1)) .or.
     &     nonzero(aidRcd%seasrad(2)) .or.
     &     nonzero(aidRcd%seasrad(3)) .or.
     &     nonzero(aidRcd%seasrad(4)) ) then
c        Add the subregion.
         write( temp,8070 ) aidRcd%subregion
 8070    format( ' ',a3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the max seas.
         write( temp,8080 ) aidRcd%maxseas
 8080    format( ' ',i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the initials.
         write( temp,8090 ) aidRcd%initials
 8090    format( ' ',a3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the dir and speed if available.
         write( temp,9000 ) aidRcd%dir, aidRcd%speed
 9000    format( ' ',i3,", ",i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Add the stormname and system depth.
      if( nonzero(LEN_TRIM(aidRcd%stormname)) .or.
     &    nonzero(LEN(aidRcd%depth)) .or.
     &    nonzero(aidRcd%seasrad(1)) .or.
     &    nonzero(aidRcd%seasrad(2)) .or.
     &    nonzero(aidRcd%seasrad(3)) .or.
     &    nonzero(aidRcd%seasrad(4)) ) then
         write( temp,9010 ) aidRcd%stormname, aidRcd%depth
 9010    format( ' ',a10,", ",a1,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Add on the seas, seascode and seas radii data.
      if( nonzero(aidRcd%seasrad(1)) .or.
     &    nonzero(aidRcd%seasrad(2)) .or.
     &    nonzero(aidRcd%seasrad(3)) .or.
     &    nonzero(aidRcd%seasrad(4)) ) then
         write( temp,9020 ) aidRcd%seas, aidRcd%seascode,
     &        (aidRcd%seasrad(ii),ii=1,4)
 9020    format( ' ',i2,", ",a3,", ",i3,", ",i3,", ",i3,", ",i3)
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Write the line out to the data file.
c     Write with different format statements because I didn't want to
c     have to add a compile switch to get variable-format expressions.
c     And I didn't want every line to be 200 characters in length. ajs
      if( writeline .eq. 1 ) then
         write( datFile,'( a100 )') line
      elseif (writeline .eq. 2 ) then
         write( datFile,'( a125 )') line
      elseif (writeline .eq. 3 ) then
         write( datFile,'( a150 )') line
      elseif (writeline .eq. 4 ) then
         write( datFile,'( a165 )') line
      elseif (writeline .eq. 5 ) then
         write( datFile,'( a195 )') line
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doWriteErrRadRcd
C
C  DESCRIPTION:  Write the error radius record to the output data file.
C
C  PROGRAMMER, DATE:  Jim Goerss   (NRL)  March 2004
C
C  USAGE:  call doWriteErrRadRcd (datFile,stormID,cdtg,techname,itau,llrad)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     itau - forecast period
C     llrad - array of integers dimensioned (3) where
C             and the three components are lat, lon and error radius
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doWriteErrRadRcd (datFile, stormID, cdtg, techname,
     1     itau, llrad )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           itau
      integer           llrad(llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif

cx    Check for model runs where lat/lon out of range - convert to 0's
      if( llrad(2) .lt. 0 ) then
          llrad(2) = llrad(2) + 3600
      endif

      if( llrad(1) .lt. -900  .or.
     1    llrad(1) .gt.  900  .or.
     2    llrad(2) .lt.    0  .or.
     3    llrad(2) .gt.  3600 ) then
         llrad(1) = 0
         llrad(2) = 0
      endif
cx    Check for model runs where error radius is out of range - convert to 0's
      if( llrad(3) .lt. 0 .or. llrad(3) .gt. 9999)
     1     llrad(3) = 0

      if( llrad(1) .ne. 0 .or. llrad(2) .ne. 0
     1     .or. llrad(3) .ne. 0) then
c     Convert from -900 thru 900 to 900S thru 900N
         ns = 'N'
         ilat = llrad(1)
         if( ilat .lt. 0 ) then
            ilat = -ilat
            ns = 'S'
         endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
         ew = 'W'
         ilon = llrad(2)
         if( ilon .gt. 1800 ) then
            ilon = 3600 - ilon
            ew = 'E'
         endif
c     Write the aid record...
cx    only if the lat and lon are meaningful
         if ( ilon .le. 1800 .and. ilat .lt. 900 ) then
            write(datFile,9080) basin, stormnum, cdtg,
     1           technum, techname, itau, ilat, ns, ilon, ew,
     1           llrad(3)
 9080       format( A2,", ",A2,", ",A10,", ",A2,", ",A4,", ",
     1           I3,", ",I3,A1,", ",I4,A1,',  , ',I6 )
         endif
      endif

C
      END

C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  newWriteErrRadRcd
C
C  DESCRIPTION:  Write the error radius record to the output data file,
C                NHC version
C
C  PROGRAMMER, DATE:  Jim Goerss   (NRL)  March 2004
C
C  USAGE:  call newWriteErrRadRcd (datFile,stormID,cdtg,techname,ltlnrad)
C
C  INPUT PARAMETERS:
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnrad - array of integers dimensioned (newnumtau,llw) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and error radius
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine newWriteErrRadRcd (datFile, stormID, cdtg, techname,
     1     ltlnrad )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnrad(newnumtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llrad(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = newnumtau

c     For all aids loop on the taus:
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120
      do ii = 1, loopend
	 itau = (ii-1) * 12
         do jj = 1, llw
            llrad(jj) = ltlnrad(ii,jj)
         enddo
         call doWriteErrRadRcd(datFile, stormID, cdtg, techname, itau,
     &                      llrad )
      enddo
C
      END
C


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readFRecord
C
C  DESCRIPTION:  Reads one FIX_DATA data info from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call readFRecord ( datFile, fixData, result )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the fixes file
C
C  OUTPUT PARAMETERS:
C     fixData - structure to read data into
C     result - return 0 for fail, neg for unexpected end-of-file
C              1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readFRecord (datFile, fixData, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      type (FIX_DATA) fixData
      integer         result
c
c         local variables
      integer            ii
      integer            readStat
      type (FIX_RECORD)  recrd
      character          savDTG*12
      character          savfixtype*4
      logical*2          done
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 0
c
c     Read the next record in the data file
      call readNextFix( datFile, recrd, readStat )
c
c     Save the date-time-group and the fixtype
      if( readStat .eq. 0 ) then
         result = 1
         savDTG = recrd%DTG
         savfixtype = recrd%fixtype
      endif
c
c     Read all of the radii for this DTG and fixtype
      ii=0
      fixData%numrcrds = 0
      done = .false.
      do while (result.eq.1 .and. readStat.eq.0
     &          .and. ii.lt.AidRadMax .and. .not.done)
         ii = ii + 1
c
c        Process the F_RECORD
         call processFrcd( fixData%fRecord(ii), recrd, result )
         fixData%numrcrds = ii
c
c        Read the next record in the data file
         call readNextFix( datFile, recrd, readStat )
c
c        If new dtg or fixtype or rad is not 50, 64 or 100 then
c        flag done and backup one record.
         if( readStat.eq.0 .and. (savDTG.ne.recrd%DTG .or.
     &        savfixtype.ne.recrd%fixtype .or. recrd%rad.eq.'   '
     &        .or. recrd%rad.eq.' 34') ) then
            done = .true.
            backspace( datFile )
         endif
      enddo

      if( readStat .gt. 0 ) result = 0
      if( readStat .lt. 0 .and. fixData%numrcrds .eq. 0 )
     &     result = readStat
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processFrcd
C
C  DESCRIPTION:  Assigns the data for an F_RECORD structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processFrcd ( fixData%fRecord(ii), recrd, result )
C
C  INPUT PARAMETERS:
C     recrd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     fixRcd - F_RECORD struct to receive data
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processFrcd (fixRcd, recrd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)    fixRcd
      type (FIX_RECORD)  recrd
      integer            result
c
c         local variables
      integer   ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1

      fixRcd%basin = recrd%basin
      read( recrd%cyNum, '(i2)' ) fixRcd%cyNum
      fixRcd%DTG = recrd%DTG
      read( recrd%fixformat, '(i3)' ) fixRcd%fixformat
      fixRcd%fixtype = recrd%fixtype
      fixRcd%centerFix = .false.
      fixRcd%intensityFix = .false.
      fixRcd%radiiFix = .false.
      fixRcd%pressureFix = .false.
      do ii = 1, LEN( recrd%cicode )
         if( recrd%cicode(ii:ii) .eq. 'C' ) then
            fixRcd%centerFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'I' ) then
            fixRcd%intensityFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'R' ) then
            fixRcd%radiiFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'P' ) then
            fixRcd%pressureFix = .true.
         endif
      enddo
      fixRcd%flagged = recrd%flagged
      read( recrd%latns, '(f4.2,a1)' ) fixRcd%lat, fixRcd%NS
      read( recrd%lonew, '(f5.2,a1)' ) fixRcd%lon, fixRcd%EW
      read( recrd%height, '(i5)' ) fixRcd%height
      read( recrd%positConf, '(i1)' ) fixRcd%positConf
      read( recrd%v, '(i3)' ) fixRcd%v
      read( recrd%vConf, '(i1)' ) fixRcd%vConf
      read( recrd%pressure, '(i4)' ) fixRcd%pressure
      read( recrd%presConf, '(i1)' ) fixRcd%presConf
      fixRcd%presDeriv = recrd%presDeriv
      read( recrd%rad, '(i3)' ) fixRcd%rad
      fixRcd%windcode = recrd%windcode
      read( recrd%radii(1), '(i4)' ) fixRcd%radii(1)
      read( recrd%radii(2), '(i4)' ) fixRcd%radii(2)
      read( recrd%radii(3), '(i4)' ) fixRcd%radii(3)
      read( recrd%radii(4), '(i4)' ) fixRcd%radii(4)
      do ii = 1, 4
         fixRcd%edge(ii) = .false.
         fixRcd%cut(ii) = .false.
      enddo
      do ii = 1, 4
         if( recrd%radMod(ii) .eq. 'E' ) then
            fixRcd%edge(ii) = .true.
         else if( recrd%radMod(ii) .eq. 'C' ) then
            fixRcd%cut(ii) = .true.
         else if( recrd%radMod(ii) .eq. 'B' ) then
            fixRcd%edge(ii) = .true.
            fixRcd%cut(ii) = .true.
         endif
      enddo
      read( recrd%radConf, '(i1)' ) fixRcd%radConf
      read( recrd%mrd, '(i3)' ) fixRcd%mrd
      read( recrd%eye, '(i3)' ) fixRcd%eye
      fixRcd%subregion = recrd%subregion
      fixRcd%fixsite = recrd%fixsite
      fixRcd%initials = recrd%initials
c
c      print*, 'basin = ',fixRcd%basin
c      print*, 'DTG = ',fixRcd%DTG
c      print*, 'fixformat = ',fixRcd%fixformat
c      print*, 'fixtype = ',fixRcd%fixtype
c      print*, 'centerFix = ',fixRcd%centerFix
c      print*, 'intensityFix = ',fixRcd%intensityFix
c      print*, 'radiiFix = ',fixRcd%radiiFix
c      print*, 'pressureFix = ',fixRcd%pressureFix
c      print*, 'flagged = ',fixRcd%flagged
c      print*, 'lat NS = ',fixRcd%lat, fixRcd%NS
c      print*, 'lon EW = ',fixRcd%lon, fixRcd%EW
c      print*, 'height = ',fixRcd%height
c      print*, 'positConf = ',fixRcd%positConf
c      print*, 'v, vConf = ',fixRcd%v, fixRcd%vConf
c      print*, 'pressure = ',fixRcd%pressure
c      print*, 'presConf = ',fixRcd%presConf
c      print*, 'presDeriv = ',fixRcd%presDeriv
c      print*, 'rad = ',fixRcd%rad
c      print*, 'radii = ',(fixRcd%radii(ii),ii=1,4)
c      print*, 'edge = ',(fixRcd%edge(ii),ii=1,4)
c      print*, 'cut = ',(fixRcd%cut(ii),ii=1,4)
c      print*, 'radConf = ',fixRcd%radConf
c      print*, 'mrd = ',fixRcd%mrd
c      print*, 'eye = ',fixRcd%eye
c      print*, 'subregion = ',fixRcd%subregion
c      print*, 'fixsite = ',fixRcd%fixsite
c      print*, 'initials = ',fixRcd%initials
c
c        Process the rest of the record
      if( fixRcd%fixformat .eq. DVTStype ) then
         call processDVTS( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. DVTOtype ) then
         call processDVTO( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. MICRtype .or.
     &        fixRcd%fixformat .eq. SCATtype ) then
         call processMicrowave( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. RDRtype ) then
         call processRadar( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. AIRCtype ) then
         call processAircraft( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. DROPtype ) then
         call processDropsonde( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. ANALtype ) then
         call processAnalysis( fixRcd, recrd )
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDvorak
C
C  DESCRIPTION:  Assigns the data for a DVORAK fix structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDvorak( dPtr, fixPtr, column,  option )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C     col - column position of fix type specific portion of
C           fix record
C     option - 'short-term' or 'long-term'
C
C  OUTPUT PARAMETERS:
C     dPtr - DVORAK structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDvorak (dvrk, fixRcd, col, option )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (DVORAK)     dvrk
      type (FIX_RECORD) fixRcd
      integer           col
      character         option*10
c
c         local variables
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Don't process the t-number, ci number or intensity change for
c     the short term trend dvorak.

      if( option(1:9) .eq. 'long-term' ) then
         dvrk%tnum = 0
         dvrk%cinum = 0
         if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &        fixRcd%remainder(col:col+1) .ne. '  ' ) then
            read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%tnum
         endif
         col = col + 2
         if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &        fixRcd%remainder(col:col+1) .ne. '  ' ) then
            read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%cinum
         endif
         col = col + 2
         dvrk%intChg = fixRcd%remainder(col:col)
         col = col + 1
      endif
      dvrk%tnumChg = 0
      dvrk%lastEvalHrsAgo = 0
      dvrk%pastChg = fixRcd%remainder(col:col)
      col = col + 1
      if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &     fixRcd%remainder(col:col+1) .ne. '  ' ) then
         read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%tnumChg
      endif
      col = col + 2
      if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &     fixRcd%remainder(col:col+1) .ne. '  ' ) then
         read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%lastEvalHrsAgo
      endif
      col = col + 2
c     add 2 for the comma and space
      col = col + 2

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDVTS
C
C  DESCRIPTION:  Assigns the data for a subjective dvorak fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDVTS( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDVTS (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%dvts%sensor = fixRcd%remainder(col:col+3)
      col = col + 6
      read( fixRcd%remainder(col:col), '(i1)' ) f_rcrd%dvts%pcn
      col = col + 3
      call processDvorak( f_rcrd%dvts%longTerm, fixRcd, col,
     &     'long-term' )
      call processDvorak( f_rcrd%dvts%shortTerm, fixRcd, col,
     &     'short-term' )
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvts%ci24hr
      col = col + 4
      f_rcrd%dvts%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      f_rcrd%dvts%centertype = fixRcd%remainder(col:col+3)
      col = col + 6
      f_rcrd%dvts%tropical = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%dvts%comments = fixRcd%remainder(col:col+51)

c      print*,'sensor = ',f_rcrd%dvts%sensor
c      print*,'pcn = ',f_rcrd%dvts%pcn
c      print*,'longterm dvk = ',f_rcrd%dvts%longTerm
c      print*,'shortterm dvk = ',f_rcrd%dvts%shortTerm
c      print*,'ci24hr = ',f_rcrd%dvts%ci24hr
c      print*,'satType = ',f_rcrd%dvts%satType
c      print*,'centertype = ',f_rcrd%dvts%centertype
c      print*,'tropical = ',f_rcrd%dvts%tropical
c      print*,'comments = ',f_rcrd%dvts%comments
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDVTO
C
C  DESCRIPTION:  Assigns the data for an objective dvorak fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDVTO( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDVTO (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%dvto%sensor = fixRcd%remainder(col:col+3)
      col = col + 6
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%cinum
      col = col + 4
      read( fixRcd%remainder(col:col), '(i1)' ) f_rcrd%dvto%ciConf
      col = col + 3
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%tnumAvg
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%dvto%tnumAvgTime
      col = col + 5
      f_rcrd%dvto%tnumAvgDeriv = fixRcd%remainder(col:col)
      col = col + 3
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%tnumRaw
      col = col + 4
c     If no eyeTemp assign -999 to signify none
      if( fixRcd%remainder(col:col+3) .eq. '    ' ) then
         f_rcrd%dvto%eyeTemp = -999
      else
         read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%dvto%eyeTemp
      endif
      col = col + 6
c     If no cloudTemp assign -999 to signify none
      if( fixRcd%remainder(col:col+3) .eq. '    ' ) then
         f_rcrd%dvto%cloudTemp = -999
      else
         read( fixRcd%remainder(col:col+3), '(i4)' )
     &        f_rcrd%dvto%cloudTemp
      endif
      col = col + 6
      f_rcrd%dvto%sceneType = fixRcd%remainder(col:col+3)
      col = col + 6
      f_rcrd%dvto%algorithm = fixRcd%remainder(col:col+1)
      col = col + 4
      f_rcrd%dvto%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      f_rcrd%dvto%tropical = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%dvto%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processMicrowave
C
C  DESCRIPTION:  Assigns the data for a microwave fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processMicrowave( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processMicrowave (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%micro%rain = .false.
      if( fixRcd%remainder(col:col) .eq. 'R' )
     &     f_rcrd%micro%rain = .true.
      col = col + 3
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%micro%rainrate
      col = col + 5
      f_rcrd%micro%algorithm = fixRcd%remainder(col:col+5)
      col = col + 8
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%micro%wave
      col = col + 4
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%micro%temp
      col = col + 6
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%micro%slpraw
      col = col + 6
      read( fixRcd%remainder(col:col+3), '(i4)' )
     &     f_rcrd%micro%slpretr
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%micro%seas
      col = col + 5
      f_rcrd%micro%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%micro%rad
      col = col + 5
      f_rcrd%micro%windcode = fixRcd%remainder(col:col+3)
      col = col + 6
c     call RemoveSpaces() ?????   ajs
      do ii=1, 8
         read( fixRcd%remainder(col:col+3), '(i4)' )
     &        f_rcrd%micro%radii(ii)
         col = col + 6
      enddo
      do ii=1, 8
         f_rcrd%micro%edge = .false.
         f_rcrd%micro%cut = .false.
      enddo
      do ii=1, 8
         if( fixRcd%remainder(col:col) .eq. 'E' ) then
            f_rcrd%micro%edge(ii) = .true.
         else if( fixRcd%remainder(col:col) .eq. 'C' ) then
            f_rcrd%micro%cut(ii) = .true.
         else if( fixRcd%remainder(col:col) .eq. 'B' ) then
            f_rcrd%micro%edge(ii) = .true.
            f_rcrd%micro%cut(ii) = .true.
         endif
         col = col + 3
      enddo
      read( fixRcd%remainder(col:col), '(i1)' )
     &     f_rcrd%micro%radConf
      col = col + 3
      f_rcrd%micro%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processRadar
C
C  DESCRIPTION:  Assigns the data for a radar fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processRadar( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processRadar (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%radar%rdrtype = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%radar%plainlanguage = .false.
      f_rcrd%radar%radobcode = .false.
      f_rcrd%radar%doppler = .false.
      if( fixRcd%remainder(col:col) .eq. 'P' ) then
         f_rcrd%radar%plainlanguage = .true.
      elseif( fixRcd%remainder(col:col) .eq. 'R' ) then
         f_rcrd%radar%radobcode = .true.
      elseif( fixRcd%remainder(col:col) .eq. 'D' ) then
         f_rcrd%radar%doppler = .true.
      endif
      col = col + 3
      f_rcrd%radar%radob = fixRcd%remainder(col:col+9)
      col = col + 12
      f_rcrd%radar%eyeShape = fixRcd%remainder(col:col+1)
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%radar%eyeWallOb
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%radar%spiralOv
      col = col + 4
      read( fixRcd%remainder(col:col+4), '(f4.2,a1)' )
     &     f_rcrd%radar%lat, f_rcrd%radar%NS
      col = col + 7
      read( fixRcd%remainder(col:col+5), '(f5.2,a1)' )
     &     f_rcrd%radar%lon, f_rcrd%radar%EW
      col = col + 8
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%radar%vmaxin
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%azimuthin
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%rangein
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(i5)' ) f_rcrd%radar%elevin
      col = col + 7
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%vmaxout
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%azimuthout
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%rangeout
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(i5)' )
     &     f_rcrd%radar%elevout
      col = col + 7
      read( fixRcd%remainder(col:col+4), '(i5)' )
     &     f_rcrd%radar%cloudHeight
      col = col + 7
      read( fixRcd%remainder(col:col+4), '(f5.2)' )
     &     f_rcrd%radar%rainAccum
      col = col + 7
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%radar%rainAcTimeInt
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(f4.2,a1)' )
     &     f_rcrd%radar%rainlat, f_rcrd%radar%rainNS
      col = col + 7
      read( fixRcd%remainder(col:col+5), '(f5.2,a1)' )
     &     f_rcrd%radar%rainlon, f_rcrd%radar%rainEW
      col = col + 8
      f_rcrd%radar%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processAircraft
C
C  DESCRIPTION:  Assigns the data for an aircraft fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processAircraft( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processAircraft (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%FlightLevelFt
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%FlightLevelMB
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' )
     &     f_rcrd%air%MinHeight
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxSurfaceWind
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxSurfaceWindBearing
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxSurfaceWindRange
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxFlightLevelWindDir
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxFlightLevelWindInt
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxFlightLevelWindBearing
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%MaxFlightLevelWindRange
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' )
     &     f_rcrd%air%MinSeaLevelPressure
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%EyeTempOutside
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%EyeTempInside
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%DewPoint
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%SeaSurface
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%WallCloudThickness
      col = col + 4
      f_rcrd%air%EyeShape = 0
      if( fixRcd%remainder(col:col+1) .eq. 'CI' ) then
         f_rcrd%air%EyeShape = 1
      elseif( fixRcd%remainder(col:col+1) .eq. 'EL' ) then
         f_rcrd%air%EyeShape = 2
      elseif( fixRcd%remainder(col:col+1) .eq. 'CO' ) then
         f_rcrd%air%EyeShape = 3
      endif
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%air%EyeOrientation
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%DiameterLongAxis
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%DiameterShortAxis
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(f3.1)' )
     &     f_rcrd%air%NavigationalAccuracy
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(f3.1)' )
     &     f_rcrd%air%NavigationalMeteorological
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' )
     &     f_rcrd%air%MissionNumber
      col = col + 4
      f_rcrd%air%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDropsonde
C
C  DESCRIPTION:  Assigns the data for a dropsonde fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDropsonde( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDropsonde (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%drop%sondeEnv = fixRcd%remainder(col:col+9)
      col = col + 12
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%drop%height150
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%drop%vspd150
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%drop%vspd500
      col = col + 5
      f_rcrd%drop%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processAnalysis
C
C  DESCRIPTION:  Assigns the data for an analysis fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processAnalysis( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processAnalysis (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%anal%initials = fixRcd%remainder(col:col+2)
      col = col + 5
      f_rcrd%anal%startDTG = fixRcd%remainder(col:col+11)
      col = col + 14
      f_rcrd%anal%endDTG = fixRcd%remainder(col:col+11)
      col = col + 14
      read( fixRcd%remainder(col:col+2), '(i3)' )
     &     f_rcrd%anal%DistanceToNearestData
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%anal%SST
      col = col + 6
      f_rcrd%anal%obSources = fixRcd%remainder(col:col+23)
      col = col + 26
      f_rcrd%anal%comments = fixRcd%remainder(col:col+51)

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  buildDvorak
C
C  DESCRIPTION:  Builds a long or short term trend dvorak code.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call buildDvorak ( fixRcd%dvts, 'long', dvorakStr )
C
C  INPUT PARAMETERS:
C     fix - DVTS_RECORD containing the dvts data
C     option - 'long' or 'short'
C
C  OUTPUT PARAMETERS:
C     dvorakStr - returned dvorak code
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine buildDvorak (fix, option, dvorakStr)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (DVTS_RECORD) fix
      character*5        option
      character*10       dvorakStr
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

c     Initialize dvorak
      dvorakStr = ' '

      if( option(1:4) .eq. 'long' ) then
         if( fix%longTerm%tnum .ne. 0 .or.
     &        fix%longTerm%cinum .ne. 0 .or.
     &        fix%longTerm%intChg .eq. '+' .or.
     &        fix%longTerm%intChg .eq. '-' ) then
            if( fix%longTerm%tnumChg .ne. 0 .or.
     &           fix%longTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '(i2.2,i2.2,a1,a1,i2.2,i2.2)' )
     &              fix%longTerm%tnum, fix%longTerm%cinum,
     &              fix%longTerm%intChg, fix%longTerm%pastChg,
     &              fix%longTerm%tnumChg, fix%longTerm%lastEvalHrsAgo
            else
               write( dvorakStr, '(i2.2,i2.2,a1,"/////")' )
     &              fix%longTerm%tnum, fix%longTerm%cinum,
     &              fix%longTerm%intChg
            endif
         else
            if( fix%longTerm%tnumChg .ne. 0 .or.
     &           fix%longTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '("     ",a1,i2.2,i2.2)' )
     &              fix%longTerm%pastChg, fix%longTerm%tnumChg,
     &              fix%longTerm%lastEvalHrsAgo
            endif
         endif
      elseif( option(1:5) .eq. 'short' ) then
         if( fix%longTerm%tnum .ne. 0 .or.
     &        fix%longTerm%cinum .ne. 0 .or.
     &        fix%longTerm%intChg .eq. '+' .or.
     &        fix%longTerm%intChg .eq. '-' ) then
            if( fix%shortTerm%tnumChg .ne. 0 .or.
     &           fix%shortTerm%lastEvalHrsAgo .ne. 0) then
               write( dvorakStr, '(i2.2,i2.2,a1,a1,i2.2,i2.2)' )
     &              fix%shortTerm%tnum, fix%shortTerm%cinum,
     &              fix%shortTerm%intChg, fix%shortTerm%pastChg,
     &              fix%shortTerm%tnumChg, fix%shortTerm%lastEvalHrsAgo
            else
               write( dvorakStr, '(i2.2,i2.2,a1,"/////")' )
     &              fix%shortTerm%tnum, fix%shortTerm%cinum,
     &              fix%shortTerm%intChg
            endif
         else
            if( fix%shortTerm%tnumChg .ne. 0 .or.
     &           fix%shortTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '("     ",a1,i2.2,i2.2)' )
     &              fix%shortTerm%pastChg, fix%shortTerm%tnumChg,
     &              fix%shortTerm%lastEvalHrsAgo
            endif
         endif
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDVTO
C
C  DESCRIPTION:  Build the objective dvorak section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDVTO ( lineEnd, fixRcd%dvto )
C
C  INPUT PARAMETERS:
C     fixRcd - DVTO_RECORD containing the dvto data being written out
C
C  OUTPUT PARAMETERS:
C     lineEnd - the DVTO specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDVTO (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DVTO_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the objective dvorak section of the output fix line.
c
c     Start with the sensor.
      write( lineEnd, '(" ",a4,",")' )  ADJUSTR(fix%sensor)

c     Add the CI number and confidence.
      if( fix%cinum .ne. 0 ) then
         write( temp, '(" ",i2,", ",i1,",")' )  fix%cinum, fix%ciConf
      else
         write( temp, '("   ,  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the t-number average, t-number averaging time period and
c     t-number averaging derivation
      if( fix%tnumAvg .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%tnumAvg
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      if( fix%tnumAvgTime .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fix%tnumAvgTime
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      write( temp, '(" ",a1,",")' )  fix%tnumAvgDeriv
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the t-number (raw)
      if( fix%tnumRaw .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%tnumRaw
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the eye temperature
      if( fix%eyeTemp .ne. -999 ) then
         write( temp, '(" ",i4,",")' )  fix%eyeTemp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the cloud temp,
      if( fix%cloudTemp .ne. -999 ) then
         write( temp, '(" ",i4,",")' )  fix%cloudTemp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the scene type and algorithm
      write( temp, '(" ",a4,", ",a2,",")' )
     &     ADJUSTR(fix%sceneType), ADJUSTR(fix%algorithm)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add on the satellite type, tropical indicator and comments.
      write( temp, '(" ",a6,", ",a1,", ",a)' )
     &     ADJUSTR(fix%satType), fix%tropical, fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDVTS
C
C  DESCRIPTION:  Build the subjective dvorak section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDVTS ( lineEnd, fixRcd%dvts )
C
C  INPUT PARAMETERS:
C     fixRcd - DVTS_RECORD containing the dvts data being written out
C
C  OUTPUT PARAMETERS:
C     lineEnd - the DVTS specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDVTS (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DVTS_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*10      dvorakStr
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the subjective dvorak section of the output fix line.
c
c     Start with the sensor.
      write( lineEnd, '(" ",a4,",")' ) ADJUSTR(fix%sensor)

c     Add the pcn.
      if( fix%pcn .ne. 0 .and. fix%pcn .ne. -1 ) then
         write( temp, '(" ",i1,",")' ) fix%pcn
      else
         write( temp, '("  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Build the long term trend dvorak.
      call buildDvorak( fix, 'long', dvorakStr )
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//" "//dvorakStr//","

c     Short term dvorak code
      if( fix%shortTerm%tnumChg .ne. 0 .or.
     &     fix%shortTerm%lastEvalHrsAgo .ne. 0) then
         write( dvorakStr, '(a1,i2.2,i2.2)' )
     &        fix%shortTerm%pastChg, fix%shortTerm%tnumChg,
     &        fix%shortTerm%lastEvalHrsAgo
      else
         dvorakStr = '     '
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//" "//dvorakStr(1:5)//","

c     Add on the ci 24 hour forecast.
      if( fix%ci24hr .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%ci24hr
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add on the satellite type, center type, tropical indicator and
c     comments.
      write( temp, '(" ",a6,", ",a4,", ",a1,", ",a)' )
     &     ADJUSTR(fix%satType), ADJUSTR(fix%centerType),
     &     fix%tropical, fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputMicrowave
C
C  DESCRIPTION:  Build the microwave section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputMicrowave ( lineEnd, fixRcd%micro )
C
C  INPUT PARAMETERS:
C     fixRcd - MICROWAVE_RECORD containing the microwave data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the microwave specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputMicrowave (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (MICROWAVE_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*1       rainchar
      character*1       edgecut(8)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the microwave section of the output fix line.
c
      if( fix%rain ) then
         rainchar = 'R'
      else
         rainchar = ' '
      endif
c     Start with the rain flag
      write( lineEnd, '(" ",a1,",")' )  rainchar

c     Add the rainrate and algorithm
      if( fix%rainrate .ne. 0 ) then
         write( temp, '(" ",i3,", ",a6,",")' )
     &        fix%rainrate, ADJUSTR(fix%algorithm)
      else
         write( temp, '("    , ",a6,",")' )  ADJUSTR(fix%algorithm)
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the wave height
      if( fix%wave .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%wave
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the temp
      if( fix%temp .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fix%temp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the slp raw
      if( fix%slpraw .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%slpraw
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the slp retrieved
      if( fix%slpretr .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%slpretr
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the max seas
      if( fix%seas .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fix%seas
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the satellite type
      write( temp, '(" ",a6,",")' )  ADJUSTR(fix%satType)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rad, windcode and radii.
      if( fix%radii(1) .ne. 0 .or. fix%radii(2) .ne. 0 .or.
     &     fix%radii(3) .ne. 0 .or. fix%radii(4) .ne. 0 ) then
         write( temp, '(" ",i3,", ",a4,8(", ",i4),",")' )
     &        fix%rad, ADJUSTR(fix%windcode), (fix%radii(ii),ii=1,8)
      else
         write( temp, '("    ,     ,     ,     ,     ,     ,     , ",
     &        "    ,     ,     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radii modifiers
      do ii = 1, 8
         edgecut(ii) = ' '
         if( fix%edge(ii) .and. fix%cut(ii) ) then
            edgecut(ii) = 'B'
         elseif( fix%edge(ii) ) then
            edgecut(ii) = 'E'
         elseif( fix%cut(ii) ) then
            edgecut(ii) = 'C'
         endif
      enddo
      write( temp, '( 8(" ",a1,",") )' )
     &     (edgecut(ii),ii=1,8)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radii confidence
      if( fix%radConf .ne. 0 ) then
         write( temp, '(" ",i1,"," )' ) fix%radConf
      else
         write( temp, '("  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments
      write( temp, '(" ",a )' )  fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputRadar
C
C  DESCRIPTION:  Build the radar section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputRadar ( lineEnd, fixRcd%radar )
C
C  INPUT PARAMETERS:
C     fixRcd - RADAR_RECORD containing the radar data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the radar specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputRadar (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (RADAR_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*1       radarformat
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the radar section of the output fix line.
c
c     Start with the radar type and radar format
      if( fix%radobcode ) then
         radarformat = 'R'
      elseif( fix%plainlanguage ) then
         radarformat = 'P'
      elseif( fix%doppler ) then
         radarformat = 'D'
      endif
      write( lineEnd, '(" ",a1,", ",a1,",")' )
     &     fix%rdrtype, radarformat

c     Add the radob code, eye shape
      write( temp, '(" ",a10,", ",a2,",")' )
     &     fix%radob, ADJUSTR(fix%eyeShape)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the % of eye wall observed
      if( fix%eyeWallOb .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%eyeWallOb
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the spiral overlay
      if( fix%spiralOV .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%spiralOV
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radar site lat/lon.
      if( fix%lat .ne. 0. .or. fix%lon .ne. 0. ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' )
     &        NINT(fix%lat*100.), fix%NS, NINT(fix%lon*100.), fix%EW
      else
         write( temp, '("      ,       ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the inbound wind
      if( fix%vmaxin .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%vmaxin
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the azimuth
      if( fix%azimuthin .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%azimuthin
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the range
      if( fix%rangein .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rangein
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the elevation
      if( fix%elevin .ne. 0 ) then
         write( temp, '(" ",i5,",")' ) fix%elevin
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the outbound wind
      if( fix%vmaxout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%vmaxout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the azimuth
      if( fix%azimuthout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%azimuthout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the range
      if( fix%rangeout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rangeout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the elevation
      if( fix%elevout .ne. 0 ) then
         write( temp, '(" ",i5,",")' ) fix%elevout
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the cloud height.
      if( fix%cloudHeight .ne. 0 ) then
         write( temp, '(" ",i5,",")' )  fix%cloudHeight
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rain info  accumulation
      if( fix%rainAccum .ne. 0. ) then
         write( temp, '(" ",i5,",")' ) NINT(fix%rainAccum*100.)
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rain info time interval
      if( fix%rainAcTimeInt .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rainAcTimeInt
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rani info lat/lon
      if( fix%rainlat .ne. 0. .or. fix%rainlon .ne. 0 ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' )
     &        NINT(fix%rainlat*100.), fix%rainNS,
     &        NINT(fix%rainlon*100.), fix%rainEW
      else
         write( temp, '("      ,       ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputAircraft
C
C  DESCRIPTION:  Build the aircraft section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputAircraft ( lineEnd, fixRcd%air )
C
C  INPUT PARAMETERS:
C     fixRcd - AIRCRAFT_RECORD containing the aircraft data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the aircraft specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputAircraft (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (AIRCRAFT_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*2       eyeshapeStr
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the aircraft section of the output fix line.
c
c     Start with flight level in 100's of feet
      if( fix%FlightLevelFt .ne. 0 ) then
         write( lineEnd, '(" ",i2,",")' )  fix%FlightLevelFt
      else
         write( lineEnd, '("   ,")' )
      endif

c     Add the flight level in mb
      if( fix%FlightLevelMB .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%FlightLevelMB
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the min height
      if( fix%MinHeight .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fix%MinHeight
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max sfc wind intensity
      if( fix%MaxSurfaceWind .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWind
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound bearing
      if( fix%MaxSurfaceWindBearing .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWindBearing
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound range
      if( fix%MaxSurfaceWindRange .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWindRange
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind dir
      if( fix%MaxFlightLevelWindDir .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindDir
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind intensity
      if( fix%MaxFlightLevelWindInt .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindInt
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind bearing
      if( fix%MaxFlightLevelWindBearing .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindBearing
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level range
      if( fix%MaxFlightLevelWindRange .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindRange
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add MSLP
      if( fix%MinSeaLevelPressure .ne. 0 ) then
         write( temp, '(" ",i4,",")') fix%MinSeaLevelPressure
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add temp outside eye
      if( fix%EyeTempOutside .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%EyeTempOutside
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add temp inside eye
      if( fix%EyeTempInside .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%EyeTempInside
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add dew pt temp
      if( fix%DewPoint .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%DewPoint
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add ss temp
      if( fix%SeaSurface .ne. 0 ) then
         write( temp, '(" ",i2,",")') fix%SeaSurface
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add wall cloud thickness
      if( fix%WallCloudThickness .ne. 0 ) then
         write( temp, '(" ",i2,",")') fix%WallCloudThickness
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      if( fix%EyeShape .eq. 0 ) then
         eyeshapeStr = '  '
      elseif( fix%EyeShape .eq. 1 ) then
         eyeshapeStr = 'CI'
      elseif( fix%EyeShape .eq. 2 ) then
         eyeshapeStr = 'EL'
      elseif( fix%EyeShape .eq. 3 ) then
         eyeshapeStr = 'CO'
      endif
c     Add eye characteristics, shape
      write( temp, '(" ",a2,",")' ) ADJUSTR(eyeshapeStr)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, orientation
      if( fix%EyeOrientation .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%EyeOrientation
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, diameter
      if( fix%DiameterLongAxis .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%DiameterLongAxis
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, short axis
      if( fix%DiameterShortAxis .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%DiameterShortAxis
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the accuracy, navigational
      if( fix%NavigationalAccuracy .ne. 0 ) then
         write( temp, '(" ",i3,",")' )
     &        NINT(fix%NavigationalAccuracy*10.)
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the accuracy, meteorological
      if( fix%NavigationalMeteorological .ne. 0. ) then
         write( temp, '(" ",i3,",")' )
     &        NINT(fix%NavigationalMeteorological*10.)
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the mission #
      if( fix%MissionNumber .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%MissionNumber
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDropsonde
C
C  DESCRIPTION:  Build the dropsonde section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDropsonde ( lineEnd, fixRcd%drop )
C
C  INPUT PARAMETERS:
C     fixRcd - DROPSONDE_RECORD containing the dropsonde data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the dropsonde specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDropsonde (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DROPSONDE_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the dropsonde section of the output fix line.
c
c     Start with the sonde environment and midpoint height
      write( lineEnd, '(" ",a10,", ",i3,",")' )
     &     ADJUSTR(fix%sondeEnv), fix%height150

c     Add spd of mean wind lowest 150 m and spd of mean wind 0-500 m
      write( temp, '(" ",i3,", ",i3,",")' )  fix%vspd150, fix%vspd500
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputAnalysis
C
C  DESCRIPTION:  Build the analysis section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputAnalysis ( lineEnd, fixRcd%anal )
C
C  INPUT PARAMETERS:
C     fixRcd - ANALYSIS_RECORD containing the analysis data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the analysis specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputAnalysis (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (ANALYSIS_RECORD) fix
c
c         local variables
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the analysis section of the output fix line.
c
c     Start with the analyst initials, start time, end time and
c     distance to nearest data
      write( lineEnd, '(" ",a3,", ",a12,", ",a12,", ",i3,",")' )
     &     fix%initials, fix%startDTG, fix%endDTG,
     &     fix%DistanceToNearestData

c     Start with the analyst initials, start time and end time
      write( lineEnd, '(" ",a3,", ",a12,", ",a12,",")' )
     &     ADJUSTR(fix%initials),
     &     ADJUSTR(fix%startDTG), ADJUSTR(fix%endDTG)

c     Add the distance to nearest data
      if( fix%DistanceToNearestData .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%DistanceToNearestData
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the sst
      if( fix%SST .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%SST
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the observation sources
      write( temp, '(" ",a24,",")' ) ADJUSTR(fix%obSources)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputFrcd
C
C  DESCRIPTION:  Writes the F_RECORD to the spectified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputFrcd ( datFile, fixdata%fRecord(ii), result )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the objective aids file
C     fixRcd - F_RECORD struct containing record to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputFrcd (datFile, fixRcd, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (F_RECORD)   fixRcd
      integer           result
c
c         local variables
      integer           ii
      character*400     line
      character*100     temp
      character*200     lineEnd
      character*10      cicode
      character*1       edgecut(4)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Build the output fix line. */
c
c     Start with the basin, cyclone number and date-time-group.
      write( line, '(a2,", ",i2.2,", ",a12,",")' )
     &     fixRcd%basin, fixRcd%cyNum, fixRcd%DTG

      cicode = ' '
      if( fixRcd%centerFix ) cicode = 'C'
      if( fixRcd%intensityFix )
     &     cicode = cicode(1:LEN_TRIM(cicode))//"I"
      if( fixRcd%radiiFix ) cicode = cicode(1:LEN_TRIM(cicode))//"R"
      if( fixRcd%pressureFix ) cicode = cicode(1:LEN_TRIM(cicode))//"P"

c     Add on the fix format, fix code, center/intens code and flagged.
      write( temp, '(" ",i3,", ",a4,", ",a10,", ",a1,",")' )
     &     fixRcd%fixformat, ADJUSTR(fixRcd%fixtype), ADJUSTR(cicode),
     &     fixRcd%flagged
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the lat and lon.
      if( fixRcd%lat .ne. 0. .or. fixRcd%lon .ne. 0. ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' )
     &        NINT(fixRcd%lat*100.), fixRcd%NS,
     &        NINT(fixRcd%lon*100.), fixRcd%EW
      else
         write( temp, '("      ,       ," )' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the height
      if( fixRcd%height .ne. 0 ) then
         write( temp, '(" ",i5,",")' )  fixRcd%height
      else
         write( temp, '("      ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the position confidence
      if( fixRcd%positConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%positConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the vmax and v confidence
      if( fixRcd%v .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fixRcd%v
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the v confidence
      if( fixRcd%vConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' ) fixRcd%vConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure
      if( fixRcd%pressure .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fixRcd%pressure
      else
         write( temp, '("     ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure confidence
      if( fixRcd%presConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%presConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure derivation
      write( temp, '(" ",a4,",")' )  ADJUSTR(fixRcd%presDeriv)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

      do ii = 1, 4
         edgecut(ii) = " "
         if( fixRcd%edge(ii) .and. fixRcd%cut(ii) ) then
            edgecut(ii) = "B"
         elseif( fixRcd%edge(ii) ) then
            edgecut(ii) = "E"
         elseif( fixRcd%cut(ii) ) then
            edgecut(ii) = "C"
         endif
      enddo

c     Add on the rad, windcode and radii data
      if( fixRcd%radii(1) .ne. 0 .or. fixRcd%radii(2) .ne. 0 .or.
     &     fixRcd%radii(3) .ne. 0 .or. fixRcd%radii(4) .ne. 0 ) then
         write( temp, '(" ",i3,", ",a4,4(", ",i4),",")' )
     &        fixRcd%rad, ADJUSTR(fixRcd%windcode),
     &        (fixRcd%radii(ii),ii=1,4)
      else
         write( temp, '("    ,     ,     ,     ,     ,     ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the radii modifier
      write( temp, '(" ",a1,", ",a1,", ",a1,", ",a1,",")' )
     &     (edgecut(ii),ii=1,4)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the radii confidence.
      if( fixRcd%radConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%radConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the radius of max winds
      if( fixRcd%mrd .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fixRcd%mrd
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the eye
      if( fixRcd%eye .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fixRcd%eye
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the subregion, fix site and initials.
      write( temp, '(" ",a1,", ",a5,", ",a3,",")' )
     &     fixRcd%subregion, ADJUSTR(fixRcd%fixsite),
     &     ADJUSTR(fixRcd%initials)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Process the rest of the record.
      if( fixRcd%fixformat .eq. DVTStype ) then
         call outputDVTS( lineEnd, fixRcd%dvts )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. DVTOtype ) then
         call outputDVTO( lineEnd, fixRcd%dvto )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. MICRtype .or.
     &        fixRcd%fixformat .eq. SCATtype ) then
         call outputMicrowave( lineEnd, fixRcd%micro )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. RDRtype ) then
         call outputRadar( lineEnd, fixRcd%radar )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. AIRCtype ) then
         call outputAircraft( lineEnd, fixRcd%air )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. DROPtype ) then
         call outputDropsonde( lineEnd, fixRcd%drop )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. ANALtype ) then
         call outputAnalysis( lineEnd, fixRcd%anal )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      endif

C
      END




C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeFix
C
C  DESCRIPTION:  Writes a FIX_DATA record to the specified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call writeFix ( datFile, fixdata, result )
C
C  INPUT PARAMETERS:
C     datFile - unit number of the objective aids file
C     fixdata - FIX_DATA struct containing data to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeFix (datFile, fixdata, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (FIX_DATA)   fixdata
      integer           result
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
C
      do ii = 1, fixdata%numrcrds
         call outputFrcd( datFile, fixdata%fRecord(ii), result )
      enddo
C
      END


C******************************************************************
C
C  BRIEF DESCRIPTION OF PROGRAM MODULES:
C
C   dtgchk -  Check to see if a DTG is valid.
C   dtgdif -  wrapper for the dtgdif2 routine.
C   dtgdif2 - Given two DTGs, return the difference in hours (=MDTG-NDTG).
C   dtgmod -  Given base DTG and increment (+/- hours), return new DTG
C                 ( = indtg + idif ) and the status value.
C   dtgnum -  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year.
C   dtgyrhr - Given a year and hours of the year, DTGYRHR returns
C                a DTG of format YYYYMMDDHH in NEWDTG.
C   icrdtg -  wrapper for the dtgmod routine.
C
C******************************************************************

      subroutine icrdtg ( idtg1, idtg2, incr )
C
C
C   DESCRIPTION:
C      This is a wrapper for the dtgmod routine.
C      The purpose of the routine is to convert idtg1
C      into a 10 digit dtg prior to calling dtgmod and then
C      to convert idtg2 from 10 digits back to 8 digits after
C      returning from the dtgmod call.
C
C     DTGMOD description:
C      Given base DTG and increment (+/- hours), return new
C      DTG ( = idtg1 + incr ) and the status value.
C      Year 2000 compliant.
C
C   Programmer:  Ann Schrader  9/98
C
C   USAGE (CALLING SEQUENCE):
C      icrdtg( idtg1, idtg2, incr );
C
C  VARIABLES AND THEIR USE
C    PASSED
C      idtg1 - Base DTG in the format YYMMDDHH.
C      incr - Difference in hours.
C    RETURNED
C      idtg2 - New DTG, the sum of idtg1 and incr.  If one of
C               the errors listed below occurrs, idtg2 will
C               be returned asterisk-filled.
C
C      error possibilities -  0 = OK,
C                            -1 = invalid DTG,
C                            -2 = invalid increment,
C                            -3 = dtgyrhr returned a non-zero status.
C
C ..............MAINTENANCE SECTION............................
C
C   METHOD:
C
C   LANGUAGE:  C
C
C   RECORD OF CHANGES:
C
      implicit none

      character*8 idtg1, idtg2
      character*10 indtg, newdtg
      integer incr, istat
      integer iyr, imo, iday, ihour

      IYR    = 0

C     Do an internal read to get the year portion of the input idtg1
      read( idtg1, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit input dtg for the dtgmod call.
C     The ATCF database contains data as far back as 1945, any data
C     with a year of less than 45 must by default be century = 20.
C     This logic, of course, will not work past the year 2044.
      if( iyr .lt. 45 ) then
         indtg = '20'//idtg1
      else
         indtg = '19'//idtg1
      endif

C     Call dtgmod, the year 2000 compliant version of icrdtg
      call dtgmod( indtg, incr, newdtg, istat )

      if( istat .eq. 0 ) then
C        Get the newdtg without the first two digits of the year.
         idtg2 = newdtg(3:10)
      else
         idtg2 = '********'
      endif

      return
      end


      SUBROUTINE DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGMOD
C
C  DESCRIPTION:  Given base DTG and increment (+/- hours), return new DTG
C                 ( = indtg + idif ) and the status value.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CHARACTER*10 INDTG, NEWDTG
C
C    CALL DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	Base DTG in the format YYYYMMDDHH.
C    IDIF       INT     Difference in hours
C		         (-8775216 through +4294967295).
C
C  OUTPUT PARAMETERS:
C
C    NEWDTG	C*10	New DTG, the sum of INDTG and IDIF. If one of
C			  the errors listed below occurrs, NEWDTG will
C			  be returned asterisk-filled.
C    ISTAT      INT     Error return.
C			  ISTAT =  0, ok.
C			  ISTAT = -1, Invalid DTG.
C			  ISTAT = -2, Invalid increment.
C			  ISTAT = -3, DTGYRHR returned a non-zero
C				      status.
C
C  CALLS:
C
C    DTGNUM		Get integer year, month, day, hour, days of the
C			  year and hours of the year from DTG.
C    DTGYRHR		Get DTG from julian date.
C
C  EXAMPLE:
C
C    CHARACTER*10 CURDTG, NEWDTG
C    INTEGER IDIF
C
C    DATA CURDTG / '1991030612' /`
C    DATA IDIF  / 2 /
C
C    CALL DTGMOD ( CURDTG, IDIF, NEWDTG, ISTAT )
C		...
C
C    NEWDTG will contain 1991030614.
C
C  ERROR CONDITIONS:
C
C    Invalid DTG.   ISTAT=-1 at return. NEWDTG asterisk-filled.
C    Invalid IDIF.  ISTAT=-2 at return. NEWDTG asterisk-filled.
C    Non-zero status return from DTGYRHR. ISTAT=-3 at return.
C      NEWDTG asterisk-filled.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG     C*10    Value returned in NEWDTG if an error occured.
C    IDX        INT     Set to 1 if the input year is a leap
C			  year. Otherwise, it will be 2, 3 or 4
C			  (the remainder resulting from the mod
C			  function). Used as a subscript for the
C			  IHOURS array.
C    MONTH(12,2) INT	Number of days elapsed before getting to
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C     IHOURS(4)	INT	Number of hours in the year.  IHOURS(1) contains
C			  the number of hours in a leap year.
C     IYR	INT	Year, extracted from INDTG by DTGNUM.
C     IMO	INT	Month, extracted from INDTG by DTGNUM.
C     IDAY	INT	Day, extracted from INDTG by DTGNUM.
C     IHR	INT	Hour, extracted from INDTG by DTGNUM.
C     IDAOFYR	INT	Day of the year, extracted from INDTG by DTGNUM.
C     IHROFYR	INT	Hour of year, extracted from INDTG by DTGNUM.
C     JSTAT	INT	Error return from DTGNUM. If=0, ok.
C     KSTAT	INT	Error return from DTGYRHR. If=0, ok.
C
C  METHOD
C
C     1.  Call DTGNUM to get hours of the year.
C     2.  Add IDIF to the hours of the year.
C     3.  If the new sum is negative or has too many hours for one year,
C         adjust the sum and input year until the sum is positive and within
C         one year.
C     4.  Call DTGYRHR, passing it the year and hours of the year, to get
C         the new DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGMOD required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 INDTG, NEWDTG, BADDTG
C
      INTEGER IHOURS(4), IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR, NEWHRS,
     *        IDIF, IDX, I, ISTAT, JSTAT, KSTAT
C
      DATA IHOURS / 8784, 3*8760 /
      DATA BADDTG / '**********' /
C
      ISTAT = 0
C
      CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
	 NEWDTG = BADDTG
         GO TO 5000
      END IF

****************************************************************************
*        Increment (decrement) hour of the year (IHROFYR) by IDIF,
*        test for change of year, adjust if necessary, reformat to NEWDTG.
****************************************************************************

      NEWHRS = IHROFYR + IDIF

****************************************************************************
*         See if NEWHRS is negative.  If it is,  perform a loop that
*		Subtracts 1 from the year,
*		Adds a year's worth of hours to NEWHRS, the number of hours
*		  depending on whether the year is a leap year or not.
*		Sees if NEWHRS is still negative.  Leave the loop when it
*		  becomes zero or positive.
****************************************************************************

      IF ( NEWHRS .LT. 0 ) THEN
C
	 DO 10 I=1,1000
            IYR = IYR - 1
C
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
    	    NEWHRS = NEWHRS + IHOURS(IDX)
            IF ( NEWHRS .GE. 0 ) GO TO 30
   10    CONTINUE
C
         GO TO 900

****************************************************************************
*         NEWHRS is positive or 0.
*         Perform a loop until NEWHRS' value is less than or equal to the
*           number of hours in a year.
*              See if there is more than one year's worth of hours in NEWHRS.
*              If there is, perform a loop that
*	         Subtracts a year's worth of hours from NEWHRS, the number
*		   of hours depending on whether the year is a leap year or
*                  not.
*	         Adds 1 to the year.
***************************************************************************

      ELSE
         DO 20 I=1,1000
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
            IF ( NEWHRS .LT. IHOURS(IDX) ) GO TO 30
            NEWHRS = NEWHRS - IHOURS(IDX)
            IYR = IYR + 1
   20    CONTINUE
C
          GO TO 900
      END IF

   30 CALL DTGYRHR ( IYR, NEWHRS, NEWDTG, KSTAT )
      IF ( KSTAT .NE. 0 ) THEN
         NEWDTG = BADDTG
	 ISTAT = -3
      END IF
C
      GO TO 5000

**************************************************************************
*        Error.
**************************************************************************

  900 NEWDTG = BADDTG
      ISTAT = -2
C
 5000 RETURN
      END


      SUBROUTINE DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGYRHR
C
C  DESCRIPTION:  Given a year and hours of the year, DTGYRHR returns
C                a DTG of format YYYYMMDDHH in NEWDTG.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):  CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    IYR	INT     4-digit year, e.g. 1995.
C    IHRS	INT	Hours into the year.
C
C  OUTPUT PARAMETERS:
C
C    NEWDTG	C*10	DTG of form YYYYMMDDHH, e.g. 1995041606.
C    ISTAT	INT     Status.
C			  =  0 - OK.
C			  = -1 - Invalid year.
C			  = -2 - Invalid hour-of-the-year value.
C
C  CALLED BY:  DTGMOD
C
C  CALLS:      None.
C
C  EXAMPLE:
C
C    CHARACTER NEWDTG*10
C    INTEGER IYR, IHRS, ISTAT
C
C    DATA IYR / 1991 /, IHRS / 674 /
C
C    CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C    IF ( ISTAT .NE. 0 ) THEN
C	Error...
C
C    NEWDTG will contain 1991012902
C
C  ERROR CONDITIONS:
C
C    Negative year passed. Set NEWDTG to all asterisks, set ISTAT
C     to -1, and return.
C    Invalid hours value passed. Set NEWDTG to all asterisks,
C     set hours to max hours for that year, and set ISTAT to -2.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG	  C*10  Returned in NEWDTG if an error occurred.
C    MONTH(12,2)  INT	Number of days elapsed before getting to
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C    IDAYS	  INT	Number, of hours (IHRS) converted into whole
C			  days.
C    IHOURS       INT   Number of hours into the next day.
C
C    IDX	  INT	Subscript for month.
C			     =1, non-leap year
C			     =2, leap year
C
C    IMO	  INT   IMO is initially set to 0.  It is incremented
C			  by 1 as the month loop runs.  When the correct
C			  month is found, IMO will be one less than I.
C			  If no correct month is found, IMO is set to
C			  12 and in either case becomes the month in the
C			  new DTG.
C
C    MAXHRS(2)	  INT	Maximum number of hours in one year.  Used for
C			  checking hours argument.
C
C  METHOD:
C
C    1.  Calculate the number of the day by dividing the hours by 24
C	 and adding 1 (IDAYS).
C    2.  Find what month the day is in by finding where the day
C	 falls in the month array (see description of MONTH, above).
C    3.  The month array has two sections, one for leap years and
C	 one for non-leap years.  Get the subscript for this dimension
C	 by taking mod 4 of the year.
C    4.  Calculate the days in the retrieved month by subtracting the
C        the number of days up to the month from the days of the year
C	 (IDAYS).
C    5.  Calculate the hours in the retrieved day by taking mod 24 of
C	 the hours input argument (IHRS).
C    6.  Do an internal write of the given year and derived month,
C	 day and hour into the output DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGYRHR required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 NEWDTG, BADDTG
C
      INTEGER IYR, IHRS, ISTAT, IDX, MOD, IDAYS, IMO, I, IDAY, IHOURS
      INTEGER MONTH(12,2), MAXHRS(2)
C
      DATA MONTH/0,31,59,90,120,151,181,212,243,273,304,334,
     *           0,31,60,91,121,152,182,213,244,274,305,335/
      DATA MAXHRS / 8759, 8783 /
      DATA BADDTG / '**********' /
C
      ISTAT = 0

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      IF ( MOD ( IYR, 100 ) . EQ. 0 ) THEN
	 IF ( MOD ( IYR, 400 ) .EQ. 0 ) THEN
	    IDX = 2
	 ELSE
	    IDX = 1
	 END IF

      ELSEIF (MOD ( IYR, 4 ) .EQ. 0 ) THEN
	 IDX = 2

      ELSE
	 IDX = 1
      END IF

***********************************************************************
*        Calculate number of whole days in IHRS.
*        Validate the hours argument.
***********************************************************************

      IF ( IYR .LT. 0 ) THEN
         ISTAT = -1
	 NEWDTG = BADDTG
         GO TO 5000
      END IF
C
      IF ( (IHRS .LT. 0) .OR. (IHRS .GT. MAXHRS(IDX)) ) THEN
         ISTAT = -2
         IHRS = MAXHRS(IDX)
 	 NEWDTG = BADDTG
      END IF
C
      IDAYS = IHRS/24+1

***********************************************************************
*        Find the proper month by determining where this number of
*        days falls in the MONTH array.
***********************************************************************

      IMO = 0
      DO 2 I = 2,12
         IMO = IMO + 1
         IF ( IDAYS .LE. MONTH(I,IDX) ) GO TO 3
 2    CONTINUE
      IMO = IMO + 1
C
 3    IHOURS = MOD(IHRS,24)
      IDAY = IDAYS - MONTH(IMO,IDX)
      WRITE(NEWDTG, '(I4.4, 3I2.2)') IYR, IMO, IDAY, IHOURS
C
 5000 RETURN
      END


      SUBROUTINE DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
     *                    ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGNUM
C
C  DESCRIPTION:  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	    Day-time group, yyyymmddhh
C
C  OUTPUT PARAMETERS:
C
C    IYR	INT	    Year.
C    IMO	INT	    Month.
C    IDAY	INT         Day.
C    IHOUR	INT	    Hour.
C    IYRDAY	INT	    Days of the year.
C    IYRHRS	INT	    Hours of the year.
C    ISTAT	INT	    Error status return, = 0, OK; = -1,
C			     bad input DTG.
C
C  CALLS:
C
C    DTGCHK	A function that validates DTGs.  Output is a 10-character
C		  string that is blank if the DTG is valid.  It will
C		  contain asterisks corresponding to where the DTG is invalid
C		  otherwise.
C
C  ERRORS CONDITIONS:
C
C    Invalid DTG.  All integers will return with zero. ISTAT will be
C     set to -1.
C
C  EXAMPLE:
C
C    CHARACTER*10 INDTG
C
C    INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT
C    DATA INDTG / '1990120312' /
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
C   *              ISTAT )
C
C    		...
C
C    Values returned will be:
C	IYR     - 1990
C	IMO     -   12
C	IDAY    -   03
C	IHOUR	-   12
C	IYRDAY  -  337
C 	IYRHRS  - 8076
C       ISTAT   -    0
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    DTGERR	 C*10	Holds error return from DTGCHK.
C    IDX         INT    Set to 1 if the input year is not a leap
C			  year. If it is a leap year, it will be
C			  set to 2. Used as a subscript for the
C			  month array.
C    MONTH(12,2) INT	Number of days elapsed before getting to
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C  METHOD:
C
C    1.  Do an internal read of the input DTG to get year, month,
C	 day, hour.  Call DTGCHK to make sure it's valid first.
C	 If it is invalid, set ISTAT to 0 and return.
C    2.  Set leap year index, IDX.
C    3.  Calculate number of days in the year by adding IDAY to the
C	 number of days elapsed before the first of the month, IMO.
C	 Days elapsed is in the MONTH array.
C    4.  Calculate number of hours into the year by multiplying
C        whole days of the year (IYRDAY-1) by 24 hours and adding
C        IHOUR.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................

C
      IMPLICIT NONE

      CHARACTER*10 INDTG, DTGERR, DTGCHK
C
      INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT, IDX, MOD
      INTEGER MONTH(12,2)
C
      DATA MONTH /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     *            0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
     *           /
C
      DATA DTGERR /'          '/
C
      IYR    = 0
      IMO    = 0
      IDAY   = 0
      IHOUR  = 0
      IYRDAY = 0
      IYRHRS = 0
      ISTAT  = 0

************************************************************************
*	 Call DTGCHK to validate the DTG.
*	 If it's ok, do an internal read of the integer date parts.
************************************************************************

      DTGERR = DTGCHK ( INDTG )
      IF ( DTGERR .NE. '          ' ) THEN
	 ISTAT = -1
         GO TO 5000
      ELSE
         READ ( INDTG, '(I4, 3I2)' ) IYR, IMO, IDAY, IHOUR
      END IF

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      IF ( MOD ( IYR, 100 ) . EQ. 0 ) THEN
	 IF ( MOD ( IYR, 400 ) .EQ. 0 ) THEN
	    IDX = 2
	 ELSE
	    IDX = 1
	 END IF

      ELSEIF (MOD ( IYR, 4 ) .EQ. 0 ) THEN
	 IDX = 2

      ELSE
	 IDX = 1
      END IF
C
      IYRDAY = MONTH(IMO,IDX) + IDAY
      IYRHRS = ( IYRDAY-1 ) * 24 + IHOUR
C
 5000 RETURN
      END


      FUNCTION DTGCHK ( DTG )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGCHK
C
C  DESCRIPTION:  Check to see if a DTG is valid.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CHARACTER*10 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG )
C            or
C    CHARACTER*12 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG )
C
C  INPUT PARAMETERS:
C
C    DTG           	  Date-time group in one of two formats:
C		C*10        YYYYMMDDHH
C		C*12	    YYYYMMDDHHmm
C
C  OUTPUT PARAMETERS:
C
C    ERRDTG	C*10/12   Error designator.  All blank if DTG was valid.
C			    Characters 1-4   = **** if year was bad.
C		            Characters 5-6   = **   if month was bad.
C			    Characters 7-8   = **   if day was bad.
C			    Characters 9-10  = **   if hour was bad.
C			    Characters 11-12 = **   if minutes were bad.
C
C  CALLS:
C
C    LEN	Returns the length of a string.
C
C  EXAMPLE:
C
C    1)  CHARACTER*10 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '1995070312' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    2)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '198607031245' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    3)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C
C	 DATA DTG / '198677031290' /
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will look like this, where a period denotes a blank:
C	   ....**....**	 meaning that the month and minutes are invalid.
C
C  RESTRICTIONS:
C
C    The DTG may not contain any blanks.
C    The DTG must be either 10 or 12 digits long.
C    DTGCHK rejects DTG years that are outside the range 1800-2799.
C
C  ERROR CONDITIONS:
C
C    Non-numeric DTG. DTGCHK will be asterisk-filled.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    IDAY	INT	Integer day from the input DTG.
C    IHOUR	INT	Integer hour from the input DTG.
C    IMO	INT	Integer month from the input DTG.
C    IYR	INT	Integer year from the input DTG.
C    LENGTH	INT	Gets the return from LEN, i.e. the length
C			  of the argument string.
C    MIN	INT	Integer minutes from the input DTG.
C    MONTH(12)	INT	An array containing the number of days in
C			  each month.
C
C  METHOD:
C
C    Check that each value is numeric and within the ranges:
C	Year:  1800-2799.
C	Month: 1-12.
C	Day:   1-31 for Jan, Mar, May, July, Aug, Oct and Dec.
C	       1-30 for Apr, June, Sep, Nov.
C	       28 or 29 for Feb, depending on whether year is a leap year.
C		  Note that 1900 is not a leap year and 2000 is.
C	Hour:  0-23
C	Min:   0-59
C    Whenever a value is outside its range, set the corresponding characters
C	of DTGCHK to asterisks.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGCHK required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER DTGCHK*(*), DTG*(*)
C
      INTEGER MONTH(12), IYR, IMO, IDAY, IHOUR, MIN, LENGTH, LEN, I
C
      DATA MONTH / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
      LENGTH = LEN ( DTG )
      IF ( LENGTH .NE. 10 .AND. LENGTH .NE. 12 ) THEN
	 DTGCHK = '************'
	 GO TO 5000
      END IF

****************************************************************************
*        Make sure every character in the DTG is a number.
****************************************************************************

      DO 10 I=1,LENGTH
	 IF ( DTG(I:I) .LT. '0' .OR. DTG(I:I) .GT. '9' ) THEN
C
	    IF (LENGTH .EQ. 10 ) THEN
               DTGCHK = '**********'
	    ELSE
  	       DTGCHK = '************'
	    END IF
C
	    GO TO 5000
         END IF
   10 CONTINUE

****************************************************************************
*        MONTH(2) will be changed to 29 later if the year is a leap year.
****************************************************************************

      MONTH(2) = 28

****************************************************************************
*        Get the integer versions of the year, month, day, and hour.
****************************************************************************

      READ ( DTG, '(I4,3I2)' ) IYR, IMO, IDAY, IHOUR

****************************************************************************
*       Get minutes if they're there.
****************************************************************************

      DTGCHK(1:LENGTH) = ' '
C
      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (DTG(11:11) .GE. '0') .AND. (DTG(11:11) .LE. '9') .AND.
     *        (DTG(12:12) .GE. '0') .AND. (DTG(12:12) .LE. '9') ) THEN
  	    READ (DTG, '(10X, I2)' ) MIN
	 ELSE
	    MIN = 99
	 END IF
      END IF

****************************************************************************
*        Check out the DTG.
*          Year...
*        Allow iyr == 0 for util_julday(), ajs  09/2002
****************************************************************************

      IF ( ((IYR .LT. 1800) .OR. (IYR .GT. 2799)) .AND. IYR .NE. 0 )
     *   DTGCHK(1:4) = '****'

****************************************************************************
*          Month...
****************************************************************************

      IF ( (IMO .LT. 1) .OR. (IMO .GT. 12) ) DTGCHK(5:6) = '**'

****************************************************************************
*          Day...
*	     Set February days to 29 if this is a leap year.
*
*	     If DTGCHK(1:6) (year/month) already has asterisks, it was
*              invalid, so don't do the day check.  If the year was invalid,
*	       we can't see if it was a leap year.  If the month was
*	       invalid, we can't see if it was February.
*	     See if this is a leap year, i.e. evenly divisible by four or,
*	       if it's a century (dtg(3:4) = '00'), evenly divisible
*	       by 400.
****************************************************************************

      IF ( DTGCHK(1:6) .EQ. '  ' ) THEN
         IF ( (DTG(3:4) .NE. '00') .AND. (MOD(IYR, 4) .EQ. 0) .OR.
     *      ( (DTG(3:4) .EQ. '00') .AND. (MOD(IYR, 400) .EQ. 0 ) )  )
     * 	       MONTH(2) = 29
C
         IF ( (IDAY .LT. 1) .OR. (IDAY .GT. MONTH(IMO)) )
     *      DTGCHK(7:8) = '**'
      END IF

****************************************************************************
*          Hour...
****************************************************************************

      IF ( (IHOUR .LT. 0) .OR. (IHOUR .GT. 23) ) DTGCHK(9:10) = '**'

****************************************************************************
*	   Minutes...
****************************************************************************

      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (MIN .LT. 0) .OR. (MIN .GT. 59) ) DTGCHK(11:12) = '**'
      END IF
C
 5000 RETURN
      END


      subroutine dtgdif ( dtg1, dtg2, idif )
C
C
C   DESCRIPTION:
C      This is a wrapper for the dtgdif2 routine.
C      The purpose of the routine is to convert dtg1 and
C      dtg2 into 10 digit dtg's prior to calling dtgdif2.
C
C     DTGDIF2 description:
C      Given two DTGs, return the difference in hours (=dtg2 - dtg1).
C      Year 2000 compliant.
C
C   Programmer:  Ann Schrader  9/98
C
C   USAGE (CALLING SEQUENCE):
C      dtgdif( dtg1, dtg2, idif );
C
C  VARIABLES AND THEIR USE
C    PASSED
C      dtg1 - a dtg of format YYMMDDHH
C      dtg2 - a dtg of format YYMMDDHH
C    RETURNED
C      idif - difference in hours (dtg2 - dtg1)
C
C ..............MAINTENANCE SECTION............................
C
C   METHOD:
C
C   LANGUAGE:  C
C
C   RECORD OF CHANGES:
C
      implicit none

      character*8 dtg1, dtg2
      character*10 ndtg, mdtg
      integer idif, istat
      integer iyr, imo, iday, ihour

      IYR    = 0
C     Do an internal read to get the year portion of dtg1
      read( dtg1, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit, ndtg, for the dtgdif2 call.
C     The ATCF database contains data as far back as 1945, any data
C     with a year of less than 45 must by default be century = 20.
C     This logic, of course, will not work past the year 2044.
      if( iyr .lt. 45 ) then
         ndtg = '20'//dtg1
      else
         ndtg = '19'//dtg1
      endif

      IYR    = 0
C     Do an internal read to get the year portion of dtg2
      read( dtg2, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit, mdtg, for the dtgdif2 call.
      if( iyr .lt. 45 ) then
         mdtg = '20'//dtg2
      else
         mdtg = '19'//dtg2
      endif

C     Call dtgdif2, the year 2000 compliant version of dtgdif
      call dtgdif2( ndtg, mdtg, idif, istat )

C      if( istat .eq. 0 ) then
C      else
C      endif

      return
      end


      SUBROUTINE DTGDIF2 ( NDTG, MDTG, IHRS, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGDIF2
C
C  DESCRIPTION:
C
C     Given two DTGs, return the difference in hours (=MDTG-NDTG).
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C     CALL DTGDIF2 ( NDTG, MDTG, IHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C     NDTG	C*10	A DTG of format YYYYMMDDHH.
C     MDTG	C*10    A DTG of format YYYYMMDDHH.
C
C     Note that NDTG can be greater than MDTG. The result will be a
C     negative difference.
C
C  OUTPUT PARAMETERS:
C
C     IHRS	INT	Difference in hours (MDTG-NDTG)
C     ISTAT	INT	Status variable.
C			  ISTAT =  0, ok
C			  ISTAT = -1, invalid DTG.
C
C  CALLS:
C
C     DTGNUM		Get integer year, month, day, hour, hours
C			  into the year, days into the year from DTG.
C
C  EXAMPLE:
C
C     CHARACTER*10 CUTDTG, REPDTG
C
C     INTEGER IDIFF, IHRS, ISTAT
C
C     DATA CUTDTG / '1991022800' /
C     DATA REPDTG / '1991030112' /
C
C     CALL DTGDIF2 ( CUTDTG, REPDTG, IHRS, ISTAT )
C     IF ( ISTAT .NE. 0 ) THEN
C	 Error...
C     ELSE
C	 Okay...
C     END IF
C	 	 :
C                :
C
C     The resulting value returned in IDIFF will be 36.
C     For the call CALL DTGDIF2 ( REPDTG, CUTDTG, IHRS, ISTAT ),
C	the value returned will be -36.
C
C  RESTRICTIONS:
C
C     DTGDIF2 handles DTGs in the range 1800 through 2799.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C     IDIFF		The difference between the two DTG's years.
C     IDUM		A dummy variable to use for the call to
C			  DTGNUM.  We don't need most of the
C			  values returned by DTGNUM.
C     IHOURS(4)		Total hours in the year.  IHOURS(1) is for
C			  leap years, and the other three are for
C			  non-leap years.
C     IHRYRA/B		Number of hours in each DTG's year. They start
C		  	  out containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.
C     ITEMP		Temporary variable used for swapping the
C			  IYEARA/B and IHRYRA/B values.
C     IYEARA/B		Integer year for each DTG. They start out
C		  	  containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.
C     JSTAT	  	Error return variable from DTGNUM. If JSTAT =
C			  =0, ok.
C     LEAPCT		Counts the number of leap years between IYEARA
C			  and IYEARB.
C     LEAPYR		A logical variable that is true if the year is
C			  a leap year.
C     SWAP        	A logical variable that is .TRUE. if the
C			  values of the IHRYR and IYEAR variables
C			  are swapped.
C
C  METHOD:
C
C     1.  Call DTGNUM to get the integer years and hours into the
C	  year for both input DTGs.
C     2.  If the two years are different:
C	  a.  If NDTG < MDTG
C	      1)  Set swap flag on
C	      2)  Put MDTG's value in NDTG and vice versa.
C         b.  END IF
C         c.  Figure out whether this is a leap year or not and
C	      set IDX, the leap year subscript.
C	  d.  Add the remaining hours of MDTG to the hours of NDTG
C             and put the result in IHRS.
C	  e.  If there is more than one year between the two DTGs,
C	      add in those years' hours.
C	  f.  If the two values were swapped, change the sign of
C	      IHRS.
C     3.  If the two years are the same, use the difference between
C	  their hours-of-the-year.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                   1) from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C                   2) initialize ISTAT = 0
cx                  sampson, nrl 5/25/98   Change name to dtgdif2 for use in ATCF
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 NDTG, MDTG
C
      INTEGER IDIFF, IHOURS(4), IHRYRA, IHRYRB, IYEARA,
     *        IYEARB, LEAPCT, ITEMP, IDX, IREM, I, J, K, MOD,
     *        IDUM1, IDUM2, IDUM3, IDUM4, IHRS, ISTAT, JSTAT
C
      LOGICAL SWAP, LEAPYR
C
      DATA IHOURS / 8784, 3*8760 /
C
      ISTAT = 0
      SWAP = .FALSE.
C
      CALL DTGNUM ( MDTG, IYEARA, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRA,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C
      CALL DTGNUM ( NDTG, IYEARB, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRB,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C
      IF ( IYEARA .NE. IYEARB ) THEN
         IF ( IYEARA .LT. IYEARB ) THEN

**********************************************************************
*        The years are unequal.
*        Year A is less than year B.  Swap values so year A is
*          greater than year B.  Then use the "greater than"
*          section to do the calculation and change the sign of
*          the output hours.
**********************************************************************

            SWAP = .TRUE.
            ITEMP  = IYEARA
	    IYEARA = IYEARB
  	    IYEARB = ITEMP
c
            ITEMP  = IHRYRA
    	    IHRYRA = IHRYRB
	    IHRYRB = ITEMP
         END IF

**********************************************************************
*		Section that calulates the difference for
*		  years that are not the same.
*		Set leap year subscript. (If a century is not a leap
*		  year, we set it unequal to 1, arbitrarily choosing
*		  2.)
* 		Get the hours remaining in year B by subtracting
*		  its hours-into-the-year from the total hours
*		  in the year.
*	        Add them to the other year's hours, then change
*	          the sign of the result if necessary.
**********************************************************************

	 IDX = MOD (IYEARB, 4) + 1

	 IF ( MOD ( IYEARB, 100 ) . EQ. 0 ) THEN
	    IF ( MOD ( IYEARB, 400 ) .NE. 0 ) THEN
	       IDX = 2
 	    END IF
   	 ELSEIF (MOD ( IYEARB, 4 ) .EQ. 0 ) THEN
	    IDX = 1
	 END IF
C
         IREM = IHOURS(IDX) - IHRYRB
         IHRS = IREM + IHRYRA

**********************************************************************
*           The above logic will handle dates from YY010100 through
*	      123123 of the next year.  The next section handles cases
*             where the time difference is greater than that.
**********************************************************************

         IDIFF = IYEARA - IYEARB
         IF ( IDIFF .GT. 1 ) THEN

**********************************************************************
*	    Count the leap years in the set of years between the two
*	    given years.
**********************************************************************

            LEAPCT = 0
	    J = IYEARB
	    K = IYEARA
C
	    DO 10 I=J+1,K-1
               LEAPYR = .FALSE.
	       IF ( MOD ( I, 100 ) . EQ. 0 ) THEN
	          IF ( MOD ( I, 400 ) .EQ. 0 ) THEN
	             LEAPYR = .TRUE.
 	          END IF
	       ELSEIF (MOD ( I, 4 ) .EQ. 0 ) THEN
	          LEAPYR = .TRUE.
	       END IF
C
	       IF ( LEAPYR ) LEAPCT = LEAPCT + 1
   10 	    CONTINUE

**********************************************************************
*	    Add the additional years' hours to the total hours (IHRS).
*	    First, add leap hours in one year (IHOURS(1)) multiplied
*	      by the number of leap years (LEAPCT).
*	    Then, add non-leap hours in one year (IHOURS(2)) multiplied
*	      by the number of years between the given years
*	      (IDIFF-1) less the number of leap years (LEAPCT).
**********************************************************************

            IHRS = IHRS + (IHOURS(1) * LEAPCT)
	    IHRS = IHRS + (IHOURS(2) * (IDIFF-1 - LEAPCT))
	 END IF
C
         IF ( SWAP ) IHRS = -IHRS

      ELSE
**********************************************************************
*		Section that calculates the difference when the
*		two years are the same.
**********************************************************************

         IHRS = IHRYRA - IHRYRB
      END IF
C
 5000 RETURN
      END
      SUBROUTINE RLTLG(FLAT,FLNG,TLAT,TLNG,DIR,DST)
C
C  THIS IS A "GLTLG" SUBROUTINE; FROM POINT (FLAT,FLNG) AND DIR,DST
C   FINDS THE END POINT (TLAT,TLNG)
C   DIRECTION IS THE RHUMB-LINE DIRECTION.
C
C
      TLAT=0.
      TLNG=0.
      ICRS=DIR
      IF (ICRS.EQ.90.OR.ICRS.EQ.270) GO TO 150
      CRPD = DIR*3.1415926535898/180.
      TLAT = FLAT+(DST*COS(CRPD)/60.)
      IF (TLAT.GT.89.0) TLAT = 89.0
      IF (TLAT.LT.-89.0) TLAT = -89.0
      TD1 = TAN((45.0+(0.5*TLAT))*3.1415926535898/180.)
      TD2 = TAN((45.0+(0.5*FLAT))*3.1415926535898/180.)
      RLTD1 = ALOG(TD1)
      RLTD2 = ALOG(TD2)
      DENOM = RLTD1-RLTD2
      TLNG = FLNG-((TAN(CRPD)*DENOM)*180./3.1415926535898)
      GO TO 100
150   DLON = DST/(60.0*COS(FLAT*3.1415926535898/180.))
      IF (ICRS.EQ.90) TLNG = FLNG - DLON
      IF (ICRS.EQ.270) TLNG = FLNG + DLON
      TLAT = FLAT
100   ICRS=TLAT*10.+0.5
      TLAT=FLOAT(ICRS)/10.
      ICRS=TLNG*10.+0.5
      TLNG=FLOAT(ICRS)/10.
      END
      subroutine upcase (string,nchar)

c  this routine converts all lower case characters (ascii 97 - 122)
c  in the variable string to upper case characters (ascii 65 - 90).

      character string*(*)

c  loop thru each character in the string


c     call prctrc('upcase',.true.)

      do 100 i=1,nchar

c  if it is lower case, subtract 32 from it to make it upper case.

      ich = ichar(string(i:i))
      if ((ich .gt. 96) .and. (ich .lt. 123)) string(i:i) =
     &         char(ichar(string(i:i))-32)
  100 continue

c     call prctrc('upcase',.false.)

      return
      end
      subroutine dirdst( flat , flng , tlat , tlng , dir , dis )

cxxx  This routine calculates the direction (dir in degree 1.-360.) and
cxxx  distance (dis in n.m.) of two points.
cxxx  flat,flng:  from this lat,long the dis and dir are calculated
cxxx  tlat,tlng:    to this lat,long the dis and dir are calculated

cxxx  Longitude ranges from 1. to 360. Western Hemisphere.

ckpd  No, it doesn't.  Longitude ranges from 0.1 to 360.0; we do our
ckpd  positions to the tenth of a degree.  This oversight has spawned
ckpd  an amazing replication of erroneous direction modulus
ckpd  calculations thoughout MS-DOS ATCF.  See one example, below.

ckpd                   Input "from" point latitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           flat

ckpd                   Input "from" point longitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           flng

ckpd                   Input "to" point latitude, in degrees; carries at
ckpd                   least one significant decimal, may carry more.

      real*4           tlat

ckpd                   Input "to" point longitude, in degrees; carries
ckpd                   at least one significant decimal, may carry more.

      real*4           tlng

ckpd                   Output rhumb line direction of "from" - "to"
ckpd                   "vector" in degrees; by convention, value lies
ckpd                   between 0.1 and 360.0, but in fact extra
ckpd                   precision is carried until the value is printed.

ckpd                   Also used as a flag on input; if the value is
ckpd                   less than 1.0, the direction calculation is
ckpd                   skipped, saving some trig calls.

      real*4           dir

ckpd                   Output rhumb line distance of "from" - "to"
ckpd                   "vector" in nautical miles.  Display and archive
ckpd                   values are in whole miles, but additional
ckpd                   precision is carried through calculations.

      real*4           dis

ckpd                   The half-circumference of a circle in radians.

      real*4           pi

ckpd                   Conversion factor from degrees to radians.

      real*4           degrad

ckpd                   Conversion factor from radians to degrees.

      real*4           raddeg

ckpd                   Conversion factor from great circle degrees to
ckpd                   nautical miles.

      real*4           degnmi

ckpd                   Work variable in which the cosine angle of the
ckpd                   arc subtended by the rhumb line distance is
ckpd                   developed.

      real*4           ca

ckpd                   Work variable into which the sine angle of the
ckpd                   arc subtended by the rhumb line distance is
ckpd                   developed.

      real*4           sa

ckpd                   Work variable into which the absolute difference
ckpd                   in longitudes is calculated for a check to detect
ckpd                   a substantially north-south vector.

      real*4           delng

ckpd                   Work variable into which the radian difference in
ckpd                   longitudes in calculated.  Used in cosine angle
ckpd                   computation.

      real*4           angle

ckpd                   Work variable into which the mathematician's
ckpd                   compass version of the "from" latitude is
ckpd                   calculated.  Used in the cosine angle computation
ckpd                   and the direction computation.

      real*4           from

ckpd                   Work variable into which the mathematician's
ckpd                   compass version of the "to" latitude is
ckpd                   calculated.  Used in the cosine angle computation
ckpd                   and the direction computation.

      real*4           to

ckpd                   Flag to identify cases where longitudes are
ckpd                   converted to achieve the desired order for the
ckpd                   calculation, so that they can be converted back
ckpd                   before being returned to the calling program.

      logical*1        lngchg

ckpd  An amazing argument in favor of literate programming and parallel
ckpd  layouts of parallel steps.  The author typed in a value of pi each
ckpd  time it was used, and used different values of pi in the same
ckpd  calculation!

c  KPD  Since we write on the tlat,tlng variables, and someone might
c       well have passed an expression rather than a variable for
c  either, save our own lives by copying them to temporaries and using
c  the temporaries instead.


      real*4           myflng
      real*4           mytlng

c     call prctrc('dirdst',.true.)

      myflng = flng

      mytlng = tlng

      pi = acos( -1.0 )

      degrad = pi / 180.0

      raddeg = 180.0 / pi

      degnmi = 60.0

      lngchg = .false.

      if ( abs( myflng - mytlng ) .ge. 180.0 ) then

        lngchg = .true.

        myflng = amod( myflng + 180.0 , 360.0 )

        mytlng = amod( mytlng + 180.0 , 360.0 )

      endif

      delng = abs( myflng - mytlng )

ckpd  If the "from" - "to" vector is substantially north - south, ignore
ckpd  the small longitude contribution and take advantage of the cheap
ckpd  direct conversion of nautical miles to latitude degrees by a
ckpd  simple scaling computation.

      if ( delng .lt. 0.05 ) then

        dis = 60.0 * ( tlat - flat )

        dir = 360.0

        if ( dis .lt. 0.0 ) dir = 180.0

        dis = abs( dis )

      else

ckpd  Commented out code converted to standard software engineering
ckpd  "parallel code layout of parallel semantics" format to make the
ckpd  problem jump off the screen at the reader.

ckpd  150 from  = ( 90.0 - flat )         * 3.141592654     / 180.0

ckpd      to    = ( 90.0 - tlat )         * 3.1415926535898 / 180.0

ckpd      angle = abs( myflng - mytlng )      * 3.1415926535898 / 180.0

ckpd    Convert from mariners' compass, with zero at north and angles in
ckpd    degrees increasing clockwise, to mathematician's compass, with
ckpd    zero at east, and angles in radians increasing counterclockwise.

ckpd    This is needed because we forecast for mariners but do so using
ckpd    trig functions designed for mathematicians.

        from  = ( 90.0 - flat ) * degrad

        to    = ( 90.0 - tlat ) * degrad

        angle = abs( myflng - mytlng ) * degrad

ckpd    This is a truly wondrous way to achieve a result.  What I want
ckpd    to do is to take

ckpd      the square root of
ckpd      (
ckpd        (
ckpd          the square of
ckpd          (
ckpd            the latitude difference converted to nautical miles by
ckpd            simple scaling
ckpd          )
ckpd        )
ckpd        plus
ckpd        (
ckpd          the square of
ckpd          (
ckpd            the longitude difference in degrees converted to
ckpd            nautical miles by simple scaling and appropriately
ckpd            scaled down by the cosine of the mid-latitude [to adjust
ckpd            for degrees of longitude shrinking in nautical mile size
ckpd            away from the equator]
ckpd          )
ckpd        )
ckpd      )

ckpd    which I would then call the great circle distance between the
ckpd    points.

ckpd    This calculation achieves substantially the same results, at
ckpd    least for small difference vectors, and has almost the format of
ckpd    the above calculation, though with quite different values, but
ckpd    gives few clues as to how that correct result happens!

        ca =   cos( from ) * cos( to )
     &       + sin( from ) * sin( to ) * cos( angle )

ckpd      dis = acos( ca ) * 60.0 * 180.0 / 3.1415926535898

        dis = acos( ca ) * raddeg * degnmi

        dis = abs( dis )

ckpd    Check the flag value of dir before wasting time on an unwanted
ckpd    calculation.

        if ( dir .ge. 1.0 ) then

          sa = sqrt( 1.0 - ca * ca )

          dir = ( cos( to ) - cos( from ) * ca ) / ( sin( from ) * sa )

ckpd        dir = acos( dir ) *      180.0 / 3.1415926535898

          dir = acos( dir ) * raddeg

          if ( mytlng .gt. myflng ) dir = 360.0 - dir

ckpd  This code is done just this way, wrong, an amazing number of times
ckpd  thoughout MS-DOS ATCF, lending support to Dr. Nancy Levison's
ckpd  software engineering studies on the correlation of errors in
ckpd  "independently" developed software [to the point that agreement of
ckpd  independently coded computations of the same results from the same
ckpd  data gives little guarantee of correctness of the programs or of
ckpd  the results].

ckpd  What is _supposed_ to be happening here is that a direction of 0.0
ckpd  degrees is reserved for a "no interesting value" token, usually
ckpd  used when the corresponding distance is zero, while meaningful
ckpd  directions are encoded between 0.1 and 360.0.

ckpd  The problem arose because the various routine authors were
ckpd  thinking "greater than zero and less than or equal to three-sixty"
ckpd  in degrees instead of tenths of degrees, and ended up with an
ckpd  encoding range from 1.0 to 360.9, surely not what was intended!

ckpd  Errors like this are _rife_ throughout FNOC code, because "old
ckpd  school" FORTRAN programmers, who have never learned to incorporate
ckpd  good software engineering practices in their code, and hate the
ckpd  very thought, insist on "saving" bytes by omitting inline comments
ckpd  fully describing the calculation, and by typing as little as
ckpd  humanly possible, coding, in this case, real values as "3."
ckpd  instead of "3.0".

ckpd  By so doing, they forget, because they fail to remind themselves
ckpd  constantly with the menmonic "3.0" format in the code, that the
ckpd  data is carrying a significant digit to the right of the decimal
ckpd  place, and so code errors like this occur, that a moment's thought
ckpd  and a frequent reminder in the code would avoid.

ckpd        dir = dir + 359.0

ckpd        dir = amod( dir , 360.0 ) + 1.0

ckpd      Despite all that ranting, even this is imperfect, because the
ckpd      display angle is rarely used, while the full precision angle
ckpd      is passed from computation to computation often.  In the
ckpd      latter case, the result here is an angle between the binary
ckpd      values analogous to the decimal range 000.100 to 360.099.

ckpd      Better perhaps would be the range 000.05000+ to 360.050-,
ckpd      which would round correctly where intended for display use.

ckpd      Luckily the directions are generally handed to trig functions
ckpd      with values still very close to those functions' validated
ckpd      domains of (usually) minus two pi to two pi.

          dir = amod( ( dir + 359.9 ) , 360.0 ) + 0.1

        endif

      endif

c  KPD  This is now nonsense since we made a temporary copy for our use,
c       so don't do it any more.

ckpd  Check for messing with caller's data above, if so, restore it
ckpd  before returning.

ckpd  if ( lngchg ) then

ckpd    myflng = myflng - 180.0

ckpd    mytlng = mytlng - 180.0

ckpd    if ( myflng .le. 0.0 ) myflng = 360.0 + myflng

ckpd    if ( mytlng .le. 0.0 ) mytlng = 360.0 + mytlng

ckpd  endif

c     call prctrc('dirdst',.false.)

      return

ckpd  Notice that we managed to get here needing neither the goto
ckpd  statements nor the statement labels of the spagetti coded
ckpd  original.

      end

      subroutine get_tccor (iprob)
c
c     This routine uses the same logic as the strategic TC-CORs in nhc_wnd_probability.f
c     only with the site specific wind probabilities from nomograms.
c
      implicit none
      parameter ( maxtau = 10 )
      integer i
      integer maxtau
      integer iprob(0:maxtau)
      integer ipthresh(4)
      integer itccor
      integer itccorp
      character*6   tccor_time
      character*2   cpsave
      character*2   cprob(0:maxtau)
      character*9   tccor
      character*23  tccor_conf


c     atlantic thresholds from watch and warning database
      data ipthresh     / 12, 8, 6, 5 /

c
c    Get the previous tc-cor from output file, assumed to be previous run
c
                 call getprev ( itccorp )
c
c    Set tc-cor initially to 5 (none)
c
                 itccor=5
                 tccor='NONE     '
c
c**   Create the character representation of the station probability line
c
               do i = 0, 10
                  if ( iprob(i) .gt. 99 ) iprob(i) = 99
                  write ( cprob(i), '(i2)' ) iprob(i)
                  if ( cprob(i) .eq. ' 0' ) cprob(i) = ' X'
               enddo
c
c**   Get TC-COR settings, 50 kt
c
                 if (iprob(6).ge.ipthresh(4))
     &              itccor=4
                 if (iprob(5).ge.ipthresh(4))
     &              itccor=4
                 if (iprob(4).ge.ipthresh(3))
     &              itccor=3
                 if (iprob(3).ge.ipthresh(3))
     &              itccor=3
                 if (iprob(2).ge.ipthresh(2))
     &              itccor=2
                 if (iprob(1).ge.ipthresh(1))
     &              itccor=1
c    Leave a tccor on when there are still positive probabilities
                 if     (itccor.eq.5 .and. itccorp.eq.4) then
                         if (iprob(6).gt.0) itccor = 4
                         if (iprob(5).gt.0) itccor = 4
                 elseif (itccor.eq.5 .and. itccorp.eq.3) then
                         if (iprob(4).gt.0) itccor = 3
                         if (iprob(3).gt.0) itccor = 3
                 elseif (itccor.eq.5 .and. itccorp.eq.2) then
                         if (iprob(2).gt.0) itccor = 2
                 elseif (itccor.eq.5 .and. itccorp.eq.1) then
                         if (iprob(1).gt.0) itccor = 1
c    Also restrict tc-cor settings from going backwards
                 elseif (itccor.lt.5 .and. itccorp.lt.itccor) then
                         itccor = itccorp
                 endif
c    Determine the confidence for the tccor
                 if     (itccor.eq.5) then
                              cpsave= '  '
                              tccor_conf ="Continue To Monitor TC "
                              tccor_time ="      "
                 elseif (itccor.eq.4) then
                              cpsave=cprob(6)
                              tccor_conf ="Hold And Review TC-COR "
                              tccor_time ="at 72h"
                     if (iprob(6) .ge. ipthresh(4))
     &                        tccor_conf ="Consider Setting TC-COR"
                     if (iprob(6) .ge. ipthresh(4)*2)
     &                        tccor_conf ="TC-COR Should Be Set   "
                     if (iprob(6) .ge. ipthresh(4)*4)
     &                        tccor_conf ="Prepare For TC-COR 3   "
                 elseif (itccor.eq.3) then
                              cpsave=cprob(4)
                              tccor_conf ="Hold And Review TC-COR "
                              tccor_time ="at 48h"
                     if (iprob(4) .ge. ipthresh(3))
     &                        tccor_conf ="Consider Setting TC-COR"
                     if (iprob(4) .ge. ipthresh(3)*2)
     &                        tccor_conf ="TC-COR Should Be Set   "
                     if (iprob(4) .ge. ipthresh(3)*4)
     &                        tccor_conf ="Prepare For TC-COR 2   "
                 elseif (itccor.eq.2) then
                              cpsave=cprob(2)
                              tccor_conf ="Hold And Review TC-COR "
                              tccor_time ="at 24h"
                     if (iprob(2) .ge. ipthresh(2))
     &                        tccor_conf ="Consider Setting TC-COR"
                     if (iprob(2) .ge. ipthresh(2)*2)
     &                        tccor_conf ="TC-COR Should Be Set   "
                     if (iprob(2) .ge. ipthresh(2)*4)
     &                        tccor_conf ="Prepare For TC-COR 1   "
                 elseif (itccor.eq.1) then
                              cpsave=cprob(1)
                              tccor_conf ="Hold And Review TC-COR "
                              tccor_time ="at 12h"
                     if (iprob(1) .ge. ipthresh(1))
     &                        tccor_conf ="Consider Setting TC-COR"
                     if (iprob(1) .ge. ipthresh(1)*2)
     &                        tccor_conf ="TC-COR Should Be Set   "
                     if (iprob(1) .ge. ipthresh(1)*4)
     &                        tccor_conf ="50-KT Winds Probable   "
                     if (iprob(1) .ge. ipthresh(1)*6)
     &                        tccor_conf ="50-KT Winds Imminent   "
                 endif

c
c** Write character string to print
c

      write ( tccor, '(a7,i1)' ) "TC-COR=",itccor
      if ( itccor .eq. 5 ) then
              tccor = 'NO TC-COR'
              tccor_time = 'Is Low'
      endif
      write ( 25, *)"                                     "
      write ( 25, *)"-------------- TC-COR Guidance ------------ "
      write ( 25, '( 1x, a9, 2x,a7, a2, a6, 1x, a6)')
     &       tccor,"Reason:",cpsave,"% Prob",tccor_time
      write ( 25, '( 1x, a8,a23)')
     &        "Comment:", tccor_conf
      write (25,*) "-----------------------------------------------"
      write (25,*) "                                               "
      write ( 25, *)"Notes: This site-specific guidance is "
      write ( 25, *)"generated by running 1000 plausible forecasts"
      write ( 25, *)"(track and intensity) through site-specific"
      write ( 25, *)"nomograms and is for onset of 50-kt sustained"
      write ( 25, *)"winds at the site.  TC-COR guidance is derived "
      write ( 25, *)"from wind probability thresholds:"
      write ( 25, *)" TC-COR 4   5% Probability of 50 kt at 72h   "
      write ( 25, *)" TC-COR 3   6% Probability of 50 kt at 48h   "
      write ( 25, *)" TC-COR 2   8% Probability of 50 kt at 24h   "
      write ( 25, *)" TC-COR 1  12% Probability of 50 kt at 12h   "
      write ( 25, *)"The Comment is determined by whether probability ",
     &              "is 1x, 2x, 4x the threshold value.  The TC-CORs",
     &              "remain in place until probabilities drop to 0."

      close (25)
      return
      end

c*************************************************************************
      subroutine getprev (itccorpre)
c
c     This routine gets the previous tccor, assumed to be in previous <stormid>.<dtg>.wndprb.nomo.<station>
c     The file is already open as unit 28.
c
c     Output: itccorpre = prior TC-COR setting
c
      integer iunit
      integer itccorpre
      character*100 line
      itccorpre = 5
c
c**   Read the old TC-COR settings, if they exist
c
      do i = 1, 1000
         read (28, '(a80)', end=1010, err=1010) line
         if (line(2:8) .eq. "TC-COR=") then
              read (line(9:9), '(i1)', end=1010) itccorpre
              print *, 'Found Prior TC-COR=', line(9:9)
              return
         endif
      enddo
 1010 continue
      close (28)
      print *, ' Cant read prior TC-COR setting, continuing without'
      return
      end

