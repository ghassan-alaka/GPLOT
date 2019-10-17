subroutine xy2cyn_3d(dataxy,datacyn,im,jm,km,imc,jmc,icen,jcen,undef)
   implicit none
   Real(8):: pi_cnst = 3.141592653589793238462643383279502884
   real :: dataxy(im,jm,km)
   real :: datacyn(0:imc,0:jmc,km)
   real :: rcos(0:imc), rsin(0:imc)
   Logical:: defined,bad_cylin
   Integer :: im,jm,km,imc,jmc,kout,i,j,iloc,k,m,m1
   Integer :: icen,jcen,ibd,i1,i2,jbd,j1,j2,jloc,n,n1
   Real    :: undef,dlamda,xxx,aaa,bbb,yyy,ccc,ddd,eee
   dlamda = 2*pi_cnst/imc   !* azimuthal resolution  ( dr = yrlen )

   datacyn(:,:,:)=undef
   ibd = 3 ; i1 = 1 + ibd ; i2 = im - ibd
   jbd = 3 ; j1 = 1 + jbd ; j2 = jm - jbd
   bad_cylin = .false.    ; kout = km + 1

   DO i = 0, imc !* South-Polar Projection
       xxx = 1.5*pi_cnst - i*dlamda
      rcos(i) = cos(xxx) ; rsin(i) = sin(xxx)
   ENDDO

   DO j = 1, jmc
   DO i = 1, imc
      xxx = j*rcos(i) ; iloc = int(xxx) ; IF(xxx<0)iloc=iloc-1; aaa = xxx - iloc ; bbb = 1 - aaa
      yyy = j*rsin(i) ; jloc = int(yyy) ; IF(yyy<0)jloc=jloc-1; ccc = yyy - jloc ; ddd = 1 - ccc

      DO k = 1, km
         m = icen + iloc ; m1 = m + 1 ; n = jcen + jloc ; n1 = n + 1
         defined = (i1 <= m).AND.(m1 <= i2) .AND. (j1 <= n).AND.(n1 <= j2)
         IF ( .not. defined ) THEN
!            bad_cylin = .true. ; kout = min(k,kout) ; WRITE(0,*)'BAD CYLINDER',i,j,k;EXIT
            EXIT
         ENDIF
         eee = min( dataxy(m,n,k), dataxy(m,n1,k), dataxy(m1,n,k), dataxy(m1,n1,k) )
         IF ( eee > undef ) THEN !* ------------------------------------
            datacyn(i,j,k) = bbb*( ddd*dataxy(m ,n,k) + ccc*dataxy(m ,n1,k) ) &
                       + aaa*( ddd*dataxy(m1,n,k) + ccc*dataxy(m1,n1,k) )
         ELSE
!          WRITE(0,*)'PROBLEMS WITH HORIZONTAL INTERPOLATION',I,J,K
!          WRITE(0,*)dataxy(m,n,k),dataxy(m,n1,k),dataxy(m1,n,k),dataxy(m1,n1,k)
         ENDIF !* ------------------------------------------------------
      ENDDO
   ENDDO
   ENDDO
         do k=1,km
           datacyn(0,:,k)=datacyn(imc,:,k)
           datacyn(:,0,k)=dataxy(icen,jcen,k)
         enddo
END

subroutine xy2cyn_2d(dataxy,datacyn,im,jm,imc,jmc,icen,jcen,undef)
   implicit none
   Real(8):: pi_cnst = 3.141592653589793238462643383279502884
   real :: dataxy(im,jm)
   real :: datacyn(0:imc,0:jmc)
   real :: rcos(0:imc), rsin(0:imc)
   Logical:: defined,bad_cylin
   Integer :: im,jm,imc,jmc,i,j,iloc,m,m1
   Integer :: icen,jcen,ibd,i1,i2,jbd,j1,j2,jloc,n,n1
   Real    :: undef,dlamda,xxx,aaa,bbb,yyy,ccc,ddd,eee
   dlamda = 2*pi_cnst/imc   !* azimuthal resolution  ( dr = yrlen )

   datacyn(:,:)=undef
   ibd = 3 ; i1 = 1 + ibd ; i2 = im - ibd
   jbd = 3 ; j1 = 1 + jbd ; j2 = jm - jbd
   bad_cylin = .false.    ; 

   DO i = 0, imc !* South-Polar Projection
       xxx = 1.5*pi_cnst - i*dlamda
      rcos(i) = cos(xxx) ; rsin(i) = sin(xxx)
   ENDDO

   DO j = 1, jmc
   DO i = 1, imc
      xxx = j*rcos(i) ; iloc = int(xxx) ; IF(xxx<0)iloc=iloc-1; aaa = xxx - iloc ; bbb = 1 - aaa
      yyy = j*rsin(i) ; jloc = int(yyy) ; IF(yyy<0)jloc=jloc-1; ccc = yyy - jloc ; ddd = 1 - ccc

         m = icen + iloc ; m1 = m + 1 ; n = jcen + jloc ; n1 = n + 1
         defined = (i1 <= m).AND.(m1 <= i2) .AND. (j1 <= n).AND.(n1 <= j2)
         IF ( .not. defined ) THEN
            EXIT
         ENDIF
         eee = min( dataxy(m,n), dataxy(m,n1), dataxy(m1,n), dataxy(m1,n1) )
         IF ( eee > undef ) THEN !* ------------------------------------
            datacyn(i,j) = bbb*( ddd*dataxy(m ,n) + ccc*dataxy(m ,n1) ) &
                       + aaa*( ddd*dataxy(m1,n) + ccc*dataxy(m1,n1) )
         ELSE
!          WRITE(0,*)'PROBLEMS WITH HORIZONTAL INTERPOLATION',I,J,K
!          WRITE(0,*)dataxy(m,n,k),dataxy(m,n1,k),dataxy(m1,n,k),dataxy(m1,n1,k)
         ENDIF !* ------------------------------------------------------
      ENDDO
   ENDDO
           datacyn(0,:)=datacyn(imc,:)
           datacyn(:,0)=dataxy(icen,jcen)
END


subroutine xy2cyn_3d_vector(u,v,vt,vr,im,jm,km,imc,jmc,icen,jcen,undef)
   implicit none
   Real(8):: pi_cnst = 3.141592653589793238462643383279502884
   real :: u(im,jm,km),v(im,jm,km)
   real :: vr(0:imc,0:jmc,km),vt(0:imc,0:jmc,km)
   real :: rcos(0:imc), rsin(0:imc)
   Logical:: defined,bad_cylin
   Integer :: im,jm,km,imc,jmc
   Integer :: icen,jcen,ibd,i1,i2,jbd,j1,j2,kout,i,j,k,iloc,jloc,m,m1
   Integer :: n,n1
   Real    :: undef,dlamda,xxx,aaa,bbb,yyy,ccc,ddd,eee,uur,vvr
   dlamda = 2*pi_cnst/imc   !* azimuthal resolution  ( dr = yrlen )

   vt(:,:,:)=undef
   vr(:,:,:)=undef

   ibd = 3 ; i1 = 1 + ibd ; i2 = im - ibd
   jbd = 3 ; j1 = 1 + jbd ; j2 = jm - jbd
   bad_cylin = .false.    ; kout = km + 1

   DO i = 0, imc !* South-Polar Projection
       xxx = 1.5*pi_cnst - i*dlamda
      rcos(i) = cos(xxx) ; rsin(i) = sin(xxx)
   ENDDO

   DO j = 1, jmc
   DO i = 1, imc
      xxx = j*rcos(i) ; iloc = int(xxx) ; IF(xxx<0)iloc=iloc-1; aaa = xxx - iloc ; bbb = 1 - aaa
      yyy = j*rsin(i) ; jloc = int(yyy) ; IF(yyy<0)jloc=jloc-1; ccc = yyy - jloc ; ddd = 1 - ccc

      DO k = 1, km
         m = icen + iloc ; m1 = m + 1 ; n = jcen + jloc ; n1 = n + 1
         defined = (i1 <= m).AND.(m1 <= i2) .AND. (j1 <= n).AND.(n1 <= j2)
         IF ( .not. defined ) THEN
!            bad_cylin = .true. ; kout = min(k,kout) ; WRITE(0,*)'BAD CYLINDER',i,j,k;EXIT
            EXIT
         ENDIF
         eee = min( u(m,n,k), u(m,n1,k), u(m1,n,k), u(m1,n1,k) )
         IF ( eee > undef ) THEN !* ------------------------------------
                  uur  = bbb*( ddd*u(m ,n,k) + ccc*u(m ,n1,k) ) &
                       + aaa*( ddd*u(m1,n,k) + ccc*u(m1,n1,k) )
                  vvr  = bbb*( ddd*v(m ,n,k) + ccc*v(m ,n1,k) ) &
                       + aaa*( ddd*v(m1,n,k) + ccc*v(m1,n1,k) )
            vr(i,j,k) =   rcos(i)*uur + rsin(i)*vvr !*   radial   wind
            vt(i,j,k) = - rsin(i)*uur + rcos(i)*vvr !* tangential wind

         ELSE
!          WRITE(0,*)'PROBLEMS WITH HORIZONTAL INTERPOLATION',I,J,K
!          WRITE(0,*)u(m,n,k),u(m,n1,k),u(m1,n,k),u(m1,n1,k)
         ENDIF !* ------------------------------------------------------
      ENDDO
   ENDDO
   ENDDO
         do k=1,km
           vt(0,:,k)=vt(imc,:,k)
           vr(0,:,k)=vr(imc,:,k)
         enddo
END

subroutine xy2cyn_2d_vector(u,v,vt,vr,im,jm,imc,jmc,icen,jcen,undef)
   implicit none
   Real(8):: pi_cnst = 3.141592653589793238462643383279502884
   real :: u(im,jm),v(im,jm)
   real :: vr(0:imc,0:jmc),vt(0:imc,0:jmc)
   real :: rcos(0:imc), rsin(0:imc)
   Logical:: defined,bad_cylin
   Integer :: im,jm,imc,jmc
   Integer :: icen,jcen,ibd,i1,i2,jbd,j1,j2,i,j,iloc,jloc,m,m1
   Integer :: n,n1
   Real    :: undef,dlamda,xxx,aaa,bbb,yyy,ccc,ddd,eee,uur,vvr
   dlamda = 2*pi_cnst/imc   !* azimuthal resolution  ( dr = yrlen )

   vt(:,:)=undef
   vr(:,:)=undef

   ibd = 3 ; i1 = 1 + ibd ; i2 = im - ibd
   jbd = 3 ; j1 = 1 + jbd ; j2 = jm - jbd
   bad_cylin = .false.    ; 

   DO i = 0, imc !* South-Polar Projection
       xxx = 1.5*pi_cnst - i*dlamda
      rcos(i) = cos(xxx) ; rsin(i) = sin(xxx)
   ENDDO

   DO j = 1, jmc
   DO i = 1, imc
      xxx = j*rcos(i) ; iloc = int(xxx) ; IF(xxx<0)iloc=iloc-1; aaa = xxx - iloc ; bbb = 1 - aaa
      yyy = j*rsin(i) ; jloc = int(yyy) ; IF(yyy<0)jloc=jloc-1; ccc = yyy - jloc ; ddd = 1 - ccc

         m = icen + iloc ; m1 = m + 1 ; n = jcen + jloc ; n1 = n + 1
         defined = (i1 <= m).AND.(m1 <= i2) .AND. (j1 <= n).AND.(n1 <= j2)
         IF ( .not. defined ) THEN
!            bad_cylin = .true. ; kout = min(k,kout) ; WRITE(0,*)'BAD CYLINDER',i,j,k;EXIT
            EXIT
         ENDIF
         eee = min( u(m,n), u(m,n1), u(m1,n), u(m1,n1) )
         IF ( eee > undef ) THEN !* ------------------------------------
                  uur  = bbb*( ddd*u(m ,n) + ccc*u(m ,n1) ) &
                       + aaa*( ddd*u(m1,n) + ccc*u(m1,n1) )
                  vvr  = bbb*( ddd*v(m ,n) + ccc*v(m ,n1) ) &
                       + aaa*( ddd*v(m1,n) + ccc*v(m1,n1) )
            vr(i,j) =   rcos(i)*uur + rsin(i)*vvr !*   radial   wind
            vt(i,j) = - rsin(i)*uur + rcos(i)*vvr !* tangential wind

         ELSE
!          WRITE(0,*)'PROBLEMS WITH HORIZONTAL INTERPOLATION',I,J,K
!          WRITE(0,*)u(m,n,k),u(m,n1,k),u(m1,n,k),u(m1,n1,k)
         ENDIF !* ------------------------------------------------------
      ENDDO
   ENDDO
           vt(0,:)=vt(imc,:)
           vr(0,:)=vr(imc,:)
END
!______________________________________________________________________________


CALL xy2cyn_3d(data_in,data_out,im,jm,km,imc,jmc,icen,jcen,undef)
data_in: data in cartesian coordinate
data_out: data in cylindrical coordiante
data_in(im,jm,km)
data_out(0:imc,0:jmc,km)
icen,jcen: center location 
undef: missing value
