      subroutine splcal(plev,u,v,cx,cy,alpha,ml,
     +                  dw,w,ubard,vbard,pbard,ubar,vbar,pbar)

c     This routine calculates the weights w to determine the vertically
c     averaged horizontal wind that is as close as possible to
c     the storm motion cx,cy. The weights are used to determine the
c     pressure of the steering level.
c
c     Input:
c       plev        - 1-D array containing the pressure levels (mb)
c       u,v         - The enviromental wind at the levels plev
c       cx,cy       - The components of the storm motion vector
c       alpha       - The coefficient for contraining the steering
c                     weights so that they are not "too far" from
c                     the mass weighted average weights. 0.4 is recommended value.
c       ml          - The number of pressure levels
c
c     Output:
c       dw          - The weights for a mass-weighted average
c       w           - The weights for the optimal steering
c       ubard,vbard - The mass-weighted average horizontal wind
c       ubar,vbar   - The optimally weighted horizontal mean
c       pbard       - The mass-weighted pressure
c       pbar        - The optimally weighted pressure
c
      dimension plev(ml),u(ml),v(ml),dw(ml),w(ml)
c
c     Local variables
      parameter (np=12,mp=2)
c
      dimension a(np,np),b(np,mp)
c
c     Set ipen=1 for penalty in terms of (W-M) or 
c         ipen=2 for penalty in terms of (W/M-1)
      ipen=2
c
c     Make sure np is large enough
      if (ml .gt. np) then
         write(6,100)
  100    format(/,' np too small in routine wcal')
         stop
      endif
c
      n=ml-1
c
c     Calculate deep-layer mean weights
      call dlmw(plev,dw,ml)
c
c     Calculate column matrix on the RHS of the linear system for w
      uk = u(ml)
      vk = v(ml)
      dk= dw(ml)
c
      if (ipen .eq. 1) then
         do 10 k=1,n
            b(k,1) = (cx-uk)*(u(k)-uk) + (cy-vk)*(v(k)-vk) +
     +               alpha*(1.0 + dw(k) - dk)
   10    continue
      else
         do 11 k=1,n
            b(k,1) = (cx-uk)*(u(k)-uk) + (cy-vk)*(v(k)-vk) +
     +               alpha*(1.0/(dk*dk) + 1.0/dw(k) - 1.0/dk)
   11    continue
      endif
c
c     Calculate w coefficient matrix
      if (ipen .eq. 1) then
         do 15 j=1,n
         do 15 i=1,n
            if (i .eq. j) then
               ac = 2.0
            else
               ac = 1.0
            endif
c
            a(i,j) = (u(i)-uk)*(u(j)-uk) +
     +               (v(i)-vk)*(v(j)-vk) + ac*alpha
   15    continue
      else
         do 16 j=1,n
         do 16 i=1,n
            if (i .eq. j) then
               ac = (1.0/dk)**2 + (1.0/dw(j))**2
            else
               ac = (1.0/dk)**2
            endif
c
            a(i,j) = (u(i)-uk)*(u(j)-uk) +
     +               (v(i)-vk)*(v(j)-vk) + ac*alpha
   16    continue
      endif
c
c     Calculate optimal weights
      call gaussj(a,n,np,b,1,mp)
c
      do 30 i=1,n
         w(i) = b(i,1)
   30 continue
c
      w(ml) = 1.0
      do 40 i=1,n
         w(ml) = w(ml) - w(i)
   40 continue
c
c     Calculate vertically weighted variables
      ubard = 0.0
      vbard = 0.0
      ubar  = 0.0
      vbar  = 0.0
      pbard = 0.0
      pbar  = 0.0
      do 50 k=1,ml
         ubard = ubard + dw(k)*u(k)
         vbard = vbard + dw(k)*v(k)
         pbard = pbard + dw(k)*plev(k)
         ubar  = ubar  +  w(k)*u(k)
         vbar  = vbar  +  w(k)*v(k)
         pbar  = pbar  +  w(k)*plev(k)
   50 continue
c
      return
      end
      subroutine dlmw(plev,dw,ml)
      dimension plev(ml),dw(ml)
c
      if (ml .eq. 1) then
         dw(1) = 1.0
         return
      endif
c
      pdeep = plev(ml) - plev(1)
c
      dw( 1) = 0.5*(plev(2)-plev(1))/pdeep
      dw(ml) = 0.5*(plev(ml)-plev(ml-1))/pdeep
c
      if (ml .eq. 2) return
c
      do 10 k=2,ml-1
         dw(k) = 0.5*(plev(k+1)-plev(k-1))/pdeep
   10 continue
c
      return
      end
      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
      PARAMETER (NMAX=50)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                PAUSE 'Singular matrix'
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN
      END
