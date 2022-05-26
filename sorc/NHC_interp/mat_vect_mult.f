      subroutine mat_vect_mult(A,n,m,b,c)

c***  This subroutine computes c=A*b where A is
c***  an nxm matrix, b is a vector of length m, and
c***  c is a vector of length n.
c***
c***  Written by: Jim Hansen
c***  Last modified: February 2009

      implicit none

      integer i, j, n, m

      real A(n,m), B(m), C(n)
      real sumvar

      do i=1,n
         sumvar=0.
         do j=1,m
            sumvar=sumvar+A(i,j)*b(j)
         enddo
         c(i)=sumvar
      enddo

      return
      end
      