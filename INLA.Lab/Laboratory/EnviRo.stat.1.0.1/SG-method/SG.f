      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
c
      subroutine  dcopy(n,dx,incx,dy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end
c
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end

      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      DNRM2 = NORM
      RETURN
*
*     End of DNRM2.
*
      END
      subroutine dqrdc(x,ldx,n,p,qraux,jpvt,work,job)
      integer ldx,n,p,job
      integer jpvt(1)
      double precision x(ldx,1),qraux(1),work(1)
c
c     dqrdc uses householder transformations to compute the qr
c     factorization of an n by p matrix x.  column pivoting
c     based on the 2-norms of the reduced columns may be
c     performed at the users option.
c
c     on entry
c
c        x       double precision(ldx,p), where ldx .ge. n.
c                x contains the matrix whose decomposition is to be
c                computed.
c
c        ldx     integer.
c                ldx is the leading dimension of the array x.
c
c        n       integer.
c                n is the number of rows of the matrix x.
c
c        p       integer.
c                p is the number of columns of the matrix x.
c
c        jpvt    integer(p).
c                jpvt contains integers that control the selection
c                of the pivot columns.  the k-th column x(k) of x
c                is placed in one of three classes according to the
c                value of jpvt(k).
c
c                   if jpvt(k) .gt. 0, then x(k) is an initial
c                                      column.
c
c                   if jpvt(k) .eq. 0, then x(k) is a free column.
c
c                   if jpvt(k) .lt. 0, then x(k) is a final column.
c
c                before the decomposition is computed, initial columns
c                are moved to the beginning of the array x and final
c                columns to the end.  both initial and final columns
c                are frozen in place during the computation and only
c                free columns are moved.  at the k-th stage of the
c                reduction, if x(k) is occupied by a free column
c                it is interchanged with the free column of largest
c                reduced norm.  jpvt is not referenced if
c                job .eq. 0.
c
c        work    double precision(p).
c                work is a work array.  work is not referenced if
c                job .eq. 0.
c
c        job     integer.
c                job is an integer that initiates column pivoting.
c                if job .eq. 0, no pivoting is done.
c                if job .ne. 0, pivoting is done.
c
c     on return
c
c        x       x contains in its upper triangle the upper
c                triangular matrix r of the qr factorization.
c                below its diagonal x contains information from
c                which the orthogonal part of the decomposition
c                can be recovered.  note that if pivoting has
c                been requested, the decomposition is not that
c                of the original matrix x but that of x
c                with its columns permuted as described by jpvt.
c
c        qraux   double precision(p).
c                qraux contains further information required to recover
c                the orthogonal part of the decomposition.
c
c        jpvt    jpvt(k) contains the index of the column of the
c                original matrix that has been interchanged into
c                the k-th column, if pivoting was requested.
c
c     linpack. this version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dqrdc uses the following functions and subprograms.
c
c     blas daxpy,ddot,dscal,dswap,dnrm2
c     fortran dabs,dmax1,min0,dsqrt
c
c     internal variables
c
      integer j,jp,l,lp1,lup,maxj,pl,pu
      double precision maxnrm,dnrm2,tt
      double precision ddot,nrmxl,t
      logical negj,swapj
c
c
      pl = 1
      pu = 0
      if (job .eq. 0) go to 60
c
c        pivoting has been requested.  rearrange the columns
c        according to jpvt.
c
         do 20 j = 1, p
            swapj = jpvt(j) .gt. 0
            negj = jpvt(j) .lt. 0
            jpvt(j) = j
            if (negj) jpvt(j) = -j
            if (.not.swapj) go to 10
               if (j .ne. pl) call dswap(n,x(1,pl),1,x(1,j),1)
               jpvt(j) = jpvt(pl)
               jpvt(pl) = j
               pl = pl + 1
   10       continue
   20    continue
         pu = p
         do 50 jj = 1, p
            j = p - jj + 1
            if (jpvt(j) .ge. 0) go to 40
               jpvt(j) = -jpvt(j)
               if (j .eq. pu) go to 30
                  call dswap(n,x(1,pu),1,x(1,j),1)
                  jp = jpvt(pu)
                  jpvt(pu) = jpvt(j)
                  jpvt(j) = jp
   30          continue
               pu = pu - 1
   40       continue
   50    continue
   60 continue
c
c     compute the norms of the free columns.
c
      if (pu .lt. pl) go to 80
      do 70 j = pl, pu
         qraux(j) = dnrm2(n,x(1,j),1)
         work(j) = qraux(j)
   70 continue
   80 continue
c
c     perform the householder reduction of x.
c
      lup = min0(n,p)
      do 200 l = 1, lup
         if (l .lt. pl .or. l .ge. pu) go to 120
c
c           locate the column of largest norm and bring it
c           into the pivot position.
c
            maxnrm = 0.0d0
            maxj = l
            do 100 j = l, pu
               if (qraux(j) .le. maxnrm) go to 90
                  maxnrm = qraux(j)
                  maxj = j
   90          continue
  100       continue
            if (maxj .eq. l) go to 110
               call dswap(n,x(1,l),1,x(1,maxj),1)
               qraux(maxj) = qraux(l)
               work(maxj) = work(l)
               jp = jpvt(maxj)
               jpvt(maxj) = jpvt(l)
               jpvt(l) = jp
  110       continue
  120    continue
         qraux(l) = 0.0d0
         if (l .eq. n) go to 190
c
c           compute the householder transformation for column l.
c
            nrmxl = dnrm2(n-l+1,x(l,l),1)
            if (nrmxl .eq. 0.0d0) go to 180
               if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l))
               call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
c
c              apply the transformation to the remaining columns,
c              updating the norms.
c
               lp1 = l + 1
               if (p .lt. lp1) go to 170
               do 160 j = lp1, p
                  t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
                  call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
                  if (j .lt. pl .or. j .gt. pu) go to 150
                  if (qraux(j) .eq. 0.0d0) go to 150
                     tt = 1.0d0 - (dabs(x(l,j))/qraux(j))**2
                     tt = dmax1(tt,0.0d0)
                     t = tt
                     tt = 1.0d0 + 0.05d0*tt*(qraux(j)/work(j))**2
                     if (tt .eq. 1.0d0) go to 130
                        qraux(j) = qraux(j)*dsqrt(t)
                     go to 140
  130                continue
                        qraux(j) = dnrm2(n-l,x(l+1,j),1)
                        work(j) = qraux(j)
  140                continue
  150             continue
  160          continue
  170          continue
c
c              save the transformation.
c
               qraux(l) = x(l,l)
               x(l,l) = -nrmxl
  180       continue
  190    continue
  200 continue
      return
      end
      subroutine dqrsl(x,ldx,n,k,qraux,y,qy,qty,b,rsd,xb,job,info)
      integer ldx,n,k,job,info
      double precision x(ldx,1),qraux(1),y(1),qy(1),qty(1),b(1),rsd(1),
     *                 xb(1)
c
c     dqrsl applies the output of dqrdc to compute coordinate
c     transformations, projections, and least squares solutions.
c     for k .le. min(n,p), let xk be the matrix
c
c            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k)))
c
c     formed from columnns jpvt(1), ... ,jpvt(k) of the original
c     n x p matrix x that was input to dqrdc (if no pivoting was
c     done, xk consists of the first k columns of x in their
c     original order).  dqrdc produces a factored orthogonal matrix q
c     and an upper triangular matrix r such that
c
c              xk = q * (r)
c                       (0)
c
c     this information is contained in coded form in the arrays
c     x and qraux.
c
c     on entry
c
c        x      double precision(ldx,p).
c               x contains the output of dqrdc.
c
c        ldx    integer.
c               ldx is the leading dimension of the array x.
c
c        n      integer.
c               n is the number of rows of the matrix xk.  it must
c               have the same value as n in dqrdc.
c
c        k      integer.
c               k is the number of columns of the matrix xk.  k
c               must nnot be greater than min(n,p), where p is the
c               same as in the calling sequence to dqrdc.
c
c        qraux  double precision(p).
c               qraux contains the auxiliary output from dqrdc.
c
c        y      double precision(n)
c               y contains an n-vector that is to be manipulated
c               by dqrsl.
c
c        job    integer.
c               job specifies what is to be computed.  job has
c               the decimal expansion abcde, with the following
c               meaning.
c
c                    if a.ne.0, compute qy.
c                    if b,c,d, or e .ne. 0, compute qty.
c                    if c.ne.0, compute b.
c                    if d.ne.0, compute rsd.
c                    if e.ne.0, compute xb.
c
c               note that a request to compute b, rsd, or xb
c               automatically triggers the computation of qty, for
c               which an array must be provided in the calling
c               sequence.
c
c     on return
c
c        qy     double precision(n).
c               qy conntains q*y, if its computation has been
c               requested.
c
c        qty    double precision(n).
c               qty contains trans(q)*y, if its computation has
c               been requested.  here trans(q) is the
c               transpose of the matrix q.
c
c        b      double precision(k)
c               b contains the solution of the least squares problem
c
c                    minimize norm2(y - xk*b),
c
c               if its computation has been requested.  (note that
c               if pivoting was requested in dqrdc, the j-th
c               component of b will be associated with column jpvt(j)
c               of the original matrix x that was input into dqrdc.)
c
c        rsd    double precision(n).
c               rsd contains the least squares residual y - xk*b,
c               if its computation has been requested.  rsd is
c               also the orthogonal projection of y onto the
c               orthogonal complement of the column space of xk.
c
c        xb     double precision(n).
c               xb contains the least squares approximation xk*b,
c               if its computation has been requested.  xb is also
c               the orthogonal projection of y onto the column space
c               of x.
c
c        info   integer.
c               info is zero unless the computation of b has
c               been requested and r is exactly singular.  in
c               this case, info is the index of the first zero
c               diagonal element of r and b is left unaltered.
c
c     the parameters qy, qty, b, rsd, and xb are not referenced
c     if their computation is not requested and in this case
c     can be replaced by dummy variables in the calling program.
c     to save storage, the user may in some cases use the same
c     array for different parameters in the calling sequence.  a
c     frequently occuring example is when one wishes to compute
c     any of b, rsd, or xb and does not need y or qty.  in this
c     case one may identify y, qty, and one of b, rsd, or xb, while
c     providing separate arrays for anything else that is to be
c     computed.  thus the calling sequence
c
c          call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info)
c
c     will result in the computation of b and rsd, with rsd
c     overwriting y.  more generally, each item in the following
c     list contains groups of permissible identifications for
c     a single callinng sequence.
c
c          1. (y,qty,b) (rsd) (xb) (qy)
c
c          2. (y,qty,rsd) (b) (xb) (qy)
c
c          3. (y,qty,xb) (b) (rsd) (qy)
c
c          4. (y,qy) (qty,b) (rsd) (xb)
c
c          5. (y,qy) (qty,rsd) (b) (xb)
c
c          6. (y,qy) (qty,xb) (b) (rsd)
c
c     in any group the value returned in the array allocated to
c     the group corresponds to the last member of the group.
c
c     linpack. this version dated 08/14/78 .
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dqrsl uses the following functions and subprograms.
c
c     blas daxpy,dcopy,ddot
c     fortran dabs,min0,mod
c
c     internal variables
c
      integer i,j,jj,ju,kp1
      double precision ddot,t,temp
      logical cb,cqy,cqty,cr,cxb
c
c
c     set info flag.
c
      info = 0
c
c     determine what is to be computed.
c
      cqy = job/10000 .ne. 0
      cqty = mod(job,10000) .ne. 0
      cb = mod(job,1000)/100 .ne. 0
      cr = mod(job,100)/10 .ne. 0
      cxb = mod(job,10) .ne. 0
      ju = min0(k,n-1)
c
c     special action when n=1.
c
      if (ju .ne. 0) go to 40
         if (cqy) qy(1) = y(1)
         if (cqty) qty(1) = y(1)
         if (cxb) xb(1) = y(1)
         if (.not.cb) go to 30
            if (x(1,1) .ne. 0.0d0) go to 10
               info = 1
            go to 20
   10       continue
               b(1) = y(1)/x(1,1)
   20       continue
   30    continue
         if (cr) rsd(1) = 0.0d0
      go to 250
   40 continue
c
c        set up to compute qy or qty.
c
         if (cqy) call dcopy(n,y,1,qy,1)
         if (cqty) call dcopy(n,y,1,qty,1)
         if (.not.cqy) go to 70
c
c           compute qy.
c
            do 60 jj = 1, ju
               j = ju - jj + 1
               if (qraux(j) .eq. 0.0d0) go to 50
                  temp = x(j,j)
                  x(j,j) = qraux(j)
                  t = -ddot(n-j+1,x(j,j),1,qy(j),1)/x(j,j)
                  call daxpy(n-j+1,t,x(j,j),1,qy(j),1)
                  x(j,j) = temp
   50          continue
   60       continue
   70    continue
         if (.not.cqty) go to 100
c
c           compute trans(q)*y.
c
            do 90 j = 1, ju
               if (qraux(j) .eq. 0.0d0) go to 80
                  temp = x(j,j)
                  x(j,j) = qraux(j)
                  t = -ddot(n-j+1,x(j,j),1,qty(j),1)/x(j,j)
                  call daxpy(n-j+1,t,x(j,j),1,qty(j),1)
                  x(j,j) = temp
   80          continue
   90       continue
  100    continue
c
c        set up to compute b, rsd, or xb.
c
         if (cb) call dcopy(k,qty,1,b,1)
         kp1 = k + 1
         if (cxb) call dcopy(k,qty,1,xb,1)
         if (cr .and. k .lt. n) call dcopy(n-k,qty(kp1),1,rsd(kp1),1)
         if (.not.cxb .or. kp1 .gt. n) go to 120
            do 110 i = kp1, n
               xb(i) = 0.0d0
  110       continue
  120    continue
         if (.not.cr) go to 140
            do 130 i = 1, k
               rsd(i) = 0.0d0
  130       continue
  140    continue
         if (.not.cb) go to 190
c
c           compute b.
c
            do 170 jj = 1, k
               j = k - jj + 1
               if (x(j,j) .ne. 0.0d0) go to 150
                  info = j
c           ......exit
                  go to 180
  150          continue
               b(j) = b(j)/x(j,j)
               if (j .eq. 1) go to 160
                  t = -b(j)
                  call daxpy(j-1,t,x(1,j),1,b,1)
  160          continue
  170       continue
  180       continue
  190    continue
         if (.not.cr .and. .not.cxb) go to 240
c
c           compute rsd or xb as required.
c
            do 230 jj = 1, ju
               j = ju - jj + 1
               if (qraux(j) .eq. 0.0d0) go to 220
                  temp = x(j,j)
                  x(j,j) = qraux(j)
                  if (.not.cr) go to 200
                     t = -ddot(n-j+1,x(j,j),1,rsd(j),1)/x(j,j)
                     call daxpy(n-j+1,t,x(j,j),1,rsd(j),1)
  200             continue
                  if (.not.cxb) go to 210
                     t = -ddot(n-j+1,x(j,j),1,xb(j),1)/x(j,j)
                     call daxpy(n-j+1,t,x(j,j),1,xb(j),1)
  210             continue
                  x(j,j) = temp
  220          continue
  230       continue
  240    continue
  250 continue
      return
      end

      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end

      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine bgcopy(nn,grid,fldmag,nstore,ngrid,wstor,wfmag)
      integer nn,nstore,ngrid,i1
      real grid(2,nn),fldmag(nn),wstor(2,nn),wfmag(nn)
      i1=1 
23000 if(.not.(i1 .le. nstore ))goto 23002
      if( (ngrid+i1) .gt. nn)then
      nn = 999
      nstore = i1 - 1
      goto 23002
      endif
      grid(1,ngrid+i1) = wstor(1,i1)
      grid(2,ngrid+i1) = wstor(2,i1)
      fldmag(ngrid+i1) = wfmag(i1)
23001 i1=i1+1
      goto 23000
23002 continue
      ngrid = ngrid + nstore
      return
      end

C Output from Public domain Ratfor, version 1.0
C NL: Modified to deal with single precision required by this routine
C     new declaration: datain1, coefin1, grid1, wstor3,wstor4
C     - changing the types before and after the call- marked by **!
C Old call
C     subroutine bgrid(nn,start,xylim,datain,datlen,coefin,coefdim,grid,
C    *ngrid,fldmag, iterlim,perpc,wstor1,wstor2,wfmag,warc1,warc2)
C New call
      subroutine bgrid(nn,start,xylim,datain1,datlen,coefin1,coefdim,
     *grid1,ngrid,fldmag,iterlim,perpc,wstor3,wstor4,wfmag,warc1,warc2)
      integer nn,datlen,coefdim,ngrid,iterlim,perpc,xlen,dcnt,n(2),i1,i2
      integer nstore,nl1,nl2, nl3
      double precision datain1(datlen,2),coefin1(datlen+3,coefdim),
     *grid1(2,nn),wstor3(2,nn),wstor4(2,nn)
      real start(2),xylim(4),grid(2,nn),fldmag(nn)
      real datain(datlen,2),coefin(datlen+3,coefdim)
      real wstor1(2,nn),wstor2(2,nn),wfmag(nn),warc1(nn),warc2(nn)
      real xdata(1000,2),spcoef(1003,5)
      logical mindir
      common /spinfo/ xlen, dcnt, xdata, spcoef
      common /finteg/ mindir
C **
      do 800 nl1 = 1,datlen
      do 801 nl2 = 1,2 
         datain(nl1,nl2) = datain1(nl1,nl2)
801   continue
800   continue
      nl3 = datlen+3
      do 802 nl1 = 1,nl3
      do 803 nl2 = 1,coefdim
         coefin(nl1,nl2) = coefin1(nl1,nl2)
803   continue
802   continue
      do 804 nl1 = 1,2
      do 805 nl2 = 1,nn
         grid(nl1,nl2) = 1.0*grid1(nl1,nl2)
         wstor1(nl1,nl2)= wstor3(nl1,nl2)
         wstor2(nl1,nl2)= wstor4(nl1,nl2)
805   continue
804   continue
C **
      xlen = datlen
      dcnt = min(coefdim,5)
      do 23000 i1 = 1,xlen
      xdata(i1,1) = datain(i1,1)
      xdata(i1,2) = datain(i1,2)
      do 23002 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23002 continue
23003 continue
23000 continue
23001 continue
      do 23004 i1 = xlen+1,xlen+3
      do 23006 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23006 continue
23007 continue
23004 continue
23005 continue
      mindir = .false.
      ngrid = 0
      call curv(nn,start,xylim,dcnt,wstor1,warc1,nstore,n,wfmag,iterlim)
      call bgcopy(nn,grid,fldmag,nstore,ngrid,wstor1,wfmag)
      mindir = .true.
      call gperp(nn,wstor1,xylim,dcnt,warc1,n,grid,ngrid,fldmag,iterlim,
     * perpc,wstor2,wfmag,warc2)
      call curv(nn,start,xylim,dcnt,wstor1,warc1,nstore,n,wfmag,iterlim)
      call bgcopy(nn,grid,fldmag,nstore,ngrid,wstor1,wfmag)
      mindir = .false.
      call gperp(nn,wstor1,xylim,dcnt,warc1,n,grid,ngrid,fldmag,iterlim,
     * perpc,wstor2,wfmag,warc2)
C **
      do 900 nl1 = 1,datlen
      do 901 nl2 = 1,2
         datain1(nl1,nl2) = datain(nl1,nl2)
901   continue
900   continue
      do 902 nl1 = 1,nl3
      do 903 nl2 = 1,coefdim
         coefin1(nl1,nl2) = coefin(nl1,nl2)
903   continue
902   continue
      do 904 nl1 = 1,2
      do 905 nl2 = 1,nn
         grid1(nl1,nl2) = grid(nl1,nl2)
         wstor3(nl1,nl2)= wstor1(nl1,nl2)
         wstor4(nl1,nl2)= wstor2(nl1,nl2)
905   continue
904   continue
C **
      return
      end

      subroutine gperp(nn,wstor1,xylim,ddim,warc1,n,grid,ngrid,fldmag,it
     *erlim, perpc,wstor2,wfmag,warc2)
      integer nn,ddim,n(2),ngrid,n1(2),iterlim,perpc,lower,i1,i2
      real wstor1(2,nn),xylim(4),warc1(nn),grid(2,nn),fldmag(nn)
      real wstor2(2,nn),wfmag(nn),warc2(nn),dist,spacing
      logical mindir
      common /finteg/ mindir
      spacing = ( warc1(n(2)) + warc1(n(1)) ) / perpc
      lower = 2
      do 23008 i1=1,2
      dist = 0.0
      do 23010 i2=lower,n(i1)
      dist = dist + warc1(i2) - warc1(i2-1)
      if(dist .ge. spacing)then
      call curv(nn,wstor1(1,i2),xylim,ddim,wstor2,warc2,nstor,n1, wfmag,
     *iterlim)
      call bgcopy(nn,grid,fldmag,nstor,ngrid,wstor2,wfmag)
      dist = 0.0
      endif
23010 continue
23011 continue
      lower = n(1) + 3
23008 continue
23009 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine curv(nn,start,xylim,ddim,line,arclen,nline,n,fldmag,ite
     *rlim)
      integer nn,ddim,nline,n(2),iterlim,neqn,iflag,iwork(5),check,m0,in
     *d,i1,i2,i3
      real start(2),xylim(4),line(2,nn),arclen(nn),fldmag(nn)
      double precision ad(5,2),evec(2,2),eval(2)
      real y(2),t,tout,work(15),relerr,abserr
      real oyp(2),fmag,tstep,oyp0(2),fmag0,mag
      logical inregion,mindir,sing
      external f
      common /fcurv/ oyp, fmag, check
      common /finteg/ mindir
      data neqn/2/
      relerr = 1.0e-4
      abserr = 1.0e-9
      tstep = (xylim(2)-xylim(1)) / 20.0
      call tform(start,ad)
      call eigvec(ad,ddim,2,evec,eval,check)
      if(check .ne. 0)then
      return
      endif
      if((eval(1) .gt. eval(2) .and. mindir) .or. ( eval(1) .le. eval(2)
     * .and. .not.mindir))then
      i1 = 2
      else
      i1 = 1
      endif
      oyp0(1) = (evec(1,i1))
      oyp0(2) = (evec(2,i1))
      fmag0 = (eval(i1))
      m0 = 1
      do 23004 ind = 1,2 
      sing = .false.
      t = 0.0
      tout = tstep
      y(1) = start(1)
      y(2) = start(2)
      oyp(1) = oyp0(1)
      oyp(2) = oyp0(2)
      fmag = fmag0
      iflag = -1
      i2 = m0
23006 if(inregion(y,xylim) .and. (i2.lt.nn))then
      line(1,i2) = y(1)
      line(2,i2) = y(2)
      fldmag(i2) = fmag
      arclen(i2) = t
      call rkf45(f,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
      if(check .ne. 0)then
      goto 23007
      endif
      if(iflag .eq. 6)then
      relerr = relerr * 1.5
      abserr = abserr * 1.5
      endif
      if( ( i2 .gt. (iterlim+m0) ) .and. (iflag .ne. 6) .and. ( (arclen(
     *i2)-arclen(i2-1)) .lt. (arclen(i2-1)-arclen(i2-2)) ) )then
      sing = arclen(i2) - arclen(i2-iterlim) .lt. tstep
      endif
      if((iflag .eq. 7) .or. sing)then
      i3 = i2 - 1
23016 if((i3.gt.m0) .and. (arclen(i2) - arclen(i3) .le. tstep))then
      i3 = i3-1
      goto 23016
      endif
23017 continue
      oyp(1) = line(1,i2) - line(1,i3)
      oyp(2) = line(2,i2) - line(2,i3)
      y(1) = line(1,i2) + oyp(1)
      y(2) = line(2,i2) + oyp(2)
      mag = abs(cmplx(oyp(1),oyp(2)))
      if(mag .ne. 0.0)then
      oyp(1) = oyp(1) / mag
      oyp(2) = oyp(2) / mag
      endif
      t = t + arclen(i2) - arclen(i3)
      i2 = i2 + 1
      m0 = i2 + 1
      line(1,i2) = 999.0
      line(2,i2) = 999.0
      fldmag(i2) = 999.0
      arclen(i2) = arclen(i2-1)
      sing = .false.
      endif
      iflag = -2
      tout = t + tstep
      i2 = i2 + 1
      goto 23006
      endif
23007 continue
      n(ind) = i2 - 1
      line(1,i2) = 999.0
      line(2,i2) = 999.0
      fldmag(i2) = 999.0
      arclen(i2) = 0.0
      m0 = n(1) + 2
      oyp0(1) = -oyp0(1)
      oyp0(2) = -oyp0(2)
23004 continue
23005 continue
      nline = min(nn,i2)
      return
      end
      subroutine f(t,y,yp)
      real t,y(2),yp(2),oyp(2),fmag,t1
      double precision ad(5,2),evec(2,2),eval(2)
      integer xlen,dcnt,check,i1
      real xdata(1000,2),spcoef(1003,5)
      logical mindir
      common /spinfo/ xlen, dcnt, xdata, spcoef
      common /fcurv/ oyp, fmag, check
      common /finteg/ mindir
      call tform(y,ad)
      call eigvec(ad,dcnt,2,evec,eval,check)
      if(check .ne. 0)then
      yp(1) = 0.0
      yp(2) = 0.0
      else
      if((eval(1) .gt. eval(2) .and. mindir) .or. (eval(1) .lt. eval(2) 
     *.and. .not. mindir))then
      i1 = 2
      else
      i1 = 1
      endif
      yp(1) = (evec(1,i1))
      yp(2) = (evec(2,i1))
      fmag = (eval(i1))
      t1 = oyp(1)*yp(1) + oyp(2)*yp(2)
      if(t1 .lt. 0.0)then
      yp(1) = -yp(1)
      yp(2) = -yp(2)
      endif
      oyp(1) = yp(1)
      oyp(2) = yp(2)
      endif
      return
      end
      SUBROUTINE DSPFA(AP,N,KPVT,INFO)
C***BEGIN PROLOGUE  DSPFA
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2B1A
C***KEYWORDS  DOUBLE PRECISION,FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX,
C             PACKED,SYMMETRIC
C***AUTHOR  BUNCH, J., (UCSD)
C***PURPOSE  Factors a double precision SYMMETRIC matrix stored in
C            packed form by elimination with symmetric pivoting.
C***DESCRIPTION
C
C     DSPFA factors a double precision symmetric matrix stored in
C     packed form by elimination with symmetric pivoting.
C
C     To solve  A*X = B , follow DSPFA by DSPSL.
C     To compute  INVERSE(A)*C , follow DSPFA by DSPSL.
C     To compute  DETERMINANT(A) , follow DSPFA by DSPDI.
C     To compute  INERTIA(A) , follow DSPFA by DSPDI.
C     To compute  INVERSE(A) , follow DSPFA by DSPDI.
C
C     On Entry
C
C        AP      DOUBLE PRECISION (N*(N+1)/2)
C                the packed form of a symmetric matrix  A .  The
C                columns of the upper triangle are stored sequentially
C                in a one-dimensional array of length  N*(N+1)/2 .
C                See comments below for details.
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     Output
C
C        AP      a block diagonal matrix and the multipliers which
C                were used to obtain it stored in packed form.
C                The factorization can be written  A = U*D*TRANS(U)
C                where  U  is a product of permutation and unit
C                upper triangular matrices, TRANS(U) is the
C                transpose of  U , and  D  is block diagonal
C                with 1 by 1 and 2 by 2 blocks.
C
C        KPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  if the K-th pivot block is singular.  This is
C                     not an error condition for this subroutine,
C                     but it does indicate that DSPSL or DSPDI may
C                     divide by zero if called.
C
C     Packed Storage
C
C          The following program segment will pack the upper
C          triangle of a symmetric matrix.
C
C                K = 0
C                DO 20 J = 1, N
C                   DO 10 I = 1, J
C                      K = K + 1
C                      AP(K)  = A(I,J)
C             10    CONTINUE
C             20 CONTINUE
C
C     LINPACK.  This version dated 08/14/78 .
C     James Bunch, Univ. Calif. San Diego, Argonne Nat. Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DSWAP,IDAMAX
C     Fortran DABS,DMAX1,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DSWAP,IDAMAX
C***END PROLOGUE  DSPFA
      INTEGER N,KPVT(1),INFO
      DOUBLE PRECISION AP(1)
C
      DOUBLE PRECISION AK,AKM1,BK,BKM1,DENOM,MULK,MULKM1,T
      DOUBLE PRECISION ABSAKK,ALPHA,COLMAX,ROWMAX
      INTEGER IDAMAX,IJ,IJJ,IK,IKM1,IM,IMAX,IMAXP1,IMIM,IMJ,IMK
      INTEGER J,JJ,JK,JKM1,JMAX,JMIM,K,KK,KM1,KM1K,KM1KM1,KM2,KSTEP
      LOGICAL SWAP
C
C     INITIALIZE
C
C     ALPHA IS USED IN CHOOSING PIVOT BLOCK SIZE.
C***FIRST EXECUTABLE STATEMENT  DSPFA
      ALPHA = (1.0D0 + DSQRT(17.0D0))/8.0D0
C
      INFO = 0
C
C     MAIN LOOP ON K, WHICH GOES FROM N TO 1.
C
      K = N
      IK = (N*(N - 1))/2
   10 CONTINUE
C
C        LEAVE THE LOOP IF K=0 OR K=1.
C
C     ...EXIT
         IF (K .EQ. 0) GO TO 200
         IF (K .GT. 1) GO TO 20
            KPVT(1) = 1
            IF (AP(1) .EQ. 0.0D0) INFO = 1
C     ......EXIT
            GO TO 200
   20    CONTINUE
C
C        THIS SECTION OF CODE DETERMINES THE KIND OF
C        ELIMINATION TO BE PERFORMED.  WHEN IT IS COMPLETED,
C        KSTEP WILL BE SET TO THE SIZE OF THE PIVOT BLOCK, AND
C        SWAP WILL BE SET TO .TRUE. IF AN INTERCHANGE IS
C        REQUIRED.
C
         KM1 = K - 1
         KK = IK + K
         ABSAKK = DABS(AP(KK))
C
C        DETERMINE THE LARGEST OFF-DIAGONAL ELEMENT IN
C        COLUMN K.
C
         IMAX = IDAMAX(K-1,AP(IK+1),1)
         IMK = IK + IMAX
         COLMAX = DABS(AP(IMK))
         IF (ABSAKK .LT. ALPHA*COLMAX) GO TO 30
            KSTEP = 1
            SWAP = .FALSE.
         GO TO 90
   30    CONTINUE
C
C           DETERMINE THE LARGEST OFF-DIAGONAL ELEMENT IN
C           ROW IMAX.
C
            ROWMAX = 0.0D0
            IMAXP1 = IMAX + 1
            IM = IMAX*(IMAX - 1)/2
            IMJ = IM + 2*IMAX
            DO 40 J = IMAXP1, K
               ROWMAX = DMAX1(ROWMAX,DABS(AP(IMJ)))
               IMJ = IMJ + J
   40       CONTINUE
            IF (IMAX .EQ. 1) GO TO 50
               JMAX = IDAMAX(IMAX-1,AP(IM+1),1)
               JMIM = JMAX + IM
               ROWMAX = DMAX1(ROWMAX,DABS(AP(JMIM)))
   50       CONTINUE
            IMIM = IMAX + IM
            IF (DABS(AP(IMIM)) .LT. ALPHA*ROWMAX) GO TO 60
               KSTEP = 1
               SWAP = .TRUE.
            GO TO 80
   60       CONTINUE
            IF (ABSAKK .LT. ALPHA*COLMAX*(COLMAX/ROWMAX)) GO TO 70
               KSTEP = 1
               SWAP = .FALSE.
            GO TO 80
   70       CONTINUE
               KSTEP = 2
               SWAP = IMAX .NE. KM1
   80       CONTINUE
   90    CONTINUE
         IF (DMAX1(ABSAKK,COLMAX) .NE. 0.0D0) GO TO 100
C
C           COLUMN K IS ZERO.  SET INFO AND ITERATE THE LOOP.
C
            KPVT(K) = K
            INFO = K
         GO TO 190
  100    CONTINUE
         IF (KSTEP .EQ. 2) GO TO 140
C
C           1 X 1 PIVOT BLOCK.
C
            IF (.NOT.SWAP) GO TO 120
C
C              PERFORM AN INTERCHANGE.
C
               CALL DSWAP(IMAX,AP(IM+1),1,AP(IK+1),1)
               IMJ = IK + IMAX
               DO 110 JJ = IMAX, K
                  J = K + IMAX - JJ
                  JK = IK + J
                  T = AP(JK)
                  AP(JK) = AP(IMJ)
                  AP(IMJ) = T
                  IMJ = IMJ - (J - 1)
  110          CONTINUE
  120       CONTINUE
C
C           PERFORM THE ELIMINATION.
C
            IJ = IK - (K - 1)
            DO 130 JJ = 1, KM1
               J = K - JJ
               JK = IK + J
               MULK = -AP(JK)/AP(KK)
               T = MULK
               CALL DAXPY(J,T,AP(IK+1),1,AP(IJ+1),1)
               IJJ = IJ + J
               AP(JK) = MULK
               IJ = IJ - (J - 1)
  130       CONTINUE
C
C           SET THE PIVOT ARRAY.
C
            KPVT(K) = K
            IF (SWAP) KPVT(K) = IMAX
         GO TO 190
  140    CONTINUE
C
C           2 X 2 PIVOT BLOCK.
C
            KM1K = IK + K - 1
            IKM1 = IK - (K - 1)
            IF (.NOT.SWAP) GO TO 160
C
C              PERFORM AN INTERCHANGE.
C
               CALL DSWAP(IMAX,AP(IM+1),1,AP(IKM1+1),1)
               IMJ = IKM1 + IMAX
               DO 150 JJ = IMAX, KM1
                  J = KM1 + IMAX - JJ
                  JKM1 = IKM1 + J
                  T = AP(JKM1)
                  AP(JKM1) = AP(IMJ)
                  AP(IMJ) = T
                  IMJ = IMJ - (J - 1)
  150          CONTINUE
               T = AP(KM1K)
               AP(KM1K) = AP(IMK)
               AP(IMK) = T
  160       CONTINUE
C
C           PERFORM THE ELIMINATION.
C
            KM2 = K - 2
            IF (KM2 .EQ. 0) GO TO 180
               AK = AP(KK)/AP(KM1K)
               KM1KM1 = IKM1 + K - 1
               AKM1 = AP(KM1KM1)/AP(KM1K)
               DENOM = 1.0D0 - AK*AKM1
               IJ = IK - (K - 1) - (K - 2)
               DO 170 JJ = 1, KM2
                  J = KM1 - JJ
                  JK = IK + J
                  BK = AP(JK)/AP(KM1K)
                  JKM1 = IKM1 + J
                  BKM1 = AP(JKM1)/AP(KM1K)
                  MULK = (AKM1*BK - BKM1)/DENOM
                  MULKM1 = (AK*BKM1 - BK)/DENOM
                  T = MULK
                  CALL DAXPY(J,T,AP(IK+1),1,AP(IJ+1),1)
                  T = MULKM1
                  CALL DAXPY(J,T,AP(IKM1+1),1,AP(IJ+1),1)
                  AP(JK) = MULK
                  AP(JKM1) = MULKM1
                  IJJ = IJ + J
                  IJ = IJ - (J - 1)
  170          CONTINUE
  180       CONTINUE
C
C           SET THE PIVOT ARRAY.
C
            KPVT(K) = 1 - K
            IF (SWAP) KPVT(K) = -IMAX
            KPVT(K-1) = KPVT(K)
  190    CONTINUE
         IK = IK - (K - 1)
         IF (KSTEP .EQ. 2) IK = IK - (K - 2)
         K = K - KSTEP
      GO TO 10
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DSPSL(AP,N,KPVT,B)
C***BEGIN PROLOGUE  DSPSL
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2B1A
C***KEYWORDS  DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,MATRIX,PACKED,
C             SOLVE,SYMMETRIC
C***AUTHOR  BUNCH, J., (UCSD)
C***PURPOSE  Solves the double precision SYMMETRIC system  A*X=B
C            using the factors computed by DSPFA.
C***DESCRIPTION
C
C     DSISL solves the double precision symmetric system
C     A * X = B
C     using the factors computed by DSPFA.
C
C     On Entry
C
C        AP      DOUBLE PRECISION(N*(N+1)/2)
C                the output from DSPFA.
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        KPVT    INTEGER(N)
C                the pivot vector from DSPFA.
C
C        B       DOUBLE PRECISION(N)
C                the right hand side vector.
C
C     On Return
C
C        B       the solution vector  X .
C
C     Error Condition
C
C        A division by zero may occur if  DSPCO  has set RCOND .EQ. 0.0
C        or  DSPFA  has set INFO .NE. 0  .
C
C     To compute  INVERSE(A) * C  where  C  is a matrix
C     with  P  columns
C           CALL DSPFA(AP,N,KPVT,INFO)
C           IF (INFO .NE. 0) GO TO ...
C           DO 10 J = 1, P
C              CALL DSPSL(AP,N,KPVT,C(1,J))
C        10 CONTINUE
C
C     LINPACK.  This version dated 08/14/78 .
C     James Bunch, Univ. Calif. San Diego, Argonne Nat. Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DDOT
C     Fortran IABS
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT
C***END PROLOGUE  DSPSL
      INTEGER N,KPVT(1)
      DOUBLE PRECISION AP(1),B(1)
C
      DOUBLE PRECISION AK,AKM1,BK,BKM1,DDOT,DENOM,TEMP
      INTEGER IK,IKM1,IKP1,K,KK,KM1K,KM1KM1,KP
C
C     LOOP BACKWARD APPLYING THE TRANSFORMATIONS AND
C     D INVERSE TO B.
C
C***FIRST EXECUTABLE STATEMENT  DSPSL
      K = N
      IK = (N*(N - 1))/2
   10 IF (K .EQ. 0) GO TO 80
         KK = IK + K
         IF (KPVT(K) .LT. 0) GO TO 40
C
C           1 X 1 PIVOT BLOCK.
C
            IF (K .EQ. 1) GO TO 30
               KP = KPVT(K)
               IF (KP .EQ. K) GO TO 20
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
   20          CONTINUE
C
C              APPLY THE TRANSFORMATION.
C
               CALL DAXPY(K-1,B(K),AP(IK+1),1,B(1),1)
   30       CONTINUE
C
C           APPLY D INVERSE.
C
            B(K) = B(K)/AP(KK)
            K = K - 1
            IK = IK - K
         GO TO 70
   40    CONTINUE
C
C           2 X 2 PIVOT BLOCK.
C
            IKM1 = IK - (K - 1)
            IF (K .EQ. 2) GO TO 60
               KP = IABS(KPVT(K))
               IF (KP .EQ. K - 1) GO TO 50
C
C                 INTERCHANGE.
C
                  TEMP = B(K-1)
                  B(K-1) = B(KP)
                  B(KP) = TEMP
   50          CONTINUE
C
C              APPLY THE TRANSFORMATION.
C
               CALL DAXPY(K-2,B(K),AP(IK+1),1,B(1),1)
               CALL DAXPY(K-2,B(K-1),AP(IKM1+1),1,B(1),1)
   60       CONTINUE
C
C           APPLY D INVERSE.
C
            KM1K = IK + K - 1
            KK = IK + K
            AK = AP(KK)/AP(KM1K)
            KM1KM1 = IKM1 + K - 1
            AKM1 = AP(KM1KM1)/AP(KM1K)
            BK = B(K)/AP(KM1K)
            BKM1 = B(K-1)/AP(KM1K)
            DENOM = AK*AKM1 - 1.0D0
            B(K) = (AKM1*BK - BKM1)/DENOM
            B(K-1) = (AK*BKM1 - BK)/DENOM
            K = K - 2
            IK = IK - (K + 1) - K
   70    CONTINUE
      GO TO 10
   80 CONTINUE
C
C     LOOP FORWARD APPLYING THE TRANSFORMATIONS.
C
      K = 1
      IK = 0
   90 IF (K .GT. N) GO TO 160
         IF (KPVT(K) .LT. 0) GO TO 120
C
C           1 X 1 PIVOT BLOCK.
C
            IF (K .EQ. 1) GO TO 110
C
C              APPLY THE TRANSFORMATION.
C
               B(K) = B(K) + DDOT(K-1,AP(IK+1),1,B(1),1)
               KP = KPVT(K)
               IF (KP .EQ. K) GO TO 100
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
  100          CONTINUE
  110       CONTINUE
            IK = IK + K
            K = K + 1
         GO TO 150
  120    CONTINUE
C
C           2 X 2 PIVOT BLOCK.
C
            IF (K .EQ. 1) GO TO 140
C
C              APPLY THE TRANSFORMATION.
C
               B(K) = B(K) + DDOT(K-1,AP(IK+1),1,B(1),1)
               IKP1 = IK + K
               B(K+1) = B(K+1) + DDOT(K-1,AP(IKP1+1),1,B(1),1)
               KP = IABS(KPVT(K))
               IF (KP .EQ. K) GO TO 130
C
C                 INTERCHANGE.
C
                  TEMP = B(K)
                  B(K) = B(KP)
                  B(KP) = TEMP
  130          CONTINUE
  140       CONTINUE
            IK = IK + K + K + 1
            K = K + 2
  150    CONTINUE
      GO TO 90
  160 CONTINUE
      RETURN
      END
C Output from Public domain Ratfor, version 1.0
      subroutine eiggrid(ngrid,grid,ndat3,nr,datain,coefin,evec,eval)
      integer ngrid,ndat3,nr,xlen,dcnt,check,i1,i2,i3
      real grid(ngrid,2),datain(ndat3-3,2),coefin(ndat3,nr)
      real evec(ngrid,4),eval(ngrid,2),y(2)
      real xdata(1000,2),spcoef(1003,5)
      double precision ad(5,2),devec(2,2),deval(2)
      common /spinfo/ xlen, dcnt, xdata, spcoef
      xlen = ndat3 - 3
      dcnt = min(nr,5)
      do 23000 i1 = 1,xlen
      xdata(i1,1) = datain(i1,1)
      xdata(i1,2) = datain(i1,2)
      do 23002 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23002 continue
23003 continue
23000 continue
23001 continue
      do 23004 i1 = xlen+1,xlen+3
      do 23006 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23006 continue
23007 continue
23004 continue
23005 continue
      do 23008 i1 = 1,ngrid
      y(1)=grid(i1,1)
      y(2)=grid(i1,2)
      call tform(y,ad)
      call eigvec(ad,nr,2,devec,deval,check)
      if(check .ne. 0)then
      eval(i1,1) = 0.0
      eval(i1,2) = 0.0
      evec(i1,1) = 0.0
      evec(i1,2) = 0.0
      evec(i1,3) = 0.0
      evec(i1,4) = 0.0
      return
      endif
      if(deval(2) .gt. deval(1))then
      i2 = 2
      i3 = 1
      else
      i2 = 1
      i3 = 2
      endif
      eval(i1,1) = (deval(i2))
      eval(i1,2) = (deval(i3))
      evec(i1,1) = (devec(i2,1))
      evec(i1,2) = (devec(i2,2))
      evec(i1,3) = (devec(i3,1))
      evec(i1,4) = (devec(i3,2))
23008 continue
23009 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine eigvec(ad,ddim,gdim,evec,eval,check)
      integer ddim,gdim,check,i1
      double precision ad(5,2),evec(gdim,gdim),eval(gdim),discr,a,b
      double precision z,zlen,z1,z2
      a = 0.0
      b = 0.0
      do 23000 i1=1,ddim
      a = a + ad(i1,1)*ad(i1,2)
      b = b + (ad(i1,2)**2) - (ad(i1,1)**2)
23000 continue
23001 continue
      discr = (b**2) + 4*(a**2)
      if(discr .le. 0.0)then
      check = 1
      return
      endif
      z = (-b + sqrt(discr))/(2*a)
      zlen = sqrt(z**2+1.0)
      z1 = z/zlen
      z2 = 1/zlen
      eval(1) = 0.0
      eval(2) = 0.0
      do 23004 i1=1,ddim
      eval(1) = eval(1) + ( ad(i1,1)*z1 + ad(i1,2)*z2 )**2
      eval(2) = eval(2) + ( ad(i1,2)*z1 - ad(i1,1)*z2 )**2
23004 continue
23005 continue
      do 23006 i2=1,gdim
      eval(i2) = sqrt(eval(i2))
23006 continue
23007 continue
      evec(1,1) = z1
      evec(2,1) = z2
      evec(1,2) = -z2
      evec(2,2) = z1
      check = 0
      return
      end
      SUBROUTINE FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,S)
C
C     FEHLBERG FOURTH-FIFTH ORDER RUNGE-KUTTA METHOD
C
C***********************************************************************
C    FEHL INTEGRATES A SYSTEM OF NEQN FIRST ORDER
C    ORDINARY DIFFERENTIAL EQUATIONS OF THE FORM
C             DY(I)/DT=F(T,Y(1),---,Y(NEQN))
C    WHERE THE INITIAL VALUES Y(I) AND THE INITIAL DERIVATIVES
C    YP(I) ARE SPECIFIED AT THE STARTING POINT T. FEHL ADVANCES
C    THE SOLUTION OVER THE FIXED STEP H AND RETURNS
C    THE FIFTH ORDER (SIXTH ORDER ACCURATE LOCALLY) SOLUTION
C    APPROXIMATION AT T+H IN ARRAY S(I).
C    F1,---,F5 ARE ARRAYS OF DIMENSION NEQN WHICH ARE NEEDED
C    FOR INTERNAL STORAGE.
C    THE FORMULAS HAVE BEEN GROUPED TO CONTROL LOSS OF SIGNIFICANCE.
C    FEHL SHOULD BE CALLED WITH AN H NOT SMALLER THAN 13 UNITS OF
C    ROUNDOFF IN T SO THAT THE VARIOUS INDEPENDENT ARGUMENTS CAN BE
C    DISTINGUISHED.
C***********************************************************************
C
C
      DIMENSION Y(NEQN),YP(NEQN),F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),
     1          F5(NEQN),S(NEQN)
C
      CH=H/4.
      DO 221 K=1,NEQN
  221   F5(K)=Y(K)+CH*YP(K)
      CALL F(T+CH,F5,F1)
C
      CH=3.*H/32.
      DO 222 K=1,NEQN
  222   F5(K)=Y(K)+CH*(YP(K)+3.*F1(K))
      CALL F(T+3.*H/8.,F5,F2)
C
      CH=H/2197.
      DO 223 K=1,NEQN
  223   F5(K)=Y(K)+CH*(1932.*YP(K)+(7296.*F2(K)-7200.*F1(K)))
      CALL F(T+12.*H/13.,F5,F3)
C
      CH=H/4104.
      DO 224 K=1,NEQN
  224   F5(K)=Y(K)+CH*((8341.*YP(K)-845.*F3(K))+
     1                            (29440.*F2(K)-32832.*F1(K)))
      CALL F(T+H,F5,F4)
C
      CH=H/20520.
      DO 225 K=1,NEQN
  225   F1(K)=Y(K)+CH*((-6080.*YP(K)+(9295.*F3(K)-5643.*F4(K)))+
     1                             (41040.*F1(K)-28352.*F2(K)))
      CALL F(T+H/2.,F1,F5)
C
C     COMPUTE APPROXIMATE SOLUTION AT T+H
C
      CH=H/7618050.
      DO 230 K=1,NEQN
  230   S(K)=Y(K)+CH*((902880.*YP(K)+(3855735.*F3(K)-1371249.*F4(K)))+
     1                (3953664.*F2(K)+277020.*F5(K)))
C
      RETURN
      END
C Output from Public domain Ratfor, version 1.0
      logical function inregion(y,xylim)
      real y(2),xylim(4)
      inregion = (y(1) .ge. xylim(1)) .and. (y(1) .le. xylim(2)) .and. (
     *y(2) .ge. xylim(3)) .and. (y(2) .le. xylim(4))
      return
      end

      subroutine intdrv(x,d,n,y,ny,m,len,f,a,b,sol,iwork,lam,nlam,ainf,
     +                  linf,lsq,db,dlen)

      integer d,n,ny,m,len,f(d+m),iwork(len),nlam,ainf(nlam),linf(ny),
     +        dlen
      double precision x(d,n),y(n,ny),a(len*(len+1)/2,nlam),b(len-n,ny)
      double precision sol(len,ny,nlam),lam(nlam),db(dlen)

      integer odd,lamcnt,lenp,lenz,asub,nsb,ja,jximxj,jldwrk,i1
      double precision conval,lamcon
      logical lsq

************************************************************************
*                                                                      *
*  Routines called: mur,mfill,msol,tcon,least(dqrdc,dqrsl: from -lcm). *
*                                                                      *
************************************************************************

      lenp = len * (len+1) / 2
      lenz = len - n
      ja = 0
      jximxj = ja + lenp * nlam
      jldwrk = jximxj + d

************************************************************************
*                                                                      *
*  Routine mur computes upper right triangle of matrix.                *
*                                                                      *
************************************************************************

      call mur(x,d,n,db(ja+1),lenp,f,m)
      odd = d - (d/2) * 2

************************************************************************
*                                                                      *
*  If lsq=.true., call routine to subtract off least squares fit.      *
*  Subroutine least replaces y with the residuals, and returns in the  *
*  b matrix the polynomial coefficients.                               *
*                                                                      *
************************************************************************

      if (lsq) call least(db(ja+1),y,b,db(jldwrk+1),iwork,lenp,n,ny,
     +                    lenz,linf)

************************************************************************
*                                                                      *
*  Subroutine mfill computes the rest of the matrix.                   *
*                                                                      *
************************************************************************

      call mfill(x,db(jximxj+1),d,n,db(ja+1),lenp,f,m,odd,len)

************************************************************************
*                                                                      *
*  Copy the "a" matrix into spots for other lambdas.                   *
*                                                                      *
************************************************************************

        do 50 lamcnt=2,nlam
        nsb = ja + (lamcnt-1) * lenp
          do 30 i1=1,lenp
          db(nsb+i1) = db(ja+i1)
   30     continue
   50   continue

************************************************************************
*                                                                      *
*  Insert appropriate values of (-1)**m*lambda into "a" matrices.      *
*                                                                      *
************************************************************************

      call tcon(f,d,m,odd,conval)
      conval = conval * (-1)**m

        do 100 lamcnt=1,nlam
        lamcon = conval * lam(lamcnt)
        nsb = ja + (lamcnt-1) * lenp
          do 60 i1=1,n
          db(nsb+i1*(i1+1)/2) = lamcon
   60     continue

************************************************************************
*                                                                      *
*  Prepare the "a" matrix for its return.                              *
*                                                                      *
************************************************************************

        asub = ja + (lamcnt-1) * lenp
          do 70 i1=1,lenp
          a(i1,lamcnt) = db(asub+i1)
   70     continue

************************************************************************
*                                                                      *
*  Call factoring and solving routines.                                *
*                                                                      *
************************************************************************

        call msol(db(nsb+1),lenp,len,iwork,ainf(lamcnt),y,n,ny,
     +            sol(1,1,lamcnt))
  100   continue

      return
      end

C Output from Public domain Ratfor, version 1.0
      subroutine integ(nn,start,xylim,datain,datlen,coefin,coefdim,grid,
     *ngrid,ind, fldmag,iterlim,wstor,warc,wfmag)
      integer nn,datlen,coefdim,ngrid,ind,iterlim,xlen,dcnt,n(2),i1,i2
      real start(2),xylim(4),grid(2,nn),fldmag(nn)
      real datain(datlen,2),coefin(datlen+3,coefdim)
      real wstor(2,nn),warc(nn),wfmag(nn)
      real xdata(1000,2),spcoef(1003,5)
      logical mindir
      common /spinfo/ xlen, dcnt, xdata, spcoef
      common /finteg/ mindir
      xlen = datlen
      dcnt = min(coefdim,5)
      do 23000 i1 = 1,xlen
      xdata(i1,1) = datain(i1,1)
      xdata(i1,2) = datain(i1,2)
      do 23002 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23002 continue
23003 continue
23000 continue
23001 continue
      do 23004 i1 = xlen+1,xlen+3
      do 23006 i2 = 1,dcnt
      spcoef(i1,i2) = coefin(i1,i2)
23006 continue
23007 continue
23004 continue
23005 continue
      ngrid = 0
      if(ind .ne. 0)then
      mindir = .false.
      call curv(nn,start,xylim,dcnt,wstor,warc,nstor,n,wfmag,iterlim)
      call bgcopy(nn,grid,fldmag,nstor,ngrid,wstor,wfmag)
      endif
      if(ind .ne. 1)then
      mindir = .true.
      call curv(nn,start,xylim,dcnt,wstor,warc,nstor,n,wfmag,iterlim)
      call bgcopy(nn,grid,fldmag,nstor,ngrid,wstor,wfmag)
      endif
      return
      end

      subroutine least(alin,y,b,db,iwork,lenp,n,ny,lz,linf)
      integer n,ny,lz,iwork(lz),lenp,linf(ny)
      double precision alin(lenp),b(lz,ny),db(n+(2+n)*lz)
      double precision y(n,ny)
      integer i1,i2,jar,jq,jd,jwork,job,arsub,asub

      jar = 0
      jq = jar + n * lz
      jd = jq + lz
      jwork = jd + n

************************************************************************
*                                                                      *
*  Make a matrix rectangular.                                          *
*                                                                      *
************************************************************************

      asub = n * (n+1) / 2
      arsub = jar
        do 20 i1=1,lz
          do 10 i2=1,n
          db(arsub+i2) = alin(asub+i2)
   10     continue
        asub = asub + i1 + n
        arsub = arsub + n
   20   continue

      job = 110
        do 30 i2=1,lz
        iwork(i2) = 0
   30   continue

************************************************************************
*                                                                      *
*  Call qr factorization routine.                                      *
*                                                                      *
************************************************************************

      call dqrdc(db(jar+1),n,n,lz,db(jq+1),iwork,db(jwork+1),job)

************************************************************************
*                                                                      *
*  Do least squares.  y is written over by residuals.                  *
*                                                                      *
************************************************************************

        do 50 i1=1,ny
        call dqrsl(db(jar+1),n,n,lz,db(jq+1),y(1,i1),db(jd+1),y(1,i1),
     +             b(1,i1),y(1,i1),db(jd+1),job,linf(i1))
   50   continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine mat(point,dblx,d,n,odd,m,lam,solcol,slen,f,flen,dbla,al
     *en,ximxj,yout)
      integer d,n,odd,m,slen,flen,f(flen),alen
      double precision point(d),dblx(d,n),lam,solcol(slen),dbla(alen),xi
     *mxj(d),yout
      integer exp,newsub,degree,jstart,joldst,joldfn,i1,i2,i3
      double precision small,prod,mag
      double precision dnrm2
      exp = (2 * m) - d
      dbla(n+1) = 1.0d0
      small = 1.0d-25
      do 23000 i1=1,d
      dbla(i1+n+1) = point(i1)
23000 continue
23001 continue
      if(m.gt.2)then
      joldst = n + 2
      joldfn = n + 1 + d
      do 23004 degree=2,(m-1)
      jstart = joldfn
      do 23006 i2=1,d 
      do 23008 i3=joldst,joldfn 
      newsub = jstart + i3 - joldst + 1
      dbla(newsub) = point(i2) * dbla(i3)
23008 continue
23009 continue
      jstart = jstart + (joldfn-joldst) + 1
      joldst = joldst+f(d-i2+degree-1)/f(degree-1)/f(d-i2+1)
23006 continue
23007 continue
      joldfn = newsub
23004 continue
23005 continue
      endif
      do 23010 i1=1,n 
      do 23012 i2=1,d
      ximxj(i2) = point(i2) - dblx(i2,i1)
23012 continue
23013 continue
      prod = 0.0d0
      mag = dnrm2(d,ximxj(1),1)
      if(mag.gt.small)then
      if(odd.eq.0)then
      prod = log(mag) * (mag**exp)
      else
      prod = mag ** exp 
      endif
      endif
      dbla(i1) = prod
23010 continue
23011 continue
      yout = 0
      do 23018 i2=1,slen
      yout = yout + dbla(i2) * solcol(i2)
23018 continue
23019 continue
      return
      end
      subroutine mfill(x,ximxj,d,n,a,lenp,f,m,odd,len)

      integer d,n,lenp,f(d+m),m,odd,len
      double precision x(d,n),ximxj(d),a(lenp)
      integer exp,oddexp,newsub,i1,i2,i3
      double precision small,prod,mag
      double precision dnrm2

************************************************************************
*                                                                      *
*  This routine fills the portion of the "a" matrix not filled by mur. *
*                                                                      *
************************************************************************

      exp = (2 * m) - d
      oddexp = -odd + 1
      small = 1.0d-30

************************************************************************
*                                                                      *
*  Insert zeros.                                                       *
*                                                                      *
************************************************************************

        do 120 i1=(n+1),len
        newsub = i1 * (i1-1) / 2
          do 110 i2=(n+1),i1
          a(newsub+i2) = 0.0d0
  110     continue
  120   continue

************************************************************************
*                                                                      *
*  Insert u(xi-xj).                                                    *
*                                                                      *
************************************************************************

        do 150 i1=1,n
        newsub = i1 * (i1-1) / 2
          do 140 i2=1,i1
            do 130 i3=1,d
            ximxj(i3) = x(i3,i2) - x(i3,i1)
  130       continue 
          prod = 0.0d0
          mag = dnrm2(d,ximxj(1),1)
          if (mag.gt.small) prod = mag**exp * log(mag)**oddexp
          a(newsub+i2) = prod
  140     continue
  150   continue

      return
      end
      subroutine msol(a,lenp,len,iwork,info,y,n,ny,sol)

      integer lenp,len,iwork(len),info,n,ny,ycnt,i1
      double precision a(lenp),y(n,ny),sol(len,ny)

************************************************************************
*                                                                      *
*  This routine factors and solves the "a" matrix system.              *
*                                                                      *
************************************************************************

      call dspfa(a,len,iwork,info)
      if (info.ne.0) goto 200

        do 100 ycnt=1,ny
          do 10 i1=1,n
          sol(i1,ycnt) = y(i1,ycnt)
   10     continue
          do 20 i1=(n+1),len
          sol(i1,ycnt) = 0.d0
   20     continue
        call dspsl(a,len,iwork,sol(1,ycnt))
  100   continue

  200 return
      end
************************************************************************
*                                                                      *
*  This routine fills the upper right corner of the "a" matrix.        *
*                                                                      *
************************************************************************

      subroutine mur(x,d,n,a,lenp,f,m)
      integer n,m,d,f(d+m),lenp
      integer i1,i2,jstart,joldst,joldfn,newsub,temp,oldsub,degree
      double precision x(d,n),a(lenp)

*  Insert zero degree polynomials.

      newsub = n * (n+1) / 2
        do 10 i2=1,n
        a(newsub+i2) = 1.d0
   10   continue

*  Insert degree 1 polynomials.

        do 30 i1=1,d
        newsub = (i1+n) * (i1+n+1) / 2
          do 20 i2=1,n
          a(newsub+i2) = x(i1,i2)
   20     continue
   30   continue

      if (m.eq.2) goto 105
      joldst = n + 2
      joldfn = n + 1 + d

*  Recursively compute remaining polynomials.

        do 100 degree=2,(m-1)
        jstart = joldfn
          do 90 i1=1,d
            do 80 k=joldst,joldfn
            temp = jstart + k - joldst + 1
            newsub = temp * (temp-1) / 2
            oldsub = (k-1) * k / 2
              do 70 i2=1,n
              a(newsub+i2) = x(i1,i2) * a(oldsub+i2)
   70         continue
   80       continue
          jstart = jstart + (joldfn-joldst) + 1
          joldst = joldst + f(d-i1+degree-1) / f(degree-1) / f(d-i1+1)
   90     continue
        joldfn = temp
  100   continue

  105 return
      end
      REAL FUNCTION R1MACH(I)
C***BEGIN PROLOGUE  R1MACH
C***DATE WRITTEN   790101   (YYMMDD)
C***REVISION DATE  860825   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns single precision machine dependent constants
C***DESCRIPTION
C
C     R1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subroutine with one (input) argument, and can be called
C     as follows, for example
C
C          A = R1MACH(I)
C
C     where I=1,...,5.  The (output) value of A above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  Single-Precision Machine Constants
C  R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
C  R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C  R1MACH(3) = B**(-T), the smallest relative spacing.
C  R1MACH(4) = B**(1-T), the largest relative spacing.
C  R1MACH(5) = LOG10(B)
C***REFERENCES  FOX, P.A., HALL, A.D., SCHRYER, N.L, *FRAMEWORK FOR
C                 A PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHE-
C                 MATICAL SOFTWARE, VOL. 4, NO. 2, JUNE 1978,
C                 PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  R1MACH
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
C
      REAL RMACH(5)
C
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA RMACH(1) / Z400800000 /
C     DATA RMACH(2) / Z5FFFFFFFF /
C     DATA RMACH(3) / Z4E9800000 /
C     DATA RMACH(4) / Z4EA800000 /
C     DATA RMACH(5) / Z500E730E8 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.
C
C     DATA RMACH(1) / O1771000000000000 /
C     DATA RMACH(2) / O0777777777777777 /
C     DATA RMACH(3) / O1311000000000000 /
C     DATA RMACH(4) / O1301000000000000 /
C     DATA RMACH(5) / O1157163034761675 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (NATIVE MODE)
C
C     DATA RMACH(1) / 2.9387360E-39 /
C     DATA RMACH(2) / 1.7014117E+38 /
C     DATA RMACH(3) / 5.9604645E-08 /
C     DATA RMACH(4) / 1.1920929E-07 /
C     DATA RMACH(5) / 3.0102999E-01 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (NATIVE MODE)
C     WITH -R8 OPTION
C
C     DATA RMACH(1) / 5.562684646268007D-309 /
C     DATA RMACH(2) / 8.988465674311577D+307 /
C     DATA RMACH(3) / 1.110223024625157D-016 /
C     DATA RMACH(4) / 2.220446049250313D-016 /
C     DATA RMACH(5) / 3.010299956639812D-001 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (IEEE MODE)
C
C     DATA RMACH(1) / 1.1754945E-38 /
C     DATA RMACH(2) / 3.4028234E+38 /
C     DATA RMACH(3) / 5.9604645E-08 /
C     DATA RMACH(4) / 1.1920929E-07 /
C     DATA RMACH(5) / 3.0102999E-01 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (IEEE MODE)
C     WITH -R8 OPTION
C
C     DATA RMACH(1) / 2.225073858507202D-308 /
C     DATA RMACH(2) / 1.797693134862315D+308 /
C     DATA RMACH(3) / 1.110223024625157D-016 /
C     DATA RMACH(4) / 2.220446049250313D-016 /
C     DATA RMACH(5) / 3.010299956639812D-001 /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 170 SERIES (FTN5).
C
C      DATA RMACH(1) / O"00014000000000000000" /
C      DATA RMACH(2) / O"37767777777777777777" /
C      DATA RMACH(3) / O"16404000000000000000" /
C      DATA RMACH(4) / O"16414000000000000000" /
C      DATA RMACH(5) / O"17164642023241175720" /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA RMACH(1) / Z"3001800000000000" /
C     DATA RMACH(2) / Z"4FFEFFFFFFFFFFFE" /
C     DATA RMACH(3) / Z"3FD2800000000000" /
C     DATA RMACH(4) / Z"3FD3800000000000" /
C     DATA RMACH(5) / Z"3FFF9A209A84FBCF" /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 205
C
C     DATA RMACH(1) / X'9000400000000000' /
C     DATA RMACH(2) / X'6FFF7FFFFFFFFFFE' /
C     DATA RMACH(3) / X'FFA3400000000000' /
C     DATA RMACH(4) / X'FFA4400000000000' /
C     DATA RMACH(5) / X'FFD04D104D427DE8' /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C     DATA RMACH(1) / 00564000000000000000B /
C     DATA RMACH(2) / 37767777777777777776B /
C     DATA RMACH(3) / 16414000000000000000B /
C     DATA RMACH(4) / 16424000000000000000B /
C     DATA RMACH(5) / 17164642023241175720B /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA RMACH(1) / 200034000000000000000B /
C     DATA RMACH(2) / 577767777777777777776B /
C     DATA RMACH(3) / 377224000000000000000B /
C     DATA RMACH(4) / 377234000000000000000B /
C     DATA RMACH(5) / 377774642023241175720B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC RMACH(5)
C
C     DATA SMALL/20K,0/,LARGE/77777K,177777K/
C     DATA RIGHT/35420K,0/,DIVER/36020K,0/
C     DATA LOG10/40423K,42023K/
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
C     DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
C     DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
C     DATA LOG10(1),LOG10(2) / '23210115, '00000377 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C
C     DATA RMACH(1) / O402400000000 /
C     DATA RMACH(2) / O376777777777 /
C     DATA RMACH(3) / O714400000000 /
C     DATA RMACH(4) / O716400000000 /
C     DATA RMACH(5) / O776464202324 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE91), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA RMACH(1) / Z00100000 /
C     DATA RMACH(2) / Z7FFFFFFF /
C     DATA RMACH(3) / Z3B100000 /
C     DATA RMACH(4) / Z3C100000 /
C     DATA RMACH(5) / Z41134413 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).
C
C     DATA RMACH(1) / "000400000000 /
C     DATA RMACH(2) / "377777777777 /
C     DATA RMACH(3) / "146400000000 /
C     DATA RMACH(4) / "147400000000 /
C     DATA RMACH(5) / "177464202324 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1) /    8388608 /
C     DATA LARGE(1) / 2147483647 /
C     DATA RIGHT(1) /  880803840 /
C     DATA DIVER(1) /  889192448 /
C     DATA LOG10(1) / 1067065499 /
C
C     DATA RMACH(1) / O00040000000 /
C     DATA RMACH(2) / O17777777777 /
C     DATA RMACH(3) / O06440000000 /
C     DATA RMACH(4) / O06500000000 /
C     DATA RMACH(5) / O07746420233 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1),SMALL(2) /   128,     0 /
C     DATA LARGE(1),LARGE(2) / 32767,    -1 /
C     DATA RIGHT(1),RIGHT(2) / 13440,     0 /
C     DATA DIVER(1),DIVER(2) / 13568,     0 /
C     DATA LOG10(1),LOG10(2) / 16282,  8347 /
C
C     DATA SMALL(1),SMALL(2) / O000200, O000000 /
C     DATA LARGE(1),LARGE(2) / O077777, O177777 /
C     DATA RIGHT(1),RIGHT(2) / O032200, O000000 /
C     DATA DIVER(1),DIVER(2) / O032400, O000000 /
C     DATA LOG10(1),LOG10(2) / O037632, O020233 /
C
C     MACHINE CONSTANTS FOR THE SUN 3 (68881 OR FPA)
C
      DATA SMALL(1) / X'00800000' /
      DATA LARGE(1) / X'7F7FFFFF' /
      DATA RIGHT(1) / X'33800000' /
      DATA DIVER(1) / X'34000000' /
      DATA LOG10(1) / X'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     DATA RMACH(1) / O000400000000 /
C     DATA RMACH(2) / O377777777777 /
C     DATA RMACH(3) / O146400000000 /
C     DATA RMACH(4) / O147400000000 /
C     DATA RMACH(5) / O177464202324 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C    (EXPRESSED IN INTEGER AND HEXADECIMAL)
C  ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS***
C  *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1) /       128 /
C     DATA LARGE(1) /    -32769 /
C     DATA RIGHT(1) /     13440 /
C     DATA DIVER(1) /     13568 /
C     DATA LOG10(1) / 547045274 /
C
C     DATA SMALL(1) / Z00000080 /
C     DATA LARGE(1) / ZFFFF7FFF /
C     DATA RIGHT(1) / Z00003480 /
C     DATA DIVER(1) / Z00003500 /
C     DATA LOG10(1) / Z209B3F9A /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA SMALL(1),SMALL(2) /     0,    256/
C     DATA LARGE(1),LARGE(2) /    -1,   -129/
C     DATA RIGHT(1),RIGHT(2) /     0,  26880/
C     DATA DIVER(1),DIVER(2) /     0,  27136/
C     DATA LOG10(1),LOG10(2) /  8347,  32538/
C
C
C***FIRST EXECUTABLE STATEMENT  R1MACH
      IF (I .LT. 1  .OR.  I .GT. 5) R1MACH = 0
C    1   CALL XERROR ( 'R1MACH -- I OUT OF BOUNDS',25,1,2)
C
      ELSE R1MACH = RMACH(I)
      RETURN
C
      END
      SUBROUTINE RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
C
C     FEHLBERG FOURTH-FIFTH ORDER RUNGE-KUTTA METHOD
C
C
C    RKF45 IS PRIMARILY DESIGNED TO SOLVE NON-STIFF AND MILDLY STIFF
C    DIFFERENTIAL EQUATIONS WHEN DERIVATIVE EVALUATIONS ARE INEXPENSIVE.
C    RKF45 SHOULD GENERALLY NOT BE USED WHEN THE USER IS DEMANDING
C    HIGH ACCURACY.
C
C***********************************************************************
C ABSTRACT
C***********************************************************************
C
C    SUBROUTINE  RKF45  INTEGRATES A SYSTEM OF NEQN FIRST ORDER
C    ORDINARY DIFFERENTIAL EQUATIONS OF THE FORM
C             DY(I)/DT = F(T,Y(1),Y(2),...,Y(NEQN))
C              WHERE THE Y(I) ARE GIVEN AT T .
C    TYPICALLY THE SUBROUTINE IS USED TO INTEGRATE FROM T TO TOUT BUT IT
C    CAN BE USED AS A ONE-STEP INTEGRATOR TO ADVANCE THE SOLUTION A
C    SINGLE STEP IN THE DIRECTION OF TOUT.  ON RETURN THE PARAMETERS IN
C    THE CALL LIST ARE SET FOR CONTINUING THE INTEGRATION. THE USER HAS
C    ONLY TO CALL RKF45 AGAIN (AND PERHAPS DEFINE A NEW VALUE FOR TOUT).
C    ACTUALLY, RKF45 IS AN INTERFACING ROUTINE WHICH CALLS SUBROUTINE
C    RKFS FOR THE SOLUTION.  RKFS IN TURN CALLS SUBROUTINE  FEHL WHICH
C    COMPUTES AN APPROXIMATE SOLUTION OVER ONE STEP.
C
C    RKF45  USES THE RUNGE-KUTTA-FEHLBERG (4,5)  METHOD DESCRIBED
C    IN THE REFERENCE
C    E.FEHLBERG , LOW-ORDER CLASSICAL RUNGE-KUTTA FORMULAS WITH STEPSIZE
C                 CONTROL , NASA TR R-315
C
C    THE PERFORMANCE OF RKF45 IS ILLUSTRATED IN THE REFERENCE
C    L.F.SHAMPINE,H.A.WATTS,S.DAVENPORT, SOLVING NON-STIFF ORDINARY
C                 DIFFERENTIAL EQUATIONS-THE STATE OF THE ART ,
C                 SANDIA LABORATORIES REPORT SAND75-0182 ,
C                 TO APPEAR IN SIAM REVIEW.
C
C
C    THE PARAMETERS REPRESENT-
C      F -- SUBROUTINE F(T,Y,YP) TO EVALUATE DERIVATIVES YP(I)=DY(I)/DT
C      NEQN -- NUMBER OF EQUATIONS TO BE INTEGRATED
C      Y(*) -- SOLUTION VECTOR AT T
C      T -- INDEPENDENT VARIABLE
C      TOUT -- OUTPUT POINT AT WHICH SOLUTION IS DESIRED
C      RELERR,ABSERR -- RELATIVE AND ABSOLUTE ERROR TOLERANCES FOR LOCAL
C            ERROR TEST. AT EACH STEP THE CODE REQUIRES THAT
C                 ABS(LOCAL ERROR) .LE. RELERR*ABS(Y) + ABSERR
C            FOR EACH COMPONENT OF THE LOCAL ERROR AND SOLUTION VECTORS
C      IFLAG -- INDICATOR FOR STATUS OF INTEGRATION
C      WORK(*) -- ARRAY TO HOLD INFORMATION INTERNAL TO RKF45 WHICH IS
C            NECESSARY FOR SUBSEQUENT CALLS. MUST BE DIMENSIONED
C            AT LEAST  3+6*NEQN
C      IWORK(*) -- INTEGER ARRAY USED TO HOLD INFORMATION INTERNAL TO
C            RKF45 WHICH IS NECESSARY FOR SUBSEQUENT CALLS. MUST BE
C            DIMENSIONED AT LEAST  5
C
C
C***********************************************************************
C  FIRST CALL TO RKF45
C***********************************************************************
C
C    THE USER MUST PROVIDE STORAGE IN HIS CALLING PROGRAM FOR THE ARRAYS
C    IN THE CALL LIST  -      Y(NEQN) , WORK(3+6*NEQN) , IWORK(5)  ,
C    DECLARE F IN AN EXTERNAL STATEMENT, SUPPLY SUBROUTINE F(T,Y,YP) AND
C    INITIALIZE THE FOLLOWING PARAMETERS-
C
C      NEQN -- NUMBER OF EQUATIONS TO BE INTEGRATED.  (NEQN .GE. 1)
C      Y(*) -- VECTOR OF INITIAL CONDITIONS
C      T -- STARTING POINT OF INTEGRATION , MUST BE A VARIABLE
C      TOUT -- OUTPUT POINT AT WHICH SOLUTION IS DESIRED.
C            T=TOUT IS ALLOWED ON THE FIRST CALL ONLY, IN WHICH CASE
C            RKF45 RETURNS WITH IFLAG=2 IF CONTINUATION IS POSSIBLE.
C      RELERR,ABSERR -- RELATIVE AND ABSOLUTE LOCAL ERROR TOLERANCES
C            WHICH MUST BE NON-NEGATIVE. RELERR MUST BE A VARIABLE WHILE
C            ABSERR MAY BE A CONSTANT. THE CODE SHOULD NORMALLY NOT BE
C            USED WITH RELATIVE ERROR CONTROL SMALLER THAN ABOUT 1.E-8 .
C            TO AVOID LIMITING PRECISION DIFFICULTIES THE CODE REQUIRES
C            RELERR TO BE LARGER THAN AN INTERNALLY COMPUTED RELATIVE
C            ERROR PARAMETER WHICH IS MACHINE DEPENDENT. IN PARTICULAR,
C            PURE ABSOLUTE ERROR IS NOT PERMITTED. IF A SMALLER THAN
C            ALLOWABLE VALUE OF RELERR IS ATTEMPTED, RKF45 INCREASES
C            RELERR APPROPRIATELY AND RETURNS CONTROL TO THE USER BEFORE
C            CONTINUING THE INTEGRATION.
C      IFLAG -- +1,-1  INDICATOR TO INITIALIZE THE CODE FOR EACH NEW
C            PROBLEM. NORMAL INPUT IS +1. THE USER SHOULD SET IFLAG=-1
C            ONLY WHEN ONE-STEP INTEGRATOR CONTROL IS ESSENTIAL. IN THIS
C            CASE, RKF45 ATTEMPTS TO ADVANCE THE SOLUTION A SINGLE STEP
C            IN THE DIRECTION OF TOUT EACH TIME IT IS CALLED. SINCE THIS
C            MODE OF OPERATION RESULTS IN EXTRA COMPUTING OVERHEAD, IT
C            SHOULD BE AVOIDED UNLESS NEEDED.
C
C
C***********************************************************************
C  OUTPUT FROM RKF45
C***********************************************************************
C
C      Y(*) -- SOLUTION AT T
C      T -- LAST POINT REACHED IN INTEGRATION.
C      IFLAG = 2 -- INTEGRATION REACHED TOUT.INDICATES SUCCESSFUL RETURN
C                   AND IS THE NORMAL MODE FOR CONTINUING INTEGRATION.
C            =-2 -- A SINGLE SUCCESSFUL STEP IN THE DIRECTION OF TOUT
C                   HAS BEEN TAKEN. NORMAL MODE FOR CONTINUING
C                   INTEGRATION ONE STEP AT A TIME.
C            = 3 -- INTEGRATION WAS NOT COMPLETED BECAUSE RELATIVE ERROR
C                   TOLERANCE WAS TOO SMALL. RELERR HAS BEEN INCREASED
C                   APPROPRIATELY FOR CONTINUING.
C            = 4 -- INTEGRATION WAS NOT COMPLETED BECAUSE MORE THAN
C                   3000 DERIVATIVE EVALUATIONS WERE NEEDED. THIS
C                   IS APPROXIMATELY 500 STEPS.
C            = 5 -- INTEGRATION WAS NOT COMPLETED BECAUSE SOLUTION
C                   VANISHED MAKING A PURE RELATIVE ERROR TEST
C                   IMPOSSIBLE. MUST USE NON-ZERO ABSERR TO CONTINUE.
C                   USING THE ONE-STEP INTEGRATION MODE FOR ONE STEP
C                   IS A GOOD WAY TO PROCEED.
C            = 6 -- INTEGRATION WAS NOT COMPLETED BECAUSE REQUESTED
C                   ACCURACY COULD NOT BE ACHIEVED USING SMALLEST
C                   ALLOWABLE STEPSIZE. USER MUST INCREASE THE ERROR
C                   TOLERANCE BEFORE CONTINUED INTEGRATION CAN BE
C                   ATTEMPTED.
C            = 7 -- IT IS LIKELY THAT RKF45 IS INEFFICIENT FOR SOLVING
C                   THIS PROBLEM. TOO MUCH OUTPUT IS RESTRICTING THE
C                   NATURAL STEPSIZE CHOICE. USE THE ONE-STEP INTEGRATOR
C                   MODE.
C            = 8 -- INVALID INPUT PARAMETERS
C                   THIS INDICATOR OCCURS IF ANY OF THE FOLLOWING IS
C                   SATISFIED -   NEQN .LE. 0
C                                 T=TOUT  AND  IFLAG .NE. +1 OR -1
C                                 RELERR OR ABSERR .LT. 0.
C                                 IFLAG .EQ. 0  OR  .LT. -2  OR  .GT. 8
C      WORK(*),IWORK(*) -- INFORMATION WHICH IS USUALLY OF NO INTEREST
C                   TO THE USER BUT NECESSARY FOR SUBSEQUENT CALLS.
C                   WORK(1),...,WORK(NEQN) CONTAIN THE FIRST DERIVATIVES
C                   OF THE SOLUTION VECTOR Y AT T. WORK(NEQN+1) CONTAINS
C                   THE STEPSIZE H TO BE ATTEMPTED ON THE NEXT STEP.
C                   IWORK(1) CONTAINS THE DERIVATIVE EVALUATION COUNTER.
C
C
C***********************************************************************
C  SUBSEQUENT CALLS TO RKF45
C***********************************************************************
C
C    SUBROUTINE RKF45 RETURNS WITH ALL INFORMATION NEEDED TO CONTINUE
C    THE INTEGRATION. IF THE INTEGRATION REACHED TOUT,THE USER NEED ONLY
C    DEFINE A NEW TOUT AND CALL RKF45 AGAIN. IN THE ONE-STEP INTEGRATOR
C    MODE (IFLAG=-2) THE USER MUST KEEP IN MIND THAT EACH STEP TAKEN IS
C    IN THE DIRECTION OF THE CURRENT TOUT. UPON REACHING TOUT (INDICATED
C    BY CHANGING IFLAG TO 2),THE USER MUST THEN DEFINE A NEW TOUT AND
C    RESET IFLAG TO -2 TO CONTINUE IN THE ONE-STEP INTEGRATOR MODE.
C
C    IF THE INTEGRATION WAS NOT COMPLETED BUT THE USER STILL WANTS TO
C    CONTINUE (IFLAG=3,4 CASES), HE JUST CALLS RKF45 AGAIN. WITH IFLAG=3
C    THE RELERR PARAMETER HAS BEEN ADJUSTED APPROPRIATELY FOR CONTINUING
C    THE INTEGRATION. IN THE CASE OF IFLAG=4 THE FUNCTION COUNTER WILL
C    BE RESET TO 0 AND ANOTHER 3000 FUNCTION EVALUATIONS ARE ALLOWED.
C
C    HOWEVER,IN THE CASE IFLAG=5, THE USER MUST FIRST ALTER THE ERROR
C    CRITERION TO USE A POSITIVE VALUE OF ABSERR BEFORE INTEGRATION CAN
C    PROCEED. IF HE DOES NOT,EXECUTION IS TERMINATED.
C
C    ALSO,IN THE CASE IFLAG=6, IT IS NECESSARY FOR THE USER TO RESET
C    IFLAG TO 2 (OR -2 WHEN THE ONE-STEP INTEGRATION MODE IS BEING USED)
C    AS WELL AS INCREASING EITHER ABSERR,RELERR OR BOTH BEFORE THE
C    INTEGRATION CAN BE CONTINUED. IF THIS IS NOT DONE, EXECUTION WILL
C    BE TERMINATED. THE OCCURRENCE OF IFLAG=6 INDICATES A TROUBLE SPOT
C    (SOLUTION IS CHANGING RAPIDLY,SINGULARITY MAY BE PRESENT) AND IT
C    OFTEN IS INADVISABLE TO CONTINUE.
C
C    IF IFLAG=7 IS ENCOUNTERED, THE USER SHOULD USE THE ONE-STEP
C    INTEGRATION MODE WITH THE STEPSIZE DETERMINED BY THE CODE OR
C    CONSIDER SWITCHING TO THE ADAMS CODES DE/STEP,INTRP. IF THE USER
C    INSISTS UPON CONTINUING THE INTEGRATION WITH RKF45, HE MUST RESET
C    IFLAG TO 2 BEFORE CALLING RKF45 AGAIN. OTHERWISE,EXECUTION WILL BE
C    TERMINATED.
C
C    IF IFLAG=8 IS OBTAINED, INTEGRATION CAN NOT BE CONTINUED UNLESS
C    THE INVALID INPUT PARAMETERS ARE CORRECTED.
C
C    IT SHOULD BE NOTED THAT THE ARRAYS WORK,IWORK CONTAIN INFORMATION
C    REQUIRED FOR SUBSEQUENT INTEGRATION. ACCORDINGLY, WORK AND IWORK
C    SHOULD NOT BE ALTERED.
C
C***********************************************************************
C
      DIMENSION Y(NEQN),WORK(1),IWORK(5)
C
      EXTERNAL F
C
C
C     COMPUTE INDICES FOR THE SPLITTING OF THE WORK ARRAY
C
      K1M=NEQN+1
      K1=K1M+1
      K2=K1+NEQN
      K3=K2+NEQN
      K4=K3+NEQN
      K5=K4+NEQN
      K6=K5+NEQN
C
C***********************************************************************
C     THIS INTERFACING ROUTINE MERELY RELIEVES THE USER OF A LONG
C     CALLING LIST VIA THE SPLITTING APART OF TWO WORKING STORAGE
C     ARRAYS. IF THIS IS NOT COMPATIBLE WITH THE USERS COMPILER,
C     HE MUST USE RKFS DIRECTLY.
C***********************************************************************
C
      CALL RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK(1),WORK(K1M),
     1          WORK(K1),WORK(K2),WORK(K3),WORK(K4),WORK(K5),WORK(K6),
     2          WORK(K6+1),IWORK(1),IWORK(2),IWORK(3),IWORK(4),IWORK(5))
C
      RETURN
      END
      SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,YP,H,F1,F2,F3,
     1                F4,F5,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG,KFLAG)
C
C     FEHLBERG FOURTH-FIFTH ORDER RUNGE-KUTTA METHOD
C
C***********************************************************************
C
C     RKFS INTEGRATES A SYSTEM OF FIRST ORDER ORDINARY DIFFERENTIAL
C     EQUATIONS AS DESCRIBED IN THE COMMENTS FOR RKF45 .
C     THE ARRAYS YP,F1,F2,F3,F4,AND F5 (OF DIMENSION AT LEAST NEQN) AND
C     THE VARIABLES H,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG,AND KFLAG ARE USED
C     INTERNALLY BY THE CODE AND APPEAR IN THE CALL LIST TO ELIMINATE
C     LOCAL RETENTION OF VARIABLES BETWEEN CALLS. ACCORDINGLY, THEY
C     SHOULD NOT BE ALTERED. ITEMS OF POSSIBLE INTEREST ARE
C         YP - DERIVATIVE OF SOLUTION VECTOR AT T
C         H  - AN APPROPRIATE STEPSIZE TO BE USED FOR THE NEXT STEP
C         NFE- COUNTER ON THE NUMBER OF DERIVATIVE FUNCTION EVALUATIONS
C
C***********************************************************************
C
      LOGICAL HFAILD,OUTPUT
C
      DIMENSION Y(NEQN),YP(NEQN),F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),
     1          F5(NEQN)
C
      EXTERNAL F
C
C***********************************************************************
C
C  THE COMPUTER UNIT ROUNDOFF ERROR U IS THE SMALLEST POSITIVE VALUE
C  REPRESENTABLE IN THE MACHINE SUCH THAT  1.+ U .GT. 1.
C                  VALUES TO BE USED ARE
C              U = 9.5E-7          FOR IBM 360/370
C              U = 1.5E-8          FOR UNIVAC 1108
C              U = 7.5E-9          FOR PDP-10
C              U = 7.1E-15         FOR CDC 6000  SERIES
C              U = 2.2E-16         FOR IBM 360/370  DOUBLE PRECISION
      DATA U /0.0/
C
C***********************************************************************
C
C  REMIN IS A TOLERANCE THRESHOLD WHICH IS ALSO DETERMINED BY THE
C  INTEGRATION METHOD. IN PARTICULAR, A FIFTH ORDER METHOD WILL
C  GENERALLY NOT BE CAPABLE OF DELIVERING ACCURACIES NEAR LIMITING
C  PRECISION ON COMPUTERS WITH LONG WORDLENGTHS.
C
      DATA REMIN /0.0/
      DATA MAXNFE/3000/
      IF (U .EQ. 0.0) U = R1MACH (4)
      IF (REMIN .EQ. 0.0) REMIN = 140. * R1MACH (4)
C
C***********************************************************************
C
C     THE EXPENSE IS CONTROLLED BY RESTRICTING THE NUMBER
C     OF FUNCTION EVALUATIONS TO BE APPROXIMATELY MAXNFE.
C     AS SET,THIS CORRESPONDS TO ABOUT 500 STEPS.
C
C
C***********************************************************************
C
C
C     CHECK INPUT PARAMETERS
C
C
      IF (NEQN .LT. 1) GO TO 10
      IF ((RELERR .LT. 0.)  .OR.  (ABSERR .LT. 0.)) GO TO 10
      MFLAG=IABS(IFLAG)
      IF ((MFLAG .GE. 1)  .AND.  (MFLAG .LE. 8)) GO TO 20
C
C     INVALID INPUT
   10 IFLAG=8
      RETURN
C
C     IS THIS THE FIRST CALL
   20 IF (MFLAG .EQ. 1) GO TO 50
C
C     CHECK CONTINUATION POSSIBILITIES
C
      IF ((T .EQ. TOUT) .AND. (KFLAG .NE. 3)) GO TO 10
      IF (MFLAG .NE. 2) GO TO 25
C
C     IFLAG = +2 OR -2
      IF (KFLAG .EQ. 3) GO TO 45
      IF (INIT .EQ. 0) GO TO 45
      IF (KFLAG .EQ. 4) GO TO 40
C  THE NEXT THREE LINES HAVE BEEN COMMENTED OUT.  SML
C     IF ((KFLAG .EQ. 5)  .AND.  (ABSERR .EQ. 0.)) GO TO 30
C     IF ((KFLAG .EQ. 6)  .AND.  (RELERR .LE. SAVRE)  .AND.
C    1    (ABSERR .LE. SAVAE)) GO TO 30
      GO TO 50
C
C     IFLAG = 3,4,5,6,7 OR 8
   25 IF (IFLAG .EQ. 3) GO TO 45
      IF (IFLAG .EQ. 4) GO TO 40
      IF ((IFLAG .EQ. 5) .AND. (ABSERR .GT. 0.)) GO TO 45
C
C     INTEGRATION CANNOT BE CONTINUED SINCE USER DID NOT RESPOND TO
C     THE INSTRUCTIONS PERTAINING TO IFLAG=5,6,7 OR 8
C  30 STOP
C
C***********************************************************************
C
C     RESET FUNCTION EVALUATION COUNTER
   40 NFE=0
      IF (MFLAG .EQ. 2) GO TO 50
C
C     RESET FLAG VALUE FROM PREVIOUS CALL
   45 IFLAG=JFLAG
      IF (KFLAG .EQ. 3) MFLAG=IABS(IFLAG)
C
C     SAVE INPUT IFLAG AND SET CONTINUATION FLAG VALUE FOR SUBSEQUENT
C     INPUT CHECKING
   50 JFLAG=IFLAG
      KFLAG=0
C
C     SAVE RELERR AND ABSERR FOR CHECKING INPUT ON SUBSEQUENT CALLS
      SAVRE=RELERR
      SAVAE=ABSERR
C
C     RESTRICT RELATIVE ERROR TOLERANCE TO BE AT LEAST AS LARGE AS
C     2U+REMIN TO AVOID LIMITING PRECISION DIFFICULTIES ARISING FROM
C     IMPOSSIBLE ACCURACY REQUESTS
C
      RER=2.*U+REMIN
      IF (RELERR .GE. RER) GO TO 55
C
C     RELATIVE ERROR TOLERANCE TOO SMALL
      RELERR=RER
      IFLAG=3
      KFLAG=3
      RETURN
C
   55 U26=26.*U
C
      DT=TOUT-T
C
      IF (MFLAG .EQ. 1) GO TO 60
      IF (INIT .EQ. 0) GO TO 65
      GO TO 80
C
C
C***********************************************************************
C
C     INITIALIZATION --
C                       SET INITIALIZATION COMPLETION INDICATOR,INIT
C                       SET INDICATOR FOR TOO MANY OUTPUT POINTS,KOP
C                       EVALUATE INITIAL DERIVATIVES
C                       SET COUNTER FOR FUNCTION EVALUATIONS,NFE
C                       ESTIMATE STARTING STEPSIZE
C
   60 INIT=0
      KOP=0
C
      A=T
      CALL F(A,Y,YP)
      NFE=1
      IF (T .NE. TOUT) GO TO 65
      IFLAG=2
      RETURN
C
C
   65 INIT=1
      H=ABS(DT)
      TOLN=0.
      DO 70 K=1,NEQN
        TOL=RELERR*ABS(Y(K))+ABSERR
        IF (TOL .LE. 0.) GO TO 70
        TOLN=TOL
        YPK=ABS(YP(K))
        IF (YPK*H**5 .GT. TOL) H=(TOL/YPK)**0.2
   70 CONTINUE
      IF (TOLN .LE. 0.) H=0.
      H=AMAX1(H,U26*AMAX1(ABS(T),ABS(DT)))
      JFLAG=ISIGN(2,IFLAG)
C
C
C***********************************************************************
C
C     SET STEPSIZE FOR INTEGRATION IN THE DIRECTION FROM T TO TOUT
C
   80 H=SIGN(H,DT)
C
C     TEST TO SEE IF RKF45 IS BEING SEVERELY IMPACTED BY TOO MANY
C     OUTPUT POINTS
C
      IF (ABS(H) .GE. 2.*ABS(DT)) KOP=KOP+1
      IF (KOP .NE. 100) GO TO 85
C
C     UNNECESSARY FREQUENCY OF OUTPUT
      KOP=0
      IFLAG=7
      RETURN
C
   85 IF (ABS(DT) .GT. U26*ABS(T)) GO TO 95
C
C     IF TOO CLOSE TO OUTPUT POINT,EXTRAPOLATE AND RETURN
C
      DO 90 K=1,NEQN
   90   Y(K)=Y(K)+DT*YP(K)
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
C
C
C     INITIALIZE OUTPUT POINT INDICATOR
C
   95 OUTPUT= .FALSE.
C
C     TO AVOID PREMATURE UNDERFLOW IN THE ERROR TOLERANCE FUNCTION,
C     SCALE THE ERROR TOLERANCES
C
      SCALE=2./RELERR
      AE=SCALE*ABSERR
C
C
C***********************************************************************
C***********************************************************************
C     STEP BY STEP INTEGRATION
C
  100 HFAILD= .FALSE.
C
C     SET SMALLEST ALLOWABLE STEPSIZE
C
      HMIN=U26*ABS(T)
C
C     ADJUST STEPSIZE IF NECESSARY TO HIT THE OUTPUT POINT.
C     LOOK AHEAD TWO STEPS TO AVOID DRASTIC CHANGES IN THE STEPSIZE AND
C     THUS LESSEN THE IMPACT OF OUTPUT POINTS ON THE CODE.
C
      DT=TOUT-T
      IF (ABS(DT) .GE. 2.*ABS(H)) GO TO 200
      IF (ABS(DT) .GT. ABS(H)) GO TO 150
C
C     THE NEXT SUCCESSFUL STEP WILL COMPLETE THE INTEGRATION TO THE
C     OUTPUT POINT
C
      OUTPUT= .TRUE.
      H=DT
      GO TO 200
C
  150 H=0.5*DT
C
C
C
C***********************************************************************
C     CORE INTEGRATOR FOR TAKING A SINGLE STEP
C***********************************************************************
C     THE TOLERANCES HAVE BEEN SCALED TO AVOID PREMATURE UNDERFLOW IN
C     COMPUTING THE ERROR TOLERANCE FUNCTION ET.
C     TO AVOID PROBLEMS WITH ZERO CROSSINGS,RELATIVE ERROR IS MEASURED
C     USING THE AVERAGE OF THE MAGNITUDES OF THE SOLUTION AT THE
C     BEGINNING AND END OF A STEP.
C     THE ERROR ESTIMATE FORMULA HAS BEEN GROUPED TO CONTROL LOSS OF
C     SIGNIFICANCE.
C     TO DISTINGUISH THE VARIOUS ARGUMENTS, H IS NOT PERMITTED
C     TO BECOME SMALLER THAN 26 UNITS OF ROUNDOFF IN T.
C     PRACTICAL LIMITS ON THE CHANGE IN THE STEPSIZE ARE ENFORCED TO
C     SMOOTH THE STEPSIZE SELECTION PROCESS AND TO AVOID EXCESSIVE
C     CHATTERING ON PROBLEMS HAVING DISCONTINUITIES.
C     TO PREVENT UNNECESSARY FAILURES, THE CODE USES 9/10 THE STEPSIZE
C     IT ESTIMATES WILL SUCCEED.
C     AFTER A STEP FAILURE, THE STEPSIZE IS NOT ALLOWED TO INCREASE FOR
C     THE NEXT ATTEMPTED STEP. THIS MAKES THE CODE MORE EFFICIENT ON
C     PROBLEMS HAVING DISCONTINUITIES AND MORE EFFECTIVE IN GENERAL
C     SINCE LOCAL EXTRAPOLATION IS BEING USED AND EXTRA CAUTION SEEMS
C     WARRANTED.
C***********************************************************************
C
C
C     TEST NUMBER OF DERIVATIVE FUNCTION EVALUATIONS.
C     IF OKAY,TRY TO ADVANCE THE INTEGRATION FROM T TO T+H
C
  200 IF (NFE .LE. MAXNFE) GO TO 220
C
C     TOO MUCH WORK
      IFLAG=4
      KFLAG=4
      RETURN
C
C     ADVANCE AN APPROXIMATE SOLUTION OVER ONE STEP OF LENGTH H
C
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
C
C     COMPUTE AND TEST ALLOWABLE TOLERANCES VERSUS LOCAL ERROR ESTIMATES
C     AND REMOVE SCALING OF TOLERANCES. NOTE THAT RELATIVE ERROR IS
C     MEASURED WITH RESPECT TO THE AVERAGE OF THE MAGNITUDES OF THE
C     SOLUTION AT THE BEGINNING AND END OF THE STEP.
C
      EEOET=0.
      DO 250 K=1,NEQN
        ET=ABS(Y(K))+ABS(F1(K))+AE
        IF (ET .GT. 0.) GO TO 240
C
C       INAPPROPRIATE ERROR TOLERANCE
        IFLAG=5
        KFLAG=5
        RETURN
C
  240   EE=ABS((-2090.*YP(K)+(21970.*F3(K)-15048.*F4(K)))+
     1                        (22528.*F2(K)-27360.*F5(K)))
  250   EEOET=AMAX1(EEOET,EE/ET)
C
      ESTTOL=ABS(H)*EEOET*SCALE/752400.
C
      IF (ESTTOL .LE. 1.) GO TO 260
C
C
C     UNSUCCESSFUL STEP
C                       REDUCE THE STEPSIZE , TRY AGAIN
C                       THE DECREASE IS LIMITED TO A FACTOR OF 1/10
C
      HFAILD= .TRUE.
      OUTPUT= .FALSE.
      S=0.1
      IF (ESTTOL .LT. 59049.) S=0.9/ESTTOL**0.2
      H=S*H
      IF (ABS(H) .GT. HMIN) GO TO 200
C
C     REQUESTED ERROR UNATTAINABLE AT SMALLEST ALLOWABLE STEPSIZE
      IFLAG=6
      KFLAG=6
      RETURN
C
C
C     SUCCESSFUL STEP
C                        STORE SOLUTION AT T+H
C                        AND EVALUATE DERIVATIVES THERE
C
  260 T=T+H
      DO 270 K=1,NEQN
  270   Y(K)=F1(K)
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
C
C
C                       CHOOSE NEXT STEPSIZE
C                       THE INCREASE IS LIMITED TO A FACTOR OF 5
C                       IF STEP FAILURE HAS JUST OCCURRED, NEXT
C                          STEPSIZE IS NOT ALLOWED TO INCREASE
C
      S=5.
      IF (ESTTOL .GT. 1.889568E-4) S=0.9/ESTTOL**0.2
      IF (HFAILD) S=AMIN1(S,1.)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
C
C***********************************************************************
C     END OF CORE INTEGRATOR
C***********************************************************************
C
C
C
C     SHOULD WE TAKE ANOTHER STEP
C
      IF (OUTPUT) GO TO 300
      IF (IFLAG .GT. 0) GO TO 100
C
C***********************************************************************
C***********************************************************************
C
C
C     INTEGRATION SUCCESSFULLY COMPLETED
C
C     ONE-STEP MODE
      IFLAG=-2
      RETURN
C
C     INTERVAL MODE
  300 T=TOUT
      IFLAG=2
      RETURN
C
      END
C Output from Public domain Ratfor, version 1.0
      subroutine seval(points,d,np,dblx,n,odd,m,lam,nlam,sol,slen,f,
     * flen, dbla,alen,ximxj,yout,y,ny)
      integer d,np,n,odd,m,nlam,slen,flen,f(flen),alen,ny,i1,i2,i3
      double precision points(d,np),dblx(d,n),lam(nlam),
     * sol(slen,ny,nlam)
C    * sol(slen,ny,nlam), sol1(slen)
      double precision dbla(alen),ximxj(d),yout,y(ny,np,nlam)
C     double precision dbla(alen),ximxj(d),yout,y(ny,np,nlam),ppt(d)
      do 23000 i3=1,nlam
      do 23002 i2=1,np
      do 23004 i1=1,ny
      call mat(points(1,i2),dblx,d,n,odd,m,lam(i3), sol(1,i1,i3),slen,f,
     *flen,dbla,alen, ximxj,yout)
C replacing "1" to get the proper dimension for the 'mat' call
C     do 23105 i4=1,d
C     ppt(i4) = points(i4,i2)
C 23105 continue
C       do 23106 i4=1,slen
C       sol1(i4) = sol(i4,i1,i3) 
C 23106 continue
C     call mat(ppt,dblx,d,n,odd,m,lam(i3), sol1,slen,f,
C    *flen,dbla,alen, ximxj,yout)
      y(i1,i2,i3) = yout
23004 continue
C 23005 continue
23002 continue
C 23003 continue
23000 continue
C 23001 continue
      return
      end
      subroutine tcon(f,d,m,odd,conval)
      integer d,m,f(d+m),odd
      double precision conval,pi,gamma
      real arg

      pi = atan(1.0) * 4.d0
      if (odd.eq.1) goto 20
      conval = 2**(2*m-1) * pi**(d/2) * f(m) * f(m-d/2+1) /
     +         (-1)**(d/2+1)
      return

   20 gamma = sqrt(pi)
        do 30 arg=0,(m-(d+1)/2)
        gamma = gamma / (-arg-1./2.)
   30   continue
      conval = 2**(2*m) * pi**(d/2) * f(m) / (-1)**m / gamma
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine tform(x,add)
      real x(2),ads(5,2),xdiff,ydiff,ray
      double precision add(5,2)
      integer xlen,dcnt,i1,i2
      real xdata(1000,2),spcoef(1003,5)
      common /spinfo/ xlen, dcnt, xdata, spcoef
      do 23000 i2 = 1,dcnt
      ads(i2,1) = spcoef(xlen+2,i2)
      ads(i2,2) = spcoef(xlen+3,i2)
23000 continue
23001 continue
      do 23002 i1 = 1,xlen
      xdiff = x(1) - xdata(i1,1)
      ydiff = x(2) - xdata(i1,2)
      ray = log( (xdiff**2) + (ydiff**2) ) + 1.0
      do 23004 i2 = 1,dcnt
      ads(i2,1) = ads(i2,1) + spcoef(i1,i2) * xdiff * ray
      ads(i2,2) = ads(i2,2) + spcoef(i1,i2) * ydiff * ray
23004 continue
23005 continue
23002 continue
23003 continue
      do 23006 i2 = 1,dcnt
      add(i2,1) = dble( ads(i2,1) )
      add(i2,2) = dble( ads(i2,2) )
23006 continue
23007 continue
      return
      end
