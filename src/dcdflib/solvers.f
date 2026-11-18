c
c
      SUBROUTINE dinvr(status,x,fx,qleft,qhi)
c**********************************************************************
c
c     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI)
c          Double precision
c          bounds the zero of the function and invokes zror
c                    Reverse Communication
c
c
c                              Function
c
c
c     Bounds the    function  and  invokes  ZROR   to perform the   zero
c     finding.  STINVR  must  have   been  called  before this   routine
c     in order to set its parameters.
c
c
c                              Arguments
c
c
c     STATUS <--> At the beginning of a zero finding problem, STATUS
c                 should be set to 0 and INVR invoked.  (The value
c                 of parameters other than X will be ignored on this cal
c
c                 When INVR needs the function evaluated, it will set
c                 STATUS to 1 and return.  The value of the function
c                 should be set in FX and INVR again called without
c                 changing any of its other parameters.
c
c                 When INVR has finished without error, it will return
c                 with STATUS 0.  In that case X is approximately a root
c                 of F(X).
c
c                 If INVR cannot bound the function, it returns status
c                 -1 and sets QLEFT and QHI.
c                         INTEGER STATUS
c
c     X <-- The value of X at which F(X) is to be evaluated.
c                         DOUBLE PRECISION X
c
c     FX --> The value of F(X) calculated when INVR returns with
c            STATUS = 1.
c                         DOUBLE PRECISION FX
c
c     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
c          case it is .TRUE. If the stepping search terminated
c          unsucessfully at SMALL.  If it is .FALSE. the search
c          terminated unsucessfully at BIG.
c                    QLEFT is LOGICAL
c
c     QHI <-- Defined only if QMFINV returns .FALSE.  In that
c          case it is .TRUE. if F(X) .GT. Y at the termination
c          of the search and .FALSE. if F(X) .LT. Y at the
c          termination of the search.
c                    QHI is LOGICAL

c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,zabsst,zabsto,zbig,zrelst,zrelto,zsmall,
     +                 zstpmu
      INTEGER status
      LOGICAL qhi,qleft
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION absstp,abstol,big,fbig,fsmall,relstp,reltol,
     +                 small,step,stpmul,xhi,xlb,xlo,xsave,xub,yy,zx,zy,
     +                 zz
      INTEGER i99999
      LOGICAL qbdd,qcond,qdum1,qdum2,qincr,qlim,qok,qup
c     ..
c     .. External Subroutines ..
      EXTERNAL dstzr,dzror
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,max,min
c     ..
c     .. Statement Functions ..
      LOGICAL qxmon
c     ..
c     .. Save statement ..
      SAVE
c     ..
c     .. Statement Function definitions ..
      qxmon(zx,zy,zz) = zx .LE. zy .AND. zy .LE. zz
c     ..
c     .. Executable Statements ..

      IF (status.GT.0) GO TO 310

      qcond = .NOT. qxmon(small,x,big)
      IF (qcond) STOP ' SMALL, X, BIG not monotone in INVR'
      xsave = x
c
c     See that SMALL and BIG bound the zero and set QINCR
c
      x = small
c     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 300

   10 fsmall = fx
      x = big
c     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 300

   20 fbig = fx
      qincr = fbig .GT. fsmall
      IF (.NOT. (qincr)) GO TO 50
      IF (fsmall.LE.0.0D0) GO TO 30
      status = -1
      qleft = .TRUE.
      qhi = .TRUE.
      RETURN

   30 IF (fbig.GE.0.0D0) GO TO 40
      status = -1
      qleft = .FALSE.
      qhi = .FALSE.
      RETURN

   40 GO TO 80

   50 IF (fsmall.GE.0.0D0) GO TO 60
      status = -1
      qleft = .TRUE.
      qhi = .FALSE.
      RETURN

   60 IF (fbig.LE.0.0D0) GO TO 70
      status = -1
      qleft = .FALSE.
      qhi = .TRUE.
      RETURN

   70 CONTINUE
   80 x = xsave
      step = max(absstp,relstp*abs(x))
c      YY = F(X) - Y
c     GET-FUNCTION-VALUE
      ASSIGN 90 TO i99999
      GO TO 300

   90 yy = fx
      IF (.NOT. (yy.EQ.0.0D0)) GO TO 100
      status = 0
      qok = .TRUE.
      RETURN

  100 qup = (qincr .AND. (yy.LT.0.0D0)) .OR.
     +      (.NOT.qincr .AND. (yy.GT.0.0D0))
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     HANDLE CASE IN WHICH WE MUST STEP HIGHER
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (.NOT. (qup)) GO TO 170
      xlb = xsave
      xub = min(xlb+step,big)
      GO TO 120

  110 IF (qcond) GO TO 150
c      YY = F(XUB) - Y
  120 x = xub
c     GET-FUNCTION-VALUE
      ASSIGN 130 TO i99999
      GO TO 300

  130 yy = fx
      qbdd = (qincr .AND. (yy.GE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.LE.0.0D0))
      qlim = xub .GE. big
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 140
      step = stpmul*step
      xlb = xub
      xub = min(xlb+step,big)
  140 GO TO 110

  150 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 160
      status = -1
      qleft = .FALSE.
      qhi = .NOT. qincr
      x = big
      RETURN

  160 GO TO 240
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     HANDLE CASE IN WHICH WE MUST STEP LOWER
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  170 xub = xsave
      xlb = max(xub-step,small)
      GO TO 190

  180 IF (qcond) GO TO 220
c      YY = F(XLB) - Y
  190 x = xlb
c     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 300

  200 yy = fx
      qbdd = (qincr .AND. (yy.LE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.GE.0.0D0))
      qlim = xlb .LE. small
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 210
      step = stpmul*step
      xub = xlb
      xlb = max(xub-step,small)
  210 GO TO 180

  220 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 230
      status = -1
      qleft = .TRUE.
      qhi = qincr
      x = small
      RETURN

  230 CONTINUE
  240 CALL dstzr(xlb,xub,abstol,reltol)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      status = 0
      GO TO 260

  250 IF (.NOT. (status.EQ.1)) GO TO 290
  260 CALL dzror(status,x,fx,xlo,xhi,qdum1,qdum2)
      IF (.NOT. (status.EQ.1)) GO TO 280
c     GET-FUNCTION-VALUE
      ASSIGN 270 TO i99999
      GO TO 300

  270 CONTINUE
  280 GO TO 250

  290 x = xlo
      status = 0
      RETURN

      ENTRY dstinv(zsmall,zbig,zabsst,zrelst,zstpmu,zabsto,zrelto)
c**********************************************************************
c
c      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL,
c     +                   ABSTOL, RELTOL )
c      Double Precision - SeT INverse finder - Reverse Communication
c
c
c                              Function
c
c
c     Concise Description - Given a monotone function F finds X
c     such that F(X) = Y.  Uses Reverse communication -- see invr.
c     This routine sets quantities needed by INVR.
c
c          More Precise Description of INVR -
c
c     F must be a monotone function, the results of QMFINV are
c     otherwise undefined.  QINCR must be .TRUE. if F is non-
c     decreasing and .FALSE. if F is non-increasing.
c
c     QMFINV will return .TRUE. if and only if F(SMALL) and
c     F(BIG) bracket Y, i. e.,
c          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
c          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
c
c     if QMFINV returns .TRUE., then the X returned satisfies
c     the following condition.  let
c               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
c     then if QINCR is .TRUE.,
c          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
c     and if QINCR is .FALSE.
c          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
c
c
c                              Arguments
c
c
c     SMALL --> The left endpoint of the interval to be
c          searched for a solution.
c                    SMALL is DOUBLE PRECISION
c
c     BIG --> The right endpoint of the interval to be
c          searched for a solution.
c                    BIG is DOUBLE PRECISION
c
c     ABSSTP, RELSTP --> The initial step size in the search
c          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
c                    ABSSTP is DOUBLE PRECISION
c                    RELSTP is DOUBLE PRECISION
c
c     STPMUL --> When a step doesn't bound the zero, the step
c                size is multiplied by STPMUL and another step
c                taken.  A popular value is 2.0
c                    DOUBLE PRECISION STPMUL
c
c     ABSTOL, RELTOL --> Two numbers that determine the accuracy
c          of the solution.  See function for a precise definition.
c                    ABSTOL is DOUBLE PRECISION
c                    RELTOL is DOUBLE PRECISION
c
c
c                              Method
c
c
c     Compares F(X) with Y for the input value of X then uses QINCR
c     to determine whether to step left or right to bound the
c     desired x.  the initial step size is
c          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
c     Iteratively steps right or left until it bounds X.
c     At each step which doesn't bound X, the step size is doubled.
c     The routine is careful never to step beyond SMALL or BIG.  If
c     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
c     after setting QLEFT and QHI.
c
c     If X is successfully bounded then Algorithm R of the paper
c     'Two Efficient Algorithms with Guaranteed Convergence for
c     Finding a Zero of a Function' by J. C. P. Bus and
c     T. J. Dekker in ACM Transactions on Mathematical
c     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
c     to find the zero of the function F(X)-Y. This is routine
c     QRZERO.
c
c**********************************************************************
      small = zsmall
      big = zbig
      absstp = zabsst
      relstp = zrelst
      stpmul = zstpmu
      abstol = zabsto
      reltol = zrelto
      RETURN

cc    STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
c     TO GET-FUNCTION-VALUE
  300 status = 1
      RETURN

  310 CONTINUE
      GO TO i99999
      END
c
c
      SUBROUTINE dzror(status,x,fx,xlo,xhi,qleft,qhi)
c**********************************************************************
c
c     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI)
c     Double precision ZeRo of a function -- Reverse Communication
c
c
c                              Function
c
c
c     Performs the zero finding.  STZROR must have been called before
c     this routine in order to set its parameters.
c
c
c                              Arguments
c
c
c     STATUS <--> At the beginning of a zero finding problem, STATUS
c                 should be set to 0 and ZROR invoked.  (The value
c                 of other parameters will be ignored on this call.)
c
c                 When ZROR needs the function evaluated, it will set
c                 STATUS to 1 and return.  The value of the function
c                 should be set in FX and ZROR again called without
c                 changing any of its other parameters.
c
c                 When ZROR has finished without error, it will return
c                 with STATUS 0.  In that case (XLO,XHI) bound the answe
c
c                 If ZROR finds an error (which implies that F(XLO)-Y an
c                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
c                 this case, XLO and XHI are undefined.
c                         INTEGER STATUS
c
c     X <-- The value of X at which F(X) is to be evaluated.
c                         DOUBLE PRECISION X
c
c     FX --> The value of F(X) calculated when ZROR returns with
c            STATUS = 1.
c                         DOUBLE PRECISION FX
c
c     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
c             inverval in X containing the solution below.
c                         DOUBLE PRECISION XLO
c
c     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
c             inverval in X containing the solution above.
c                         DOUBLE PRECISION XHI
c
c     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
c                at XLO.  If it is .FALSE. the search terminated
c                unsucessfully at XHI.
c                    QLEFT is LOGICAL
c
c     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
c              search and .FALSE. if F(X) .LT. Y at the
c              termination of the search.
c                    QHI is LOGICAL
c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,xhi,xlo,zabstl,zreltl,zxhi,zxlo
      INTEGER status
      LOGICAL qhi,qleft
c     ..
c     .. Save statement ..
      SAVE
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a,abstol,b,c,d,fa,fb,fc,fd,fda,fdb,m,mb,p,q,
     +                 reltol,tol,w,xxhi,xxlo,zx
      INTEGER ext,i99999
      LOGICAL first,qrzero
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,max,sign
c     ..
c     .. Statement Functions ..
      DOUBLE PRECISION ftol
c     ..
c     .. Statement Function definitions ..
      ftol(zx) = 0.5D0*max(abstol,reltol*abs(zx))
c     ..
c     .. Executable Statements ..

      IF (status.GT.0) GO TO 280
      xlo = xxlo
      xhi = xxhi
      b = xlo
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 270

   10 fb = fx
      xlo = xhi
      a = xlo
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 270
c
c     Check that F(ZXLO) < 0 < F(ZXHI)  or
c                F(ZXLO) > 0 > F(ZXHI)
c
   20 IF (.NOT. (fb.LT.0.0D0)) GO TO 40
      IF (.NOT. (fx.LT.0.0D0)) GO TO 30
      status = -1
      qleft = fx .LT. fb
      qhi = .FALSE.
      RETURN

   30 CONTINUE
   40 IF (.NOT. (fb.GT.0.0D0)) GO TO 60
      IF (.NOT. (fx.GT.0.0D0)) GO TO 50
      status = -1
      qleft = fx .GT. fb
      qhi = .TRUE.
      RETURN

   50 CONTINUE
   60 fa = fx
c
      first = .TRUE.
   70 c = a
      fc = fa
      ext = 0
   80 IF (.NOT. (abs(fc).LT.abs(fb))) GO TO 100
      IF (.NOT. (c.NE.a)) GO TO 90
      d = a
      fd = fa
   90 a = b
      fa = fb
      xlo = c
      b = xlo
      fb = fc
      c = a
      fc = fa
  100 tol = ftol(xlo)
      m = (c+b)*.5D0
      mb = m - b
      IF (.NOT. (abs(mb).GT.tol)) GO TO 240
      IF (.NOT. (ext.GT.3)) GO TO 110
      w = mb
      GO TO 190

  110 tol = sign(tol,mb)
      p = (b-a)*fb
      IF (.NOT. (first)) GO TO 120
      q = fa - fb
      first = .FALSE.
      GO TO 130

  120 fdb = (fd-fb)/ (d-b)
      fda = (fd-fa)/ (d-a)
      p = fda*p
      q = fdb*fa - fda*fb
  130 IF (.NOT. (p.LT.0.0D0)) GO TO 140
      p = -p
      q = -q
  140 IF (ext.EQ.3) p = p*2.0D0
      IF (.NOT. ((p*1.0D0).EQ.0.0D0.OR.p.LE. (q*tol))) GO TO 150
      w = tol
      GO TO 180

  150 IF (.NOT. (p.LT. (mb*q))) GO TO 160
      w = p/q
      GO TO 170

  160 w = mb
  170 CONTINUE
  180 CONTINUE
  190 d = a
      fd = fa
      a = b
      fa = fb
      b = b + w
      xlo = b
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 270

  200 fb = fx
      IF (.NOT. ((fc*fb).GE.0.0D0)) GO TO 210
      GO TO 70

  210 IF (.NOT. (w.EQ.mb)) GO TO 220
      ext = 0
      GO TO 230

  220 ext = ext + 1
  230 GO TO 80

  240 xhi = c
      qrzero = (fc.GE.0.0D0 .AND. fb.LE.0.0D0) .OR.
     +         (fc.LT.0.0D0 .AND. fb.GE.0.0D0)
      IF (.NOT. (qrzero)) GO TO 250
      status = 0
      GO TO 260

  250 status = -1
  260 RETURN

      ENTRY dstzr(zxlo,zxhi,zabstl,zreltl)
c**********************************************************************
c
c     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL )
c     Double precision SeT ZeRo finder - Reverse communication version
c
c
c                              Function
c
c
c
c     Sets quantities needed by ZROR.  The function of ZROR
c     and the quantities set is given here.
c
c     Concise Description - Given a function F
c     find XLO such that F(XLO) = 0.
c
c          More Precise Description -
c
c     Input condition. F is a double precision function of a single
c     double precision argument and XLO and XHI are such that
c          F(XLO)*F(XHI)  .LE.  0.0
c
c     If the input condition is met, QRZERO returns .TRUE.
c     and output values of XLO and XHI satisfy the following
c          F(XLO)*F(XHI)  .LE. 0.
c          ABS(F(XLO)  .LE. ABS(F(XHI)
c          ABS(XLO-XHI)  .LE. TOL(X)
c     where
c          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
c
c     If this algorithm does not find XLO and XHI satisfying
c     these conditions then QRZERO returns .FALSE.  This
c     implies that the input condition was not met.
c
c
c                              Arguments
c
c
c     XLO --> The left endpoint of the interval to be
c           searched for a solution.
c                    XLO is DOUBLE PRECISION
c
c     XHI --> The right endpoint of the interval to be
c           for a solution.
c                    XHI is DOUBLE PRECISION
c
c     ABSTOL, RELTOL --> Two numbers that determine the accuracy
c                      of the solution.  See function for a
c                      precise definition.
c                    ABSTOL is DOUBLE PRECISION
c                    RELTOL is DOUBLE PRECISION
c
c
c                              Method
c
c
c     Algorithm R of the paper 'Two Efficient Algorithms with
c     Guaranteed Convergence for Finding a Zero of a Function'
c     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
c     Mathematical Software, Volume 1, no. 4 page 330
c     (Dec. '75) is employed to find the zero of F(X)-Y.
c
c**********************************************************************
      xxlo = zxlo
      xxhi = zxhi
      abstol = zabstl
      reltol = zreltl
      RETURN

cc    STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
c     TO GET-FUNCTION-VALUE
  270 status = 1
      RETURN

  280 CONTINUE
      GO TO i99999
      END
