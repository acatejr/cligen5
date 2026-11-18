c
c
c -------< ACM Chi-square code from Anderson Cancer Center in Texas: >-------
c
      SUBROUTINE cdfchi(which,p,q,x,df,status,bound)
c**********************************************************************
c
c      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND )
c               Cumulative Distribution Function
c               CHI-Square distribution
c
c
c                              Function
c
c
c     Calculates any one parameter of the chi-square
c     distribution given values for the others.
c
c
c                              Arguments
c
c
c     WHICH --> Integer indicating which of the next three argument
c               values is to be calculated from the others.
c               Legal range: 1..3
c               iwhich = 1 : Calculate P and Q from X and DF
c               iwhich = 2 : Calculate X from P,Q and DF
c               iwhich = 3 : Calculate DF from P,Q and X
c                    INTEGER WHICH
c
c     P <--> The integral from 0 to X of the chi-square
c            distribution.
c            Input range: [0, 1].
c                    DOUBLE PRECISION P
c
c     Q <--> 1-P.
c            Input range: (0, 1].
c            P + Q = 1.0.
c                    DOUBLE PRECISION Q
c
c     X <--> Upper limit of integration of the non-central
c            chi-square distribution.
c            Input range: [0, +infinity).
c            Search range: [0,1E100]
c                    DOUBLE PRECISION X
c
c     DF <--> Degrees of freedom of the
c             chi-square distribution.
c             Input range: (0, +infinity).
c             Search range: [ 1E-100, 1E100]
c                    DOUBLE PRECISION DF
c
c     STATUS <-- 0 if calculation completed correctly
c               -I if input parameter number I is out of range
c                1 if answer appears to be lower than lowest
c                  search bound
c                2 if answer appears to be higher than greatest
c                  search bound
c                3 if P + Q .ne. 1
c               10 indicates error returned from cumgam.  See
c                  references in cdfgam
c                    INTEGER STATUS
c
c     BOUND <-- Undefined if STATUS is 0
c
c               Bound exceeded by parameter number I if STATUS
c               is negative.
c
c               Lower search bound if STATUS is 1.
c
c               Upper search bound if STATUS is 2.
c
c
c                              Method
c
c
c     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of
c     Mathematical Functions   (1966) is used   to reduce the chisqure
c     distribution to the incomplete distribution.
c
c     Computation of other parameters involve a seach for a value that
c     produces  the desired  value  of P.   The search relies  on  the
c     monotinicity of P with the other parameter.
c
c**********************************************************************
c     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-100,inf=1.0D100)
c     ..
c     .. Scalar Arguments ..
      DOUBLE PRECISION bound,df,p,q,x
      INTEGER status,which
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION ccum,cum,fx,porq,pq
      LOGICAL qhi,qleft,qporq
c     ..
c     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
c     ..
c     .. External Subroutines ..
      EXTERNAL cumchi,dinvr,dstinv
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs
c     ..
c
c -- XXX -- Added 4/13/01 -- CRM:
      data porq/0.d0/
      IF (.NOT. ((which.LT.1).OR. (which.GT.3))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 3.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
      IF (.NOT. (x.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
      IF (.NOT. (df.LE.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.1) GO TO 190
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 180
      IF (.NOT. (pq.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = 3
      RETURN

  180 CONTINUE
  190 IF (which.EQ.1) GO TO 220
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 200
      porq = p
      GO TO 210

  200 porq = q
  210 CONTINUE
  220 IF ((1).EQ. (which)) THEN
          status = 0
          CALL cumchi(x,df,p,q)
          IF (porq.GT.1.5D0) THEN
              status = 10
              RETURN

          END IF

      ELSE IF ((2).EQ. (which)) THEN
          x = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,x,fx,qleft,qhi)
  230     IF (.NOT. (status.EQ.1)) GO TO 270
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 240
          fx = cum - p
          GO TO 250

  240     fx = ccum - q
  250     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 260
          status = 10
          RETURN

  260     CALL dinvr(status,x,fx,qleft,qhi)
          GO TO 230

  270     IF (.NOT. (status.EQ.-1)) GO TO 300
          IF (.NOT. (qleft)) GO TO 280
          status = 1
          bound = 0.0D0
          GO TO 290

  280     status = 2
          bound = inf
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
          df = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,df,fx,qleft,qhi)
  310     IF (.NOT. (status.EQ.1)) GO TO 350
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 320
          fx = cum - p
          GO TO 330

  320     fx = ccum - q
  330     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 340
          status = 10
          RETURN

  340     CALL dinvr(status,df,fx,qleft,qhi)
          GO TO 310

  350     IF (.NOT. (status.EQ.-1)) GO TO 380
          IF (.NOT. (qleft)) GO TO 360
          status = 1
          bound = zero
          GO TO 370

  360     status = 2
          bound = inf
  370     CONTINUE
  380 END IF

      RETURN
      END
c
c
      SUBROUTINE cumchi(x,df,cum,ccum)
c**********************************************************************
c
c     SUBROUTINE FUNCTION CUMCHI(X,DF,CUM,CCUM)
c             CUMulative of the CHi-square distribution
c
c
c                              Function
c
c
c     Calculates the cumulative chi-square distribution.
c
c
c                              Arguments
c
c
c     X       --> Upper limit of integration of the
c                 chi-square distribution.
c                                                 X is DOUBLE PRECISION
c
c     DF      --> Degrees of freedom of the
c                 chi-square distribution.
c                                                 DF is DOUBLE PRECISION
c
c     CUM <-- Cumulative chi-square distribution.
c                                                 CUM is DOUBLE PRECISIO
c
c     CCUM <-- Compliment of Cumulative chi-square distribution.
c                                                 CCUM is DOUBLE PRECISI
c
c
c                              Method
c
c
c     Calls incomplete gamma function (CUMGAM)
c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION df,x,cum,ccum
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a,xx
c     ..
c     .. External Subroutines ..
      EXTERNAL cumgam
c     ..
c     .. Executable Statements ..
      a = df*0.5D0
      xx = x*0.5D0
      CALL cumgam(xx,a,cum,ccum)
      RETURN
      END
c
c
      SUBROUTINE cumgam(x,a,cum,ccum)
c**********************************************************************
c
c     SUBROUTINE CUMGAM(X,A,CUM,CCUM)
c           Double precision cUMulative incomplete GAMma distribution
c
c
c                              Function
c
c
c     Computes   the  cumulative        of    the     incomplete   gamma
c     distribution, i.e., the integral from 0 to X of
c          (1/GAM(A))*EXP(-T)*T**(A-1) DT
c     where GAM(A) is the complete gamma function of A, i.e.,
c          GAM(A) = integral from 0 to infinity of
c                    EXP(-T)*T**(A-1) DT
c
c
c                              Arguments
c
c
c     X --> The upper limit of integration of the incomplete gamma.
c                                                X is DOUBLE PRECISION
c
c     A --> The shape parameter of the incomplete gamma.
c                                                A is DOUBLE PRECISION
c
c     CUM <-- Cumulative incomplete gamma distribution.
c                                        CUM is DOUBLE PRECISION
c
c     CCUM <-- Compliment of Cumulative incomplete gamma distribution.
c                                                CCUM is DOUBLE PRECISIO
c
c
c                              Method
c
c
c     Calls the routine GRATIO.
c
c**********************************************************************
c
c     ..
c     .. Scalar Arguments ..
      DOUBLE PRECISION a,x,cum,ccum
c     ..
c     .. External Routines ..
      EXTERNAL gratio
c     ..
c     .. Executable Statements ..
      IF (.NOT. (x.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 CALL gratio(a,x,cum,ccum,0)

c     Call gratio routine

      RETURN
      END
