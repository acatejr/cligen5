c
c
      INTEGER FUNCTION ipmpar(i)
c-----------------------------------------------------------------------
c
c     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
c     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
c     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...
c
c  INTEGERS.
c
c     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM
c
c               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )
c
c               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.
c
c     IPMPAR(1) = A, THE BASE.
c
c     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS.
c
c     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE.
c
c  FLOATING-POINT NUMBERS.
c
c     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
c     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
c     NONZERO NUMBERS ARE REPRESENTED IN THE FORM
c
c               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)
c
c               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
c               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.
c
c     IPMPAR(4) = B, THE BASE.
c
c  SINGLE-PRECISION
c
c     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.
c
c     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.
c
c     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.
c
c  DOUBLE-PRECISION
c
c     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.
c
c     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.
c
c     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.
c
c-----------------------------------------------------------------------
c
c     TO DEFINE THIS FUNCTION FOR THE COMPUTER BEING USED, ACTIVATE
c     THE DATA STATMENTS FOR THE COMPUTER BY REMOVING THE C FROM
c     COLUMN 1. (ALL THE OTHER DATA STATEMENTS SHOULD HAVE C IN
c     COLUMN 1.)
c
c-----------------------------------------------------------------------
c
c     IPMPAR IS AN ADAPTATION OF THE FUNCTION I1MACH, WRITTEN BY
c     P.A. FOX, A.D. HALL, AND N.L. SCHRYER (BELL LABORATORIES).
c     IPMPAR WAS FORMED BY A.H. MORRIS (NSWC). THE CONSTANTS ARE
c     FROM BELL LABORATORIES, NSWC, AND OTHER SOURCES.
c
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      INTEGER i
c     ..
c     .. Local Arrays ..
      INTEGER imach(10)
c     ..
c     .. Data statements ..
c
c     MACHINE CONSTANTS FOR AMDAHL MACHINES.
c
c     DATA IMACH( 1) /   2 /
c     DATA IMACH( 2) /  31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /  16 /
c     DATA IMACH( 5) /   6 /
c     DATA IMACH( 6) / -64 /
c     DATA IMACH( 7) /  63 /
c     DATA IMACH( 8) /  14 /
c     DATA IMACH( 9) / -64 /
c     DATA IMACH(10) /  63 /
c
c     MACHINE CONSTANTS FOR THE AT&T 3B SERIES, AT&T
c     PC 7300, AND AT&T 6300.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   33 /
c     DATA IMACH( 3) / 8589934591 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -256 /
c     DATA IMACH( 7) /  255 /
c     DATA IMACH( 8) /   60 /
c     DATA IMACH( 9) / -256 /
c     DATA IMACH(10) /  255 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   39 /
c     DATA IMACH( 3) / 549755813887 /
c     DATA IMACH( 4) /    8 /
c     DATA IMACH( 5) /   13 /
c     DATA IMACH( 6) /  -50 /
c     DATA IMACH( 7) /   76 /
c     DATA IMACH( 8) /   26 /
c     DATA IMACH( 9) /  -50 /
c     DATA IMACH(10) /   76 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
c
c     DATA IMACH( 1) /      2 /
c     DATA IMACH( 2) /     39 /
c     DATA IMACH( 3) / 549755813887 /
c     DATA IMACH( 4) /      8 /
c     DATA IMACH( 5) /     13 /
c     DATA IMACH( 6) /    -50 /
c     DATA IMACH( 7) /     76 /
c     DATA IMACH( 8) /     26 /
c     DATA IMACH( 9) / -32754 /
c     DATA IMACH(10) /  32780 /
c
c     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
c     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
c     ARITHMETIC (NOS OPERATING SYSTEM).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   48 /
c     DATA IMACH( 3) / 281474976710655 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   48 /
c     DATA IMACH( 6) / -974 /
c     DATA IMACH( 7) / 1070 /
c     DATA IMACH( 8) /   95 /
c     DATA IMACH( 9) / -926 /
c     DATA IMACH(10) / 1070 /
c
c     MACHINE CONSTANTS FOR THE CDC CYBER 995 64 BIT
c     ARITHMETIC (NOS/VE OPERATING SYSTEM).
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    63 /
c     DATA IMACH( 3) / 9223372036854775807 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    48 /
c     DATA IMACH( 6) / -4096 /
c     DATA IMACH( 7) /  4095 /
c     DATA IMACH( 8) /    96 /
c     DATA IMACH( 9) / -4096 /
c     DATA IMACH(10) /  4095 /
c
c     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    63 /
c     DATA IMACH( 3) / 9223372036854775807 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    47 /
c     DATA IMACH( 6) / -8189 /
c     DATA IMACH( 7) /  8190 /
c     DATA IMACH( 8) /    94 /
c     DATA IMACH( 9) / -8099 /
c     DATA IMACH(10) /  8190 /
c
c     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /   16 /
c     DATA IMACH( 5) /    6 /
c     DATA IMACH( 6) /  -64 /
c     DATA IMACH( 7) /   63 /
c     DATA IMACH( 8) /   14 /
c     DATA IMACH( 9) /  -64 /
c     DATA IMACH(10) /   63 /
c
c     MACHINE CONSTANTS FOR THE HARRIS 220.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   23 /
c     DATA IMACH( 3) / 8388607 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   38 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000
c     AND DPS 8/70 SERIES.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   63 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 2100
c     3 WORD DOUBLE PRECISION OPTION WITH FTN4
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   39 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 2100
c     4 WORD DOUBLE PRECISION OPTION WITH FTN4
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   55 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 9000.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -126 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
c     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
c     5/7/9 AND THE SEL SYSTEMS 85/86.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /   16 /
c     DATA IMACH( 5) /    6 /
c     DATA IMACH( 6) /  -64 /
c     DATA IMACH( 7) /   63 /
c     DATA IMACH( 8) /   14 /
c     DATA IMACH( 9) /  -64 /
c     DATA IMACH(10) /   63 /
c
c     MACHINE CONSTANTS FOR THE IBM PC.
c
c      DATA imach(1)/2/
c      DATA imach(2)/31/
c      DATA imach(3)/2147483647/
c      DATA imach(4)/2/
c      DATA imach(5)/24/
c      DATA imach(6)/-125/
c      DATA imach(7)/128/
c      DATA imach(8)/53/
c      DATA imach(9)/-1021/
c      DATA imach(10)/1024/
c
c     MACHINE CONSTANTS FOR THE MACINTOSH II - ABSOFT
c     MACFORTRAN II.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE MICROVAX - S FORTRAN.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   54 /
c     DATA IMACH( 9) / -101 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   62 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING
c     32-BIT INTEGER ARITHMETIC.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE SILICON GRAPHICS IRIS-4D
c     SERIES (MIPS R3000 PROCESSOR).
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
c     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
c     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
c
      DATA IMACH( 1) /     2 /
      DATA IMACH( 2) /    31 /
      DATA IMACH( 3) / 2147483647 /
      DATA IMACH( 4) /     2 /
      DATA IMACH( 5) /    24 /
      DATA IMACH( 6) /  -125 /
      DATA IMACH( 7) /   128 /
      DATA IMACH( 8) /    53 /
      DATA IMACH( 9) / -1021 /
      DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   60 /
c     DATA IMACH( 9) /-1024 /
c     DATA IMACH(10) / 1023 /
c
c     MACHINE CONSTANTS FOR THE VAX 11/780.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
      ipmpar = imach(i)
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION spmpar(i)
c-----------------------------------------------------------------------
c
c     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
c     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
c     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
c     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
c     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
c
c        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
c
c        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
c
c        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
c
c-----------------------------------------------------------------------
c     WRITTEN BY
c        ALFRED H. MORRIS, JR.
c        NAVAL SURFACE WARFARE CENTER
c        DAHLGREN VIRGINIA
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
c     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
c     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      INTEGER i
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION b,binv,bm1,one,w,z
      INTEGER emax,emin,ibeta,m
c     ..
c     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC dble
c     ..
c     .. Executable Statements ..
c
      IF (i.GT.1) GO TO 10
      b = ipmpar(4)
      m = ipmpar(8)
      spmpar = b** (1-m)
      RETURN
c
   10 IF (i.GT.2) GO TO 20
      b = ipmpar(4)
      emin = ipmpar(9)
      one = dble(1)
      binv = one/b
      w = b** (emin+2)
      spmpar = ((w*binv)*binv)*binv
      RETURN
c
   20 ibeta = ipmpar(4)
      m = ipmpar(8)
      emax = ipmpar(10)
c
      b = ibeta
      bm1 = ibeta - 1
      one = dble(1)
      z = b** (m-1)
      w = ((z-one)*b+bm1)/ (b*z)
c
      z = b** (emax-2)
      spmpar = ((w*z)*b)*b
      RETURN
      END
