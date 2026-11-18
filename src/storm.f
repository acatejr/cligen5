c
c
c
      subroutine alph
c
c     + + + PURPOSE + + +
c     Computes alpha, a dimensionless parameter that expresses the fraction 
c     of total rainfall that occurs during 0.5 ho.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstg
c
c     + + + END SPECIFICATIONS + + +
c
      ei=r(ida)-sml
      ai=ab1/(wi(mo)-ab)
      if (ei.lt.1.0) then
        ajp=1.0
      else
        ajp=1.0-exp(-5.0/ei)
      endif
      r1=dstg(ai,k7)
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
      return
      end
c
c
      real function dstg
     i         (ai,k7)
c
c     + + + PURPOSE + + +
c     Provides numbers from a gamma PDF f(x) given two random numbers.
c     This is used in Cligen to calculate alpha_0.5, the ratio of the
c     maximum 30-min rainfall to the total rainfall; ie, intensity for
c     individual events.
c
c     The result is a gamma distribution normalized in the y-direction,
c     and normalized in the x-direction from zero to 'ai', the maximum
c     x-value for the current month.  (Note: 5.795 was substituted for 
c     ai for reasons explained in comments at the top of this code.)
c
c     This is a general approach that can be used to generate random
c     deviates for *ANY* PDF.  For explanation of method, see "Handbook
c     of Mathematical Functions with Formulas, Graphs, and Mathematical 
c     Tables"; Milton Abramowitz and Irene A. Stegun, Eds.; 1965; Dover
c     Publications, Inc., NY, NY; pp. 925-996.  Of course the procedure
c     does require a properly functioning uniform random number generator.
c
c     Note that N. T. Kottegoda gives an alternate algorithm on p. 103ff
c     of "Stochastic Water Resources Technology", Wiley & Son NY, 1980.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ai,rn1
      integer k7(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ai          - Maximum value used in the "x" direction
c     k7          - Seed for random number generation.
c     rn1         - Random Gamma deviate returned from DSTG.
c
      include 'crandom3.inc'
c
c     + + + DATA INITIALIZATIONS + + +
c     data xn1/10.0/
c
c     Modified by Bofu Yu on July 4, 1999
c                        based on break-point data, B.YU
      data xn1/6.28/
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
      double precision fu,xx
c
c -- Added for QC 10/2003 -- CRM:
      integer level0,level1,itryct,itryc2,iarrct
      integer flg_10,flg_21
      real array(30)
c     level0 -- pass/fail (0/1) on chi-square test with up to 20 bins.
c     level1 -- pass/fail (0/1) on K-S test with up to 20 bins.
c     itryct -- count number of tries to generate Uniform set of 20, this call.
c     itryc2 -- count number of tries to generate Gamma deviate, this call.
c     flg_10 -- flag, return to '10' if set.
c     flg_21 -- flag, return to '21' if set.
c     iarrct -- index of current element in "array"
c      array -- array of next 20 available uniform deviates.
      data iarrct /30/
      save array,iarrct
c
c     + + + END SPECIFICATIONS + + +
c
      itryct = 0
      itryc2 = 0
c
 10   continue
        flg_10 = 0
c
c------Generate 20 uniform random deviates, and check with chi-square test.
        if(iarrct .eq. 30) then
 21       continue
            flg_21 = 0
            itryct = itryct + 1
            do 25, i=1,30
              array(i) = randn(k7)
              ichi = int(array(i)*20.0) + 1
              chicnt(10,mox,ichi) = chicnt(10,mox,ichi) + 1
 25         continue
C           call chitst(10,level0)
            call ks_tst(10,level1)
c
c----------passed chitst
C           if(level0 .eq. 0) then
c----------passed ks_tst
            if(level1 .eq. 0) then
              iarrct = 0
c----------failed chitst
            else
              if(itryct .eq. 10000) then
c -------------- Write exactly once.
                write(*,*) 'Uniform could not succeed in 10,000 tries.'
                write(*,*) 'Precipitation Intensities are suspect.'
                iarrct = 0
                flg_21 = 0
              else
                do 26, i=1,20
                  ichi = int(array(i)*20.0) + 1
                  chicnt(10,mox,ichi) = chicnt(10,mox,ichi) - 1
 26             continue
                flg_21 = 1
              endif
            endif
          if(flg_21 .ne. 0) goto 21
        endif
c
c -------- Grab next two uniform random deviates.
        iarrct = iarrct + 1
        rn1=array(iarrct)
        iarrct = iarrct + 1
        rn=array(iarrct)
        itryc2 = itryc2 + 1
c
        dstg=rn1
c
c------- Random "x" Deviate normalized to the max-x value:
        xx=rn1*ai
CC      xx=rn1*5.795
c------- f(x) value computed from "x" deviate (above):
        fu=xx**xn1*exp(xn1*(1.0-xx))
c------- If Gamma Deviate gets Rejected:
        if(fu.lt.rn) then
          flg_10 = 1
          itryc2 = itryc2 + 1
          if(itryc2 .eq. 10000) then
            write(*,*) 'Gamma could not succeed in 10,000 tries.'
            write(*,*) 'Precipitation Intensities are suspect.'
            flg_10 = 0
            dstg=rn1
          endif
        endif
      if(flg_10 .ne.0) goto 10
c
      return
      end
c
c
c
      subroutine r5mon
     i           (tp6)
c
c     + + + PURPOSE + + +
c     Smoothes and corrects .r hour monthly rainfall intensity data.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real tp6
c
c     + + + ARGUMENT DEFINITIONS + + +
c     tp6         - maximum 6 hour precipitation depth (inches).
c
c     + + + COMMON BLOCKS + + +
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c
      include 'cbk7.inc'
c      modify: rst,prw 
c     rst(i,j)    - Array Of Monthly precipitation stats.
c                       dim1: month 1..12
c                       dim2: 1=mean of daily rainfall
c                               mean liquid equivalent precipitation
c                               depth (inches) for a day precipitation
c                               occurs (by month) [=avg total precip
c                               for month / # wet days in month]
c                             2=std deviation of daily rainfall
c                               standard deviation of the daily precip
c                               value (inches) (by month)
c                             3=skew coefficient of daily rainfall
c
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c
      include 'cbk4.inc'
c      read: nc
c     nc          - Number of days in the (non-leap) year preceeding each month.
c
c     + + + END SPECIFICATIONS + + +
c
      real sm(12),smm(12)
c
      xy2=0.5/tp6
c ---- convert WI in inches to WI in mm.
      do 10 i=1,12
         wi(i)=wi(i)*25.4
 10   continue
c
c ---- average each month's WI with the month on either side.
      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0
c
      do 30 i=1,12
         if(prw(i,1).eq.0.0) prw(i,1)=0.01
         if(prw(i,2).eq.0.0) prw(i,2)=0.01
         if(rst(i,1).eq.0.0) rst(i,1)=0.01
         xm=nc(i+1)-nc(i)
c -- XXX -- The equation below is backwards from the one to compute
c           Historic Monthly Precip amount -- CRM -- 7/30/00.
         smm(i)=xm*prw(i,1)/(1.0-prw(i,2)+prw(i,1))
         r25=rst(i,1)
         f=xy2/smm(i)
         wi(i)=-sm(i)/alog(f)
         wi(i)=1.0-exp(-wi(i)/r25)
         if (wi(i).lt.0.1) wi(i)=0.1
         if (wi(i).gt.0.95) wi(i)=0.95
 30   continue
c
      return
      end
c
c
c
      real function timepk
     i         (timpkd,k10)
c
c     + + + PURPOSE + + +
c     Calculates the time to peak from the 12 interval time to peak
c     accummulated distribution parameters TIMPKD input for each station
c     location.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real timpkd(0:12)
      integer k10(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     k10         - Seed for random number generation.
c
      include 'crandom3.inc'
c        read: zx, dax
c       write: z
      include 'cbk4.inc'
c         read: iopt 
c      
CC    include 'ctap2.inc'
c
c     + + + END SPECIFICATIONS + + +
c
      if (iopt.eq.6) then
         z = randn(k10)
      else
         z=zx(dax)
      endif
      
CC    tap4=z
C     write(*,*) tap4
      i=0
 10   continue
        i=i+1
      if(timpkd(i).lt.z .and. i.lt.12) goto 10
c
      diff1= timpkd(i)-z
      diff2=timpkd(i) - timpkd(i-1)
      ratio= diff1/diff2
      timepk=0.08333*i - ratio*0.08333
c
      return
      end
c
c
c ----------------------< Begin Bofu Yu's Corrections: >---------------------
c
c
      subroutine alphb
c
c     This subroutine computes alpha, a dimensionless parameter that
c     expresses the ratio of the max 30-min rain to total rain.
c
c     This subroutine was re-written by B. YU on June 30, 1999.
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1
c
c      dimension k7(4)
c
      ei=r(ida)-sml

c                                           ei is the rainfall for the
c                                           day in inches since sml = 0, B.YU

      ai=ab1/(wi(mo)-ab)

c                                     ai is basically 1/alpha
c                                     ab is the lower bound =0.02083
c                                     ab1 = 1 - ab, B.YU

      if (ei .lt. 1.0) then
        ajp = 1.
c
c                                  if rain < 1 inch, the upper bound is 1, by
c
      else

c               in CLIGEN 4.2, if rain > 1 inch, the upper bound is reduced
c               after Arnold and Williams (1989).  This may be all right for
c               middle latitude without high intensity rain.  Note that
c               the observed maximum 30-min rain ever recorded can be as high as
c               299 mm (Linsley et al 1982). This max. 30-min rain in mm was
c               estimated by Rmm = 417.*t(hr)**0.48.  B.YU.  125 mm is used
c               here to be consistent with Arnold and Williams (1989).
c               Probably more work is required here.

        tmax = 125./25.4
c
        ajp=1.0-exp(-tmax/ei)
      endif

c                              generate random number using parameter ai
c                              and seed k7, by

CRM   r1=dstg(ai,k7,rn1)
      r1=dstg(ai,k7)
C     write(*,*) "R1:", r1

c                              This basically re-scales r1 which is in the
c                              range from 0 to 1 into the range from
c                              ab to ajp, where ajp is the upper bound. by
CCC -- XXX -- Added to output values of Gamma dist for testing -- CRM 5/16/03:
C     write(73,2001) mo, r1
C2001 format(i2,1x,6f17.13)
c
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
c
      return
      end
c
c
      subroutine r5monb
c
c  This subroutine was full of errors,
c  what follows is a complete re-write. B.YU
c
c  This subroutine smoothes max 30-min rainfall data and computes
c  the R30/R ratio for each month.  On return the ratio is stored in the
c  array wi(i), i=1,12.  This subroutine will be called only once in the main
c  program to generate parameters for storm simulation

c  The subroutine was re-written by B. YU on June 30, 1999
c
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1

      dimension sm(12),smm(12)

c                                      Smoothing 30-min rainfall data
c                                      3-month running average

      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0

      do 30 i=1,12

         if(prw(i,2).eq.0.0) then
           smm(i) = 0.0006944
c                               ! = 1 min
         else

c                     xm - no. of calender days for the month,
c                     smm - no. of rain days

           xm=nc(i+1)-nc(i)
           smm(i)=xm*prw(i,2)/(1.0-prw(i,1)+prw(i,2))
         endif

         if(rst(i,1).eq.0.0) then
           r25 = 0.001
c                    so that it won't overflow = 1/1000 of an inch
         else
           r25 = rst(i,1)
c                    rain per rain day for the month
         endif

c                          See the paper for details.  The idea was that
c                          if there are n rain days, the max 30-min rain is
c                          Rmax30, then the mean of the max 30-min rain for
c                          the month will be R30 = - Rmax30/ln(F), where
c                          F is the exceedance frequency for Rmax, which
c                          is approximately 1/(n + 0.5).  The original Hazen
c                          formula or Weibull's formula were not used, although
c                          what is used here is similar to Weibull's when
c                          the number of rain days is large.  B.YU

         f = 1./(smm(i)+0.5)
         f = -1./alog(f)
CRM      if (f .gt. 1.) then
         if (f .gt. 1.0 .or. f .le. 0.0) then

c                         If no. raindays/month < 2.218, no adjustment to
c                         max 30-min rain is made.

           wi(i) = sm(i)
         else
           wi(i) = f*sm(i)
         endif

c                        sm(i) is the smoothed max 30-min rain for the month
c                        wi(i)  is the mean of the max 30-min rain

         wi(i) = wi(i)/r25
c                        alpha = R30/R


30    continue
      return
      end
c
c
c ----------------------< End Bofu Yu's Corrections. >---------------------
c
c
