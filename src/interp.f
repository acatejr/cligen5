      subroutine lintrp
     i           (mo,jd,ntd)
c
c      + + + PURPOSE + + +
c     Used for smoothing between average monthly values (like max & min
c     temp), from which daily values are stochastically generated (using
c     random numbers) by CLIGEN.  Provides factors "lf" & "rf" to perform
c     a linear interpolation for the input date, between the middle of
c     the current month, and the middle of an adjacent month.  Depending
c     on the day of the month, values will be needed for either the
c     previous month, or the following month.  The value "o_mo" is the
c     number of the other month involved in the interpolation.  The
c     interpolated value (Vi) is simply computed as follows:
c       Vi = V(mo)*lf + V(o_mo)*rf .
c
c     Uses linear interpolation.  Outputs "weighting factors" for the
c     average monthly parameters associated with the midpoints of the
c     months on either side of the date that is input.
c
c     Written by Charles R. Meyer 12/02/99 in consultation with Roel Vining.
c
c     Note that for a linear interpolation "lf" is simply the fraction
c     of the distance from the _upper_ time midpoint, and "rf" is the
c     converse at the other end of the interval.  Also lf + rf = 1.0 .
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer mo, jd, ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     mo     - month input.
c     jd     - day of the month input.
c     ntd    - days in this year (365 or 366)
c
c      + + + COMMON BLOCKS + + +
      include 'cinterp.inc'
c      write: lf, rf, o_mo
c     lf     - weighting factor for the midpoint value on this month's end
c              of the time interval.
c     rf     - weighting factor for the midpoint value on the "other" end.
c     o_mo   - month (on the "other" end) whose average value should be used.
c
c      + + + LOCAL VARIABLES + + +
      real mp(12),ni(12)
c
c      + + + LOCAL DEFINITIONS + + +
c     mp      - Number of days from beginning to midpoint of each month
c               (non-leap year).
c     ni      - Number of days from midpoint of previous month to midpoint
c               of current month; ie, number of days in the interval.
c
c     + + + DATA INITIALIZATIONS + + +
      data mp/15.5,14.0,15.5,15.0,15.5,15.0,15.5,15.5,15.0,15.5,15.0,
     1        15.5/
      data ni/31.0,29.5,30.0,30.5,30.5,30.5,30.5,31.0,30.5,30.5,30.5,
     1        30.5/
c
c      + + + END SPECIFICATIONS + + +
c
c ---- Adjust February's midpoint (ni) for Leap Year.
      if(ntd .eq. 366) then
        ni(2) = 30.0
      else
        ni(2) = 29.5
      endif
c
c      There are two intervals related to each month.  The appropriate
c      one to use depends upon the day of the month.  Determine whether
c      it is the previous month, or the following month that is of interest.
c ---- Compare to the next month.
      if(float(jd) .gt. mp(mo)) then
        o_mo = mod(mo+1,12)
        if(o_mo .eq. 0) o_mo = 12
        rf = (jd - mp(mo)) / ni(o_mo)
c ---- Compare to the previous month.
      else
        o_mo = mod(mo-1,12)
        if(o_mo .eq. 0) o_mo = 12
        rf = (mp(mo) - jd) / ni(mo)
      endif
c
      lf = 1.0 - rf
c
      return
      end
c
c
      subroutine fouri1
     i                 (x, indpar)
c     Subroutine to calculate Fourier Series coefficients.
c     Code received 01/21/99 from Clarence Richardson.
c     Modified by C. R. Meyer and verified to give identical results.
c
      real x(12)
      integer indpar
c
c     x      - 12 measured monthly average values (means, sd, skew).
c     indpar - index of the current parameter.
c
c     s      - sum of the x's.
c     x_bar   - average (mean) of the x's.
c
      include 'cinterp.inc'
c     write: c, t
c     c      - fourier series coefficient.
c     t      - fourier series coefficient.
c
c ---- sum the x's:
      s=0.0
      do 5 i=1,12
        s=s+x(i)
  5   continue
c
c ---- compute t & c:
      x_bar(indpar)=s/12.
      do 20 j=1,6
        suma = 0.0
        sumb=0.0
c
        do 10 i=1,12
          d=x(i)-x_bar(indpar)
          v=6.2832*float(i)*float(j)/12.
          suma=suma+(d*cos(v))
          sumb=sumb+(d*sin(v))
 10     continue
c
        a=suma*(2./12.)
        b=sumb*(2./12.)
        t(j,indpar)=atan(-b/a)
        c(j,indpar)=a/cos(t(j,indpar))
 20   continue
c
      return
      end
c
c
      function fouri2
     i               (indpar)
c     Subroutine to calculate Fourier Series representation of seasonal
c     variables.
c     Code received 01/21/99 from Clarence Richardson.
c     Modified by C. R. Meyer and verified to give identical results.
c
      integer indpar
c
c     indpar - index of the current parameter.
c
c     + + + COMMON BLOCKS + + +
      include 'cinterp.inc'
c      read: c, t
c     c      - fourier series coefficient.
c     t      - fourier series coefficient.
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.
c
c + + + END SPECIFICATIONS + + +
c
c ---- compute the daily values from c & t:
      dd=(float(ida)+15.5)/366.
      fouri2 = x_bar(indpar)
     1       + c(1,indpar)*cos(6.2832*float(1)*dd+t(1,indpar))
     2       + c(2,indpar)*cos(6.2832*float(2)*dd+t(2,indpar))
     3       + c(3,indpar)*cos(6.2832*float(3)*dd+t(3,indpar))
     4       + c(4,indpar)*cos(6.2832*float(4)*dd+t(4,indpar))
     5       + c(5,indpar)*cos(6.2832*float(5)*dd+t(5,indpar))
     6       + c(6,indpar)*cos(6.2832*float(6)*dd+t(6,indpar))
c
      return
      end
c
c
      subroutine ryf1
     i               (x, indpar)
c
c     Subroutine 1 of 2 to interpolate preserving the monthly means.
c     This is accomplished in most months by shifting the time that
c     the mean occurs ("pseudo-midpoint").  In months whose mean is
c     a local minimum or maximum, the mid-month value is shifted to
c     a pseudo-value and the mean is assumed to occur at two times,
c     one before the midpoint, and one after.
c
c     Written 8/4/2000 by C. R. Meyer to implement an algorithm
c     from Daniel Yoder, and modified by George Foster in response
c     to C. R. Meyer's criticism of lack of a solution for minima
c     or maxima.  The essential idea is to make the area (under the
c     parameter curve) above the monthly mean the same as the area
c     below it.  This shifts the time of the mean (PMT) from the
c     midpoint of the month.  However, for maxima and minima, it
c     leaves the time at the midpoint, but shifts the value (PMV).
c     The values at the end (and beginning) of each month (EMV)
c     resulting from a simple linear interpolation are utilized in
c     the calculations.
c
c     From the mean value and the length of each month, calculate
c     the interpolated values at the month boundaries.  Also calculate
c     the time(s) of the pseudo-midpoint, of the value.
c
      real x(12)
      integer indpar
c
c     x      - 12 measured monthly average values (means, sd, skew).
c     indpar - index of the current parameter.
c
      include 'cinterp.inc'
c       write: emv, pmt, pmv, xes
c     emv    - End-of-the-Month Value for the monthly mean of each parameter.
c     pmt    - Pseudo-Midpoint Time for each month, for each parameter.
c     pmv    - Pseudo-Midpoint Value for each month, for each parameter.
c     xes    - the monthly values (mean, SD, skew) for the 14 values.
c              (Set to -9999.0 if this is not a Max or Min month.)
c
c      + + + LOCAL VARIABLES + + +
      integer dim(12)
c
c      + + + LOCAL DEFINITIONS + + +
c ---- dim -- Days in each Month.
c
c     + + + DATA INITIALIZATIONS + + +
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c
c      + + + END SPECIFICATIONS + + +
c
c ---- End of Month Values (EMV):
      do 100 i=1,11
        tte = dim(i)/2.0
        tfs = dim(i+1)/2.0
c--------(fraction of time between middles, from middle to end of this month)
        ratio = tte/(tte+tfs)
        emv(i,indpar) = x(i) + (x(i+1)-x(i))*ratio
 100  continue
c--------(December)
        emv(12,indpar) = (x(12)+x(1))*0.5
c--------(End of January Value, Leap Years)
        emv(13,indpar) = x(1) + (x(2)-x(1))*0.516667
c--------(End of February Value, Leap Years)
        emv(14,indpar) = x(2) + (x(3)-x(2))*0.483333
c
c --- Pseudo-Midpoint Times & Values (PMT & PMV):
      do 200 i=2,12
c-------(3 consecutive identical monthly values)
        if(emv(i-1,indpar).eq.x(i).and.emv(i,indpar).eq.x(i)) then
           pmt(i,indpar) = dim(i)/2.0
           pmv(i,indpar) = x(i)
           xes(i,indpar) = -9999
c-------(not a max or min)
        else if(emv(i-1,indpar).lt.x(i).and.emv(i,indpar).gt.x(i) .or.
     1     emv(i-1,indpar).gt.x(i).and.emv(i,indpar).lt.x(i)) then
           pmt(i,indpar) = dim(i) *
     1            (emv(i,indpar)-x(i))/(emv(i,indpar)-emv(i-1,indpar))
           pmv(i,indpar) = x(i)
           xes(i,indpar) = -9999.0
c-------(max, min, or 1 EOM value identical to x(i))
        else
           pmv(i,indpar) = 2.0*x(i)-(emv(i,indpar)+emv(i-1,indpar))/2.0
           pmt(i,indpar) = dim(i)/2.0
           xes(i,indpar) = x(i)
        endif
 200  continue
c--------(January)
        if(emv(12,indpar).eq.x(1).and.emv(1,indpar).eq.x(1)) then
           pmt(1,indpar) = dim(1)/2.0
           pmv(1,indpar) = x(1)
           xes(1,indpar) = -9999
        else if(emv(12,indpar).lt.x(1).and.emv(1,indpar).gt.x(1) .or.
     1     emv(12,indpar).gt.x(1).and.emv(1,indpar).lt.x(1)) then
           pmt(1,indpar) = 31.0 *
     1            (emv(1,indpar)-x(1))/(emv(1,indpar)-emv(12,indpar))
           pmv(1,indpar) = x(1)
           xes(1,indpar) = -9999.0
        else
           pmv(1,indpar) = 2.0*x(1)-(emv(1,indpar)+emv(12,indpar))/2.0
           pmt(1,indpar) = 15.5
           xes(1,indpar) = x(1)
        endif
c--------(February, Leap Years)
        if(emv(1,indpar).eq.x(2).and.emv(2,indpar).eq.x(2)) then
           pmt(13,indpar) = 14.5
           pmv(13,indpar) = x(2)
        else if(emv(1,indpar).lt.x(2).and.emv(2,indpar).gt.x(2) .or.
     1     emv(1,indpar).gt.x(2).and.emv(2,indpar).lt.x(2)) then
           pmt(13,indpar) = 29.0 *
     1            (emv(2,indpar)-x(2))/(emv(2,indpar)-emv(1,indpar))
           pmv(13,indpar) = x(2)
        else
           pmv(13,indpar) = 2.0*x(2)-(emv(2,indpar)+emv(1,indpar))/2.0
           pmt(13,indpar) = 14.5
        endif
c
      return
      end
c
c
      function ryf2
     i               (mo,jd,ntd,indpar)
c     Function 2 of 2 to interpolate preserving the monthly means.
c     From the values of the month endpoints and the time and value
c     of the pseudo-midpoint, calculate a daily value for the X-parameter
c     with index "indpar", for the current day.
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer mo, jd
      integer indpar
c
c      + + + ARGUMENT DEFINITIONS + + +
c     mo     - month input.
c     jd     - day of the month input.
c     indpar - index of the current parameter.
c     ntd    - days in this year (365 or 366)
c
      include 'cinterp.inc'
c        read: emv, pmt, pmv, xes
c     emv    - End-of-the-Month Value for the monthly mean of each parameter.
c     pmt    - Pseudo-Midpoint Time for each month, for each parameter.
c     pmv    - Pseudo-Midpoint Value for each month, for each parameter.
c
c      + + + LOCAL VARIABLES + + +
      integer dim(12)
      real idim,ipmt,ipmv,emv0,emv1,ratio,mjd
c
c      + + + LOCAL DEFINITIONS + + +
c     dim     - Days in each Month.
c     idim    - Number of days in the current month.
c     ipmt    - Pseudo-Midpoint Time of the current month.
c     ipmv    - Pseudo-Midpoint Value of the current month.
c     emv0    - End of Month Value at Start of current month.
c     emv1    - End of Month Value at End of current month.
c     mjd     - (middle of) day of the month input (ie, noon).
c
c
c     + + + DATA INITIALIZATIONS + + +
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c
c      + + + END SPECIFICATIONS + + +
c
c      The intercepts at the end of the months are based on Midnight.
c      However, the appropriate time to represent a day is Noon (the
c      middle of the day, or the expected value).  Therefore it is
c      necessary to adjust from Midnight on day-X to Noon on day-X; ie,
c      to subtract half a day.
c
c      There are two intervals related to each month, before the pseudo-
c      midpoint, and after it.  The appropriate one to use depends upon
c      the current day of the month (JD).  Determine which to use, and
c      interpolate a daily value for the current parameter.
c
c-----(not February in a LY)
      if(mo.ne.2 .or. ntd.ne.366) then
        idim = dim(mo)
        ipmt = pmt(mo,indpar)
        ipmv = pmv(mo,indpar)
        if(mo.gt.1) then
          emv0 = emv(mo-1,indpar)
        else
          emv0 = emv(12,indpar)
        endif
        emv1 = emv(mo,indpar)
c-----(it is February in a LY)
      else
        idim = 29
        ipmt = pmt(13,indpar)
        ipmv = pmv(13,indpar)
        emv0 = emv(13,indpar)
        emv1 = emv(14,indpar)
      endif
c
c ---- Compare current date (MJD) to the pseudo-midpoint of month (IPMT).
      mjd = float(jd) - 0.5
c-----(last half of month)
      if(mjd .gt. ipmt) then
c-------(not a max or min month)
	if(xes(mo,indpar) .eq. -9999.0) then
          ratio = (idim - mjd) / (idim - ipmt)
          ryf2 = ratio*ipmv + (1-ratio)*emv1
c-------(it is a max or min month)
	else
c---------(last quarter of the month)
          if(mjd .gt. 0.75*idim) then
            ratio = (idim - mjd) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*emv1
c---------(third quarter)
          else
            ratio = (mjd - 0.5*idim) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*ipmv
          endif
        endif
c ---- Compare to the previous month.
      else
	if(xes(mo,indpar) .eq. -9999.0) then
          ratio = mjd / ipmt
          ryf2 = ratio*ipmv + (1-ratio)*emv0
	else
c---------(first quarter of the month)
          if(mjd .lt. 0.25*idim) then
            ratio = mjd / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*emv0
c---------(second quarter)
          else
            ratio = (0.5*idim - mjd) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*ipmv
          endif
        endif
      endif
c
      return
      end
