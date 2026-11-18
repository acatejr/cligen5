c
c
c
      subroutine clgen(ntd,iyear)
c
c     + + + PURPOSE + + +
c     Simulates daily solar radiation, simulates daily precipitation
c     and/or maximum and minimum air temperature at the users option,
c     and calls functions RANDN and DSTN1.
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     ntd    - days in this year (365 or 366)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: rh
c      modify: tdp
c     rh          - Avg Monthly Dew Point Temperature.  Used to calculate TDP.
c     tdp         - Generated dewpoint temperature (C).
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo, nt
c     mo          - The current month (1=Jan, 2=Feb...).
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
      include 'cbk7.inc'
c      read: prw,obmx,obmn,obsl,k1,k2,k3,k4,k5,k9,yls,ylc,pit,nsim,msim,
c            stdtx,stdtm
c      modify: rst,v1,v3,v5,v7,v11,ra,tmxg,tmng,rmx,l
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c     obmx        - Mean Observed Maximum Temperature for the month.
c     obmn        - Mean Observed Minimum Temperature for the month.
c     k1 ... k9   - Seeds for random number generation.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)
c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)
c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - Set to one under option-6 if precip data is missing.
c     msim        - Set to one under option-6 if temperature data is missing.
c     stdtx       - Standard deviation of daily max. temp. for the month.
c     stdtm       - Standard deviation of daily min. temp. for the month.
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
c     v1 ... v11  - Random numbers used to generate various daily values.
c     ra          - Generated radiation?  RADG receives RA's value for output.
c     tmxg        - Generated daily max temp.
c     tmng        - Generated daily min temp.
c     rmx         - Maximum possible solar radiation.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
      include 'cbk5.inc'
c      modify: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cinterp.inc'
c      read: lf, rf, o_mo
c     lf     - weighting factor for the midpoint value on this month's end
c              of the time interval.
c     rf     - weighting factor for the midpoint value on the "other" end.
c     o_mo   - month (on the "other" end) whose average value should be used.
c
CC    include 'ctap2.inc'
c       write: tap1, tap2, tap3, tap4, tap5, tap6
c
      include 'crandom3.inc'
c        read: vvx,v2x,v4x,v6x,v8x,v12x
c      modify: mox,dax
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
c
c     + + + END SPECIFICATIONS + + +
c
cC    integer tdprob
cC    data tdprob/0/
cC    save tdprob
c
      xi=ida
      sd=0.4102*sin((xi-80.25)/pit)
      ch=-yls*tan(sd)/ylc
c
      if (ch.ge.1.0) then
        h=0.0
      else if (ch.le.-1.0) then
        h=3.1416
      else
        h=acos(ch)
      endif
c
      ys=yls*sin(sd)
C -- XXX -- Note that YC is also used to store a Y/N user response! 
C           CRM -- 10/21/99
      yc=ylc*cos(sd)
c ---- max possible solar radiation for this day of the year.
      rmx=711.0*(h*ys+yc*sin(h))
c
c ---- Generate a month's worth of Consecutive Random Numbers for
c       each parameter.  (All at once -- not interleaved! -- CRM)
      if(mo .ne. mox) then
        mox = mo
        dax = 1
        call ranset(ntd,iyear)
      else
        dax = dax + 1
      endif
c 
      if (nsim.ne.0) then
        vv=vvx(dax)
CC      tap1=vv
        if ((prw(mo,l).le.0.0).or.(vv.gt.prw(mo,l))) then
          r(ida)=0.0
          l=2
CC        tap3=0.0
	else
c -------- A Mutated variant of Equation 2.1.5
c -------- Generated Daily Precip
c          According to Larry M. Younkin, the amount of precip is
c          Assumed to follow a Pearson type III
c          distribution: 
c 
c                     2s  / /  g  /      g  \     \3     \
c          R = Rbar + --- | | --- | x - --- | + 1 |  - 1 |   
c                      g  \ \  6  \      6  /     /      / 
c 
          v8=v8x(dax)
CC    tap3=v8
C         if(rst(mo,3).eq.0.0) rst(mo,3)=0.01
C         r6=rst(mo,3)/6.0
C -- XXX -- Range Check for Skewness Coefficient -- CRM -- 3/6/03
          if(rst(mo,3) .gt. 4.5) rst(mo,3) = 4.5
          if(rst(mo,3) .lt. -4.5) rst(mo,3) = -4.5
c ---------(interpolate precip skewness)
          if(interp.eq.0) then
            tmpvr3 = rst(mo,3)
          else if(interp .eq. 1) then
            tmpvr3 = rst(mo,3)*lf + rst(o_mo,3)*rf
          else if(interp .eq. 2) then
            tmpvr3 = fouri2(3)
          else if(interp .eq. 3) then
            tmpvr3 = ryf2(mo,dax,ntd,3)
          endif
          if(tmpvr3.eq.0.0) tmpvr3=0.01
          r6=tmpvr3/6.0
c
c -- XXX -- Bandaid: -- CRM 4/26/2000
          if(v7 .eq. 0.0) v7 = randn(k5)
          xlv=(dstn1(v7,v8)-r6)*r6+1.0
          xlv=(xlv**3-1.0)*2.0/tmpvr3
C         tap3=xlv
          v7=v8
c ---------(interpolate precip std. dev. & mean)
          if(interp.eq.0) then
            tmpvr1 = rst(mo,1)
            tmpvr2 = rst(mo,2)
          else if(interp .eq. 1) then
            tmpvr1 = rst(mo,1)*lf + rst(o_mo,1)*rf
            tmpvr2 = rst(mo,2)*lf + rst(o_mo,2)*rf
          else if(interp .eq. 2) then
            tmpvr1 = fouri2(1)
            tmpvr2 = fouri2(2)
          else if(interp .eq. 3) then
            tmpvr1 = ryf2(mo,dax,ntd,1)
            tmpvr2 = ryf2(mo,dax,ntd,2)
          endif
c         r(ida)=xlv*rst(mo,2)+rst(mo,1)
          r(ida)=xlv*tmpvr2 + tmpvr1
          if (r(ida).lt.0.01) r(ida)=0.01
          l=1
        endif
      endif
c
c------Option-6, and Observed Temperature Data Value is present.
      if(msim.eq.0) then
        xx=1.
        v12=v12x(dax)
        tdp=xx*dstn1(v11,v12)
        v11=v12
C ------(interpolate max & min temp means & SD's)
        if(interp.eq.0) then
          tmpvr6 = obmx(mo)
          tmpvr8 = stdtx(mo)
          tmpvr7 = obmn(mo)
          tmpvr9 = stdtm(mo)
        else if(interp .eq. 1) then
          tmpvr6 = obmx(mo)*lf + obmx(o_mo)*rf
          tmpvr8 = stdtx(mo)*lf + stdtx(o_mo)*rf
          tmpvr7 = obmn(mo)*lf + obmn(o_mo)*rf
          tmpvr9 = stdtm(mo)*lf + stdtm(o_mo)*rf
        else if(interp .eq. 2) then
          tmpvr6 = fouri2(6)
          tmpvr8 = fouri2(8)
          tmpvr7 = fouri2(7)
          tmpvr9 = fouri2(9)
        else if(interp .eq. 3) then
          tmpvr6 = ryf2(mo,dax,ntd,6)
          tmpvr8 = ryf2(mo,dax,ntd,8)
          tmpvr7 = ryf2(mo,dax,ntd,7)
          tmpvr9 = ryf2(mo,dax,ntd,9)
        endif
c
c  following by jrf 1/14/2008, dew point was not being
c  generated for a type 6 run. 
c  
c ---------(interpolate Dew Point Temp)
          if(interp.eq.0) then
            tmpv13 = rh(mo)
          else if(interp .eq. 1) then
            tmpv13 = rh(mo)*lf + rh(o_mo)*rf
          else if(interp .eq. 2) then
            tmpv13 = fouri2(13)
          else if(interp .eq. 3) then
            tmpv13 = ryf2(mo,dax,ntd,13)
          endif
          
          if(tmpvr8 .ge. tmpvr9) then
c          SD_Delta:
          twiddle = sqrt(tmpvr8**2 - tmpvr9**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr9**2))
          twiddld = tmpv13 - tmpvr7
          tdp = tmng + twiddld + tdp*twiddle
c
cC ------Generate Tdew based on Tmax-Delta:
        else
c          SD_Delta:
          twiddle = sqrt(tmpvr9**2 - tmpvr8**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr8**2))
          twiddld = tmpvr6 - tmpv13
          tdp = tmxg - twiddld + tdp*twiddle
        endif
          
c -- end insert by jrf 1/14/2008
          
      else
        xx=1.
        v2=v2x(dax)
C     tap1=v2
        tmxg=xx*dstn1(v1,v2)
C       tap1=tmxg
c ------ Shift 2nd constant to 1st position for "reuse".
        v1=v2
        v4=v4x(dax)
C     tap2=v4
        tmng=xx*dstn1(v3,v4)
C       tap2=tmng
        v12=v12x(dax)
C     tap4=v12
        tdp =xx*dstn1(v11,v12)
C       tap4=tdp
c ------ Shift 2nd constant to 1st position for "reuse".
        v3=v4
        v11=v12
c
c -------Equations 2.1.10 & 2.1.11
C ------(interpolate max & min temp means & SD's)
        if(interp.eq.0) then
          tmpvr6 = obmx(mo)
          tmpvr8 = stdtx(mo)
          tmpvr7 = obmn(mo)
          tmpvr9 = stdtm(mo)
        else if(interp .eq. 1) then
          tmpvr6 = obmx(mo)*lf + obmx(o_mo)*rf
          tmpvr8 = stdtx(mo)*lf + stdtx(o_mo)*rf
          tmpvr7 = obmn(mo)*lf + obmn(o_mo)*rf
          tmpvr9 = stdtm(mo)*lf + stdtm(o_mo)*rf
        else if(interp .eq. 2) then
          tmpvr6 = fouri2(6)
          tmpvr8 = fouri2(8)
          tmpvr7 = fouri2(7)
          tmpvr9 = fouri2(9)
        else if(interp .eq. 3) then
          tmpvr6 = ryf2(mo,dax,ntd,6)
          tmpvr8 = ryf2(mo,dax,ntd,8)
          tmpvr7 = ryf2(mo,dax,ntd,7)
          tmpvr9 = ryf2(mo,dax,ntd,9)
        endif
c
c ---------(interpolate Dep Point Temp)
          if(interp.eq.0) then
            tmpv13 = rh(mo)
          else if(interp .eq. 1) then
            tmpv13 = rh(mo)*lf + rh(o_mo)*rf
          else if(interp .eq. 2) then
            tmpv13 = fouri2(13)
          else if(interp .eq. 3) then
            tmpv13 = ryf2(mo,dax,ntd,13)
          endif
c
c ------ Generated Max & Min Daily Temps.
C       tmxg=obmx(mo)+tmxg*stdtx(mo)
C       tmng=obmn(mo)+tmng*stdtm(mo)
cC      tmng=tmpvr7 + tmng*tmpvr9
cC      tmxg=tmpvr6 + tmxg*tmpvr8
c
cC ------Tmin, Tmax, & Tdew are generated for each day so they are related.
cC       The other two parameters are based on whichever of Tmin & Tmax has
cC       the smaller SD for the day (after any interpolations).
cC       CRM -- 1/6/04.
c
c - XXX -- Here:
c -------Generate Tmax & Tdew based on Tmin+Delta (Today, SD_Tmin <= SD_Tmax):
        if(tmpvr8 .ge. tmpvr9) then
c          Tmin:
          tmng=tmpvr7 + tmng*tmpvr9
c          SD_Delta:
          twiddle = sqrt(tmpvr8**2 - tmpvr9**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tmax:
          tmxg=tmng + twiddld + tmxg*twiddle
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr9**2))
          twiddld = tmpv13 - tmpvr7
          tdp = tmng + twiddld + tdp*twiddle
c
cC ------Generate Tmin & Tdew based on Tmax-Delta:
        else
c          Tmax:
          tmxg=tmpvr6 + tmxg*tmpvr8
c          SD_Delta:
          twiddle = sqrt(tmpvr9**2 - tmpvr8**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tmin:
          tmng=tmxg - twiddld - tmng*twiddle
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr8**2))
          twiddld = tmpvr6 - tmpv13
          tdp = tmxg - twiddld + tdp*twiddle
        endif
c --- RANGE CHECK on MIN TEMP:
cC      if (tmng.gt.tmxg) write(*,*) 'Tmin adjusted below Tmax.'
        if (tmng.gt.tmxg) tmng=tmxg-0.2*abs(tmxg)
      endif
c
c       TDP now calculated using standard dev. instead of CV. 3/95
c ---- Generated Daily Dew Point Temperature.
c -----A mutated version of Equation 2.1.14:
c -- XXX -- CRM -- 10/21/99 -- I think there is a mis-placed paren here.
c     tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo)/2.))
cc       I believe it should be as follows:
cc    tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo))/2.)
cC    tdp = tmpv13+tdp*(tmpvr8+tmpvr9)/2.0
c
c ----Range check on Tdew.  It must be less than avg. daily temp.
Cc    if (tdp.gt.((tmxg+tmng)/2.)) write(*,*)'Tdew rangecheck executed.'
cC    if (tdp.gt.((tmxg+tmng)/2.)) then
cC        tdprob = tdprob + 1
cC        write(*,*)'Tdew Problem #', tdprob
cC    endif
c     if (tdp.gt.((tmxg+tmng)/2.)) tdp=((tmxg+tmng)/2.)*0.99
      if (tdp .gt. .99*(tmxg+tmng)/2.) tdp=((tmxg+tmng)/2.)*0.99
c     Change to limit - dew point in cold months ADNe
      if(tdp.lt.-10.) write(*,*)'Tdew -10 rangecheck executed.'
      if(tdp.lt.-10.) tdp=1.1*tmng
c
      v6=v6x(dax)
CC    tap5=v6
c ---- Compute Daily Radiation.
      ra=xx*dstn1(v5,v6)
C     tap5=ra
c ---------(interpolate radiation)
c   Changed 3/3/2010 - to use the standard deviation from the par file
c   when coming up with daily solar radiation values. Similar to how 
c   tmax and tmin are done. jrf
      if(interp.eq.0) then
        tmpv10 = obsl(mo)
        tmpv11 = stdsl(mo)
      else if(interp .eq. 1) then
        tmpv10 = obsl(mo)*lf + obsl(o_mo)*rf
        tmpv11 = stdsl(mo)*lf + stdsl(o_mo)*rf
      else if(interp .eq. 2) then
        tmpv10 = fouri2(10)
        tmpv11 = fouri2(11)
      else if(interp .eq. 3) then
        tmpv10 = ryf2(mo,dax,ntd,10)
        tmpv11 = ryf2(mo,dax,ntd,11)
      endif
c --- include standard deviation in value          
      ra = tmpv10 + ra*tmpv11
c ---- end changes for interpolation      
c -----A mutation of Equations 2.1.12 & 2.1.13: (Mis-placed parens,
c      this time in WEPP User Doc ?)
c     rx=rmx-obsl(mo)
c     if (obsl(mo).gt.rx) rx=obsl(mo)
c     ra=obsl(mo)+ra*rx/4.0
c      rx=rmx-tmpv10
c      if (tmpv10.gt.rx) rx=tmpv10
c      ra=tmpv10+ra*rx/4.0
c      if(ra.ge.rmx) ra=0.90*rmx
c      if (ra.le.0.0) ra=0.05*rmx      
c  check that solar radiation is within bounds
c  updated 1/31/2013 to match wepp documentation
      if(ra.gt.rmx) ra=rmx
      if (ra.lt.(rmx*0.05)) ra=0.05*rmx
c ------ Shift 2nd constant to 1st position for "reuse".
      v5=v6
c
      return
      end
c
c
      real function dstn1 
     i         (rn1,rn2)
c
c     + + + PURPOSE + + +
c     Computes the distance from the mean of a normal distribution,
c     with mean = 0 and standard deviation = 1, given two random numbers.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real rn1,rn2
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rn1         - ??? -- used for precip gamma dsn
c     rn2         -
c
c     + + + END SPECIFICATIONS + + +
c
c     Note: This algorithm is found on p. 103ff of N. T. Kottegoda's
c           "Stochastic Water Resources Technology", Wiley & Sons NY, 1980.
      dstn1=sqrt(-2.0*alog(rn1))*cos(6.283185*rn2)
c ---- range check:
      if(dstn1 .lt. -10.0) dstn1 = -10.0
      if(dstn1 .gt. 10.0) dstn1 = 10.0
c
      return
      end
c
c
c
      subroutine windg
c
c     + + + PURPOSE + + +
c     Simulates daily average wind velocity using mean, standard deviation,
c     and skew coefficient of wind speed.  Wind direction is selected from
c     a uniform distribution.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: dir,pi2
c      modify: wvl,wv,th
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          dim1: month
c                          dim2: compass direction
c     pi2         - Pi * 2; ie, a full rotation.
c     wvl(i,j,k)  - array of wind parameters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c     wv          - Wind Velocity (m/sec)
c     th          - Wind Direction (radians from North)
c
      include 'cbk3.inc'
c -- XXX -- Note that 'j' is a counter in a loop! -- CRM 10/18/99
c      modify: j
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk7.inc'
c      read: k6,k8
c      modify: v9
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c
CC    include 'ctap2.inc'
c       write: tap6
c
      include 'crandom3.inc'
c        read: fxx
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
c
c     + + + END SPECIFICATIONS + + +
c
      fx=fxx(dax)
CC    tap2=fx
      j = 0
 1    continue
        j = j + 1
        j1=j-1
        ndflag = 0
        if (dir(mo,j).gt.fx) ndflag = 2
      if(ndflag.eq.0 .and. j.lt.16) goto 1
c
c       calm condition found
c
c
c **** L1 IF ****
      if(ndflag.eq.0) then
        wv=0.0
        th=0.0
CC      tap6=0.0
c **** L1 ELSE ****
      else 
c
c       Wind direction calculations
c
        if (j.eq.1) then
          g=fx/dir(mo,j)
        else
          g=(fx-dir(mo,j1))/(dir(mo,j)-dir(mo,j1))
        endif
        xj1=j1
        th=pi2*(g+xj1-.5)/16.
        if (th.lt.0.) th=pi2+th
c
c         Wind speed calculations
c
        v10=v10x(dax)
CC      tap6=v10
        if(wvl(j,4,mo).eq.0.0) wvl(j,4,mo)=0.01
        r6=wvl(j,4,mo)/6.0
        xlv=(dstn1(v9,v10)-r6)*r6+1.0
        xlv=(xlv**3-1.0)*2.0/wvl(j,4,mo)
        v9=v10
C	tap6=xlv
        wv=xlv*wvl(j,3,mo)+wvl(j,2,mo)
        if (wv.lt.0.) wv=.1
c **** L1 ENDIF ****
      endif
c
      return
      end
