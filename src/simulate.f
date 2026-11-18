c
c
c
      integer function jdt 
     i         (nc,i,m,nt)
c
c     + + + PURPOSE + + +
c     Computes the (Julian) day of the year, given the month and the day
c     of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nc(13),i,m,nt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     i           - Day of the Storm ("jd").
c     m           - The current month (1=Jan, 2=Feb...)("mo").
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
c     + + + END SPECIFICATIONS + + +
c
      if (m.gt.2) then
        jdt=nc(m)+nt+i
      else
        jdt=nc(m)+i
      endif
c
      return
      end
c
c
c
      subroutine jlt 
     i           (ntd,
     m            jday,
     o            mo,nday)
c
c     + + + PURPOSE + + +
c     Given the day of the year (Julian date) determine the month
c     and day of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ntd,jday,mo,nday
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ntd         - number of days in year (365; 366 if leap year and
c                    iopt=3, 5, 6)
c     jday        - Julian date; ie, day of the year.
c     mo          - The current month (1=Jan, 2=Feb...).
c     nday        - The day of the current month.
c
c     + + + LOCAL VARIABLES + + +
      integer nn(12)
c
c     + + + LOCAL DEFINITIONS + + +
c     nn          - Number of days in each month (non-leap year).
c
c     + + + DATA INITIALIZATIONS + + +
      data nn/31,28,31,30,31,30,31,31,30,31,30,31/
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Adjust for Leap Year.
      if(ntd.eq.366) then
        nn(2) = 29
      else
        nn(2) = 28
      endif
      kday = jday
c
c ---- Find the month.
      j = 0
 10   continue
        j = j + 1
        jday = jday -nn(j)
        ndflag = 0
        if (jday.le.0) then
          nday = jday +nn(j)
          ndflag = 50
        endif
      if(ndflag.eq.0 .and. j.lt.12) goto 10
c
      jday = kday
      mo = j
c
      return
      end
c
c
c
      subroutine day_gen
     i           (nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
     i            itype,ntd1,
     m            ntd,moveto)
c     + + + PURPOSE + + +
c     To generate the Daily Outputs for CLIGEN.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nbt,jd,iyear,itype,ntd1,ntd,moveto
      real tymax(4),timpkd(0:12),usdur,damt,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nbt         - Julian day of the year.
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     ntd         - Days in this year; ie, 365 or 366.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: wv
c      modify: th,tdp
c
      include 'cbk3.inc'
c      modify: ida
c
      include 'cbk4.inc'
c      read: mo,iopt,dtp
c
      include 'cbk7.inc'
c      read: k10,ra
c      write: nsim,msim
c      modify: tmxg,tmng
c
      include 'cbk5.inc'
c      modify: r
c
      include 'cbk9.inc'
c      read: r1 
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn
c      modify: radg,dur
c
CC    include 'ctap2.inc'
c        read: tap1, tap2, tap3, tap4, tap5, tap6
c
      include 'cinterp.inc'
c
c     + + + LOCAL VARIABLES + + +
c     tpr         - ratio of (time to rainfall peak)/(rainfall duration)
c     xmav        - ratio of (max rainfall intensity)/(avg rainfall intensity)
c     r5p         - apparently: max rainfall intensity; peak rainfall rate 
c                    (mm/h).  (Yu's r_p): instantaneous peak intensity (mm/h)
c
c
c     + + + SUBROUTINES CALLED + + +
c     jlt
c     clgen
c     windg
c     alph
c     timepk
c
c     + + + INPUT FORMATS + + +
 1000 format(15x,3i5)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(2i3,1x,i5,1x,f5.1,1x,f5.2,1x,f4.2,1x,f6.2,2(1x,f5.1),
     1       1x,f4.0,1x,f4.1,2x,f4.0,1x,f5.1)
c2001 format(i2,1x,6g17.9)
 2001 format(i2,1x,6f17.13)
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Start of the Daily Generation Loop  with nbt and ntd
      if(iopt.eq.4.or.iopt.eq.7) ntd=ntd1
      ida=nbt
 180  continue
        if(iopt.eq.6) then
          msim=0
          nsim=0
          moveto = 225
          read(9,1000,end=199)irida,itmxg,itmng
          moveto = 0
 199      continue
          if(moveto .eq. 0) then
            if(irida.eq.9999) nsim=1
            if(itmxg.eq.9999) msim=1
            if(itmng.eq.9999) msim=1
            r(ida)=irida*.01
            tmxg=itmxg
            tmng=itmng
          endif
        endif
c ****** L1 IF ****
        if(moveto .eq. 0) then
          idr=ida
          call jlt(ntd,idr,mo,jd)
c -------- Interpolation Code inserted here.
          if(interp .eq. 1) then
c-----------(compute lf, rf, & o_mo for current day)
            call lintrp(mo,jd,ntd)
          endif
          call clgen(ntd,iyear)
          call windg
c
c ----------- Output Standard Normal deviates for testing CLIGEN.
c           write(73,*) mo, tap1, tap2, tap3, tap4, tap5, tap6
C           write(73,2001) mo, tap1, tap2, tap3, tap4, tap5, tap6
c ----------- Output date and generated temps, rad & RF for testing CLIGEN.
CC          write(72,2223) mo,jd,iyear,tmxg,tmng,ra,r(ida)
 2223 format(2i3,1x,i4,2(1x,f5.1),1x,f4.0,1x,f5.2)
c
          th=th*clt
          prcip(mo,jd)=r(ida)
c -------- generated max & min daily temp (F)
          tgmx(mo,jd)=tmxg
          tgmn(mo,jd)=tmng
c -------- convert generated max & min daily temp from F to C.
          tmxg=(tmxg-32.0)*(5.0/9.0)
          tmng=(tmng-32.0)*(5.0/9.0)
          tdp = (tdp-32.0)*(5.0/9.0)
          radg(mo,jd)=ra
          if(r(ida).le.0.0) then
            r(ida)=0.0
            dur(mo,jd)=0.0
          else
c           call alph
            call alphb
c ---------- Equation 2.1.6
c           dur(mo,jd)=9.210/(-2.0*alog(1.0-r1))
c ---- This is the new cofficient 3.99 developed for the validation sites.
c      B.YU -- 6/99.
            dur(mo,jd) = 3.99/(-2.0*alog(1.0-r1)) 
C     write(*,*) "R1:",r1," Dur(mo,jd):",dur(mo,jd)
c 
            if(dur(mo,jd).gt.24.0) dur(mo,jd)=24.0
          endif
c
c        Set duration if a single storm is selected
c
c -- XXX -- Where is "RDUR" set? -- CRM -- 9/22/99
c -- Added initialization of RDUR to Zero:
c         rdur = 0.0
c         if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=rdur
          if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=0.0
c ******** L2 IF ****
          if(iopt.ge.4) then
            if(r(ida).gt.0.) then
c             call alph
              call alphb
              xr=r(ida)*25.4
              tpr=timepk(timpkd,k10)
C     tap4=z
              if(tpr.gt.0.99) tpr=0.99
c ------------ Equation 2.1.7
              r5p=-2.0*xr*alog(1.0-r1)
              if(r5p.gt.tymax(itype)) r5p = tymax(itype)
              xmav=r5p/(xr/dur(mo,jd))
              if((tmxg+tmng)/2.0.le.0.0) xmav = 1.01
              if(xmav.lt.1.01) xmav =1.01
            else
      tap4=0.0
              xr=r(ida)*25.4
              xmav=0.0
              tpr=0.0
            endif
c -------- Change for new option 4 and 7
            if(iopt.eq.4) then
              dur(mo,jd)=usdur
              xr=damt*25.4
              tpr=ustpr
              xmav=(uxmav*25.4)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
            else if(iopt.eq.7) then
              dur(mo,jd)=24.
              xr = damt*25.4
              xmav=tymax(itype)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
              tpr=dtp(itype)
            endif
c
c            Write WEPP Continuous Storm File
c            Writes WEPP Single Storm when nbt=ndt
            write(7,2000) jd,mo,iyear,xr,dur(mo,jd),tpr,
     1                 xmav,tmxg,tmng,radg(mo,jd),wv,th,tdp
c ******** L2 ENDIF ****
          endif
c ****** L1 ENDIF ****
        endif
c180  continue
      ida=ida+1
      if(ida .le. ntd) goto 180
c
c      End of Daily Loop
c
c
      moveto = 0
c     
      return
      end
c
c
c
      subroutine wxr_gen
     i           (version,igcode,stidd,ylt,yll,years,elev,
     i            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
CC   m            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     m            iyear,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     o            sumpp,sumptx,sumptm,sumprd,sumpdr)
c
c ----- Split out from the CLIGEN main module 10/8/99 by C. R. Meyer.
c
c     + + + PURPOSE + + +
c     The "guts" of the weather generating code.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real version,ylt,yll,xm,clt,damt,ustpr,uxmav
      real smy(12),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
C     integer igcode,years,elev,iyear,numyr,ntd1,jd,itype,moveto
      integer igcode,years,elev,iyear,ntd1,jd,itype,moveto
      character*41 stidd
c
c     + + + ARGUMENT DEFINITIONS + + +
c     version     - CLIGEN version (ie, 5.103)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     stidd       - 41-character alphanumeric station name.
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     iyear       - Beginning Simulation Year.
c     numyr       - Number of years to simulate.
c     xm          - number of days in the month of interest
c     smy         - Observed Monthly Average Precipitation (mm)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,mo,iopt
c      modify: nt
c
      include 'cbk7.inc'
c      read: rst,prw,obmx,obmn,obsl
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn,radg,dur
c
      include 'command6.inc'
c       read: numyr, irand
c
      include 'cinterp.inc'
c       read: interp
c
c     + + + LOCAL VARIABLES + + +
c     isim        - simulation mode
c                     1 -- continuous storm (iopt = 5, 6)
c                     2 -- single storm     (iopt = 4, 7)
c     itemp       - breakpoint data flag
c                     0 -- no breakpoint data used
c                     1 -- breakpoint data used
c     xr          - daily precipitation amount (mm of water)
c                    simulated rainfall amount (mm)  [Yu's P]
c
c     + + + SUBROUTINES CALLED + + +
c     day_gen
c     opt_calc
c
c     + + + FUNCTION DECLARATIONS + + +
      integer jdt
c
c     + + + OUTPUT FORMATS + + +
 500  format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 520  format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 555  format(1x,'Observed monthly ave precipitation (mm)',/,
     1       12(1x,f5.1))
 642  format(f7.5)
c644  format('  Station: ',a41,6x,' CLIGEN VER. 5.22564 -r:',i5,' -I:'
 644  format('  Station: ',a41,6x,' CLIGEN VER.',f8.5,' -r:',i5,' -I:'
     1       ,i2,/ ' Latitude Longitude Elevation (m) Obs. Years ',
     1       '  Beginning year  Years simulated ', 'Command Line:'/
     1       2f9.2,i12,2i12,i16,
     1       '          ',a)
 648  format(' da mo year  prcp  dur   tp     ip  tmax',
     1       '  tmin  rad  w-vl w-dir  tdew',
     2         /,13x,'(mm)  (h)',15x,'(C)   (C)',
     3       ' (l/d) (m/s)(Deg)   (C)')
 778  format(3i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
c     Get Everything Ready to Start Generation by Options Selected
c
 140    continue
        do 150 i=1,12
          sumpp(i)=0.0
          sumptx(i)=0.0
          sumptm(i)=0.0
          sumprd(i)=0.0
          sumpdr(i)=0.0
 150    continue
        sumpp(13)=0.0
c
c     See What Option was Selected and Set Paths
c
        nbt=1
        if(iopt .ge. 4)then
          isim=1
          if(iopt.eq.4.or.iopt.eq.7) isim =2
          itemp=0
          write(7,642)version
          write(7,778)isim,itemp,igcode
          if(iopt.ge.4) then
C           write(7,644)stidd,ylt,yll,elev,years,iyear,numyr
c           write(7,644)stidd,irand,interp,ylt,yll,elev,years,iyear,
c    1                   numyr,arg_v(:av_len)
            write(7,644)stidd,version,irand,interp,ylt,yll,elev,years,
     1                   iyear,numyr,arg_v(:av_len)
c           write(7,646) iyear,numyr
c
c  **************************************************************
c  *   P(A,B) = P(A|B) * P(B), so:                              *
c  *                                                            *
c  *   P(W) = P(W,D)+P(W,W) = P(W|D) / [1 - P(W|W) + P(W|D)]    *
c  *                                                            *
c  *   C. R. Meyer -- 7/30/2000                                 *
c  **************************************************************
c
c  CALCULATE MONTHLY RAINFALL AMOUNTS
            do 111 kkk = 1,12
              xm = nc(kkk+1)-nc(kkk)
c ---------- calculate number of days of rainfall in month
              smy(kkk) = xm*prw(kkk,2)/(1.-prw(kkk,1) + prw(kkk,2))
c ---------- monthly rainfall in mm
              smy(kkk) = smy(kkk) * rst(kkk,1) * 25.4
c ---------- Convert Observed Temps to degrees-C
              tmpcmx(kkk)=(obmx(kkk)-32.0)*(5.0/9.0)
              tmpcmn(kkk)=(obmn(kkk)-32.0)*(5.0/9.0)
 111        continue
            write(7,500)tmpcmx,tmpcmn
            write(7,520)obsl
            write(7,555)smy
            write(7,648)
          endif
        endif
  
        if(iopt.eq.4.or.iopt.eq.7) then
          nt=0
          if((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1       .and.(iyear-iyear/100*100.eq.0)))  nt=1
          ntd1 = jdt(nc,jd,mo,nt)
          nbt = ntd1
          numyr=1
        endif
c
c      Generate Data by Number of Years and Option - MAIN LOOP
c
        ii = 1
 160    continue
          moveto = 0
          ntd=365
          if((iopt.le.3.or.iopt.eq.5.or.iopt.eq.6) .and.
     1       ((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1        .and..not.(iyear-iyear/100*100.eq.0)))) ntd=366
          do 170 i=1,12
            do 169 jk=1,31
              prcip(i,jk)=0.0
              tgmx(i,jk)=0.0
              tgmn(i,jk)=0.0
              radg(i,jk)=0.0
              dur(i,jk)=0.0
 169        continue
 170      continue
c
          call day_gen(nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,
     1                 uxmav,itype,ntd1,ntd,moveto)
c
          if(moveto .ne. 225) then
CC          call opt_calc(iyear,stidd,numyr,nstat,ii,sumpp,sumptx,
            call opt_calc(iyear,stidd,nstat,ii,sumpp,sumptx,
     1                    sumptm,sumprd,sumpdr,moveto)
c
            if(moveto.eq.0) then
              iyear=iyear+1
              ii = ii + 1
            endif
          endif
        if((moveto.eq.0 .and. ii.le.numyr).or.(moveto.eq.160)) goto 160
c
      return
      end
