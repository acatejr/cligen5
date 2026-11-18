c
c
c
      subroutine clmout
     i           (iview)
c
c     + + + PURPOSE + + +
c     Calculates daily and monthly values for options 1 and 2.  Writes
c     output to screen for option 1.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iview
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iview       -
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,iopt
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c
      include 'ccl1.inc'
c      read: prcip,tgmx,tgmn,radg,dur
c     prcip       - ??? Avg. Daily Precip?  Set to R.  Divided by NUMYR and
c                    added to SUMP.
c     tgmx        - ??? Avg. Max Daily Temp?  Set to TMXG.  Divided by NUMYR 
c                    and added to SUMTX.
c     tgmn        - ??? Avg. Min Daily Temp?  Set to TMNG.  Divided by NUMYR 
c                    and added to SUMTM.
c     radg        - Daily Solar Radiation (Langleys/Day)
c     dur         - Storm Duration in Hours for Single Storm.
c
      include 'csumr.inc'
c      modify: sump,sumtx,sumtm,sumrd,sumdr
c     sump        - [Avg Annual] Sum of Daily Precip?
c     sumtx       - [Avg Annual] Sum of Daily Max Temps?
c     sumtm       - [Avg Annual] Sum of Daily Min Temps?
c     sumrd       - [Avg Annual] Sum of Daily Radiation?
c     sumdr       - Sum of Storm Durations?
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Climate Data Viewing Options',/,1x,7('-'),
     11x,4('-'),1x,7('-'),1x,7('-'),/,1x,'1 - Precipitation',/,1x,
     1'2 - Maximum Air Temperature',/,1x,'3 - Minimum Air',
     1' Temperature',/,1x,'4 - Solar Radiation',/,1x,'5 - Storm',
     1' Duration',/,1x,'6 - End',//,1x,'Enter viewing option')
 2010 format(20x,'Daily Precipitation Amounts (Inches)'/)
 2020 format(1x,'     J     F     M     A     M     J     J     A',
     1'     S     O     N     D')
 2030 format(1x,i2,12f6.2)
 2040 format(/1x,'tot',f5.2,11f6.2)
 2050 format(/1x,'Annual Precipitation ',f6.2/)
 2060 format(/20x,'Daily  Maximum Air Temperature (F)'/)
 2070 format(1x,i2,12f6.0)
 2080 format(/1x,'ave',f5.0,11f6.0/)
 2090 format(/20x,'Daily Minimum Air Temperature (F)'/)
 2100 format(1x,i2,12f6.0)
 2110 format(/20x,'Daily Solar Radiation (Langleys) '/)
 2120 format(1x,i2,12f6.0)
 2130 format(/20x,'Storm Duration (Hours)'/)
 2140 format(1x,i2,12f6.2)
c
c     + + + END SPECIFICATIONS + + +
c
      do 10 i =1,12
         sump(i) = 0.0
         sumtx(i)= 0.0
         sumtm(i)= 0.0
         sumrd(i)= 0.0
         sumdr(i)= 0.0
 10   continue
      sump(13)= 0.0
      do 20 i =1,12
         an = nc(i+1)-nc(i)
         do 20 j =1,31
            sump(i)=sump(i)+prcip(i,j)
            sump(13)=sump(13)+prcip(i,j)
            sumtx(i)=sumtx(i)+tgmx(i,j)/an
            sumtm(i)=sumtm(i)+tgmn(i,j)/an
            sumrd(i)=sumrd(i)+radg(i,j)/an
            sumdr(i)=sumdr(i)+dur(i,j)
 20   continue
c
      if(iopt.ne.2 .and. iview.ne.0) then
 30     continue
          write(*,2000)
          read(*,*)i
          if(i.eq.1) then
            write(*,2010)
            write(*,2020)
            write(*,2030)(j,(prcip(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sump(i),i=1,12)
            write(*,2050)sump(13)
          elseif(i.eq.2) then
            write(*,2060)
            write(*,2020)
            write(*,2070)(j,(tgmx(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtx(i),i=1,12)
          elseif(i.eq.3) then
            write(*,2090)
            write(*,2020)
            write(*,2100)(j,(tgmn(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtm(i),i=1,12)
          elseif(i.eq.4) then
            write(*,2110)
            write(*,2020)
            write(*,2120)(j,(radg(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumrd(i),i=1,12)
          elseif(i.eq.5) then
            write(*,2130)
            write(*,2020)
            write(*,2140)(j,(dur(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sumdr(i),i=1,12)
          endif
c -- XXX -- How does one EXIT this loop? -- CRM -- 10/18/99
c           Replaced statement below:       CRM -- 11/10/99
c       goto 30
        if(i.gt.0 .and. i.lt.6) goto 30
      endif
c
 90   continue
      return
      end
c
c
c
      subroutine opt_calc
CC   i           (iyear,stidd,numyr,nstat,ii,
     i           (iyear,stidd,nstat,ii,
     m            sumpp,sumptx,sumptm,sumprd,sumpdr,
     o            moveto)
c     + + + PURPOSE + + +
c     To handle Options 1-3.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     iyear       - Beginning Simulation Year.
c     stidd       - 41-character alphanumeric station name.
c     numyr       - Number of years to simulate.
c     nstat       - 4-digit Numeric Station Code.
c     ii          -
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
      character*41 stidd
c
      character*1 yc
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: iopt
c
      include 'csumr.inc'
c      read: sump,sumtx,sumtm,sumrd,sumdr
c
      include 'cbk5.inc'
c      modify: r
c
      include 'command6.inc'
c       read: numyr
c
c     + + + SUBROUTINES CALLED + + +
c     clmout
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view generated data (y/n)? ')
 2010 format(/1x,'Do you want to simulate another year (y/n)? ')
 2020 format(/1x,'Do you want another Station (y/n)? ')
 2030 format(/15x,'Summary of Elements Generated - Year ',i4)
 2040 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2050 format(1x,'prcp',i3,12f6.2)
 2060 format(1x,'tmax',i3,12f6.2)
 2070 format(1x,'tmin',i3,12f6.2)
 2080 format(1x,'rad ',i3,12f6.1)
 2090 format(1x,'dur ',i3,12f6.2)
 2100 format(/1x,'Annual Precipitation =',f6.2,a30/)
 2110 format(i5,i5,10f5.2,8x,i2)
 2120 format(i5,i5,6f5.2,28x,i2)
c
c     + + + END SPECIFICATIONS + + +
c
c      Option 1 Stuff
      if(iopt.le.1) then
        write(*,2000)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          iview=1
          call clmout(iview)
        end if
        write(*,2010)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          moveto = 160
        else
          write(*,2020)
          read(*,'(a1)')yc
          if(yc.eq.'y'.or.yc.eq.'Y') then
            moveto = 10
          else
            moveto = 230
          endif
        endif
c
c      Option 2 Stuff
      elseif(iopt.eq.2) then
        jj=ii
        call clmout(0)
        write(*,2030) iyear
        write(*,*)' '
        write(*,2040)
        write(*,2050)jj,(sump(i),i=1,12)
        write(*,2060)jj,(sumtx(i),i=1,12)
        write(*,2070)jj,(sumtm(i),i=1,12)
        write(*,2080)jj,(sumrd(i),i=1,12)
        write(*,2090)jj,(sumdr(i),i=1,12)
        write(*,2100)sump(13),stidd
        an=numyr
        do 200 i=1,12
          sumpp(i)=sumpp(i)+sump(i)/an
          sumptx(i)=sumptx(i)+sumtx(i)/an
          sumptm(i)=sumptm(i)+sumtm(i)/an
          sumprd(i)=sumprd(i)+sumrd(i)/an
          sumpdr(i)=sumpdr(i)+sumdr(i)/an
 200    continue
        sumpp(13)=sumpp(13)+sump(13)/an
c
c      Option 3 Stuff
      elseif(iopt.eq.3) then
        nb=1
        ne=10
        do 210  j=1, 36
          write(8,2110)nstat,iyear,(r(i),i=nb,ne),j
          nb=ne + 1
          ne=nb + 9
 210    continue
        nb=361
        ne=366
        if((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1     .and..not.(iyear-iyear/100*100.eq.0))) r(366)=0.0
        j=37
        write(8,2120)nstat,iyear,(r(i),i=nb,ne),j
      endif
c
      return
      end
