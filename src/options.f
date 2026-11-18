c
c
c
      subroutine sing_stm
     i           (ioyr,
     m            moveto,
     o            jd,iyear,damt,usdur,ustpr,uxmav)
c     + + + PURPOSE + + +
c     Generate Single Storm Data.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
CC    integer ioyr,moveto,jd,iyear,index,numyr
      integer ioyr,moveto,jd,iyear
      character*1 yc
      real damt,usdur,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ioyr        - Years of Record for Observed Data File (?)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     usdur       - Storm Duration in Hours for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     numyr       - Number of years to simulate.
c     index       - 4-digit numeric station index.
c     outfil      - File which will contain generated climate data.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c       read: iopt
c      write: mo
c
      include 'command6.inc'
c     modify: numyr
c
c     + + + LOCAL VARIABLES + + +
c     ibyear      - year of storm (iopt 4 or 7); beginning simulation year
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Enter beginning simulation year',
     1' (positive integer value; e.g. 1 ): ')
 2010 format(/1x,'Enter number of years to simulate: ')
 2020 format(/1x,'Enter output file name (ex. Indy.cli): ',
     1       'Station No. ',i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
c **** L1 IF ****
      if (iopt.ne.1) then
c ****** K2 IF ****
        if(moveto.ne.135) then
c
c       Single storm input data section
c
          if (iopt.eq.4.or.iopt.eq.7) then
            write(*,*)
            write(*,*)' Enter Month Day and Year of Storm (mo da yr)'
            read(*,*)mo,jd,ibyear
            write(*,*)' Enter Design Storm Amount in Inches (e.g. 6.30)'
            read(*,*)damt
            if(iopt.eq.4) then
              write(*,*)' Enter Storm Duration in Hours (e.g. 6)'
              read(*,*)usdur
              write(*,*)
     1              ' Enter Time to Peak Intensity (% Duration e.g. .4)'
              read(*,*)ustpr
              write(*,*)' Enter Maximum Intensity Inches/Hour (e.g. 3.0)
     1'
              read(*,*)uxmav
            endif
          else
            if(iopt.eq.6) then
C             ibyear=ioyr
C             numyr=100
              if(ibyear .eq. -1) ibyear=ioyr
              if(numyr .eq. -1) numyr=100
            else
              if(ibyear.le.0) then
C     write(*,*) "Ibyear:", ibyear
                write(*,2000)
                read(*,*) ibyear
              endif
              if(numyr.le.0) then
                write(*,2010)
                read(*,*) numyr
              endif
            endif
          endif
c
          iyear=ibyear
c ****** K2 ENDIF ****
        endif
c
c ****** L2 IF ****
c       if (moveto.eq.135 .or. iopt.ne.2) then
        if (iopt.ne.2) then
 135      continue
          if(outfil(1:3).eq."XXX") then
            write(*,2020)index
            read(*,1000)outfil
          endif
c
c         Output Options
c          3 - CREAMS - GLEAMS
c          4 - Single Storm WEPP
c          5 - Continuous Storms WEPP
c          6 - Observed Data WEPP
c          7 - Design Storm WEPP
c
c -------- Output Numbers to Analyze Confidence Limits:
CC        open(72,file='dist_nrs',status='unknown')
C         open(73,file='rand_nrs',status='unknown')
c
          if(iopt.eq.3) then
             ndflag = 136
             open(8,file=outfil,status='new',err=136,iostat=ndfalt)
             ndflag = 0
             moveto = 0
             rewind (8)
 136         continue
          else
             ndflag = 137
             open(7,file=outfil,status='new',err=137,iostat=ndfalt)
             ndflag = 0
             moveto = 0
             rewind (7)
 137         continue
          endif
          if(ndflag.ne.0) then
c ----------- Ask about file over-write.
            if(force .eq. 0) then
              write(*,*)' '
              write(*,*)'     **** File Already Exists *****'
c             write(*,*)' '
              write(*,*)'           ',outfil
c             write(*,*)' '
c             write(*,*)'          Enter New File Name '
              write(*,*)'              Overwrite?'
              write(*,*)' '
              read(*,'(a1)')yc
c ----------- Set flag to Force file over-write.
            else
              yc = 'y'
            endif
            if(yc.eq.'y' .or. yc.eq.'Y') then
              if(ndflag .eq. 136) then
                open(8,file=outfil,status='unknown')
              else if(ndflag .eq. 137) then
                open(7,file=outfil,status='unknown')
              endif
              moveto = 0
            else
              write(outfil(1:3), '("XXX")')
              moveto = 135
            endif
          endif
          if(moveto.eq.135) goto 135
c ****** L2 ENDIF ****
        endif
c **** L1 ENDIF ****
      endif
c
      return
      end
c
c
c
      subroutine usr_opt
     m           (moveto,
     o            ioyr)
c     + + + PURPOSE + + +
c     Get Options from User.
c
c ----- Split out from the CLIGEN main module 10/6/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer moveto,ioyr
c
c     + + + ARGUMENT DEFINITIONS + + +
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ioyr        - Years of Record for Observed Data File (?)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c     odfile      - File contains observed climate data.
c     ndflag      - A flag for loop control, local to this module.
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
C1010 format(13x,i2)
 1010 format(10x,i5)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Weather Generator Options',/,1x,7('-'),1x,
     19('-'),1x,7('-'),//,1x,'1 - Single Year Simulation - Screen',/,
     11x,'2 - Multiple Year - Screen Output',/,
     11x,'3 - Multiple Year Simulation - CREAMS - GLEAMS Output File',/,
     11x,'4 - Selected Single Storm WEPP - Output File',/,
     11x,'5 - Multiple Year - WEPP Output File',/,
     11x,'6 - Read Observed P and Temp and Generate Missing Data',/,
     11x,'7 - Single Design Storm - TR 55 Storm Type WEPP Output File',/
     11x,'8 - Exit Weather Generator Program',//,
     11x,'Enter generator option (1-8): ')
c
c     + + + END SPECIFICATIONS + + +
c
c
 56   continue
        if(iopt.lt.0) then
          write(*,2000)
          read(*,*)iopt
        endif
        if (iopt.eq.6) then
          if(odfile.eq."XXX") then
            write(*,*)'Enter Observed Data Input File Name'
            read(*,1000)odfile
          else
            write(*,*) odfile
          endif
          ndflag = 57
          open(9,file=odfile,status='old',err=57)
          ndflag = 0
 57       continue
          if(ndflag.eq.57) then
            write(*,*)' Error - File not Found '
            write(*,*)' Check Directory for Observed Data File or'
            write(*,*)' Re-enter File Name'
            write(*,*)' Enter c to continue or q to quit'
            read(*,'(a1)')yc
            if(yc.ne.'q'.and.yc.ne.'Q') then
              ndflag = 56
            else
              moveto = 230
            endif
          else
            rewind(9)
            read(9,1010)ioyr
            backspace (9)
          endif
        elseif (iopt.eq.8) then
          ndflag = 0
          moveto = 230
        else
          ndflag = 0
        endif
      if(ndflag.eq.56) goto 56
c     if(moveto.ne.230) numyr=1
      return
      end
