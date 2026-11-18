c
c
c
c     subroutine header
      subroutine header(version)
c
c     + + + PURPOSE + + +
c     Writes header information to screen.
c
c     + + + OUTPUT FORMATS + + +
 2000 format(//,2x,68('*'),/,2x,'*',66x,'*',
     1       /,2x,'*',14x,'USDA - WATER EROSION PREDICTION',
     2       ' PROJECT',13x,'*',/,2x,'*',16x,
     3       ' WEPP CLIMATE INPUT DATA GENERATOR',16x,'*'/2x,'*',66x,
     4       '*',/,2x,'*',20x,'CONTINUOUS SIMULATION AND',21x,'*',/,
     5       2x,'*',23x,'SINGLE STORM OPTIONS',23x,'*',/,
     6       2x,'*',20x,'with Command Line Options,',20x,'*',/,
     7       2x,'*',24x,'and Corrections to',24x,'*',/,
     8       2x,'*',18x,'Rainfall Intensity Calculations',17x,'*',/,
     9       2x,'*',19x,'and Random Number Generation.',18x,'*')
 2001 format(
c    1       2x,'*',66x,'*',/,2x,'*',27x,'VERSION ',f5.3,26x,'*',/,
c    1       2x,'*',66x,'*',/,2x,'*',27x,'VERSION ',f6.4,25x,'*',/,
     1       2x,'*',66x,'*',/,2x,'*',26x,'VERSION ',f7.5,25x,'*',/,
     2       2x,'*',21x,'Revised from VERSION 4.2',21x,'*',/,
c    3       2x,'*',26x,' January 2013 ',26x,'*',/,2x,'*',66x,'*',/,
     3       2x,'*',26x,'September 2024',26x,'*',/,2x,'*',66x,'*',/,
     4       2x,'*',11x,'(Use -h or /h to list command line options.)',
     5       11x,'*',/,2x,'*',66x,'*',/,2x,68('*'),//)
c
c     + + + END SPECIFICATIONS + + +
c
      write(*,2000)
      write(*,2001) version
      return
      end
c
c
c
      subroutine sta_dat
c    i           (ylt,yll,years,elev,itype,tp6,wgt,
     i           (ylt,yll,years,elev,itype,tp6,wgt,version,
     m            moveto,
     o            stidd,timpkd,igcode)
c     + + + PURPOSE + + +
c     To determine desired station and load the parameters required to
c     generate its weather.
c
c ----- Split out from the CLIGEN main module 9/22-27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ylt,yll,wgt(3),timpkd(0:12)
c--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
c           data file it is a floating-point!!!  --- CRM -- 9/27/99
      integer years,elev,moveto
      character*41 stidd
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c     years       - Years of Record at Station.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     stidd       - 41-character alphanumeric station name.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c
c     + + + COMMON BLOCKS + + +
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer iscnt,nst,nstat,ndflag
      character*41 stid
      character*2 dr(55)
      character*1 yc
      character*80 buffer
c
c     + + + LOCAL DEFINITIONS + + +
c      Variables Passed to other Modules:
c     istate      - Numeric Climate Code of Desired State.
c     iscnt       - Number of Stations in Selected State.
c     stid        - 41-character ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c
c     nst         - Numeric State Code
c     index       - 4-digit numeric station index.
c     ndflag      - "Set" if an "End-of-File" is read.
c     dr          - Array of 2-character state (postal) abbreviations.
c     yc          - 1-character user response (y/n).
c     dr          - Array of 2-character state (postal) abbreviations.
c                    File by that name contains parameters for all the
c                    weather stations in that state for which we can
c                    generate weather.
c     version     - CLIGEN version (ie, 5.103)
c     buffer      - Stores reads from Input File for parsing.
c
c     + + + SUBROUTINES CALLED + + +
c     header
c     sta_name
c     sta_parms
c
c     + + + DATA INITIALIZATIONS + + +
      data dr/'al','az','ar','ca','co','ct','de','fl','ga','id',
     1        'il','in','ia','ks','ky','la','me','md','ma','mi',
     1        'mn','ms','mo','mt','ne','nv','nh','nj','nm','ny',
     1        'nc','nd','oh','ok','or','pa','ri','sc','sd','tn',
     1        'tx','ut','vt','va','wa','wv','wi','wy','dc','ak',
     1        'hi','pr','pi','if','  '/
c
c     + + + INPUT FORMATS + + +
 1000 format(i4)
 1010 format(a41,i2,i4,i2)
 1020 format(//////////////////////////////////////////////////
     1       ///////////////////////////////)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'No stations available - do you want to',
     1' continue (y/n)? ')
 2010 format(/1x,'Enter the station index: ')
c
c
c     + + + END SPECIFICATIONS + + +
c
      if(moveto.ne.10) then
        timpkd(0)=0.0
c ------ Version number set here for option 5 output header
C       version=5.103
c
c       call header
        call header(version)
c ------ "Pure" Interactive Mode (No command line arguments):
C       if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
        if(numarg.eq.0) then
          write(*,*)' Press Enter to Continue'
          read(*,'(a1)')yc 
        endif 
      endif
c
c ***************************************************************************
c ---- Determine Desired State, and display Available Stations.
c10     continue
c     if(istate.le.0 .and. infile.eq."XXX") then 
      if(istate.le.0 .and. (infile.eq."XXX" .or. index.ge.0)) then 
        call sta_name(iscnt,stid,nstat,moveto)
      endif
c ***************************************************************************
c -- Move this section (through #50) into STA_NAME? -- CRM 7/13/00.
 40   continue
C     if(index.le.0 .and. infile.eq."XXX") then
      if(index.le.0 .and. (infile.eq."XXX" .or. istate.ge.0)) then
        if(moveto.eq.40 .or. (iscnt.le.0 .and. moveto .ne. 50)) then
c -------- No Stations Found
          write(*,2000)
          read(*,'(a1)')yc
          if(yc.eq.'y'.or.yc.eq.'Y') then
            moveto = 10
          else
            moveto = 230
          endif
        endif
      endif
c
 50     continue
c ****** L1 IF ****
      ndflag = 0
      if(moveto.ne.10 .and. moveto.ne.230) then
        if(index.le.0 .and. infile.eq."XXX") then 
          write(*,2010)
        endif
        iname = istate
        if(iname.eq.66) iname=52
        if(iname.eq.91) iname=53
        if(iname.eq.99) iname=54
        if(index.le.0 .and. infile.eq."XXX") then 
c -------- Choose the Desired Station, from the State Selected.
          read(*,1000)index
        endif
        ndflag = 55
        if(infile.eq."XXX") then
          open(10,file=dr(iname),status='old',err=55)
        else
          open(10,file=infile,status='old',err=55)
      write(*,*) infile
        endif
        rewind (10)
        ndflag = 0
 55     continue
        if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
          if(ndflag .ne. 0) then
C           write(*,*)'  Mother Nature does not want to run state ',
C    1                   dr(iname)
C           write(*,*)'  Parameters for this state are not loaded on '
C           write(*,*)'  this computer.  Check directory for file ',
C    1                   dr(iname)
            write(*,*)'  Parameters for this state must be loaded in'
            write(*,*)'  the same directory as the Cligen program.'
            write(*,*)'  Make sure you have downloaded the file: ',
     1                   dr(iname)
            write(*,*)'  -- Enter q to quit or c to Continue'
            read(*,'(a1)')yc
            if(yc.eq.'q'.or.yc.eq.'Q') then
              moveto = 230
            else
              moveto = 10
            endif
          endif
        endif
c
c ****** L2 IF ****
        if(moveto.ne.10 .and. moveto.ne.230) then
c          Gettin Station Parameter Data from File
C       if(infile.eq."XXX") then
c ------ State & Station known.
        if(istate.gt.0 .and. index.gt.0) then
          ndflag = 61
 60       continue
c         read(10,1010,end=61)stidd,nst,nstat,igcode
          read(10,'(a)',end=61) buffer
          read(buffer,1010,err=60,end=61)stidd,nst,nstat,igcode
c -------- Match 2-digit state code & 4-digit station code.
          if((nst.ne.istate).or.(nstat.ne.index)) goto 60
c         write(*,*) "State, Station:", istate, index
          ndflag = 0
c
 61       continue
c
c ---------- Requested Station Not Found -- Try Again?
          if(ndflag.eq.61) then
            if(istate.gt.0 .and. index.gt.0) then
              write(*,*)
              write(*,*)'  Parameters for this state must be loaded in'
              write(*,*)'  the same directory as the Cligen program.'
              write(*,*)'  Make sure you have downloaded the file \"',
     1                     dr(iname), '\".'
              write(*,*)'  http://horizon.nserl.purdue.edu/Cligen/ .'
              write(*,*)
C             stop(" Requested Station Not Found.")
              stop " Requested Station Not Found."
            else
              ndflag = 40
            endif
          endif
c         if(ndflag.eq.60) goto 60
c ------ For Single Station File use first station found.
        else
          read(10,1010)stidd,nst,nstat,igcode
          ndflag = 1
        endif
c
CC    write(*,*) "NDFLAG", ndflag
CC    write(*,*)"St_id: ",stidd, "NST: ",nst
CC    write(*,*)"Station_ID: ",stidd
CC    write(*,*)" State:",istate," Station Number:",index
          if(ndflag.ne.40)
c    1    call sta_parms(stidd,nstat,tp6,wgt,ylt,yll,years,elev,itype,
c    2                   timpkd)
     1    call sta_parms(stidd,index,tp6,wgt,ylt,yll,years,elev,itype,
     2                   timpkd)
c
c ****** L2 ENDIF ****
        endif
c **** L1 ENDIF ****
      endif
c
c ---- Try Again.
      if(ndflag.eq.40) goto 40
c
c230  continue
      return
      end
c
c
      subroutine sta_name
     m           (iscnt,stid,nstat,moveto)
c
c     + + + PURPOSE + + +
c     Choose a State (istate) and display the available climate stations
c     for the State selected.
c
c ----- Split out from the STA_DAT module 9/27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iscnt,nstat,moveto
      character*41 stid
c
c     + + + ARGUMENT DEFINITIONS + + +
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     iscnt       - Number of Stations in Selected State.
c     stid        - ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + COMMON BLOCKS + + +
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer ndflag
c -- XXX -- COUNTY is supposed to be read & written, but space is never
c           allocated to _store_ it!  Added stmt below.  CRM -- 9/27/99.
      character*20 county
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c
c     + + + INPUT FORMATS + + +
 1000 format(i2)
 1010 format(a41,i2)
 1020 format(a41,i2,i4,i3,i4,a20)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(1x,'State Climate Code - Available Stations'/)
 2010 format(1x,'01 Alabama       20 Michigan       39 S. Dakota',
     1/1x,'02 Arizona       21 Minnesota      40 Tennessee',
     2/1x,'03 Arkansas      22 Mississippi    41 Texas',
     3/1x,'04 California    23 Missouri       42 Utah',
     4/1x,'05 Colorado      24 Montana        43 Vermont',
     5/1x,'06 Connecticut   25 Nebraska       44 Virginia',
     6/1x,'07 Delaware      26 Nevada         45 Washington',
     7/1x,'08 Florida       27 New Hampshire  46 West Virginia',
     8/1x,'09 Georgia       28 New Jersey     47 Wisconsin',
     9/1x,'10 Idaho         29 New Mexico     48 Wyoming')
 2020 format(1x,'11 Illinois      30 New York       49 Washington DC',
     1/1x,'12 Indiana       31 North Carolina 50 Alaska',
     2/1x,'13 Iowa          32 North Dakota   51 Hawaii',
     3/1x,'14 Kansas        33 Ohio           66 Puerto Rico',
     4/1x,'15 Kentucky      34 Oklahoma       91 Pacific Islands',
     5/1x,'16 Louisiana     35 Oregon         99 Interpolated File',
     6/1x,'17 Maine         36 Pennsylvania   ',
     7/1x,'18 Maryland      37 Rhode Island    ',
     8/1x,'19 Massachusetts 38 South Carolina  ')
 2030 format(/1x,'Enter state climate code (ex. 01 for Alabama): ')
c2040 format(/1x,'Stations Available',19x,' Station No. ',
 2040 format(/1x,'Stations Available',18x,' Station No. ',
     1       'Lat.  Long County'/,
c    11x,8('-'),1x,9('-'),19x,17('-'),2x,4('-'),1x,7('-')/)
     11x,8('-'),1x,9('-'),18x,17('-'),2x,4('-'),1x,7('-')/)
c2050 format(1x,a41,2x,i4,3x,i3,2x,i4,1x,a19)
 2050 format(1x,a41,1x,i4,3x,i3,2x,i4,1x,a19)
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Determine Desired State
c --- (skip, if state is a command line arg.) 
cc    if(istate.le.0) then 
 10     continue
        write(*,2000)
        write(*,2010)
        write(*,2020)
        write(*,2030)
        ndflag = 15 
        read(*,1000,err=15)istate
        ndflag = 0
 15     continue 
        if(ndflag .eq. 15) then 
          write(*,*)' Error Entering State Code '
          write(*,*)
          write(*,*)' Press Enter to Continue'
          read(*,'(a1)')yc 
        endif 
        if(ndflag .ne. 0) goto 10 
cc    endif 
c
c ---- Interpolated File
      moveto = 0
      if(istate.eq.99) then
        iscnt = 1
c       iyr = 0
        moveto = 50
      end if
c
      if(moveto.ne.50 .and. index.le.0) then
c
c ------ Read List of Available Stations
c -- istate -- Numeric Climate Code of Desired State
c -- kknt   -- Lines Displayed on Screen so far
c -- nstate -- Numeric State Code
c -- nst    -- Numeric State Code (same)
c
        open(11,file='stations',status='old',err=16)
        goto 17
c-------Stations File Not Found (not opened).
 16     continue
        write(*,*) 
        write(*,*) 'The \"stations\" file must be in the same directory'
        write(*,*) 'as the Cligen executable.  It is available on the'
        write(*,*) 'website http://horizon.nserl.purdue.edu/Cligen/ .'
        write(*,*) 
        stop
 17     continue
        write(*,*) 
        write(*,*) 'While this approach is fine for exploring use of'
        write(*,*) 'the Cligen program, for the best quality results'
        write(*,*) 'we recommend you use the new Forest Service data'
        write(*,*) 'instead.'
        write(*,*) 
        write(*,*) 'Because development of Cligen was funded by the'
        write(*,*) 'U. S. taxpayers, the available station datasets'
        write(*,*) 'are primarily for the continental United States.'
        write(*,*) 'Users outside the U. S. are welcome, but will'
        write(*,*) 'probably need to build their own station parameter'
        write(*,*) 'files.  \"Survey of Climatology\", 1982, by Griffith
     1s'
        write(*,*) 'and Driscoll will prove helpful in finding a U. S.'
cLEW    write(*,*) 'station with a climate similar to yours. Cligen\'s'
        write(*,*) 'station with a climate similar to yours. Cligen''s'
        write(*,*) '\"option-6\" may also prove useful.'
        write(*,*) 
        write(*,*) 'Press "ENTER" to continue.'
        read(*,'(a1)') yc
        rewind (11)
        iscnt=0
c       iyr=0
        write(*,2040)
        kknt=0
 20     continue
        ndflag =29 
        read(11,1010,end=29)stid,nstate
        if(nstate.eq.istate) then
          kknt=kknt+1
          iscnt=iscnt+1
          backspace(11)
          read(11,1020)stid,nst,nstat,lat,long,county
          if(kknt.gt.20) then
            kknt=0
            write(*,*) 'Press "ENTER" to continue.'
            read(*,'(a1)') yc
          endif
          write(*,2050)stid,nstat,lat,long,county
        endif
        ndflag = 0
 29     continue
        if(ndflag.eq.0 .and. nstate.le.istate) goto 20
      endif
c
      close (11)
      return
      end
c
c
c
      subroutine sta_parms
     i           (stidd,nstat,
     m            tp6,wgt,
     o            ylt,yll,years,elev,itype,timpkd)
c     + + + PURPOSE + + +
c     Derive parameters for the desired climate station.
c
c ----- Split out from the CLIGEN main module 9/28/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      character*41 stidd
      integer nstat,elev,years
      real ylt,yll,tp6,wgt(3),timpkd(0:12)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     stidd       - 41-character alphanumeric station name.
c     nstat       - 4-digit Numeric Station Code
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      modify: wvl,dir,rh,calm
c     wvl(i,j,k)  - array of wind paramters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          derived from wvl()
c                          dim1: month
c                          dim2: compass direction
c     rh          - Dew Point Temp.
c     calm        - % time air is calm (by month).
c                   Calm is treated separately [from WVL] as direction 0,
c                   speed 0.  Only a % time value is need for calm generation.
c
      include 'cbk7.inc'
c      modify: rst,prw,obmx,obmn,obsl,cvs,cvtx,cvtm,stdtx,stdtm,stdsl
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
c     obmx        - Maximum Temperature.
c     obmn        - Minimum Temperature.
c     obsl        - Observed mean daily solar radiation (Langleys) (by month)
c     cvs         - Coefficient of Variation of Solar Radiation (by month)
c     cvtx        - Coefficient of Variation of Maximum Temperature (by month)
c     cvtm        - Coefficient of Variation of Minimum Temperature (by month)
c     stdmx       - Standard Dev. Tmax.
c     stdmn       - Standard Dev. Tmin.
c     stdsl       - Standard Dev. Sol.
c
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip.
c
      include 'cinterp.inc'
c
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
      character*19 site(3)
      real rst1(12),rst2(12),rst3(12),prw1(12),prw2(12)
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c     site(3)     - 3 wind station names used for triangulation.  Stations
c                   from which wind data were interpolated.  (Not used in
c                   CLIGEN computations, but reported in the CLIGEN output.)
c
c     + + + INPUT FORMATS + + +
 1000 format(6x,f7.2,6x,f7.2,7x,i3,7x,i2/12x,i5,17x,f5.2)
 1010 format(8x,12f6.2)
 1020 format(8x,12f6.3)
 1030 format(a19,f6.3,2(2x,a19,f6.3))
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view data found for station',//,
     1 a41,2x,i4,' (y/n)?: ')
 2010 format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 2020 format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 2030 format(/1x,'wet-dry state probabilities'/)
 2040 format(1x,2f10.5)
 2050 format(/1x,'mean,st.dev.,and skew coef. of daily rainfall'/)
 2060 format(1x,3f10.5)
 2070 format(/1x,'standard deviation for max and min temp,',
     1          ' and solar radiation'/)
 2080 format(1x,12f6.2)
 2090 format(1x,12f6.1)
 2100 format(/1x,'coefficient of variation for max, min temp,',
     1          ' solar radiation, and max .5 hr rain'/)
 2110 format(1x,'Average Monthly Dew Point Temperature',/1x,12f6.2)
 2120 format(1x,'Wind Data Interpolated from',
     1           /1x,a19,f6.3,2(2x,a19,f6.3))
 2130 format(1x,12f6.2)
c
c     + + + EQUIVALENCES + + +
      equivalence(rst1,rst(1,1))
      equivalence(rst2,rst(1,2))
      equivalence(rst3,rst(1,3))
      equivalence(prw1,prw(1,1))
      equivalence(prw2,prw(1,2))
c
c     + + + END SPECIFICATIONS + + +
c
c     Read Precipitation, Temperature, Radiation, etc.
c
      read(10,1000)ylt,yll,years,itype,elev,tp6
      read(10,1010)(rst(i,1),i=1,12),(rst(i,2),i=1,12),(rst(i,3),
     1             i=1,12)
      read(10,1010)(prw(i,1),i=1,12),(prw(i,2),i=1,12)
      read(10,1010)(obmx(i),i=1,12)
      read(10,1010)(obmn(i),i=1,12)
      read(10,1010)(stdtx(i),i=1,12),(stdtm(i),i=1,12)
      read(10,1010)(obsl(i),i=1,12)
      read(10,1010)(stdsl(i),i=1,12)
      read(10,1010)(wi(i),i=1,12)
c
c --- Bofu Yu's code discovered & added 7/19/2000 -- CRM:

c                  wi is input as max 30-min rainfall intenisty
c                  now wi is converted into depth as it should be.
c                  B.YU, 7/7/1999
c
      do 65 i=1,12
        wi(i) = 0.5*wi(i)
65    continue
c
      read(10,1010)(rh(i),i=1,12)
      read(10,1020)(timpkd(i),i=1,12)
c
c ----- Output the Max & Min Temp, Rad, and Precip values for testing CLIGEN.
CC     open(71,file='pop_nrs',status='unknown')
CC     write(71,1010)(obmx(i),i=1,12)
CC     write(71,1010)(stdtx(i),i=1,12)
CC     write(71,1010)(obmn(i),i=1,12)
CC     write(71,1010)(stdtm(i),i=1,12)
CC     write(71,1010)(obsl(i),i=1,12)
CC     write(71,1010)(stdsl(i),i=1,12)
CC     write(71,1010)(rst(i,1),i=1,12)
CC     write(71,1010)(rst(i,2),i=1,12)
CC     close (71)
c
c     Compute Fourier Coefficients, if Needed.
      if(interp .eq. 2) then
        call fouri1(rst1,1)
        call fouri1(rst2,2)
        call fouri1(rst3,3)
        call fouri1(prw1,4)
        call fouri1(prw2,5)
        call fouri1(obmx,6)
        call fouri1(obmn,7)
        call fouri1(stdtx,8)
        call fouri1(stdtm,9)
        call fouri1(obsl,10)
        call fouri1(stdsl,11)
        call fouri1(wi,12)
        call fouri1(rh,13)
        call fouri1(timpkd,14)
      else if(interp .eq. 3) then
        call ryf1(rst1,1)
        call ryf1(rst2,2)
        call ryf1(rst3,3)
        call ryf1(prw1,4)
        call ryf1(prw2,5)
        call ryf1(obmx,6)
        call ryf1(obmn,7)
        call ryf1(stdtx,8)
        call ryf1(stdtm,9)
        call ryf1(obsl,10)
        call ryf1(stdsl,11)
        call ryf1(wi,12)
        call ryf1(rh,13)
        call ryf1(timpkd,14)
C     write(*,*) "EMV:", emv
C     write(*,*)
C     write(*,*) "PMT:", pmt
C     write(*,*)
C     write(*,*) "PMV:", pmv
C     write(*,*)
C     write(*,*) "XES:", xes
C     write(*,*)
      endif
c
c     Wind Data read in here.
c     wvl(i,j,k) - array of wind paramters where
c     i - ith direction (1 - north  - 16 nnw)
c     j - parameters (1 - 4)
c       1 - % time from direction i
c       2 - mean speed from direction i
c       3 - standard deviation of speed from direction i
c       4 - skew coeficient of speed from direction i
c     Calm is treated seperately as direction 0, speed 0
c     Only a % time values is need for calm generation.
c
      read(10,1010)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
      read(10,1010)(calm(i),i=1,12)
      read(10,1030)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
c--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
c           data file it is a floating-point!!!  --- CRM -- 9/27/99
      elev=elev*.3048
c
c
      do 70 i=1,12
c -- XXX -- Huh??? -- CRM -- 9/14/99
c       wi(i)=wi(i)
c        cvtm(i)=stdtm(i)/obmn(i)
c        cvtx(i)=stdtx(i)/obmx(i)
c     This assumes that observed temperatures are Farenheit in the CLIGEN records.
c     The monthly minimum is converted to Rankine for calculation of CV
c     see https://en.wikipedia.org/wiki/Coefficient_of_variation#Examples_of_misuse
        cvtm(i)=stdtm(i)/(obmn(i)+459.67)
        cvtx(i)=stdtx(i)/(obmx(i)+459.67)          
        if(obsl(i).le.0.0) then
          cvs(i)=0.0
        else
          cvs(i)=stdsl(i)/obsl(i)
        endif
 70   continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
c     do 80 i=1,12
c       wi(i)=wi(i)
c
      do 6000 i=1,12
6000  dir(i,1)=wvl(1,1,i)
c
      do 7010 i=1,12
        do 7000 j=2,16
          dir(i,j) = dir(i,j-1)+wvl(j,1,i)
7000    continue
        j=17
        dir(i,j)=100.0
7010  continue
c
      do 7050 i=1,12
        do 7049 j=1,17
          dir(i,j)=dir(i,j)*.01
7049    continue
7050  continue
c
      close (10)
c       Close the Parameter File and Write the Parameter if He Wants.
c
c     if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
      if(numarg.eq.0) then
        write(*,2000)stidd,nstat
        read(*,'(a1)')yc
      else
        yc = 'N'
      endif
c
c **** L1 IF ****
      if(yc.ne.'n'.and.yc.ne.'N') then
        write(*,2010)obmx,obmn
        write(*,2020)obsl
        write(*,2030)
        do 90 i=1,12
          write(*,2040) prw(i,1),prw(i,2)
 90     continue
        write(*,2050)
        do 100 i=1,12
          write(*,2060)(rst(i,j),j=1,3)
 100    continue
        write(*,2070)
        write(*,2080)(stdtx(i),i=1,12)
        write(*,2080)(stdtm(i),i=1,12)
        write(*,2090)(stdsl(i),i=1,12)
        write(*,2100)
        write(*,2080)(cvtx(i),i=1,12)
        write(*,2080)(cvtm(i),i=1,12)
        write(*,2080)(cvs(i),i=1,12)
        write(*,2080)(wi(i),i=1,12)
        write(*,2110)rh
        write(*,2080)tp6
        write(*,2120)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
        write(*,2130)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
        write(*,2130)(calm(i),i=1,12)
c **** L1 ENDIF ****
      endif
      return
      end
