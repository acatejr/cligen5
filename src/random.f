c
c
c
      real function randn
     m         (k)
c
c     + + + PURPOSE + + +
c     Provides random numbers ranging from 0.0 to 1.0
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer k(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     k           - Seed for random number generation.
c
c     + + + END SPECIFICATIONS + + +
c
 10   continue
      k(4)=3*k(4)+k(2)
      k(3)=3*k(3)+k(1)
      k(2)=3*k(2)
      k(1)=3*k(1)
      i=k(1)/1000
      k(1)=k(1)-i*1000
      k(2)=k(2)+i
      i=k(2)/100
      k(2)=k(2)-100*i
      k(3)=k(3)+i
      i=k(3)/1000
      k(3)=k(3)-i*1000
      k(4)=k(4)+i
      i=k(4)/100
      k(4)=k(4)-100*i
      randn=(((float(k(1))*.001+float(k(2)))*.01+float(k(3)))*.001+
     1      float(k(4)))*.01
c ---- range check:
      if(randn .le. 0.0 .or. randn .ge. 1.0) goto 10
c
      return
      end
c
c
c
      subroutine nrmd
     m           (r1)
c
c     + + + PURPOSE + + +
c     Returns the standard normal deviate for a given probability value
c     (e.g. r1=.99 returns r1=2.328)
c
c     + + + ARGUMENT DECLARATIONS + + +
      real r1
c
c     + + + ARGUMENT DEFINITIONS + + +
c     r1          - initially, the probability; finally the std. norm. deviate.
c
c     + + + END SPECIFICATIONS + + +
c
      if(r1 .ge. 0.5) then
        sgn = 1.0
        hp = 1.0 - r1
      else
        sgn = -1.0
        hp = r1
      endif 
      t = sqrt(alog(1./(hp*hp)))
      r1 = sgn*(t-(2.30753+.27061*t)/(1.+.99229*t+.04481*t*t))
      return
      end
      subroutine ranset(ntd,iyear)
c ---- Purpose: Generate Array of Random Numbers, an entire month at a
c               time, for each parameter.  Ensure that for the run to 
c               date, the numbers will produce standard normal deviates
c               with a mean at the specified confidence level ("thresh")
c               and a varience at the specified confidence level ("thres2").
c               C. R. Meyer -- 4/6-24/2000 and 5/16-17/2000.
c
c    Modified: Was using NT to determine whether the current year is a
c              Leap Year; however, NT only gets set for options 4 & 7,
c              and only for the *initial year*.  Modified to use NTD.
c              C. R. Meyer -- 3/7/2001
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     ntd    - days in this year (365 or 366)
c
c   RanAry elements:
c       ranary(1,1) -- precip prob
c       ranary(1,2) -- max temp
c       ranary(1,3) -- min temp
c       ranary(1,4) -- radiation
c       ranary(1,5) -- precip amount
c       ranary(1,6) -- wind dir
c       ranary(1,7) -- wind vel
c       ranary(1,8) -- TDP
c       ranary(1,9) -- TP
c
C -- XXX -- NT is no longer used.  Probably can eliminate this include:
C       CRM -- 3/20/01.
      include 'cbk4.inc'
c        read: nt
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero
c
      include 'cbk7.inc'
c      modify: k1-k6, k8-k10, v1-v12
c
      include 'crandom3.inc'
c        read: mox, dax, thresh, thres2, vv
c      modify: chicnt
c       write: ranary,g_dimi,g_dimp,g_dsum,g_ssum,chicnt
c
c ---- dim -- Days in each Month.
      integer dim(12), dimi, ldimp, ell, ellx
      character*15 params(9)
      real last_r(9), lst_rx(9)
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c ---- ell -- ell=1 ==> Precip; ell=2 ==> No Precip.
      data ell/2/
      data last_r/9* -1.0/
      data params/"Prob. of Precip", "Max. Temp.", "Min. Temp.",
     & "Radiation", "Precip. Amt.", "Wind Dir.", "Wind Vel.",
     & "Temp. Dew Pt.", "Time to Peak"/
      save ell, last_r
CC    save chisum
CC    save chicnt
c
c     real ransum,ranavg,level
      integer level0,level1,chisum(20),ichi
      real ransum,level,level2
c     ransum -- sum of this month's random std norm deviates
c     x2sum  -- sum of S^2 (ie, Xi^2) for this month's random std norm deviates
c     chisum -- count of the chi-square bins for this month only
c     ranavg -- avg of this month's random std norm deviates
c     level0 -- pass/fail (0/1) on chi-square test with up to 20 bins. 
c     level1 -- pass/fail (0/1) on K-S test with up to 20 bins. 
c     level  -- level at which we're confident of a difference in the means.
c     level2 -- level at which we're confident of a difference in the S^2's.
c     g_dsum -- Sum of std norm deviates, for this parameter, for this month,
c                from the beginning of the simulation.
c     g_ssum -- Sum of S^2 of std norm deviates, for this parameter, for this
c                month, from the beginning of the simulation.
c     chi_n  -- number of observations
c     chicnt -- Sum of bins for chi-square test of RNG.
c     sumchi -- V-statistic
c     g_dimi -- Total days for this month of the year, from the beginning
c                of simulation to end of current month.
c     g_dimp -- Total days with precip for this month of the year, from the
c                beginning of simulation to end of current month.
c     g_davg -- avg to date for this parm's random std norm deviates, for
c                this month of the year.
c     ldimp  -- Local variable for days in the month with precip.
c     last_r -- Value of last random deviate generated for this parm.
c     ellx   -- Saves the value of "ell" in case we re-do preip amts.
c     lst_rx -- Saves the value of "last_r(j)" in case of a re-do
c
c ---- update the number of days to end of this month.
      if(mox.ne.2 .or. ntd.eq.365) then
        dimi = dim(mox)
      else
        dimi = 29
      endif
      g_dimi(mox) = g_dimi(mox) + dimi
c
c ---- If RANSET has not executed before, load initial RNG values:
      if(last_r(1) .eq. -1.0) then
        vv = randn(k1)
        last_r(1) = vv
        v1 = randn(k2)
        last_r(2) = v1
        v3 = randn(k3)
        last_r(3) = v3
        v5 = randn(k4)
        last_r(4) = v5
        v7 = randn(k5)
        last_r(5) = v7
        fx = randn(k6)
        last_r(6) = fx
        v9 = randn(k8)
        last_r(7) = v9
        v11 = randn(k9)
        last_r(8) = v11
        z = randn(k10)
        last_r(9) = z
      endif
c
c ------ prepare to count number of re-do's.
      iredo = 0
c ------ save the value of "ell" in case we re-do precip amount.
      ellx = ell
c
      do 999 j=1, 9
c ------ save "last_r(j)" in case we have a re-do.
        lst_rx(j) = last_r(j)
c
c ------ Sum up the standard normal deviates for this month.
 10     continue
          if(j .eq.5) ldimp = 0
          ransum=0.0
          x2sum =0.0
          do 112 ichi=1, 20
            chisum(ichi) = 0
  112     continue
c
c ------ BEGIN random value generation for the current parm.
c        (Each parm uses its own instance of the random number generator.)
          do 998 i=1, dimi
            if(j .eq. 1) then
              ranary(i,j) = randn(k1)
            else if(j .eq. 2) then
              ranary(i,j) = randn(k2)
            else if(j .eq. 3) then
              ranary(i,j) = randn(k3)
            else if(j .eq. 4) then
              ranary(i,j) = randn(k4)
            else if(j .eq. 5) then
c ---------- we had precip today.
              if(ranary(i,1) .le. prw(mox,ell)) then
                ranary(i,j) = randn(k5)
                ell = 1
                ldimp = ldimp + 1
c ---------- we didnt have precip today.
              else
                ranary(i,j) = 0.0
                ell = 2
              endif
            else if(j .eq. 6) then
              ranary(i,j) = randn(k6)
            else if(j .eq. 7) then
              ranary(i,j) = randn(k8)
            else if(j .eq. 8) then
              ranary(i,j) = randn(k9)
            else if(j .eq. 9) then
              if (iopt.eq.6) then
                 ranary(i,j) = 0.0
              else
c------------   if there was precip today, generate a Tpeak:
                if(ranary(i,5) .gt. 0.0) then
C     write(*,*) "yes precip"
                  ranary(i,j) = randn(k10)
                 ell = 1
                else
C     write(*,*) " no precip"
                  ranary(i,j) = 0.0
                  ell = 2
                endif
              endif
C     write(*,*) ranary(i,j)
            endif
c ------ END random value generation for the current parm.
c
c -------- Sum the matching Chi-square bin.
            if(ranary(i,j) .gt. 0.0) then
              ichi = int(ranary(i,j)*20.0) + 1
              if(ichi.gt.20) ichi = 20
              chisum(ichi) = chisum(ichi) + 1
            endif
c -------- Sum Random Standard Normal Deviates for the current parm. & month
CC          if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9) then
            if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                         .and. j.ne.10) then
              if(j .ne. 5 .or. ell .eq. 1) then
                ransum = ransum + dstn1(last_r(j),ranary(i,j))
                x2sum = x2sum + dstn1(last_r(j),ranary(i,j))**2
                last_r(j) = ranary(i,j)
              endif
            endif
c
 998      continue
 
c        do stat's for everything but observed and tpeak 
         if (iopt.ne.6 .or. j.ne.9) then
c
c ------ Update the Grand Count for the chi-square deviates for this month.
C         if(iredo .eq. 9996)
C    &       write(*,*) " CHICNT IN:",(chicnt(j,mox,ichi),ichi=1,20)
          do 113 ichi=1, 20
            chicnt(j,mox,ichi) = chicnt(j,mox,ichi) + chisum(ichi)
  113     continue
c
c ------ Perform SN tests -- This is a SN distribution.
          if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                        .and. j.ne.10) then
c -------- Update the Grand Sum of the standard normal deviates for this month.
            g_dsum(j,mox) = g_dsum(j,mox) + ransum
            g_ssum(j,mox) = g_ssum(j,mox) + x2sum
c -------- Compute the Grand Average of the Std Norm Dev's for this month.
c -------- There is an Observation each day.
            if(j .ne. 5) then
              g_davg = g_dsum(j,mox) / float(g_dimi(mox))
CC    write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C             call chitst(j,level0)
C             if(level0.eq.0) then
              call ks_tst(j,level1)
              if(level1.eq.0) then
                call conflm(g_davg,g_dimi(mox),0.0,1.0,level)
                call confls(real(g_ssum(j,mox)),g_dimi(mox),level2)
              endif
c -------- There is not an Observation every day (Pcp Amt).
            else
              g_dimp(mox) = g_dimp(mox) + ldimp
              if(g_dimp(mox) .gt. 0) then
                g_davg = g_dsum(j,mox) / float(g_dimp(mox))
              else
                g_davg = 0.0
              endif
CC    write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C             call chitst(j,level0)
C             if(level0.eq.0) then
              call ks_tst(j,level1)
              if(level1.eq.0) then
                call conflm(g_davg,g_dimp(mox),0.0,1.0,level)
                call confls(real(g_ssum(j,mox)),g_dimp(mox),level2)
C               if((level0.gt.0) .or. (level.gt.thresh(j)))
C    &             write(*,*) "Passed Chi-Square, But *FAILED* SN."
              endif
            endif
c
c ------ Don't Perform SN tests -- uniform dist (P(pcp), Wdir, Wvel, & Tpeak).
          else
CC      write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C           call chitst(j,level0)
            call ks_tst(j,level1)
            level=-1.0
            level2=-1.0
          endif
c
CC    if(j.eq.5 .and. mox.eq.9) then
CC      write(*,*) " "
CC      write(*,*) "Parm:",params(j), " Mon:",mox," Year:",iyear," N:",
CC   &             chi_n," V:",sumchi
CC      write(*,*) "Chicnt:",(chicnt(j,mox,ichi),ichi=1,20)
CC      write(*,*) " "
CC    endif
c
c ------ If the result throws us outside the confidence limits, RE-DO.
C         if((level0.gt.0) .or. (level.gt.thresh(j)) .or. 
          if((level1.gt.0) .or. (level.gt.thresh(j)) .or. 
     &       (level2.gt.thres2(j))) then
CC          write(*,*) " ********** REDO ***********"
c----------- Count the number of "re-do's" to exit after the prescribed number.
            iredo = iredo + 1
            if(iredo .gt. 9995) then
c ---------- Print error messages:
C             if(level0.gt.0) then
              if(level1.gt.0) then
CC              write(*,*) "Failed Chi-square test."
CC              write(*,*) "Chicnt:",(chicnt(j,mox,ichi),ichi=1,20)
C               write(*,*) "FAILED Chi-Sq -- Parm:",params(j), " Mon:",
C    &                     mox," Year:",iyear," N:",chi_n," V:",sumchi
                write(*,*) "FAILED K-S -- Parm:",params(j), " Mon:",
     &                     mox," Year:",iyear," N:",chi_n
              endif
c ---------- SN parameters:
              if(level.gt.thresh(j) .or. level2.gt.thres2(j)) then
                if(level.gt.thresh(j)) write(*,*) "Failed SN Mean test."
                if(level2.gt.thres2(j)) write(*,*) "Failed SN SD test."
                if(j.ne.5) then
                  write(*,*) "Parm:",params(j)," Month:",mox," Year:",
     &              iyear," Mlevel=",level," SDlevel=",level2," Days:",
     &              g_dimi(mox)
                else
                  write(*,*) "Parm:",params(j)," Month:",mox," Year:",
     &              iyear," Mlevel=",level," SDlevel=",level2," Days:",
     &              g_dimp(mox)
                endif
              endif
            endif
            if(iredo.ne.10000) then
c ---------- Subtract this iteration from QC stats:
CC            if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9) then
              if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                           .and. j.ne.10) then
                g_dsum(j,mox) = g_dsum(j,mox) - ransum
                g_ssum(j,mox) = g_ssum(j,mox) - x2sum
              endif
              if(j .eq. 5) then
                g_dimp(mox) = g_dimp(mox) - ldimp
                ell = ellx
              endif
              do 114 ichi=1, 20
                chicnt(j,mox,ichi) = chicnt(j,mox,ichi) - chisum(ichi)
  114         continue
c
CC            if(iredo .eq. 9996) then
CC              write(*,*) "    CHISUM:",(chisum(ichi),ichi=1,20)
CC              write(*,*) "CHICNT OUT:",(chicnt(j,mox,ichi),ichi=1,20)
CC            endif
              last_r(j) = lst_rx(j)
            else
c ---------- Give up and Bail Out:
              write(*, '("*** ERROR *** Could not produce desired",
     &              " level of quality in",/20x, "<< ", a, " >>",
     &              " random deviates.")') params(j)
            endif
          endif
C       if((level0.gt.0.or.level.gt.thresh(j).or.level2.gt.thres2(j))
        if((level1.gt.0.or.level.gt.thresh(j).or.level2.gt.thres2(j))
     &     .and. iredo.lt.10000) goto 10
c     
c      don't do stats for observed and tpeak     
       endif
c
 999  continue
c
      return
      end
