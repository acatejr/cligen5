c
      subroutine chitst(n,level0)
      integer n,level0
c
c     n      -- parameter number
c     level0 -- pass/fail = 0/1
c
      include 'crandom3.inc'
c        read: mox, chicnt
c       write: chi_n, sumchi
c     chicnt -- count of this month's chi-square bins
c     chi_n  -- number of observations 
c     sumchi -- V-statistic
c
      real e_chi
CC    write(*,*) 'ChiCnt:', (chicnt(n,mox,ichi),ichi=1,20)
c
c ---- Determine number of bins to use for chi-square test.
c      (Count number of observations and divide by 5.)
CC    write(*,*) 'N: ', n, ' Level0:', level0
      chi_n = 0
      do 10, i=1, 20
        chi_n = chi_n + chicnt(n,mox,i)
 10   continue
CC    write(*,*) (chicnt(10,mox,i),i=1,20)
CC    write(*,*) 'Chi_N: ', chi_n, ' Mox: ', mox
c
      level0 = 0
      sumchi = 0.0
c ---- Use 20 bins.
      if(chi_n.ge.100) then
        e_chi = 0.05*chi_n
        do 20, i=1, 20
          sumchi = sumchi + (e_chi - chicnt(n,mox,i))**2 / e_chi
 20     continue
        if(sumchi.gt.18.3376) level0 = 1
C       if(sumchi.gt.15.3517) level0 = 1
c
c ---- Use 10 bins.
      else if(chi_n.ge.50) then
        e_chi = 0.1*chi_n
        do 30, i=1, 20, 2
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1))**2 / e_chi
 30     continue
        if(sumchi.gt.8.34283) level0 = 1
C       if(sumchi.gt.6.3933) level0 = 1
c 
c ---- Use 5 bins.
      else if(chi_n.ge.25) then
        e_chi = 0.2*chi_n
        do 40, i=1, 20, 4
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3))**2 / e_chi
 40     continue
        if(sumchi.gt.3.35669) level0 = 1
C       if(sumchi.gt.2.1947) level0 = 1
c 
c ---- Use 4 bins.
      else if(chi_n.ge.20) then
        e_chi = 0.25*chi_n
        do 50, i=1, 20, 5
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3)
     4                             - chicnt(n,mox,i+4))**2 / e_chi
 50     continue
        if(sumchi.gt.2.36597) level0 = 1
C       if(sumchi.gt.1.4237) level0 = 1
c
c ---- Use 2 bins.
c      Note: Not valid for chi_n < 10, but keeps things from going 
c            totally out of control before chi_n reaches 10....
Cc    else if(chi_n.ge.10) then
      else if(chi_n.ge.2) then
        e_chi = 0.5*chi_n
        do 60, i=1, 20, 10
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3)
     4                             - chicnt(n,mox,i+4)
     5                             - chicnt(n,mox,i+5)
     6                             - chicnt(n,mox,i+6)
     7                             - chicnt(n,mox,i+7)
     8                             - chicnt(n,mox,i+8)
     9                             - chicnt(n,mox,i+9))**2 / e_chi
 60     continue
        if(sumchi.gt.0.45494) level0 = 1
C       if(sumchi.gt.0.14847) level0 = 1
c
c ---- Skip Chi-square test because it's not valid.
      else
        level0 = 0
        e_chi = 0.0
        sumchi = 0.0
      endif
c
CC    if(level0.eq.1) then
CC      write(*,*) 'Chi Reject'
CC      write(*,*) 'Parm:',n,' Month:',mox,' N:',chi_n,' V:',sumchi
CC    else
CC      write(*,*) 'Chi Pass'
CC    endif
c
      return
      end
c
c
      subroutine ks_tst(n,level1)
      integer n,level1
c
c    Purpose: Calculate whether parameters are uniformly distributed, using
c             the Kolmogorov-Smirnov goodness of fit test.
c    Comment: Based on the Chi-Square routine.
c    Written 5/5/2004 -- C. R. Meyer
c
c     n      -- parameter number
c     level1 -- pass/fail = 0/1
c
      include 'crandom3.inc'
c        read: mox, chicnt
c       write: chi_n
c     chicnt -- count of this month's chi-square bins
c     chi_n  -- number of observations 
c
      real e_chi
      integer ks_cnt(20)
      real ks_dif(20), maxdif
c
c      e_chi -- number of obs expected in each Chi-Sq bin
c     ks_cnt -- cumulative count of chi-square bins
c     ks_dif -- cum difference between expected and count of bins
c
CC    write(*,*) 'ChiCnt:', (chicnt(n,mox,ichi),ichi=1,20)
c
c ---- Determine number of bins to use for chi-square test.
c      (Ie, count number of observations and divide by 5.)
      level1 = 0
      chi_n = 0
      do 10, i=1, 20
        chi_n = chi_n + chicnt(n,mox,i)
        ks_cnt(i) = 0
        ks_dif(i) = 0.0
 10   continue
c
c ---- Use 20 bins.
      if(chi_n.ge.100) then
        e_chi = 0.05*chi_n
        ks_cnt(1) = chicnt(n,mox,1)
        ks_dif(1) = abs(ks_cnt(1) - e_chi)
        maxdif = ks_dif(1)
        do 20, i=2, 20
          e_chi = i*0.05*chi_n
          ks_cnt(i) = ks_cnt(i-1) + chicnt(n,mox,i)
          ks_dif(i) = abs(ks_cnt(i) - e_chi)
          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
 20     continue
        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
c
Cc ---- Use 10 bins.
C      else if(chi_n.ge.50) then
C        e_chi = 0.1*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 30, i=3, 20, 2
C          e_chi = i*0.1*chi_n
C          ks_cnt(i) = ks_cnt(i-2) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 30     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc 
Cc ---- Use 5 bins.
C      else if(chi_n.ge.25) then
C        e_chi = 0.2*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2) + chicnt(n,mox,3)
C     1              + chicnt(n,mox,4)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 40, i=5, 20, 4
C          e_chi = i*0.2*chi_n
C          ks_cnt(i) = ks_cnt(i-4) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C     1                + chicnt(n,mox,i+2) + chicnt(n,mox,i+3)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 40     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc 
Cc ---- Use 4 bins.
C      else if(chi_n.ge.20) then
C        e_chi = 0.25*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2) + chicnt(n,mox,3)
C     1              + chicnt(n,mox,4) + chicnt(n,mox,5)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 50, i=6, 20, 5
C          e_chi = i*0.25*chi_n
C          ks_cnt(i) = ks_cnt(i-4) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C     1                + chicnt(n,mox,i+2) + chicnt(n,mox,i+3)
C     2                + chicnt(n,mox,i+4)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 50     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc
Cc ---- Use 2 bins.
Cc      Note: Not valid for chi_n < 10, but keeps things from going 
Cc            totally out of control before chi_n reaches 10....
CCc    else if(chi_n.ge.10) then
C      else if(chi_n.ge.2) then
C        e_chi = 0.5*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2)
C     1              + chicnt(n,mox,3) + chicnt(n,mox,4)
C     2              + chicnt(n,mox,5) + chicnt(n,mox,6)
C     3              + chicnt(n,mox,7) + chicnt(n,mox,8)
C     4              + chicnt(n,mox,9) + chicnt(n,mox,10)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        e_chi = chi_n
C        ks_cnt(11) = chicnt(n,mox,11) + chicnt(n,mox,12)
C     1              + chicnt(n,mox,13) + chicnt(n,mox,14)
C     2              + chicnt(n,mox,15) + chicnt(n,mox,16)
C     3              + chicnt(n,mox,17) + chicnt(n,mox,18)
C     4              + chicnt(n,mox,19) + chicnt(n,mox,20)
C        ks_dif(11) = abs(ks_cnt(11) - e_chi)
C        if(maxdif .lt. ks_dif(11)) maxdif = ks_dif(11)
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
c
c ---- Skip Chi-square (and K-S) test because it's not valid.
      else
        level1 = 0
      endif
c
C     if(level1.eq.1) then
C       write(*,*) 'KS Reject -- Parm:',n,' Month:',mox,' N:',chi_n
C     else
C       write(*,*) 'KS Pass'
C     endif
c
      return
      end
c
c
      subroutine conflm(xbar,n,mu,sigma,level)
      real xbar,mu,sigma,level
      integer n
c
c    A confidence interval on the sample mean.
c    Returns the "level" (percent) at which one can be confident
c    that the sample of 'N' measurements which produced Xbar, came
c    from a population DIFFERENT FROM a population with mean 'Mu'
c    and variance 'Sigma'.  Note that this is a test of the sample
c    mean only -- it does not involve or test the sample varience.
c
c    Generally:
c               xbar - mu
c            ---------------  ~  N(0,1); ie, Std. Normal
c            sigma / sqrt(n)
c
c    This was originally a recursive routine.  F-77 doesn't support
c    recursion.
c
c    Written 12/28/99 -- C. R. Meyer
c
      parameter (nz=15)
      real z(nz),prob(nz)
      real up_lim,lowlim,margin
      integer bkthru
c
c ------ Standard Normal Z-values:
      data z/2.807,2.576,1.96,1.645,1.282,1.036,0.8416,0.6745,
     1       0.5244,0.3853,0.2533,0.1257,0.06271,0.01253,0.006267/
c ------ Probabilities (percent) that populations are DIFFERENT.
      data prob/99.5,99.0,95.0,90.0,80.0,70.0,60.0,50.0,40.0,
     1       30.0,20.0,10.0,5.0,1.0,0.5/
c
      if(n. gt. 0) then
        index = 0
 10     continue
        bkthru = 0
        if(index .lt. nz) then
          index = index + 1
          margin = z(index)*sigma/sqrt(float(n))
          up_lim = xbar + margin
          lowlim = xbar - margin
c
          if((mu .gt. up_lim).or.(mu .lt.lowlim)) then
            level = prob(index)
          else
            bkthru = 1
c           call conflm(xbar,n,mu,sigma,level,index)
          endif
        else
          level = 0.0
        endif
        if(bkthru .ne. 0) goto 10
      else
        level = 0.0
      endif
c
      return
      end
c
c
      subroutine confls(x2sum,n,level)
c ---- x2sum  -- sum of the squares of the standard normal deviates.
c ---- n      -- number of deviates (degrees of freedom)
c ---- level  -- level at which one can be sure the sample of deviates
c                 is DIFFERENT FROM the parent population.
c
      real x2sum,level
      integer n
c ---- For ACM routines:
      double precision bound,df,p,q,x
      integer status,which
      data which /1/
      x = dble(x2sum)
      df = dble(n)
c
c    A confidence interval on the *Standard Normal* sample varience.
c    Returns the "level" (percent) at which one can be confident
c    that the sample of 'N' measurements which produced X2sum, came
c    from a population DIFFERENT FROM a population with mean=0.0,
c    and variance=1.0 .  Note that this is a test of the sample
c    varience only -- it does not involve or test the sample mean.
c
c    Generally:
c             / Xi - mu \ 2       2
c        Sum  | ------- |     ~  X (N); ie, Chi-Square(N)
c             \  sigma  /
c
c    But for our purposes, since mu=0 and sigma=1, this reduces to:
c
c            /  2\      2
c        Sum \Xi /  ~  X (N); ie, Chi-Square(N)
c
c    Written 05/02/00 -- C. R. Meyer
c    Rewritten 5/16/00 to utilize Chi-Square routines from Numerical Recipes
c    --- CRM
c
      if(n .gt. 0) then
c ------- LEVEL is a decimal fraction here:
c ------- To use binary "chi-sq.o" from copyrighted Numerical Recipes:
c       level = gammp(float(n)/2.0,x2sum/2.0)
c ------- To use public domain ACM code (below):
        call cdfchi(which,p,q,x,df,status,bound)
        level = p
c ------- LEVEL is a percent here:
        level = 100.0 * 2.0*abs(0.5 - level)
      else
        level = 0.0
      endif
c
      return
      end
