c
c
c
      block data
c
c     + + + PURPOSE + + +
c     Initialize variables in the Common Blocks.
c     Contains generator seeds for the weather generator.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
      include 'cbk7.inc'
c -- XXX -- CRM Added 4/3/2000:
      include 'crandom3.inc'
c -- XXX -- CRM Added 7/17/2000:
      include 'command6.inc'
c -- XXX -- CRM Added 8/22/2000:
      include 'cinterp.inc'
c
c     + + + DATA INITIALIZATIONS + + +
      data k1/9,98,915,92/
      data k2/135,28,203,85/
      data k3/43,54,619,33/
      data k4/645,9,948,65/
      data k5/885,41,696,62/
      data k6/51,78,648,0/
      data k7/227,57,929,37/
      data k8/205,90,215,31/
      data k9/320,73,631,49/
      data k10/22,103,82,4/
      data nc/0,31,59,90,120,151,181,212,243,273,304,334,365/
      data dtp/.4,.32,.5,.5/
      data dmxi/18.24,5.76,32.88,20.16/
c -- XXX -- CRM Added 4/3/2000 for checking quality of Random Numbers:
      data mox/0/
      data dax/0/
      data ranary/279*0.0/
      data g_dimi/12*0/
      data g_dimp/12*0/
      data g_dsum/108*0.d0/
      data g_ssum/108*0.d0/
C     data chicnt/2160*0/
      data chicnt/2400*0/
      data thresh/50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0/
      data thres2/50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0/
      data istate /-1/
      data index /-1/
      data ibyear /-1/
      data numyr /-1/
      data iopt /-1/
      data force /0/
      data outfil /"XXX"/
      data infile /"XXX"/
      data odfile /"XXX"/
      data dohedr /.true./
      data interp /0/
      end
