c
c
c     Cligen version 5.322. 09/10/2024 Fred Fox
c        - Corrected calculation of Coefficient of Variation so 
c          a zero monthly average temperature does not cause a
c          divide by zero. Merged change from WEPS version 5.3004 12/13/18
c
c     Cligen version 5.32.1  04/06/2022  Roger Lew
c       - Fix for leap years to include years divisible by 400 but not
c          divisible by 100
c
c     Cligen version 5.32  03/14/2013 Jim Frankenberger
c       - using observed option (type=6) the tpeak variable
c         was being generated based only the cligen predicted days
c         with precip so most days with observed data had tpeak=0.
c         Updated to make sure type 6 input has the same distribution
c         of tpeak as generated precip.
c
c       Cligen version 5.31  01/31/2013 Jim Frankenberger
c        - Corrected solar radiation, it was not using standard
c          deviations from input par file. Increased year field to 5
c          digits for 10000 year+ runs.
c
c       Cligen version 5.30002. 09/14/09 Fred Fox
c        - Extended character string length for reading command line
c          arguments to allow longer output path length on linux.
c          Should now allow paths as long as WEPS. Path length on
c          Windows was already longer than maximum allowed.
c
c	Cligen version 5.30001. 06/30/09 Bill Rust
c	 - Changed order of reading in wet/wet & wet/dry
c	   Equivalence statement had interleaved the values
c	   making them incorrect
c	   Affected only Yoder-Foster & Fourier interpolation
c
c       Cligen version 5.3. 01/15/2008 Jim Frankenberger
c        - Corrected dew point calculation for a type 6 run.
c          Dew point was not being calculated from tmin and tmax values
c          read from the observed input.
c
c       Cligen version 5.22564.  10/26/2004.  C. R. Meyer
c        - In SR R5MONB variable WI(I) which is used to calculate AI,
c          was getting hosed when F went negative.  This would mess up Ip
c          for very dry climates where the average number of rainfall
c          events is less than 0.5 .
c
c       Cligen version 5.22563.  10/21/2004.  C. R. Meyer
c        - In SR USR_OPT variable NDFLAG was not being initialized unless
c          the type of run was 6 or 8.  Added a default "else" to the loop
c          to set it to zero.  Thanks to Larry Wagner & Fred Fox of WERU 
c          for pointing out this error.
c
c       Cligen version 5.22562.  10/01/2004.  C. R. Meyer
c        - Declared av_len to be integer in commandX.inc.  Note "command5.inc"
c          is no longer used -- "command6.inc" replaces it.
c
c       Cligen version 5.22561.  09/30/2004.  C. R. Meyer
c        - Using the Lahey compilers for Linux and Windows, a typo in SR RYF1
c          was generating a subscript-out-of-range error, unlike the GNU
c          Win-32 compiler.  Many thanks to Fred Fox and Larry Wagner of
c          ARS-WERU for their patience and persistence in pointing out that 
c          error.
c
c       Cligen version 5.2256.  07/02/2004.  C. R. Meyer
c        - For the Bloomington, Indiana site the Gamma distribution could 
c          not produce the desired level of quality with a lot size of just 
c          20 numbers.  An infinite loop resulted.  The lot size was increased 
c          to 30.
c
c       Cligen version 5.2255.  05/05/2004 - 05/06/04.  C. R. Meyer
c        - Running Excel with varying bin sizes from 10 to 20 showed that
c          the Chi-square test gave very inconsistent results in evaluating
c          goodness of fit for 30 year runs at 7 stations.  Reading articles
c          on the Internet revealed that some authors say Chi-Sq is only
c          applicable to discrete distributions -- for continuous distros
c          one should use the Kolmogorov-Smirnov test.  One problem: I
c          could not find D-value for P=50%; ie, P(50).  Equation is
c          P(D) = sum((-1^k)*exp(-2*k^2*D^2), k=-infinity, +infinity .
c          Wrote routine KS_test, using D-value derived interatively
c          using a spreadsheet (0.8276).
c
c       Cligen version 5.2254.  04/03/2004 - 04/05/04.  C. R. Meyer
c        - Fixed bug in DSTG that sometimes resulted in an infinite loop.
c          Removed third parameter (rn1) from DSTG, since it is in common
c          block crandom3.inc .
c
c       Cligen version 5.2253.  11/07/2003 - 01/07/04.  C. R. Meyer
c        - Changed generation of daily temperature values so that monthly
c          means & SD's of neither Tmin or Tmax are altered "up front", but
c          day-by-day as Tmin, Tmax, & Tdew are generated together for that
c          day.  Daily values are based on the _difference_ from Tmin or
c          Tmax -- whichever one has the smaller SD for that day.  This
c          approach developed after a suggestion by Fred Fox.
c
c     NOTE: With this version there are several mathematically ingenious features
c           incorporated in Cligen.  a) quality control on the random number 
c           generator, which is totally necessary and to date not known to be 
c           provided in any other stochastic model;  b) the generic random number 
c           generation scheme used for the Gamma, which can be employed for _any_
c           _function_ that can be utilized between x and f(x) max-min limits;
c           c) use of _monthly_ distributions of Tmin, Tmax, and Tdew to generate
c           related _daily_ values of these parameters which are based on each 
c           other, and reproduce the monthly means and SD's.
c
c       Cligen version 5.224.  10/20-20/2003.  C. R. Meyer
c	Changes:
c        - Changed generation of daily Tmin value from being independently 
c          generated, to being generated based on Tmax minus the _difference_
c          Tmax-Tmin.  This makes the daily Tmin a function of the daily Tmax
c          and can be accomplished using the same inputs.
c
c       Cligen version 5.223.  10/08-16/2003.  C. R. Meyer
c	Changes:
c        - Added QC code to function DSTG, which is involved in calculating
c          individual storm durations and peak intensities.
c        - In DSTG used *TWO* uniform deviates to generate each Gamma
c          deviate, instead of one as has been incorrectly used up to now.
c        - Obsoleted crandom2.inc, replacing it with crandom3.inc.
c
c       Cligen version 5.222.  09/23/2003.  C. R. Meyer
c	Changes:
c        - Reverted from "xx=rn1*5.795" to "xx=rn1*ai" in function DSTG.
c          Storm duration was getting hosed.
c
c       Cligen version 5.221.  09/23/2003.  C. R. Meyer
c	Changes:
c        - Disabled use of file "rand_nrs" which exists solely to verify
c          quality of random numbers.
c
c       Cligen version 5.220.  08/04/2003.  C. R. Meyer
c	Changes:
c        - Implemented a Chi-square test with up to 20 bins to test the RNG.
c        - Corrected a long-standing minor coding bug in 10,000th
c          (unsuccessful) retry.
c
c       Cligen version 5.213.  08/01/2003.  C. R. Meyer
c	Changes:
c        - In DSTG the maximum-x allowed for the Gamma function was originally
c          restricted to 'ai', the ratio of "total rain" to "30-minute rain".
c          Ai is different for each site & month combination.  Chi-square tests
c          on 5 cases where ai ranged between 1.49 and 2.25 show this virtually
c          guarantees a poor match between inputs and outputs (P => 0.98 in all
c          5 cases tested).  Integrating the function using Maple reveals that
c          it continues to accrue area under the curve until 'x' reaches 5.795,
c          reaching 99 percent by x = 2.9.  So the maximum x used in DSTG was
c          changed from ai to a constant 5.795, which yields P < .625 for the
c          same 5 cases.  This calculation impacts Storm Duration and Ipeak.
c
c       Note: In "Evaluation of CLIGEN precipitation parameters and their
c             implication on WEPP runoff and erosion prediction", Trans. ASAE,
c             Vol. 46(2):311-320, Zhang and Garbrecht seemed to feel CLIGEN
c             V-5.107's main problem was the single storm characteristics.
c             They specifically mentioned storm duration, peak intensity, and
c             Tpeak which are impacted by these most recent changes to CLIGEN
c             (V-5.212 & V-5.213).  The areas that C. R. Meyer had already
c             addressed, seemed to be functioning satisfactorily.
c
c       Cligen version 5.212.  07/24/2003.  C. R. Meyer
c	Changes:
c        - RANSET was radically recoded for Tpeak which chi-square tests 
c          revealed was _not_ uniformly distributed.  Tpeak values are now
c          only generated for days with precip, instead of generating them
c          for all days and then using the values only for days with precip.
c          The RANSET code was also generally cleaned up.
c        - Added comments to DSTG explaining generation of random Gamma
c          deviates.  This is a general approach which can be used with *ANY*
c          PDF.  It requires you to pick a maximum and minimum value for X
c          and f(X).  It requires a uniform random number generator which
c          functions correctly.  Without using a quality control filter,
c          we have not found one which does.  It is also iterative, so you
c          cannot predict exactly how many values you'll have to generate.
c
c       Cligen version 5.200.  03/13/2003.  C. R. Meyer
c	Changes:
c        - At line #1061 (SR CLGEN), added limits to keep the monthly skew
c          coefficient for precipitation from getting larger than 4.5 or
c          smaller than -4.5 .  This is necessary because the Pearson Type-III
c          equation used generates negative precip values, which are converted
c          to the default value of 0.01 inch (0.3 mm).  With high skew values,
c          90+ percent of the generated values can be negative.
c
c       Cligen version 5.111.  10/16&17/2002.  C. R. Meyer
c	Changes:
c        - Added some prompts for users running Cligen interactively using the
c          "old" datasets, advising use of the F. S. data.  Also prompt with
c          instructions regarding how to _fix_ the problem, if "stations" file
c          or state file are missing.  Added prompt for international users
c          to consult "Survey of Climatology", 1982, by Griffiths & Driscoll.
c         (All changes below involve "option 6" which uses observed precip
c          and temperature data from one station, in conjunction with station
c          parameters for another (similar) station.)
c        - Allow years to have more than 2 digits in the output file.
c        - Allow runs of more or less than 100 years.  100 years becomes
c          the default.
c        - Allows user to specify beginning year.  If unspecified, uses
c          first year listed in observed data file.
c        - Allow option-6 runs from the command line.  This entails adding
c          a new switch, capital-O, for the observed data filename.  Note:
c          "command4.inc" is no longer used -- "command5.inc" replaces it.
c
c       Cligen version 5.110.  10/26/2001.  C. R. Meyer
c	Changes:
c        - Records any command line parameters by appending them to the
c          fifth line of the output file.  Note: "command3.inc" is no
c          longer used -- "command4.inc" replaces it.
c        - Eliminates infinite loop caused by specifying an output file
c          that already exists.
c
c	Cligen version 5.109.  10/11/2001.  C. R. Meyer
c	Changes:
c	 - Increases length of permissible command line argument (argv)
c          from 60 to 256 characters.  This permits longer file pathnames
c          to be used on the command line.  Note: "command2.inc" is no
c          longer used -- "command3.inc" is used instead.
c
c	Cligen version 5.108.  8/29/2001.  C. R. Meyer
c	Changes:
c	 - Corrects problem with temperature dewpoints, under option 6.
c	   Added interpolation code to SR clgen for when msim=0.  Previously
c	   overlooked.
c
c       Cligen version 5.107.  5/11-17/01.  C. R. Meyer
c       Changes:
c        - There are now two cases handled for station data inputs:
c           1) Runs where _both_ a state and station index are specified.
c              If an input file is specified, it is used.  If not, the
c              corresponding state file is used.  This case applies to
c              files of concatenated FS data.
c           2) Other runs with an input filename.  In this case it is assumed
c              to be a single-station parameter file, and the _first_ station
c              found is used.
c        - Changed code in SR "sta_dat" that stupidly blows past exactly 
c          81 lines when Station-ID and State don't match.  (Approx. line 
c          number: 1913.)  Revised code reads each line until it finds a
c          match to both state and station.  This lets Cligen read files
c          of concatenated FS data without WERU's "fs_flag".
c
c       Cligen version 5.106.  5/4/01. C. R. Meyer
c       Changes:
c        - Accepts filenames which contain blanks & slashes.  (Must enclose 
c          filenames in double-quotes under MS-Windows.)
c        - Reversed meaning of '-H' command line option.
c        - Cull out options not preceeded by dashes.
c        - Program termination message changed.
c        - Syntax error corrected for "unknown option".
c
c       Cligen version 5.105.  4/17/01.  C. R. Meyer
c       Changes:
c        - There is one place where Cligen is iterative -- in the QC process
c          applied to the RNG (SR RANSET).  The only exit was production of
c          a distribution meeting the acceptance criteria.  Not guaranteed!
c          For Yuma, AZ and Wupatki Natl. Mon., AZ, the program got hung in
c          an infinite loop as it tried to produce acceptable standard normal
c          deviates for Wind Velocity and Precip.  (Observation: It was possible
c          to greatly improve the situation for WV by changing the random seed
c          k8(4) from 31 to 41, but this is not the solution pursued.)  A
c          counter "iredo" was inserted in the loop, and when it reaches
c          10,000 an error is printed to the screen, and an exit is effected
c          from the loop.
c        - In SR CDFCHI initialized PORQ to zero with a data statement.
c
c       Cligen version 5.104.  4/5/01.  C. R. Meyer
c       Changes:
c        (With the addition of command line argument capability, Cligen
c         was modified to run: totally in command line mode; totally
c         interactively; or a mixture of the two.  A couple of interactive
c         options had been disabled in the process.  If the value of
c         "numarg" is zero, the mode is totally interactive.  Now, for
c         that mode behavior again duplicates V-4.2, given the same inputs.)
c        - Asks if user wants to make another run.
c        - Asks if user wants to see Station Parameters.
c
c	Cligen version 5.103.  4/5/01.  C. R. Meyer
c       Changes: 
c        (All the changes made are purely cosmetic.  The outputs produced
c         remain totally unchanged.)
c        - Within an MS-Windows DOS window, a space is incorrectly prepended
c          to each line.  The station parameters appear double-spaced
c          because each line of data requires *two* lines for display.
c          The first half of the stations to scroll off the screen before they
c          can be read.  Write formats were altered to produce single-spaced
c          screen outputs.
c        - Some debugging statements were commented out, which displayed
c          parameters to the screen, like Station_ID, State, and Ibyear
c          (beginning year).
c        - Some changes to facilitate more global version number changes, by
c          changing the value of the variable "version".
c
c       Cligen version 5.102.  3/20/01.  C. R. Meyer
c       Changes:
c        - Correction to +/- 10.0 SD range check in DSTN1.
c        - RANSET now uses NTD, because NT only reflects whether the _initial_
c           year is a leap year, not the current one.
c        - RANSET uses ELLX to save the value of ELL (whether yesterday had
c           precipitation or not) for the current call, in case rainfall
c           amounts are re-generated.
c        - RANSET uses LST_RX to save the value of LAST_X (value from previous
c           day) in the event of a re-do.
c
c       Cligen version 5.101.  2/6/01.  Changes made to SR RYF1 to accomodate
c       occurance of three identical consecutive monthly values.
c
c	Cligen source as of 01/25/2001.  Changed version number to "5.1".
c
c	Changes made as of 11/08/2000:  Modified to permit use of single
c       station ".par" input file with simulation types other than continuous.
c       Also changed version number to "4.2c".  Added "-rxxx" "-Ix" to first
c       of output file, so random number cycles and interpolation used could
c       be re-created.  Commented lines that produce files containing results
c       of CI tests on the means & SD'.
c
c       Changes made as of 8/30/2000:  "-h" option added (equivalent to
c       existing "-?" help option).  Corrected definitions for PRW(1&2)
c       which had been reversed.  Added interpolation -- linear, Fourier,
c       and one to preserve the monthly means.  For INDY max temp the 
c       worst deviation between the monthly average of daily values and
c       average monthly values, for this latter scheme was 0.002 degree 
c       F for an individual month, and 0.0003 overall.
c
c       This version includes one of Bofu Yu's corrections that had 
c       been missed earlier.  The code is verified to produce results
c       identical to Bofu's, using the 11/11/99 release of CLIGEN,
c       which is recoded to the WEPP coding convention, but which does
c       not include the RNG-QC code.
c
c       Adds command line options including choice of a State & Station
c       from the CLIGEN climate files, or use of a single input file.
c       Also accepts arguments specifying starting year, duration of run,
c       output file name, output header info, output version info, and
c       modify the random number seed.
c
c       This is done through use of the UNIX & GNU "getarg" & "iargc" options.
c       Options are also provided for the Watcom and  BSD(?) UNIX compilers.
c       (Simply uncomment the 2 lines approriate for your compiler.)
c       WERU functions nargs() & getarg() are provided for use with Lahey
c       which has no direct means for returning argument count, or specific
c       arguments.  (This code was significantly modified and tested with 
c       Lahey 4.0 F-77 -- a 1989 product, which, incidentally _has_ a built
c       in nargs() function, but it only works for SUBprograms.)
c
c       This version also tests both the mean and the variance of the
c       standard normal deviates generated, using a prescribed value for
c       each ("thresh" & "thres2") set here at 50 percent.  Calculations 
c       are performed by parameter, by month, on the population of numbers
c       generated to the current point in the simulation.
c
c      	CLIGEN V-4.2 with the following differences:
c        - Recoded by the WEPP F-77 Coding Conventions.  The logic
c          is greatly simplified; the structure of the code is much
c          improved; and the in-line documentation is greatly expanded.
c        - Includes Bofu Yu's corrections to make rainfall intensity 
c          responsive to latitude.  Routines ALPH and R5MON were 
c          replaced.  Constants DUR in DAY_GEN, and XN1 in DSTG were
c          altered.
c        - Cligen's outputs start with 9 uniform random distributions
c          and subsequent corresponding standard normal distributions.
c          Parameters derived from historic data are scaled from these
c          to produce daily values.  Statistical testing demonstrated
c          that more often than chance would dictate, the starting
c          distributions were doing a poor job of reproducing the numbers
c          they originated from.  A feedback loop was added to apply
c          "quality control" to the distributions are they are being 
c          produced.
c
c       To compile under UNIX execute:
c         "f77 -o cligen cligen.f"
c
c       To compile for W-95/98/NT:
c         Use the GNU MinGW-32 compiler.
c         From the W-9x command line execute:
c          "g77_setup.bat"
c          "g77 -o cligen cligen.f"
c
c --------------------------------------------------------------------------

      program cligen

c     program Cligen. WEPP Water Erosion Project Durant, OK.  Version 5.1
c     Please address inquiries to
c
c         WEPP Technical Support
c         USDA-ARS-NSERL
c         275 South Russell St.
c         West Lafayette, IN 47907-2077
c         Phone 765 494-8673
c
c --------------------------------------------------------------------------
c
c     (Radically) Recoded by Charles R. Meyer   August - November 1999.
c     e-mail: meyerc@ecn.purdue.edu  (Same surface mail as above.)
c     Note that questionable lines contain the string 'XXX'.
c     MANY variable definitions provided by David Hall, USFS, Moscow, ID.
c
c              Structure of Recoded CLIGEN:
c      
c      Main---sta_dat---header
c           |         |-sta_name
c           |         |-sta_parms
c           |   
c           |-r5mon
c           |-*randn
c           |-usr_opt
c           |-sing_stm
c           |-wxr_gen---*jdt
c                     |-day_gen---jlt
c                     |         |-clgen---*dstn1
c                     |         |       |-*randn
c                     |         |       |---ranset
c                     |         |         |-*randn
c                     |         |
c                     |         |-windg---*dstn1
c                     |         |-alph---*dstg--*randn
c                     |         |-timepk
c                     | 
c                     |-opt_calc---clmout
c       
c       
c      * -- denotes function. 
c       
c      Note: NRMD does not seem to be used.
c       
c --------------------------------------------------------------------------
c     DSTN1 requires a pair of random numbers (supplied by RANDN).  A scheme
c     of "reuse" was instituted in which the first random number for each
c     pair is generated at the beginning of the run.  Then during the run,
c     the second is generated; DSTN1 is called; and the second number replaces
c     the first.  For example to generate daily maximum temperature values,
c     initially v1 = randn(k2); then v2 = randn(k2); DSTN1 is called with
c     arguments v1 & v2; v1 = v2; and the loop repeats.  Each population of
c     random numbers is generated from its own set of seeds, making it progress
c     independently of the others, AND ensuring that a random sequence was
c     used for each parameter without omitting values.  (Note that subsequent
c     days for a given parameter are NOT independent of each other!)  Below is
c     a summary of the parameters ane their use.
c     -- CRM -- 4/25/2000.
c
c             Used_For:    Deviate(s):   RandomSeeds:
c            ----------------------------------------
c              MaxTemp      v1, v2           k2
c              MinTemp      v3, v4           k3
c             Radiation     v5, v6           k4
c              Precip       v7, v8           k5
c              WindVel      v9, v10          k8
c                TDP        v11, v12         k9
c            ----------------------------------------
c             PrecipProb    vv               k1
c               DSTG        rn1, rn          k7
c              WindDir      fx               k6
c             TimeToPeak    z                k10
c
c --------------------------------------------------------------------------
c      
c     Version 4.2 April 1997  West Lafayette, IN.
c     Correction of version numbers.
c     Multiple year generation, output file, and summary added
c     Dewpoint temperature added with wind speed and velocity
c     using mean standard deviation and skew coefficient.
c     Change made to place parameter in state files (i.e. al Alabama parms).
c     All parameters rainfall, temperature, radiation, dewpoint temperature,
c     wind speed and direction are combined into one set with 82 lines of data.
c     Storm duration calculation change (4.607 to 9.210).
c     Weighting factor removed from Maximum, Minimum, Dew Point Temperature and
c     Solar Radiation.
c     Addition of formatting to allow use of interpolated station file.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      write: pi2
c     pi2         - Pi * 2; ie, a full rotation.
c
      include 'cbk4.inc'
c      read: iopt
c      write: nt
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
      include 'cbk7.inc'
c      read: prw,k1,k2,k3,k4,k5,k7,k8,k9
c      write: v1,v3,v5,v7,v9,v11,yls,ylc,pit,nsim,msim,l
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - Set to one under option-6 if precip data is missing.
c     msim        - Set to one under option-6 if temperature data is missing.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
c
      include 'cbk5.inc'
c      write: sml
c     sml         - Used to compute R1
c
      include 'cbk9.inc'
c      write: ab1,rn1
c      modify: wi,ab
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c
      include 'crandom3.inc'
c      (Referenced here to "save" the variables until CLIGEN exits.)
c
      include 'command6.inc'
      include 'cinterp.inc'
c
c
c     + + + LOCAL VARIABLES + + +
CC    integer numarg
      character*512 argv
      integer ti(4)
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
      real smy(12),wgt(3),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      integer elev,years,moveto
      character*1 yc
c     character*6 nstat
      character*41 stidd
c
c     + + + LOCAL DEFINITIONS + + +
c     numarg      - number of arguments passed on the command line.
c     argv        - a specified arg from the list of command line arguments.
c     infile      - cligen db input file name (interactive mode)
c     odfile      - observed data file for option 6.
c     ti          - variable for setting random seed from system clock.
c     dohedr      - flag which tells whether to add a WEPS header.
c     yc          - 1-character user response (y/n).
c     outfil      - Cligen Output (.cli) file name.
c     xx          - latitude / 57.296; ie, in radians
c     vv          - random deviate (Probability of Precip today).
c     nt          - 0 or 1 ("leap year?" for jdt) (iopt = 4, 7)
c     r5max       - "max of monthly maximum .5-hr rain"
c
c      Variables Passed to other Modules:
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     damt        - Design Storm Amount in Inches for Single Storm.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     ioyr        - first # of "infile" (-> ibyear) (iopt 6)
c     itype       - integer value [1..4] to set single storm parameters.
c     iyear       - Beginning Simulation Year.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     numyr       - number of years to simulate
c     smy         - Observed Monthly Average Precipitation (mm)
c     stidd       - 41-character alphanumeric station name.
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     tp6         - maximum 6 hour precipitation depth (inches).
c     tymax(4)    - upper limit of r5p (based on itype)
c     usdur       - [User Supplied] Storm Duration in Hours for Single Storm.
c     ustpr       - [User Supplied] Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     version     - CLIGEN version (ie, 5.103)
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     xm          - number of days in the month of interest
c     years       - Years of Record at the Station.
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c
c     + + + SUBROUTINES CALLED + + +
c     sta_dat
c     r5mon
c     usr_opt
c     sing_stm
c     wxr_gen
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
c     + + + DATA INITIALIZATIONS + + +
      data tymax/180.34,154.94,307.34,330.2/
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/22x,'Average Values for ',i2,' Years'/)
 2010 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2020 format(1x,'prcp   ',12f6.2)
 2030 format(1x,'tmax   ',12f6.2)
 2040 format(1x,'tmin   ',12f6.2)
 2050 format(1x,'rad    ',12f6.1)
 2060 format(1x,'dur    ',12f6.2)
 2070 format(/1x,'Average Annual Precipitation for ',i2,
     1' Years =',f6.2,a30/)
 2080 format(/1x,'Do you want to continue (y/n)? ')
c
c     + + + END SPECIFICATIONS + + +
      version=5.3220
c
c *************************************************************
c **************** BEGIN COMPILER-SPECIFIC CODE ***************
c *************************************************************
c     NOTE: Functions to determine the number of command line
c           arguments, and to return the individual arguments
c           are specific to the compiler used.  For Lahey they
c           apparently do not exist.  The functions narg() and
c           argopt() are included here to correct that deficiency.
c           Several popular compilers are supported by this code.
c           Simply locate the compiler of choice and uncomment
c           the related lines.
c
c     NOTE: Verify that for your compiler the number of command
c           line arguments does not include the command itself;
c           ie, for a command without arguments, the value returned
c           is zero.  If it is "one", simply subtract one from 
c           "numarg".
c     
c
c ----Determine Number of Command Line Arguments.
c ---- UNIX (BSD(?)):
c     numarg = nargs()
c ---- Lahey:
c     numarg = narg()
c ---- Solaris (System-V UNIX), GNU, & Watcom:
      numarg = iargc()
c     write(*,*) 'NUMARG:', numarg
c
c ---- Process Each Command Line Option Specified.
c ----- *** L1 IF ***
      if (numarg .gt. 0) then
        do 09 i = 1, numarg
c ------Read Each Command Line Argument.
c ------ BSD(?) UNIX:
c         call getarg(i, argv, ii)
c ------ Lahey:
c         call argopt(i, argv)
c ------ Watcom:
c         ii = igetarg(i, argv)
c ------ Solaris (System-V UNIX) & GNU:
          call getarg (i, argv)
c *************************************************************
c ***************** END COMPILER-SPECIFIC CODE ****************
c *************************************************************
c
c *********** Append COMMAND LINE ARGUMENTS to arg_v **********
          if(i .ne. 1) then
c ----------- Append current argument to arg_v.
            write(arg_v(av_len+1:), '(a)') argv
c ----------- Find length of string.
            av_len = 1025
 1001       av_len = av_len - 1
            if(arg_v(av_len:av_len) .eq. ' ' .and. av_len .gt. 1) 
     1        goto 1001
            av_len = av_len + 1
          else
            write(arg_v, '(a)') argv
            av_len = 1025
 1002       av_len = av_len - 1
            if(arg_v(av_len:av_len) .eq. ' ' .and. av_len .gt. 1) 
     1        goto 1002
            av_len = av_len + 1
c ----------- Find length of string.
          endif

C     write(*,*) arg_v(1:av_len)
c
c ************ BEGIN PARSING COMMAND LINE ARGUMENTS ***********
c
c         Make sure all options start with '-'
          if(argv(1:1) .ne. '-') then
            write(*,*) 'Option ignored, no option flag: ', argv
c
c         State Number:
          else if(argv(2:2) .eq. 'S') then
            read(argv(3:),'(i2)') istate
c
c         Station Number:
          else if(argv(2:2) .eq. 's') then
            read(argv(3:),*) index
c
c         Random Seed Value:
          else if(argv(2:2) .eq. 'r') then
c ---------- use default seed values
            if(argv(3:).eq.'0') then
               continue
c ------- use totally random seeds (based on system clock)
            else if(argv(3:4).eq.'-1') then
C             call gettim(ti(1),ti(2),ti(3),ti(4))
C             ti(3)=ti(4)+ti(3)*7+ti(2)*5+ti(1)*3
C             ti(3)=ti(3)-(ti(3)/199)*199
C             if (ti(3).lt.0) ti(3)=0-ti(3)
C             do 2 j=1,ti(3)
C               fx=randn(k1)
C               fx=randn(k2)
C               fx=randn(k3)
C               fx=randn(k4)
C               fx=randn(k5)
C               fx=randn(k6)
C               fx=randn(k7)
C               fx=randn(k8)
C               fx=randn(k9)
 2            continue
c -------- use specified seed value (to discard the designated number of RN's)
            else
              read(argv(3:),*) irand
              do 3 j=1,irand
                fx=randn(k1)
                fx=randn(k2)
                fx=randn(k3)
                fx=randn(k4)
                fx=randn(k5)
                fx=randn(k6)
                fx=randn(k7)
                fx=randn(k8)
                fx=randn(k9)
 3            continue
            endif
c
c         Simulation Type:
          else if(argv(2:2) .eq. 't') then
            read(argv(3:),*) iopt
c
c         Beginning Year:
          else if(argv(2:2) .eq. 'b') then
            read(argv(3:),*) ibyear
c
c         Duration in Years:
          else if(argv(2:2) .eq. 'y') then
            read(argv(3:),*) numyr
c
c         Input File:
          else if(argv(2:2) .eq. 'i') then
            infile = argv(3:)
c
c         Output File:
          else if(argv(2:2) .eq. 'o') then
            outfil = argv(3:)
c
c         Force overwrite of output file:
          else if(argv(2:2) .eq. 'F') then
            force = 1
c
c         Header in Output File:
          else if(argv(2:2) .eq. 'H') then
            dohedr=.false.
c
          else if((argv(2:2) .eq. 'v') .or. (argv(2:2) .eq. 'V')) then
            write(*,*)
c           write(*,*) 'CLIGEN - Climate Generator V-4.2c August 2000'
c           write(*,*) 'CLIGEN - Climate Generator V-5.1 August 2000'
c           write(*,*) 'CLIGEN - Climate Generator V-5.101 Feb. 2001'
c           write(*,*) 'CLIGEN - Climate Generator V-5.102 Mar. 2001'
c           write(*,*) 'CLIGEN - Climate Generator V-5.103 Apr. 2001'
            write(*,"('CLIGEN - Climate Generator V-', f7.5,
     1                ' Sep. 2024')") version

            write(*,*) 'Modified to support Command Line Options.'
            write(*,*)
c
          else if(argv(2:2).eq.'?' .or. argv(2:2).eq.'h') then
            write(*,*)
            write(*,"(' CLIGEN V-', f7.5, ' - Climate Generator w/ QC-',
     1                'SNDG')") version
            write(*,*) '   Usage:'
            write(*,*) '    cligen -S<state number> -s<station ID number
     1>'
            write(*,*) '           -i<input file name> -o<output file na
     1me>'
            write(*,*) '           -b<beginning year> -y<duration in yea
     1rs>'
            write(*,*) '           -f (old WEPS record format)'
            write(*,*) '           -F (overwrite output file if it exist
     1s)'
            write(*,*) '           -H (omit header output) -r<random see
     1d>'
            write(*,*) '           -t<Sim Type (WEPP: 4=SglStm, 5=Contin
     1)>'
            write(*,*) '           -I0 <no interpolation (default)>'
            write(*,*) '           -I1 <linear interpolation>'
            write(*,*) '           -I2 <Fourier interpolation>'
            write(*,*) '           -I3 <interpolation to preserve avgs>'
            write(*,*) '           -v, -V<verbose> -h, -?, -\\? <help>'
            write(*,*) '           -O <option 6 -- observed data filenam
     1e>'
            write(*,*) 'Make sure there are no spaces between each flag'
            write(*,*) 'and its parameter.'
            write(*,*) 'If command line options are omitted, CLIGEN will
     1'
            write(*,*) 'interactively request the required information.'
            write(*,*)
            stop
          else if(argv(2:2) .eq. 'I') then
c
c-----------no interpolation
            if(argv(3:3) .eq. '0') then
              interp = 0
c-----------linear interpolation
            else if(argv(3:3) .eq. '1') then 
              interp = 1
c-----------fourier interpolation
            else if(argv(3:3) .eq. '2') then 
              interp = 2
c-----------ryf interpolation
            else if(argv(3:3) .eq. '3') then 
              interp = 3
            endif
c
c---------Observed Data File (option 6):
          else if(argv(2:2) .eq. 'O') then
            odfile = argv(3:)
c
          else
            write(*, '("Unknown option: ", a," ignored.") ') argv 
          endif
 09     continue
c
CC    write(*,*) " State:", istate, " Station:", index
CC    write(*,*) " Beg_Year:", ibyear, " Num_Yr:", numyr
CC    write(*,*) " Interpolation:", interp
c
c ----- *** L1 ENDIF ***
      endif
c ************* END PARSING COMMAND LINE ARGUMENTS ************
c
c
      timpkd(0)=0.0
      moveto = 0
c     Version number set here for option 5 output header
c     version=5.103
c
c ***************************************************************************
c ---- Determine the desired station and return its climate generation parms.
 10   continue
      moveto = 0
c     call sta_dat(ylt,yll,years,elev,itype,tp6,wgt,moveto,
      call sta_dat(ylt,yll,years,elev,itype,tp6,wgt,version,moveto,
     1            stidd,timpkd,igcode)
      if(moveto.eq.10) goto 10
c ***************************************************************************
c
c     Begin Climate Generation
c
c **** L1 IF ****
      if(moveto.ne.230) then
        sml=0.0
        r5max=0.0
        do 120 i=1,12
          if(wi(i).ge.r5max) r5max=wi(i)
 120    continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
C       do 125 i=1,12
C         wi(i)=wi(i)
C125    continue
c
c ------ Done Once per Run:
C       call r5mon(tp6)
        call r5monb
        ab=0.02083
        ab1=1.0-ab
        nt=0
        clt=57.296
        pit=58.13
        pi2=6.283185
        xx=ylt/clt
        yls=sin(xx)
        ylc=cos(xx)
        vv=randn(k1)
        l=2
        if(vv.gt.prw(1,1)) l=1
        rn1=randn(k7)
c
c ------ Initialize the 1st of 2 constants for std norm deviate generator.
        v1=randn(k2)
        v3=randn(k3)
        v5=randn(k4)
        v7=randn(k5)
        v9=randn(k8)
        v11=randn(k9)
c
        msim=1
        nsim=1
c
c ---- Get Options from User
      call usr_opt(moveto,ioyr)
c **** L1 ENDIF ****
      endif
c
c **** M1 IF ****
      if(moveto.ne.230) then
c
        call sing_stm(ioyr,moveto,jd,iyear,damt,usdur,ustpr,uxmav)
        call wxr_gen(version,igcode,stidd,ylt,yll,years,elev,
     1            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
CC   2            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     2            iyear,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     3            sumpp,sumptx,sumptm,sumprd,sumpdr)
c **** M1 ENDIF ****
      endif
c
c **** N1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230 .and. moveto.ne.225) then
        if(iopt.eq.2) then
          write(*,2000)numyr
          write(*,2010)
          write(*,2020)(sumpp(i),i=1,12)
          write(*,2030)(sumptx(i),i=1,12)
          write(*,2040)(sumptm(i),i=1,12)
          write(*,2050)(sumprd(i),i=1,12)
          write(*,2060)(sumpdr(i),i=1,12)
          write(*,2070)numyr,sumpp(13),stidd
        endif
c **** N1 ENDIF ****
      endif
c
c      End - MAIN LOOP
c
c    Check for Another Run or End
c
c **** P1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230) then
c ------ "Pure" Interactive Mode (No Command Line Arguments).
c       if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
        if(numarg.eq.0) then
          write(*,2080)
          read(*,'(a1)')yc
        else
          yc = 'N'
          moveto = 230
        endif
        if(yc.eq.'y'.or.yc.eq.'Y') then
	  moveto = 10
          istate = -1
          index  = -1
          ibyear = -1
          numyr  = -1
          iopt   = -1
          force  = 0
          outfil = 'XXX'
          infile = 'XXX'
          odfile = 'XXX'
          dohedr =.true.
          interp = 0
        else
          if(iopt.ge.4) then
            write(7,*) ' '
            close (7)
CC          close (72)
C           close (73)
          else if(iopt.eq.3) then
            write(8,*)' '
            close (8)
          endif
        endif
c **** P1 ENDIF ****
      endif
c
      if(moveto.eq.10) goto 10
      stop 'Normal program termination' 
      end
c
c 
c =============== BEGIN LAHEY-SPECIFIC CODE: =====================
c
c -- Don't use these two functions except with the Lahey compiler:
cc----< Modules written by WERU to support command line args with Lahey: >----
cc      Modified 7/11/2000 by C. R. Meyer to be more generic.
cc
cc This function returns the number of args/options on the commandline
cc It is Lahey lf95 PC compiler specific because it uses the getcl()
cc subroutine call - LEW
c 
c      function narg()
cc
c      integer narg
c      character*128 cmdline
c      integer i,j
cc       
c      narg = 0
cc------Lahey compiler specific
c      call getcl(cmdline)
c      j = 0
cc------find next '-'
c 10   continue
c        i = index(cmdline(j:), ' -')
cc       write(6,*) 'i,j,narg is: ', i,j,narg
c        if (i .ne. 0) then
c          narg = narg + 1
c          j = j+ i + 1
c        endif
c      if(i .ne. 0) goto 10
cc
c      return
c      end
cc
cc
cc This subroutine returns the argument/option specified by argcnt
cc from the commandline
cc cannot return argcnt = 1 (filename), therefore, we return ""
cc This is Lahey lf95 PC compiler specific because it uses the getcl()
cc subroutine call - LEW
cc      Modified 7/11/2000 by C. R. Meyer to be more generic.
cc
c      subroutine argopt(argcnt, option)
cc
c      integer argcnt
c      character*128 option
cc
c      integer narg
c      character*128 cmdline
c      integer i,j
cc
cc ---- If there are no parameters after the command name...
c      if (argcnt .eq. 0) then
c        option = ' '
cc ---- There are Parameters after the Command Name
c      else
c        narg = 0
c        j = 0
cc ---- Lahey compiler specific
c        call getcl(cmdline)
cc------Find Start of Desired Argument.
c 10     continue
cc----------find next '-'
c          i = index(cmdline(j:), ' -')
c          if (i .ne. 0) then
c            narg = narg + 1
c            j = j+ i + 1
c          endif
c        if (narg .ne. argcnt) goto 10
cc ------ Find End of Desired Argument.
c        i = index(cmdline(j:), ' -')
c        if (i .ne. 0) then
c          option = cmdline(j:j+i-1)
c        else
c          option = cmdline(j:)
c        endif
cc
c      endif
cc
c      return
c      end
c
c ================= END LAHEY-SPECIFIC CODE: =====================
c
c
