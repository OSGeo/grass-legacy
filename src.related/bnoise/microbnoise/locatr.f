C$NOFLOATCALLS
      SUBROUTINE LOCATR                                                 LOC    2
      CHARACTER*10  CHOICE,NAME,LOCATE
      CHARACTER*2  STARS
      CHARACTER*1  STAR,POINT,EOI
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS
        LOGICAL  BDS
C * LABLES FIRING POINTS AND TARGETS ON THE PLOT.  TARGET="X" FIRING PT.="0"
      COMMON/IO/KARD,KPRINT                                             LOC    3
C     POINT IS EITHER AN "X" FOR TARGETS OR AN "0" FOR FIRING 
C     POINTS.  STAR INDICATES EOF WHILE FLUSHING CARDS. 
C     FLAGS FOR LABELS ON OUTPUT
      LOGICAL TARG,FPT,NAM,LOC                                          LOC    5
C     ARRAY USED TO DENOTE DEFAULT VALUES 
      DIMENSION STARS(2)                                                LOC    6
C     IN1=TAPE 7  IOUT=TAPE 3 
      DATA IN1/7/,IOUT/3/                                               LOC    7
      STARS(1)='  '                                                     LOC    8
	STARS(2)='  '
      CPTIME=SECOND(CP)                                                 LOC    9
  
  
  
C WRITE TITLE OF MODULE "LOCATOR" 
      WRITE(KPRINT,25)                                                  LOC   10
 25   FORMAT(1H1,20X,'.....     LOCATOR      .....')                    LOC   11
  
      REWIND IOUT                                                       LOC   12
  
C PHASE 4 PLOTS SPECIAL LINES AND TEXT ON THE MAP AND DRAWS A BORDER ON THE MAP.
      WRITE(IOUT,10)                                                    LOC   13
 10   FORMAT(1h ,4HPHS4)
C                                                                       LOC   15
C        READ SPECIFICATION CARD                                        LOC   16
C                                                                       LOC   17
C THIS IS INFORMATION FOUND ONTHE LOC-2 CARD, 
      READ(KARD,20)CHOICE,NAME,LOCATE  ,SIZE,ANGLE                      LOC   18
 20   FORMAT(3A10,2F10.0)                                               LOC   19
C                                                                       LOC   20
C        SPECIFICATION CARD CHECKS                                      LOC   21
C                                                                       LOC   22
C THIS IF-LOOP CHECKS TO SEE IF A DEFAULT VALUE IS NEEDED FOR THE VARIABLE
C SIZE OR IF THERE IS ONE GIVEN BY THE USER,
      IF(SIZE.NE.0) GO TO 27                                            LOC   23
      SIZE=.14                                                          LOC   24
      STARS(1)='**'                                                     LOC   25
  
  
C THIS IF-LOOP CHECKS FOR THE DEFAULT FOR THE VARIABLE ANGLE
C IF A DEFAULT VALUE IS USED, IT IS DENOTED IN THE OUTPUT BY STARS. 
 27   IF(ANGLE.NE.0.) GO TO 29                                          LOC   26
      ANGLE=0.0                                                         LOC   27
      STARS(2)='**'                                                     LOC   28
  
  
C TARG,FPT,NAM, AND LOC ARE ALL LOGICAL VALUE TO SEE WHICH VALUES WERE
C SUBMITTED BY THE USER FOR THE TARGET, FIRING PT., NAME, AND LOCATE VARIABLES. 
 29   TARG=((CHOICE.EQ.'ALL').OR.(CHOICE.EQ.'TARGET'))                  LOC   29
      FPT=((CHOICE.EQ.'ALL').OR.(CHOICE.EQ.'FIRING'))                   LOC   30
      NAM=(NAME.EQ.'NAME')                                              LOC   31
      LOC=(LOCATE  .EQ.'LOCATION')                                      LOC   32
C                                                                       LOC   33
C HEADING"OPTIONS REQUESTED"
C        PRINT REPORT HEADING AND OPTIONS REQUESTED                     LOC   34
C                                                                       LOC   35
      WRITE(KPRINT,30)                                                  LOC   36
 30   FORMAT(1H0,23X,'..OPTIONS REQUESTED..'/)                          LOC   37
      IF((TARG).OR.(FPT)) GO TO 34                                      LOC   38
C MESSAGE"NO OPTIONS REQUESTED-JOB ENDED" 
      WRITE(KPRINT,32)                                                  LOC   39
 32   FORMAT(1H0,23X,'NO OPTIONS REQUESTED - JOB ENDED')                LOC   40
      REWIND IOUT                                                       LOC   41
      GO TO 250                                                         LOC   42
  
C THIS IF-STATEMENTS WRITE THE OPTIONS
 34   IF(TARG)WRITE(KPRINT,35) 'TARGET'                                 LOC   43
      IF(FPT) WRITE(KPRINT,35) 'FIRING POI','NT        '                          
      IF(NAM)  WRITE(KPRINT,35) 'NAME'                                  LOC   45
      IF(LOC)WRITE(KPRINT,35)  'LOCATION'                               LOC   46
 35   FORMAT(25X,2A10)                                                  LOC   47
      WRITE(KPRINT,40) SIZE,STARS(1),ANGLE,STARS(2)                     LOC   48
 40   FORMAT(1H0,25X,5HSIZE=,F5.2,A2,5X,6HANGLE=,F6.2,A2)               LOC   49
  
  
C MESSAGE "STARS INDICATE DEFAULT VALUES" 
      WRITE(KPRINT,45)                                                  LOC   50
 45   FORMAT(26X,'STARS INDICATE DEFAULT VALUES')                       LOC   51
      WRITE(KPRINT,36)                                                  LOC   52
 36   FORMAT(1H0,24X,7HID CODE,8X,8HX COORD.,5X,8HY COORD.,5X,7HG CORR.,
     15X,6HBOUNDS)
C     TEST IF BOUNDS SET?  NO THEN STOP 
      IF(BDS) GO TO 80
      WRITE(KPRINT,85)
85    FORMAT('  ...NO BOUNDS CALL BEFORE LOCATOR...') 
      STOP
C                                                                       LOC   54
C        READ GUN TYPES                                                 LOC   55
C                                                                       LOC   56
80    REWIND IN1                                                        LOC   57
 100  READ(IN1,110,END=260)STAR                                                 
 110  FORMAT(A1)                                                        LOC   59
      IF(STAR.NE.'*') GO TO 100                                         LOC   61
C IF WE HAVE A TARGET,PLACE AN 'X' ON THE POINT HEADING-"TARGET DATA" 
C                                                                       LOC   62
C        READ TARGET CARDS                                              LOC   63
      IF(TARG) GO TO 140                                                LOC   64
C     FLUSH TARGET CARDS
 120  READ(IN1,130,END=260) STAR                                                
 130  FORMAT(A1)                                                        LOC   66
      IF(STAR.NE.'*') GO TO 120                                         LOC   67
      WRITE(KPRINT,135)                                                 LOC   68
 135  FORMAT(1H0,23X,'TARGETS NOT REQUESTED')                           LOC   69
      GO TO 205                                                         LOC   70
C                                                                       LOC   71
C        TARGET/FIRING POINT CARDS PRODUCED                             LOC   72
C                                                                       LOC   73
140   POINT='X'                                                         LOC   74
      WRITE(KPRINT,141)                                                 LOC   75
 141  FORMAT(    11X,'..TARGET DATA..'/)                                LOC   76
 145  READ(IN1,150,END=250) EOI,IDCODE,XPOS,YPOS,GCORR                          
 150  FORMAT(A1,1X,A3,1X,3F6.0)                                         LOC   78
C     TEST IF POINT OUT OFN BOUNDS  IF SO FLAG
      IF (XPOS.LT.XMIN.OR.XPOS.GT.XMAX)  GOTO 164 
      IF (YPOS.LT.YMIN.OR.YPOS.GT.YMAX)  GOTO  164
      WRITE(KPRINT,160)IDCODE,XPOS,YPOS,GCORR                           LOC   81
 160  FORMAT(26X,A3,10X,F8.0,5X,F8.0,5X,F8.0)                           LOC   82
  
      GOTO  163 
C     OUT OF BOUNDS DATA,  TELL USER
164   WRITE(KPRINT,161)  IDCODE,XPOS,YPOS,GCORR 
161   FORMAT(26X,A3,5X,3(5X,F8.0),7X,3HOUT) 
      GOTO 200
  
C     L IS AN INPUT VARIABLE TO NASAPLOT
163   L=1                                                               LOC   83
      IF(NAM) GO TO 165                                                 LOC   84
      IDCODE='   '                                                      LOC   85
      GO TO 166                                                         LOC   86
 165  L=5                                                               LOC   87
 166  IF(LOC) GO TO 180                                                 LOC   88
      WRITE(IOUT,170)XPOS,YPOS,SIZE,ANGLE,POINT,IDCODE,L                LOC   89
  170 FORMAT(1h ,4HTEXT,2X,2F8.0,8X,2F8.3,2H 1,A1,1X,A3,19X,I2)
      GO TO 200                                                         LOC   91
 180  L=23                                                              LOC   92
      WRITE(IOUT,190)XPOS,YPOS,SIZE,ANGLE,POINT,IDCODE,XPOS,YPOS,L      LOC   93
  190 FORMAT(1h ,4HTEXT,2X,2F8.0,8X,2F8.3,2H 1,A1,1X,A3,1X,F8.0,1X,F8.0,
     11X,I2)
  
C     ARE WE DOING FIRING PTS?
  200 IF(POINT.EQ.'0') GO TO 206
C     DOING TARGETS--IS IS THE LAST ONE?
      IF(EOI.NE.'*') GO TO 145
C                                                                       LOC   96
C        FIRING POINT CARDS                                             LOC   97
C                                                                       LOC   98
C IF WE HAVE A FIRING POINT PLACE AN "0" ON THE POINT HEADING-FIRING POINT
C DATA" 
 205  IF(.NOT.FPT) GO TO 240                                            LOC   99
      WRITE(KPRINT,210)                                                 LOC  101
 210  FORMAT(    11X,'..FIRING POINT DATA..'/)                          LOC  102
      POINT='0'                                                         LOC  103
      GO TO 145                                                         LOC  104
  
C     FLUSH OUT FIRING POINT INFO FOR THIS FIRING PT. 
  206 READ(IN1,130,END=260) STAR
      IF(STAR.EQ.'*') GO TO 145 
      GO TO 206 
 240  WRITE(KPRINT,245)                                                 LOC  105
 245  FORMAT(1H0,23X,'FIRING POINTS NOT REQUESTED')                     LOC  106
  
  
C THIS COMPUTES THE TIME IN LOCATR SUBPROGRAM 
 250  CPTIM11=SECOND(CP)                                                LOC  107
      CPTIM11=CPTIM11-CPTIME                                            LOC  108
  
      WRITE(KPRINT,251)CPTIM11                                          LOC  109
 251  FORMAT(1H0,20X,'.....  TIME OF',F8.3,' SECONDS IN LOCATOR  .....')         
      GO TO 300                                                         LOC  111
C "ERROR-PREMATURE EOF ON DATA BASE FILE TAPE 3 NOT CREATED"
 260  WRITE(KPRINT,261)                                                 LOC  112
 261  FORMAT(1H0,20X,35H** PREMATURE EOF ON DATA BASE FILE  ,           LOC  113
     1                                             22H**TAPE3 NOT CREATELOC  114
     1D **)                                                             LOC  115
      REWIND IOUT                                                       LOC  116
      STOP                                                              LOC  117
  300 RETURN                                                            LOC  118
      END                                                               LOC  119
