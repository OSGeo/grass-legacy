C$NOFLOATCALLS
C     PROGRAM LCDN(INPUT=128,OUTPUT=128,TAPE5=INPUT,TAPE6=OUTPUT, 
C    1TAPE7=128,TAPE8=512,TAPE1=128,TAPE2=128,TAPE3=128,TAPE4=128,
C    2 TAPE20=512,TAPE71=128,TAPE72=128,TAPE70=128,TAPE75=128,          ZNEF   4
C    3 TAPE90=128,TAPE91=128,TAPE99=128,TAPE51=128,TAPE55=128,          ZNEF   5
C    4 TAPE10)
	CHARACTER*1 CC
	CHARACTER*10 IMETER,IBOTH,HDR,HEAD
C     VARIABLES ADDED FOR GIS OUTPUT:
		LOGICAL GIS
		COMMON/GRASS/GIS
C     IO BLOCK CONTAINS VARIABLES KARD, WHICH REPRESENTS THE
C     CARD READER AND KPRINT, WHICH REPRESENTS THE LINE PRINTER 
      COMMON/IO/KARD,KPRINT
C     ARRAY HEAD WILL CONTAIN THE NAMES OF THE DIFFERENT
C     MODULES CALLED BY THE MAIN PROGRAM. 
      DIMENSION HEAD(11)                                                ZNEF   7
C     COMMON BLOCK BOUND CONTAINS THE MAXIMUM AND MINIMUM 
C     BOUNDS FOR THE X AND Y GRID COORDINATES AND THE LOGICAL 
C     FLAG BDS THAT CHECKS TO SEE IF THESE VALUES ARE ACCEPTABLE
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS                              ZNEF   8
      LOGICAL BDS,LARGE                                                 ZNEF   9
C     BLOCK PLOTCM CONTAINS INFORMATION WHICH IS COMPUTED 
C     IN SUBROUTINE PLOT AND USED BY THE NASAPLOT PROGRAM 
C     FOR CONTOUR MAKING. 
      COMMON/PLOTCM/PLTCT,SXTABLE,SYTABLE,LARGE                         ZNEF  10
C     PLTCT IS THE NUMBER OF PLOTS REQUESTED
      INTEGER PLTCT                                                     ZNEF  11
C     BLOCK SRCS CONTAINS THE NUMBER OF SOURCES.  IT IS 
C     USED IN FORMA, CALCNR, AND PGRID. 
      COMMON /SRCS/ NSRCS 
C     COMMON BLOCK FT CONTAINS INFORMATION CONCERNING 
C     THE LOCATIONS OF THE TARGETS AND FIRING POINTS, 
C     TOTAL NUMBER OF DAY AND NIGHT FIRINGS,(ALL OF WHICH 
C     ARE DECLARED ARRAYS), THE GRID SIZE GRDSZ, AND THE
C     CNR, WHICH IS THE DB VALUE AT A GIVEN POINT.
      COMMON/FT/GRDSZ,CNR,XLOC,YLOC,DAYNO,DARKNO,TEMPB                  ZNEF  13
C     TEMPB IS NEEDED TO ALLOW FOR SPACE IN THE MAPS SUBROUTINE 
      DIMENSION TEMPB(7000),TEMPA(5000) 

      DIMENSION XLOC(2000),YLOC(2000),DARKNO(2000),DAYNO(2000)          ZNEF  14
C     IN COMMON BLOCK GRID, NRCNR,EAS,RCNR,TEMP,AND DIST
C     ARE NO LONGER USED IN THE PROGRAM OR THE SUBROUTINES
C     ARRAY SDBWH STORES CAHRGE INFORMATION; ARRAYS ANGSIN, 
C     AND ANGCOS STORE SINE AND COSINE VALUES FOR THE ANGLE 
C     OF FIRING.
      COMMON/GRID/NRCNR,EAS,RCNR,DIST,SDBWH,              ANGCOS,ANGSIN 
     1  ,TEMP(4000),TEMPA 
C   TMPA IS NEEDED FOR EXTRA SPACE IN THE MAPS SUBROUTINE 
      DIMENSION RCNR(10),DIST(10),SDBWH(2000),
     1ANGCOS(2000),ANGSIN(2000)                                         ZNEF  17
      COMMON/DEBUG/CHECK,REED,TABRD                                     ZNEF  20
      COMMON/PARM/THRESH,PENITE,PPIP,IMP
C     LOGICAL FLAGS%  CHECK=FLAG FOR GROUND CORR FACTOR 
C     ON NEF-1 CARD; REED=FLAG FOR READIN;TABRD=FLAG FOR READTB.
      COMMON/METRIC/METERS,IMETER,IBOTH                                 ZNEF  21
      LOGICAL METERS                                                    ZNEF  22
C     DAYCNR AND DARKCNR ARE LOGICAL FLAGS THAT DENOTE
C     IF WE ARE COMPUTING DAYTIME OR NIGHTTIME DATA.
      COMMON/CALC/DAYCNR,DRKCNR                                         ZNEF  23
      LOGICAL DAYCNR,DRKCNR                                             ZNEF  24
C     THIS COMMON BLOCK(ANGLE) IS NO LONGER USED. 
C     COMMON/ANGLE/COSC,SINC                                            ZNEF  25
C     DIMENSION COSC(72),SINC(72)                                       ZNEF  26
      LOGICAL CHECK,REED,TABRD                                          ZNEF  27
C     STP IS A LOGICAL FLAG THAT IS SOMETIMES USED TO INVOKE THE
C     STOPP ROUTINE.
      LOGICAL STP                                                       ZNEF  29
C     KARD = TAPE 5(INPUT) AND KPRINT = TAPE 6(OUTPUT)
C     DATA KARD/5/,KPRINT/0/                                            ZNEF  30
C     DATA GRDSZ,EAS/5000.0,0.500/
C     INITIALIZE ARRAY HEAD TO MODULE NAMES 
      DATA HEAD/ 'MAP       ', 'FORM-A    ', 'POINT     ', 'PUDDLE GRI',
     1 'PUDDLEGRID', 'PLOT      ', 'BASE      ', 'SCATTER   ',
     2 'STOP      ', 'LOCATOR    ', 'BOUNDS     '/                      ZNEF  36
     3 NP/11/ 
	DATA I55/0/
	CP1=SECOND(CP)
	KARD=15
	GRDSZ=5000.0
	EAS=0.500
C     INITIALIZE FLAGS AND COUNTERS 
	CALL FILEOPEN
	CC=CHAR(15)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 6/18/88 commenting out the next write statement as it causes a
C control character to be printed in the output file (^O) which
C hangs during a 'more' of the file...
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	WRITE(KPRINT,10)CC
10	FORMAT(1H\,A1)
      STP=.FALSE.                                                       ZNEF  39
      REED=.FALSE.                                                      ZNEF  40
      TABRD=.FALSE.                                                     ZNEF  41
      CHECK=.FALSE.                                                     ZNEF  42
      LARGE=.FALSE. 
C 
	DAYCNR=.TRUE.
      DRKCNR=       .TRUE.                                              ZNEF  43
      BDS=.FALSE.                                                       ZNEF  44
      XMIN=0.                                                           ZNEF  45
      YMIN=0.                                                           ZNEF  46
      XMAX=0.                                                           ZNEF  47
      YMAX=0.                                                           ZNEF  48
      PLTCT=0                                                           ZNEF  49
      SXTABLE=0.                                                        ZNEF  50
      SYTABLE=0.                                                        ZNEF  51
  
 
C THE PROGRAM READS THE FIRST CARD OF THE MODULE INPUT, WHICH CONTAINS THE UNITS
C USED IN COMPUTATION(METERS OR FEET).  THE FIRST PAGE OF OUTPUT STATES 
C "DISTANCE EXPRESSED IN METERS"
C     READ(KARD,700)HDR,ICHECK,LCHECK,TI,PI,PPI,IMP                     ZNEF  59
      READ(KARD,700)HDR
C     IMPULSE FACTOR INSERTED BY PAUL SCHOMER  10 JAN 80
      PPIP=10**(PPI/10) 
C     IF THE UNIT METERS IS REQUESTED FOR USE, SET APPROPRIATE FLAG 
      METERS=(HDR .EQ.  'METERS    ')                                   ZNEF  60
C      CHECK FOR THE AVAILABILITY OF LARGE PAPER
      LARGE=(LCHECK .GT. 0) 
C   SET PARAMETERS IF REQUESTED.
      THRESH=85.0 
      PENITE=10.0 
      IF(TI .NE. 0) THRESH=TI 
      IF(PI .NE. 0) PENITE=PI 
      CHECK=(ICHECK .GT. 0)                                             ZNEF  61
C     UNITS COULD BE IN FEET
      IMETER= 'FEET      '                                              ZNEF  62
C     IF THE METERS FLAG IS TRUE, THEN WE USE METERS. 
      IF (METERS) IMETER= 'METERS    '                                  ZNEF  63
C     USING DAYTIME AND NIGHTTIME DATA
      IBOTH= 'BOTH D + N'                                               ZNEF  64
      WRITE(KPRINT,800)IMETER                                           ZNEF  65

C*   ECHO SPECIAL PARAMETERS FOR TESTING
      WRITE(KPRINT,810) THRESH,PENITE,PPIP,IMP
  
  
C THE ARRAY HEAD(15) CONTAINS THE NAMES OF THE MODULES.  THEREFORE, THE IF-GO-
C TO STATEMENT RELATES TO THAT TEST FOR EACH MODULE NAME. 
 100  READ(KARD,700,END=300) HDR                                                
C     THIS LOOP READS EACH MODULE NAME AND THE MULTIPLE GO TO SENDS 
C     THE PROGRAM TO THE APPROPRIATE CALL STATEMENT.
      DO 150 I=1,NP                                                     ZNEF  68
      IF (HDR .EQ. HEAD(I)) GO TO 200                                   ZNEF  69
 150  CONTINUE                                                          ZNEF  70
C     AN ILLEGAL CALL WAS ATTEMPTED 
      WRITE(KPRINT,701) HDR                                             ZNEF  71
      STOP                                                              ZNEF  72
  
C     JUMP TO APPROPRIATE CALL ACCORDING TO THE VALUE OF I
200   GOTO (210,220,225,230,230,240,250,260,310,320,330) , I
  
  
210	CONTINUE
	OPEN(7,FILE='TAPE7.DAT',IOSTAT=IOC)
 	OPEN(75,FILE='TAPE75.DAT',IOSTAT=IOC)
 	OPEN(70,FILE='TAPE70.DAT',IOSTAT=IOC)
	CALL MAP
	CLOSE(7)
      GO TO 100                                                         ZNEF  76
220	CONTINUE
	OPEN(7,FILE='TAPE7.DAT')
	OPEN(20,FILE='TAPE20.DAT',
     1FORM='UNFORMATTED')
	OPEN(8,FILE='TAPE8.DAT',FORM='UNFORMATTED',STATUS='NEW')
	CALL FORMA
	CLOSE(7)
	CLOSE(20)
	CLOSE(8)
      GO TO 100                                                         ZNEF  78
225	CONTINUE
	OPEN(8,FILE='FILE8.DAT',FORM='UNFORMATTED')
	OPEN(20,FILE='TAPE20.DAT',
     1FORM='UNFORMATTED')
	CALL POINT
	CLOSE(8)
	CLOSE(20)
      GO TO 100                                                         ZNEF  80
230	CONTINUE
	OPEN(8,FILE='TAPE8.DAT',FORM='UNFORMATTED')
	OPEN(20,FILE='TAPE20.DAT',
     1FORM='UNFORMATTED')
	OPEN(1,FILE='TAPE1.DAT',STATUS='NEW')
	CALL PGRID
	CLOSE(8)
	CLOSE(20)
	CLOSE(1)
C   no further processing needed at this point, so terminate program
      GO TO 300
C     GO TO 100                                                         ZNEF  83
240	CONTINUE
	IF(I55.EQ.0)OPEN(55,FILE='TAPE55.DAT',STATUS='NEW')
	I55=1
	CALL PLOT
      GO TO 100                                                         ZNEF  85
250	CONTINUE
	OPEN(2,FILE='TAPE2.DAT',STATUS='NEW')
	CALL BASE
	CLOSE(2)
      GO TO 100                                                         ZNEF  87
260	CONTINUE
	OPEN(7,FILE='TAPE7.DAT')
	OPEN(4,FILE='TAPE4.DAT',STATUS='NEW')
	CALL SCATTR
	CLOSE(4)
	CLOSE(7)
      GO TO 100                                                         ZNEF  89
  
C CLOSE NASAPLOT INPUT FILE                                             ZNEF 114
310	CONTINUE
	IF(I55.EQ.0)  OPEN(55,FILE='TAPE55.DAT',STATUS='NEW')
	CALL STOPP
      STP=.TRUE.                                                        ZNEF 116
      GO TO 100                                                         ZNEF 117
320	CONTINUE
	OPEN(7,FILE='TAPE7.DAT')
	OPEN(3,FILE='TAPE3.DAT',STATUS='NEW')
	CALL LOCATR
	CLOSE(7)
	CLOSE(3)
      GO TO 100    
  330 CALL BOUNDS 
      GO TO 100  
 300  CONTINUE  
C CALL PLOT TERMINATION ROUTINE IF NOT PREVIOUSLY CALLED            
      IF(.NOT.STP)CALL STOPP 
C	CP2=SECOND(CP)
C	CP=TIMEDIF(CP2,CP1)/60.
C	WRITE(0,699)CP
C699	FORMAT(/' TIME IN LCDN IS ',F6.1,' MINUTES')
C     STOP  
 700  FORMAT(A10,2I1,8X,3F10.0,I2)                                     
 701  FORMAT(1H1,///,'.....ERROR..... NAME = 10H',A10,' IS NOT A LEGAL C
     1ALL TO NEF MAIN PROGRAM.....',//)                              
  800 FORMAT(1H1///T20,'.....    BNOISE3.2        .....'/////          
     121X,'DISTANCES EXPRESSED IN ',A10)                                

C*   FORMAT FOR TEST ECHO 
  810 FORMAT(///1H ,10X,'THRESH',5X,F5.1/1H ,10X,'PENITE',5X,F5.1/1H ,
     1 10X,'PPIP',E12.4/1H ,10X,'IMP',10X,I2) 
         END
