C$NOFLOATCALLS
C$LARGE
      SUBROUTINE SCATPL(N,X,Y,SD)                                       SCATPL 2
C THIS ROUTINE PLOTS POINTS ACCORDING TO A NORMAL DISTRIBUTION AROUND THE 
C TARGET AREA.  (CALLED FROM SUBROUTINE SCATTER)
      COMMON/IO/KARD,KPRINT                                             SCATPL 3
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX                                  SCATPL 4
C     XD AND YD STORE THE COORDINATES OF THE DOTS 
C     NCALLS= NUMBER OF TIMES THE RANDOM NO. GENERATOR IS CALLED
C     NLEFT=NUMBER OF CALLS LEFT
      DIMENSION XD(1000),YD(1000),REP(1)                                SCATPL 5
      DATA IOUT/4/                                                      SCATPL 6
      DATA MAXRN/1000/,LIMOUT/5/,NREP/1/                                SCATPL 7
      DATA HGT/.1/,NVAR/1/,IVAR/1/,IPRNT/0/,IX/123456789/,IY/987654321/ SCATPL 8
C** ADDITIONAL DATA TO CONVERT TO BCS "RAND" (FROM NSRDC "NRAND")       DE100379
      DATA RNOPT/1/,FMX/16777213/,FMY/16777219/                         DE100379
C                                                                       DE100379
C VALUE OF VARIABLES%  IOUT=4  LIMOUT=5  HGT=.1  IVAR=1  IX=123456789 
C MAXRN=1000  NREP=1  NVAR=1  IPRNT=0  IY=987654321 
      NCALLS=(N-1)/MAXRN                                                SCATPL 9
C N IS THE VALUE SENT FROM SCATTER, WHILE PROCESSING ALL NOISE POINTS 
C FOR ONE NOISE SOURCE.  NCALLS IS THE INTEGER VALUE OF THIS DIVISION STATEMENT.
      NLEFT=(N-1)-NCALLS*MAXRN                                          SCATPL10
      NRN=MAXRN                                                         SCATPL11
C GENERATE A DOT AT THE NOISE SOURCE. 
      XDOT=X
      YDOT=Y
      WRITE(IOUT,1)XDOT,YDOT,HGT
      IF(N.LE.1)RETURN
   50 IF(NCALLS.GT.0)GO TO 100                                          SCATPL12
C     NO SOURCES LEFT-RETURN
      IF(NLEFT .LE. 0) RETURN 
      NRN=NLEFT                                                         SCATPL14
      NLEFT=0                                                           SCATPL15
C**************************************************                     DE100379
C LIBRARY ROUTINE NRAND                                                 SCATPL16
C GENERATES PSEUDO-RANDOM NOS. - NORMALLY DISTRIBUTED                   SCATPL17
C ..... SOURCE                                                          SCATPL18
C MATH SCIENCE LIBRARY -- CDC PROPRIETARY PRODUCT                       SCATPL19
C VOL. 7, P. 7-151                                                      SCATPL20
C .....PARAMETERS                                                       SCATPL21
C NRAND(N,M,I,XM,SIG,IU,X,IP)                                           SCATPL22
C  N   = TOTAL NO. RANDOM NOS. TO BE GENERATED                          SCATPL23
C  M   = TOTAL NO. VARIABLES IN DATA ARRAY X                            SCATPL24
C I   = RANDOM NOS. WILL BE STORED AS VARIABLE I IN THE MULTIPLEXED     SCATPL25
C        ARRAY X                                                        SCATPL26
C  XM  = MEAN VALUE                                                     SCATPL27
C  SIG = STANDARD DEVIATION                                             SCATPL28
C  IU = START MULTIPLIER -- MUST BE ODD                                 SCATPL29
C  IP  = PRINT INDICATOR -- .GT.0 - NOS. WILL BE PRINTED                SCATPL30
C  X   = MULTIPLEXED DATA ARRAY                                         SCATPL31
C**************************************************                     DE100379
C** ABOVE ROUTINE "NRAND" IS NO LONGER IN USE                           DE100379
C** HAS BEEN REPLACED WITH BCS ROUTINE "RAND"                           DE100379
C** WILL DOCUMENT IN FULL AT LATER DATE                                 DE100379
C                                                                       DE100379
C** OLD SUBROUTINE CALLS                                                DE100379
C*100 CALL NRAND(NRN,NVAR,IVAR,X,SD,IX,XD,IPRNT)                        SCATPL32
C*    CALL NRAND(NRN,NVAR,IVAR,Y,SD,IY,YD,IPRNT)                        SCATPL33
C                                                                       DE100379
C** NEW SUBROUTINE CALLS                                                DE100379
  100 CALL RAND(IX,FMX,RNOPT,NRN,XD)                                    DE100379
      CALL RAND(IY,FMY,RNOPT,NRN,YD)                                    DE100379

C** CONVERT NORMALLY DISTRIBUTED RANDOM NUMBERS W/ MEAN=0 AND 
C** SD=1, TO NORMALLY DISTRIBUTED RANDOM NUMBERS W/ MEAN=X(OR Y)
C** AND SD="SD" (INPUT TO SUBROUTINE THROUGH PARAM LIST). 
      DO 105 I=1,NRN
      XD(I)=SD*XD(I)+X
  105 YD(I)=SD*YD(I)+Y
C                                                                       DE100379
C KEEPS TRACK OF HOW MANY POINTS WE MUST COMPUTE. 
      NCALLS=NCALLS-1                                                   SCATPL34
      DO 200 I=1,NRN                                                    SCATPL35
      NOUT=0                                                            SCATPL36
      XDOT=XD(I)                                                        SCATPL37
C CHECK IF PT. IS WITHIN PLOT BOUNDARY                                  SCATPL38
  110 IF((XDOT.GE.XMIN).AND.(XDOT.LE.XMAX))GO TO 150                    SCATPL39
C XDOT OUT OF BOUNDS -- TRY AGAIN 2                                     SCATPL40
      NOUT=NOUT+1                                                       SCATPL41
      IF(NOUT.GT.LIMOUT)GO TO 190                                       SCATPL42
C                                                                       DE100379
C** OLD DOCUMENTATION                                                   DE100379
C**************************************************                     DE100379
C THE PARAMETERS SENT TO NRAND HAVE THE FOLLOWING USE IN THAT LIBRARY ROUTINE%
C     NRN=N=TOTAL NO. OF RANDOM NUMBERS TO BE GENERATED 
C     NVAR=M=TOTAL NO. OF VARIABLES IN DATA ARRAY X 
C     IVAR=I=RANDOM NOS. WILL BE STORED AS I IN ARRAY X.
C     X(Y)=XM=MEAN VALUE
C SD=SIG=STANDARD DEVIATION 
C XD(YD)=PRINT INDICATOR--GT. 0--NOS. WILL BE PRINTED.
C IX(IY)=START MULTIPLIER--MUST BE ODD. 
C IPRNT=X=MULTIPLEXED ARRAY 
C**************************************************                     DE100379
C                                                                       DE100379
C** OLD SUBROUTINE CALL                                                 DE100379
C*    CALL NRAND(NREP,NVAR,IVAR,X,SD,IX,REP,IPRNT)                      SCATPL43
C                                                                       DE100379
C** NEW SUBROUTINE CALL                                                 DE100379
      CALL RAND(IX,FMX,RNOPT,NREP,REP)                                  DE100379
C** CONVERT MEAN AND SD 
      REP(1)=SD*REP(1)+X
C                                                                       DE100379
      XDOT=REP(1)                                                       SCATPL44
      GO TO 110                                                         SCATPL45
  150 NOUT=0                                                            SCATPL46
      YDOT=YD(I)                                                        SCATPL47
  160 IF((YDOT.GE.YMIN).AND.(YDOT.LE.YMAX))GO TO 180                    SCATPL48
C YDOT OUT OF BOUNDS -- TRY AGAIN                                       SCATPL49
      NOUT=NOUT+1                                                       SCATPL50
C "WARNING--GENERATED SCATTER POINT FOR LOCATION (X,Y) OUT OF BOUNDS
C AFTER (LIMOUT) TRIES; POINT IGNORED.
      IF(NOUT.GT.LIMOUT)GO TO 190                                       SCATPL51
C                                                                       DE100379
C** OLD SUBROUTINE CALL                                                 DE100379
C*    CALL NRAND(NREP,NVAR,IVAR,Y,SD,IY,REP,IPRNT)                      SCATPL52
C                                                                       DE100379
C** NEW SUBROUTINE CALL                                                 DE100379
      CALL RAND(IY,FMY,RNOPT,NREP,REP)                                  DE100379
C** CONVERT MEAN AND SD 
      REP(1)=SD*REP(1)+Y
C                                                                       DE100379
      YDOT=REP(1)                                                       SCATPL53
      GO TO 160                                                         SCATPL54
C CREATE NASAPLOT TEXT CARD TO PUT DOT AT (XDOT,YDOT)                   SCATPL55
  180 WRITE(IOUT,1)XDOT,YDOT,HGT                                        SCATPL56
    1 FORMAT(1h ,'TEXT',2X,2F8.0,8X,F8.3,T48,'1.')
      GO TO 200                                                         SCATPL58
C UNRECOVERABLE OUT OF BOUNDS CONDITION                                 SCATPL59
  190 WRITE(KPRINT,191)X,Y,LIMOUT                                       SCATPL60
  191 FORMAT(6H0***** ,' WARNING -- GENERATED SCATTER POINT FOR LOCATIONSCATPL61
     1 (',F8.0,',',F8.0,') OUT OF BOUNDS AFTER',I3,' TRIES% PT. IGNORED'SCATPL62
     2)                                                                 SCATPL63
  200 CONTINUE                                                          SCATPL64
      GO TO 50                                                          SCATPL65
      END                                                               SCATPL67
