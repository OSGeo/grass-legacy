C$NOFLOATCALLS
      SUBROUTINE POINT
	CHARACTER*1  IFLAG
	CHARACTER*10  X3,IBOTH,NAME
C     SEE ZNEF FOR THE DESCRIPTION OF THESE VARIABLES 
      COMMON/IO/KARD,KPRINT                                             PGRID  3
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS                              PGRID  4
      LOGICAL BDS                                                       PGRID  5
      COMMON /SRCS/ NSRCS 
      COMMON/FT/GRDSZ,CNR,XLOC,YLOC,DAYNO,DARKNO                        PGRID  7
      DIMENSION XLOC(2000),YLOC(2000),DARKNO(2000),DAYNO(2000)          PGRID  8
      COMMON/FACTI/ RINV1,RINV2,RINV3 
      COMMON/DEBUG/CHECK,REED,TABRD                                     PGRID 10
      COMMON/CALC/DAYCNR,DRKCNR                                         PGRID 11
      COMMON/METRIC/METERS,IMETER,IBOTH                                 PGRID 12
      LOGICAL METERS                                                    PGRID 13
      LOGICAL DAYCNR,DRKCNR                                             PGRID 14
      LOGICAL OUT 
C     FLAGS FOR READING TAPE8 INFO(REED), TAPE20 INFO(TABRD)
      LOGICAL CHECK,REED,TABRD                                          PGRID 15
      T100=SECOND(CP  )                                                 PGRID 22
      WRITE(KPRINT,610)                                                 PGRID 23
C IF THE DATA IS NOT ALREADY READ IN FOR USE IN POINT, THEN CALL SUBROUTINE 
C READIN TO READ THE DATA FROM TAPE 8.
      IF (.NOT. REED) CALL READIN                                       PGRID 24
C IF GRID BOUNDARIES ARE NOT PROPERLY INITIALIZED, COMPLAIN.
C GRID BOUNDARIES NOT PROPERLY INITIALIZED -- ABORT                     PGRID 25
      IF(BDS) GO TO 10
      WRITE(KPRINT,5) 
5     FORMAT('  ... NO BOUNDS CALL BEFORE POINT CALL...') 
      STOP
C READ INPUT DIRECTIVE,SET DEFAULTS,IF NECESSARY                        PGRID 28
C THIS READS IN THE CARD CONTAINING THE INVERSION FACTORS, AND DAY/NIGHT/BOTH 
10    READ(KARD,700,END=300) X1,X2,X4,X3
C ERROR MESSAGE%  ERROR--MISSING INPUT DIRECTIVE; JOB ABORTED 
C INVERSION FACTORS                                                     PGRID 31
C THIS SECTION READS, VERIFIES OR FINDS OUT ABOUT THE INVERSION FACTORS USED
C FROM SUBROUTINE READTB OR THE DIRECTIVE CARD. 
      IF(.NOT.TABRD)GO TO 50                                            PGRID 32
      IF(X1.EQ.RINV1.AND.X2.EQ.RINV2.AND.X4.EQ.RINV3)   GOTO 75 
      TABRD=.FALSE.                                                     PGRID 34
   50 RINV1=X1                                                          PGRID 35
      RINV2=X2                                                          PGRID 36
      RINV3= X4 
      CALL READTB                                                       PGRID 37
   75 CONTINUE                                                          PGRID 38
C DAY +/OR NIGHT CONSIDERED                                             PGRID 42
      DAYCNR=(X3.EQ. 'BOTH       '.OR. X3.EQ.'DAY       '    )          PGRID 43
      DRKCNR=(X3.EQ. 'BOTH       '.OR. X3.EQ. 'NIGHT     '   )          PGRID 44
      IF(DAYCNR.OR.DRKCNR)GO TO 90                                      PGRID 45
      DAYCNR=.TRUE.                                                     PGRID 46
      DRKCNR=.TRUE.                                                     PGRID 47
   90 CONTINUE                                                          PGRID 48
      IF (DRKCNR) IBOTH= 'NIGHT ONLY'                                   PGRID 49
      IF (DAYCNR) IBOTH= '  DAY ONLY'                                   PGRID 50
      IF (DAYCNR.AND.DRKCNR) IBOTH= 'BOTH D + N'                        PGRID 51
C PRINT PARAMETERS                                                      PGRID 60
      WRITE(KPRINT,620) RINV1,RINV2,RINV3 
      WRITE(KPRINT,622) IBOTH                                           PGRID 64
  
C     LOOP THUR THE DATA CARDS UNTILL A STAR IS READ
      WRITE(KPRINT,599) 
      J=0 
150   CONTINUE
      WRITE(KPRINT,600) 
      DO 200 I=1,30 
      J=J+1 
      READ(KARD,601,END=3000) IFLAG,NAME,X,Y 
C     TEST IF THERE WAS A CARD
C TEST IF POINT IS IN BOUNDS
      OUT=.FALSE. 
      IF (X.LT.XMIN.OR.X.GT.XMAX) OUT=.TRUE.
      IF (Y.LT.YMIN.OR.Y.GT.YMAX) OUT=.TRUE.
C THIS CALLS CALCNR TO COMPUTE THE LCDN FOR GRID COORDINATES X AND Y AND STORES 
C THE VALUES AS VNEF, OR CNR. 
      CALL CALCNR(X,Y)
      IF (CNR.GT.99.) CNR=99. 
      IF (OUT) WRITE(KPRINT,602) J,NAME,X,Y,CNR 
      IF (.NOT.OUT) WRITE(KPRINT,603) J,NAME,X,Y,CNR
      WRITE(10,800) CNR 
      IF (IFLAG.EQ.'*')  GOTO 400 
200   CONTINUE
      WRITE(KPRINT,640) 
      GOTO 150
  
400   CONTINUE
C THIS CALCULATES THE TIME SPENT IN POINT 
      T99= SECOND(CP )                                                  PGRID132
      T99=T99-T100                                                      PGRID133
      WRITE(KPRINT,900) T99                                             PGRID134
      RETURN                                                            PGRID135
C     EARLY END OF FILE 
3000  WRITE(KPRINT,699) 
      STOP
C EOF EXIT                                                              PGRID136
  300 WRITE(KPRINT,698)                                                 PGRID137
      STOP                                                              PGRID138
599   FORMAT(///3H    ) 
600   FORMAT(11X,4HNAME,9X,1HX,9X,1HY,8X,5HLEVEL  ) 
601   FORMAT(1A1,1A9,2F10.0)
602   FORMAT(1h ,I5,4X,1A9,2F10.0,F10.2,5X,13HOUT OF BOUNDS  )
603   FORMAT(1h ,I5,4X,1A9,2F10.0,F10.2)
 610  FORMAT(1H1,///,T15,'.....        POINT        .....') 
620   FORMAT(//T16,'  INVERSION =', 3F8.2 //) 
 622  FORMAT(T12,'CALCULATIONS FOR NEF WILL USE ',A10,' OF DAY AND NIGHTPGRID156
     1 CALCULATIONS',//)                                                PGRID157
640   FORMAT(1H1) 
  698 FORMAT(///   56H *****     ERROR -- MISSING INPUT DIRECTIVE; JOB APGRID160
     1BORTED  )                                                         PGRID161
699   FORMAT(///   56H *****     ERROR -- EARLY END OF FILE ; JOB ABO 
     1RTED      ) 
700   FORMAT(3F10.0,A10)
  800 FORMAT(1h ,F10.2) 
900   FORMAT(//,T15,'.....   TIME FOR THE POINT SUBPROGRAM IS ',F8.3
     1' SECONDS')                                                       PGRID165
      END                                                               PGRID166
