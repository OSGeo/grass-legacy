      SUBROUTINE PGRID                                                  PGRID  2
	CHARACTER*10 X3,GRDNAME,IBOTH,IMETER
C SETS UP A GRID OF LCDN VALUES FOR A SPECIFIED AREA. 
C     SEE ZNEF FOR THE DESCRIPTION OF THESE VARIABLES 
		LOGICAL GIS
		COMMON/GRASS/GIS
      REAL GARRAY(1:500)
		INTEGER GINDX,GMAX
      COMMON/IO/KARD,KPRINT
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
C     FLAGS FOR READING TAPE8 INFO(REED), TAPE20 INFO(TABRD)
      LOGICAL CHECK,REED,TABRD                                          PGRID 15
C     FLAG TO DENOTE WHETHER OR NOT THE ORIGINAL BOUNDS WERE CHANGED
      LOGICAL DELBDS 
C     ARRAYS FOR LABELS(IL,JL) XARRY CONTAINS LCDN VALUES
      DIMENSION IL(500),JL(500),XARRY(500)           
      EQUIVALENCE (JL(1),XARRY(1))                  
C     DEFGSZ=DEFAULT GRID SIZE
C     KOLS=MAX. NUMBER OF COLUMNS 
C     GMULT=MULTIPLE VALUE
C     TOL = TOLERENCE 
      DATA DEFGSZ/2000./,KOLS/500/,GMULT/50./,TOL/.01/
C     NSKIP AIDS IN SETTING UP TABLE FOR OUTPUT 
      DATA BLANK/' '/,ZERO/'0'/,NSKIP/5/,GMAX/500/
C     IOUT=TAPE1
      DATA IOUT/1/                                                      PGRID 21
      T100=SECOND(CP  )                                                 PGRID 22
      WRITE(KPRINT,610)                                                 PGRID 23
C IF THE DATA IS NOT ALREADY READ IN FOR USE IN PGRID, THEN CALL SUBROUTINE 
C READIN TO READ THE DATA FROM TAPE 8.
      IF (.NOT. REED) CALL READIN                                       PGRID 24
C IF GRID BOUNDARIES ARE NOT PROPERLY INITIALIZED, COMPLAIN.
C GRID BOUNDARIES NOT PROPERLY INITIALIZED -- ABORT                     PGRID 25
      IF(BDS) GO TO 10
      WRITE(KPRINT,5) 
5     FORMAT('  ...NO BOUNDS CALL BEFORE PUDDLE GRID CALL...')
      STOP
C READ INPUT DIRECTIVE,SET DEFAULTS,IF NECESSARY                        PGRID 28
C THIS READS PGRID-2 CARD CONTAINING THE INVERSION FACTORS, GRID SIZE, AND DAY
C AND/OR NIGHT. 
10    READ(KARD,700,END=300) X1,X2,X4,GRDSZ,X3,GRDNAME
C ERROR MESSAGE:  ERROR--MISSING INPUT DIRECTIVE; JOB ABORTED 
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
C GRID SIZE                                                             PGRID 39
C IF THERE IS NO GRID SIZE ON THE DIRECTIVE CARD, THEN IT ASSUMES A DEFAULT 
C VALUE (HERE DEFGSZ=2000)
      IF(GRDSZ.LE.0.) GRDSZ=DEFGSZ                                      PGRID 40
C THIS IS A WARNING STATING THAT THE INPUT GRID SIZE IS NOT A MULTIPLE OF SOME
C NUMBER,GMULT.  (HERE, GMULT=50)
      IF(AMOD(GRDSZ,GMULT).GT.TOL)WRITE(KPRINT,697)GRDSZ,GMULT          PGRID 41
C DAY +/OR NIGHT CONSIDERED                                             PGRID 42
      DAYCNR=(X3.EQ. 'BOTH      '.OR. X3.EQ. 'DAY       '   )           PGRID 43
      DRKCNR=(X3.EQ. 'BOTH      ' .OR. X3.EQ. 'NIGHT     '   )          PGRID 44
      IF(DAYCNR.OR.DRKCNR)GO TO 90                                      PGRID 45
      DAYCNR=.TRUE.                                                     PGRID 46
      DRKCNR=.TRUE.                                                     PGRID 47
   90 CONTINUE                                                          PGRID 48
      IF (DRKCNR) IBOTH= 'NIGHT ONLY'                                   PGRID 49
      IF (DAYCNR) IBOTH= '  DAY ONLY'                                   PGRID 50
      IF (DAYCNR.AND.DRKCNR) IBOTH= 'BOTH D + N'                        PGRID 51
C THE NEXT FEW LINES CHECK TO MAKE SURE EACH BOUND SUBMITTED BY THE USER IS 
C CORRECT BY CALLING SUBROUTINE BDSET.
      PGXMIN=XMIN 
      PGYMIN=YMIN 
      PGXMAX=XMAX 
       PGYMAX=YMAX
      DELBDS=.FALSE.                                                    PGRID 55
C THESE CALLS SEND BOTH THE MINIMUM AND MAXIMUM BOUNDARY VALUES TO
C BDSET SO AS TO FIND THE AREA THEY EMCOMPASS.
      CALL BDSET(PGXMIN,PGXMAX,GRDSZ,NJ,DELBDS) 
      CALL BDSET(PGYMIN,PGYMAX,GRDSZ,NI,DELBDS) 
C PRINT PARAMETERS                                                      PGRID 60
C IF THE FLAG IS TRUE THEN THE BOUNDS ARE MODIFIED,  IF NOT, PGRID WILL USE 
C THE PRESENT BOUNDS. 
      IF(DELBDS) WRITE(KPRINT,612) XMIN,YMIN,XMAX,YMAX                  PGRID 61
      WRITE(KPRINT,611)PGXMIN,PGYMIN,PGXMAX,PGYMAX                      PGRID 62
      WRITE(KPRINT,620) RINV1,RINV2,RINV3 
      WRITE(KPRINT,622) IBOTH                                           PGRID 64
      WRITE(KPRINT,621) GRDSZ,IMETER                                    PGRID 65
C NASAPLOT PROGRAM INPUT -- WRITE HDR INFO                              PGRID 80
      REWIND IOUT                                                       PGRID 81
      WRITE(IOUT,701)GRDNAME,GRDSZ,PGXMIN,PGYMIN,PGXMAX,PGYMAX,IMETER   PGRID 82
      WRITE(IOUT,605)                                                   PGRID 83
      WRITE(IOUT,603)NI,NJ                                              PGRID 84
C ADD ONE TO DIFFERENCES TO GET ACTUAL ROW,COL. COUNTS FOR LOOPING;     PGRID 85
C   NASAPLOT ROWS,COLS = ACTUAL -1                                      PGRID 86
      NI=NI+1                                                           PGRID 87
      NJ=NJ+1                                                           PGRID 88
C STORE ROW LABELS                                                      PGRID 89
      IGRDSZ=GRDSZ                                                      PGRID 90
C THIS LOOP INITIALIZES THE IL ARRAY TO THE ROW LABELS USED IN SETTING UP THE 
C GRID. 
      IL(1)=PGYMAX                                                      PGRID 91
      DO 130 I=2,NI                                                     PGRID 92
C     CHECK FOR OVERFLOW
      IF(I .GT. 500) GO TO 3000 
130   IL(I)= IL(I-1) - IGRDSZ 
C IXP = STARTING X VALUE FOR CURRENT PAGE                               PGRID 94
      IXP=0 
C
C IF GIS OUTPUT THEN WRITE ABBREVIATED VERSION OF TAPE1.DAT
		IF (GIS) GO TO 1140
C
C ELSE PERFORM THIS LOOP TO WRITE FULL LENGTH TAPE1.DAT
C
C     LOOP HERE FOR NEXT PAGE 
  140 IX=IXP                                                            PGRID 96
      IY=NI-1 
      LINECT=0                                                          PGRID 98
      CC=ZERO                                                           PGRID 99
C SET UP PAGING                                                         PGRID100
      NJP=NJ                                                            PGRID101
      IF(NJ.LE.KOLS)GO TO 150                                           PGRID102
      NJP=KOLS                                                          PGRID103
  150 CONTINUE                                                          PGRID104
C PRINT COL. LABELS FOR THIS PAGE                                       PGRID105
C THIS LOOP INITIALIZES THE JL ARRAY TO THE COLUMN LABLES USED IN SETTING UP
C THE GRID.  NEXT, IT PRINTS ALL OF THE COLUMN LABELS.
      JL(1)=PGXMIN+ IX*GRDSZ
      DO 160 J=2,NJP                                                    PGRID107
      IF(J .EQ. 500) GO TO 3000
160   JL(J)=JL(J-1) + IGRDSZ
C***********************************************************************
C  MODIFIED 2/10/87 
C  PRINTING OF GRID VALUES TO OUTPUT HAS BEEN SUPPRESSED TO SPEED UP
C  THE ALGORITHM.  USER CAN LOOK IN EITHER TAPE1 OR TAPE55 FOR THESE VALUES
C***********************************************************************
C     WRITE(KPRINT,600) (JL(J),J=1,NJP)                                 PGRID109
C THIS DO-LOOP CALCULATES THE VALUES ON THE GRID FOR X AND Y TO BE SENT TO
C SUBROUTINE CALCNR TO COMPUTE THE LCDN VALUES. 
      DO 180 I=1,NI                                                     PGRID110
C     THIS SETS UP THE ROW LABEL (Y COORDINATE) USED BY NASAPLOT
      II=NI-I+1                                                         PGRID111
      DO 170 J=1,NJP                                                    PGRID112
C THIS SETS UP THE COLUMN LABEL (OR X COORDINATE USED). 
      X=IX*GRDSZ+PGXMIN 
      Y=IY*GRDSZ+PGYMIN 
C THIS CALLS CALCNR TO COMPUTE THE LCDN FOR GRID COORDINATES X AND Y AND STORES 
C THE VALUES AS VNEF, OR CNR. 
      CALL CALCNR(X,Y)
C     VNEF,II,JJ--INPUT TO NASAPLOT 
	VNEF=CNR
	XARRY(J)=CNR
      IF(VNEF.GT.80.)VNEF=80.                                           PGRID116
      JJ = IX+1 
		WRITE(IOUT,604) II,JJ,VNEF
170   IX=IX+1 
      IF(MOD(LINECT,NSKIP).EQ.0)CC=ZERO                                 PGRID119
C THIS PRINTS OUT THE LCDN VALUES FOR THE PUDDLE GRID OUTPUT. 
C suppress printing of these values to user because it takes too long
C     WRITE(KPRINT,601)CC,IL(I),(XARRY(J),J=1,NJP)                      PGRID120
      LINECT=LINECT+1                                                   PGRID121
      CC=BLANK                                                          PGRID122
      IY=IY-1 
      IX=IXP                                                            PGRID124
  180 CONTINUE                                                          PGRID125
      NJ=NJ-NJP                                                         PGRID126
C ANOTHER PAGE? 
C THIS CHECKS TO SEE IF THERE IS A NEED FOR ANOTHER PAGE OF OUTPUT SINCE THERE
C IS A LIMIT PER PAGE OF OUTPUT.
      IF(NJ.LE.0)GO TO 200                                              PGRID128
      IXP=IXP+NJP                                                       PGRID129
C     START A NEW PAGE
      GO TO 140                                                         PGRID130
 200  CONTINUE                                                          PGRID131
      GO TO 999
C  
C     
C IF GIS OUTPUT THEN PERFORM THIS LOOP INSTEAD                
C
1140  CONTINUE
		GINDX=0
		DO 131 I=1,GMAX
131   GARRAY(I)=0
      IX=IXP
      IY=NI-1
      NJP=NJ       
      DO 1180 I=1,NI
      II=NI-I+1     
      DO 1170 J=1,NJP
      X=IX*GRDSZ+PGXMIN
      Y=IY*GRDSZ+PGYMIN 
C THIS CALLS CALCNR TO COMPUTE THE LCDN FOR GRID COORDINATES X AND Y AND STORES 
C THE VALUES AS VNEF, OR CNR. 
      CALL CALCNR(X,Y)
      IF(CNR.GT.80.)CNR=80. 
		GINDX = GINDX + 1
		GARRAY(GINDX) = CNR
		IF (GINDX.GE.GMAX) THEN
C  output to GIS
			WRITE(IOUT,171) GARRAY
			GINDX = 0
		ENDIF
171   FORMAT(1h ,500F7.3)
1170  IX=IX+1 
      IY=IY-1 
      IX=IXP      
 1180 CONTINUE   
C
C
C IF GIS OUTPUT THEN MUST PRINT ANY REMAINING VALUES IN GARRAY
		WRITE(IOUT,171) (GARRAY(J),J=1,GINDX)
C
C THIS CALCULATES THE TIME SPENT IN PGRID.
999   T99= SECOND(CP )       
	T99=TIMEDIF(T99,T100)
      WRITE(KPRINT,900) T99 
      RETURN 
 3000 WRITE (KPRINT,702)
	  STOP
C EOF EXIT
300   WRITE(KPRINT,698)
      STOP                                                              PGRID138
  600 FORMAT(1H1///8X,15(I7,1X) )                                       PGRID139
  601 FORMAT(1h ,A1,I7,15(F6.1,2X))
  603 FORMAT(1h ,4HGRID,2X,2I8)   
  604 FORMAT(1h ,4HGRDI,2X,2I8,F8.3)
  605 FORMAT(1h ,4HPHS2)           
 610  FORMAT(1H1,///,T15,'.....     PUDDLE GRID     .....')             PGRID144
  611 FORMAT(                                             ///T12,'START PGRID145
     1AT DATA BASE COORDINATES (',F9.1,',',F9.1,' )'                    PGRID146
     2                                                     //T12,'STOP  PGRID147
     1AT DATA BASE COORDINATES (',F9.1,',',F9.1,' )' )                  PGRID148
  612 FORMAT(///   40H *****     WARNING -- SPECIFIED BOUNDS ( ,F9.1,','PGRID149
     1,F9.1,' ):(',F9.1,',',F9.1,' ) DO NOT CORRESPOND TO INTEGRAL GRID PGRID150
     2BOUNDS.'/21X,'MODIFIED BOUNDS WILL BE USED TO PRODUCE THE GRID ANDPGRID151
     3 TO DEFINE ANY PLOT UTILIZING THIS GRID')                         PGRID152
620   FORMAT(//T16,'  INVERSION =', 3F8.2 //) 
  621 FORMAT(/12X,'GRID SIZE =',F8.1,                  ' ; DISTANCES IN PGRID154
     1',A10//)                                                          PGRID155
 622  FORMAT(T12,'CALCULATIONS FOR NEF WILL USE ',A10,' OF DAY AND NIGHTPGRID156
     1 CALCULATIONS',//)                                                PGRID157
  697 FORMAT(///   31H *****     WARNING -- GRID SIZE ,F8.1,' NOT MULTIPPGRID158
     1LE OF',F8.1)                                                      PGRID159
  698 FORMAT(///   56H *****     ERROR -- MISSING INPUT DIRECTIVE; JOB APGRID160
     1BORTED  )                                                         PGRID161
700   FORMAT(4F10.0,2A10) 
  701 FORMAT(1h ,A10,5F10.0,A10)
  702 FORMAT(/// 48H 'ARRAY INDEX LARGER THAN THE DIMENSIONS; ABORT') 
 900  FORMAT(//,T15,'.....     TIME FOR PUDDLING SUBPROGRAM IS ',F8.3,  PGRID164
     1' SECONDS')                                                       PGRID165
      END                                                               PGRID166
