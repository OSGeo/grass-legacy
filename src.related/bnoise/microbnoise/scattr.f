C$NOFLOATCALLS
      SUBROUTINE SCATTR                                                 SCATTR 2
	CHARACTER*10  DHDR,KTAR,KFPT
C THIS SUBROUTINE WRITES TEXT CARDS OUT TO TAPE TO BE USED BY THE PLOT
C SUBROUTINE TO PRODUCE A SCATTER GRAM, HAVING A NORMAL DISTRIBUTION AND A
C SPECIFIED STANDARD DEVIATION. 
      COMMON/IO/KARD,KPRINT                                             SCATTR 3
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS                              SCATTR 4
C     BDS IS FLAG USED TO CHECK IF BOUNDS WERE INITALIZED 
      LOGICAL BDS                                                       SCATTR 5
C     THE FOLLOWING ARE FLAGS TO TELL WHAT KIND OF DATA 
C     IS BEING PROCESSED. 
      LOGICAL TRG,FPT,GUNS,DAY,NIGHT                                    SCATTR 6
C     IDTRG=TARGETS,TCORDS=LOCATION(X,Y) OF TARGETS 
C     THITS=TARGET HITS 
C     IDGUN=GUN TYPES 
      DIMENSION IDTRG(50),TCORDS(50,2),THITS(50),IDGUN(50),ICHAR(7),    SCATTR 7
     1   DHDR(3),IK(4)                                                  SCATTR 8
      EQUIVALENCE (KBLNK,ICHAR(1))                                      SCATTR 9
C     ICHAR=CHARACTERS USED TO IDENTIFY DATA(INDICATED ON INPUT CARD) 
C     DHDR=DENOTES DAY, NIGHT, OR BOTH
      DATA ICHAR/' ','T','F','B','D','N','G'/,DHDR/'   DAY   ',         
     1   '  NIGHT  ','DAY+NIGHT'/,                                      SCATTR11
     1 KTAR/'TARGETS'/,KFPT/ 'FIRING PTS'/,STAR/'*'/                    SCATTR12
C     NUMTRG=NUMBER OF TARGETS
C     NUMGT=NUMBER OF GUN TYPES 
      DATA NUMTRG/50/,NUMGT/50/                                         SCATTR13
C     IN=TAPE 7   IOUT=TAPE 4 
      DATA IN/7/,IOUT/4/                                                SCATTR14
C VALUE OF VARIABLES:  ICHAR(7)= ,T,F,B,D,N,G  KTAR=7H TARGETS  STAR=* NUMGT=28 
C DHDR(3)=DAY,NIGHT, DAY+NIGHT  KFPT= FIRING PTS  NUMTRG=30 IN=7  IOUT=4
      T100= SECOND( CP)                                                 SCATTR15
C     SCATTER HEADING 
      WRITE(KPRINT,600)                                                 SCATTR16
C CHECK BOUNDARIES                                                      SCATTR17
C GRID BOUNDARIES NOT PROPERLY INITIALIZED -- ABORT                     SCATTR18
      IF(BDS) GO TO 20
      WRITE(KPRINT,15)
15    FORMAT('  ...BOUNDS MUST BE CALLED BEFORE SCATTER...')
      STOP
C 
   20 CONTINUE                                                          SCATTR20
C INITIALIZE(NOTE:  TRG,FPT,DAY,NIGHT,GUNS,ARE LOGICAL FLAGS) 
      TRG=.FALSE.                                                       SCATTR22
      FPT=.FALSE.                                                       SCATTR23
      DAY=.FALSE.                                                       SCATTR24
      NIGHT=.FALSE.                                                     SCATTR25
      GUNS=.FALSE.                                                      SCATTR26
C INITIALIZE                                                            SCATTR21
      NTRG=0                                                            SCATTR27
      NFPT=0                                                            SCATTR28
      NHITS=0                                                           SCATTR29
      NSHOTS=0                                                          SCATTR30
      DO 10 I=1,NUMTRG                                                  SCATTR31
   10 THITS(I)=0.                                                       SCATTR32
C READ DIRECTIVES                                                       SCATTR33
C READ SCT-2 CARD WITH INFORMATION ABOUT WHICH VARIABLES ARE REQUESTED(IE,
C TARGETS, FPS,ETC. SD,MUTIPLICATION FACTOR)
      READ(KARD,601,END=550)IK,(IDGUN(I),I=1,28),FACT,SD                        
C ERROR..PREMATURE EOF ON TAPE, EXECUTION ABORTED.
C DEFAULT VALUE OF STANDARD DEVIATION IS 300. 
      IF(SD.LE.0.) SD=300.
      IH=0                                                              SCATTR36
C FOR COLUMNS 1-4, CHECK FOR WHICH VARIABLE REQUESTED.
      DO 37 I=1,4                                                       SCATTR37
      KAR=IK(I)                                                         SCATTR38
      DO 30 J=1,7                                                       SCATTR39
      IF(KAR.EQ.ICHAR(J))GO TO (37,31,32,33,34,35,36),J                 SCATTR40
   30 CONTINUE                                                          SCATTR41
C     SET APPROPRIATE FLAGS 
C T = TARGETS                                                           SCATTR42
   31 TRG=.TRUE.                                                        SCATTR43
      GO TO 37                                                          SCATTR44
C F = FIRING PTS.                                                       SCATTR45
   32 FPT=.TRUE.                                                        SCATTR46
      GO TO 37                                                          SCATTR47
C B = BOTH DAY AND NIGHT DATA                                           SCATTR48
   33 DAY=.TRUE.                                                        SCATTR49
      NIGHT=.TRUE.                                                      SCATTR50
      IH=3                                                              SCATTR51
      GO TO 37                                                          SCATTR52
C D = DAY DATA ONLY                                                     SCATTR53
   34 DAY=.TRUE.                                                        SCATTR54
      IH=IH+1                                                           SCATTR55
      GO TO 37                                                          SCATTR56
C N = NIGHT DATA ONLY                                                   SCATTR57
   35 NIGHT=.TRUE.                                                      SCATTR58
      IH=IH+2                                                           SCATTR59
      GO TO 37                                                          SCATTR60
C G = SPECIFIED GUNS ONLY                                               SCATTR61
   36 GUNS=.TRUE.                                                       SCATTR62
   37 CONTINUE                                                          SCATTR63
C CHECK FOR VALID OPTION SET                                            SCATTR64
      IF(IH.GT.3)IH=3                                                   SCATTR65
      IF(TRG.OR.FPT) GO TO 38                                           SCATTR66
      TRG=.TRUE.                                                        SCATTR67
      FPT=.TRUE.                                                        SCATTR68
   38 IF(DAY.OR.NIGHT)GO TO 40                                          SCATTR69
      DAY=.TRUE.                                                        SCATTR70
      NIGHT=.TRUE.                                                      SCATTR71
      IH=3                                                              SCATTR72
C THIS WRITES OUT WHICH TYPE OF DATA WE HAVE (DAY,NIGHT,DAY+NIGHT) FROM ARRAY 
C DHDR. 
   40 WRITE(KPRINT,603)DHDR(IH)                                         SCATTR73
C THIS WRITES OUT A HEADING FOR TARGETS AND/OR FIRING POINTS USING KTAR OR KFPT 
      IF(TRG)WRITE(KPRINT,604)KTAR                                      SCATTR74
      IF(FPT)WRITE(KPRINT,604)KFPT                                      SCATTR75
C MESSAGE--"ALL GUN TYPES"
      IF(.NOT.GUNS)GO TO 55                                             SCATTR76
C UP TO NUMGT GUN TYPES ARE READ,BUT ONLY THOSE UP TO FIRST BLANK ID ARESCATTR77
C     USED                                                              SCATTR78
      DO 50 I=1,NUMGT                                                   SCATTR79
      IF(IDGUN(I).NE.KBLNK)GO TO 50                                     SCATTR80
      NGUNS=I-1                                                         SCATTR81
      GO TO 51                                                          SCATTR82
   50 CONTINUE                                                          SCATTR83
      NGUNS=NUMGT                                                       SCATTR84
   51 IF(NGUNS.GT.0)GO TO 52                                            SCATTR85
C ERROR--GUN OPTION SELECTED BUT NO GUN TYPES SPECIFIED:  EXECUTION ABORTED.
      WRITE(KPRINT,906)                                                 SCATTR86
      STOP                                                              SCATTR87
C     WRITE OUT GUN TYPE ID'S USED
   52 WRITE(KPRINT,605)(IDGUN(I),I=1,NGUNS)                             SCATTR88
      GO TO 60                                                          SCATTR89
   55 WRITE(KPRINT,606)                                                 SCATTR90
C MESSAGE--BOUNDED BY XMIN,XMAX,YMIN,YMAX 
   60 WRITE(KPRINT,613)XMIN,YMIN,XMAX,YMAX                              SCATTR91
C FACT DEFAULTS TO 1
      IF(FACT.LE.0.)FACT=1.                                             SCATTR92
C     PRINT MULTIPLIER, STANDARD DEV., NUMBER OF DAYS 
      WRITE(KPRINT,612)FACT                                             SCATTR93
      WRITE(KPRINT,607)SD                                               SCATTR94
      READ(KARD,602,END=550)DAYS                                                
C ERROR..PREMATURE EOF ON TAPE, EXECUTION ABORTED.
C DAYS DEFAULTS TO 1.  MESSAGE:  "DAYS DATA IN DATA BASE", (DAYS) 
      IF(DAYS.LE.0.) DAYS=1.                                            SCATTR97
      WRITE(KPRINT,608)DAYS                                             SCATTR98
C WRITE HEADER INFO TO OUTPUT TAPE                                      SCATTR99
      REWIND IOUT                                                       SCATT100
C WRITE OUT TEXT CARDS TO TAPE 4 FORM PHS4, ECHO, 
      WRITE(IOUT,700)                                                   SCATT101
C     WRITE(IOUT,701)                                                   SCATT102
C START PROCESSING DATA BASE FILE                                       SCATT103
      REWIND IN                                                         SCATT104
C READ THRU GUN TYPE CARDS -- NOT USED                                  SCATT105
  100 READ(IN,609,END=590)COL1                                                  
C ERROR..PREMATURE EOF ON TAPE, EXECUTION ABORTED.
      IF(COL1.NE.STAR)GO TO 100                                         SCATT108
C TARGETS                                                               SCATT109
      IF(.NOT.TRG)GO TO 290                                             SCATT110
C READ AND STORE TARGET ID S,COORDS                                     SCATT111
      NTRG=0                                                            SCATT112
      DO 210 I=1,NUMTRG                                                 SCATT113
      READ(IN,609,END=590)COL1,IDTRG(I),TCORDS(I,1),TCORDS(I,2)                 
      NTRG=NTRG+1                                                       SCATT116
      IF(COL1.EQ.STAR)GO TO 300                                         SCATT117
  210 CONTINUE                                                          SCATT118
C TABLE OVERFLOW -- ASSUMING CORRECT DATA BASE FILE,SHOULDNT HAPPEN     SCATT119
      WRITE(KPRINT,901)                                                 SCATT120
      STOP                                                              SCATT121
C READ THRU TARGET CARDS -- NOT REQUESTED                               SCATT122
  290 READ(IN,609,END=590)COL1                                                  
      IF(COL1.NE.STAR)GO TO 290                                         SCATT125
C FIRING PTS                                                            SCATT126
  300 READ(IN,609,END=400)COL1,ID,X,Y                                           
      NFPT=NFPT+1                                                       SCATT129
      FSHOTS=0.                                                         SCATT130
C READ DEF CARD                                                         SCATT131
  305 READ(IN,610)COL1,IDG,DAYNO,DARKNO,IDT,KFLAG                       SCATT132
      IF(.NOT.GUNS)GO TO 320                                            SCATT133
C CHECK IF ONE OF REQUESTED GUN TYPES;IF NOT,READ NEXT DEF CARD         SCATT134
      DO 310 I=1,NGUNS                                                  SCATT135
      IF(IDG.EQ.IDGUN(I))GO TO 320                                      SCATT136
  310 CONTINUE                                                          SCATT137
      GO TO 390                                                         SCATT138
C IF TARGETS REQUESTED,CHECK HIT FLAG;IF.NE.0,NO HIT                    SCATT139
  320 IF(.NOT.TRG.OR.KFLAG.NE.0)GO TO 350                               SCATT140
      DO 325 I=1,NTRG                                                   SCATT141
      IF(IDT.EQ.IDTRG(I))GO TO 330                                      SCATT142
  325 CONTINUE                                                          SCATT143
C ERROR -- UNDEFINED TARGET                                             SCATT144
      WRITE(KPRINT,902)IDT                                              SCATT145
      STOP                                                              SCATT146
C CUMULATE TARGET HITS                                                  SCATT147
  330 IF(DAY) THITS(I)=THITS(I)+DAYNO                                   SCATT148
      IF(NIGHT) THITS(I)=THITS(I)+DARKNO                                SCATT149
  350 IF(.NOT.FPT)GO TO 390                                             SCATT150
C CUMULATE FIRING PT SHOTS                                              SCATT151
      IF(DAY)FSHOTS=FSHOTS+DAYNO                                        SCATT152
      IF(NIGHT)FSHOTS=FSHOTS+DARKNO                                     SCATT153
C LAST DEF CARD&                                                        SCATT154
  390 IF(COL1.NE.STAR)GO TO 305                                         SCATT155
      IF(.NOT.FPT)GO TO 300                                             SCATT156
C     TEST IF DATA POINT IS IN BOUNDS  IF NOT DON'T DO
      IF (X.LT.XMIN.OR.X.GT.XMAX)  GOTO 300 
      IF (Y.LT.YMIN.OR.Y.GT.YMAX)  GOTO 300 
C PROCESS ALL POINTS FOR 1 FIRING PT                                    SCATT157
      N=FACT*FSHOTS/DAYS+.5                                             SCATT158
C     TOTAL NUMBER OF FIRINGS FOR 1 FIRING POINT
      NSHOTS=NSHOTS+N                                                   SCATT159
C     CALCULATE STANDARD DEVIATION
      SS=(SD/10.)*(SQRT(FLOAT(N)))
      IF(N.GT.0)CALL SCATPL(N,X,Y,SS)                                   SCATT160
      GO TO 300                                                         SCATT161
C PROCESS TARGET HITS,IF REQUESTED                                      SCATT162
  400 IF(.NOT.TRG)GO TO 500                                             SCATT163
      DO 450 I=1,NTRG                                                   SCATT164
      IF(THITS(I).EQ.0.)GO TO 450                                       SCATT165
C     TEST IF DATA POINT IS IN BOUNDS 
      IF (TCORDS(I,1).LT.XMIN.OR.TCORDS(I,1).GT.XMAX)  GOTO 450 
      IF (TCORDS(I,2).LT.YMIN.OR.TCORDS(I,2).GT.YMAX) GOTO 450
C     TOTAL NUMBER OF TARGET HITS(MORE NOISE SOURCES) 
      N=FACT*THITS(I)/DAYS+.5                                           SCATT166
      NHITS=NHITS+N                                                     SCATT167
C COMPUTE THE STANDARD DEVIATION TO BE PROPORTIONAL TO THE
C NUMBER OF POINTS. 
      SS=(SD/10.)*(SQRT(FLOAT(N)))
      IF(N.GT.0)CALL SCATPL(N,TCORDS(I,1),TCORDS(I,2),SS)               SCATT168
  450 CONTINUE                                                          SCATT169
C EOJ PROCESSING                                                        SCATT170
C WRITE TRAILER INFO TO OUTPUT TAPE AND CLOSE                           SCATT171
500	CONTINUE
C 500 WRITE(IOUT,701)                                                   SCATT172
      REWIND IOUT                                                       SCATT173
C STATISTICS                                                            SCATT174
      WRITE(KPRINT,611)IOUT,NTRG,NHITS,NFPT,NSHOTS,FACT                 SCATT175
C COMPUTE TIME SPENT IN SCATTER,
      T99= SECOND(CP )                                                  SCATT176
      T99=T99-T100                                                      SCATT177
      WRITE(KPRINT,904)T99                                              SCATT178
      RETURN                                                            SCATT179
C EOF -- INPUT DIRECTIVES                                               SCATT180
  550 WRITE(KPRINT,905) KARD                                            SCATT181
      STOP                                                              SCATT182
C EOF - DATA BASE                                                       SCATT183
  590 WRITE(KPRINT,905)IN                                               SCATT184
      STOP                                                              SCATT185
  600 FORMAT(1H1///T15,'.....     SCATTER     .....')                   SCATT186
  601 FORMAT(4A1,28A2,2F10.0)                                           SCATT187
  602 FORMAT(F10.0)                                                     SCATT188
  603 FORMAT(////' THE SCATTER DIAGRAM WILL REPRESENT ..',A9,'.. DATA FOSCATT189
     1R:'//)                                                            SCATT190
  604 FORMAT(/10X,'...',A10)                                            SCATT191
  605 FORMAT(/10X,'...GUN TYPES:',33(1X,A2))                            SCATT192
  606 FORMAT(/10X,'...ALL GUN TYPES')                                   SCATT193
  607 FORMAT(//5X,'...STANDARD DEVIATION =',F10.3)                      SCATT194
  608 FORMAT(//5X,'...',F4.0,' DAYS DATA IN DATA BASE')                 SCATT195
  609 FORMAT(A1,1X,A3,1X,2F6.0)                                         SCATT196
  610 FORMAT(A1,17X,A2,2F4.0,4X,A3,I1)                                  SCATT197
  611 FORMAT(////' OUTPUT FILE TAPE',I1,' CONTAINS NASAPLOT PHS4 INPUT DSCATT198
     1ATA REPRESENTING:'                                                SCATT199
     1                ///5X,I5,' TARGETS   :',I6,' SCATTER PTS.'        SCATT200
     1                 //5X,I5,' FIRING PTS:',I6,' SCATTER PTS.'        SCATT201
     1                ///5X,'MULTIPLIER USED =',F10.1)                  SCATT202
  612 FORMAT(////5X,'...MULTIPLIER =',F10.1)                            SCATT203
  613 FORMAT(//' BOUNDED BY (',F9.1,',',F9.1,') - (',F9.1,',',F9.1,')') SCATT204
  700 FORMAT(1H ,4HPHS4)
  701 FORMAT(1H ,4HECHO)
  901 FORMAT(///60H *****     ERROR -- TARGET TABLE OVERFLOW: EXECUTION SCATT207
     1ABORTED      )                                                    SCATT208
  902 FORMAT(///60H *****     ERROR -- ERROR IN DATA BASE: UNDEFINED TARSCATT209
     1GET ID   ,A3,'; EXECUTION ABORTED')                               SCATT210
  904 FORMAT(///T15,'.....     TIME IN SCATTER IS',F8.3,' SECONDS')     SCATT213
  905 FORMAT(///41H *****     ERROR -- PREMATURE EOF ON TAPE ,I1,'; EXECSCATT214
     1UTION ABORTED')                                                   SCATT215
  906 FORMAT(///86H *****     ERROR -- GUN OPTION SELECTED BUT NO GUN TYSCATT216
     1PES SPECIFIED: EXECUTION ABORTED      )                           SCATT217
      END                                                               SCATT218
