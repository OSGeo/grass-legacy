C$NOFLOATCALLS
C$LARGE
      SUBROUTINE FORMA                                                  FORMA  2
C THIS SUBROUTINE:(1) TAKES INPUT FROM THE DATA BASE CONSISTING OF
C AN ID NUMBER AND LOCATION COORDINATES FOR EACH FIRING POINT.
C ALSO INCLUDED ARE DEFINITION CARDS FOR ALL THE NOISE SOURCES AT 
C THAT SITE, GIVING GUN TYPE, NUMBER OF DAY AND NIGHT FIRINGS,
C MAXIMUM AND MINIMUM CHARGE ZONES, TARGET ID OR AN INDICATION
C THAT THE SOURCE IS OMNIDIRECTIONAL, AND HEIGHT FOR EACH FIRING; 
C (2) CALCULATES THE SINE AND COSINE OF THE NOISE DIRECTION 
C (999.0 FOR OMNIDIRECTIONAL SOURCES, INCLUDING TARGETS), DB CHARGE 
C CORRECTION FACTORS FOR TNT WEIGHT (DBWT) AND HEIGHT (DBHT)
C (FOUND IN ARRAY SDBWH); (3) OUTPUTS A TABLE WITH AN ENTRY FOR EACH
C UNIQUE NOISE SOURCE, EACH UNIQUE FIRING POINT-TARGET/HEIGHT-CHARGE
C OR TARGET-CHARGE COMBINATION, A TARGET ALSO BEING A SOURCE OF OMNIDIRECTIONAL 
C NOISE WHEN HIT FOR EACH ENTRY, FORMA ACCUMULATES THE NUMBER OF DAY
C AND NIGHT FIRINGS WITH THAT CHARGE FROM (FIRING PT,) OR TOWARD
C (TARGET) THAT LOCATION. 
  
  
  
      CHARACTER*20  NAME
      CHARACTER*10 IFUNC,CHAT
      CHARACTER*1 IFLAG
      COMMON/ENTRYF/DAYS
      COMMON/IO/KARD,KPRINT                                             FORMA  3
      COMMON /SRCS/ NSRCS 
      COMMON/FT/GRDSZ,CNR,XLOC,YLOC,DAYNO,DARKNO,HEIGHT,HCORR                        
      DIMENSION XLOC(2000),YLOC(2000),DARKNO(2000),DAYNO(2000),NAME(100)
      COMMON/GRID/NRCNR,EAS,RCNR,DIST,SDBWH,              ANGCOS,ANGSIN 
      DIMENSION RCNR(10),DIST(10),SDBWH(2000),
     1ANGCOS(2000),ANGSIN(2000)                                         FORMA  9
      COMMON /DEBUG/ CHECK,REED,TABRD 
      COMMON/CONTR/IDGUN,CONTOR 
      DIMENSION IDGUN(50),CONTOR(50,15) 
      LOGICAL CHECK,REED,TABRD
      INTEGER GTYPE                                                     FORMA 12
      INTEGER GUNT(2000)                                                FORMA 13
      COMMON /GUN/GUNT                                                  FORMA 14
      LOGICAL FIRST                                                     FORMA 15
      COMMON /TABL1/ DBV(301,9,2) , PERV(301,4,2) , ENV(1501) , 
     1 CSCF(601) ,FON1(301,4,2),FON2(301,4,2),FTWO(151,2) 
C     CHARGE ARRAY CONTAINS THE PROPELLANT CHARGES OF TNT 
C     FOR EACH WEAPON.
C     TCHARGE ARRAY CONTAINS THE PROJECTILE CHARGE FOR EACH WEAPON
      DIMENSION CHARGE(10,50),TCHARG(50)
C     IDTYPE HOLDS ID'S FOR FIRING PTS AND TARGETS
      DIMENSION IDTYPE(2000),NOPER(2000)                                FORMA 17
      EQUIVALENCE (IDTYPE(1),ANGCOS(1)),(NOPER(1),ANGSIN(1))            FORMA 18
C     IDTRG HOLDS TARGET ID'S ONLY; CHAT ARRAY HOLDS NAME 
C     OF AVERAGING TECHNIQUE
      DIMENSION IDTRG(50),CHAT(3,3)                                     FORMA 19
C     ARRAYS FOR HEIGHT AND HEIGHT CORRECTION ARE INITIALIZED 
C     TO ZERO.  GROUND CORRECTION IS INITIALIZED TO 1.5 
      DIMENSION HEIGHT(2000),HCORR(2000)                                FORMA 20
      DATA GHCORR/1.5/                   
C CHAT (3,3):  USED TO IDENTIFY THE AVERAGE TECHNIQUE USED. 
      DATA CHAT/ 'MAXIMUM CH', 'AVERAGE OF', 'AVERAGE OF', 'ARGE ZONE ',FORMA 22
     1 ' CHARGE ZO', ' TNT EQUIV',' ','NES','ALENTS'/                   FORMA 23
  
  
C     IN1=TAPE 7(INPUT); IOUT=TAPE 8(OUTPUT)
      DATA IN1/7/,IOUT/8/                                               FORMA 24
C     TABLE LIMIT 
      DATA ITABLIM/50/
      DATA IBLNK/' '/                                                   FORMA 25
      T100=SECOND(CP  )                                                 FORMA 26
	DO 10 IT=1,2000
	HEIGHT(IT)=0.
10	HCORR(IT) =0.
C IF TABGEN HASN'T ALREADY BEEN READ, CALL READTB.
      IF (.NOT.TABRD) CALL READTB 
C     INITIALIZE
      TABRD=.FALSE. 
      YMAX=-99999999.0
      YMIN= 99999999.0
      XMAX=     -99999999.0                                             FORMA 27
      XMIN=      99999999.0                                             FORMA 28
C     HEADING FOR FORMA 
      WRITE(KPRINT,701)                                                 FORMA 29
      ASSIGN 211 TO JUMP                                                FORMA 31
      NDX=1                                                             FORMA 32
  
  
C READ FORM-A-2 CARD (AVERAGING TECHNIQUE, GROUND CORRECTION, 
C AND REWIND INHIBIT FLAG). 
      READ(KARD,603) IFUNC,X2,NOWIND                                    FORMA 33
      IF(X2.GT.0.)GHCORR=X2                                             FORMA 34
      IF(NOWIND .EQ. 0) REWIND IN1
  
C ASSIGN THE GO-TO STATEMENT FOR EACH TECHNIQUE.
      IF (IFUNC .EQ.  'MAX       ') GO TO 91                            FORMA 35
      IF (IFUNC .EQ.  'IAVE      ') GO TO 92                            FORMA 36
      IF (IFUNC .EQ.  'CAVE      ') GO TO 93                            FORMA 37
      GO TO 99                                                          FORMA 38
   91 ASSIGN 211 TO JUMP                                                FORMA 39
C     NDX IS THE INDEX USED WHEN WRITING THE HEADING FOR
C     THE AVERAGING TECHNIQUE.
      NDX=1                                                             FORMA 40
      GO TO 99                                                          FORMA 41
   92 ASSIGN 212 TO JUMP                                                FORMA 42
      NDX=2                                                             FORMA 43
      GO TO 99                                                          FORMA 44
   93 ASSIGN 213 TO JUMP                                                FORMA 45
      NDX=3                                                             FORMA 46
C WRITE HEADING ABOUT TECHNIQUE USING CHAT ARRAY. 
  99  WRITE(KPRINT,609)(CHAT(NDX,I),I=1,3)                              FORMA 47
  
  
  
C     READ GUN TYPE TABLE                                               FORMA 48
      I=1                                                               FORMA 49
   50 READ(IN1,604,END=520)IDGUN(I),TCHARG(I),(CHARGE(J,I),J=1,10), 
     1NAME(2*I-1),IFLAG,(CONTOR(I,J),J=1,15)
C     IF END OF FILE STOP 
C     IF '*' READ TARGETS 
      IF(IFLAG.EQ.'*') GO  TO 60
C     COUNT GUN TYPES 
      I=I+1                                                             FORMA 53
C     IF TABLE OVERFLOWS, THEN STOP.
      IF(I.LE.ITABLIM) GO TO 50 
      WRITE(KPRINT,707) I 
      STOP
C     READ TARGETS UPON FINDING  * IN COL 1 PROCEED WITH SOURCES        FORMA 56
C     NTYPES=NUMBER OF GUN TYPES IN DATA BASE 
   60 NTYPES=I                                                          FORMA 57
  
  
  
      I=1                                                               FORMA 58
      NTAR=0                                                            FORMA 59
C     READ TARGETS
100   READ(IN1,600,END=500) IFLAG,J,XLOC(I),YLOC(I),PHCORR                      
C     IF END OF FILE, STOP
C     COUNT TARGET CARDS
      NTAR=NTAR+1                                                       FORMA 63
C     STORE TARGET ID IN IDTRG ARRAY
      IDTRG(I)=J                                                        FORMA 64
C     IF NO GROUND CORR GIVEN, IT TAKES A DEFAULT VALUE 
      IF(PHCORR.EQ.0.)PHCORR=GHCORR                                     FORMA 65
C     CHECK TO SEE IF TARGET IS WITHIN SPECIFIED BOUNDS 
      IF (XLOC(I) .GT. XMAX) XMAX=XLOC(I)                               FORMA 66
      IF (XLOC(I) .LT. XMIN) XMIN=XLOC(I)                               FORMA 67
      IF (YLOC(I) .GT. YMAX) YMAX=YLOC(I)                               FORMA 68
      IF (YLOC(I) .LT. YMIN) YMIN=YLOC(I)                               FORMA 69
C     INITIALIZE ARRAY IDTYPE WITH A NEGATIVE NUMBER TO DENOTE
C     A TARGET. 
      IDTYPE(I)=-I                                                      FORMA 70
C     INITIALIZE GUNT TO 0. 
      GUNT(I)=0                                                         FORMA 71
C     INITIALIZE SDBWH TO DENOTE A TARGET NEVER ACCESSED
      SDBWH(I)=-100.0                                                   FORMA 72
      HCORR(I)=PHCORR                                                   FORMA 73
C     IF '*' READ FIRING POINT CARDS
      IF (IFLAG .EQ. '*') GO TO 200                                     FORMA 74
      I=I+1 
C     IF OVERFLOW THEN STOP 
      IF(I .LE. ITABLIM) GO TO 100
      WRITE(KPRINT,709) I 
      STOP
  
  
  
C     COUNT FP CARDS
 200  I=I+1                                                             FORMA 76
C FIRING POINT CARDS. 
C     READ FP-1 CARD
      READ(IN1,601,END=300) XLOC(I),YLOC(I),PHCORR                              
C     IF END OF FILE, THEN END OF DATA. 
C     IF PHCORR DOESN'T HAVE A VALUE THEN GIVE IT DEFAULT 
      IF(PHCORR.EQ.0.)PHCORR=GHCORR                                     FORMA 79
C     CHECK TO SEE IF FP IS WITHIN BOUNDARIES 
      IF (XLOC(I) .GT. XMAX) XMAX=XLOC(I)                               FORMA 80
      IF (XLOC(I) .LT. XMIN) XMIN=XLOC(I)                               FORMA 81
      IF (YLOC(I) .GT. YMAX) YMAX=YLOC(I)                               FORMA 82
      IF (YLOC(I) .LT. YMIN) YMIN=YLOC(I)                               FORMA 83
C     IPTR POINTS TO THE BEGINNING OF DEF. CARDS FOR THIS FIRING PT.
      IPTR=I                                                            FORMA 84
      FIRST=.TRUE.                                                      FORMA 85
  
  
C FP-2 CARDS
 210  READ(IN1,602,END=300) IFLAG,GTYPE,DAY,DARK,MIN,MAX,ID,KFLAG,HGT           
      DO 201 IT=1,NTYPES                                                FORMA 89
      IF(GTYPE.EQ.IDGUN(IT))GO TO 202                                   FORMA 90
  201 CONTINUE                                                          FORMA 91
C     UNDEFINED GUN TYPE                                                FORMA 92
      WRITE(KPRINT,705)ITYPE,XLOC(IPTR),YLOC(IPTR)                      FORMA 93
      STOP                                                              FORMA 94
C     SET POINTER TO THE LOCATION IN GUN TYPE TABLE 
C     ITYPE IS NOW SUBSCRIPT INTO GUN TABLE 
  
  202 ITYPE=IT                                                          FORMA 95
      IF(ID.NE.IBLNK)GO TO 204                                          FORMA 96
      IF(KFLAG.EQ.1)GO TO 203                                           FORMA 97
      WRITE(KPRINT,708)XLOC(IPTR),YLOC(IPTR)                            FORMA 98
      STOP                                                              FORMA 99
C     HIT FLAG SET
  
  203 ID=0                                                              FORMA100
      GO TO 207                                                         FORMA101
  204 CONTINUE                                                          FORMA102
C     CHECK TO SEE IF THIS IS A LEGITIMATE TARGET 
      DO 205 IT=1,NTAR                                                  FORMA103
      IF(ID.EQ.IDTRG(IT)) GO TO 206                                     FORMA104
  205 CONTINUE                                                          FORMA105
  
C     UNDEFINED TARGET                                                  FORMA106
      WRITE(KPRINT,704)ID,XLOC(IPTR),YLOC(IPTR)                         FORMA107
      STOP                                                              FORMA108
C     UPON LOCATING THE TARGET ID, SET A POINTER TO THE LOCATION
  
  206 ID=IT                                                             FORMA109
  207 CONTINUE                                                          FORMA110
  
  
C IN THIS SECTION, THE SUBROUTINE JUMPS TO WHICHEVER METHOD OF AVERAGING IT 
C IS GOING TO USE ACCORDING TO THE CARD DIRECTIVE.  IT CALCULATES THE EQUIVALENT
C TNT WEIGHTS, WITH THE INFORMATION STORED IN ARRAY CHARGE. 
      GO TO JUMP,(211,212,213)                                          FORMA111
C     AVE. TECHNIQUE MAX
 211  CONTINUE                                                          FORMA112
      IAVE=MAX0(MIN,MAX)                                                FORMA113
      GO TO 214                                                         FORMA114
C     AVE. TECHNIQUE IAVE 
 212  CONTINUE                                                          FORMA115
       AVE=(MIN+MAX)*.5                                                 FORMA116
      IAVE=AVE                                                          FORMA117
      IF((AVE-IAVE).GT..01)IAVE=IAVE+1                                  FORMA118
      GO TO 214                                                         FORMA119
C     AVE. TECHNIQUE CAVE 
 213  CONTINUE                                                          FORMA120
      CHAR=(CHARGE(MIN,ITYPE)+CHARGE(MAX,ITYPE))*.5                     FORMA121
      GO TO 216                                                         FORMA122
 214  CONTINUE                                                          FORMA123
      CHAR=CHARGE(IAVE,ITYPE)                                           FORMA124
  216 CONTINUE                                                          FORMA125
      TCHAR=TCHARG(ITYPE)                                               FORMA126
  
  
C     IF FIRST DEFINITION CARD FOR (XLOC,YLOC) PROCESS                  FORMA127
      IF (FIRST) GO TO 250                                              FORMA128
  
C     CHECK FOR OLD TARGET (ALSO OMNI)                                  FORMA129
      IF(ID.EQ.0)GO TO 217                                              FORMA130
  
C     OLD TARGET SO CHECK TO SEE IF IT IS A DUPLICATE 
      DO 215 IOLD=IPTR,I                                                FORMA131
C     SAME TARGET, CHARGE, AND GUN; UPDATE
      IF (IDTYPE(IOLD).EQ.ID.AND.SDBWH(IOLD).EQ.CHAR.AND.               FORMA132
     1 GUNT(IOLD).EQ.ITYPE)  GOTO 220                                   FORMA133
 215  CONTINUE                                                          FORMA134
  
      GO TO 219                                                         FORMA135
  
C     SEE IF TARGET IS SAME 
  217 DO 218 IOLD=IPTR,I                                                FORMA136
C     OMNI AND GUN
      IF(SDBWH(IOLD).EQ.CHAR.AND.HEIGHT(IOLD).EQ.HGT)GO TO 220          FORMA137
  218 CONTINUE                                                          FORMA138
  
  
  219 CONTINUE                                                          FORMA139
C     NEW TARGET,NEW CHARGE,OR NEW HEIGHT FOUND FOR (X,Y)               FORMA140
      I=I+1                                                             FORMA141
      XLOC(I)=XLOC(IPTR)                                                FORMA142
      YLOC(I)=YLOC(IPTR)                                                FORMA143
      SDBWH(I)=CHAR                                                     FORMA144
      GUNT(I)=ITYPE                                                     FORMA145
      IDTYPE(I)=ID                                                      FORMA146
      DAYNO(I)=DAY                                                      FORMA147
      DARKNO(I)=DARK                                                    FORMA148
      HEIGHT(I)=HGT                                                     FORMA149
      HCORR(I)=PHCORR                                                   FORMA150
  
C     NO HIT AT TARGET; ALL DONE
C     IF OMNI SOURCE DONE                                               FORMA151
 221  IF (KFLAG .EQ. 1) GO TO 240                                       FORMA152
  
C     ON FIRST ACCESS TO A TARGET SET TCHARGE                           FORMA153
      IF (SDBWH(ID) .LT. 0) GO TO 270                                   FORMA154
C     LINK TO TARGETS OF SAME TYPE BUT DIFFERENT CHARGE                 FORMA155
      IDOLD=ID                                                          FORMA156
 222  IF (SDBWH(IDOLD) .EQ. TCHAR.AND.HEIGHT(IDOLD).EQ.HGT)GO TO 225    FORMA157
      IF (IABS(IDTYPE(IDOLD)) .EQ. IDOLD) GO TO 227                     FORMA158
      IDOLD=IABS(IDTYPE(IDOLD))                                         FORMA159
      GO TO 222                                                         FORMA160
  
 225  CONTINUE                                                          FORMA161
C     UPDATE OLD TARGET AND SOURCE                                      FORMA162
      DAYNO(IDOLD)=DAYNO(IDOLD)+DAY                                     FORMA163
      DARKNO(IDOLD)=DARKNO(IDOLD)+DARK                                  FORMA164
      GO TO 240                                                         FORMA165
  
C     NEW TARGET-CHARGE-HEIGHT COMBINATION                              FORMA166
C PROCESSES FP-2 CARDS
 227  I=I+1                                                             FORMA167
      IDTYPE(IDOLD)=-I                                                  FORMA168
      IDTYPE(I)=-I                                                      FORMA169
      SDBWH(I)=TCHAR                                                    FORMA170
      GUNT(I)=ITYPE                                                     FORMA171
      XLOC(I)=XLOC(ID)                                                  FORMA172
      YLOC(I)=YLOC(ID)                                                  FORMA173
      HCORR(I)=HCORR(ID)                                                FORMA174
      HEIGHT(I)=HGT                                                     FORMA175
      DAYNO(I)=DAY                                                      FORMA176
      DARKNO(I)=DARK                                                    FORMA177
      GO TO 240                                                         FORMA178
  
 220  CONTINUE                                                          FORMA179
C     UPDATE OLD CHARGE OF SOURCE                                       FORMA180
      DAYNO(IOLD)=DAYNO(IOLD)+DAY                                       FORMA181
      DARKNO(IOLD)=DARKNO(IOLD)+DARK                                    FORMA182
      GO TO 221                                                         FORMA183
  
C     TOO MANY SOURCES
 240  IF (I .GE. 2000) GO TO 510                                        FORMA184
C GO BACK AND READ NEW FP-1 CARD. 
C     STAR * INDICATES END OF FP-2 CARDS FOR A PARTICULAR FIRING PT.
      IF (IFLAG .EQ. '*') GO TO 200                                     FORMA185
      GO TO 210                                                         FORMA186
  
  
C     SET FIRST SOURCE ACCESS                                           FORMA187
C     STORE ALL INFORMATION OF FP-2 INTO APPROPRIATE ARRAYS 
 250  CONTINUE                                                          FORMA188
      IDTYPE(I)=ID                                                      FORMA189
      DAYNO(I)=DAY                                                      FORMA190
      DARKNO(I)=DARK                                                    FORMA191
      GUNT(I)=ITYPE                                                     FORMA192
      SDBWH(I)=CHAR                                                     FORMA193
      HEIGHT(I)=HGT                                                     FORMA194
      HCORR(I)=PHCORR                                                   FORMA195
      FIRST=.FALSE.                                                     FORMA196
      GO TO 221                                                         FORMA197
  
C     SET FIRST TARGET ACCESS                                           FORMA198
 270  CONTINUE                                                          FORMA199
      SDBWH(ID)=TCHAR                                                   FORMA200
      HEIGHT(ID)=HGT                                                    FORMA201
      DAYNO(ID)=DAY                                                     FORMA202
      DARKNO(ID)=DARK                                                   FORMA203
      GO TO 240                                                         FORMA204
  
  
  
 300  CONTINUE                                                          FORMA205
C     NUMBER OF SOURCES 
      NSRCS=I-1                                                         FORMA206
C FRMA-2 DIRECTIVE CARD.
C     NUMBER OF DAYS CARD 
      READ(KARD,607,END=301) DAYS                                               
	GO TO 302
301	DAYS=0.0
302	CONTINUE
      IF (DAYS .LE. 0.0) DAYS = 1.0                                     FORMA231
      DAYN=0.0                                                          FORMA232
      WRITE(KPRINT,610) DAYS                                            FORMA233
      DARKN=0.0                                                         FORMA234
  
  
C     BEGIN TO COMPRESS DATA
      DO 420 I=1,NSRCS                                                  FORMA235
C     IF GUN, CALCULATE ANGLE AT WHICH IT IS POINTING 
      IF (IDTYPE(I).GT.0)  GOTO 410                                     FORMA236
C     FOR OMNIDIRECTIONAL FIRINGS, ANGSIN=ANGCOS=999.0
      ANGSIN(I)=999.0                                                   FORMA237
      ANGCOS(I)=999.0                                                   FORMA238
      GOTO 420                                                          FORMA239
  
410   CONTINUE                                                          FORMA240
C     CALCULATE THE ANGLE OF FIRING TOWARDS A TARGET
C     CALCULATE DISTANCE FROM GUN TO TARGET 
      A = XLOC(IDTYPE(I))-XLOC(I)                                       FORMA241
      B=  YLOC(IDTYPE(I))-YLOC(I)                                       FORMA242
      C = SQRT(A*A+B*B)                                                 FORMA243
      IF (C.NE.0)  GOTO 405                                             FORMA244
      WRITE(KPRINT,710) XLOC(I),YLOC(I)                                 FORMA245
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 6/18/88 following format statement changed to
C print out use of UTM coord's correctly
C710    FORMAT(//T15,'......  WARNING   GUN IS POINTING AT SELF' ,      FORMA246
C    1   '...  LOCATION  (',F8.0,',',F8.0,')')                          FORMA247
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
710    FORMAT(//T15,'......  WARNING   GUN IS POINTING AT SELF' ,       FORMA246
     1   '...  LOCATION  (',F9.1,',',F9.1,')')                          FORMA247
C     MAKE IT OMNIDIRECTIONAL 
      ANGSIN(I)=999.0 
      ANGCOS(I)=999.0 
      GO TO 420 
C     GET SINE AND COSINE OF THE ANGLE
405   ANGSIN(I)=B/C                                                     FORMA248
      ANGCOS(I)=A/C                                                     FORMA249
420   CONTINUE                                                          FORMA250
  
  
  
      I=0 
  400 I=I+1 
C     IF SDBWH<0 THEN ITEM IS A TARGET  NEVER ACCESSED
      IF(SDBWH(I).GT.0.)GO TO 401                                       FORMA253
  402 IF(SDBWH(NSRCS).GT.0.)GO TO 403                                   FORMA254
      NSRCS=NSRCS-1                                                     FORMA255
      IF(NSRCS.GT.I)GO TO 402                                           FORMA256
      NSRCS=I-1                                                         FORMA257
      GO TO 4000
  
  403 CONTINUE                                                          FORMA259
C     SWITCH BOTTOM ELEMENTS WITH PRESENT ELEMENTS
      XLOC(I)=XLOC(NSRCS)                                               FORMA260
      YLOC(I)=YLOC(NSRCS)                                               FORMA261
      SDBWH(I)=SDBWH(NSRCS)                                             FORMA262
      IDTYPE(I)=IDTYPE(NSRCS)                                           FORMA263
      GUNT(I)=GUNT(NSRCS)                                               FORMA264
      ANGSIN(I)=ANGSIN(NSRCS)                                           FORMA265
      ANGCOS(I)=ANGCOS(NSRCS)                                           FORMA266
      DAYNO(I)=DAYNO(NSRCS)                                             FORMA267
      DARKNO(I)=DARKNO(NSRCS)                                           FORMA268
      HEIGHT(I)=HEIGHT(NSRCS)                                           FORMA269
      HCORR(I)=HCORR(NSRCS)                                             FORMA270
      NSRCS=NSRCS-1                                                     FORMA271
  
401   CONTINUE                                                          FORMA272
C     FOR GREATER THAN ONE DAY CALCULATION DAYNO MUST BE DIVIDED BY     FORMA273
C        NUMBER OF DAYS                                                 FORMA274
      DAYNO(I)=DAYNO(I)/DAYS                                            FORMA275
      DARKNO(I)=DARKNO(I)/DAYS                                          FORMA276
      DAYN=DAYN+DAYNO(I)                                                FORMA277
      DARKN=DARKN+DARKNO(I)                                             FORMA278
C                                                                       FORMA287
  
  
C     CALCULATE THE CORRECTION FACTOR DB OF WEIGHT                      FORMA288
C CHECK FOR OMNI; IF SO, FIND DIFF FROM 5 LBS. OF C-4 
      IF(ANGSIN(I).EQ.999.0) GO TO 4021 
  
C     IF GUN USED, FIND IT IN THE TABLE 
      K=GUNT(I) 
      IF(K .GT. 0) GO TO 4020 
C     GUN NOT FOUND 
      WRITE(KPRINT,4011)
 4011 FORMAT(//T10,'.....ERROR.....GUN NOT FOUND IN GUN TYPE TABLE...JOB
     1 ABORTED.....'//) 
      STOP
  
C     FIND EQUATION PARAMETERS FOR C-4 IN ARRAY CONTOR
 4020 PARAMA=CONTOR(K,1)
      PARAMB=CONTOR(K,2)
      AVG=CONTOR(K,15)
C     CALCULATE INNER RING ENERGY LEVEL 
      A=PARAMA+(PARAMB*ALOG10(SDBWH(I)*16)) 
C     FIND DIFF FROM 5 LBS OF C-4 
      DBWT=A-119.0-AVG
      GO TO 4022
  
C      FOR OMNI 
 4021 DBWT=CFOUR(SDBWH(I)*16)-119.0 
  
  
 4022 DBHI=0.0
C CORRECTION FOR GROUND LEVEL OR BELOW OMNIDIRECTIONAL SOURCES          FORMA292
      IF(ANGSIN(I).NE.999..OR.HEIGHT(I).GT.0.)GO TO 425                 FORMA293
      H100=SDBWH(I)/100.                                                FORMA294
      DBHI=-HCORR(I)+AMIN1(H100,HCORR(I))                               FORMA295
      IF(HEIGHT(I).EQ.0.)GO TO 425                                      FORMA296
  
C     CALCULATE THE DB FOR BELOW THE GROUND                             FORMA297
      DEPTH = ABS(HEIGHT(I)/SDBWH(I)**.333333333)                       FORMA298
      DEP1 = DEPTH/(-1.362)                                             FORMA299
      DEP2 = (DEPTH+17.46)/(-11.933)                                    FORMA300
      IF (DEPTH .GE. 0.0.AND.DEPTH .LE. 2.25) TD=10.0**DEP1             FORMA301
      IF (DEPTH .GT. 2.25) TD=10.0**DEP2                                FORMA302
      DBHI=20.0*ALOG10(TD)+DBHI                                         FORMA303
  
  
C     ADD HEIGHT AND WEIGHT/WEAPON CORRECTION FACTORS IN SDBWH
 425  SDBWH(I)=DBWT+DBHI                                                FORMA304
  
      IF(NSRCS.EQ.I)GO TO 4000
C     ANY MORE SOURCES? 
      IF(I.LT.NSRCS) GO TO 400
  
  
  
C "NUMBER OF UNIQUE NOISE SOURCES COUNTED IS.." 
 4000 WRITE(KPRINT,700) NSRCS 
C "TOTAL PER DAY DAY FIRING IS..."
C "TOTAL PER DAY NIGHT FIRING IS..."
      WRITE(KPRINT,721) DAYN,DARKN                                      FORMA307
  
  
C     ENTRY FORMA2                                                      FORMA308
C WRITE ALL OF THIS OUT TO TAPE8. 
      WRITE(IOUT) NSRCS,DAYS
      WRITE(IOUT) XLOC                                                  FORMA310
      WRITE(IOUT) YLOC                                                  FORMA311
      WRITE(IOUT) SDBWH                                                 FORMA312
      WRITE(IOUT) ANGSIN                                                FORMA313
      WRITE(IOUT) ANGCOS                                                FORMA314
      WRITE(IOUT) DAYNO                                                 FORMA315
      WRITE(IOUT) DARKNO                                                FORMA317
      REWIND IOUT                                                       FORMA319
      REED=.TRUE.                                                       FORMA320
  
C COMPUTE TIME IN FORM-A. 
      T99= SECOND(CP )                                                  FORMA321
      T99=T99-T100                                                      FORMA322
      WRITE(KPRINT,900) T99                                             FORMA323
      RETURN                                                            FORMA324
  
  
  
C "ERROR.. ALL SOURCES ARE TARGETS...NUMBER OF SOURCES COUNTED ARE..."
 500  WRITE(KPRINT,702) I                                               FORMA325
      RETURN                                                            FORMA326
C "ERROR...NUMBER OF SOURCES WILL EXCEED SPACE.  NUMBER OF SOURCES
C NOW IS..."
 510  WRITE(KPRINT,703) I                                               FORMA327
      CALL EXIT                                                         FORMA328
C "ERROR...EOF ENCOUNTERED WHILE PROCESSING GUN TYPE DATA". 
 520  WRITE(KPRINT,706)                                                 FORMA329
      STOP                                                              FORMA330
  
  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 6/18/88  following 2 formats changed to allow 
C for use of UTM's
C600 FORMAT(A1,1X,A3,1X,F6.0,F6.0,F6.0)
C601  FORMAT(6X,F6.0,F6.0,F6.0)                                         FORMA334
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  600 FORMAT(A1,1X,A3,1X,F9.1,1X,F9.1,F6.0)
 601  FORMAT(6X,F9.1,1X,F9.1,F6.0)                                      FORMA334
 602  FORMAT(A1,17X,A2,2F4.0,2I2,A3,I1,F5.0)                            FORMA335
 603  FORMAT(A10,F10.0,I1)                                              FORMA336
  604 FORMAT(1X,A2,11F7.0/3X,A20/A1,2X,F6.2,14F5.2) 
 607  FORMAT(2F10.0)                                                    FORMA346
  609 FORMAT(///                                                        FORMA349
     1         T15,'.....     FOR TNT EQUIVALENT, FORM-A WILL USE ',    FORMA350
     13A10)                                                             FORMA351
  610 FORMAT(//T15,'.....     DATA BASE TIME PERIOD:',F5.0,' DAY(S)')   FORMA352
 700  FORMAT(/////,T25,'NUMBER OF UNIQUE NOISE SOURCES COUNTED IS ',I5) FORMA353
 701  FORMAT(1H1,///,T15,'.....     FORM-A CALCULATION     .....')      FORMA354
 702  FORMAT(//,T15'.....     ERROR     ..... ALL SOURCES ARE TARGETS', FORMA355
     1/,T20,'NUMBER OF SOURCES COUNTED ARE:',I5,//)                     FORMA356
 703  FORMAT(//,T15,'.....     ERROR     .....NUMBER OF SOURCES WILL EXCFORMA357
     1EED SPACE',/,T25,'NUMBER OF SOURCES NOW IS ',I6,//)               FORMA358
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 6/18/88  formats 704, 705, 708 are changed
C to print out UTM coord's correctly
C 704 FORMAT(//T15,'.....     ERROR     ..... UNDEFINED TARGET ID',A5,  FORMA359
C    1' FOR FIRING PT (',F8.0,',',F8.0,')'//)                           FORMA360
C 705 FORMAT(//T15,'.....     ERROR     ..... UNDEFINED GUN    ID',3X,A2FORMA361
C    1,'FOR FIRING PT (',F8.0,',',F8.0,')'//)                           FORMA362
C 708 FORMAT(//T15,'.....     ERROR     .....BLANK TARGET ID,OMNI/KILL FFORMA367
C    1LAG NOT SET FOR SOURCE AT FIRING PT(',F8.0,',',F8.0,')'//)        FORMA368
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  704 FORMAT(//T15,'.....     ERROR     ..... UNDEFINED TARGET ID',A5,  FORMA359
     1' FOR FIRING PT (',F9.1,',',F9.1,')'//)                           FORMA360
  705 FORMAT(//T15,'.....     ERROR     ..... UNDEFINED GUN    ID',3X,A2FORMA361
     1,'FOR FIRING PT (',F9.1,',',F9.1,')'//)                           FORMA362
  706 FORMAT(//T15,'....     ERROR     .....EOF ENCOUNTERED WHILE PROCESFORMA363
     1SING GUN TYPE TABLE DATA'//)                                      FORMA364
  707 FORMAT(//T15,'.....     ERROR     .....',I3,' GUN TYPES EXCEED TABFORMA365
     1LE LIMIT'//)                                                      FORMA366
  708 FORMAT(//T15,'.....     ERROR     .....BLANK TARGET ID,OMNI/KILL FFORMA367
     1LAG NOT SET FOR SOURCE AT FIRING PT(',F9.1,',',F9.1,')'//)        FORMA368
709   FORMAT(//T15,' ...ERROR.  ',I3,' TARGET TYPES EXCEED TABLE',
     1' LIMIT...'//)
 721  FORMAT(/,T15,'TOTAL PER DAY DAY EXPLOSIONS IS   ',F10.2,          FORMA369
     1/,T15,'TOTAL PER DAY NIGHT EXPLOSIONS IS ',F10.2)                 FORMA370
 900  FORMAT(//,T15,'.....     TIME FOR FORM-A SUBPROGRAM IS ',F8.3,    FORMA371
     1' SECONDS')                                                       FORMA372
      END                                                               FORMA373
