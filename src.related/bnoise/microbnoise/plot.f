C$NOFLOATCALLS
C  	INTERFACE  TO SUBROUTINE TIME (N,STR)
C  	CHARACTER*10 STR [NEAR,REFERENCE]
C  	INTEGER*2 N [VALUE]
C   END
C  	INTERFACE TO SUBROUTINE  DATE(N,STR)
C  	CHARACTER*10 STR [NEAR,REFERENCE]
C  	INTEGER*2 N [VALUE]
C  	END
      SUBROUTINE PLOT                                                   PLOT   2
	CHARACTER*9  TAPE(4)
	CHARACTER*2  STARS
	CHARACTER*1  STAR,TEXT,PLT(80)
	CHARACTER*10 TEMP,MEASURE,HEAD,Q,C,B,GRDNAME,ITEXT
C    FILE USAGE CARD                                                    PLOT   3
C        PUDG=PUDDLEGRID                                                PLOT   4
C        LOC=LOCATION                                                   PLOT   5
C        SCAT=SCATTER                                                   PLOT   6
C        BAS=BASE                                                       PLOT   7
C    TAPE NAMES                                                         PLOT   8
C        IN1 =PUDDLE GRID TAPE                                          PLOT   9
C        IN2 =BASE TAPE                                                 PLOT  10
C        IN3 =LOCATION TAPE                                             PLOT  11
C        IN4 =SCATTER TAPE                                              PLOT  12
      COMMON/IO/KARD,KPRINT                                             PLOT  13
      COMMON/PLOTCM/PLTCT,SXTABLE,SYTABLE,LARGE                         PLOT  14
      COMMON/BOUND/XMIN1,YMIN1,XMAX1,YMAX1,BDS                          PLOT  15
      LOGICAL BDS,LARGE                                                 PLOT  16
      COMMON /METRIC/METERS                                             PLOT  17
      LOGICAL METERS                                                    PLOT  18
      REAL MAG                                                          PLOT  19
      DIMENSION STARS(12)                                               PLOT  20
      DIMENSION HEAD(4)                                                 PLOT  21
      DIMENSION IFILE(4)                                                PLOT  22
      DIMENSION ITEXT(3)                                                PLOT  23
      INTEGER PLOTX ,PLOTY                                              PLOT  24
      DIMENSION TEXT(200)
      DIMENSION HGT(200)                                                PLOT  26
      INTEGER START,STOP,LSTART,LSTOP,LABEL,LINC                        PLOT  27
      INTEGER PLTCT,PUDG,SCAT,LOC,BAS                                   PLOT  28
      DIMENSION MEASURE(2)                                              PLOT  29
      DIMENSION IUNIT(4)                                                PLOT  30
      EQUIVALENCE (IUNIT(1),IN1),(IUNIT(2),IN2),(IUNIT(3),IN3),(IUNIT(4)PLOT  31
     1,IN4)                                                             PLOT  32
      EQUIVALENCE (IFILE(1),PUDG),(IFILE(2),BAS),(IFILE(3),LOC),(IFILE(4PLOT  33
     1),SCAT)                                                           PLOT  34
	DATA TAPE/'TAPE1.DAT','TAPE2.DAT','TAPE3.DAT','TAPE4.DAT'/
      DATA MEASURE/ 'METERS    ', 'FEET      '/                         PLOT  35
      DATA HEAD / 'PGRID     ', 'BASE      ', 'LOCATOR   ','SCATTER  '/ PLOT  36
C                                                                       PLOT  37
C    VARIABLE IOUT MUST BE INITIALIZED TO OUTPUT UNIT NUMBER            PLOT  38
C                                                                       PLOT  39
      DATA Z/0./,IOUT/55/,IN1/1/,IN2/2/,IN3/3/,IN4/4/,IC1/0/            PLOT  40
C     TAPE55 IS PLOT INPUT FOR NASAPLOT 
      CPTIME=SECOND(CP)                                                 PLOT  41
      WRITE(KPRINT,150)                                                 PLOT  42
 150  FORMAT(1H1,/30X,30H.......      PLOT      .......)                PLOT  43
C     COUNT NUMBER OF PLOTS DESIRED 
      PLTCT=PLTCT+1                                                     PLOT  83
      IFLAG=0                                                           PLOT  44
C     EXECUTE THE FOLLOWING INTERNAL ROUTINES 
      CALL DATE(10,B)                                                      
      CALL TIME(10,C)                                                      
      CALL GETJN(Q)                                                       
      WRITE(KPRINT,181)Q,C,B,PLTCT                                      PLOT  48
 181  FORMAT(1H0,28X,A10,2X,A10,2X,A10,4X,4HPLOT,I3)                    PLOT  49
      GRDSZ=0.                                                          PLOT  50
      DO 11 I=1,12                                                      PLOT  51
11	STARS(I)='  '
C     READ PLT-2 CARD WITH MODULES IN USE INDICATED 
      READ(KARD,20)PUDG,LOC,SCAT,BAS                                    PLOT  53
 20   FORMAT(4I1)                                                       PLOT  54
      IF(BDS .OR. (PUDG .EQ. 1)) GO TO 17 
      WRITE(KPRINT,15)
15    FORMAT('  ...NEED BOUNDS OR PGRID CALLS BEFORE PLOT...')
      STOP
C                                                                       PLOT  56
C        THE FOLLOWING IS USED TO DETERMINE IF REQUESTED FILES          PLOT  57
C                     ARE AVAILABLE FOR PROCESSING                      PLOT  58
C                                                                       PLOT  59
17    JFLAG=0                                                           PLOT  60
      DO 3 I=1,4                                                        PLOT  61
C     IF FILE NOT REQUESTED, GO TO NEXT ONE 
      IF(IFILE(I).EQ.0)GO TO 3                                          PLOT  62
      ITEST=IUNIT(I)                                                    PLOT  63
	OPEN(ITEST,FILE=TAPE(I))
      REWIND ITEST                                                      PLOT  64
      READ(ITEST,2,END=6)                                                     
 2    FORMAT(A1)                                                        PLOT  66
      REWIND ITEST                                                      PLOT  68
	CLOSE(ITEST)
 3    CONTINUE                                                          PLOT  69
      IF(JFLAG.EQ.1) STOP                                               PLOT  70
      GO TO 1111                                                        PLOT  71
 6    IF(JFLAG.EQ.1) GO TO 8                                            PLOT  72
      WRITE(KPRINT,7)                                                   PLOT  73
 7    FORMAT(1H0,20X,'THE FOLLOWING FILES WERE REQUESTED BUT NOT AVAILABPLOT  74
     1LE'/21X,'...JOB ABORTED...'/)                                     PLOT  75
 8    WRITE(KPRINT,9) HEAD(I)                                           PLOT  76
 9    FORMAT(25X,A10)                                                   PLOT  77
      JFLAG=1                                                           PLOT  78
      STOP                                                              PLOT  79
C     WRITE OUT NAME OF REQUESTED FILE TO TAPE 55 
 1111 WRITE(IOUT,10)'JOB '                                              PLOT  80
      WRITE(IOUT  ,10) 'PHS1'                                           PLOT  81
 10   FORMAT(A4)                                                        PLOT  82
C     READ PLT-3 CARD WITH PLOT SPECIFICATIONS
      READ(KARD,100,END=106)SCALE,PERCX,PERCY,MAG,PERCSM,START,STOP,     
     1LSTART,LSTOP,LABEL,INC,LINC,XI,XF,YI,YF
100   FORMAT(F7.0,4F4.0,4I3,4I2,4F9.1)
C i added following two cards
	  IDUMP = 0
	  IFLAG = 1
C    1LSTART,LSTOP,LABEL,INC,LINC,IDUMP,XI,XF,YI,YF
	GO TO 111
C     PLT-3 CARD IS BLANK 
106	SCALE=0.
	PERCX=0.
	PERCY=0.
	MAG=0.
	PERCSM=0.
	START=0
	STOP=0
	LSTART=0
	LSTOP=0
	LABEL=2
	INC=0
	LINC=0
      IFLAG=1                                                           PLOT  90
C                                                                       PLOT  91
C                                                                       PLOT  92
C        DEFAULT ROUTINES                                               PLOT  93
C                                                                       PLOT  94
C                                                                       PLOT  95
C    SCALE - DEFAULT 50000                                              PLOT  96
111   IF (SCALE.LT.100.0)  SCALE=50000.0
      IF(SCALE.EQ.50000.)STARS(1)='**'                                  PLOT  98
C    PERCX - DEFAULT 1.0                                                PLOT  99
      IF((PERCX.GT.9.0).OR.(PERCX.LT.0.01))PERCX=1.0                    PLOT 100
      IF(PERCX.EQ.1.0) STARS(2)='**'                                    PLOT 101
C    PERCY - DEFAULT 1.0                                                PLOT 102
      IF((PERCY.GT.9.0).OR.(PERCY.LT.0.01))PERCY=1.0                    PLOT 103
      IF(PERCY.EQ.1.0) STARS(3)='**'                                    PLOT 104
C    MAG - DEFAULT 1.0                                                  PLOT 105
      IF((MAG.GT.9.0).OR.(MAG.LT. 0.01)) MAG=1.0                        PLOT 106
      IF(MAG.EQ.1.0) STARS(4)='**'                                      PLOT 107
C    PERCSM DEFAULT .333                                                PLOT 108
      IF((PERCSM.GT.9.0).OR.(PERCSM.LT. 0.01)) PERCSM=.333              PLOT 109
      IF(PERCSM.EQ..333) STARS(5)='**'                                  PLOT 110
C    START - DEFAULT 65 
      IF((START.GT.999).OR.(START.LT.1)) START=65 
      IF(START.EQ.65) STARS(6)='**' 
C    STOP - DEFAULT 75
      IF((STOP.GT.999).OR.(STOP.LT.1)) STOP=75
      IF(STOP.EQ.75) STARS(7)='**'
C    LSTART - DEFAULT 65
      IF((LSTART.GT.999).OR.(LSTART.LT.1)) LSTART=65
      IF(LSTART.EQ.65) STARS(8)='**'
C    LSTOP - DEFAULT 75 
      IF((LSTOP.GT.999).OR.(LSTOP.LT.1)) LSTOP=75 
      IF(LSTOP.EQ.75) STARS(9)='**' 
C    LABEL - DEFAULT 1                                                  PLOT 123
      IF((LABEL.NE.-1).AND.(LABEL.EQ.0).AND.(LABEL.NE.1)) LABEL=1       PLOT 124
      IF(LABEL.EQ.1)     STARS(10)='**'                                 PLOT 125
C    INC - DEFAULT 10                                                    PLOT 126
      IF((INC.GT.99).OR.(INC.LE.0)) INC=10                               PLOT 127
      IF(INC.EQ.10) STARS(11)='**'                                       PLOT 128
C    LINC- DEFAULT10                                                     PLOT 129
      IF((LINC.GT.99).OR.(LINC.LE.0)) LINC=10                            PLOT 130
      IF(LINC.EQ.10) STARS(12)='**'                                      PLOT 131
C    INPUT VALIDITY CHECK                                               PLOT 132
      WRITE(KPRINT,151)                                                 PLOT 133
 151  FORMAT(1H0,20X,30HFOLLOWING FILES WERE REQUESTED/)                PLOT 134
      DO 153 I=1,4                                                      PLOT 135
      IF(IFILE(I).EQ.1) WRITE(KPRINT,152) HEAD(I)                       PLOT 136
 152  FORMAT(25X,A10)                                                   PLOT 137
 153  CONTINUE                                                          PLOT 138
C     CHECK PLOT BOUNDS AND LABEL BOUNDS
      IF(START.GE.STOP) GO TO 300                                       PLOT 139
      IF(LSTART.GE.LSTOP) GO TO 320                                     PLOT 140
      I5=0                                                              PLOT 141
 200  J1=MOD(START,INC)                                                 PLOT 142
      J2=MOD(STOP,INC)                                                  PLOT 143
      IF(J1.EQ.J2) GO TO 210                                            PLOT 144
      I5=1                                                              PLOT 145
      STOP=STOP+1                                                       PLOT 146
      GO TO 200                                                         PLOT 147
 210  IF (I5.EQ.1) WRITE(KPRINT,220) STOP                               PLOT 148
 220  FORMAT(15X,'WARNING..START + (INC * X ) .NE. STOP',/15X,'STOP INCRPLOT 149
     1EASED TO ',I3)                                                    PLOT 150
 230  J1=MOD(LSTART,INC)                                                PLOT 151
      J2=MOD(LSTOP,INC)                                                 PLOT 152
      IF(J1.EQ.J2) GO TO 240                                            PLOT 153
      I5=2                                                              PLOT 154
      LSTOP=LSTOP+1                                                     PLOT 155
      GO TO 230                                                         PLOT 156
 240  IF(I5.EQ.2) WRITE(KPRINT,250) LSTOP                               PLOT 157
 250  FORMAT(15X,'WARNING..LSTART+(INC*X) .NE. LSTOP',/15X,'LSTOP INCREAPLOT 158
     1SED TO ',I3)                                                      PLOT 159
      GO TO 105                                                         PLOT 160
 300  WRITE(KPRINT,310)                                                 PLOT 161
 310  FORMAT('START .GE.STOP...JOB ENDED')                              PLOT 162
      REWIND IOUT                                                       PLOT 163
      GO TO 900                                                         PLOT 164
 320  WRITE(KPRINT,330)                                                 PLOT 165
 330  FORMAT('LSTART .GE. LSTOP...JOB ENDED')                           PLOT 166
      REWIND IOUT                                                       PLOT 167
      GO TO 900                                                         PLOT 168
C                                                                       PLOT 169
C                                                                       PLOT 170
C    DEFAULT AND ERRORS CHECKED - CREATE CARDS                          PLOT 171
C                                                                       PLOT 172
C                                                                       PLOT 173
C                                                                       PLOT 174
C        THE FOLLOWING COMPARES BOUNDARY VALUES FROM COMMON             PLOT 175
C        WITH THOSE READ FROM PGRID TAPE. IF DIFFERENT PGRID            PLOT 176
C        VALUES USED.                                                   PLOT 177
C                                                                       PLOT 178
 105  IF(PUDG.NE.1) GO TO 108                                           PLOT 179
	OPEN(IN1,FILE=TAPE(1))
C     PGRID INFORMATION IS FOUND ON TAPE 1(IN1) 
      REWIND IN1                                                        PLOT 180
      READ(IN1,166)GRDNAME,GRDSZ,XMIN,YMIN,XMAX,YMAX                    PLOT 181
 166  FORMAT(A10,5F10.0)                                                PLOT 182
      IF((XMIN.EQ.XMIN1).AND.(YMIN.EQ.YMIN1).AND.(XMAX.EQ.XMAX1)        PLOT 183
     1.AND.(YMAX.EQ.YMAX1)) GO TO 109                                   PLOT 184
      WRITE(KPRINT,107) XMIN,XMIN1,YMIN,YMIN1,XMAX,XMAX1,YMAX,YMAX1     PLOT 185
 107  FORMAT(1H0, 7X,'WARNING...PUDDLE GRID BOUNDS DO NOT MATCH SPECIFIEPLOT 186
     1D BOUNDS...'//25X,'PGRID VALUES USED',10X,'SPECIFIED BOUNDS'/     PLOT 187
     115X,'XMIN',6X,F10.2,17X,F10.2/                                    PLOT 188
     115X,'YMIN',6X,F10.2,17X,F10.2/                                    PLOT 189
     115X,'XMAX',6X,F10.2,17X,F10.2/                                    PLOT 190
     115X,'YMAX',6X,F10.2,17X,F10.2)                                    PLOT 191
      GO TO 109                                                         PLOT 192
 108  XMIN =XMIN1                                                       PLOT 193
      YMIN=YMIN1                                                        PLOT 194
      XMAX=XMAX1                                                        PLOT 195
      YMAX=YMAX1                                                        PLOT 196
109   XSCALE=SCALE/PERCX/12.0 
      YSCALE=SCALE/PERCY/12.0 
      IF (METERS) XSCALE=XSCALE*0.02540 *12.0 
      IF (METERS) YSCALE=YSCALE*0.02540 *12.0 
      XOR=3.0*MAG                                                       PLOT 199
	XOR=0.5
      YOR=0.5                                                           PLOT 200
C     WRITE OUT ON TAPE 55 INFORMATION ABOUT BOUNDS, MAGNIFICATION, 
C     AND SCALING 
      WRITE(IOUT  ,110)'MAPS',XSCALE,YSCALE,XMIN,XMAX,YMIN,YMAX,XOR,YOR PLOT 201
 110  FORMAT(A4,2X,2F8.3,4F8.0,2F8.3)                                   PLOT 202
      WRITE(IOUT ,115) 'SCLE',MAG                                       PLOT 203
 115  FORMAT(A4,2X,F8.3)                                                PLOT 204
C     IF(SXTABLE.NE.0) GO TO 116                                        PLOT 205
      SXTABLE=(XMAX-XMIN)/XSCALE*PERCX*MAG+XOR+5.                       PLOT 206
	SYTABLE=11.
	SXTABLE=10.2
	SYTABLE=7.5
      IF(.NOT. LARGE) GOTO 117
      IF(((YMAX-YMIN)/YSCALE*PERCY*MAG+YOR) .GT. 11.) SYTABLE=34.5
C 
117	WRITE(IOUT,120)'DIMT',XI,XF,YI,YF
 120  FORMAT(A4,2X,4F8.3)                                               PLOT 212
C                                                                       PLOT 215
C                                                                       PLOT 216
C    IF 2ND PLOT                                                        PLOT 217
C                                                                       PLOT 218
C                                                                       PLOT 219
116   IF(PLTCT.EQ.1) GO TO 395                                          PLOT 220
      WRITE(IOUT ,301) 'PAGE'                                           PLOT 221
 301  FORMAT(A4)                                                        PLOT 222
C                                                                       PLOT 223
C                                                                       PLOT 224
C    IF PUDDLEGRID                                                      PLOT 225
C                                                                       PLOT 226
C                                                                       PLOT 227
 395  IF(PUDG.NE.1) GO TO 495                                           PLOT 228
 400  READ(IN1,99,END=430) PLT                                                      
410   WRITE(IOUT,99)  PLT
      IF (IDUMP.NE.0)  WRITE(KPRINT,99) 
      GO TO 400                                                         PLOT 232
 430  WRITE(IOUT ,440) 'PHS3'                                           PLOT 233
 440  FORMAT(A4)                                                        PLOT 234
      ZMIN=START                                                        PLOT 235
      ZLMIN=LSTART                                                      PLOT 236
      D2=INC                                                            PLOT 237
      D2L=LINC                                                          PLOT 238
      NLEVS=(STOP-START)/D2+1.0001                                      PLOT 239
      KLEV=0                                                            PLOT 240
      L1=LABEL                                                          PLOT 241
      L2=1                                                              PLOT 242
      ID=-1                                                             PLOT 243
      DISL=2.0                                                          PLOT 244
      HGTL=.333                                                         PLOT 245
      DIST=0                                                            PLOT 246
      TLNG=0                                                            PLOT 247
      TLER=(PERCSM*2)/(XSCALE+YSCALE)*GRDSZ                             PLOT 248
      I=2                                                               PLOT 249
      SKIP=0                                                            PLOT 250
C     WRITE OUT PGRID INFO INTO PHS3 OF TAPE 55 
      WRITE(IOUT ,441)'PLOT',ZMIN,ZLMIN,D2,D2L,NLEVS,KLEV,L1,L2,ID,DISL,PLOT 251
     1HGTL,DIST,TLNG,TLER,I,SKIP                                        PLOT 252
 441  FORMAT(A4,2X,2F7.0,2F6.0,I5,I4,3I2,5F4.2,I1,F4.2)                 PLOT 253
	CLOSE(IN1)
C                                                                       PLOT 254
C                                                                       PLOT 255
C    IF LOCATION                                                        PLOT 256
C                                                                       PLOT 257
C                                                                       PLOT 258
 495  IF(LOC.NE.1) GO TO 530                                            PLOT 259
	OPEN(IN3,FILE=TAPE(3))
C     TAPE 3 CONTAINS INFORMATION FROM LOCATOR
      REWIND IN3                                                        PLOT 260
      READ(IN3,99) PLT                                                      
      WRITE(IOUT,99) PLT                                                    
 500  READ(IN3,501,END=528) XPOS,YPOS,HITE,ANG,(ITEXT(I),I=1,3),L               
 501  FORMAT(6X,2F8.0,8X,2F8.3,2X,2A10,A4,I2)                           PLOT 264
C                                                                       PLOT 266
C        THE FOLLOWING CENTERS THE X OR O ON THE POINT. OTHERWISE       PLOT 267
C        THE POINT WOULD BE AT THE LOWER LEFT CORNER OF THE MARK.       PLOT 268
C                                                                       PLOT 269
      ANG1=ANG*3.1416/180.0                                             PLOT 270
      XOFF=.5*(6./7.*HITE)*XSCALE                                       PLOT 271
      YOFF=.5*(HITE)*YSCALE                                             PLOT 272
      YPRIME=(SQRT(XOFF**2+YOFF**2))*                                   PLOT 273
     1      (SIN(ANG1+(ATAN(YOFF/XOFF))))                               PLOT 274
      XPRIME=(SQRT(XOFF**2+YOFF**2))*                                   PLOT 275
     1      (COS(ANG1+(ATAN(YOFF/XOFF))))                               PLOT 276
      XPOS=XPOS-XPRIME                                                  PLOT 277
      YPOS=YPOS-YPRIME                                                  PLOT 278
C                                                                       PLOT 279
C        THE FOLLOWING CHECKS FOR ENOUGH ROOM TO PRINT THE              PLOT 280
C        LABELING INFORMATION WITHIN THE BOUNDARY.                      PLOT 281
C                                                                       PLOT 282
      LENTH=L*6./7.*HITE*XSCALE                                         PLOT 283
C        IF ENOUGH ROOM GIVEN ANGLE                                     PLOT 284
      IF(((XPOS+LENTH*COS(ANG1)).GT.XMAX).OR.                           PLOT 285
     1((YPOS+LENTH*SIN(ANG1)).GT.YMAX).OR.                              PLOT 286
     1((YPOS+LENTH*SIN(ANG1)).LT.YMIN).OR.                              PLOT 287
     1((XPOS+LENTH*COS(ANG1)).LT.XMIN)) GO TO 21                        PLOT 288
      GO TO 51                                                          PLOT 289
C        IF ENOUGH ROOM 90 DEGREES                                      PLOT 290
 21   IF((YPOS+LENTH).GT.YMAX) GO TO 22                                 PLOT 291
      ANG=90.0                                                          PLOT 292
      GO TO 51                                                          PLOT 293
C        IF ENOUGH ROOM 270 DEGREES                                     PLOT 294
 22   IF((YPOS-LENTH).LT.YMIN) GO TO 23                                 PLOT 295
      ANG=270.0                                                         PLOT 296
      GO TO 51                                                          PLOT 297
C        IF ENOUGH ROOM 180 DEGREES                                     PLOT 298
 23   IF((XPOS-LENTH).LT.XMIN) GO TO 49                                 PLOT 299
      ANG=180.0                                                         PLOT 300
      GO TO 51                                                          PLOT 301
 49   GO TO 51                                                          PLOT 302
C     WRITE OUT LOC INFO TO TAPE 55 
  51  WRITE(IOUT,502)XPOS,YPOS,HITE,ANG,(ITEXT(I),I=1,3)                PLOT 303
 502  FORMAT(4HTEXT,2X,2F8.0,8X,2F8.3,2H 1,2A10,A4)                     PLOT 304
      GO TO 500                                                         PLOT 305
528	CLOSE(IN3)
C                                                                       PLOT 306
C                                                                       PLOT 307
C    IF SCATTER                                                         PLOT 308
C                                                                       PLOT 309
C                                                                       PLOT 310
 530  IF(SCAT.NE.1) GO TO 560                                           PLOT 311
C     TAPE 4 CONTAINS INFORMATION FROM SCATTER
	OPEN(IN4,FILE=TAPE(4))
      REWIND IN4                                                        PLOT 312
 535  READ(IN4,99,END=558) PLT                                                      
C     WRITE OUT SCATTER INFO TO TAPE 55 
 545  WRITE(IOUT,99) PLT                                                    
      GO TO 535                                                         PLOT 316
558	CLOSE(IN4)
C                                                                       PLOT 317
C                                                                       PLOT 318
C     IF BASE                                                           PLOT 319
C                                                                       PLOT 320
C                                                                       PLOT 321
 560  IF(BAS.NE.1) GO TO 580                                            PLOT 322
C     TAPE 2 CONTAINS INFORMATION FROM BASE 
	OPEN(IN2,FILE=TAPE(2))
      REWIND IN2                                                        PLOT 323
 565  READ(IN2,99,END=580) PLT                                                      
 572  WRITE(IOUT,99) PLT                                                    
99	FORMAT(80A1)
      GO TO 565                                                         PLOT 330
 580  WRITE(KPRINT,575)                                                 PLOT 331
	CLOSE(IN2)
 575  FORMAT(1H0,25X,20HVALUES USED BY PLOT /)                          PLOT 332
      WRITE(KPRINT,590) STARS(1),SCALE,STARS(2),PERCX,STARS(3),PERCY    PLOT 333
     1,STARS(4),MAG,STARS(5),PERCSM,STARS(6),START,STARS(7),STOP,       PLOT 334
     1STARS(8),LSTART,STARS(9),LSTOP,STARS(10),LABEL,STARS(11),INC,     PLOT 335
     1STARS(12),LINC,GRDSZ                                              PLOT 336
590   FORMAT(18X,A2,10HSCALE    =,F7.1//                                PLOT 337
     118X,A2,10HPERCENT X=,F4.2,10X,A2,10HPERCENT Y=,F4.2//             PLOT 338
     118X,A2,10HMAG      =,F4.2,10X,A2,10HPERC SMTH=,F4.2//             PLOT 339
     118X,A2,10HSTART    =,I3,11X,A2,10HSTOP     =,I3//                 PLOT 340
     118X,A2,10HL START  =,I3,11X,A2,10HL STOP   =,I3//                 PLOT 341
     118X,A2,10HLABEL    =,I2,12X,A2,10HINCREMENT=,I2//                 PLOT 342
     118X,A2,10HL INCREMT=,I2,14X,10HGRID SIZE=,F6.0)                   PLOT 343
      WRITE(KPRINT,13)                                                  PLOT 344
 13   FORMAT(1H0,18X,29HSTARS INDICATE DEFAULT VALUES)                  PLOT 345
C                                                                       PLOT 346
C                                                                       PLOT 347
C     PRINT ANY USER TEXT                                               PLOT 348
C                                                                       PLOT 349
C                                                                       PLOT 350
      WRITE(IOUT  ,690) 'PHS4'                                          PLOT 351
      WRITE(IOUT,690)'BRDR'                                             PLOT 352
 690  FORMAT(A4)                                                        PLOT 353
      IF(IFLAG.EQ.1) GO TO 800                                          PLOT 354
      WRITE(KPRINT,156)                                                 PLOT 355
 156  FORMAT(1H0,18X,36HFOLLOWING CARDS WERE USER TEXT INPUT)           PLOT 356
C     READ PLT-4 CARD WITH USER TEXT INPUT
	  WRITE(6,2000)IFLAG
2000  FORMAT(7HIFLAG= ,I1)
 700  READ(KARD,705,END=800)X,Y,HT,ANGLE,IC,(TEXT(I),I=1,38),STAR               
      ANGLE1=ANGLE*3.1416/180.0                                         PLOT 358
	  WRITE(6,2001)
2001  FORMAT(6HPLT4  ,11HGOT CALLED )
      DO 701 I=1,200                                                    PLOT 359
 701  HGT(I)=0.0                                                        PLOT 360
 705  FORMAT(4F10.0,I1,39A1)                                            PLOT 361
      IF((STAR.EQ.'*').AND.(X.EQ.0.)) GO TO 800                          
      IFLAG=2                                                           PLOT 364
      WRITE(KPRINT,157)X,Y,HT,ANGLE,IC,(TEXT(I),I=1,38),STAR            PLOT 365
 157  FORMAT(1H0,18X,1H",4F10.3,I1,39A1,1H")                            PLOT 366
      DO 720 J=1,38                                                     PLOT 367
C     IF $ CONTINUE TEXT ON NEXT CARD(PLT-5)
      IF(TEXT(J).EQ.'$') GO TO 730                                      PLOT 368
 720  CONTINUE                                                          PLOT 369
      J=38                                                              PLOT 370
 730  HGT(1)=HT                                                         PLOT 371
      IF (STAR.EQ.'*') GO TO 801                                        PLOT 372
 735  K=J+68                                                            PLOT 373
C     READ PLT-5 CARD 
      READ(KARD,740) HT1,(TEXT(I),I=J,K),STAR                           PLOT 374
      WRITE(KPRINT,158)HT1,(TEXT(I),I=J,K),STAR                         PLOT 375
  158 FORMAT(1H ,18X,F10.3,70A1)                                        PLOT 376
 740  FORMAT(F10.0,70A1)                                                PLOT 377
      IF((HT1.EQ.HT).OR.(HT1.EQ.0)) GO TO 745                           PLOT 378
      HGT(J)=HT1                                                        PLOT 379
 745  DO 750 J2=J,K                                                     PLOT 380
      HT=HT1                                                            PLOT 381
      IF(TEXT(J2).EQ.'$') GO TO 760                                     PLOT 382
 750  CONTINUE                                                          PLOT 383
      J2=K                                                              PLOT 384
 760  J=J2                                                              PLOT 385
C     MAYBE MORE TEXT INPUT 
      IF(STAR.NE.'*') GO TO 735                                         PLOT 386
 801  K1=1                                                              PLOT 387
      DO 850 K=2,J                                                      PLOT 388
	I1=0
	I2=0
	I3=0
      IF(K.EQ.J) GO TO 815                                              PLOT 390
      IF(HGT(K).EQ.0) GO TO 850                                         PLOT 391
 815  K2=K-K1                                                           PLOT 392
C   K2=NUMBER OF CHARACTERS - THIS HEIGHT                               PLOT 393
C    K1= POSITION OF HEIGHT AND STARTING CHARACTER                      PLOT 394
      IF(K2.GT.24) GO TO 820                                            PLOT 395
      I1=K1+K2-1                                                        PLOT 396
      GO TO 845                                                         PLOT 397
 820  IF(K2.GT.90) GO TO 830                                            PLOT 398
      I1=K1+23                                                          PLOT 399
      I2=K2+K1-1                                                        PLOT 400
      GO TO 845                                                         PLOT 401
 830  I1=23+K1                                                          PLOT 402
      I2=K1+89                                                          PLOT 403
      I3=K2                                                             PLOT 404
C CHOOSE FORMAT ACCORDING TO WHETHER OR NOT IC=0 OR IC=1
      IF(IC.NE.0) GO TO 779 
 845  WRITE(IOUT  ,780)'TEXT',X,Y,Z,HGT(K1),ANGLE,IC,(TEXT(I),I=K1,I1)  PLOT 405
 780  FORMAT(A4,2X,5F8.3,I2,24A1)                                       PLOT 406
      GO TO 782 
  779 WRITE(IOUT,781)'TEXT',X,Y,Z,HGT(K1),ANGLE,IC,(TEXT(I),I=K1,I1)
  781 FORMAT(A4,2X,2F8.0,3F8.3,I2,24A1) 
  782 IF(I2.EQ.0) GO TO 840 
      I1=I1+1                                                           PLOT 408
C     WRITE USER TEXT TO TAPE 55
      WRITE(IOUT,790) 'CTEX',(TEXT(I),I=I1,I2)                          PLOT 409
      IF(I3.EQ.0) GO TO 840                                             PLOT 410
      I2=I2+1                                                           PLOT 411
      WRITE(IOUT  ,790)'CTEX',(TEXT(I),I=I2,I3)                         PLOT 412
 790  FORMAT(A4,2X,66A1)                                                PLOT 413
 840  X=X+K2*(6./7.) *HGT(K1)*COS(ANGLE1)                               PLOT 414
      Y=Y+K2*(6./7.)*HGT(K1)*SIN(ANGLE1)                                PLOT 415
      K1=K                                                              PLOT 416
 850  CONTINUE                                                          PLOT 417
      GO TO 700                                                         PLOT 418
C                                                                       PLOT 419
C                                                                       PLOT 420
C     PRINT IDENTIFYING TEXT (TIME OF DAY)                              PLOT 421
C                                                                       PLOT 422
C                                                                       PLOT 423
 800  PLOTY=((YMAX-YMIN)+GRDSZ)/YSCALE*PERCY*MAG+YOR                    PLOT 424
      PLOTX=(XMAX-XMIN)/XSCALE*PERCX*MAG+XOR                            PLOT 425
      IF(IFLAG.EQ.1) WRITE(KPRINT,802)                                  PLOT 426
      IF((IFLAG.NE.2).AND.(IFLAG.NE.1)) WRITE(KPRINT,802)               PLOT 427
 802  FORMAT(1H0,18X,24HNO USER TEXT CARDS INPUT)                       PLOT 428
      WRITE(KPRINT,159)PLOTX,PLOTY                                      PLOT 429
 159  FORMAT(1H0,18X,12HTHIS PLOT IS,I4,10H INCHES BY,I4,7H INCHES)     PLOT 430
      LTN=2                                                             PLOT 431
C     SCALE IF MEASUREMENTS ARE IN METERS 
      IF(METERS) LTN=1                                                  PLOT 432
      FAKTOR=(XSCALE+YSCALE)/2/MAG                                      PLOT 433
      WRITE(KPRINT,149)FAKTOR,MEASURE(LTN)                              PLOT 434
 149  FORMAT(1H0,18X,20HONE INCH IS EQUAL TO,F7.0,1X,A10)               PLOT 435
      DO 630 J1=1,50                                                    PLOT 436
      IF(PLOTX.LE.J1*10.2)GO TO 633                                     PLOT 437
 630  CONTINUE                                                          PLOT 438
 633  DO 634 J=1,10                                                     PLOT 439
      IYSZ=SYTABLE+0.4
      IF(PLOTY.LE.J*7.5 ) GO TO 635                                     PLOT 440
 634  CONTINUE                                                          PLOT 441
635	Z1=0
	X1=1.0*MAG
	X1=0.15
	ANGLE=90.0
	HT=.3*MAG
	HT=0.14
      X2=2.0*MAG                                                        PLOT 443
	X2=0.30
      CALL DATE(10,B)                                                      
      CALL GETJN(Q)                                                     PLOT 445
      DO 645  I=1,J                                                     PLOT 446
      Y1=((I-1)*IYSZ+1)                                                 PLOT 447
C     WRITE OUT HEIGHT, ANGLE MEASUREMENTS TO TAPE 55 
      WRITE(IOUT,640)X1,Y1,Z1,HT,ANGLE,IC1,Q,B                          PLOT 448
 640  FORMAT(4HTEXT,2X,5F8.3,I2,2A10,2X,2HPL)                           PLOT 449
C     WRITE OUT NUMBER OF PLOTS REQUESTED TO TAPE 55
      WRITE(IOUT,641)PLTCT,I,J                                          PLOT 450
 641  FORMAT(4HCTEX,2X,2HOT,I2,3X,I1,1X,2HOF,1X,I1)                     PLOT 451
      WRITE(IOUT,642)X2,Y1,Z1,HT,ANGLE,IC1,SCALE                        PLOT 452
 642  FORMAT(4HTEXT,2X,5F8.3,I2,8HSCALE 1:,F7.0,2X,7H1 INCH=)
      WRITE(IOUT,643)FAKTOR,MEASURE(LTN)
 643  FORMAT(4HCTEX,2X,F6.0,1X,A10)                                     PLOT 455
 645  CONTINUE                                                          PLOT 456
      WRITE(IOUT,301)'END '                                             PLOT 457
      WRITE(KPRINT,160) J1,J                                            PLOT 458
 160  FORMAT(1H0,18X,14HIT CONSISTS OF,I3,' PAGES IN THE X DIRECTIO',   PLOT 459
     1'N AND',I3,' SECTIONS IN THE Y DIRECTION')
C     COMPUTE TIME IN PLOT
      CPTIM11=SECOND(CP)                                                PLOT 461
      CPTIME=CPTIM11-CPTIME                                             PLOT 462
      WRITE(KPRINT,161)CPTIME                                           PLOT 463
 161  FORMAT(1H0,20X,21H..... TIME IN PLOT IS,F8.3,6H .....)            PLOT 464
 900  RETURN                                                            PLOT 465
      END                                                               PLOT 466
