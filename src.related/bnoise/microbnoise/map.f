C$NOFLOATCALLS
C$LARGE
      SUBROUTINE MAP                                                    MAP    2
	INTEGER  SHIFT
	CHARACTER*10 NAME1,NAME2
	CHARACTER*1 IFLAG
C                                                                       MAP    3
C     THIS SUBROUTINE PRODUCES A MAP OF THE POINTS                      MAP    4
C                                                                       MAP    5
      COMMON/FT/TXFPT 
      COMMON/IO/KARD,KPRINT                                             MAP    7
      COMMON/GRID/GTXFPT
	COMMON/CONTR/IDGUN,CONTOR
	COMMON/TABL1/TXGT,XREF,FCORDS,ADFPT,CHARGE,ADTRG,TCORDS,
     $		     NAME1,NAME2,NUL
	INTEGER*2 NTCOLS,NDCOLS
      DIMENSION IDGUN(50),CHARGE(11,50),IDTRG(50),IDFPT(300),NAME1(50), 
     1 TCORDS(50,2),FCORDS(300,2),TXFPT(50,300),TXGT(50,50),GTXFPT(50,  MAP   10
     2  300)                                                            MAP   11
     3  ,ADGUN(50),ADTRG(50),ADFPT(300),XREF(2500),NAME2(50)
      EQUIVALENCE (ADGUN(1),IDGUN(1)),(ADTRG(1),IDTRG(1)),(ADFPT(1),    MAP   13
     1   IDFPT(1))                                                      MAP   14
      DIMENSION NUL(80),NTCOLS(50),NDCOLS(49) 
      DIMENSION CONTOR(50,15) 
      DIMENSION IN(3)                                                   MAP   21
      COMMON/DEBUG/CHECK,REED                                           MAP   22
      LOGICAL CHECK,REED                                                MAP   23
      LOGICAL REOF                                                      MAP   24
      LOGICAL PRNT                                                      MAP   25
      LOGICAL OVRFL,IMAGE,POS                                           MAP   26
      LOGICAL DUP                                                       MAP   27
      LOGICAL LO1                                                       MAP   28
C XREF FLAGS;0=NO TABLE;1= TABLE;2= INCOMPLETE TABLE DUE TO OVERFLOW    MAP   29
      INTEGER XTF,XTG,XGF                                               MAP   30
      INTEGER HDRRTN                                                    MAP   31
      DATA IBLNK/' '/                                                   MAP   32
      DATA IN/7,75,70/,NFILES/3/                                        MAP   33
C TABLE LIMITS,ETC.                                                     MAP   34
      DATA NUMTRG/50/,NUMGT/50/,NUMFPT/300/,NUMLIN/50/,NUMCHG/11/,      MAP   35
     1     BIGCHG/50./                                                  MAP   36
C BLANK CARD COLUMN MAPS                                                MAP   37
      DATA NTCOLS/2,6,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,  MAP   38
     1  41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,   56,57,58,59,60, MAP   39
     2  61,62,63,64,65,66,67,68,69,70,71,72/,                           MAP   40
     3NDCOLS/2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,                MAP   41
     1  41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,   56,57,58,59,60, MAP   42
     2  61,62,63,64,65,66,67,68,69,70,71,72/                            MAP   43
      T100=SECOND(CP  )                                                 MAP   44
C SET TAPE8 READ FLAG--WILL HAVE TO BE CREATED OR REREAD FOR FURTHER    MAP   45
C   PROCESSING, SINCE MAP USES ITS COMMON STORAGE                       MAP   46
      REED=.FALSE.                                                      MAP   47
      REOF=.FALSE.                                                      MAP   48
      WRITE(KPRINT,700)                                                 MAP   49
      NCARDS=0                                                          MAP   50
      NT=0                                                              MAP   51
      NS=0                                                              MAP   52
      NG=0                                                              MAP   53
      LINES=0                                                           MAP   54
      NERR=0                                                            MAP   55
      NLRG=-9999                                                        MAP   56
      NSML=9999                                                         MAP   57
      CHLRG=-999999999.                                                 MAP   58
      CHSML=999999999.                                                  MAP   59
      CHCUMD=0.                                                         MAP   60
      CHCUMN=0. 
	DAYN=0.0
	DARKN=0.0
	XLRG=-99999999.0
	YLRG=-99999999.0
	XSML= 99999999.0
	YSML= 99999999.0
	HABV=0.0
	HBEL=0.0
      XTF=0                                                             MAP   65
      XTG=0                                                             MAP   66
      XGF=0                                                             MAP   67
C CHECK FOR DATA BASE FILES--WILL PROCESS BASIC(OR OUTPUT FROM MERGE)-  MAP   68
C        IN(1)- IF PRESENT;                                             MAP   69
C       IF NOT WILL CHECK FOR (1) IN(2)-OUTPUT FROM SORT,               MAP   70
C       (2) IN(3)-OUTPUT FROM CMBINE                                    MAP   71
      DO 5 I=1,NFILES                                                   MAP   72
      IU=IN(I)                                                          MAP   73
      REWIND IU                                                         MAP   74
      READ(IU,609,END=6)                                                      
	GO TO 6
    5 CONTINUE                                                          MAP   77
C NO INPUT FILE                                                         MAP   78
      WRITE(KPRINT,899)                                                 MAP   79
      STOP                                                              MAP   80
    6 IN1=IU                                                            MAP   81
      REWIND IN1                                                        MAP   82
C     READ MAP-2 CARD WITH FLAGS FOR CROSS-REFERENCE TABLES 
C     IFLAG IS A FLAG FOR PRINTING DATA BASE INFO 
      READ(KARD,600) IFLAG,ITF,ITG,IGT,IGF,IO1                          MAP   83
C     LOI IS A FLAG FOR PRINTING ERROR MESSAGE FOR EXTRANEOUS DATA
      LO1=(IO1.EQ.0)                                                    MAP   84
C     PRINT FLAG PRNT FOR MAP MODULE OUTPUT 
      PRNT=(IFLAG.NE.'0')
C     CHECK FOR CROSS-REFERENCE TABLE PRINTING
      IF(ITF.GT.0)XTF=1                                                 MAP   86
      IF(ITG.GT.0.OR.IGT.GT.0)XTG=1                                     MAP   87
      IF(IGF.GT.0)XGF=1                                                 MAP   88
C CLEAR XREF TABLES                                                     MAP   89
      IF(XTF.LE.0.AND.XGF.LE.0)GO TO 15                                 MAP   90
      DO 14 J=1,NUMFPT                                                  MAP   91
      DO 10 I=1,NUMTRG                                                  MAP   92
   10 TXFPT(I,J)=0.0                                                    MAP   93
      DO 11 I=1,NUMGT                                                   MAP   94
   11 GTXFPT(I,J)=0.0                                                   MAP   95
   14 CONTINUE                                                          MAP   96
   15 IF(XTG.LE.0)GO TO 21                                              MAP   97
      DO 16 J=1,NUMGT                                                   MAP   98
      DO 16 I=1,NUMTRG                                                  MAP   99
   16 TXGT(I,J)=0.0                                                     MAP  100
   21 CONTINUE                                                          MAP  101
C                                                                       MAP  102
C READ GUN TYPE TABLE                                                   MAP  103
C                                                                       MAP  104
C     WRITE HEADINGS FOR GUN TYPE TABLE 
      WRITE(KPRINT,724)                                                 MAP  105
      OVRFL=.FALSE.                                                     MAP  106
      KERR=0                                                            MAP  107
      I=1                                                               MAP  108
   50 READ(IN1,609,END=499)IDGUN(I),(CHARGE(J,I),J=1,NUMCHG),NAME1(I), 
     1NAME2(I),IFLAG,(CONTOR(I,J),J=1,15)
C     COUNT DATA BASE CARDS 
      NCARDS=NCARDS + 3 
C     COUNT GUN TYPES 
      NG=NG+1                                                           MAP  112
C -0 TO 0 FOR PRINTING (CDC GLITCH)                                     MAP  113
      IF(IDGUN(I).EQ.0)IDGUN(I)=0                                       MAP  114
      DO 55 J=1,NUMCHG                                                  MAP  115
   55 IF(CHARGE(J,I).EQ.0.)CHARGE(J,I)=0.                               MAP  116
C     IF IFLAG NOT ZERO PRINT DATA BASE INFO
      IF(PRNT)WRITE(KPRINT,725)IFLAG,IDGUN(I),(CHARGE(J,I),J=1,NUMCHG)
      IMAGE=PRNT                                                        MAP  118
C CHECK CHARGE LIMITS                                                   MAP  119
      NEG=0                                                             MAP  120
      LRG=0                                                             MAP  121
      POS=.FALSE.                                                       MAP  122
      DO 60 J=1,NUMCHG                                                  MAP  123
      IF(CHARGE(J,I))58,60,59                                           MAP  124
C     CHARGE SIZE<0 
   58 NEG=NEG+1                                                         MAP  125
      KERR=KERR+1                                                       MAP  126
      GO TO 60                                                          MAP  127
C     CHARGE SIZE>0 
   59 POS=.TRUE.                                                        MAP  128
      IF(CHARGE(J,I).LE.BIGCHG)GO TO 60                                 MAP  129
      LRG=LRG+1                                                         MAP  130
      KERR=KERR+1                                                       MAP  131
   60 CONTINUE                                                          MAP  132
      IF(POS.AND.(NEG+LRG).EQ.0)GO TO 70                                MAP  133
      IF(IMAGE)GO TO 65                                                 MAP  134
      WRITE(KPRINT,725)IFLAG,IDGUN(I), (CHARGE(J,I),J=1,NUMCHG)         MAP  150
      IMAGE=.TRUE.                                                      MAP  136
C     NO POSITIVE CHARGES FOR THIS GUN TYPE 
 65   IF(.NOT.POS)WRITE(KPRINT,731)                                     MAP  137
C     NEG CHARGES ENCOUNTERED 
      IF(NEG.GT.0)WRITE(KPRINT,732)NEG                                  MAP  138
C     CHARGE SIZE TOO LARGE 
      IF(LRG.GT.0)WRITE(KPRINT,733)LRG,BIGCHG                           MAP  139
   70 CONTINUE                                                          MAP  140
C CHECK FOR DUP. ID;FIRST OCCURRENCE USED FOR TABLE                     MAP  141
      IF(I.EQ.1)GO TO 80                                                MAP  142
      NTD=I-1                                                           MAP  143
      ID=IDGUN(I)                                                       MAP  144
      DO 75 K=1,NTD                                                     MAP  145
      IF(ID      .EQ.IDGUN(K))GO TO 76                                  MAP  146
   75 CONTINUE                                                          MAP  147
      GO TO 80                                                          MAP  148
   76 IF(IMAGE)GO TO 77                                                 MAP  149
      WRITE(KPRINT,725)IFLAG,IDGUN(I),(CHARGE(J,I),J=1,NUMCHG)          MAP  135
      IMAGE=.TRUE.                                                      MAP  151
C     DUPLICATE ID FOUND
   77 WRITE(KPRINT,734)                                                 MAP  152
      KERR=KERR+1                                                       MAP  153
      I=NTD                                                             MAP  154
   80 CONTINUE                                                          MAP  155
C LAST GUN TABLE CARD?                                                  MAP  156
      IF(IFLAG.EQ.'*')GO TO 90                                          MAP  157
      IF(I.GE.NUMGT)GO TO 85                                            MAP  158
      I=I+1                                                             MAP  159
      GO TO 50                                                          MAP  160
C TABLE OVERFLOW                                                        MAP  161
   85 IF(OVRFL)GO TO 50                                                 MAP  162
      WRITE(KPRINT,726)NUMGT                                            MAP  163
      I=NUMGT+1                                                         MAP  164
      OVRFL=.TRUE.                                                      MAP  165
C FLAG OVERFLOW FOR RELEVANT XREF TABLES                                MAP  166
      IF(XTG.EQ.1)XTG=2                                                 MAP  167
      IF(XGF.EQ.1)XGF=2                                                 MAP  168
      GO TO 50                                                          MAP  169
C END OF GUN TABLE INPUT                                                MAP  170
   90 NTYPES=MIN0(I,NUMGT)                                              MAP  171
      IF(.NOT. PRNT) GO TO 95 
C     WRITE HEADING FOR SECOND PART OF GUN TYPE TABLE FOR CONTOUR 
C     INFORMATION.
      WRITE(KPRINT,7241)
 7241 FORMAT(////T2,'GTYPE',8X,'NAME',14X,'EQ PARA',2X,'EQ PARB',29X, 
     1'DIRECTIVITY VALUES',30X,'AVG'//) 
C     WRITE OUT NAME AND CONTOUR INFORMATION
      DO 91 K=1,NTYPES
         WRITE(KPRINT,7251)IDGUN(K),NAME1(K),NAME2(K),
     1(CONTOR(K,J),J=1,15)
 7251 FORMAT(T4,A2,5X,2A10,2X,F6.2,4X,F5.2,3X,12F6.2,2X,F5.2) 
   91 CONTINUE
C PRINT ERROR/WARNING COUNT                                             MAP  172
95    WRITE(KPRINT,730)KERR                                             MAP  173
      NERR=NERR+KERR                                                    MAP  174
      KERR=0                                                            MAP  175
C                                                                       MAP  176
C READ TARGETS                                                          MAP  177
C                                                                       MAP  178
      ASSIGN 231 TO MAXMIN                                              MAP  179
C     WRITE HEADING FOR TARGET CARDS
      WRITE(KPRINT,607)                                                 MAP  180
      WRITE(KPRINT,714)                                                 MAP  181
      OVRFL=.FALSE.                                                     MAP  182
C     READ TARGET CARDS 
      I=1                                                               MAP  183
 200  READ(IN1,601,END=299)IFLAG,NUL(1),ID,NUL(2),XLOC,YLOC,PHCORR,             
     1(NUL(J),J=3,50)                                                   MAP  185
C     COUNT TARGET CARDS WITH DATA BASE CARDS 
      NCARDS=NCARDS+1                                                   MAP  187
C     COUNT TARGET CARDS
      NT=NT+1                                                           MAP  188
C -0(BLANK) TO 0 FOR PRINTING                                           MAP  189
      IF(ID.EQ.0)ID=0                                                   MAP  190
      IF(XLOC.EQ.0.)XLOC=0.                                             MAP  191
      IF(YLOC.EQ.0.)YLOC=0.                                             MAP  192
      IF(PHCORR.EQ.0.)PHCORR=0.                                         MAP  193
C                                                                       MAP  194
      IF(PRNT)WRITE(KPRINT,715)IFLAG,NUL(1),ID,NUL(2),XLOC,YLOC,PHCORR, MAP  195
     1        (NUL(J),J=3,50)                                           MAP  196
      IMAGE=PRNT                                                        MAP  197
C CHECK BLANK FIELDS                                                    MAP  198
      DO 210 J=1,50                                                     MAP  199
      IF(NUL(J).NE.IBLNK) GO TO 211                                     MAP  200
  210 CONTINUE                                                          MAP  201
      GO TO 220                                                         MAP  202
C DATA IN BLANK FIELD                                                   MAP  203
 211  IF(.NOT. LO1) GO TO 213 
      IF(IMAGE)GO TO 212                                                MAP  204
      ASSIGN 212 TO KPRNTT                                              MAP  205
      GO TO 29991                                                       MAP  206
 212  IF(LO1)WRITE(KPRINT,735) NTCOLS(J)                                MAP  207
213   KERR=KERR+1                                                       MAP  208
C CHECK HGT CORRECTION                                                  MAP  209
  220 IF(PHCORR.GE.0..AND. PHCORR.LE.6.)GO TO 230                       MAP  210
      IF(IMAGE)GO TO 222                                                MAP  211
      ASSIGN 222 TO KPRNTT                                              MAP  212
      GO TO 29991                                                       MAP  213
 222  WRITE(KPRINT,736)                                                 MAP  214
      KERR=KERR+1                                                       MAP  215
C MAX,MIN COORDS CHECK                                                  MAP  216
  230 GO TO 99992                                                       MAP  217
  231 CONTINUE                                                          MAP  218
C DUP.ID,COORDS                                                         MAP  219
      IF(I.GT.1)GO TO 239                                               MAP  220
      IDTRG(1)=ID                                                       MAP  221
      TCORDS(1,1)=XLOC                                                  MAP  222
      TCORDS(1,2)=YLOC                                                  MAP  223
      GO TO 250                                                         MAP  224
C IF DUP. ID,USE FIRST OCCURRENCE FOR TABLE                             MAP  225
  239 NTD=I-1                                                           MAP  226
      DO 240 J=1,NTD                                                    MAP  227
      IF(ID.EQ.IDTRG(J))GO TO 241                                       MAP  228
  240 CONTINUE                                                          MAP  229
C UNIQUE ID                                                             MAP  230
      IF(OVRFL)GO TO 245                                                MAP  231
      IDTRG(I)=ID                                                       MAP  232
      TCORDS(I,1)=XLOC                                                  MAP  233
      TCORDS(I,2)=YLOC                                                  MAP  234
      GO TO 245                                                         MAP  235
C DUP                                                                   MAP  236
  241 IF(IMAGE)GO TO 242                                                MAP  237
      ASSIGN 242 TO KPRNTT                                              MAP  238
      GO TO 29991                                                       MAP  239
 242  WRITE(KPRINT,734)                                                 MAP  240
      KERR=KERR+1                                                       MAP  241
      IF(.NOT.OVRFL)I=NTD                                               MAP  242
C DUP. COORDS;WARNING,BUT CONSIDERED SEPARATE FOR TABLE                 MAP  243
  245 CONTINUE                                                          MAP  244
      DO 246 J=1,NTD                                                    MAP  245
      IF(XLOC.NE.TCORDS(J,1))GO TO 246                                  MAP  246
      IF(YLOC.EQ.TCORDS(J,2))GO TO 247                                  MAP  247
  246 CONTINUE                                                          MAP  248
      GO TO 250                                                         MAP  249
  247 IF(IMAGE)GO TO 248                                                MAP  250
      ASSIGN 248 TO KPRNTT                                              MAP  251
      GO TO 29991                                                       MAP  252
C     WARNING ABOUT DUPLICATE ID'S
 248  WRITE(KPRINT,737)                                                 MAP  253
      KERR=KERR+1                                                       MAP  254
C        LAST TRG CARD?                                                 MAP  255
  250 IF(IFLAG.EQ.'*')GO TO 290                                         MAP  256
      IF(I.GE.NUMTRG)GO TO 260                                          MAP  257
      I=I+1                                                             MAP  258
      GO TO 200                                                         MAP  259
C TABLE OVERFLOW                                                        MAP  260
  260 IF(OVRFL)GO TO 200                                                MAP  261
      WRITE(KPRINT,738)NUMTRG                                           MAP  262
      I=NUMTRG+1                                                        MAP  263
      OVRFL=.TRUE.                                                      MAP  264
C FLAG OVERFLOW FOR RELEVANT XREF TABLES                                MAP  265
      IF(XTG.EQ.1)XTG=2                                                 MAP  266
      IF(XTF.EQ.1)XTF=2                                                 MAP  267
      GO TO 200                                                         MAP  268
C*************PRINT TARGET AND FIRING PT CARD IMAGE                     MAP  269
29991 WRITE(KPRINT,715)IFLAG,NUL(1),ID,NUL(2),XLOC,YLOC,PHCORR,         MAP  270
     1        (NUL(J),J=3,50)                                           MAP  271
      IMAGE=.TRUE.                                                      MAP  272
      GO TO KPRNTT,(212,222,242,248,312,322,342,348)                    MAP  273
C*************                                                          MAP  274
C END OF TARGET CARD INPUT                                              MAP  275
  290 NTAR=MIN0(I,NUMTRG)                                               MAP  276
C PRINT ERROR/WARNING COUNT                                             MAP  277
      WRITE(KPRINT,730)KERR                                             MAP  278
      NERR=NERR+KERR                                                    MAP  279
      KERR=0                                                            MAP  280
C                                                                       MAP  281
C READ FIRING PT AND FIRING PT DEF. CARDS                               MAP  282
C                                                                       MAP  283
C READ FIRING PT. CARDS                                                 MAP  284
      ASSIGN 331 TO MAXMIN                                              MAP  285
C     WRITE HEADING FOR FIRING POINT CARDS
      WRITE(KPRINT,716)                                                 MAP  286
      WRITE(KPRINT,7161)                                                MAP  287
      OVRFL=.FALSE.                                                     MAP  288
      I=1                                                               MAP  289
  300 ASSIGN 301 TO HDRRTN                                              MAP  290
      NEG=0                                                             MAP  291
      READ(IN1,601,END=500)IFLAG,NUL(1),ID,NUL(2),XLOC,YLOC,PHCORR,             
     1(NUL(J),J=3,50)                                                   MAP  293
C     COUNT MORE DATA BASE CARDS
      NCARDS=NCARDS+1                                                   MAP  295
C     COUNT FIRING POINT CARDS
      NS=NS+1                                                           MAP  296
C -0 TO 0 FOR PRINTING (CDC GLITCH)                                     MAP  297
      IF(ID.EQ.0)ID=0                                                   MAP  298
      IF(XLOC.EQ.0.)XLOC=0.                                             MAP  299
      IF(YLOC.EQ.0.)YLOC=0.                                             MAP  300
      IF(PHCORR.EQ.0.)PHCORR=0.                                         MAP  301
C                                                                       MAP  302
      IF(PRNT.AND.LINES.GE.NUMLIN)GO TO 99991                           MAP  303
  301 CONTINUE                                                          MAP  304
      IF(PRNT)WRITE(KPRINT,715)IFLAG,NUL(1),ID,NUL(2),XLOC,YLOC,PHCORR, MAP  305
     1        (NUL(J),J=3,50)                                           MAP  306
      IMAGE=PRNT                                                        MAP  307
C CHECK BLANK FIELDS                                                    MAP  308
      DO 310 J=1,50                                                     MAP  309
      IF(NUL(J).NE.IBLNK) GO TO 311                                     MAP  310
  310 CONTINUE                                                          MAP  311
      GO TO 320                                                         MAP  312
C DATA IN BLANK FIELD                                                   MAP  313
311   IF(.NOT. LO1) GO TO 313 
      IF(IMAGE)GO TO 312                                                MAP  314
      ASSIGN 312 TO KPRNTT                                              MAP  315
      GO TO 29991                                                       MAP  316
 312  IF(LO1) WRITE(KPRINT,735) NTCOLS(J)                               MAP  317
      LINES=LINES+1 
313   KERR=KERR+1                                                       MAP  318
C CHECK HGT CORRECTION                                                  MAP  320
  320 IF(PHCORR.GE.0..AND. PHCORR.LE.6.)GO TO 330                       MAP  321
      IF(IMAGE)GO TO 322                                                MAP  322
      ASSIGN 322 TO KPRNTT                                              MAP  323
      GO TO 29991                                                       MAP  324
C     ERROR IN HEIGHT CORRECTION
 322  WRITE(KPRINT,736)                                                 MAP  325
      KERR=KERR+1                                                       MAP  326
      LINES=LINES+1                                                     MAP  327
C MAX,MIN COORDS CHECK                                                  MAP  328
C     CHECK BOUNDS
  330 GO TO 99992                                                       MAP  329
  331 CONTINUE                                                          MAP  330
C DUP.ID,COORDS                                                         MAP  331
      DUP=.FALSE.                                                       MAP  332
      IF(I.GT.1)GO TO 339                                               MAP  333
      IFP=1                                                             MAP  334
      IDFPT(1)=ID                                                       MAP  335
      FCORDS(1,1)=XLOC                                                  MAP  336
      FCORDS(1,2)=YLOC                                                  MAP  337
      GO TO 350                                                         MAP  338
C IF DUP. ID,USE FIRST OCCURRENCE FOR TABLE                             MAP  339
  339 NTD=I-1                                                           MAP  340
      DO 340 J=1,NTD                                                    MAP  341
      IF(ID.EQ.IDFPT(J))GO TO 341                                       MAP  342
  340 CONTINUE                                                          MAP  343
C UNIQUE ID                                                             MAP  344
      IF(OVRFL)GO TO 345                                                MAP  345
  343 IDFPT(I)=ID                                                       MAP  346
      IFP=I                                                             MAP  347
      FCORDS(I,1)=XLOC                                                  MAP  348
      FCORDS(I,2)=YLOC                                                  MAP  349
      GO TO 345                                                         MAP  350
C DUP                                                                   MAP  351
  341 IF(IMAGE)GO TO 342                                                MAP  352
      ASSIGN 342 TO KPRNTT                                              MAP  353
      GO TO 29991                                                       MAP  354
  342 IF(XLOC.NE.FCORDS(J,1).OR.YLOC.NE.FCORDS(J,2))GO TO 344           MAP  355
C     DUP. ID AND DUP. COORDINATES
      WRITE(KPRINT,753)                                                 MAP  356
      KERR=KERR+1                                                       MAP  357
      LINES=LINES+1                                                     MAP  358
C NULL FIRING PT. INDEX                                                 MAP  359
      IFP=0                                                             MAP  360
      DUP=.TRUE.                                                        MAP  361
      IF(.NOT.OVRFL)I=NTD                                               MAP  362
      GO TO 345                                                         MAP  363
C        DIFF COORDS                                                    MAP  364
C     DUP ID, DIFF. COORDINATES 
 344  WRITE(KPRINT,754)                                                 MAP  365
      KERR=KERR+1                                                       MAP  366
      LINES=LINES+1                                                     MAP  367
      IF(OVRFL)GO TO 345                                                MAP  368
C        GENERATED ID                                                   MAP  369
      ID=SHIFT(-I,42)                                                   MAP  370
      WRITE(KPRINT,755) ID                                              MAP  371
      LINES=LINES+1                                                     MAP  372
      GO TO 343                                                         MAP  373
C DUP. COORDS;WARNING,BUT CONSIDERED SEPARATE FOR TABLE                 MAP  374
  345 CONTINUE                                                          MAP  375
      DO 346 J=1,NTD                                                    MAP  376
      IF(XLOC.NE.FCORDS(J,1))GO TO 346                                  MAP  377
      IF(YLOC.EQ.FCORDS(J,2))GO TO 347                                  MAP  378
  346 CONTINUE                                                          MAP  379
      GO TO 350                                                         MAP  380
  347 IF(IMAGE)GO TO 348                                                MAP  381
      ASSIGN 348 TO KPRNTT                                              MAP  382
      GO TO 29991                                                       MAP  383
 348  WRITE(KPRINT,737)                                                 MAP  384
      KERR=KERR+1                                                       MAP  385
      LINES=LINES+1                                                     MAP  386
  350 CONTINUE                                                          MAP  387
C                                                                       MAP  388
C READ FIRING PT. DEF. CARDS                                            MAP  389
C                                                                       MAP  390
      ASSIGN 401 TO HDRRTN                                              MAP  391
 400  READ(IN1,603,END=399)IFLAG,(NUL(J),J=1,17),ITYPE,DAY,DARK,MIN,MAX,    
     1IDT,KFLAG,HGT,(NUL(J),J=18,56)                                        MAP  393
C     COUNT MORE DATA BASE CARDS
      NCARDS=NCARDS+1                                                   MAP  395
C -0(BLANK) TO 0 FOR PRINTING                                           MAP  396
      IF(ITYPE.EQ.0)ITYPE=0                                             MAP  397
      IF(DAY.EQ.0.)DAY=0.                                               MAP  398
      IF(DARK.EQ.0.)DARK=0.                                             MAP  399
      IF(MIN.EQ.0)MIN=0                                                 MAP  400
      IF(MAX.EQ.0)MAX=0                                                 MAP  401
      IF(KFLAG.EQ.0)KFLAG=0                                             MAP  402
      IF(IDT.EQ.0)IDT=0                                                 MAP  403
      IF(HGT.EQ.0.)HGT=0.                                               MAP  404
C                                                                       MAP  405
      IF(PRNT.AND.LINES.GE.NUMLIN)GO TO 99991                           MAP  406
  401 CONTINUE                                                          MAP  407
      IF(PRNT)WRITE(KPRINT,718)IFLAG,(NUL(J),J=1,17),ITYPE,DAY,DARK,MIN,MAP  408
     1MAX,IDT,KFLAG,HGT,(NUL(J),J=18,56)                                MAP  409
      LINES=LINES+1                                                     MAP  410
      IMAGE=PRNT                                                        MAP  411
C CHECK BLANK FIELDS                                                    MAP  412
      DO 405 J=1,49                                                     MAP  413
      IF(NUL(J).NE.IBLNK)GO TO 406                                      MAP  414
  405 CONTINUE                                                          MAP  415
      GO TO 409                                                         MAP  416
C DATA IN BLANK FIELD                                                   MAP  417
406   IF(.NOT. LO1) GO TO 408 
      IF(IMAGE)GO TO 407                                                MAP  418
      ASSIGN 407 TO KPRNTD                                              MAP  419
      GO TO 49991                                                       MAP  420
C     IF EXTRANEOUS DATA FLAG NOT ZERO,PRINT ERROR MESSAGE
 407  IF(LO1)WRITE(KPRINT,735) NDCOLS(J)                                MAP  421
      LINES=LINES+1 
408   KERR=KERR+1                                                       MAP  422
C GUN TYPE                                                              MAP  424
  409 DO 410 IG=1,NTYPES                                                MAP  425
      IF(ITYPE.EQ.IDGUN(IG))GO TO 415                                   MAP  426
  410 CONTINUE                                                          MAP  427
C UNDEFINED GUN TYPE                                                    MAP  428
      IF(IMAGE)GO TO 411                                                MAP  429
      ASSIGN 411 TO KPRNTD                                              MAP  430
      GO TO 49991                                                       MAP  431
 411  WRITE(KPRINT,727)ITYPE,ID                                         MAP  432
      KERR=KERR+1                                                       MAP  433
      LINES=LINES+1                                                     MAP  434
      IG=0                                                              MAP  435
  415 CONTINUE                                                          MAP  436
C FIRINGS                                                               MAP  437
      IF(DAY.LT.0..OR.DARK.LT.0.)GO TO 420                              MAP  438
      SHOTS=DAY+DARK                                                    MAP  439
      IF(DUP)GO TO 419                                                  MAP  440
      DAYN=DAYN+DAY                                                     MAP  441
      DARKN=DARKN+DARK                                                  MAP  442
  419 IF(SHOTS.GT.0.)GO TO 430                                          MAP  443
  420 SHOTS=0.                                                          MAP  444
      IF(IMAGE)GO TO 421                                                MAP  445
      ASSIGN 421 TO KPRNTD                                              MAP  446
      GO TO 49991                                                       MAP  447
C     NUMBER OF FIRINGS<=0
 421  WRITE(KPRINT,739)ID                                               MAP  448
      KERR=KERR+1                                                       MAP  449
      LINES=LINES+1                                                     MAP  450
C CHARGE                                                                MAP  451
  430 IF(MIN.LT.NSML)NSML=MIN                                           MAP  452
      IF(MIN.LE.0.OR.MIN.GE.NUMCHG)GO TO 434                            MAP  453
      IF(IG.EQ.0)GO TO 440                                              MAP  454
C FIRST ELEMENT IN CHARGE=TARGET CHARGE,SO OFFSET BY 1                  MAP  455
      CHG=CHARGE(MIN+1,IG)                                              MAP  456
      IF(CHG.LE.0.)GO TO 434                                            MAP  457
      IF(CHG.LT.CHSML)CHSML=CHG                                         MAP  458
      GO TO 440                                                         MAP  459
C INVALID CHARGE                                                        MAP  460
  434 IF(IMAGE)GO TO 435                                                MAP  461
      ASSIGN 435 TO KPRNTD                                              MAP  462
      GO TO 49991                                                       MAP  463
  435 KERR=KERR+1                                                       MAP  464
      IF(MIN.GT.0)GO TO 436                                             MAP  465
      NEG=NEG+1                                                         MAP  466
      GO TO 440                                                         MAP  467
C     CHARGE SIZE NO GOOD 
 436  WRITE(KPRINT,740)ID                                               MAP  468
      LINES=LINES+1                                                     MAP  469
  440 CHG=0.                                                            MAP  470
      IF(MAX.GT.NLRG)NLRG=MAX                                           MAP  471
      IF(MAX.LE.0.OR.MAX.GE.NUMCHG)GO TO 444                            MAP  472
      IF(IG.EQ.0)GO TO 446                                              MAP  473
      CHG=CHARGE(MAX+1,IG)                                              MAP  474
      IF(CHG.LE.0.)GO TO 444                                            MAP  475
      IF(CHG.GT.CHLRG)CHLRG=CHG                                         MAP  476
      IF(.NOT.DUP) CHCUMD=CHCUMD+CHG*DAY                                MAP  477
      IF(.NOT. DUP) CHCUMN=CHCUMN+CHG*DARK
      GO TO 446                                                         MAP  478
C INVALID                                                               MAP  479
  444 IF(IMAGE)GO TO 445                                                MAP  480
      ASSIGN 445 TO KPRNTD                                              MAP  481
      GO TO 49991                                                       MAP  482
445   KERR=KERR+1                                                       MAP  483
      CHG=0.                                                            MAP  484
      IF(MAX.GT.0)GO TO 448                                             MAP  485
      NEG=NEG+1                                                         MAP  486
      GO TO 446                                                         MAP  487
  448 WRITE(KPRINT,740)ID                                               MAP  488
      LINES=LINES+1                                                     MAP  489
  446 IF(MIN.LE.MAX)GO TO 450                                           MAP  490
      IF(IMAGE)GO TO 447                                                MAP  491
      ASSIGN 447 TO KPRNTD                                              MAP  492
      GO TO 49991                                                       MAP  493
  447 WRITE(KPRINT,741)ID                                               MAP  494
      KERR=KERR+1                                                       MAP  495
      LINES=LINES+1                                                     MAP  496
C HIT FLAG,TARGET ID                                                    MAP  497
C HIT FLAG MUST BE SET IF IDT BLANK                                     MAP  498
  450  IF(IDT.NE.IBLNK)GO TO 460                                        MAP  499
      IT=0                                                              MAP  500
      IF(KFLAG.EQ.1)GO TO 470                                           MAP  501
      IF(IMAGE)GO TO 451                                                MAP  502
      ASSIGN 451 TO KPRNTD                                              MAP  503
      GO TO 49991                                                       MAP  504
C     BLANK TARGET ID 
  451 WRITE(KPRINT,728)ID                                               MAP  505
      KERR=KERR+1                                                       MAP  506
      LINES=LINES+1                                                     MAP  507
      GO TO 470                                                         MAP  508
C     FIND TARGET GIVEN IN TARGET ID TABLE
  460 DO 465 IT=1,NTAR                                                  MAP  509
      IF(IDT.EQ.IDTRG(IT))GO TO 468                                     MAP  510
  465 CONTINUE                                                          MAP  511
C UNDEFINED TARGET ID                                                   MAP  512
      IF (IMAGE)GO TO 467                                               MAP  513
      ASSIGN 467 TO KPRNTD                                              MAP  514
      GO TO 49991                                                       MAP  515
  467 WRITE(KPRINT,722)IDT,ID                                           MAP  516
      KERR=KERR+1                                                       MAP  517
      LINES=LINES+1                                                     MAP  518
      IT=0                                                              MAP  519
      GO TO 470                                                         MAP  520
C VALID TARGET CHARGE?                                                  MAP  521
  468 IF(IG.LE.0)GO TO 470                                              MAP  522
      TCHG=0. 
      IF(KFLAG .EQ. 1) GO TO 470
      TCHG=CHARGE(1,IG)                                                 MAP  523
      IF(TCHG.GT.0)GO TO 470                                            MAP  524
      TCHG=0.                                                           MAP  525
  
  
  
      IF(IMAGE)GO TO 469                                                MAP  526
      ASSIGN 469 TO KPRNTD                                              MAP  527
      GO TO 49991                                                       MAP  528
C     NON POSITIVE TARGET CHARGE
  469 WRITE(KPRINT,757)ITYPE,ID                                         MAP  529
      KERR=KERR+1                                                       MAP  530
      LINES=LINES+1                                                     MAP  531
C HEIGHT                                                                MAP  532
470   IF(.NOT. DUP) CHCUMD=CHCUMD+TCHG*DAY
      IF(.NOT. DUP) CHCUMN=CHCUMN+TCHG*DARK 
      IF(ABS(HGT).GT.999.)GO TO 475 
      IF(HGT.LT.HBEL)HBEL=HGT                                           MAP  534
      IF(HGT.GT.HABV)HABV=HGT                                           MAP  535
      GO TO 480                                                         MAP  536
  475 IF(IMAGE)GO TO 477                                                MAP  537
      ASSIGN 477 TO KPRNTD                                              MAP  538
      GO TO 49991                                                       MAP  539
C     LARGE HEIGHT VALUE IN DEF. CARD FOR FIRING PT.
  477 WRITE(KPRINT,742)ID                                               MAP  540
      KERR=KERR+1                                                       MAP  541
      LINES=LINES+1                                                     MAP  542
C XREF TABLE ENTRIES                                                    MAP  543
  480 IF(SHOTS.LE.0..OR.DUP)GO TO 490                                   MAP  544
      IF(IT.LE.0)GO TO 487                                              MAP  545
      IF(XTF.LE.0.OR.IFP.LE.0)GO TO 485                                 MAP  546
C TARGET X FIRING PT -- FIRINGS                                         MAP  547
      TXFPT(IT,IFP)=TXFPT(IT,IFP)+SHOTS                                 MAP  548
C TARGETS X GUN TYPES -- FIRINGS*TARGET CHARGE                          MAP  549
  485 IF(IG.LE.0)GO TO 490                                              MAP  550
      IF(XTG.LE.0)GO TO 487                                             MAP  551
      TXGT(IT,IG)=TXGT(IT,IG)+ TCHG       *SHOTS                        MAP  552
C GUN TYPES X FIRING PT -- FIRINGS*CHARGE                               MAP  553
  487 IF(XGF.LE.0.OR.IFP.LE.0)GO TO 490                                 MAP  554
      GTXFPT(IG,IFP)=GTXFPT(IG,IFP)+CHG*SHOTS                           MAP  555
C        LAST DEF CARD?                                                 MAP  556
  490 IF(IFLAG.NE.'*')GO TO 400                                         MAP  557
C     IF NEGATIVE CHARGES---WRITE ERROR MESSAGE 
      IF(NEG.GT.0)WRITE(KPRINT,756)NEG,ID                                    
      LINES=LINES+2                                                     MAP  559
C     CHECK FOR OVERFLOW
      IF(I.GE.NUMFPT)GO TO 491                                          MAP  560
      I=I+1                                                             MAP  561
      GO TO 300                                                         MAP  562
C TABLE OVERFLOW                                                        MAP  563
  491 IF(OVRFL)GO TO 300                                                MAP  564
      WRITE(KPRINT,743)NUMFPT                                           MAP  565
      I=NUMFPT+1                                                        MAP  566
      OVRFL=.TRUE.                                                      MAP  567
C FLAG OVERFLOW FOR RELEVANT XREF TABLES                                MAP  568
      IF(XTF.EQ.1)XTF=2                                                 MAP  569
      IF(XGF.EQ.1)XGF=2                                                 MAP  570
C NULL TABLE INDEX                                                      MAP  571
      IFP=0                                                             MAP  572
      GO TO 300                                                         MAP  573
C*************PRINT DEF CARD IMAGE                                      MAP  574
49991 WRITE(KPRINT,718)IFLAG,(NUL(J),J=1,17),ITYPE,DAY,DARK,MIN,MAX,    MAP  575
     1         IDT,KFLAG,HGT,(NUL(J),J=18,56)                           MAP  576
      IMAGE=.TRUE.                                                      MAP  577
      GO TO KPRNTD,(407,411,421,435,445,447,451,469)                    MAP  578
C*************                                                          MAP  579
C****************** INTERNAL SUBROUTINES ********************           MAP  580
C HEADER ROUTINE                                                        MAP  581
99991 CONTINUE                                                          MAP  582
      WRITE(KPRINT,607)                                                 MAP  583
      WRITE(KPRINT,7161)                                                MAP  584
      LINES=0                                                           MAP  585
      GO TO HDRRTN,(301,401)                                            MAP  586
C MAX,MIN COORDS CHECK                                                  MAP  587
99992 CONTINUE                                                          MAP  588
      IF(XLOC.LE.XLRG)GO TO 100                                         MAP  589
      XLRG=XLOC                                                         MAP  590
      YLRG1=YLOC                                                        MAP  591
  100 IF(YLOC.LE.YLRG)GO TO 105                                         MAP  592
      XLRG1=XLOC                                                        MAP  593
      YLRG=YLOC                                                         MAP  594
  105 IF(XLOC.GE.XSML)GO TO 115                                         MAP  595
      XSML=XLOC                                                         MAP  596
      YSML1=YLOC                                                        MAP  597
  115 IF(YLOC.GE.YSML)GO TO 120                                         MAP  598
      XSML1=XLOC                                                        MAP  599
      YSML=YLOC                                                         MAP  600
  120 GO TO MAXMIN,(231,331)                                            MAP  601
C***********************************************************            MAP  602
C END OF FIRING PT. CARDS                                               MAP  603
  500 IF(.NOT.OVRFL)I=I-1                                               MAP  604
      NSRCS=MIN0(I,NUMFPT)                                              MAP  605
C PRINT ERROR/WARNING COUNT                                             MAP  606
      WRITE(KPRINT,730)KERR                                             MAP  607
      NERR=NERR+KERR                                                    MAP  608
C                                                                       MAP  609
C END OF INPUT PHASE                                                    MAP  610
C                                                                       MAP  611
C     TOTAL NUMBER OF DATA BASE ERRORS
      WRITE(KPRINT,744)NERR                                             MAP  612
      WRITE(KPRINT,607)                                                 MAP  613
C     TOTAL NUMBER OF DATA BASE CARDS 
      WRITE(KPRINT,701)NCARDS                                           MAP  614
C     NUMBER OF GUN TYPES, TARGETS, AND FIRING POINTS 
      WRITE(KPRINT,699)NG                                               MAP  615
      WRITE(KPRINT,702)NT                                               MAP  616
      WRITE(KPRINT,703)NS                                               MAP  617
      IF(NS.EQ.0)GO TO 299                                              MAP  618
      READ(KARD,605,END=502) DAYS                                               
	GO TO 501
502 	DAYS=1.0
      REOF=.TRUE.                                                       MAP  622
  501 CONTINUE                                                          MAP  623
C     DATA BASE TIME PERIOD 
      WRITE(KPRINT,723)DAYS                                             MAP  624
C     TOTAL OF DAY FIRINGS AND TOTAL NIGHT FIRINGS
      WRITE(KPRINT,704)DAYN,DARKN                                       MAP  625
      DAYN=DAYN/DAYS                                                    MAP  626
      DARKN=DARKN/DAYS                                                  MAP  627
      WRITE(KPRINT,721)DAYN,DARKN                                       MAP  628
      CHCUMD=CHCUMD/DAYS                                                MAP  629
      CHCUMN=CHCUMN/DAYS
C     CHARGE INFO 
      WRITE(KPRINT,713)NSML,NLRG,CHSML,CHLRG,CHCUMD,CHCUMN              MAP  630
      IF(ABS(HABV).NE.0..OR.ABS(HBEL).NE.0.0)WRITE(KPRINT,705)HABV,HBEL MAP  631
      WRITE(KPRINT,708)XLRG,XLRG,YLRG1                                  MAP  632
      WRITE(KPRINT,709)YLRG,XLRG1,YLRG                                  MAP  633
      WRITE(KPRINT,710)XSML,XSML,YSML1                                  MAP  634
      WRITE(KPRINT,711)YSML,XSML1,YSML                                  MAP  635
      DX=ABS(XLRG-XSML)                                                 MAP  636
      DY=ABS(YLRG-YSML)                                                 MAP  637
C     READ NUMBER OF GRID SIZES TO BE TESTED
      READ(KARD,604,END=510) N                                                  
      DO 505 I=1,N                                                      MAP  640
C     READ GRID SIZES 
      READ(KARD,605) GRDSZ                                              MAP  641
C     CALCULATE GRID DIMENSION
      DX1=DX/GRDSZ                                                      MAP  642
      DY1=DY/GRDSZ                                                      MAP  643
      WRITE(KPRINT,712)GRDSZ,DX1,DY1                                    MAP  644
  505 CONTINUE                                                          MAP  645
  510 CONTINUE                                                          MAP  646
C                                                                       MAP  647
C XREF TABLES                                                           MAP  648
C                                                                       MAP  649
C TARGETS X FIRING PTS                                                  MAP  650
C        ARRAY PROCESSING ORDER                                         MAP  651
C        BY COLUMN                                                      MAP  652
      IJ=1                                                              MAP  653
C        BY ROW                                                         MAP  654
      JI=2                                                              MAP  655
      IF(ITF.LE.0)GO TO 560                                             MAP  656
      WRITE(KPRINT,607)                                                 MAP  657
      IF(XTF.EQ.2)WRITE(KPRINT,746)                                     MAP  658
      WRITE(KPRINT,745)                                                 MAP  659
      CALL PUTXR(TXFPT,NUMTRG,NUMFPT,NTAR,NSRCS,ADTRG,ADFPT,DAYS,XREF,  MAP  660
     1             IJ)                                                  MAP  661
C TARGETS BY GUN TYPES AND VICE-VERSA                                   MAP  662
  560 IF(ITG.LE.0)GO TO 565                                             MAP  663
      WRITE(KPRINT,607)                                                 MAP  664
      IF(XTG.EQ.2)WRITE(KPRINT,746)                                     MAP  665
      WRITE(KPRINT,748)                                                 MAP  666
      CALL PUTXR(TXGT,NUMTRG,NUMGT,NTAR,NTYPES,ADTRG,ADGUN,DAYS,XREF,IJ)MAP  667
  565 IF(IGT.LE.0)GO TO 570                                             MAP  668
      WRITE(KPRINT,607)                                                 MAP  669
      IF(XTG.EQ.2)WRITE(KPRINT,746)                                     MAP  670
      WRITE(KPRINT,750)                                                 MAP  671
      CALL PUTXR(TXGT,NUMTRG,NUMGT,NTYPES,NTAR,ADGUN,ADTRG,DAYS,XREF,JI)MAP  672
C GUN TYPES X FIRING PT                                                 MAP  673
  570 IF(IGF.LE.0)GO TO 590                                             MAP  674
      WRITE(KPRINT,607)                                                 MAP  675
      IF(XGF.EQ.2)WRITE(KPRINT,746)                                     MAP  676
      WRITE(KPRINT,752)                                                 MAP  677
      CALL PUTXR(GTXFPT,NUMGT,NUMFPT,NTYPES,NSRCS,ADGUN,ADFPT,DAYS,XREF,MAP  678
     1             IJ)                                                  MAP  679
  590 CONTINUE                                                          MAP  680
C     COMPUTE TIME IN MAP 
      T99= SECOND(CP )                                                  MAP  681
      T99=T99-T100                                                      MAP  682
      WRITE(KPRINT,900)T99                                              MAP  683
      RETURN                                                            MAP  684
C EOF EXITS                                                             MAP  685
 299  CONTINUE                                                          MAP  686
C TARGET CARDS                                                          MAP  687
      WRITE(KPRINT,719)                                                 MAP  688
      STOP                                                              MAP  689
 399  CONTINUE                                                          MAP  690
C FIRING PT. CARDS                                                      MAP  691
      WRITE(KPRINT,720)                                                 MAP  692
      STOP                                                              MAP  693
C      GUN TABLE
  499 WRITE(KPRINT,729) 
      STOP
600   FORMAT(A1,5I1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C modified 6/17/88 to allow for reading of full UTM coordinates
C
C601  FORMAT(A1,A1,A3,A1,3F6.0,48A1)                                    MAP  698
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 601  FORMAT(A1,A1,A3,A1,F9.1,1x,F9.1,F6.0,48A1)                        MAP  698
 603  FORMAT(18A1,  A2,F4.0,F4.0,I2,I2,A3,I1,F5.0,39A1)                 MAP  699
 604  FORMAT (I2)                                                       MAP  700
 605  FORMAT(F10.0)                                                     MAP  701
  607 FORMAT(1H1)                                                       MAP  702
  609 FORMAT(1X,A2,11F7.0/3X,2A10/A1,2X,F6.2,14F5.2)
  699 FORMAT(/T15,'NUMBER OF GUN TYPES READ IS ',I5)                    MAP  704
 700  FORMAT(1H1,///,T15,'.....     MAP OF SOURCE POINTS     .....')    MAP  705
 701  FORMAT(//,T15,'NUMBER OF DATA BASE CARD IMAGES READ IS ',I5)      MAP  706
 702  FORMAT(/,T15,'NUMBER OF TARGETS READ IS ',I5)                     MAP  707
 703  FORMAT(/,T15,'NUMBER OF SOURCES READ IS ',I5)                     MAP  708
 704  FORMAT(/,T15,'TOTAL DAY FIRINGS IS   ',F10.2,                     MAP  709
     1/,T15,'TOTAL NIGHT FIRINGS IS ',F10.2)                            MAP  710
 705  FORMAT(/,T15,'MAXIMUM HEIGHT IS ',F10.2,/,T15,'MAXIMUM DEPTH IS ',MAP  711
     1F10.2,/)                                                          MAP  712
 708  FORMAT(/,T15,'MAXIMUM X IS ',F10.1,' IN PAIR  (',F10.1,' , ',F10.1MAP  713
     1,' )')                                                            MAP  714
 709  FORMAT(/,T15,'MAXIMUM Y IS ',F10.1,' IN PAIR  (',F10.1,' , ',F10.1MAP  715
     1,' )')                                                            MAP  716
 710  FORMAT(/,T15,'MINIMUM X IS ',F10.1,' IN PAIR  (',F10.1,' , ',F10.1MAP  717
     1,' )')                                                            MAP  718
 711  FORMAT(/,T15,'MINIMUM Y IS ',F10.1,' IN PAIR  (',F10.1,' , ',F10.1MAP  719
     1,' )')                                                            MAP  720
  712 FORMAT(//T15,' FOR GRID SIZE',F8.1,', GRID DIMENSIONS =',F7.1,    MAP  721
     1' X ',F7.1)                                                       MAP  722
  713 FORMAT(/T15,'MINIMUM CHARGE NUMBER',I5/T15,'MAXIMUM CHARGE NUMBER'MAP  723
     1,I5//                                                             MAP  724
     2T15,'MINIMUM CHARGE WEIGHT',F9.1,' LBS'/T15,'MAXIMUM CHARGE WEIGHTMAP  725
     3',F9.1,' LBS'//                                                   MAP  726
     4 T15,'TOTAL DAY CHARGE WEIGHT PER DAY',2X,F10.1,' LBS'/           MAP  727
     5 T15,'TOTAL NIGHT CHARGE WEIGHT PER DAY',F10.1,' LBS')
 714  FORMAT(//,T15,'.....     TARGET CARDS     .....',//T10,'FLAG',2X, MAP  728
     1'  ID ',2X,'   X   ',2X,'   Y   ',4X,'HT CORR'//)                 MAP  729
CCCCC--modified to print complete UTM numbers
C715  FORMAT(T12,A1,2X,A1,A3,A1,1X,3(2X,F7.0),48A1)                     MAP  730
 715  FORMAT(T12,A1,2X,A1,A3,A1,1X,2X,F9.1,2(2X,F9.1),48A1)             MAP  730
 716  FORMAT(1H1/                                                       MAP  731
     1          T15,'.....     FIRING PT. SOURCE AND DEFINITION CARDS   MAP  732
     1  .....'/)                                                        MAP  733
 7161 FORMAT(                                                           MAP  734
     1   T3,'FLAG',2X,'  ID ',2X,'   X   ',2X,'   Y   ',2X,'HT CORR',   MAP  735
     1 3X,'G TYPE',                                                     MAP  736
     12X,'DAYNO',2X,'NIGHTNO',2X,'MIN',2X,'MAX',2X,'T ID',2X,'FLAG',    MAP  737
     12X,'HGT',                                                         MAP  738
     2//)
  718 FORMAT                                                            MAP  740
     1     (T5,A1,17A1,                                                 MAP  741
     1              22X,A2,4X,F5.0,3X,F5.0,4X,I2,3X,I2,3X,A3,3X,I2,F7.1,MAP  742
     2               39A1)                                              MAP  743
 719  FORMAT(///,T3 ,']]]]]     ERROR -- EOF ENCOUNTERED WHILE READING TMAP  744
     1ARGET CARDS')                                                     MAP  745
 720  FORMAT(///,T3 ,']]]]]     ERROR -- SOURCE DEFINITION CARD ENDS IMPMAP  746
     1ROPERLY (WITH A EOF)     .....',//)                               MAP  747
 721  FORMAT(/,T15,'TOTAL PER DAY DAY FIRINGS IS   ',F10.2,             MAP  748
     1/,T15,'TOTAL PER DAY NIGHT FIRINGS IS ',F10.2)                    MAP  749
  722 FORMAT(T3,'.....     ERROR -- UNDEFINED TARGET ID',A5,            MAP  750
     1' FOR FIRING PT.',A3)                                             MAP  751
  723 FORMAT(//T15,'.....     DATA BASE TIME PERIOD:',F5.0,' DAY(S)')   MAP  752
  724 FORMAT(//T15,'.....     GUN TYPE CARDS     .....'//T10,'FLAG',2X, MAP  753
     1'G TYPE',2X,'T CHARGE',40X,'PROPELLANT WEIGHTS'//)                MAP  754
  725 FORMAT(T12,A1,4X,A3,3X,F9.2,2X,10F9.2)
  726 FORMAT(  T3,']]]]]     ERROR -- GUN TYPES EXCEED TABLE LIMIT IN MAMAP  756
     1P;ONLY FIRST',I3,' TYPES USED FOR SUBSEQUENT CROSS-CHECKING')     MAP  757
  727 FORMAT(  T3,']]]]]     ERROR -- UNDEFINED GUN    ID',A5,          MAP  758
     1' FOR FIRING PT.',A3)                                             MAP  759
  728 FORMAT(  T3,']]]]]     ERROR   -- BLANK TARGET ID, HIT FLAG NOT SEMAP  760
     1T: FIRING PT.',A3)                                                MAP  761
  729 FORMAT(//T3,']]]]]     ERROR -- EOF ENCOUNTERED WHILE READING GUN MAP  762
     1 TYPE DEFINITION CARDS'//)                                        MAP  763
  730 FORMAT(/T3,'.....',I4,' ERROR/WARNING CONDITIONS DETECTED FOR THISMAP  764
     1 CARD TYPE')                                                      MAP  765
  731 FORMAT(T3,'.....     ERROR -- NO POSITIVE CHARGE FOR GUN TYPE')   MAP  766
  732 FORMAT(T3,'.....     ERROR -- ',I3,' NEG. CHARGES ENCOUNTERED')   MAP  767
  733 FORMAT(T3,'.....     WARNING -- ',I3,' CHARGES LARGER THAN',F4.0, MAP  768
     1' LBS')                                                           MAP  769
  734 FORMAT(T3,'.....     ERROR -- DUPLICATE ID;FIRST OCCURRENCE USED FMAP  770
     1OR TABLE')                                                        MAP  771
  735 FORMAT(T3,'.....     WARNING -- EXTRANEOUS DATA STARTING IN CARD CMAP  772
     1OL.',                                                             MAP  773
     1I3,'; CHECK ALL FIELDS')                                          MAP  774
  736 FORMAT(T3,'.....     ERROR -- HEIGHT CORRECTION DATA OUT OF RANGE'MAP  775
     1)                                                                 MAP  776
  737 FORMAT(T3,'.....     WARNING -- DUP. POINT: IDENTICAL COORDINATES'MAP  777
     1)                                                                 MAP  778
  738 FORMAT(/T3,'.....     ERROR -- TARGETS EXCEED TABLE LIMIT IN MAP; MAP  779
     1ONLY FIRST',I3,' USED FOR SUBSEQUENT CROSS-CHECKING'/)            MAP  780
  739 FORMAT(T3,'.....     ERROR -- FIRINGS DATA NEGATIVE OR BOTH ZERO OMAP  781
     1N DEF. CARD FOR FIRING PT.',A3)                                   MAP  782
  740 FORMAT(T3,'.....     ERROR -- INVALID CHARGE NO.:NONPOSITIVE OR NOMAP  783
     1 GUN TABLE ENTRY; DEF CARD FOR FIRING PT. ',A3)                   MAP  784
  741 FORMAT(T3,'.....     ERROR -- MIN GREATER THAN MAX CHARGE NO.; DEFMAP  785
     1 CARD FOR FIRING PT.',A3)                                         MAP  786
  742 FORMAT(T3,'.....    WARNING -- LARGE HEIGHT VALUE IN DEF. CARD FORMAP  787
     1 FIRING PT. ',A3)                                                 MAP  788
  743 FORMAT(/T3,'.....    ERROR -- FIRING PTS. EXCEED TABLE LIMIT IN MAMAP  789
     1P; ONLY FIRST',I3,' USED FOR SUBSEQUENT CROSS CHECKING'/)         MAP  790
  744 FORMAT(//T3,'..... END OF INPUT PHASE:',I5,' ERROR/WARNING CONDITIMAP  791
     1ONS DETECTED')                                                    MAP  792
  745 FORMAT(/1X,'CROSS-REFERENCE: TARGETS BY FIRING POINTS ; DAILY FIRIMAP  793
     1NGS'////                                                          MAP  794
     1      1X,'TARGET ID'/10X,7('FPT ID          '))                   MAP  795
  746 FORMAT(/1X,'.....     WARNING -- DUE TO PREVIOUS TABLE OVERFLOW, TMAP  796
     1HE FOLLOWING CROSS-REFERENCE TABLE IS INCOMPLETE'/)               MAP  797
 748  FORMAT(/1X,'CROSS-REFERENCE: TARGETS BY GUN TYPES;  DAILY ' 
     1,'PROJECTILE CHARGE WEIGHT (LBS)'//// 
     1              1X,'TARGET ID'/10X,7('GUN ID          '))           MAP  800
750   FORMAT(/1X,'CROSS-REFERENCE: GUN TYPES BY TARGETS;  DAILY ' 
     1,'PROJECTILE CHARGE WEIGHT (LBS)'//// 
     1              1X,'GUN ID'/10X,7('TAR ID          ' ))             MAP  803
  752 FORMAT                                                            MAP  804
     1(/1X,'CROSS-REFERENCE: GUN TYPES BY FIRING POINTS; DAILY '
     2,'PROPELLENT CHARGE WEIGHT (LBS)'////
     1                   1X,'GUN ID'/10X,7('FPT ID          '))         MAP  807
  753 FORMAT(T3,'.....     ERROR -- DUPLICATE ID,COORDINATES; DEF CARDS MAP  808
     1CHECKED FOR ERRORS,BUT OTHERWISE IGNORED')                        MAP  809
  754 FORMAT(T3,'.....     ERROR -- DUPLICATE ID,DIFFERENT COORDINATES; MAP  810
     1TREATED AS SEPARATE ENTRY.')                                      MAP  811
  755 FORMAT(T24,'GENERATED ID = ',A3)                                  MAP  812
  756 FORMAT(/T3,'.....     ERROR --',I3,' NONPOSITIVE CHARGE NOS. ENCOUMAP  813
     1NTERED FOR FIRING PT. ',A3)                                       MAP  814
  757 FORMAT(T3,'.....     ERROR -- NONPOSITIVE TARGET CHARGE IN TABLE FMAP  815
     1OR GUN ',A3,' ;DEF CARD FOR FIRING PT.',A3)                       MAP  816
  899 FORMAT(///T3,']]]]]     ERROR -- MISSING DATA BASE FILE; EXECUTIONMAP  817
     1 ABORTED')                                                        MAP  818
 900  FORMAT(//,T15,'.....     TIME FOR MAPPING SUBPROGRAM IS ',F8.3,   MAP  819
     1' SECONDS')                                                       MAP  820
      END                                                               MAP  821
