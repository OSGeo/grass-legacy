      SUBROUTINE SELRPT
      INCLUDE 'syspar.d'
C
C     REPORT SELECTED DATA
C
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'buffer.d'
      INCLUDE 'tupler.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'files.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'selcom.d'
      INCLUDE 'whcom.d'
      INCLUDE 'msgcom.d'
C
      LOGICAL SGTLNK
      LOGICAL ALDONE,ATDONE
      LOGICAL PRISUM, OKLINE
      DOUBLE PRECISION RVAL,DSUM(ZMSEL)
      INTEGER KVAL(2),SUM(2,ZMSEL), CNT(ZMSEL)
      INTEGER NUMSEL
      EQUIVALENCE (RVAL,KVAL(1)),(DSUM(1),SUM(1,1))
C
      LKERR = 0
 
      NUMSEL = 0
      OKLINE = .TRUE.
      SLVMHD = .TRUE.
      CALL FILCH(LINE,1,UPRINL,BLANK)
      DO 20 I = 1, ZMSEL
      CNT(I) = 0
      SUM(1,I) = 0
20    SUM(2,I) = 0
 
      NLINE = 1
      IF (ULPP.GT.0) FFFLAG = 1
      CALL MSG('R',' TABLE: ','+')
      CALL AMSG(NAME,-ZC,' ')
      CALL AMSG(TITLE,-UPRINL,' ')
      CALL AMSG(MINUS,-UPRINL,' ')
C
C  OPEN THE SORT FILE IF WE HAVE "SORTED BY ....... "
C
      LENGTH = NCOL
      IF(NS.EQ.1) CALL GTSORT(MP,1,-1,LENGTH)
C
C     FUNCTION SELECT IS SPECIAL CASE
      IF (SFUNCT) GOTO 500
C
C     LOOP ON RECORDS
C
50    IF(NS.EQ.1) CALL GTSORT(MP,1,1,LENGTH)
      IF(NS.NE.1) CALL RMLOOK(MP,1,1,LENGTH)
      IF(RMSTAT.NE.0) GO TO 200
C
C     POSSIBLE SYSTEM INTERRUPTION
C
      IF (HXFLAG.NE.0) GOTO 9998
C
      DO 55 II=1,NUMATT
55    CURPOS(II) = 1
 
      NUMSEL = NUMSEL + 1
C
C     LOOP ON LINES
C
C     ALDONE WILL BE TRUE WHEN ALL PARAGRAPHING IS COMPLETED
      ALDONE = .FALSE.
C
70    ALDONE = .TRUE.
      CALL FILCH(LINE,1,UPRINL,BLANK)
C
C     LOOP ON ATTRIBUTES
C
      DO 100 I=1,NUMATT
 
C     IF LINKED - GET LINKED TUPLE --- ELSE USE MAIN TUPLE
      IF (LNKFL(I).NE.0) THEN
        IF (SGTLNK(I,MP,LP)) THEN
           IP = LP
        ELSE
           LKERR = LKERR + 1
           GOTO 50
        ENDIF
      ELSE
        IP = MP
      ENDIF
 
C     FIX VARIABLE LENGTH ATTRIBUTES
      IF(VAR(I)) THEN
        JP = IP + FP(I) - 1
        JP = BUFFER(JP) + IP - 1
        LEN(I) = BUFFER(JP)
        CALL TYPER(ATYPE(I),SVM,TYP)
        IF(TYP.EQ.KZTEXT) LEN(I) = BUFFER(JP+1)
        IF(TYP.EQ.KZDOUB) LEN(I) = LEN(I)/2
        ROWD(I) = BUFFER(JP+1)
        IF(SVM.EQ.KZMAT) COLD(I) = LEN(I)/ROWD(I)
      ENDIF
 
      JP = IP + FP(I) - 1
      IF(VAR(I)) JP = BUFFER(JP) + IP + 1
 
      CALL SELOUT(BUFFER(JP),I,ATDONE)
      ALDONE = ALDONE.AND.ATDONE
C
C     DO SUMMATIONS
C
      IF (.NOT.SUMFLG(I)) GOTO 100
      IF(BUFFER(JP).EQ.ZIMISS) GO TO 100
      IF(BUFFER(JP).EQ.ZINAPP) GO TO 100
      IF(ATYPE(I).EQ.KZINT) THEN
        SUM(1,I) = SUM(1,I) + BUFFER(JP)
      ELSE
        IF(ATYPE(I).EQ.KZREAL) THEN
           CALL RTOD(KVAL,BUFFER(JP))
        ELSE
           KVAL(1) = BUFFER(JP)
           KVAL(2) = BUFFER(JP+1)
        ENDIF
        DSUM(I) = DSUM(I) + RVAL
      ENDIF
 
100   IF (INDCUR.NE.1) CALL RMRES(1)
 
      MSUNIT = NOUTR
      CALL AMSG(LINE,-UPRINL,' ')
      NLINE = NLINE + 1
 
      IF(ULPP.NE.0 .AND. NLINE.GT.ULPP) THEN
        FFFLAG = 1
        CALL MSG('R',' TABLE: ','+')
        CALL AMSG(NAME,-ZC,' ')
        CALL AMSG(TITLE,-UPRINL,' ')
        CALL AMSG(MINUS,-UPRINL,' ')
        NLINE = 1
      ENDIF
 
      IF (.NOT.ALDONE) GOTO 70
      GO TO 50
C
C     PRINT SUM
C
200   PRISUM = .FALSE.
      CALL FILCH(LINE,1,UPRINL,ABLANK)
      DO 250  I = 1, NUMATT
      IF(SUMFLG(I)) THEN
        CALL SELPUT(SUM(1,I),ATYPE(I),ITEMW(I),COL1(I),LINE)
        PRISUM = .TRUE.
      ENDIF
250   CONTINUE
C
      IF(PRISUM) THEN
        IF(ULPP.NE.0 .AND. NLINE .GE.ULPP-1) THEN
           FFFLAG = 1
           CALL MSG('R',' TABLE: ','+')
           CALL AMSG(NAME,-ZC,' ')
           CALL AMSG(MINUS,-UPRINL,' ')
           CALL AMSG(LINE,-UPRINL,' ')
        ENDIF
        MSUNIT = NOUTR
        CALL AMSG(LINE,-UPRINL,' ')
        NLINE = NLINE + 1
      ENDIF
      GOTO 9000
C
C
C     FUNCTION SELECT - NO VAR, NO LINKS, NO TEXT
C
C     LOOP ON RECORDS
C
500   IF(NS.EQ.1) CALL GTSORT(MP,1,1,LENGTH)
      IF(NS.NE.1) CALL RMLOOK(MP,1,1,LENGTH)
C
C     POSSIBLE SYSTEM INTERRUPTION
C
      IF (HXFLAG.NE.0) GOTO 9998
C
      IF (RMSTAT.EQ.0) NUMSEL = NUMSEL + 1
C
      GP = MP + FP(SGRPBY) - 1
510   IF ( (NUMSEL.NE.1 .AND. OLDGRP.NE.BUFFER(GP)) .OR.
     1      RMSTAT.NE.0) THEN
        IF (OKLINE) THEN
          DO 600 I = 1, NUMATT
          TYP = ATYPE(I)
          IF (FUNCT(I).EQ.1) THEN
             SUM(1,I) = CNT(I)
             TYP = KZINT
          ENDIF
          IF (FUNCT(I).EQ.3) THEN
            IF(ATYPE(I).EQ.KZINT) THEN
              TYP = KZREAL
              DSUM(I) = FLOAT(SUM(1,I))/CNT(I)
            ELSE
              DSUM(I) = DSUM(I)/CNT(I)
            ENDIF
          ENDIF
600       IF (FUNCT(I).NE.0)
     1       CALL SELPUT(SUM(1,I),TYP,FORMT(I),COL1(I),LINE)
          MSUNIT = NOUTR
          CALL AMSG(LINE,-UPRINL,' ')
          NLINE = NLINE + 1
 
          IF (ULPP.NE.0 .AND. NLINE.GT.ULPP) THEN
             FFFLAG = 1
             CALL MSG('R',' TABLE: ','+')
             CALL AMSG(NAME,-ZC,' ')
             CALL AMSG(TITLE,-UPRINL,' ')
             CALL AMSG(MINUS,-UPRINL,' ')
             NLINE = 1
          ENDIF
 
        ENDIF
        OKLINE = .TRUE.
        CALL FILCH(LINE,1,UPRINL,BLANK)
        DO 620 I = 1, ZMSEL
        CNT(I) = 0
        SUM(1,I) = ZIMISS
620     SUM(2,I) = 0
 
      ENDIF
      IF(RMSTAT.NE.0) GO TO 9000
      OLDGRP = BUFFER(GP)
      IF (.NOT.OKLINE) GOTO 500
C
C     ACCUMULATE ATTRIBUTE VALUES
C
      DO 700 I=1,NUMATT
 
C     IF LINKED AND FIRST TIME GET LINKED TUPLE - ELSE USE MAIN TUPLE
      IF (LNKFL(I).NE.0 .AND. CNT(I).EQ.0) THEN
        IF (SGTLNK(I,MP,LP)) THEN
           IP = LP
        ELSE
           LKERR = LKERR + 1
           OKLINE = .FALSE.
           GOTO 500
        ENDIF
      ELSE
        IP = MP
      ENDIF
 
      JP = IP + FP(I) - 1
C
      IF(BUFFER(JP).EQ.ZIMISS) GO TO 700
      IF(BUFFER(JP).EQ.ZINAPP) GO TO 700
      CNT(I) = CNT(I) + 1
      KVAL(1) = BUFFER(JP)
      KVAL(2) = BUFFER(JP+1)
      IF (FUNCT(I).EQ.0 .AND. CNT(I).EQ.1) THEN
C       FIX VARIABLE LENGTH ATTRIBUTES
        IF(VAR(I)) THEN
          JP = BUFFER(JP) + IP - 1
          LEN(I) = BUFFER(JP)
          CALL TYPER(ATYPE(I),SVM,TYP)
          IF(TYP.EQ.KZTEXT) LEN(I) = BUFFER(JP+1)
          IF(TYP.EQ.KZDOUB) LEN(I) = LEN(I)/2
          ROWD(I) = BUFFER(JP+1)
          IF(SVM.EQ.KZMAT) COLD(I) = LEN(I)/ROWD(I)
          JP = JP + 2
        ENDIF
 
C       NOTE... NO PARAGRAPHING
        CURPOS(I) = 1
        CALL SELOUT(BUFFER(JP),I,ATDONE)
      ELSE IF (FUNCT(I).EQ.1) THEN
        SUM(1,I) = SUM(1,I) + 1
      ELSE IF (CNT(I).EQ.1) THEN
        IF(ATYPE(I).EQ.KZINT) SUM(1,I) = KVAL(1)
        IF(ATYPE(I).NE.KZINT) DSUM(I) = RVAL
      ELSE IF (FUNCT(I).EQ.2 .OR. FUNCT(I).EQ.3) THEN
        IF(ATYPE(I).EQ.KZINT) SUM(1,I) = SUM(1,I) + KVAL(1)
        IF(ATYPE(I).NE.KZINT) DSUM(I) = DSUM(I) + RVAL
      ELSE IF (FUNCT(I).EQ.4) THEN
        IF(ATYPE(I).EQ.KZINT .AND. KVAL(1).LT.SUM(1,I))
     1                        SUM(1,I) = KVAL(1)
        IF(ATYPE(I).NE.KZINT .AND. RVAL.LT.DSUM(I))
     1                        DSUM(I) = RVAL
      ELSE IF (FUNCT(I).EQ.5) THEN
        IF(ATYPE(I).EQ.KZINT .AND. KVAL(1).GT.SUM(1,I))
     1                        SUM(1,I) = KVAL(1)
        IF(ATYPE(I).NE.KZINT .AND. RVAL.GT.DSUM(I))
     1                        DSUM(I) = RVAL
      ENDIF
  700 IF (INDCUR.NE.1) CALL RMRES(1)
      GOTO 500
C
9000  CALL MSG(' ',' ','+')
      CALL IMSG(NUMSEL,9,'+')
      CALL MSG(' ',' ROWS SELECTED.',' ')
 
      IF (LKERR.NE.0) THEN
        CALL MSG('W','   THERE WERE ','+')
        CALL IMSG(LKERR,9,'+')
        CALL MSG(' ',' LINK FAILURES.',' ')
      ENDIF
C
C     INTERRUPTION
C
9998  CALL WARN(6,0,0)
      IF (NS.EQ.1) THEN
         CALL BLKCLR(1)
         CLOSE (UNIT=ZNSRT+MRINDX,STATUS='DELETE')
      ENDIF
 
9999  RETURN
      END
