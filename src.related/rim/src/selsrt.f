      FUNCTION SELSRT(FTOK,NTOK)
      INCLUDE 'syspar.d'
C
C     PROCESS THE QUERY SORT CLAUSE
C
C     FTOK IS THE FIRST SORT TOKEN ('SORT')
C     NTOK IS THE NUMBER OF SORT TOKENS
C
      LOGICAL SELSRT
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'selcom.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tupler.d'
      INCLUDE 'files.d'
      INCLUDE 'srtcom.d'
      INCLUDE 'dclar1.d'
      LOGICAL EQKEYW,SAORD, EQTOK
C
C     *********************************************** S T A R T
C
      NSOVAR = 0
      NKSORT = 1
      SELSRT = .FALSE.
C
C
C     VALIDATE ATTRIBUTES  (MAY BE 'SORTED BY')
C
      TP = FTOK + 1
      LTP = FTOK + NTOK - 1
      IF (KWS(TP).EQ.'BY') TP = TP + 1
      IF(TP.GT.LTP) THEN
        CALL MSG('E','YOU HAVE NOT SPECIFIED ANY COLUMNS TO SORT.',' ')
        GOTO 900
      ENDIF
 
100   CALL LXSREC(TP,ANAME,ZC)
      CALL LOCATT(ANAME,NAME)
      CALL ATTGET(STATUS)
      IF (STATUS.NE.0) THEN
         CALL WARN(3,ANAME,NAME)
         GOTO 900
      ENDIF
 
      NSOVAR = NSOVAR + 1
      IF(NSOVAR.GT.ZMSRT)THEN
          CALL MSG('E','TOO MANY SORT COLUMNS.',' ')
          GOTO 900
          ENDIF
C
C     CHECK FOR MODIFIERS
C
      SAORD = .TRUE.
      IF (TP.LT.LTP) THEN
      IF (EQTOK(TP+1,EQSIGN)) THEN
        IF (EQKEYW(TP+2,'A')) THEN
           SAORD = .TRUE.
        ELSE IF (EQKEYW(TP+2,'D')) THEN
           SAORD = .FALSE.
        ELSE
           CALL MSG('E','SORT DIRECTION IS ''A'' OR ''D''.',' ')
           GOTO 900
        ENDIF
        TP = TP + 2
      ENDIF
      ENDIF
C
C  CHECK FOR VARIABLE LENGTH - SORTING ON VARIABLE LENGTH
C  ATTRIBUTES IS ALLOWED ONLY FOR TEXT.
C
      IF(ATTWDS.EQ.0 .AND. ATTYPE.NE.KZTEXT) THEN
        CALL MSG('E','NON-TEXT VARIABLE LENGTH ATTRIBUTES CANNOT' //
     X     ' BE SORTED.',' ')
        GOTO 900
      ENDIF
C
C  LOAD ARRAYS
C
      SORTYP(NSOVAR) = SAORD
      VARPOS(NSOVAR) = ATTCOL
      L=1
      CALL TYPER(ATTYPE,SVM,TYP)
      IF(TYP.EQ.KZREAL) L=2
      IF(TYP.EQ.KZDOUB) L=3
      IF(TYP.EQ.KZTEXT) L=4
      VARTYP(NSOVAR) = L
      VARLEN(NSOVAR) = ATTWDS
500   TP = TP + 1
      IF(TP.LE.LTP) GO TO 100
C
      IF (NSOVAR.EQ.0) GOTO 900
      OFFSET = 0
 
      SELSRT = .TRUE.
900   RETURN
      END
