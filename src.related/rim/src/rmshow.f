      SUBROUTINE RMSHOW(*)
      INCLUDE 'syspar.d'
C
C     SHOW PARAMETER VALUES
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'tokens.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'msgcom.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'maccom.d'
      INCLUDE 'tupler.d'
 
      LOGICAL Q
      LOGICAL EQKEYW, NE
      INTEGER FMTSTR(Z), SMAC(Z)
 
      IF (EQKEYW(2,'LIMITS')) GOTO 400
      IF (EQKEYW(2,'MACROS')) GOTO 500
C
C     SHOW DATE/TIME
C
      CALL RMDATE(TDAY)
      CALL RMTIME(TTIM)
      MSUNIT = NOUT
      CALL DMSG(TDAY,0,'+',KZDATE)
      CALL MSG(' ','  ','+')
      CALL DMSG(TTIM,0,' ',KZTIME)
C
      IF (DFLAG) THEN
         CALL MSG(' ','DATABASE ''','+')
         CALL AMSG(DBNAME,-ZC,'+')
         CALL MSG(' ',''' IS OPEN','+')
      ENDIF
      IF (NE(NAME,BLANK)) THEN
         CALL MSG(' ','   CURRENT TABLE IS: ','+')
         CALL AMSG(NAME,-ZC,'+')
      ENDIF
      CALL MSG(' ',' ',' ')
 
      CALL MSG(' ',' ',' ')
C
C     CHECK FOR ARGUMENT
C
      IF (ITEMS.GT.1) THEN
         IF (EQKEYW(2,'USER')) THEN
            CALL MSG(' ','USER IS ','+')
            CALL AMSG(USERID,ZC,' ')
            GOTO 900
         ENDIF
         IF (EQKEYW(2,'DATE') .OR. EQKEYW(2,'TIME')) GOTO 900
      ENDIF
C
C     NO ARGUMENT MEANS SHOW ALL PARAMETERS
C
      CALL MSG(' ','STRING MATCHING  ','+')
      IF (CASEIG) CALL MSG(' ','CASE: IGNORE ','+')
      IF (.NOT.CASEIG) CALL MSG(' ','CASE: RESPECT','+')
 
      CALL MSG(' ','        SINGLE ARBCHAR: ','+')
      MSGPTR = MSGPTR + 1
      CALL PUTT(MSGREC,MSGPTR,ARBCHS)
      CALL MSG(' ','   MULTIPLE ARBCHAR: ','+')
      MSGPTR = MSGPTR + 1
      CALL PUTT(MSGREC,MSGPTR,ARBCHM)
      CALL AMSG(0,0,' ')
 
      CALL MSG(' ','FORMATS          ','+')
      CALL MSG(' ','DATE: ','+')
      CALL FMTDEC(KRMDTF,KZDATE,FMTSTR,12)
      CALL AMSG(FMTSTR,12,'+')
      CALL MSG(' ','   TIME: ','+')
      CALL FMTDEC(KRMTMF,KZTIME,FMTSTR,12)
      CALL AMSG(FMTSTR,12,' ')
 
      CALL MSG(' ','                 ','+')
      CALL MSG(' ','INTEGER: ','+')
      CALL FMTDEC(KRMINF,KZINT,FMTSTR,12)
      CALL AMSG(FMTSTR,12,'+')
      CALL MSG(' ','REAL: ','+')
      CALL FMTDEC(KRMRNF,KZREAL,FMTSTR,12)
      CALL AMSG(FMTSTR,12,' ')
 
      CALL MSG(' ','MISSING VALUES   ','+')
      CALL MSG(' ','MV: ','+')
      CALL AMSG(KMSSVT,KMSSVL,'+')
      CALL MSG(' ','             NA: ','+')
      CALL AMSG(KNAPVT,KNAPVL,' ')
C
      CALL MSG(' ','TERMINAL         ','+')
      CALL MSG(' ','WIDTH ','+')
      CALL IMSG(UTERML,4,'+')
      CALL MSG(' ','           ECHO: ','+')
      IF (ECHO) CALL MSG(' ','ON',' ')
      IF (.NOT.ECHO) CALL MSG(' ','OFF',' ')
 
      CALL MSG(' ','REPORTS          ','+')
      CALL MSG(' ','WIDTH ','+')
      CALL IMSG(UPRINL,4,'+')
      CALL MSG(' ','           HEIGHT ','+')
      CALL IMSG(ULPP,4,' ')
      GOTO 900
 
C
C     SHOW LIMITS
C
400   CALL MSG(' ','NAME LENGTH      ','+')
      CALL IMSG(ZC,5,' ')
      CALL MSG(' ','KEYWORD LENGTH   ','+')
      CALL IMSG(ZKEYWL,5,' ')
      CALL MSG(' ','FILENAME LENGTH  ','+')
      CALL IMSG(ZFNAML,5,' ')
      CALL MSG(' ','INPUT TOKENS     ','+')
      CALL IMSG(ZMTOK,5,' ')
      CALL MSG(' ','INPUT CHARS      ','+')
      CALL IMSG(ZMASC,5,' ')
      CALL MSG(' ','SELECT COLUMNS   ','+')
      CALL IMSG(ZMSEL,5,' ')
      CALL MSG(' ','SORT COLUMNS     ','+')
      CALL IMSG(ZMSRT,5,' ')
      CALL MSG(' ','WHERE COLUMNS    ','+')
      CALL IMSG(ZMWHR,5,' ')
      CALL MSG(' ','INPUT REC CHARS  ','+')
      CALL IMSG(ZCARDL,5,' ')
      CALL MSG(' ','OUTPUT REC CHARS ','+')
      CALL IMSG(ZPRINL,5,' ')
      CALL MSG(' ','MACROS           ','+')
      CALL IMSG(ZMXMAC,5,' ')
      GOTO 900
C
C     SHOW MACROS
C
500   IF (ITEMS.GT.2) CALL LXSREC(3,SMAC,ZC)
      DO 600 I = 1, MACNUM
      IF (ITEMS.EQ.3 .AND. NE(SMAC,MACNAM(1,I))) GOTO 600
      MSUNIT = NOUT
      CALL AMSG(MACNAM(1,I),-ZC,'+')
      CALL MSG(' ',' = ','+')
      Q = .FALSE.
      DO 550 J = 1, MACLEN(I)
      CALL GETT(MACTXT(MACPTR(I)),J,CH)
      IF (CH.GE.32) THEN
         IF (.NOT.Q) CALL MSG(' ','''','+')
         Q = .TRUE.
         IF (MSGPTR.GT.ZPRINL-2) CALL MSG(' ',' ',' ')
         MSGPTR = MSGPTR + 1
         CALL PUTT(MSGREC,MSGPTR,CH)
      ELSE
         IF (Q) CALL MSG(' ','''','+')
         Q = .FALSE.
         IF (MSGPTR.GT.ZPRINL-4) CALL MSG(' ',' ',' ')
         CALL MSG(' ',' ','+')
         CALL IMSG(CH,NDIGIT(CH),'+')
         CALL MSG(' ',' ','+')
      ENDIF
550   CONTINUE
      IF (Q) CALL MSG(' ','''','+')
      IF (MSGPTR.GT.0) CALL MSG(' ',' ',' ')
600   CONTINUE
 
C
C---- EXIT
C
900   RETURN 1
      END
