      SUBROUTINE RMSET(*)
      INCLUDE 'syspar.d'
C
C  PURPOSE:  SET A PARAMETER
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'tokens.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
C
      LOGICAL EQKEYW
      CHARACTER*(ZFNAML) FN
C
C     DO WHATEVER
C
      OP = 1
      IF (EQKEYW(1,'SET'))     OP = 2
C
C     ----- SET DATABASE NAME -----
C
      IF (EQKEYW(OP,'NAME'))   THEN
         CALL ZMOVE(DBNAME,BLANK)
         IF (ITEMS.GT.OP) CALL LXSREC(OP+1,DBNAME,ZC)
         IFMOD = .TRUE.
         GOTO 900
      ENDIF
 
C
C     ----- SET PASSWORD -----
C
      IF (EQKEYW(OP,'USER'))   THEN
         CALL ZMOVE(USERID,NONE)
         IF (ITEMS.GT.OP) CALL LXSREC(OP+1,USERID,ZC)
         IFMOD = .TRUE.
         GOTO 900
      ENDIF
 
C
C     ----- SET ECHO ON/OFF -----
C
 
      IF (EQKEYW(OP,'ECHO'))    THEN
         IF (ITEMS.LE.OP)        ECHO = .TRUE.
         IF (EQKEYW(OP+1,'ON'))  ECHO = .TRUE.
         IF (EQKEYW(OP+1,'OFF')) ECHO = .FALSE.
         GOTO 900
      ENDIF
C
C     ----- SET CASE IGNORE/RESPECT -----
C
      IF (EQKEYW(OP,'CASE'))    THEN
         IF (EQKEYW(OP+1,'IGNORE')) THEN
            CASEIG = .TRUE.
         ELSE IF (EQKEYW(OP+1,'RESPECT')) THEN
            CASEIG = .FALSE.
         ELSE
            CALL WARN(4,0,0)
         ENDIF
         GOTO 900
      ENDIF
C
C     ----- SET DATE FORMAT <FORMAT> -----
C
      IF (EQKEYW(OP,'DATE'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZDATE,FMT,LEN)
         IF (FMT.NE.0) KRMDTF = FMT
         GOTO 900
      ENDIF
C
C     ----- SET TIME FORMAT <FORMAT> -----
C
      IF (EQKEYW(OP,'TIME'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZTIME,FMT,LEN)
         IF (FMT.NE.0) KRMTMF = FMT
         GOTO 900
      ENDIF
C
C     ----- SET INTEGER FORMAT <FORMAT> -----
C
      IF (EQKEYW(OP,'INTEGER'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZINT,FMT,LEN)
         IF (FMT.NE.0) KRMINF = FMT
         GOTO 900
      ENDIF
C
C     ----- SET REAL FORMAT <FORMAT> -----
C
      IF (EQKEYW(OP,'REAL'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZREAL,FMT,LEN)
         IF (FMT.NE.0) KRMRNF = FMT
         GOTO 900
      ENDIF
C
C     ----- SET INPUT <FILENAME> -----
C
      IF (EQKEYW(OP,'INPUT'))   THEN
         IF (ITEMS.LE.OP) THEN
            FN = ZTRMIN
         ELSE
            CALL STRASC(FN,ASCREC(IDP(OP+1)),IDL(OP+1))
            IF (KWS(OP+1).EQ.'TERMINAL') FN = ZTRMIN
         ENDIF
         CALL SETIN(FN)
         GOTO 900
      ENDIF
C
C     ----- SET OUTPUT <FILENAME> -----
C
      IF (EQKEYW(OP,'OUTPUT'))  THEN
         IF (ITEMS.LE.OP) THEN
            FN = ZTRMOU
         ELSE
            CALL STRASC(FN,ASCREC(IDP(OP+1)),IDL(OP+1))
            IF (KWS(OP+1).EQ.'TERMINAL') FN = ZTRMOU
         ENDIF
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         GOTO 900
      ENDIF
C
C     ----- SET MV <MISSING_VALUE_CHAR> -----
C
      IF (EQKEYW(OP,'MV'))   THEN
         KMSSVL = 0
         IF (ITEMS.GE.OP+1) THEN
            KMSSVL = IDL(OP+1)
            CALL LXSREC(OP+1,KMSSVT,ZC)
         ENDIF
         GOTO 900
      ENDIF
C
C     ----- SET NA <NOT_APPLICABLE_CHAR> -----
C
      IF (EQKEYW(OP,'NA'))   THEN
         KNAPVL = 0
         IF (ITEMS.GE.OP+1) THEN
            KNAPVL = IDL(OP+1)
            CALL LXSREC(OP+1,KNAPVT,ZC)
         ENDIF
         GOTO 900
      ENDIF
C
C     ----- SET SINGLE ARBCHAR <CHAR> -----
C
      IF (EQKEYW(OP,'SINGLE')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'ARBCHAR')) GOTO 800
         CALL GETT(ASCREC(IDP(OP+2)),1,ARBCHS)
         GOTO 900
      ENDIF
C
C     ----- SET MUITIPLE ARBCHAR <CHAR> -----
C
      IF (EQKEYW(OP,'MULTIPLE')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'ARBCHAR')) GOTO 800
         CALL GETT(ASCREC(IDP(OP+2)),1,ARBCHM)
         GOTO 900
      ENDIF
C
C     ----- SET TERMINAL WIDTH <VALUE> -----
C
      IF (EQKEYW(OP,'TERMINAL')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'WIDTH')) GOTO 800
         IF (.NOT.TOKTYP(OP+2,KXINT)) GOTO 800
         IF (IDI(OP+2).GT.ZPRINL) GOTO 810
         UTERML = IDI(OP+2)
         GOTO 900
      ENDIF
C
C     ----- SET REPORT WIDTH/HEIGHT <VALUE> -----
C
      IF (EQKEYW(OP,'REPORT')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.TOKTYP(OP+2,KXINT)) GOTO 800
         IF (EQKEYW(OP+1,'WIDTH')) THEN
            IF (IDI(OP+2).GT.ZPRINL) GOTO 810
            UPRINL = IDI(OP+2)
         ELSE IF (EQKEYW(OP+1,'HEIGHT')) THEN
            ULPP = IDI(OP+2)
         ELSE
            GOTO 800
         ENDIF
         GOTO 900
      ENDIF
 
C
C     (TRACE IS FOR DEBUGGING ONLY)
C     SET TRACE ON/OFF <FILENAME>
C
      IF (EQKEYW(OP,'TRACE'))  THEN
         TRACE = 1
         IF (EQKEYW(OP+1,'OFF')) TRACE = 0
         IF (TOKTYP(OP+1,KXINT)) TRACE = IDI(OP+1)
C
         L = LFIND(3,ITEMS-3,'TO')
         IF (L.GT.0) THEN
            IF (L.EQ.ITEMS) GOTO 800
            CALL STRASC(FN,ASCREC(IDP(L+1)),IDL(L+1))
            CALL SETOUT(NOUTT,ZNOUTT,FN,STAT)
            IF (STAT.NE.0) TRACE = 0
         ENDIF
         IF (TRACE.NE.0) CALL MSG('T','TRACE STARTED',' ')
         GOTO 900
      ENDIF
C
C---- ERRORS
C
800   CALL WARN(4,0,0)
      GOTO 900
C
810   CALL MSG('E','THE MAXIMUM WIDTH IS ','+')
      CALL IMSG(ZPRINL,4,' ')
C
C---- EXIT
C
900   RETURN 1
      END
