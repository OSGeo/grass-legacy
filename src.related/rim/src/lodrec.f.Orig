      SUBROUTINE LODREC
      INCLUDE 'syspar.d'
C
C  SUBROUTINE TO READ A FREE-FIELD FORMAT INPUT RECORD.
C
C
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'tokens.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'maccom.d'
C
      LOGICAL EQKEYW
      CHARACTER*2 PKW
      INTEGER MACTST(Z)
C
C     ACCUMULATE TOKENS UNTIL END-OF-RECORD
C
C
10    ITEMS = 0
      ASCNXT = 1
90    PKW = '  '
100   CALL NXTTOK(EOR)
      IF (EOR.EQ.0) THEN
C
C        CHECK FOR LIGATURES
C
C        '*(' IS START OF COMMENT
C
         IF (KWS(ITEMS).EQ.'(' .AND. PKW.EQ.'*') THEN
            CALL LXCMNT(EOR)
            IF (EOR.NE.0) GOTO 200
            GOTO 90
         ENDIF
C
C           '>=', '<=' , AND '<>' ARE SINGLE TOKENS
C
         IF (KWS(ITEMS).EQ.'=' .AND.
     1       (PKW.EQ.'>'.OR.PKW.EQ.'<')) THEN
            ITEMS = ITEMS - 1
            KWS(ITEMS)(2:2) = '='
            CALL PUTT(ASCREC(IDP(ITEMS)),2,EQSIGN)
            IDL(ITEMS) = 2
         ENDIF
         IF (KWS(ITEMS).EQ.'>' .AND.
     1       (PKW.EQ.'<')) THEN
            ITEMS = ITEMS - 1
            KWS(ITEMS)(2:2) = '>'
            CALL PUTT(ASCREC(IDP(ITEMS)),2,GTSIGN)
            IDL(ITEMS) = 2
         ENDIF
         PKW = KWS(ITEMS)
C
C        CHECK FOR MACROS
C
         IF (MACNUM.EQ.0) GOTO 100
         IF (.NOT.TOKTYP(ITEMS,KXNAME)) GOTO 100
         IF (EQKEYW(1,'MACRO')) GOTO 100
 
         CALL LXSREC(ITEMS,MACTST,ZC)
         M = LOCMAC(MACTST)
         IF (M.NE.0) CALL MACEXP(M)
         GOTO 100
      ENDIF
C
200   CONTINUE
C
C     ON EOF: REREAD IF READING THE TERMINAL
C             SUPPLY 'END' IF READING THE ALTERNATE FILE
C             ELSE IS ERROR
C
900   IF (INEOF.NE.0) THEN
         IF (CONNI) THEN
            INEOF = 0
            GOTO 10
         ELSE IF (BATCH .AND. NINT.EQ.ZNINT) THEN
            CALL RMCLOS
            CALL MSG('E','EOF REACHED ON THE BATCH INPUT FILE',' ')
            CALL SYSTRP('CLEAR')
            CALL SYSEXI
            CALL EXIT
         ELSE
            ITEMS = 2
            KWS(1) = 'END'
            KWS(2) = '*EOF*'
            CALL SETIN(ZTRMIN)
         ENDIF
      ENDIF
C
C     REREAD ON NULL INPUT OR ERROR
C
      IF (ITEMS.EQ.0 .OR. EOR.LT.0) THEN
         IF (READCD.LT.0) THEN
            RMSTAT = 4
            RETURN
         ENDIF
         READCD = 0
         GOTO 10
      ENDIF
C
C     POSSIBLY ECHO INPUT
C
      IF (ECHO) THEN
         DO 920 I = 1, ITEMS
         CALL MSG(' ',' ','+')
         IF (IDT(I).EQ.KXTEXT) CALL MSG(' ','''','+')
         CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
         IF (IDT(I).EQ.KXTEXT) CALL MSG(' ','''','+')
920      CONTINUE
         CALL MSG(' ',' ',' ')
      ENDIF
      IF (TRACE.GT.20) CALL TOKDSP
 
C
C     EXIT WITH GOOD TOKENS
C
      RETURN
      END
