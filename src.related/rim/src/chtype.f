      LOGICAL FUNCTION CHTYPE(TYPE,ASCHR)
      INCLUDE 'syspar.d'
C
C     RETURN TRUE IF ASCHR IS OF TYPE 'TYPE'
C     TYPE MAY BE
C
C         'LETTER' - (UPPER OR LOWER CASE, UNDERSCORE)
C         'DIGIT'  - (0-9)
C         'XDIGIT' - (0-9, PLUS, MINUS, DECIMAL, 'E')
C         'DELIMIT'- (DELIMITERS)
C
      CHARACTER*(*) TYPE
      INCLUDE 'ascpar.d'
      INCLUDE 'lxlcom.d'
C
C
      IF (TYPE.EQ.'LETTER') THEN
         IF ( (ASCHR.GE.LA .AND. ASCHR.LE.LZ) .OR.
     X        (ASCHR.GE.UA .AND. ASCHR.LE.UZ) .OR.
     X        (ASCHR.EQ.USCOR) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
C
      IF (TYPE.EQ.'DIGIT') THEN
         IF (ASCHR.GE.U0 .AND. ASCHR.LE.U9) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
C
      IF (TYPE.EQ.'XDIGIT') THEN
         IF ( (ASCHR.GE.U0 .AND. ASCHR.LE.U9) .OR.
     X        (ASCHR.EQ.PLSIGN) .OR. (ASCHR.EQ.MNSIGN) .OR.
     X        (ASCHR.EQ.UECH) .OR. (ASCHR.EQ.LECH) .OR.
     X        (ASCHR.EQ.DECIM) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
C
C     DELIMITERS ARE: BLANK , ( ) < > = ' " : @ %
C     NOTE: = - * / ARE NOT DELIMITERS
C             (- IS ALSO UNIARY, * IS WILD-CARD, / OCCURS OFTEN IN DATES
C
      IF (TYPE.EQ.'DELIMIT') THEN
         IF ( (ASCHR.EQ.ASBLK)  .OR. (ASCHR.EQ.ASCOM) .OR.
     X        (ASCHR.EQ.ASLPAR) .OR. (ASCHR.EQ.ASRPAR).OR.
     X        (ASCHR.EQ.SQUOTE) .OR. (ASCHR.EQ.DQUOTE).OR.
     X        (ASCHR.EQ.LTSIGN) .OR. (ASCHR.EQ.GTSIGN).OR.
     X        (ASCHR.EQ.ATSIGN) .OR. (ASCHR.EQ.PCSIGN).OR.
     X        (ASCHR.EQ.EQSIGN) .OR. (ASCHR.EQ.ASCOLN) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
C
C     UNRECOGNISED TYPE CODE
C
      CHTYPE = .FALSE.
      RETURN
      END
