      SUBROUTINE CHGPSW(*)
      INCLUDE 'syspar.d'
C
C     CHANGE A PASSWORD
C
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
C
      LOGICAL EQKEYW
      LOGICAL NE
C
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C
C     CHECK FOR PERMISSION
C
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8,0,0)
         GOTO 999
      ENDIF
C
C     DO WHATEVER
C
      IF (EQKEYW(2,'OWNER'))   THEN
         IF (.NOT.EQKEYW(3,'TO')) GOTO 800
         IF (.NOT.TOKTYP(4,KXNAME)) GOTO 810
         CALL LXSREC(4,OWNER,ZC)
         IFMOD = .TRUE.
         GOTO 999
      ENDIF
 
      IF (EQKEYW(2,'RPW') .OR. EQKEYW(2,'MPW')) THEN
         IF (.NOT.EQKEYW(3,'TO')) GOTO 800
         IF (.NOT.EQKEYW(5,'FOR')) GOTO 800
         IF(ITEMS.NE.6) GO TO 800
         CALL LXSREC(6,RNAME,ZC)
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
           CALL WARN(1,RNAME,0)
           GO TO 999
         ENDIF
         L = LOCPRM(RNAME,2)
CCC      IF(L.NE.0) GO TO 999
         IF(.NOT.TOKTYP(4,KXNAME)) GOTO 810
         CALL RELGET(ISTAT)
C
C        CHANGE THE PASSWORD.
C
         IF(EQKEYW(2,'RPW')) THEN
           CALL LXSREC(4,RPW,ZC)
         ELSE
           CALL LXSREC(4,MPW,ZC)
         ENDIF
         CALL RELPUT
         GOTO 999
      ENDIF
C
C     ERRORS
C
800   CALL WARN(4,BLANK,BLANK)
      GOTO 999
810   CALL WARN(7,ASCREC(IDP(4)),0)
      GOTO 999
C
C---- EXIT
C
999   RETURN 1
      END
