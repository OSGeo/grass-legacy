      LOGICAL FUNCTION WHEVAL(ITUP)
      INCLUDE 'syspar.d'
C
C     EVALUATE THE WHERE CLAUSE FOR THE CURRENT TUPLE
C
C     ITUP-----POINTER TO TUPLE
C     RETURNS THE VALUE OF THE WHERE CLAUSE
C
 
      INCLUDE 'ascpar.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'whcom.d'
      INCLUDE 'start.d'
      INCLUDE 'buffer.d'
      INCLUDE 'flags.d'
      INCLUDE 'pgmcom.d'
C
      LOGICAL OK,BTEST
      LOGICAL EQTEST, LKSTR
      LOGICAL VSTK(20), LVALS(ZMWHR)
 
C     FIND ALL LOGICAL VALUES AND DO BOOLEANS
 
      DO 1000 J=1,NBOO
      ITYPE = KATTY(J)
      IF (ITYPE.LT.0) GOTO 1010
      IF(ITYPE.EQ.0)ITYPE = KZINT
      OK = .FALSE.
      CALL ITOH(NR,LEN,KATTL(J))
      NUM = KOMLEN(J)
      NK = KOMTYP(J)
      NUMP = KOMPOS(J)
 
      IF (KATTP(J).GT.0) THEN
         IP = ITUP + KATTP(J) - 1
      ELSE
         IP = 0 - KATTP(J)
      ENDIF
 
100   IF (NK.GE.10) THEN
C
C       ATTRIBUTE - ATTRIBUTE COMPARISON  (OR VARS)
C
        IF (NUMP.GT.0) THEN
           KP = ITUP + NUMP - 1
        ELSE
           KP = 0 - NUMP
        ENDIF
C
C       DUMMY TOLERANCE FOR ATTRIBUTE TO ATTRIBUTE
C
        IF(LEN.EQ.0) THEN
C
C         SET POINTER FOR VARIABLE ATTRIBUTES
C
          IP = BUFFER(IP) + ITUP - 1
          KP = BUFFER(KP) + ITUP - 1
          IF(NK.EQ.13) OK = .TRUE.
          LEN = BUFFER(IP)
          IF(BUFFER(KP).NE.BUFFER(IP)) GO TO 900
          IF(BUFFER(KP+1).NE.BUFFER(IP+1)) GO TO 900
          OK = .FALSE.
          IP = IP + 2
          KP = KP + 2
        ENDIF
        TTOL = TOL
        TOL = 0.0
        NK = NK - 10
        CALL KOMPXX(BUFFER(IP),BUFFER(KP),LEN,NK,OK,ITYPE)
        TOL = TTOL
        GO TO 900
      ENDIF
C
300   IF(LEN.EQ.0) THEN
C
C       SET POINTER FOR VARIABLE ATTRIBUTE
C
        IP = BUFFER(IP) + ITUP - 1
        LEN = BUFFER(IP)
        NR = BUFFER(IP+1)
        IP = IP + 2
      ENDIF
C
C     REGULAR ATTRIBUTE
C
320   NPOS = KOMPOS(J)
      NPOT = KOMPOT(J)
      OK = .TRUE.
      EQTEST = .FALSE.
      IF((NK.EQ.2).OR.(NK.EQ.9)) EQTEST = .TRUE.
      IF(EQTEST) OK = .FALSE.
 
325   DO 400 JJ=1,NUM
      BTEST = .FALSE.
      CALL ITOH(NNR,NW,WHRLEN(NPOT))
      IF(NK.LE.1) GO TO 350
      IF(BUFFER(IP).EQ.ZIMISS) GO TO 350
      IF(BUFFER(IP).EQ.ZINAPP) GO TO 350
      IF((LEN.EQ.NW).AND.(NR.EQ.NNR)) GO TO 350
C
C     COMPARE OF DIFFERENT LENGTHS
C
      IF(NK.EQ.9) GO TO 350
      IF(NK.NE.3) GO TO 375
      OK = .TRUE.
      GO TO 900
C
350   IF (NK.NE.9) THEN
         CALL KOMPXX(BUFFER(IP),WHRVAL(NPOS),NW,NK,BTEST,ITYPE)
      ELSE
         BTEST = LKSTR(BUFFER(IP),NR,WHRVAL(NPOS),NNR)
      ENDIF
C
375   IF(EQTEST) OK = OK.OR.BTEST
      IF(.NOT.EQTEST) OK = OK.AND.BTEST
      IF(OK.AND.EQTEST) GO TO 900
      NPOS = NPOS + NW
      NPOT = NPOT + 1
  400 CONTINUE
 
  900 CONTINUE
      LVALS(J) = OK
CC    WRITE(6,9992) J, OK
9992  FORMAT(' J, OK ',I5,L2)
1000  CONTINUE
 
C     EVALUATE THE CLAUSE
 
1010  IF (NBOO.LE.1) THEN
         WHEVAL = OK
         GOTO 9000
      ENDIF
      VSP = 0
      DO 1030 J = 1, NBOO
      OP = BOO(J)/10000
      IF (OP.NE.0) THEN
C        OPERATOR
         IF (OP.EQ.WHOR) THEN
            VSP = VSP - 1
            VSTK(VSP) = VSTK(VSP) .OR. VSTK(VSP+1)
CC       WRITE(6,'('' OR '',L2)') VSTK(VSP)
         ELSE IF (OP.EQ.WHAND) THEN
            VSP = VSP - 1
            VSTK(VSP) = VSTK(VSP) .AND. VSTK(VSP+1)
CC       WRITE(6,'('' AND'',L2)') VSTK(VSP)
         ELSE
            VSTK(VSP) = .NOT. VSTK(VSP)
CC       WRITE(6,'('' NOT'',L2)') VSTK(VSP)
         ENDIF
      ELSE
C        VALUE
         VSP = VSP + 1
         VSTK(VSP) = LVALS(BOO(J))
CC       WRITE(6,'('' VAL'',L2)') VSTK(VSP)
      ENDIF
1030  CONTINUE
      WHEVAL = VSTK(1)
 
9000  RETURN
      END
