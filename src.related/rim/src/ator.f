      LOGICAL FUNCTION ATOR(ASTR,SC,NC,VAL)
      INCLUDE 'syspar.d'
C
C     CONVERT ASCII-TEXT TO DP AND RETURN TRUE IF OK
C
C     SC -- STARTING CHAR IN ASTR
C     NC -- NUMBER OF CHARS
C
      INCLUDE 'lxlcom.d'
 
      REAL DFAC, EXP
      DOUBLE PRECISION VAL, V
C
      ATOR = .FALSE.
      VAL = 0.0
      EXP = 0.0
      SVAL = 1
      SEXP = 1
      IFAC = 10
      DFAC = 1.0
C
      MODE = 0
      DO 100 I = 1, NC
      CALL GETT(ASTR,SC+I-1,A)
      IF (A.EQ.ASPACE) GOTO 100
C
C
      IF (MODE.EQ.0) THEN
C        VALUE PART OF THE NUMBER
         IF (A.EQ.PLSIGN) THEN
            SVAL = 1
         ELSE IF (A.EQ.MNSIGN) THEN
            SVAL = -1
         ELSE IF (A.EQ.DECIM) THEN
            IFAC = 1
            DFAC = 10.0
         ELSE IF (A.EQ.UECH .OR. A.EQ.LECH) THEN
            MODE = 1
         ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
            V   = FLOAT(A-U0)
            IF (IFAC.NE.1) THEN
               VAL = VAL*IFAC
            ELSE
               V   = V / DFAC
               DFAC = DFAC * 10.0
            ENDIF
            VAL = VAL + V
         ELSE
            VAL = 0.0
            RETURN
         ENDIF
      ELSE
C        EXPONENT
         IF (A.EQ.PLSIGN) THEN
            SEXP = 1
         ELSE IF (A.EQ.MNSIGN) THEN
            SEXP = -1
         ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
            V   = FLOAT(A-U0)
            EXP = EXP*10 + V
         ELSE
            VAL = 0.0
            RETURN
         ENDIF
      ENDIF
100   CONTINUE
      VAL = VAL * SVAL
      IF (EXP.NE.0.0) VAL = VAL * (10.0**(EXP*SEXP))
      ATOR = .TRUE.
      RETURN
      END
