      LOGICAL FUNCTION ATOI(ASTR,SC,NC,VAL)
      INCLUDE 'syspar.d'
C
C     CONVERT ASCII-TEXT TO INTEGER AND RETURN TRUE IF OK
C
C     SC -- STARTING CHAR IN ASTR
C     NC -- NUMBER OF CHARS
C
      INCLUDE 'lxlcom.d'
C
      VAL = 0
      SGN = 1
      DO 100 I = 1, NC
      CALL GETT(ASTR,SC+I-1,A)
      IF (A.EQ.ASPACE) THEN
         GOTO 100
      ELSE IF (A.EQ.MNSIGN) THEN
         SGN = -1
      ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
         VAL = VAL*10 + A - U0
      ELSE
         ATOI = .FALSE.
         VAL = 0
         RETURN
      ENDIF
100   CONTINUE
      VAL = VAL * SGN
      ATOI = .TRUE.
      RETURN
      END
