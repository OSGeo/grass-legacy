      INTEGER FUNCTION NDIGIT(INT)
      INCLUDE 'syspar.d'
C
C     RETURN THE NUMBER OF DIGITS IN INT
C
      NDIGIT = 0
      ABS = INT
      IF (INT.LT.0) THEN
         ABS = 0 - INT
         NDIGIT = 1
      ENDIF
100   NDIGIT = NDIGIT + 1
      ABS = ABS/10
      IF (ABS.GT.0) GOTO 100
      RETURN
      END
