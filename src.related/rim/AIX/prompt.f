      SUBROUTINE PROMPT(PTXT)
      INCLUDE 'syspar.d'
C
C     **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
C
C     ISSUE TERMINAL PROMPT
C
      INCLUDE 'ascpar.d'
      INCLUDE 'msgcom.d'
      INCLUDE 'files.d'
      CHARACTER*1 CHRASC
 
      CHARACTER*(ZC) P
 
      IF(NINT.EQ.ZNINT) THEN
        L = 0
        DO 10 I = 1, ZC
        CALL GETT(PTXT,I,A)
        IF (A.EQ.ABLANK) GOTO 11
        IF (I.NE.1) A = LOCASE(A)
        P(I:I) = CHRASC(A)
10      L = I
11      IF (L.NE.0) CALL CPROMPT(P,L)
      ENDIF
      RETURN
      END
