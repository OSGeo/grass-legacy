      SUBROUTINE TIOOUT(FILE,TEXT,LEN,IERR)
      INCLUDE 'syspar.d'
C
C     **UNIX SYSTEM DEPENDENT ROUTINE **
C
C  ROUTINE TO WRITE A LINE OF TEXT
C
C  PARAMETERS
C
C         FILE----UNIT TO WRITE
C         TEXT----OUTPUT RECORD (PACKED ASCII-TEXT)
C         LEN-----NUMBER OF CHARACTERS IN TEXT
C         IERR----ERROR FLAG
C
      INTEGER FILE
      INTEGER TEXT(1)
      INCLUDE '../src/files.d'
 
      CHARACTER*1 CHRASC, ffchar
      CHARACTER*(ZPRINL) OULINE
C
      FFCHAR = ' '
      IF (FFFLAG.NE.0) THEN
         FFCHAR = '1'
         FFFLAG = 0
      ENDIF
C
C  CONVERT OUTPUT TO TEXT CHARS
C
100   OULINE = ' '
      IF (LEN.LE.0) THEN
         WRITE(FILE,'(A1)') FFCHAR
      ELSE
         L = LEN
         IF (L.GT.ZPRINL) L = ZPRINL
         DO 200 I = 1,L
         CALL GETT(TEXT,I,CH)
         OULINE(I:I) = CHRASC(CH)
200      CONTINUE
c        no forms control on terminal
         if (nint.eq.znint .or. ulpp.eq.0) then
            WRITE(FILE,'(1X,A)') OULINE(1:L)
         else
            WRITE(FILE,'(A1,A)') FFCHAR, OULINE(1:L)
         endif
      ENDIF
      IERR = 0
      RETURN
      END
