      SUBROUTINE CNENCD
     $
     $(DPVAR,CHRVAR,JSTAT)
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
C RESTRICTED  Use, duplication, or disclosure    DYNAMIC GRAPHICS, INC.<
C RIGHTS      is subject to restrictions         2855 TELEGRAPH AVE.   <
C LEGEND      stated in license agreement with:  BERKELEY, CA 94705    <
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
C
C  PURPOSE : Encode a double precision number into a 16 char field.
C
C  AUTHOR/DATE : A Kaugars, 13 JUL 87
C
C  REVISED BY   DATE       EXPLANATION
C
C----------------------------------------------------------------------+
C     D E C L A R A T I O N S
C----------------------------------------------------------------------+

C===  COMMON blocks and INSERT files

C......................................................................+

C===  Input arguments

C  Value to encode
      DOUBLE PRECISION  DPVAR
C......................................................................+


C===  Output arguments

C  Variable to encode into
      CHARACTER*(*)     CHRVAR
C  Return status
      INTEGER           JSTAT
C......................................................................+

C===  Subroutines referenced

C......................................................................+

C===  Local variables

      CHARACTER*30      COUT
      CHARACTER*1       CX
      CHARACTER*20      FMT
C======================================================================+

      JSTAT = 0
      NUMCHR = 16

C---  If DPVAR is too big or little, use 'D' format.
      IF (ABS(DPVAR) .LT. 1.0D-7 .OR. ABS(DPVAR) .GE. 1.0D+15) THEN
         NTWIDE = NUMCHR
         NDEC = NUMCHR - 9
         GOTO 90
      ENDIF

C---  Encode (F34.14), compute format to use, encode again,
C---  and left justify.
      WRITE(COUT,'(F30.13)',ERR=9010) DPVAR
      ILOC = 1

      MAXLEN = 30
      IF (ILOC .LT. MAXLEN) THEN
         ITLOC = ILOC
         DO 20 ILOC = ITLOC,MAXLEN
            IF (COUT(ILOC:ILOC) .NE. ' ') GOTO 30
20       CONTINUE
      ENDIF
      ILOC = 0
30    CONTINUE

      NTWIDE = ILOC + NUMCHR - 1
      NDEC = NTWIDE - (30-13)
      NLEFT  = 31 - ILOC
      IF (DABS(DPVAR) .GE. 1.0) THEN
         IF (NLEFT .GT. NUMCHR) GOTO 90
         NDEC = NUMCHR - NLEFT - 1
      ELSE
C---     Find first non-zero decimal digit.
         DO 40 I=ILOC,NUMCHR
            CX = COUT(I:I)
            IF (CX .GE. '1' .AND. CX .LE. '9') GOTO 50
40       CONTINUE
         GOTO 9010
50       CONTINUE
         ILOC = I
         NDEC = ILOC - 17 + 13
         IF (NDEC .GT. (NUMCHR-3)) NDEC = NUMCHR - 3
      ENDIF
      NTOT = NLEFT + 1 + NDEC
      IF (NTOT .GT. NUMCHR) GOTO 90
      IF (NDEC .LT. 0) NDEC = 0
      WRITE(FMT,5010) NTWIDE,NDEC
5010  FORMAT('(F',I2,'.',I2,')')
      WRITE(COUT,FMT,ERR=9010) DPVAR

C---  Left-justify string for output.
      DO 70 ILOC = 1,MAXLEN
         IF (COUT(ILOC:ILOC) .NE. ' ') GOTO 80
70    CONTINUE
      ILOC = 1
80    CONTINUE

      CHRVAR(1:NUMCHR) = COUT(ILOC:ILOC+NUMCHR-1)
      GOTO 9999

C---  Output in (Dw.d) format.
90    CONTINUE
      NDEC = MIN(NDEC,NTWIDE-6)
      IF (NDEC .LT. 0) GOTO 9010
      WRITE(FMT,5020) NTWIDE,NDEC
5020  FORMAT('(D',I2,'.',I2,')')
      WRITE(CHRVAR,FMT,ERR=9010) DPVAR
      GOTO 9999
C......................................................................+

C===  Error conditions

9010  CONTINUE
      JSTAT = 1
      GOTO 9999
C......................................................................+

C===  Return

9999  CONTINUE
      RETURN
      END
