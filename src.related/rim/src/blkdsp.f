      SUBROUTINE BLKDSP(NAME,BLK,CODES,LABELS,LLABEL)
      INCLUDE 'syspar.d'
C
C     DISPLAY A BLOCK OF DATA TO LOG
C
      INCLUDE 'rmatts.d'
      INTEGER BLK(1), IBLK
      LOGICAL LBLK
      EQUIVALENCE(IBLK,LBLK)
      CHARACTER*(*) NAME, CODES
      CHARACTER*6   LABELS(LLABEL)
C
      CALL MSG(' ',NAME,' ')
C
      BPTR = 1
      DO 100 I = 1, LEN(CODES)
      CALL MSG(' ',' ','+')
      CALL IMSG(I,4,'+')
      CALL MSG(' ','[','+')
      CALL IMSG(BPTR,4,'+')
      CALL MSG(' ',']','+')
      IF (I.LE.LLABEL) THEN
         CALL MSG(' ','"' // LABELS(I) // '"','+')
      ELSE
         CALL MSG(' ','"      "','+')
      ENDIF
      CALL MSG(' ',' = :','+')
      IF (CODES(I:I).EQ.'Z') THEN
         CALL AMSG(BLK(BPTR),ZC,'+')
         CALL MSG(' ',':',' ')
         BPTR = BPTR + Z
      ELSE IF (CODES(I:I).EQ.'I') THEN
         CALL IMSG(BLK(BPTR),16,' ')
         BPTR = BPTR + 1
      ELSE IF (CODES(I:I).EQ.'H') THEN
         CALL ITOH(H1,H2,BLK(BPTR))
         CALL IMSG(H1,8,'+')
         CALL IMSG(H2,8,' ')
         BPTR = BPTR + 1
      ELSE IF (CODES(I:I).EQ.'L') THEN
         IBLK = BLK(BPTR)
         IF (LBLK) THEN
            CALL MSG(' ','TRUE',' ')
         ELSE
            CALL MSG(' ','FALSE',' ')
         ENDIF
         BPTR = BPTR + 1
      ELSE IF (CODES(I:I).EQ.'D') THEN
         CALL DMSG(BLK(BPTR),0,' ',KZDATE)
         BPTR = BPTR + 1
      ELSE IF (CODES(I:I).EQ.'T') THEN
         CALL DMSG(BLK(BPTR),0,' ',KZTIME)
         BPTR = BPTR + 1
      ELSE
         CALL IMSG(BLK(BPTR),16,' ')
         BPTR = BPTR + 1
      ENDIF
100   CONTINUE
      RETURN
      END
