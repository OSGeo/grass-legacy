      LOGICAL FUNCTION PGSTOR(PGM,LPGM)
      INCLUDE 'syspar.d'
C
C     STORE PART OF A PROGRAM
C
C     INPUT:   PGM-----PROGRAM TO STORE
C              LPGM----LENGTH OF PGM
C              IF LPGM<0 THEN ALLOCATE ENSURE SPACE ONLY
C
      INCLUDE 'incore.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'buffer.d'
      INCLUDE 'flags.d'
      INCLUDE 'pgmcom.d'
C
      PGSTOR = .FALSE.
100   IF (PGPPTR+IABS(LPGM).GT.PGPMAX) THEN
         PGPMAX = PGPMAX + 1000
         CALL BLKCHG(6,PGPMAX,1)
         IF (RMSTAT.NE.0) RETURN
         GOTO 100
      ENDIF
      IF (LPGM.GT.0) THEN
         CALL BLKMOV(BUFFER(PGPPTR),PGM,LPGM)
         PGPPTR = PGPPTR + LPGM
      ENDIF
      PGSTOR = .TRUE.
      RETURN
      END
