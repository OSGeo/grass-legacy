      LOGICAL FUNCTION VARADD()
      INCLUDE 'syspar.d'
C
C     ADD A VARIABLE TO THE VAR LIST
C
C
      INCLUDE 'ascpar.d'
      INCLUDE 'incore.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'buffer.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'flags.d'
      INCLUDE 'pgmcom.d'
C
100   IF (PGVPTR+2+Z+ATTWDS.GT.PGVMAX) THEN
         CALL MSG('E','VARIABLE SPACE IS EXHAUSTED.',' ')
         VARADD = .FALSE.
         RETURN
      ENDIF
 
      IF (ATTWDS.LE.0) ATTWDS = 1
      CALL HTOI(ATTCHA,ATTWDS,ATTLEN)
      CALL ZMOVE(BUFFER(PGVPTR),ATTNAM)
      BUFFER(PGVPTR+Z) = ATTLEN
      BUFFER(PGVPTR+Z+1) = ATTYPE
      IF (ATTYPE.NE.4) THEN
         BUFFER(PGVPTR+Z+2) = 0
      ELSE
         CALL FILCH(BUFFER(PGVPTR+Z+2),1,ATTCHA,ABLANK)
      ENDIF
      PGVPTR = PGVPTR + Z + 2 + ATTWDS
      VARADD = .TRUE.
      RETURN
      END
