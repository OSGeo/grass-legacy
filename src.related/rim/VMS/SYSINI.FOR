      SUBROUTINE SYSINI
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C
C     *** VAX SYSTEM DEPENDENT ROUTINE ***
C
C     SYSTEM INITIALIZE
C
      INCLUDE 'FILES.DCK'
      INCLUDE 'PROM.DCK'
C
C     GET FILENAME FROM COMMAND LINE
C
      CHARACTER*(ZFNAML) FNAME
      LOGICAL CHKFIL
 
      open (unit=znout,file=ztrmou,status='unknown')
 
      CALL LIB$GET_FOREIGN(FNAME,,LEN)
 
      IF (LEN.EQ.0) THEN
C        input is terminal
         open (unit=znint,file=ztrmin,status='unknown')
      else
C        INPUT IS FILE
         OPEN(UNIT=ZNINT,FILE=FNAME,STATUS='OLD',IOSTAT=ERR)
         IF (ERR.NE.0) THEN
            CALL MSG(' ','COULD NOT OPEN FILE: ' // FNAME,' ')
            CALL EXIT
         ENDIF
         BATCH = .TRUE.
         conni = .false.
         conno = .false.
         prmpt = .false.
      ENDIF
 
C     Look for setup file
      IF (CHKFIL('SYS$LOGIN:LOGIN.RIM',rw)) 
     1   CALL SETIN('SYS$LOGIN:LOGIN.RIM')
      RETURN
      END
