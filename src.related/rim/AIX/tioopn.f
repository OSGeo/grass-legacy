      SUBROUTINE TIOOPN(FILE,UNIT,MODE,ERR)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     OPEN FILE FOR TEXT IO
C         FILE -- FILE NAME
C         UNIT -- UNIT NUMBER
C         MODE -- 'INPUT' OR 'OUTPUT'
C         ERR  -- = 0 IF OPEN OK
C
      CHARACTER*(*) FILE
      CHARACTER*(*) MODE
      CHARACTER*2 UN
C
      INCLUDE 'flags.d'
 
      CHARACTER*(ZFNAML) FFILE
      LOGICAL EXI
C
      ERR = 0
      FFILE = FILE
C
C     Because unix lends less importance to extensions the  
C     default extensions are NOT added.  If they were,
C     this is how it would be done.
C
CC      IF (FILE.NE.ztrmin) THEN
CC        L = 0
CC        DO 50 I = ZFNAML, 1, -1
CC        IF (FFILE(I:I).EQ.'/') goto 55
CC        IF (FFILE(I:I).EQ.' ' .OR. FFILE(I:I).EQ.'.') THEN
CC           IF (L.NE.0) GOTO 60
CC        ELSE
CC           IF (L.EQ.0) L = I
CC        ENDIF
CC50      CONTINUE
CCC
CC55      IF (MODE.EQ.'INPUT') THEN
CC           FFILE = FILE(1:L) // '.rim'
CC        ELSE
CC           FFILE = FILE(1:L) // '.lis'
CC        ENDIF
CC      ENDIF
C
60    INQUIRE(FILE=FFILE,EXIST=EXI)
C
      IF (MODE.EQ.'INPUT' .AND. EXI) THEN
         OPEN(UNIT=UNIT,FILE=FFILE,STATUS='OLD',IOSTAT=ERR)
      ELSE IF (MODE.EQ.'OUTPUT') THEN
         open(unit=UNIT,FILE=FFILE,STATUS='UNKNOWN',IOSTAT=ERR)
      ELSE
         ERR = 1
      ENDIF
      INLINE = 0
      RETURN
      END
