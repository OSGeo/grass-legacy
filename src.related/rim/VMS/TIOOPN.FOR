      SUBROUTINE TIOOPN(FILE,UNIT,MODE,ERR)
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C
C     *** VAX SYSTEM DEPENDENT ROUTINE ***
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
 
      INCLUDE 'FLAGS.DCK'
 
C
      character*(zfnaml) ffile
      LOGICAL EXI
C
      ERR = 0
      ffile = file
C
C
C     PUT IN DEFAULT FILE TYPES ('RIM' FOR INPUT, 'LIS' FOR OUTPUT)
C
      IF (FILE.NE.ztrmin) THEN
        L = 0
        DO 50 I = ZFNAML, 1, -1
        IF (FFILE(I:I).EQ.':' .OR. FFILE(I:I).EQ.']') goto 55
        IF (FFILE(I:I).EQ.' ' .OR. FFILE(I:I).EQ.'.') THEN
           IF (L.NE.0) GOTO 60
        ELSE
           IF (L.EQ.0) L = I
        ENDIF
50      CONTINUE
C
55      IF (MODE.EQ.'INPUT') THEN
           FFILE = FFILE(1:L) // '.RIM'
        ELSE
           FFILE = FFILE(1:L) // '.LIS'
        ENDIF
      ENDIF
C
60    INQUIRE(FILE=FFILE,EXIST=EXI)
c
      IF (MODE.EQ.'INPUT' .and. exi) THEN
         OPEN(UNIT=UNIT,FILE=FFILE,STATUS='OLD',IOSTAT=ERR)
      ELSE IF (MODE.EQ.'OUTPUT') THEN
         OPEN(UNIT=unit,FILE=FFILE,STATUS='UNKNOWN',IOSTAT=ERR,
     1        CARRIAGECONTROL='LIST')
      ELSE
         ERR = 1
      ENDIF
      INLINE = 0
      RETURN
      END
