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
      INCLUDE '../src/flags.d'
 
      character*(zfnaml) ffile
      LOGICAL EXI
C
      ERR = 0
      ffile = file
C
C     Because unix lends less importance to extensions the  
C     default extensions are NOT added.  If they were,
C     this is how it would be done.
C
cc      IF (FILE.NE.ztrmin) THEN
cc        L = 0
cc        DO 50 I = ZFNAML, 1, -1
cc        IF (FFILE(I:I).EQ.'/') goto 55
cc        IF (FFILE(I:I).EQ.' ' .OR. FFILE(I:I).EQ.'.') THEN
cc           IF (L.NE.0) GOTO 60
cc        ELSE
cc           IF (L.EQ.0) L = I
cc        ENDIF
cc50      CONTINUE
ccC
cc55      IF (MODE.EQ.'INPUT') THEN
cc           FFILE = FILE(1:L) // '.rim'
cc        ELSE
cc           FFILE = FILE(1:L) // '.lis'
cc        ENDIF
cc      ENDIF
C
60    INQUIRE(FILE=FFILE,EXIST=EXI)
c
      IF (MODE.EQ.'INPUT' .and. exi) THEN
         OPEN(UNIT=UNIT,FILE=FFILE,STATUS='OLD',IOSTAT=ERR)
      ELSE IF (MODE.EQ.'OUTPUT') THEN
         OPEN(UNIT=unit,FILE=FFILE,STATUS='UNKNOWN',IOSTAT=ERR)
      ELSE
         ERR = 1
      ENDIF
      INLINE = 0
      RETURN
      END
