      SUBROUTINE SYSINI
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     SYSTEM INITIALIZE
C
      INCLUDE '../src/files.d'
      INCLUDE '../src/prom.d'
C
C     GET FILENAME FROM COMMAND LINE
C
      CHARACTER*(ZFNAML) FNAME,home
      LOGICAL CHKFIL
      double precision d0
 
C     ignore any possible float overflows
      d0 = 0
C---- call trpfpe(0,d0)

      na = iargc()
 
      IF (na.gt.0) THEN
C        INPUT IS FILE
         call getarg(1,fname)
         OPEN(UNIT=ZNINT,FILE=FNAME,STATUS='OLD',IOSTAT=ERR)
         IF (ERR.NE.0) THEN
            CALL MSG(' ','COULD NOT OPEN FILE: ' // FNAME,' ')
            CALL EXIT(1)
         ENDIF
         BATCH = .TRUE.
         conni = .false.
         conno = .false.
         prmpt = .false.
      ENDIF
 
C     Look for setup file ( ~/.rimrc )
      call getenv('HOME',home)
      do 100 i = 1, zfnaml
      bp = i
100   if (home(i:i).eq.' ') goto 110
110   home(bp:zfnaml) = '/.rimrc'
      IF (CHKFIL(home,rw))  call setin(home)
      RETURN
      END
