      SUBROUTINE RIOOPN(FNAME,FILE,NWDS,IOS)
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C
C     *** VAX SYSTEM DEPENDENT ROUTINE ***
C
C  PURPOSE:   COVER ROUTINE TO OPEN A RANDOM FILE
C
C  PARAMETERS
C         FNAME---NAME OF THE FILE TO OPEN
C         FILE----UNIT TO OPEN
C         NWDS----NUMBER OF WORDS PER RECORD
C         IOS-----STATUS VARIABLE - O MEANS SUCCESS, ELSE TILT
C
      INCLUDE 'RIO.DCK'
      CHARACTER*(*) FNAME
C
      close(file)
 
C     Ignore name if a scratch file
 
      if (file.eq.znsrt1 .or. file.eq.znsrt2) then
        OPEN  (UNIT=FILE,FILE=FNAME,ACCESS='DIRECT',STATUS='SCRATCH',
     .         RECL=NWDS,IOSTAT=IOS)
      else
        OPEN  (UNIT=FILE,FILE=FNAME,ACCESS='DIRECT',STATUS='UNKNOWN',
     .         RECL=NWDS,IOSTAT=IOS,SHARED)
        IF (IOS .GT. 0)
     .     OPEN  (UNIT=FILE,FILE=FNAME,ACCESS='DIRECT',STATUS='UNKNOWN',
     .         RECL=NWDS,IOSTAT=IOS,SHARED,READONLY)
      endif
      if (ios.ne.0) write(6,1000) ios, file,fname
1000  format(' rioopn: ',i4,i4,2x,a)
 
      IUN = FILE - ZNFIL1 + 1
      IRECPS(IUN) = 0
      RETURN
      END
