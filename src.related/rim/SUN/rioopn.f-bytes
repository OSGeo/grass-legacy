      SUBROUTINE RIOOPN(FNAME,FILE,NWDS,IOS)
      INCLUDE 'syspar.d'
C
C     *** UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     *** This version of RIOOPN specifies RECL in bytes! ***
C
C
C  PURPOSE:   COVER ROUTINE TO OPEN A RANDOM FILE
C
C  PARAMETERS
C         FNAME---NAME OF THE FILE TO OPEN
C         FILE----UNIT TO OPEN
C         NWDS----NUMBER OF WORDS PER RECORD
C         IOS-----STATUS VARIABLE - O MEANS SUCCESS, ELSE TILT
C
      INCLUDE '../src/rio.d'
      CHARACTER*(*) FNAME
C
C     Ignore names for scratch files
      if (file.eq.znsrt1 .or. file.eq.znsrt2) then
        OPEN  (UNIT=FILE,           ACCESS='DIRECT',STATUS='SCRATCH',
     .         RECL=NWDS*ZCW,IOSTAT=IOS)
      else
        OPEN  (UNIT=FILE,FILE=FNAME,ACCESS='DIRECT',STATUS='UNKNOWN',
     .         RECL=NWDS*ZCW,IOSTAT=IOS)
      endif
      if (ios.ne.0) write(6,1000) ios, file,fname
1000  format(' rioopn: ',i4,i4,2x,a)
 
      IUN = FILE - ZNFIL1 + 1
      IRECPS(IUN) = 1
      RETURN
      END
