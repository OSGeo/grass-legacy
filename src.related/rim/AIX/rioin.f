      SUBROUTINE RIOIN(FILE,RECORD,BUFFER,NWDS,IOS)
      INCLUDE 'syspar.d'
C
C     **UNIX SYSTEM DEPENDENT ROUTINE **
C
C  PURPOSE:   ROUTINE FOR RANDOM INPUT
C
C  PARAMETERS
C         FILE----UNIT FOR IO
C         RECORD--RECORD NUMBER WANTED
C         BUFFER--BUFFER TO READ INTO
C         NWDS----NUMBER OF WORDS PER BUFFER
C         IOS-----STATUS VARIABLE - 0 MEANS SUCCESS, ELSE TILT
C
      INTEGER BUFFER(1)
      INCLUDE 'flags.d'
      INCLUDE 'rio.d'
 
      READ(FILE,REC=RECORD,IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      IUN = FILE - ZNFIL1 + 1
      IRECPS(IUN) = RECORD
      IF (TRACE.GE.3) THEN                                              
         CALL MSG('T','RIOIN: ','+')                                    
         CALL IMSG(FILE,5,'+')                                          
         CALL IMSG(RECORD,5,'+')                                        
         CALL IMSG(NWDS,5,'+')                                          
         CALL IMSG(IOS,4,' ')                                           
      ENDIF                                                             
      RETURN
      END
