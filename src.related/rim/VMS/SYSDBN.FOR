      SUBROUTINE SYSDBN(DBN,F1N,F2N,F3N,FXN)
      INCLUDE 'SYSPAR.DCK'
C
C     *** VAX SYSTEM DEPENDENT ROUTINE ***
C
C     BUILD FILENAMES FROM THE DATABASE NAME
C
C        DBN  =  Database name
C        F1N  =  File 1 name  
C        F2N  =  File 2 name  
C        F3N  =  File 3 name  
C        FXN  =  Setup file name
 
      INCLUDE 'ASCPAR.DCK'
      INCLUDE 'FLAGS.DCK'
C
      CHARACTER*(*) DBN,F1N,F2N,F3N,FXN
 
      COMMON /SYSEXT/ DBDRF
      CHARACTER*(ZFNAML) DBDRF
 
      CHARACTER*(ZFNAML) CDBN
C
C     Use name from DBDRF unless help DB open
C
      CDBN = DBN
      IF (LIBFLG.EQ.0) THEN
         CDBN = DBDRF
      ELSE
         CDBN = '$$RIM:' // CDBN
      ENDIF
 
      DO 10 I = 1,ZFNAML
      IF (CDBN(I:I).EQ.' ') GOTO 12
10    L = I
      L = ZFNAML
12    CONTINUE
 
      F1N = CDBN(1:L) // '.RIMDB1'
      F2N = CDBN(1:L) // '.RIMDB2'
      F3N = CDBN(1:L) // '.RIMDB3'
      FXN = CDBN(1:L) // '.RIM'
 
      STATUS = 0
      RETURN
      END
