      SUBROUTINE SYSDBN(DBN,F1N,F2N,F3N,FXN)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     BUILD FILENAMES FROM THE DATABASE NAME
C
C        DBN  =  Database name
C        F1N  =  File 1 name  
C        F2N  =  File 2 name  
C        F3N  =  File 3 name  
C        FXN  =  Setup file name
 
      CHARACTER*(*) DBN,F1N,F2N,F3N,FXN 
    
 
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
C
      COMMON /SYSEXT/ DBDRF
      CHARACTER*(ZFNAML) DBDRF
 
      CHARACTER*(ZFNAML) CDBN, xdbn
C
C     Use name from DBDRF unless help DB open
C
      xdbn = DBN
 
      IF (LIBFLG.EQ.0) THEN
         CDBN = DBDRF
      ELSE
         CDBN = '/usr/local/lib/' // xdbn
      ENDIF
 
      DO 10 I = 1,ZFNAML
      IF (CDBN(I:I).EQ.' ') GOTO 12
10    L = I
      L = ZFNAML
12    CONTINUE
 
      F1N = CDBN(1:L) // '.rimdb1'
      F2N = CDBN(1:L) // '.rimdb2'
      F3N = CDBN(1:L) // '.rimdb3'
      FXN = CDBN(1:L) // '.rim'
 
      STATUS = 0
      RETURN
      END
