      SUBROUTINE SYSDBG(DBX,STATUS)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     SYSTEM DEP PROCESSING OF DATABASE
C     (GET DIRECTORY AND FILENAME FROM COMMAND LINE)
C
C     DBX = POINTER IN TOKENS TO DATABASE NAME
C
      INCLUDE '../src/tokens.d'
      INCLUDE '../src/cflags.d'
      COMMON /SYSEXT/ DBDRF
      CHARACTER*(ZFNAML) DBDRF
 
      PARAMETER (RSBCH=93, COLCH=58)
 
      STATUS = 0
C      DBDIR = ' '
      IF (ITEMS.NE.DBX) GOTO 800
 
c     note dbdrf contains both dir and name
      CALL STRASC(DBDRF,ASCREC(IDP(DBX)),IDL(DBX))
 
c     Extract the actual filename from the input. ie remove dir
 
c     look for '/'
      P = ASCAN(ASCREC,1,IDL(DBX),RSBCH,.TRUE.)
      IF (P.LE.0) P = ASCAN(ASCREC,1,IDL(DBX),COLCH,.TRUE.)
      DBFNAM = DBDRF(P+1:ZFNAML)
 
      RETURN
800   STATUS = 1
      CALL WARN(4,0,0)
      RETURN
      END
 
 
