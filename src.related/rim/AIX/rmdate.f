      SUBROUTINE RMDATE(DAT)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C  PURPOSE:   RETURN THE CURRENT DATE AS INTEGER
C
C  PARAMETERS:
C         DAT-----THE CURRENT DATE
C
      CALL CZDATE(Y,M,D)
      CALL JULDAT(D,M,Y,DAT)
      RETURN
      END