      SUBROUTINE SYSTRP(MODE)
      INCLUDE 'syspar.d'
 
C     **UNIX SYSTEM DEPENDENT ROUTINE **
 
C     CATCH SYSTEM INTERRUPTS (CTRL-C)
C     On AIX this is all done in a c sub
 
      CHARACTER*(*) MODE
 
      call czsigl
      RETURN
      END
