      SUBROUTINE LXINIT
      INCLUDE 'syspar.d'
C
C     INITIALIZE THE LXLCOM VARIABLES
C
      INCLUDE 'lxlcom.d'
 
      ASBLK  = ASCCHR(' ')
      ASBLAN = ASCCHR(' ')
      ASCOM  = ASCCHR(',')
      ASDOL  = 0
      ASPLUS = ASCCHR('+')
      ASQUO  = SQUOTE
      ASLPAR = ASCCHR('(')
      ASRPAR = ASCCHR(')')
      ASSEMI = ASCCHR(';')
      RETURN
      END
