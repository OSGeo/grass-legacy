      FUNCTION LXWREC(I,J)
      INCLUDE 'syspar.d'
C
C  RETURN THE JTH WORD OF THE ITH ITEM OF TEXT.
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
C
      LXWREC = BLANK(1)
      IF(TOKTYP(IDT(I),KXTEXT)) THEN
         K = IDP(I)
         NW = K + J - 1
         MAX = K + ((IDL(I) - 1) / ZCW)
         IF(NW.GT.MAX) RETURN
         LXWREC = ASCREC(NW)
      ENDIF
      RETURN
      END
