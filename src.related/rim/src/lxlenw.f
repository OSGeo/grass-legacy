      FUNCTION LXLENW(I)
      INCLUDE 'syspar.d'
C
C  RETURN THE LENGTH IN WORDS FOR THE ITH ITEM.
C
      INCLUDE 'tokens.d'
      LXLENW = 1
      IF (TOKTYP(I,KXTEXT)) LXLENW = (IDL(I) - 1) / ZCW + 1
      RETURN
      END
