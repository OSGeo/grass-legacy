      FUNCTION LOCMAC(MAC)
C
C     LOCATES A MACRO BY NAME (MAC)
C
C     LOCMAC = POSITION OF MACRO OR 0 IF NOT FOUND
C
C-----------------------------------------------------
C
      INCLUDE 'syspar.d'
      INCLUDE 'maccom.d'
 
      LOGICAL EQ
      INTEGER MAC(Z)
 
      DO 100 I = 1, MACNUM
      IF (EQ(MAC,MACNAM(1,I))) THEN
         LOCMAC = I
         RETURN
      ENDIF
100   CONTINUE
      LOCMAC = 0
      RETURN
      END
