      LOGICAL FUNCTION EQKEYW(I,KEYW)
      INCLUDE 'syspar.d'
C
C     THIS FUNCTION COMPARES KEYW WITH ITEM I OF THE
C     COMMAND TOKEN LIST
C
C     INPUT - I........ITEM NUMBER
C             KEYW.....STRING WITH KEYWORD IN IT
C     OUTPUT- EQKEYW....TRUE. IFF
C                             A. ITEM I IS TEXT
C                         AND B. NUMBER OF CHARACTERS IN ITEM I
C                                IS GE MIN(3,LEN) AND LE LEN.
C                         AND C. ITEM IT MATCHES KEYWORD TO MINIMUM
C                                OF 8 AND THE NUMBER OF CHARACTERS
C                                IN ITEM I.
C
      INCLUDE 'tokens.d'
 
      CHARACTER*(*) KEYW
      L = LEN(KEYW)
      EQKEYW = .FALSE.
      IF(I.GT.ITEMS) GO TO 1000
      IF(.NOT.TOKTYP(I,KXKEYW)) GO TO 1000
      N = IDL(I)
      MIN = 3
      IF(L.LT.MIN) MIN = L
      IF(N.LT.MIN) N = MIN
      IF(N.GT.L) GO TO 1000
      IF (KWS(I)(1:N).EQ.KEYW(1:N)) EQKEYW = .TRUE.
1000  RETURN
      END
