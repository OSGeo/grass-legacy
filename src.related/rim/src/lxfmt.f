      SUBROUTINE LXFMT(I,TYP,FMT,LEN)
      INCLUDE 'syspar.d'
C
C     ENCODE A FORMAT STRING INTO AN INTEGER
C
C     I IS THE LX ITEM NUMBER
C     TYP IS THE DATA TYPE
C     FMT IS THE ENCODED FORMAT
C     LEN IS THE LENGTH OF THE DATA AS FORMATTED
C
      INCLUDE 'tokens.d'
      INCLUDE 'rmatts.d'
C
      CHARACTER*1 CHRASC
      CHARACTER*1 FC
      CHARACTER*12 CFMT
C
      LOGICAL T, ATOI
C
      FMT = 0
C     ALLOW FORMAT TO BE TEXT-ONLY STRING
      CFMT = ' '
      L = IDL(I)
      DO 10 J = 1, L
      CALL GETT(ASCREC(IDP(I)),J,A)
10    CFMT(J:J) = CHRASC(UPCASE(A))
      IF (TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) THEN
C
C     DATE/TIME FORMAT
C
        CALL DTFENC(TYP,FMT,CFMT(1:L))
        LEN = L
        GOTO 900
      ENDIF
C
C     OTHER TYPE (MXNN.DD)
C
500   DO 510 K = 1, 12
      NP = K
      FC = CFMT(K:K)
510   IF (FC.LT.'0' .OR. FC.GT.'9') GOTO 520
      GOTO 900
520   M = 0
      IF (NP.GT.1) THEN
         T = ATOI(ASCREC(IDP(I)),1,NP-1,M)
         IF (.NOT.T) GOTO 900
      ENDIF
C
      FC = CFMT(NP:NP)
      IF (FC.EQ.'T') FC = 'A'
      IF (FC.EQ.'R') FC = 'F'
      CALL TYPER(TYP,VTYP,STYP)
      IF (STYP.EQ.KZTEXT .AND. FC.NE.'A') GOTO 900
      IF (STYP.EQ.KZINT  .AND. FC.NE.'I') GOTO 900
      IF ((STYP.EQ.KZREAL .OR. STYP.EQ.KZDOUB) .AND.
     X   (FC.NE.'F' .AND. FC.NE.'E')) GOTO 900
      NP = NP + 1
C
C     GET NUMBERS
C
      DP = 0
      EP = 12
      DO 530 J = NP, 12
      EP = J-1
      IF (CFMT(J:J).EQ.' ') GOTO 540
530   IF (CFMT(J:J).EQ.'.') DP = J
      EP = 12
540   IF (DP.EQ.0) DP = EP+1
      N = 0
      D = 0
      IF (DP.GT.NP) T = ATOI(ASCREC(IDP(I)),NP,DP-NP,N)
      IF (.NOT.T) GOTO 900
      IF (DP.LT.EP) T = ATOI(ASCREC(IDP(I)),DP+1,EP-DP,D)
      IF (.NOT.T) GOTO 900
      LEN = N
      FMT = M*10000 + D*100 + N
      IF (FC.EQ.'E') FMT = 0 - FMT
C
900   IF (FMT.EQ.0) THEN
         CALL MSG('E',' ''','+')
         CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
         CALL MSG(' ',''' IS NOT A VALID FORMAT FOR THIS TYPE.',' ')
      ENDIF
      RETURN
      END
