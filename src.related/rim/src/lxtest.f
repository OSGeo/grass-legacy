      PROGRAM MAIN
      INCLUDE 'syspar.d'
C
C     TEST LXL ROUTINES
C
      INCLUDE 'tokens.d'
      INCLUDE 'cards.d'
      INCLUDE 'files.d'
      INCLUDE 'rmatts.d'
C
      LOGICAL EQKEYW
C
      CHARACTER*1 FC
 
      CALL RMCONS
      CALL RMINIT
      CALL LXINIT
      CALL SYSINI
C     DON'T USE THE INIT FILE
      IF (NINT.NE.ZNINT) CALL SETIN(ZTRMIN)
      ECHO = .TRUE.
      READCD = 0
C
10    CALL LODREC
      IF (KWS(1).EQ.'END') GOTO 900
C
      CALL TOKDSP
C
      IF (EQKEYW(1,'MACRO')) THEN
         CALL MACDEF(*10)
      ENDIF
 
 
      IF (EQKEYW(1,'FORMAT')) THEN
         FC = KWS(2)(1:1)
         IF (FC.EQ.'A') TYP = KZTEXT
         IF (FC.EQ.'I') TYP = KZINT
         IF (FC.EQ.'F') TYP = KZREAL
         IF (FC.EQ.'D') TYP = KZDATE
         IF (FC.EQ.'T') TYP = KZTIME
         CALL LXFMT(3,TYP,FMT,LEN)
         CALL MSG(' ','FORMAT = ','+')
         CALL IMSG(FMT,10,'+')
         CALL MSG(' ',', LENGTH = ','+')
         CALL IMSG(LEN,5,' ')
      ENDIF
      GOTO 10
C
900   CALL SYSEXI
      CALL EXIT
      END
