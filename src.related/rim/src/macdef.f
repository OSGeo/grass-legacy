      SUBROUTINE MACDEF(*)
C
C     COMMAND ROUTINE TO DEFINE A MACRO
C
C     SYNTAX:  MACRO <NAME> = <TEXT> ARG# <TEXT> ...
C              MACRO <NAME> CLEAR
C
C     *  =  RETURN STATEMENT
C
C-----------------------------------------------------
C
      INCLUDE 'syspar.d'
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'flags.d'
      INCLUDE 'maccom.d'
 
      LOGICAL EQKEYW
      INTEGER MAC(Z)
 
      IF (ITEMS.LT.3) GOTO 8000
      IF (.NOT.TOKTYP(2,KXNAME)) GOTO 8000
      IF (ITEMS.EQ.3 .AND. KWS(3).EQ.'=') GOTO 9000
      CALL LXSREC(2,MAC,ZC)
C
C     IF NEW MACRO THEN FIND SLOT IN MACNAM
C     ELSE DELETE OLD DEFINITION AND REUSE
C
      M = LOCMAC(MAC)
      IF (M.EQ.0) THEN
         M = LOCMAC(BLANK)
         IF (M.EQ.0) THEN
            IF (MACNUM.GE.ZMXMAC) THEN
               CALL MSG('E','TOO MANY MACRO DEFINITIONS',' ')
               GOTO 9000
            ENDIF
            MACNUM = MACNUM + 1
            M = MACNUM
         ENDIF
      ELSE
         NW = (MACLEN(M)-1)/ZCW + 1
         ST = MACPTR(M)
         CALL BLKMOV(MACTXT(ST),MACTXT(ST+NW),MACNTX-ST-NW)
         DO 50 I = 1, MACNUM
50       IF (MACPTR(I).GT.ST) MACPTR(I) = MACPTR(I) - NW
         MACNTX = MACNTX - NW
         CALL ZMOVE(MACNAM(1,M),BLANK)
         MACPTR(M) = 0
      ENDIF
 
      IF (EQKEYW(3,'CLEAR')) GOTO 9000
      IF (KWS(3).NE.'=') GOTO 8000
 
      CALL ZMOVE(MACNAM(1,M),MAC)
C
C     COPY REPLACEMENT TEXT TO MACRO AREA
C     EACH TOKEN WILL BE SURROUNDED BY A SPACE
C
      WP = MACNTX
      PTR = 0
      NARG = 0
      DO 100 I = 4, ITEMS
C     PTR = PTR + 1
      IF (PTR.GT.ZMXMTX*ZCW-2) GOTO 8200
C     CALL PUTT(MACTXT(WP),PTR,ABLANK)
      IF (IDI(I).GT.0 .AND. IDI(I).LT.32) THEN
C        INTEGER IS PARAMETER NUMBER
         PTR = PTR + 1
         CALL PUTT(MACTXT(WP),PTR,IDI(I))
         IF (IDI(I).GT.NARG) NARG = IDI(I)
      ELSE
C        IS REPLACEMENT TEXT
         IF (PTR+IDL(I).GT.ZMXMTX*ZCW-1) GOTO 8200
         CALL STRMOV(ASCREC(IDP(I)),1,IDL(I),MACTXT(WP),PTR+1)
         PTR = PTR + IDL(I)
      ENDIF
C     PTR = PTR + 1
C     CALL PUTT(MACTXT(WP),PTR,ABLANK)
100   CONTINUE
      MACNRG(M) = NARG
      MACPTR(M) = WP
      MACLEN(M) = PTR
      MACNTX = WP + (PTR-1)/ZCW + 1
      GOTO 9000
 
8000  CALL WARN(4,0,0)
      GOTO 9000
8200  CALL MSG('E','TOO MUCH MACRO TEXT',' ')
 
9000  RETURN 1
      END
