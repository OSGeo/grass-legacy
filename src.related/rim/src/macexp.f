      SUBROUTINE MACEXP(M)
C
C --- TEXT PARSING ROUTINE
C
C     EXPANDS A MACRO (M) DEFINITION AT ITEMS
C
C-----------------------------------------------------
C
      INCLUDE 'syspar.d'
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'flags.d'
      INCLUDE 'cards.d'
      INCLUDE 'maccom.d'
 
      MI = ITEMS
C     ACCUMULATE ARGUMENTS
      NARGS = MACNRG(M)
      XARGS = 0
      IF (NARGS.GT.0) THEN
         DO 100 I = 1, NARGS
         CALL NXTTOK(EOR)
         IF (EOR.NE.0) GOTO 110
100      XARGS = I
      ENDIF
 
C     MOVE TO WORK AREA
 
110   CH = ABLANK
      IF (LXEOC.NE.0 .OR. XARGS.NE.NARGS) CH = 0
      MACWPT = MACWPT - 1
      CALL PUTT(MACWRK(1),MACWPT,CH)
      LXEOC = 0
 
      DO 200 I = MACLEN(M),1,-1
      CALL GETT(MACTXT(MACPTR(M)),I,CH)
      IF (CH.GT.31) THEN
         MACWPT = MACWPT - 1
         CALL PUTT(MACWRK(1),MACWPT,CH)
      ELSEIF (CH.LE.XARGS) THEN
         ITM = MI + CH
         CALL STRMOV(ASCREC(IDP(ITM)),1,IDL(ITM),
     1               MACWRK(1),MACWPT-IDL(ITM))
         MACWPT = MACWPT - IDL(ITM)
      ENDIF
200   CONTINUE
      ITEMS = MI - 1
      RETURN
      END
