      SUBROUTINE LXCMNT(EOR)
      INCLUDE 'syspar.d'
C
C     REMOVE A COMMENT FROM THE INPUT TOKEN STREAM
C     THE COMMENT MAY HAVE A SET COMMAND WITHIN
C
C     ON ENTRY:   TOKEN(ITEMS-1) = *
C                 TOKEN(ITEMS)   = (
C
C     ON EXIT:    ITEMS = ITEMS - 2
C
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'tokens.d'
      INCLUDE 'lxlcom.d'
C
      CHARACTER*(ZKEYWL) OPT
C
      SAVITM = ITEMS - 2
      SAVNXT = ASCNXT - 2
      SET = 1
C
100   ITEMS = SAVITM
      ASCNXT = SAVNXT
      CALL NXTTOK(EOR)
C
C     EOF IS ERROR END OF COMMENT IF BATCH
C
      IF (INEOF.NE.0) THEN
         IF (.NOT.BATCH) GOTO 100
         EOR = -1
         RETURN
      ENDIF
C
C     END OF LINE IS IGNORED
C
      IF (EOR.NE.0) GOTO 100
C
C     ')' IS END OF COMMENT
C
      IF (KWS(ITEMS).EQ.')') THEN
         ITEMS = SAVITM
         ASCNXT = SAVNXT
         RETURN
      ENDIF
C
C     CHECK FOR SET COMMAND IN PROGRESS
C
      GOTO (310,320,330,340)  SET
C       ELSE IGNORE THIS FIELD
        GOTO 100
C
C       LOOK FOR 'SET'
310     IF (KWS(ITEMS).EQ.'SET') THEN
           SET = 2
        ELSE
           SET = 0
        ENDIF
        GOTO 100
C
C       GET OPTION TO SET
320     IF (TOKTYP(ITEMS,KXKEYW)) THEN
           OPT = KWS(ITEMS)
           SET = 3
        ELSE
           SET = 0
        ENDIF
        GOTO 100
C
C       LOOK FOR =
330     IF (KWS(ITEMS).EQ.'=') THEN
           SET = 4
        ELSE
           SET = 0
        ENDIF
        GOTO 100
C
C       COPY TOKEN AND DO THE SET
340     IF (KWS(ITEMS).EQ.'NULL') THEN
           ASCHR = NULL
        ELSE
           CALL GETT(ASCREC(IDP(ITEMS)),1,ASCHR)
        ENDIF
390     IF (OPT.EQ.'BLANK')  ASBLK  = ASCHR
        IF (OPT.EQ.'COMMA')  ASCOM  = ASCHR
        IF (OPT.EQ.'PLUS')   ASPLUS = ASCHR
        IF (OPT.EQ.'SEMI')   ASSEMI = ASCHR
C       PROPER VERSIONS
        IF (OPT.EQ.'DEL')  ASCOM  = ASCHR
        IF (OPT.EQ.'CON')  ASPLUS = ASCHR
        IF (OPT.EQ.'END')  ASSEMI = ASCHR
        SET = 0
        GOTO 100
C
      END
