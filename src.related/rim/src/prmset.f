      SUBROUTINE PRMSET(MODE,PR)
      INCLUDE 'syspar.d'
C
C     SET THE PROMPT CHARACTERS
C
C     INPUT  - MODE... 'INIT' - SET INITIAL VALUES
C                      'SET'  - SET NEW VALUES
C                      'RESET'- RESTORE INITIAL VALUES
C              PR..... NEW VALUE FOR PROMPT
C
      CHARACTER*(*) MODE,PR
C
      INCLUDE 'prom.d'
      CHARACTER*1 CH
C
      IF (MODE.EQ.'INIT' .OR. MODE.EQ.'SET') THEN
         DO 10 I = 1, ZC
         IF (I.LE.LEN(PR)) THEN
            CH = PR(I:I)
         ELSE
            CH = ' '
         ENDIF
         AS = ASCCHR(CH)
         IF (I.GT.1) AS = LOCASE(AS)
10       CALL PUTT(PROM,I,AS)
      ENDIF
C
      IF (MODE.EQ.'INIT') CALL ZMOVE(INIPRM,PROM)
      IF (MODE.EQ.'RESET') CALL ZMOVE(PROM,INIPRM)
      RETURN
      END
