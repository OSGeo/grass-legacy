      SUBROUTINE REPORT(*)
      INCLUDE 'syspar.d'
C
C     PROCESS REPORT COMMAND
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'files.d'
      INCLUDE 'flags.d'
C
      CHARACTER*(ZFNAML) FN
      LOGICAL OUTFIL, EQKEYW
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C     CHECK FOR OUTPUT FILE
C
      IF (ITEMS.GT.1) THEN
         IF (ITEMS.NE.3 .OR. .NOT.EQKEYW(2,'TO')) THEN
            CALL WARN(4,0,0)
            GOTO 999
         ENDIF
         CALL STRASC(FN,ASCREC(IDP(3)),IDL(3))
         IF (KWS(3).EQ.'TERMINAL') FN = ZTRMOU
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         IF (STAT.NE.0) GOTO 999
         OUTFIL = .TRUE.
      ELSE
         OUTFIL = .FALSE.
      ENDIF
C
C
100   CALL PGCOMP
      IF (RMSTAT.NE.0) THEN
         CALL MSG(' ','COMPILATION HALTED.',' ')
         GOTO 900
      ENDIF
 
200   CALL PGEXEC
 
900   IF (OUTFIL) CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
 
999   RETURN 1
      END
