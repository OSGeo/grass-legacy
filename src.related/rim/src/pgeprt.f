      SUBROUTINE PGEPRT(PTR)
      INCLUDE 'syspar.d'
C
C     EXECUTE HEAD/FOOT PRINT
C
C     PTR-- ADDRESS OF SELCOM SAVE BLOCK
 
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'buffer.d'
      INCLUDE 'tupler.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'files.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'selcom.d'
      INCLUDE 'srtcom.d'
      INCLUDE 'whcom.d'
      INCLUDE 'msgcom.d'
C
      INCLUDE 'pgmcom.d'
      INCLUDE 'expcom.d'
 
      LOGICAL ALDONE, ATDONE
      LOGICAL PGEEXP, WHEVAL
 
      IF (PTR.EQ.0) RETURN
      P = PTR
C
C     SAVE CURRENT SELCOM
C
10    CALL BLKMOV(BUFFER(WHSAVE),NUMATT,ZSELCM)
      CALL BLKMOV(BUFFER(WHSAVE+ZSELCM),LIN1,ZMSEL)
 
C
C     PRINT
C
C     THIS IS DUPLICATED FROM PGEXEC SINCE NO RECURSION POSSIBLE
C
C
      CALL PGBRES('SEL',P)
 
      DO 1210 II=1,NUMATT
1210  CURPOS(II) = 1
 
      L = 1
1220  ALDONE = .TRUE.
      CALL FILCH(LINE,1,UPRINL,BLANK)
 
      DO 1240 I = 1, NUMATT
      IF (LIN1(I).GT.L) THEN
         ALDONE = .FALSE.
         GOTO 1240
      ENDIF
 
C     FIX VARIABLE LENGTH ATTRIBUTES
      IF(VAR(I)) THEN
        JP = TP + FP(I) - 1
        JP = BUFFER(JP) + TP - 1
        LEN(I) = BUFFER(JP)
        CALL TYPER(ATYPE(I),SVM,TYP)
        IF(TYP.EQ.KZTEXT) LEN(I) = BUFFER(JP+1)
        IF(TYP.EQ.KZDOUB) LEN(I) = LEN(I)/2
        ROWD(I) = BUFFER(JP+1)
        IF(SVM.EQ.KZMAT) COLD(I) = LEN(I)/ROWD(I)
      ENDIF
 
      IF (FP(I).GT.0) THEN
         JP = TP + FP(I) - 1
         IF(VAR(I)) JP = BUFFER(JP) + TP + 1
      ELSE
         JP = 0 - FP(I)
      ENDIF
 
1239  IF (ATYPE(I).EQ.0) THEN
C        OUTPUT FIXED TEXT
         IF (LIN1(I).EQ.L)
     1      CALL STRMOV(BUFFER(FP(I)),1,LEN(I),LINE,COL1(I))
      ELSE
         CALL SELOUT(BUFFER(JP),I,ATDONE)
         ALDONE = ALDONE.AND.ATDONE
      ENDIF
 
1240  CONTINUE
 
      MSUNIT = NOUTR
      CALL AMSG(LINE,-UPRINL,' ')
      CALL FILCH(LINE,1,UPRINL,BLANK)
      IF (ALDONE) GOTO 1260
      L = L + 1
      GOTO 1220
 
1260  CONTINUE
C
C     RESTORE OLD SELCOM
C
      CALL BLKMOV(NUMATT,BUFFER(WHSAVE),ZSELCM)
      CALL BLKMOV(LIN1,BUFFER(WHSAVE+ZSELCM),ZMSEL)
      RETURN
      END
