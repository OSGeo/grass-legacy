      FUNCTION PGEEXP(VP,TP)
      INCLUDE 'syspar.d'
C
C     EVALUATE AN EXPRESSION (FOR PROGRAM MODE)
C
C     VP --- LOCATION OF VARIABLE TO RECEIVE VALUE
C     RETURNS TRUE IF EVALUATION OK
C
      LOGICAL PGEEXP
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'buffer.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'expcom.d'
      INCLUDE 'lxlcom.d'
C
      INCLUDE 'dclar1.d'
 
      PARAMETER (NGSIGN=94)
      PARAMETER (VOP=1,MOP=2)
 
      INTEGER VSTK(20), VSTKTP(20), VAL, IVAL(2)
      DOUBLE PRECISION RVSTK(20), RVAL, DVAL
      EQUIVALENCE (DVAL,IVAL(1))
 
 
      PGEEXP = .TRUE.
      TYP = BUFFER(VP+Z+1)
      IF (TYP.EQ.KZTEXT) GOTO 500
      IF (TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) TYP = KZINT
 
C     EVALUATE THE EXPRESSION
 
      VSP = 0
      DO 200 J = 1, NEOP
      V = EOP(J)
      OP = V/10000
101   IF (OP.NE.0) THEN
C        OPERATOR
         TP1 = VSTKTP(VSP)
         TP0 = KZINT
         IF (TP0.EQ.KZTEXT .OR. TP1.EQ.KZTEXT) GOTO 7100
         IF (VSP.GT.1) TP0 = VSTKTP(VSP-1)
         IF (OP.EQ.PLSIGN) THEN
            VSP = VSP - 1
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZINT )
     1          VSTK(VSP) =  VSTK(VSP) +  VSTK(VSP+1)
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) =  VSTK(VSP) + RVSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZINT )
     1         RVSTK(VSP) = RVSTK(VSP) +  VSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) = RVSTK(VSP) + RVSTK(VSP+1)
         ELSE IF (OP.EQ.MNSIGN) THEN
            VSP = VSP - 1
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZINT )
     1          VSTK(VSP) =  VSTK(VSP) -  VSTK(VSP+1)
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) =  VSTK(VSP) - RVSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZINT )
     1         RVSTK(VSP) = RVSTK(VSP) -  VSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) = RVSTK(VSP) - RVSTK(VSP+1)
         ELSE IF (OP.EQ.TMSIGN) THEN
            VSP = VSP - 1
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZINT )
     1          VSTK(VSP) =  VSTK(VSP) *  VSTK(VSP+1)
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) =  VSTK(VSP) * RVSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZINT )
     1         RVSTK(VSP) = RVSTK(VSP) *  VSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) = RVSTK(VSP) * RVSTK(VSP+1)
         ELSE IF (OP.EQ.DVSIGN) THEN
            VSP = VSP - 1
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZINT )
     1          VSTK(VSP) =  VSTK(VSP) /  VSTK(VSP+1)
            IF (TP0.EQ.KZINT  .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) =  VSTK(VSP) / RVSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZINT )
     1         RVSTK(VSP) = RVSTK(VSP) /  VSTK(VSP+1)
            IF (TP0.EQ.KZDOUB .AND. TP1.EQ.KZDOUB)
     1         RVSTK(VSP) = RVSTK(VSP) / RVSTK(VSP+1)
         ELSE IF (OP.EQ.NGSIGN) THEN
            IF (TP1.EQ.KZINT ) VSTK(VSP) = 0 - VSTK(VSP)
            IF (TP1.EQ.KZDOUB) RVSTK(VSP) = 0 - RVSTK(VSP)
            TP0 = KZINT
         ELSE
            D = OP/2
            OP = MOD(OP,2)
555         IF (OP.EQ.VOP) THEN
C              IS A VECTOR
               I1 = VSTK(VSP) - 1
               IF (D.NE.0) I1 = I1 * 2
               L  = VSTK(VSP-1)
               VSP = VSP - 2
               IF (L.NE.0) THEN
                  CP = TP + VSTK(VSP) - 1 + I1
               ELSE
                  CP = TP + BUFFER(TP+VSTK(VSP)-1) + 1 + I1
               ENDIF
            ELSE
C              IS A MATRIX
               I1 = VSTK(VSP) - 1
               IF (D.NE.0) I1 = I1 * 2
               I2 = VSTK(VSP-1) - 1
               IF (D.NE.0) I2 = I2 * 2
               L  = VSTK(VSP-2)
               VSP = VSP - 3
               CALL ITOH(R,W,L)
               IF (W.NE.0) THEN
                  I1 = (I1)*(W/R) + I2
                  CP = TP + VSTK(VSP) - 1 + I1
               ELSE
                  CP = TP + BUFFER(TP+VSTK(VSP)-1)
                  W = BUFFER(CP)
                  IF (D.NE.0) W = W*2
                  CP = CP + 1 + I1*W + I2
               ENDIF
            ENDIF
556         IVAL(1) = BUFFER(CP)
            IVAL(2) = BUFFER(CP+1)
            VSTK(VSP) = IVAL(1)
            IF (TP1.EQ.KZREAL) CALL RTOD(IVAL,IVAL(1))
            RVSTK(VSP) = DVAL
            TP0 = KZINT
 
         ENDIF
 
         IF (TP0.EQ.KZINT .AND. TP1.EQ.KZINT) THEN
            VSTKTP(VSP) = KZINT
         ELSE
            VSTKTP(VSP) = KZDOUB
         ENDIF
      ELSE
C        VALUE
         VSP = VSP + 1
         IF (EXPVTP(V).EQ.0) THEN
C           ATTRIBUTE OR VAR
            IF (EXPVAL(V).GT.0) THEN
               IVAL(1) = BUFFER(TP+EXPVAL(V)-1)
               IVAL(2) = BUFFER(TP+EXPVAL(V)  )
            ENDIF
            IF (EXPVAL(V).LT.0) THEN
               IVAL(1) = BUFFER(0-EXPVAL(V))
               IVAL(2) = BUFFER(0-EXPVAL(V)+1)
            ENDIF
            VAL = IVAL(1)
            IF (EXPTP(V).EQ.KZREAL) CALL RTOD(IVAL,IVAL(1))
            RVAL = DVAL
         ELSE
C           VALUE
            VAL = EXPVAL(V)
            RVAL = EXPVRL(V)
         ENDIF
         IF (VAL.EQ.ZIMISS .OR. VAL.EQ.ZINAPP) GOTO 700
         VSTK(VSP) = VAL
         RVSTK(VSP) = RVAL
         VSTKTP(VSP) = EXPTP(V)
         IF (VSTKTP(VSP).EQ.KZDATE) VSTKTP(VSP) = KZINT
         IF (VSTKTP(VSP).EQ.KZTIME) VSTKTP(VSP) = KZINT
         IF (VSTKTP(VSP).EQ.KZREAL) VSTKTP(VSP) = KZDOUB
      ENDIF
200   CONTINUE
C
C     EVALUATION COMPLETE
C     PUT VAR INTO THE VAR BUFFER
C
 201  TP1 = VSTKTP(1)
      IF (TYP.EQ.KZINT) THEN
         VAL = VSTK(1)
         IF (TP1.NE.KZINT) VAL = RVSTK(1)
         BUFFER(VP+Z+2) = VAL
      ELSE
         DVAL = RVSTK(1)
         IF (TP1.EQ.KZINT) DVAL = VSTK(1)
         IF (TYP.EQ.KZREAL) THEN
            BUFFER(VP+Z+2) = DTOR(IVAL)
         ELSE
            BUFFER(VP+Z+2) = IVAL(1)
            BUFFER(VP+Z+3) = IVAL(2)
         ENDIF
      ENDIF
 209  RMSTAT = 0
      RETURN
 
C
C     TEXT DATA (PUT RESULT INTO VAR)
C
500   CALL ITOH(ATTCHA,ATTWDS,BUFFER(VP+Z))
      DO 502 J = 1, ATTWDS
502   BUFFER(VP+Z+1+J) = BLANK(1)
      CP = 0
      DO 600 J = 1, NEOP
      V = EOP(J)
C     ASSUME ONLY OP IS CONCATENATE
      IF (V.GT.10000) GOTO 600
      IF (EXPTP(V).NE.KZTEXT) THEN
C        COULD BE MISSING VALUE
         VAL = EXPVAL(V)
         IF (VAL.EQ.ZIMISS .OR. VAL.EQ.ZINAPP) GOTO 700
         GOTO 7000
      ENDIF
 
      IF (EXPVTP(V).EQ.0) THEN
         IF (EXPVAL(V).GT.0) THEN
C        ATTRIBUTE
           VAL = TP+EXPVAL(V)-1
           LEN = EXPLEN(V)
           IF (LEN.EQ.0) THEN
             VAL = TP+BUFFER(VAL)+1
             LEN = BUFFER(VAL-1)
           ENDIF
         ELSE
C        VARIABLE
           VAL = 0 - EXPVAL(V)
           L = BUFFER(VAL-2)
           CALL ITOH(LEN,W,L)
         ENDIF
      ELSE
C        VALUE
         VAL = EXPVAL(V)
         LEN = EXPLEN(V)
      ENDIF
 
      IF (CP+LEN.GT.ATTCHA) LEN = ATTCHA-CP
      IF (BUFFER(VAL).EQ.ZIMISS .OR. BUFFER(VAL).EQ.ZINAPP) THEN
         VAL = BUFFER(VAL)
         GOTO 700
      ENDIF
      CALL STRMOV(BUFFER(VAL),1,LEN,BUFFER(VP+Z+2),CP+1)
      CP = CP + LEN
600   CONTINUE
      RMSTAT = 0
      RETURN
 
C
C     MISSING VALUE FOUND
C
700   BUFFER(VP+Z+2) = VAL
      RMSTAT = 0
      RETURN
 
7000  CALL MSG('E','NON-TEXT IN TEXT EXPRESSION',' ')
      PGEEXP = .FALSE.
      RETURN
 
7100  CALL MSG('E','TEXT IN ARITHMETIC EXPRESSION',' ')
      PGEEXP = .FALSE.
      RETURN
 
      END
