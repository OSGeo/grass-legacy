      SUBROUTINE UNDEF(ALL,IRCNTR,IDAY,MODE,LHASH,ATREL)
      INCLUDE 'syspar.d'
C
C     UNLOAD THE SCHEMA OF A DATABASE.
C
C  INPUTS:
C          ALL------------TRUE IF ALL RELATIONS ARE SPECIFIED.
C          IRCNTR---------NUMBER OF RELATIONS IF SPECIFIED (ALL IS FALSE
C          IDAY-----------DAY CODE FOR HASH.
C          MODE -----------COMMAND SPECIFIED.
C
      INCLUDE 'rmatts.d'
      INCLUDE 'ascpar.d'
      INCLUDE 'files.d'
      INCLUDE 'buffer.d'
      INCLUDE 'flags.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tupler.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'lxlcom.d'
      INCLUDE 'msgcom.d'
      INCLUDE 'dclar1.d'
      INCLUDE 'dclar3.d'
      LOGICAL EQ, NE
      LOGICAL ALL,PERM,LHASH
C
C
      CHARACTER*(*) MODE
      INTEGER IREL(Z,1),ATREL(Z,1)
      EQUIVALENCE (BUFFER(1),IREL(1,1))
      INTEGER FMTSTR(3)
C
C
C     MAX CHARACTERS PER LINE FOR UNLOAD
C
      PARAMETER (UMCPL=80)
C
C
      IACNTR = 0
C
1     CALL MSG('R','DEFINE ','+')
      CALL AMSG(DBNAME,ZC,' ')
C
CC    NO UNLOADING OF PASSWORDS HERE
CC
CC    CALL MSG('R','OWNER ','+')
CCC   IF (LHASH) THEN
CCC      CALL HASHIN (USERID,1,IREL(1,1),1)
CCC      CALL AMSG(IREL(1,1),ZC,' ')
CCC   ELSE
CC       CALL AMSG(USERID,ZC,' ')
CCC   ENDIF
C
C
C  PROCESS ATTRIBUTES
C
      CALL MSG('R','ATTRIBUTES',' ')
      I = 0
      IF (IRCNTR .EQ. ALL9S) IRCNTR = 0
      J = LOCREL(BLANK)
C
5     IF (ALL) GO TO 7
      I = I + 1
      IF (I .GT. IRCNTR) GO TO 50
      K = LOCATT (BLANK,IREL(1,I))
      GO TO 10
C
7     CALL CHKREL(PERM,MODE,ISTAT,USERID)
      IF (ISTAT .NE. 0) GO TO 50
      IF (.NOT. PERM)  GO TO 7
      IRCNTR = IRCNTR + 1
      K = LOCATT (BLANK,NAME)
C
10    CALL ATTGET (ISTAT)
      IF (ISTAT .NE. 0) GO TO 5
      IF (IACNTR .EQ. 0) GO TO 20
      DO 15 L = 1,IACNTR
      IF (EQ(ATTNAM,ATREL(1,L))) GO TO 10
   15 CONTINUE
C
C  NEW ATTRIBUTE
C
20    IACNTR = IACNTR + 1
      CALL ZMOVE(ATREL(1,IACNTR),ATTNAM)
      CALL TYPER (ATTYPE,STRUC,TYPE)
C
      CALL MSG('R',' ','+')
      CALL AMSG(ATTNAM,-ZC,'+')
      CALL MSG(' ',' ' // RMTYPT(ATTYPE) // ' ','+')
C
      I = ATTWDS
      IF (STRUC.NE.KZMAT) THEN
         IF (TYPE.EQ.KZTEXT) I = ATTCHA
         IF (TYPE.EQ.KZDOUB) I = I/2
         IF (I.GT.0) THEN
            CALL IMSG(I,-10,'+')
         ELSE
            CALL MSG(' ',' VAR','+')
         ENDIF
      ELSE
         IF (TYPE .EQ. KZDOUB) I = I/2
         IF (I.NE.0)  I = I/ATTCHA
         IF (ATTCHA.NE.0) THEN
            CALL IMSG(ATTCHA,-10,'+')
            IF (I.NE.0) THEN
               CALL MSG(' ',' ','+')
               CALL IMSG(I,-10,'+')
            ELSE
               CALL MSG(' ',' VAR','+')
            ENDIF
         ELSE
            CALL MSG(' ','VAR VAR','+')
         ENDIF
      ENDIF
C
      IF (ATTFOR.NE.0) THEN
         CALL MSG(' ',' FORMAT ''','+')
         CALL FMTDEC(ATTFOR,ATTYPE,FMTSTR,12)
         CALL AMSG(FMTSTR,-12,'+')
         CALL MSG(' ','''',' ')
      ELSE
         CALL MSG(' ',' ',' ')
      ENDIF
      GO TO 10
C
C
50    IF (IRCNTR .EQ. 0) GO TO 400
      J = LOCREL(BLANK)
C
C  LOOP THROUGH AND PRINT THE RELATIONS WITH THEIR ATTRIBUTES
C
      CALL MSG('R','RELATIONS',' ')
      DO 150 I = 1,IRCNTR
      IF (ALL) GO TO 90
      CALL ZMOVE(RNAME,IREL(1,I))
      J = LOCREL (RNAME)
      CALL RELGET (ISTAT)
      GO TO 95
C
90    CALL CHKREL (PERM,MODE,ISTAT,USERID)
      IF (ISTAT .NE. 0) GO TO 150
      IF (.NOT. PERM) GO TO 90
      CALL ZMOVE(RNAME,NAME)
C
95    CALL MSG('R',' ','+')
      CALL AMSG(RNAME,-ZC,'+')
      CALL MSG(' ',' WITH','+')
C
      J = LOCATT (BLANK,RNAME)
100   CALL ATTGET (ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL MSG(' ',' ',' ')
         GOTO 150
      ELSE
140      IF (MSGPTR+ZC.GT.UMCPL-2) THEN
            CALL MSG(' ','+',' ')
            CALL MSG('R','  ','+')
         ELSE
            CALL MSG(' ',' ','+')
         ENDIF
         CALL AMSG(ATTNAM,-ZC,'+')
      ENDIF
      GOTO 100
150   CONTINUE
C
C  PRINT LINKS ONLY IF DUMPING ALL RELATIONS
C
      IF (ALL) THEN
        J = LOCLNK(BLANK)
        IF (J.NE.0) GOTO 220
        CALL MSG('R','LINKS',' ')
 
210     CALL LNKGET(STATUS)
        IF(STATUS.NE.0) GO TO 220
        CALL MSG('R',' ','+')
        CALL AMSG(LNAME,-ZC,'+')
        CALL MSG(' ',' FROM ','+')
        CALL AMSG(A1NAME,-ZC,'+')
        CALL MSG(' ',' IN ','+')
        CALL AMSG(R1NAME,-ZC,'+')
        CALL MSG(' ',' TO ','+')
        CALL AMSG(A2NAME,-ZC,'+')
        CALL MSG(' ',' IN ','+')
        CALL AMSG(R2NAME,-ZC,' ')
        GOTO 210
      ENDIF
C
C  PRINT PASSWORDS 
C
220   CONTINUE
CC    CALL MSG('R','PASSWORDS',' ')
CC    J = LOCREL (BLANK)
CC    DO 300 I = 1,IRCNTR
CC    IF (ALL) GO TO 225
CC    J = LOCREL (IREL(1,I))
CC    CALL ZMOVE(RNAME,IREL(1,I))
CC    GO TO 240
C
C225   CALL CHKREL (PERM,MODE,ISTAT,USERID)
C      IF (.NOT. PERM) GO TO 225
C      CALL ZMOVE(RNAME,NAME)
C
C240   NUM = 31
C      IF (LHASH) NUM = 39
C      CALL ZMOVE(RPW1,RPW)
C      DO 250 J = 1,2
C      IF (NE(RPW1,NONE)) THEN
C         CALL MSG('R','RPW FOR ','+')
C         CALL AMSG(RNAME,-ZC,'+')
C         CALL MSG(' ','IS ''','+')
CCCCCCCCC IF (LHASH) CALL HASHIN (RPW1,IDAY,LINE,23)
C         IF (.NOT. LHASH) CALL AMSG(RPW1,-ZC,' ')
C      ENDIF
CC     RPW1 = MPW
CCC    CALL PUTT (LINE,2,K4M)
C250   CONTINUE
CC
C300   CONTINUE
C
C
400   CONTINUE
      CALL MSG('R','END',' ')
      RETURN
      END
