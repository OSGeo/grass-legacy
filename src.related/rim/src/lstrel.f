      SUBROUTINE LSTREL(*)
      INCLUDE 'syspar.d'
C
C     SUMMARIZE THE USERS DEFINITION OF A RELATION
C
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'flags.d'
      INCLUDE 'tupler.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'files.d'
      INCLUDE 'tokens.d'
 
      INTEGER STATUS
      LOGICAL EQ
      LOGICAL NE
      LOGICAL EQKEYW
      LOGICAL ALLREL
      CHARACTER*4 KEY,CRPW,CMPW
      INTEGER FMTSTR(3)
      INCLUDE 'dclar1.d'
C
C
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C
      CALL RMDATE(TDAY)
      CALL RMTIME(TTIM)
      I = LOCREL(BLANK)
      NP = 0
      IF(I.NE.0) THEN
         CALL MSG('W','THERE ARE NO TABLES.',' ')
         GO TO 999
      ENDIF
C
      IF(ITEMS.GT.2) THEN
        CALL WARN(4,0,0)
        GOTO 999
      ENDIF
      IF(ITEMS.EQ.2) GO TO 1000
C
C   LISTREL (WITH NO RELATION SPECIFIED)
C
100   CALL RELGET(STATUS)
      IF(STATUS.NE.0) THEN
        IF (NP.EQ.0) CALL WARN(8,0,0)
        GOTO 999
      ENDIF
C
C     DONT LISTREL RULE RELATIONS
C
      IF(EQ(NAME,KARDT)) GO TO 100
      IF(EQ(NAME,KARRC)) GO TO 100
C
C   VALIDATE USER
C
      IF(EQ(USERID,OWNER)) GO TO 150
      IF(EQ(RPW,NONE)) GO TO 150
      IF(EQ(RPW,USERID)) GO TO 150
      IF(EQ(MPW,USERID)) GO TO 150
      GO TO 100
  150 CONTINUE
      IF(NP.EQ.0) THEN
C       WRITE OUT HEADER
        CALL MSG('R','  TABLE NAME      ' //
     X     '   ROWS' // '  LAST MODIFIED',' ')
        CALL MSG('R','  ----------------' //
     X     ' ------' // '  ------------ ',' ')
        NP = 1
      ENDIF
      CALL MSG('R','  ','+')
      CALL AMSG(NAME,ZC,'+')
      CALL IMSG(NTUPLE,7,'+')
      CALL MSG('R','  ','+')
      CALL DMSG(RDATE,0,' ',KZDATE)
      GO TO 100
C
C   LISTREL A SPECIFIC RELATION
C
1000  IF(EQKEYW(2,'*')) THEN
        ALLREL = .TRUE.
        CALL ZMOVE(RNAME,BLANK)
      ELSE
        ALLREL = .FALSE.
        CALL LXSREC(2,RNAME,ZC)
      ENDIF
      NREL = 0
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
C  REQUESTED RELATION DOES NOT EXIST
        IF (ALLREL) CALL MSG('E','THERE ARE NO TABLES.',' ')
        IF (.NOT.ALLREL) CALL WARN(1,RNAME,BLANK)
        GOTO 999
      ENDIF
C
1100  IF(ALLREL) THEN
        CALL RELGET(STATUS)
        IF((NREL.EQ.0).AND.(STATUS.NE.0)) THEN
          CALL WARN(8,0,0)
          GOTO 999
        ENDIF
        IF(STATUS.NE.0) GO TO 999
      ENDIF
C
C   CHECK PERMISSION
C
      IF(EQ(USERID,OWNER)) GO TO 1300
      IF(EQ(RPW,NONE)) GO TO 1300
      IF(EQ(RPW,USERID)) GO TO 1300
      IF(EQ(MPW,USERID)) GO TO 1300
      IF(ALLREL) GO TO 1100
      CALL WARN(8,0,0)
      GO TO 999
 1300 CONTINUE
C
C  PRINT HEADER.
C
      NREL = NREL + 1
      CRPW = 'NONE'
      CMPW = 'NONE'
      IF(NE(RPW,NONE)) CRPW = 'YES'
      IF(NE(MPW,NONE)) CMPW = 'YES'
C
      CALL MSG('R','      DATABASE : ','+')
        CALL AMSG(DBNAME,ZC,'+')
        CALL MSG(' ','   READ PASSWORD : ' // CRPW,'+')
        CALL MSG(' ','    LAST MOD : ','+')
        CALL DMSG(RDATE,0,' ',KZDATE)
      CALL MSG('R','         TABLE : ','+')
        CALL AMSG(NAME,ZC,'+')
        CALL MSG(' ',' MODIFY PASSWORD : ' // CMPW,' ')
      CALL MSG('R',' ',' ')
C
      CALL MSG('R','  NAME            ' // '  TYPE' //
     X   '      LENGTH  ' // '   FORMAT    ' // ' KEY',' ')
      CALL MSG('R','  ----------------' // '  ----' //
     X   ' -------------' // ' ----------- ' // ' ---',' ')
C
C  FIND AND PRINT ATTRIBUTE DESCRIPTIONS
C
      I = LOCATT(BLANK,NAME)
      IF(I.NE.0) THEN
        CALL MSG('R','NO COLUMNS',' ')
        GOTO 1800
      ENDIF
1500  CALL ATTGET(STATUS)
      IF(STATUS.NE.0) GO TO 1600
      KEY = ' '
      IF(ATTKEY.NE.0) KEY = 'YES'
C
C  PRINT ATTRIBUTE INFO
C
      CALL MSG('R','  ','+')
        CALL AMSG(ATTNAM,ZC,'+')
        CALL MSG(' ','  ' //  RMTYPT(ATTYPE),'+')
 
      NCHAR = ATTCHA
      NWORDS = ATTWDS
      IF(ATTYPE.EQ.KZDOUB) NWORDS = NWORDS / 2
      IF(ATTYPE.EQ.KZDVEC) NWORDS = NWORDS / 2
      IF(ATTYPE.EQ.KZDMAT) NWORDS = NWORDS / 2
 
      IF(ATTYPE.EQ.KZTEXT) THEN
        IF (NCHAR.EQ.0) THEN
           CALL MSG(' ','      VAR      ','+')
        ELSE
           CALL IMSG(NCHAR,5,'+')
           CALL MSG(' ',' CHARS    ','+')
        ENDIF
        GO TO 1590
      ENDIF
C
      IF(ATTYPE.EQ.KZIMAT) GO TO 1520
      IF(ATTYPE.EQ.KZRMAT) GO TO 1520
      IF(ATTYPE.EQ.KZDMAT) GO TO 1520
        IF (NWORDS.EQ.0) THEN
           CALL MSG(' ','      VAR      ','+')
        ELSE
           CALL IMSG(NWORDS,5,'+')
           CALL MSG(' ',' WORDS    ','+')
        ENDIF
      GO TO 1590
 
 1520 CONTINUE
      IF(NWORDS.NE.0) THEN
        NC = NWORDS / NCHAR
        CALL IMSG(NCHAR,5,'+')
        CALL MSG(' ',' BY ','+')
        CALL IMSG(NC,5,'+')
        CALL MSG(' ',' ','+')
        GO TO 1590
      ENDIF
C
      IF(NCHAR.NE.0) THEN
        NC = NWORDS / NCHAR
        CALL IMSG(NCHAR,5,'+')
        CALL MSG(' ',' BY VAR   ','+')
        GO TO 1590
      ENDIF
C
        CALL MSG(' ',' VAR BY VAR    ','+')
C
C     FORMAT
C
1590  IF (ATTFOR.NE.0) THEN
         CALL FMTDEC(ATTFOR,ATTYPE,FMTSTR,12)
         CALL AMSG(FMTSTR,12,'+')
      ELSE
         CALL MSG(' ','            ','+')
      ENDIF
C
C     COMPLETE THE LINE
      CALL MSG(' ',' ' // KEY,' ')
      GOTO 1500
 
C
C  FIND AND PRINT LINKS
C
1600  I = LOCLNK(BLANK)
      NLINK = 0
      CALL MSG(' ',' ',' ')
1610  CALL LNKGET(STATUS)
      IF(STATUS.NE.0) GO TO 1690
      IF (NE(NAME,R1NAME) .AND. NE(NAME,R2NAME)) GOTO 1610
C
      NLINK = NLINK + 1
      CALL MSG('R','  LINK ','+')
        CALL AMSG(LNAME,-ZC,'+')
        CALL MSG(' ',' FROM ','+')
        CALL AMSG(R1NAME,-ZC,'+')
        CALL MSG(' ','(','+')
        CALL AMSG(A1NAME,-ZC,'+')
        CALL MSG(' ',') TO ','+')
        CALL AMSG(R2NAME,-ZC,'+')
        CALL MSG(' ','(','+')
        CALL AMSG(A2NAME,-ZC,'+')
        CALL MSG(' ',')',' ')
      GOTO 1610
 
1690  IF(NLINK.EQ.0) CALL MSG('R','  NO LINKS',' ')
 
1700  CALL MSG('R',' ',' ')
      CALL MSG('R','    CURRENT NUMBER OF ROWS = ','+')
      CALL IMSG(NTUPLE,5,' ')
      CALL MSG('R',' ',' ')
1800  IF(ALLREL) GO TO 1100
      GO TO 999
C
C  ALL DONE.
C
 999  RETURN  1
      END
