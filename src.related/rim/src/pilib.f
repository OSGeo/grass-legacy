      FUNCTION RIM(IND,COM)
      INCLUDE 'syspar.d'
C
C     RIM FORTRAN INTERFACE (COMMANDS)
C
C         IND  = MULTIPLE RELATION INDEX (0-ZPIMAX)
C         COM  = TEXT OF COMMAND
C         RIM  = RETURN STATUS (TRUE IF OK)
C
      LOGICAL RIM
      CHARACTER*(*) COM
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'cards.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'flags.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'files.d'
C
      LOGICAL EQKEYW
C
      DATA INITFL /0/
C
C     -----------------------------------------------------
C
      RMSTAT = 0
C
C     CHECK FOR INITIALIZE
C
      IF (INITFL.EQ.0) THEN
         KDBVER = 1200
         CALL RMCONS
         PIFLAG = .TRUE.
         BATCH = .FALSE.
         CONNI = .FALSE.
         CONNO = .FALSE.
         CALL RMINIT
         CALL LXINIT
         INITFL = 1
      ENDIF
C
C     CHECK INDEX AND SET POINTERS
C
      IF (IND.LT.1 .OR. IND.GT.ZPIMAX) THEN
         RMSTAT = 70
         GOTO 900
      ENDIF
      MRINDX = 0
C
C     SAVE CURRENT POINTERS
      IF (INDCUR.NE.IND) CALL RMSAV(INDCUR)
      IF (RMSTAT.NE.0) GOTO 900
      INDCUR = IND
      MRINDX = IND
C
C
C  CONVERT INPUT TO ASCII-TEXT
C
      CALL ASCTXT(CRDREC,ZCARDL,COM)
      CRDEND = MIN(LEN(COM),ZCARDL)
      CRDPTR = 0
      READCD = -1
C
      CALL LODREC
      IF (ITEMS.LT.1) RETURN
C
C     GET THE COMMAND
C
      IF (.NOT.TOKTYP(1,KXKEYW)) GOTO 800
C
C---- QUERY COMMANDS
C
C     IF (EQKEYW(1,'LISTREL')) CALL LSTREL(*900)
C.....IF (EQKEYW(1,'EXHIBIT')) CALL XHIBIT(*900)
C
      IF (EQKEYW(1,'SELECT'))  CALL RMQERY(*900,'SELECT')
C
C---- MODIFICATION COMMANDS
C
      IF (EQKEYW(1,'BUILD'))   CALL BUILD(*900)
      IF (EQKEYW(1,'CHANGE'))  THEN
         IF (EQKEYW(2,'OWNER'))     CALL CHGPSW(*900)
         IF (EQKEYW(2,'RPW'))       CALL CHGPSW(*900)
         IF (EQKEYW(2,'MPW'))       CALL CHGPSW(*900)
                                    CALL CHGDAT(*900)
      ENDIF
      IF (EQKEYW(1,'DELETE'))  THEN
         IF (EQKEYW(2,'ROWS'))      CALL DELROW(*900)
CCCCC    IF (EQKEYW(2,'DUPLICATES'))CALL DELDUP(*900)
      ENDIF
C     IF (EQKEYW(1,'RENAME')) THEN
C        IF (EQKEYW(2,'RELATION'))  CALL RNAMER(*900)
C        IF (EQKEYW(2,'TABLE'))     CALL RNAMER(*900)
C        IF (EQKEYW(2,'LINK'))      CALL RNAMEL(*900)
C                                   CALL RNAMEA(*900)
C     ENDIF
      IF (EQKEYW(1,'REMOVE'))  THEN
         IF (EQKEYW(2,'KEY'))       CALL REMKEY(*900)
C        IF (EQKEYW(2,'LINK'))      CALL REMLNK(*900)
C                                   CALL REMREL(*900)
      ENDIF
C
      IF (EQKEYW(1,'LOAD'))    CALL DBLOAD(*900)
C
C---- DATABASE IDENTIFICATION
C
      IF (EQKEYW(1,'OPEN'))    CALL DBOPCL(*900,'OPEN')
      IF (EQKEYW(1,'CLOSE'))   CALL DBOPCL(*900,'CLOSE')
C
C---- SCHEMA MODIFICATION
C
C.... IF (EQKEYW(1,'DEFINE'))  THEN
C                                 CALL CSC(*900)
C     ENDIF
C
C---- RELATION ALGEBRA
C
C     These have been removed because they are very rarely
C     used by programs.  Restore them if you need them.
C
C     IF (EQKEYW(1,'INTERSECT'))  CALL TUPLRC('INTERSECT',*900)
C     IF (EQKEYW(1,'UNION'))      CALL TUPLRC('UNION',*900)
C     IF (EQKEYW(1,'JOIN'))       CALL JOIREL(*900)
C     IF (EQKEYW(1,'SUBTRACT'))   CALL TUPLRC('SUBTRACT',*900)
C     IF (EQKEYW(1,'PROJECT'))    CALL PJECT(*900)
C
C---- COMMANDS THAT DO NOT AFFECT A DATABASE
C
      IF (EQKEYW(1,'MACRO'))   CALL MACDEF(*900)
C     IF (EQKEYW(1,'HELP'))    CALL RMHELP(*900)
C     IF (EQKEYW(1,'SHOW'))    CALL RMSHOW(*900)
      IF (EQKEYW(1,'SET'))     CALL RMSET(*900)
      IF (EQKEYW(1,'USER'))    CALL RMSET(*900)
      IF (EQKEYW(1,'INIT'))    THEN
        CALL RMINIT
        GOTO 900
      ENDIF
C
C     UNRECOGNISED COMMAND - POSSIBLY SYSTEM DEPENDENT
C
      CALL SYSCOM(*900)
C
800   RMSTAT = 4
      GOTO 900
C
900   IF (RMSTAT.EQ.0) THEN
         RIM = .TRUE.
      ELSE
         RIM = .FALSE.
      ENDIF
      RETURN
      END
      FUNCTION RIMDM(IND,COM,TUPLE)
      INCLUDE 'syspar.d'
C
C     RIM FORTRAN INTERFACE (DATA MOVEMENT)
C
C         IND  = MULTIPLE RELATION INDEX (0-ZPIMAX)
C         COM  = COMMAND (1ST CHAR ONLY)
C                'G' = GET
C                'P' = PUT
C                'L' = LOAD
C                'D' = DELETE
C         RIMDM= STATUS (TRUE IF OK)
C
      LOGICAL RIMDM
      CHARACTER*(*) COM
      INTEGER TUPLE(1)
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'tupler.d'
      INCLUDE 'flags.d'
C
      CHARACTER*1 OP
C
C     -----------------------------------------------------
C
      RMSTAT = 0
C
C     CHECK THE COMMAND
C
      OP = COM(1:1)
      IF (OP.NE.'G' .AND. OP.NE.'P' .AND.
     1    OP.NE.'L' .AND. OP.NE.'D') THEN
         RMSTAT = 4
         GOTO 900
      ENDIF
C
C     MAKE SURE DB IS DEFINED
C
      IF(.NOT.DFLAG) THEN
        RMSTAT = 16
        GOTO 900
      ENDIF
C
C     MAKE SURE THE DB MAY BE MODIFIED
C
      IF(OP.NE.'G' .AND. .NOT.DMFLAG) THEN
        RMSTAT = 13
        GOTO 900
      ENDIF
C
C  RESTORE THE BLOCKS AS NEEDED.
C
      CALL RMRES(IND)
      IF(RMSTAT.NE.0) GO TO 900
C
C  SET THE INDEX POINTERS
C
      MRINDX = IND
      INDEX = IND
      IF(INDEX.EQ.0) INDEX = 1
      IF(INDEX.GT.3) INDEX = 3
C
C  CHECK FOR WRITE PERMISSION ON CURRENT RELATION.
C
      IF (OP.NE.'G') THEN
         I = LOCPRM(NAME,2)
         IF(RMSTAT.NE.0) GO TO 900
      ENDIF
C
C     CALL APPROPRIATE ROUTINE
C
      IF (OP.EQ.'G') CALL RMXGET(IND,TUPLE)
      IF (OP.EQ.'P') CALL RMXPUT(IND,TUPLE)
      IF (OP.EQ.'D') CALL RMXDEL(IND)
      IF (OP.EQ.'L') CALL RMXLOD(IND,TUPLE)
C
900   IF (RMSTAT.EQ.0) THEN
         RIMDM = .TRUE.
      ELSE
         RIMDM = .FALSE.
      ENDIF
      RETURN
      END
      SUBROUTINE TXTASC(STR,ASC,NC)
      INCLUDE 'syspar.d'
C
C     RETURN THE CHARACTER EQUIVALENT OF ASC (ASCII-TEXT, LENGTH NC)
C     (LIKE STRASC BUT WITHOUT UPPER CASE TRANSLATION)
C
      CHARACTER*(*) STR
      CHARACTER*1 CHRASC
C
      STR = ' '
      DO 100 I = 1, MIN(NC,LEN(STR))
      CALL GETT(ASC,I,CH)
100   STR(I:I) = CHRASC(CH)
      RETURN
      END
