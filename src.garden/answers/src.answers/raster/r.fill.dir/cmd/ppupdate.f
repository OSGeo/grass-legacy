      IMPLICIT INTEGER*2(A-Z)
      CHARACTER*160 BAFILE
      CHARACTER*160 ELFILE, tfile
      CHARACTER*6 SYSTEM
      INTEGER*2 LEVEL(4002,3),STACK(2000),MAPTO(2,2000)
      INTEGER*2 MASK(4002,3),ALLPPTBL(8,2000)
      EQUIVALENCE(MASK(1,1),STACK(1)),(MASK(1,2),MAPTO(1,1))
      COMMON MASK,LEVEL
      COMMON /TBL/ALLPPTBL
C-- ALLOW 5000 POLYS, NS OF 20000
      LUNIT=7
      MUNIT=9
      PRINT *,'ENTER NL,NS,BASIN FILE, ELEV FILE,SYSTEM(UNIX,VMS,PRIME)'
      read(*,'(a)') tfile
      OPEN(UNIT=10,STATUS='OLD',file=tfile)
      READ(10,*)NL,NS
      READ(10,'(a)')BAFILE
      READ(10,'(a)')ELFILE
      READ(10,'(a)')SYSTEM
C  The file of basin polygons....
C  The file of elevation data....
      IF (SYSTEM.EQ.'VMS') GOTO 700
      IF (SYSTEM.EQ.'PRIME') GOTO 710
C
C     This section is for opening files under UNIX
C
      OPEN(UNIT=MUNIT,STATUS='OLD',RECL=NS*2,
     *   FORM='UNFORMATTED',ACCESS='DIRECT',
     *   file=BAFILE)
      OPEN(UNIT=LUNIT,STATUS='OLD',FORM='UNFORMATTED',
     *   ACCESS='DIRECT',file=ELFILE,RECL=NS*2)
      GOTO 13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This section is for opening files under VMS
C
 700  CONTINUE
      OPEN(UNIT=MUNIT,STATUS='OLD',RECL=(NS+1)/2,
     *   FORM='UNFORMATTED',ACCESS='DIRECT',
     *   file=BAFILE)
      OPEN(UNIT=LUNIT,STATUS='OLD',FORM='UNFORMATTED',
     *   ACCESS='DIRECT',file=ELFILE,RECL=(NS+1)/2)
      GOTO 13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This section is for opening files under PRIMOS
C
 710  CONTINUE
      OPEN(UNIT=MUNIT,STATUS='OLD',RECL=NS,
     *   FORM='UNFORMATTED',ACCESS='DIRECT',
     *   file=BAFILE)
      OPEN(UNIT=LUNIT,STATUS='OLD',FORM='UNFORMATTED',
     *   ACCESS='DIRECT',file=ELFILE,RECL=NS)
      GOTO 13
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 13   I1=1
      I2=2
      I3=3
      MAXB=0
      DO 10 I=1,NS+2
      MASK(I,I1)=0
 10   LEVEL(I,I1)=0
      DO 15 I=2,3
      MASK(NS+2,I)=0
 15   LEVEL(NS+2,I)=0
      DO 20 I=1,2000
 20   ALLPPTBL(1,I)=-1
      READ(MUNIT,REC=1)(MASK(I,I2),I=2,NS+1)
      DO 2000 LINE=1,NL
      IF(LINE.EQ.NL)GOTO 30
      READ(MUNIT,REC=LINE+1)(MASK(I,I3),I=2,NS+1)
      READ(LUNIT,REC=LINE+1)(LEVEL(I,I3),I=2,NS+1)
      GOTO 40
 30   DO 35 I=2,NS+1
      MASK(I,I3)=0
 35   LEVEL(I,I3)=0
 40   DO 500 SAMP=2,NS+1
      INDEX=MASK(SAMP,I2)
      IF(INDEX.EQ.0)GOTO 500
       CALL DO3BY3(
     *  SAMP,LINE,I1,I2,I3,
     *  INDEX)
 500  CONTINUE
      ITEMP=I1
      I1=I2
      I2=I3
      I3=ITEMP
 2000 CONTINUE
 3060 CALL CONNECT(NL,NS,
     *   LUNIT,MUNIT)
      STOP
      END
c
c
      SUBROUTINE DO3BY3(WKSAMP,
     *  LINE,I1,I2,I3,INDEX)
      IMPLICIT INTEGER*2(A-Z)
      DIMENSION M(9),E(9),LINC(9),SINC(9)
      INTEGER*2 MASK(4002,3),ELEV(4002,3),ALLPPTBL(8,2000)
      LOGICAL ELOG
      COMMON MASK,ELEV
      COMMON /TBL/ALLPPTBL
      DATA LINC/-1,-1,-1,0,0,0,1,1,1/
      DATA SINC/-2,-1,0,-2,-1,0,-2,-1,0/
      DO 10 I=1,3
      M(I)=MASK(WKSAMP+I-2,I1)
      E(I)=ELEV(WKSAMP+I-2,I1)
      M(I+3)=MASK(WKSAMP+I-2,I2)
      E(I+3)=ELEV(WKSAMP+I-2,I2)
      M(I+6)=MASK(WKSAMP+I-2,I3)
      E(I+6)=ELEV(WKSAMP+I-2,I3)
 10   CONTINUE
C-- M AND E INDEXING...
C      1  2  3
C      4  5  6
C      7  8  9
C
C-- CHECK FOR BEING ON EDGE OF POLYGON
      ELOG=.FALSE.
      DO 30 I=1,9
 30   IF(ABS(M(I)).NE.ABS(M(5)))ELOG=.TRUE.
      IF(.NOT.ELOG)GOTO 2000
C-- IS ON EDGE OF POLYGON..
      DO 35 I=1,9
 35   CALL ALLPP(M(I),M(5),E(I),E(5),LINE+LINC(I),
     *   WKSAMP+SINC(I),LINE,WKSAMP-1)
 2000 CONTINUE
      RETURN
      END
C
      SUBROUTINE ALLPP(M1,M2,E1,E2,L1,S1,L2,S2)
      IMPLICIT INTEGER*2 (A-Z)
      INTEGER*2 ALLPPTBL(8,2000)
      COMMON /TBL/ALLPPTBL
      IF(M1.EQ.M2)GOTO 2000
      BIGM=MAX(M1,M2)
      LITTLEM=MIN(M1,M2)
      DO 50 I=1,5000
      IF(ALLPPTBL(1,I).EQ.-1)GOTO 30
      IF(ALLPPTBL(1,I).NE.LITTLEM.OR.ALLPPTBL(2,I).NE.
     *   BIGM)GOTO 49
C-- FOUND IT ,UPDATE?
      IF(MAX(E1,E2).GE.MAX(ALLPPTBL(3,I),ALLPPTBL(4,I)))
     *   GOTO 2000
C-- YES, UPDATE
      GOTO 45
C-- ADD IT
 30   ALLPPTBL(1,I)=LITTLEM
      ALLPPTBL(2,I)=BIGM
 45   ALLPPTBL(3,I)=E1
      ALLPPTBL(4,I)=E2
      ALLPPTBL(5,I)=L1
      ALLPPTBL(6,I)=S1
      ALLPPTBL(7,I)=L2
      ALLPPTBL(8,I)=S2
      IF(M1.EQ.LITTLEM)GOTO 2000
      ALLPPTBL(3,I)=E2
      ALLPPTBL(4,I)=E1
      ALLPPTBL(5,I)=L2
      ALLPPTBL(6,I)=S2
      ALLPPTBL(7,I)=L1
      ALLPPTBL(8,I)=S1
      GOTO 2000
 49   IF(I.EQ.5000)STOP 72
 50   CONTINUE
 2000 CONTINUE
      RETURN
      END
      SUBROUTINE CONNECT(NL,NS,
     *   LUNIT,MUNIT)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION ALLPPTBL(8,2000),STACK(2000),
     *   MAPTO(2,2000),LEVEL(4002,3),MASK(4002,3)
      LOGICAL DONE
      EQUIVALENCE(MASK(1,1),STACK(1)),(MASK(1,2),MAPTO(1,1))
      COMMON MASK,LEVEL
      COMMON /TBL/ALLPPTBL
C  When element one of the table is -1 it is eof.
C  When element one of the table is -2 , the pp has been deleted.
C  Table element five is used to flag lowest pp's.  0=not lowest.
C  1=lowest.In the case of a tie, only one is chosen.
C
C  INIT MAPTO
      MAPTONUM=0
      CALL LOWPP(ALLPPTOT)
C  Trace a path through the low pp's.
      DONE =.FALSE.
  10  CALL PATH(DONE,NUM,LOOP,ALLPPTOT)
      IF(DONE)GOTO 1000
      IF(LOOP.EQ.0)GOTO 1100
      CALL FIXALOOP(LOOP,NUM
     *   ,ALLPPTOT,MAPTONUM)
      GOTO 10
 1100 CALL UPDATE(ALLPPTOT,NUM)
      GOTO 10
 1000 CONTINUE
      CALL FILL(ALLPPTOT,MAPTONUM,LUNIT,MUNIT,
     *   NL,NS)
      RETURN
      END
C
      SUBROUTINE LOWPP(ALLPPTOT)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION ALLPPTBL(8,2000)
      COMMON /TBL/ALLPPTBL
C  When a lowest pp is found, put "from" as element 1 and "to" as
C  element 2.  If the pp is lowest for both, duplicate it.
C
C  Move the max of elements 3 and 4 to element 3.  Initialize element 5
C  to 0, indicating it is not yet a lowpp.
      DO 10 I=1,2000
      IF(ALLPPTBL(1,I).EQ.-1)GOTO 20
      ALLPPTBL(3,I)=MAX(ALLPPTBL(3,I),ALLPPTBL(4,I))
 10   ALLPPTBL(5,I)=0
 20   ALLPPTOT=I-1
      DO 30 I=1,ALLPPTOT
      DO 35 J=1,2
      TRY=ALLPPTBL(J,I)
      IF(TRY.EQ.0)GOTO 35
      SMALLEST=0
      DO 40 K=1,ALLPPTOT
C  If a lowpp has already been found for this basin, go on.
      IF(ALLPPTBL(1,K).EQ.TRY.AND.
     *   ALLPPTBL(5,K).EQ.1)GOTO 35
      IF(ALLPPTBL(1,K).NE.TRY.AND.ALLPPTBL(2,K).NE.TRY)GOTO 40
      IF(SMALLEST.NE.0.AND.ALLPPTBL(3,K).GE.SMALLEST)GOTO 40
      SMALLEST=ALLPPTBL(3,K)
      SINDEX=K
 40   CONTINUE
      IF(ALLPPTBL(5,SINDEX).EQ.1)GOTO 50
      ALLPPTBL(5,SINDEX)=1
      IF(ALLPPTBL(1,SINDEX).EQ.TRY)GOTO 35
C  need to reverse the order
      ALLPPTBL(2,SINDEX)=ALLPPTBL(1,SINDEX)
      ALLPPTBL(1,SINDEX)=TRY
      GOTO 35
C  already a lowest
  50  ALLPPTOT=ALLPPTOT+1
      ALLPPTBL(1,ALLPPTOT)=TRY
      ALLPPTBL(2,ALLPPTOT)=ALLPPTBL(1,SINDEX)
      ALLPPTBL(3,ALLPPTOT)=SMALLEST
      ALLPPTBL(5,ALLPPTOT)=1
 35   CONTINUE
 30   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PATH(DONE,NUM,LOOP,ALLPPTOT)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION ALLPPTBL(8,2000),STACK(500)
      logical done
      INTEGER*2 MASK(4002,3),LEVEL(4002,3)
      EQUIVALENCE(MASK(1,1),STACK(1))
      COMMON MASK,LEVEL
      COMMON /TBL/ALLPPTBL
      LOOP = 0
C  Build the list of indexes to basin labels in STACK
C
C  Find first one
      DO 10 I=1,ALLPPTOT
C  deleted
      IF(ALLPPTBL(1,I).EQ.-2)GOTO 10
C  not lowest
      IF(ALLPPTBL(5,I).EQ.0)GOTO 10
C  does it already go directly to the mask?
      IF(ALLPPTBL(2,I).EQ.0)GOTO 10
C  found first one
      GOTO 20
 10   CONTINUE
      GOTO 1000
 20   NUM=1
      STACK(1)=I
C  Build stack.
 25   FROM=ALLPPTBL(2,I)
C  find next entry and add it
      DO 30 I=1,ALLPPTOT
C  not lowest
      IF(ALLPPTBL(5,I).NE.1)GOTO 30
C  not a match
      IF(ALLPPTBL(1,I).NE.FROM)GOTO 30
C  found it
      NUM=NUM+1
      STACK(NUM)=I
C  If it is 0, we are done.
      IF(ALLPPTBL(2,I).EQ.0)GOTO 2000
C  Does this basin already occur in the list?
      DO 35 J=1,NUM-1
      IF(STACK(J).NE.STACK(NUM))GOTO 35
C  Yes, there is a loop
      LOOP=J
      GOTO 2000
 35   CONTINUE
C  No loop yet...
      GOTO 25
 30   CONTINUE
 1000 DONE=.TRUE.
 2000 CONTINUE
      RETURN
      END
C
      SUBROUTINE FIXALOOP(LOOP,STACKTOT,
     *   ALLPPTOT,MAPTONUM)
      IMPLICIT INTEGER*2(A-Z)
      DIMENSION ALLPPTBL(8,2000),MEMBERS(500),STACK(500),
     *   MAPTO(2,2000)
      INTEGER*2 MASK(4002,3),LEVEL(4002,3)
      EQUIVALENCE (MASK(1,1),STACK(1)),(MASK(1,2),MAPTO(1,1))
      COMMON MASK,LEVEL
      COMMON /TBL/ALLPPTBL
C  There is a loop in the current path being processed.
C               Stack
C            ___________
C                .
C                .
C               (A)   () denotes "address of"
C     Start     (B)
C               (C)
C     STACKTOT  (A)
C
C  NOTE: THE VALUE OF LOOP IS CHANGED IN THIS ROUTINE
C  In essence, dissolve B & C into A.
      START=LOOP+1
      LOOP=STACKTOT
      NUM=LOOP-START+1
      LOOPEND = ALLPPTBL(1,STACK(LOOP))
      DO 25 I=1,NUM
 25   MEMBERS(I)=ALLPPTBL(1,STACK(START-1+I))
C  There are NUM members in the loop and their actual labels
C  are kept in MEMBERS.
C   Update MAPTO
      CALL DOMAPTO(MAPTONUM,MEMBERS,NUM,LOOPEND)
C
C  Delete all pp's that go from one loop member to another
      DO 30 I=1,ALLPPTOT
C  already deleted
      IF(ALLPPTBL(1,I).EQ.-2)GOTO 30
C  check first element
      DO 35 J=1,NUM
      IF(ALLPPTBL(1,I).EQ.MEMBERS(J))GOTO 36
 35   CONTINUE
      GOTO 30
C  check second element
 36   DO 37 J=1,NUM
      IF(ALLPPTBL(2,I).EQ.MEMBERS(J))GOTO 38
 37   CONTINUE
      GOTO 30
C  delete it
 38   ALLPPTBL(1,I)=-2
 30   CONTINUE
 39   CONTINUE
C  All pp's that go from some X to a dissolving loop member
C  should now be redirected to LOOPEND.
      DO 40 I=1,ALLPPTOT
C  deleted
      IF(ALLPPTBL(1,I).EQ.-2)GOTO 40
C  check for loop members
      DO 42 J=1,NUM-1
      IF(ALLPPTBL(1,I).EQ.MEMBERS(J))
     *   ALLPPTBL(1,I)=LOOPEND
      IF(ALLPPTBL(2,I).EQ.MEMBERS(J))
     *   ALLPPTBL(2,I)=LOOPEND
 42   CONTINUE
 40   CONTINUE
C  Find a new lowpp for loopend
      SMALLEST=0
      DO 60 I=1,ALLPPTOT
C  deleted
      IF(ALLPPTBL(1,I).EQ.-2) GOTO 60
C  Is this a loopend pp?
      IF (ALLPPTBL(1,I).NE.LOOPEND.AND.
     *    ALLPPTBL(2,I).NE.LOOPEND)GOTO 60
C  yes
      IF(SMALLEST.NE.0.AND.
     *   ALLPPTBL(3,I).GT.SMALLEST)
     *        GOTO 60
C  update
      SMALLEST=ALLPPTBL(3,I)
      PPINDEX=I
 60   CONTINUE
C  Is this also lowpp for the other basin?
 61   IF(ALLPPTBL(5,PPINDEX).EQ.0)GOTO 65
C  Yes, put it at the end instead.
      ALLPPTOT=ALLPPTOT+1
      ALLPPTBL(1,ALLPPTOT)=LOOPEND
      ALLPPTBL(2,ALLPPTOT)=ALLPPTBL(1,PPINDEX)
      ALLPPTBL(3,ALLPPTOT)=SMALLEST
      ALLPPTBL(5,ALLPPTOT)=1
      ALLPPTBL(1,ALLPPTOT+1)=-1
      GOTO 70
 65   ALLPPTBL(5,PPINDEX)=1
      IF(ALLPPTBL(1,PPINDEX).EQ.LOOPEND)GOTO 70
      ALLPPTBL(2,PPINDEX)=ALLPPTBL(1,PPINDEX)
      ALLPPTBL(1,PPINDEX)=LOOPEND
 70   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE UPDATE(ALLPPTOT,NUM)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION ALLPPTBL(8,2000),STACK(500)
      INTEGER*2 MASK(4002,3),LEVEL(4002,3)
      EQUIVALENCE(MASK(1,1),STACK(1))
      COMMON MASK,LEVEL
      COMMON /TBL/ALLPPTBL
C  STACK(NUM) goes to the mask already
      DO 10 I=1,NUM-1
C  Process STACK(I)
C  Update it's pp elev
      BIGGEST=ALLPPTBL(3,STACK(I))
      DO 20 J=I+1,NUM
 20   BIGGEST=MAX(ALLPPTBL(3,STACK(J)),BIGGEST)
      ALLPPTBL(3,STACK(I))=BIGGEST
C  Mark it as going to mask now
 10   ALLPPTBL(2,STACK(I))=0
      RETURN
      END
C
C
      SUBROUTINE DOMAPTO(MAPTONUM,MEMBERS,NUM,LOOPEND)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION MAPTO(2,2000),MEMBERS(500)
      INTEGER*2 MASK(4002,3),LEVEL(4002,3)
      EQUIVALENCE (MAPTO(1,1),MASK(1,2))
      COMMON MASK,LEVEL
C
      TOLINK=LOOPEND
      IF(MAPTONUM.EQ.0)GOTO 20
C  Is loopend already linked to something?
      DO 10 I=1,MAPTONUM
      IF(MAPTO(1,I).NE.LOOPEND)GOTO 10
      TOLINK=MAPTO(2,I)
 10   CONTINUE
C  If an old list entry drains to one of the members, change it
C  to drain to TOLINK
      DO 40 I=1,NUM
      TRY=MEMBERS(I)
      DO 50 J=1,MAPTONUM
      IF(MAPTO(2,J).NE.TRY)GOTO 50
      MAPTO(2,J)=TOLINK
 50   CONTINUE
 40   CONTINUE
C  Put members on the list
 20   DO 30 I=1,NUM-1
      MAPTONUM=MAPTONUM+1
      MAPTO(1,MAPTONUM)=MEMBERS(I)
 30   MAPTO(2,MAPTONUM)=TOLINK
      RETURN
      END
C
      SUBROUTINE FILL(ALLPPTOT,MAPTONUM,LUNIT,
     *   MUNIT,NL,NS)
      IMPLICIT INTEGER*2 (A-Z)
      DIMENSION BUFFER(4002,3),ALLPPTBL(8,2000),MAPTO(2,2000)
      INTEGER*2 MASK(4002,3)
      EQUIVALENCE(MASK(1,2),MAPTO(1,1))
      COMMON MASK,BUFFER
      COMMON /TBL/ALLPPTBL
C   Apply the pour pts to the elevations within each watershed
C
C   Rebuild a collapsed pp table where index is basin number and
C   element 8 is the ppelev.
C
      DO 10 I=1,ALLPPTOT
C   deleted
      IF(ALLPPTBL(1,I).EQ.-2)GOTO 10
C   not a lowest
      IF(ALLPPTBL(5,I).NE.1)GOTO 10
      ALLPPTBL(8,ALLPPTBL(1,I))=ALLPPTBL(3,I)
 10   CONTINUE
C   Incorporate maptos
      DO 20 I=1,MAPTONUM
 20   ALLPPTBL(8,MAPTO(1,I))=ALLPPTBL(8,MAPTO(2,I))
C   Now raise cells to pp levels
      DO 30 I=1,NL
      READ(MUNIT,REC=I)(BUFFER(J,1),J=1,NS)
      READ(LUNIT,REC=I)(BUFFER(J,2),J=1,NS)
      DO 35 J=1,NS
      IF(BUFFER(J,1).LE.0)GOTO 35
      PPELEV=ALLPPTBL(8,BUFFER(J,1))
      IF(BUFFER(J,2).LT.PPELEV)BUFFER(J,2)=PPELEV
 35   CONTINUE
      WRITE(LUNIT,REC=I)(BUFFER(J,2),J=1,NS)
 30   CONTINUE
      RETURN
      END
