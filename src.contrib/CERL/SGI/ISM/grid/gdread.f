      SUBROUTINE GDREAD
     $(GNAME,DESC,ELVARR,IDMXCL,IDMYRW,
     $ INMXCL,XGDMIN,XGDMAX,INMYRW,YGDMIN,YGDMAX,ZNLVAL,
     $ NAMSDF,SDFFLD,NAMVFL,NAMNVF,
     $ PRJFLG,IPROJ,IZONE,IUNITS,GCTPAR,
     $ ISTAT)
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
C RESTRICTED  Use, duplication, or disclosure    DYNAMIC GRAPHICS, INC.<
C RIGHTS      is subject to restrictions         2855 TELEGRAPH AVE.   <
C LEGEND      stated in license agreement with:  BERKELEY, CA 94705    <
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
C
C  PURPOSE :  User interface to read an ISM grid.  This version is valid
C             from release 6.93A.  The ISTAT variable returns the
C             success of the operation. Values greater than zero are
C             the value of IOSTAT= when the grid was opened. A value
C             of -1 indicates a read error, and a value of -2
C             indicates that the supplied ELVARR (dimensioned by
C             IDMXCL and IDMYRW) is too small to contain the grid. The
C             required size is in INMXCL,INMYRW.  A value of 0 in ISTAT
C             indicates success.
C             To ensure full data extraction, the following sizes are
C             required:
C                   DESC       CHARACTER*40
C                   ELVARR     REAL (512,512)
C                   NAMSDF     CHARACTER*20
C                   SDFFLD     CHARACTER*8
C                   NAMNVF     CHARACTER*20
C                   NAMVFL     CHARACTER*20
C                   GCTPAR     DOUBLE PRECISION (8)
C
C  AUTHOR/DATE :  Patrick Rigney,  September 1987
C
C  REVISED BY   DATE       EXPLANATION
C  A Kaugars    17 SEP 87  Added projection info handling.
C  P Rigney     10/26/87   Added scattered data field handling.
C
C----------------------------------------------------------------------+
C     D E C L A R A T I O N S
C----------------------------------------------------------------------+

C===  Input arguments

C  Name of grid to be read.
      CHARACTER*(*)        GNAME
C  1st dimension of ELVARR
      INTEGER              IDMXCL
C  2nd dimension of ELVARR
      INTEGER              IDMYRW

C......................................................................+

C===  Output arguments

C  Description (40 chars)
      CHARACTER*(*)        DESC
C  Grid
      REAL                 ELVARR(IDMXCL,IDMYRW)
C  # X columns in grid
      INTEGER              INMXCL
C  X min
      REAL                 XGDMIN
C  X max
      REAL                 XGDMAX
C  # Y rows in grid
      INTEGER              INMYRW
C  Y min
      REAL                 YGDMIN
C  Y max
      REAL                 YGDMAX
C  Null value
      REAL                 ZNLVAL
C  Scattered data file
      CHARACTER*(*)        NAMSDF
C  Field used in NAMSDF.
      CHARACTER*(*)        SDFFLD
C  Non-vert fault file
      CHARACTER*(*)        NAMNVF
C  Vert fault file
      CHARACTER*(*)        NAMVFL
C  Projection info flag
      LOGICAL              PRJFLG
C  GCTP projection number
      INTEGER              IPROJ
C  Projection zone number,
      INTEGER              IZONE
C  (if applicable)

C  GCTP units code
      INTEGER              IUNITS
C  GCTP projection params
      DOUBLE PRECISION     GCTPAR(8)
C  Status flag.
      INTEGER              ISTAT

C......................................................................+

C===  Local variables

      CHARACTER*480        CHRREC
      INTEGER              I
      INTEGER              J
C======================================================================+

C
C..... Open file.
C
      OPEN(UNIT=30,FILE=GNAME,FORM='UNFORMATTED',STATUS='OLD',
     $  IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 999
C
C..... Read header.
C
      READ(30,ERR=800,IOSTAT=ISTAT) CHRREC(1:320)
C
C..... Copy out file description and other header information.
C
      DESC = CHRREC(41:80)
      READ(CHRREC(161:240),20) XGDMIN,XGDMAX,YGDMIN,YGDMAX,ZNLVAL
      READ(CHRREC(241:248),40) INMXCL,INMYRW
      NAMSDF = CHRREC(249:268)
      SDFFLD = CHRREC(289:296)
      NAMVFL = CHRREC(305:320)
      NAMNVF = CHRREC(269:288)
C
C..... Check projection flag, and read projection info, if present.
C
      PRJFLG = CHRREC(153:156) .EQ. 'Y '
      IF (PRJFLG) THEN
         READ(30,ERR=800,IOSTAT=ISTAT) CHRREC(321:480)
         READ (CHRREC(321:),5050,ERR=800)
     $    IPROJ,IZONE,IUNITS,(GCTPAR(J),J=1,8)
5050     FORMAT (3I4, 4X, 8F16.0)
      ENDIF
C
C..... Read elevation array, if there's room.
C
      IF (INMXCL .GT. IDMXCL .OR. INMYRW .GT. IDMYRW) GOTO 850
      DO 50 J = 1,INMYRW
         READ(30,ERR=800) (ELVARR(I,J),I=1,INMXCL)
50    CONTINUE
      GOTO 900
20    FORMAT(5G16.7)
40    FORMAT(2I4)
C
C..... Error reading grid file.
C
800   ISTAT = -1
      GOTO 900
C
C..... Specified dimension of ELVARR is too small to contain grid.
C
850   ISTAT = -2
      GOTO 900
C
C..... Close file.
C
900   CLOSE(UNIT=30)
C
C..... Exit.
C
999   CONTINUE
      RETURN
      END
