      SUBROUTINE GDWRIT
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
C  PURPOSE :  Write and ISM-style grid.
C
C  AUTHOR/DATE :  Patrick Rigney,  August 1987
C
C  REVISED BY   DATE       EXPLANATION
c  Carl Godkin  21 Dec 88  Fix bug finding epsilon for ZNLVAL.
c  Carl Godkin   2 Dec 88  Always write ISM null value into grid header.
C  A Kaugars    17 SEP 87  Added projection info handling.
C  P Rigney     10/26/87   Added scattered data field handling.
C
C----------------------------------------------------------------------+
C     D E C L A R A T I O N S
C----------------------------------------------------------------------+

C===  Common blocks

C......................................................................+

C===  Input arguments

      CHARACTER*(*)        GNAME
      CHARACTER*(*)        DESC
      REAL                 ELVARR(IDMXCL,IDMYRW)
      INTEGER              INMXCL
      REAL                 XGDMIN
      REAL                 XGDMAX
      INTEGER              INMYRW
      REAL                 YGDMIN
      REAL                 YGDMAX
      REAL                 ZNLVAL
      CHARACTER*(*)        NAMSDF
      CHARACTER*(*)        SDFFLD
      CHARACTER*(*)        NAMVFL
      CHARACTER*(*)        NAMNVF
      LOGICAL              PRJFLG           ! Projection info flag
      INTEGER              IPROJ            ! GCTP projection number
      INTEGER              IZONE            ! Projection zone number,
                                            ! (if applicable)
      INTEGER              IUNITS           ! GCTP units code
      DOUBLE PRECISION     GCTPAR(8)        ! GCTP projection params

C......................................................................+

C===  Output arguments

      INTEGER              ISTAT

C......................................................................+

C===  Local variables

      CHARACTER*480        CHRREC
      INTEGER I
      INTEGER J
      REAL ZNLISM
      REAL EPSNUL
C======================================================================+

C
C..... Convert null grid cells to ISM null value.
C
      ZNLISM = 1.0E20
      EPSNUL = abs(ZNLVAL) * 0.00001
      DO 32 J=1,INMYRW
         DO 32 I=1,INMXCL
            Z = ELVARR(I,J)
            IF(ABS(Z-ZNLVAL).LE.EPSNUL) ELVARR(I,J) = ZNLISM
32    CONTINUE
C
C..... Open file.
C
      OPEN(UNIT=30,FILE=GNAME,FORM='UNFORMATTED',STATUS='UNKNOWN',
     $     IOSTAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 999
C
C..... Pack header record.
C
      CHRREC = '  '
      CHRREC(5:24) = GNAME
      CHRREC(25:28) = 'GRID'
      CHRREC(29:32) = '   1'
      CHRREC(41:80) = DESC
      CHRREC(137:144) = 'none'
      WRITE(CHRREC(161:240),20) XGDMIN,XGDMAX,YGDMIN,YGDMAX,ZNLISM
      WRITE(CHRREC(241:248),40) INMXCL,INMYRW
      CHRREC(249:268) = NAMSDF
      CHRREC(289:296) = SDFFLD
      CHRREC(305:320) = NAMVFL
      CHRREC(269:288) = NAMNVF
C
C..... Check projection flag, and write projection info, if present.
C
      IF (PRJFLG) THEN
         CHRREC(153:156) = 'Y '
         DO 60 J=1,8
            INX1 = 321 + 16 + (J - 1) * 16
            INX2 = INX1 + 15
            CALL CNENCD(GCTPAR(J),CHRREC(INX1:INX2),JSTAT)
            IF (JSTAT .NE. 0) GOTO 9010
60       CONTINUE
         WRITE (CHRREC(321:480),5050,ERR=800)
     $    IPROJ,IZONE,IUNITS,(GCTPAR(J),J=1,8)
5050     FORMAT (3I4, 4X, 8G16.7)
      ELSE
         CHRREC(153:156) = 'N '
      ENDIF
C
C..... Write header.
C
      WRITE(30,ERR=800) CHRREC(1:320)
      IF (PRJFLG) THEN
         WRITE(30,ERR=800,IOSTAT=ISTAT) CHRREC(321:480)
      ENDIF
C
C..... Write grid.
C
      DO 50 J = 1,INMYRW
         WRITE(30,ERR=800) (ELVARR(I,J),I=1,INMXCL)
50    CONTINUE
      GOTO 900
20    FORMAT(5G16.7)
40    FORMAT(2I4)
C
C..... Write error.
C
800   ISTAT = -1
      GOTO 900
C
C..... Encode error.
C
9010  CONTINUE
      WRITE(*,6010)
6010  FORMAT('Error encoding projection info.')
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
