      SUBROUTINE READIN                                                 READIN 2
C     THIS ROUTINE IS CALLED TO READ DATA FROM FORMA
      COMMON/IO/KARD,KPRINT                                             READIN 3
      COMMON /SRCS/ NSRCS 
      COMMON/FT/GRDSZ,CNR,XLOC,YLOC,DAYNO,DARKNO                        READIN 5
      DIMENSION XLOC(2000),YLOC(2000),DARKNO(2000),DAYNO(2000)          READIN 6
      COMMON/GRID/NRCNR,EAS,RCNR,DIST,SDBWH,              ANGCOS,ANGSIN 
      DIMENSION RCNR(10),DIST(10),SDBWH(2000),
     1ANGCOS(2000),ANGSIN(2000)                                         READIN 9
      COMMON/DEBUG/CHECK,REED                                           READIN10
      LOGICAL CHECK,REED                                                READIN11
      DATA IN1/8/                                                       READIN12
C     READ NUMBER OF NOISE SOURCES AND NUMBER OF DAYS 
      READ(IN1) NSRCS,DAYS
C     READ XLOC AND YLOC FOR TARGETS AND FIRING POINTS
      READ(IN1) XLOC                                                    READIN16
      READ(IN1) YLOC                                                    READIN17
C     READ CHARGE SIZES 
      READ(IN1) SDBWH                                                   READIN18
C     READ SINE AND COSINE FOR ANGLE BETWEEN GUN AND TARGET 
      READ(IN1) ANGSIN                                                  READIN19
      READ(IN1) ANGCOS                                                  READIN20
C     NUMBER OF DAY FIRINGS AND NIGHT FIRINGS 
      READ(IN1) DAYNO                                                   READIN21
      READ(IN1) DARKNO                                                  READIN23
C     REWIND FILE 
      REWIND IN1                                                        READIN25
C     SET FLAG
      REED=.TRUE.                                                       READIN26
      RETURN                                                            READIN68
      END                                                               READIN69
