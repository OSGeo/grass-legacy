C$NOFLOATCALLS
      SUBROUTINE BOUNDS                                                 BOUNDS 2
C * THIS SUBROUTINE DEFINES THE AREA OF INTEREST FOR THE LCDN CALCULATIONS AND
C SETS THE UNITS OF THE AREA IN TERMS OF MAP COORDINATES USED FOR THE CREATION
C OF THE PLOT.
  
  
  
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS                              BOUNDS 3
      COMMON /IO/KARD,KPRINT                                            BOUNDS 4
      LOGICAL BDS                                                       BOUNDS 5
      CPTIME=SECOND(CP)                                                 BOUNDS 6
  
  
  
      WRITE(KPRINT,591)                                                 BOUNDS 7
 591  FORMAT(1H1///35X,'.....    BOUNDS    .....'//)                    BOUNDS 8
      BDS=.FALSE.                                                       BOUNDS 9
  
  
C THIS INFORMATION IS FOUND ON THE BDS-2 CARD 
C XMIN= MINIMUM X COORDINATE
C YMIN= MINIMUM Y COORDINATE
      READ(KARD,600,END=900) XMIN,YMIN                                          
C ERROR MESSAGE%  " MISSING INPUT CARD%  JOB ABORTED
  
C THIS INFORMATION IS FOUND ON THE BDS-3 CARD 
C XMAX= MAXIMUM X COORDINATE
C YMAX= MAXIMUM Y COORDINATE
      READ(KARD,600,END=900) XMAX,YMAX                                          
 600  FORMAT(2F10.0)                                                    BOUNDS14
  
  
C     WRITE OUT MIN AND MAX BOUNDS;OUTPUT FOR BOUNDS
      WRITE(KPRINT,599) XMIN,YMIN,XMAX,YMAX                             BOUNDS15
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C below format statement changed as program is now using
C UTM coordiantes -- needs to print out with spacing so
C that coordinates do not run together
C599  FORMAT(20X,'MINIMUM BOUNDARY = ',2F8.0, 5X,'MAXIMUM BOUNDARY = ',2BOUNDS16
C    1F8.0)                                                             BOUNDS17
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 599  FORMAT(20X,'MINIMUM BOUNDARY = ',F10.1,1x,F10.1,5X,               BOUNDS16
     1 'MAXIMUM BOUNDARY = ',F10.1,1x,F10.1)                            BOUNDS17
  
  
C      MIN OR MAX ERROR TEST                                            BOUNDS18
      IF((XMAX.LE.XMIN).OR.(YMAX.LE.YMIN))GO TO 700                     BOUNDS19
C     BOUNDS VERIFIED--SET FLAG TRUE
      BDS=.TRUE.                                                        BOUNDS20
  
  
C VERIFICATION-" BOUNDARY VALUES VERIFIED 
      WRITE(KPRINT,650)                                                 BOUNDS21
 650  FORMAT(1H0,19X,'BOUNDARY VALUES VERIFIED')                        BOUNDS22
      GO TO 798                                                         BOUNDS23
  
  
C ERROR MESSAGE%"INCORRECT BOUNDARIES//NO BOUNDS SET" 
 700  WRITE(KPRINT,705)                                                 BOUNDS24
 705  FORMAT(20X,'INCORRECT BOUNDARIES'//40X,'NO BOUNDS SET')           BOUNDS25
      BDS=.FALSE.                                                       BOUNDS26
  
  
C CALCULATION OF TIME SPENT IN BOUNDS SUBPROGRAM
 798  CPTIM11=SECOND(CP)                                                BOUNDS27
      CPTIME=CPTIM11-CPTIME                                             BOUNDS28
      WRITE(KPRINT,799) CPTIME                                          BOUNDS29
 799  FORMAT(1H0,20X,'..... TIME IN BOUNDS IS',F8.3,' .....')           BOUNDS30
      RETURN                                                            BOUNDS31
C EOF EXIT                                                              BOUNDS32
  900 WRITE(KPRINT,901)                                                 BOUNDS33
  901 FORMAT(//' ERROR -- MISSING INPUT CARD; JOB ABORTED')             BOUNDS34
      STOP                                                              BOUNDS35
      END                                                               BOUNDS36
