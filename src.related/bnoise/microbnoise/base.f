C$NOFLOATCALLS
      SUBROUTINE BASE                                                   BASE   2
C * THIS SUBROUTINE CREATES THE OUTLINE OF THE BASE OR REGION THE USER IS 
C WORKING WITH ON THE PLOT OUTPUT BY DRAWING LINE SEGMENTS FROM COORDINATE TO 
C COORDINATE OF THE SPECIFIED REGION. 
  
  
  
C     THESE TWO ARRAYS STORE THE VALUES OF THE STARTING 
C     AND ENDING POINTS FOR THE LINES DRAWN.
      DIMENSION XCOORD(2),YCOORD(2)                                     BASE   3
C     BLOCK BOUND CONTAINS BOUNDARY INFORMATION.
	  CHARACTER STAR
      COMMON/BOUND/XMIN,YMIN,XMAX,YMAX,BDS                              BASE   4
C     BOUNDS FLAG.
      LOGICAL BDS                                                       BASE   5
C     KARD=INPUT; KPRINT=OUTPUT 
      COMMON /IO/KARD,KPRINT                                            BASE   6
C     IOUT=TAPE 2 
      DATA IOUT/2/                                                      BASE   7
C     CDC FUNCTION TO COMPUTE TIME SPENT IN A PROGRAM 
      CPTIME=SECOND(CP)                                                 BASE   8
  
  
  
      WRITE(KPRINT,400)                                                 BASE   9
 400  FORMAT(1H1///35X,'.....     BASE     .....'//)                    BASE  10
  
  
C  IF THERE ARE NOBOUNDS IN THE INPUT MODULES, COMPLAIN.
      IF(BDS) GO TO 450                                                 BASE  11
      WRITE(KPRINT,470) 
470   FORMAT('  ...ERROR.  NO BOUNDS CALL BEFORE BASE....') 
      STOP
  
C THIS PRINTS OUT THE HEADING "LINE CARDS", "X-Y START", "X-Y END"
450   WRITE(KPRINT,592) 0                                               BASE  12
 592  FORMAT(I1,42X,'LINE CARDS')                                       BASE  13
      WRITE(KPRINT,593) 0                                               BASE  14
 593  FORMAT(I1,29X,'X-Y START',35X,'X-Y END')                          BASE  15
      REWIND IOUT                                                       BASE  16
  
  
C PHASE 4 PLOTS SPECIAL LINES AND TEXT ON THE MAP AND DRAWS A BORDER ON THE 
C MAP.
C     WRITE HEADER FOR PHASE 4
      WRITE(IOUT,590)                                                   BASE  17
  590 FORMAT(1h ,4HPHS4)
C     I2 ACTS AS A FLAG TO TELL IF A SEGMENT HAS BEEN COMPLETED 
C     (I2=1)
      I2=0                                                              BASE  19
C     I SERVES AS COUNTER FOR STARTING AND ENDING PT. INFO. 
 605  I=1                                                               BASE  20
C     READ BASE-2 CARD WITH COORDS OF POINT IN SET DESCRIBING 
C     A LINE SEGMENT. 
 610  READ(KARD,620,END=790) XCOORD(I),YCOORD(I),STAR                           
 620  FORMAT(2F10.0,A1)                                                 BASE  22
C ERROR MESSAGE-"NO EOF CARD% "JOB ABORTED" 
  
  
C THIS IF-STATEMENT SAYS THAT IF THERE IS A STAR IN COL. 21 AND NOTHING 
C ELSE ON THE CARD, THIS IS THE END OF THE INPUT. 
      IF((STAR.EQ.'*').AND.(XCOORD(I).EQ.0.).AND.(SIGN(1.0,XCOORD(I))
     1.GT.0)) GO TO 798                                                     
  
  
C THIS SETS THE ROUTINE FOR NEW LINES 
      IF(I2.EQ. 1) WRITE(KPRINT,661)0                                   BASE  26
 661  FORMAT(I1,20X,'NEW LINE')                                         BASE  27
      I2=0                                                              BASE  28
      IF(STAR.EQ.'*') I2=1                                              BASE  29
C     CHECK IF POINT OUT OF BOUNDS
      IF((XCOORD(I).LT.XMIN).OR.(XCOORD(I).GT.XMAX).OR.                 BASE  30
     1(YCOORD(I).LT.YMIN).OR.(YCOORD(I).GT.YMAX)) GO TO 710             BASE  31
C     IF ONLY 1 PT. HAS BEEN PROCESSED, READ NEXT CARD
      IF(I.EQ.1) GO TO 640                                              BASE  32
C     WRITE LINE INFO TO TAPE 2 
      WRITE(IOUT,631)'LINE',(XCOORD(J),YCOORD(J),J=1,2),1               BASE  33
 631  FORMAT(1h ,A4,2X,2F8.0,8X,2F8.0,8X,I2)
C     WRITE LINE INFO IN BASE MODULE OUTPUT.
      WRITE(KPRINT,630) 'LINE',(XCOORD(J),YCOORD(J),J=1,2)              BASE  35
 630  FORMAT(20X,A4,2X,2F8.0,27X,2F8.0)                                 BASE  36
C     IF END OF SEGMENT, REINITIALIZE COORDINATE ARRAYS 
C     AND START OVER. 
      IF(I2.EQ.1) GO TO 650                                             BASE  37
  
  
C     SET NEXT PT. ON SEGMENT.
      XCOORD(1)=XCOORD(2)                                               BASE  38
      YCOORD(1)=YCOORD(2)                                               BASE  39
      XCOORD(2)=0                                                       BASE  40
      YCOORD(2)=0                                                       BASE  41
 640  I=2                                                               BASE  42
      GO TO 610                                                         BASE  43
 650  DO 660 J=1,2                                                      BASE  44
      XCOORD(J)=0                                                       BASE  45
 660  YCOORD(J)=0                                                       BASE  46
      GO TO 605                                                         BASE  47
C                                                                       BASE  48
C      ERROR ROUTINES                                                   BASE  49
C      X OR Y COORD CAUSES BOUNDARY ERROR                               BASE  50
C                                                                       BASE  51
C ERROR MESSAGE-"NEXT CARD NOT WITHIN BOUNDARIES" 
 710  WRITE(KPRINT,715) 0                                               BASE  52
 715  FORMAT(I1,20X,39H*** NEXT CARD NOT WITHIN BOUNDARIES ***)         BASE  53
      REWIND IOUT                                                       BASE  54
      ENDFILE IOUT                                                      BASE  55
  
  
 717  WRITE(KPRINT,720) XCOORD(I),YCOORD(I),STAR                        BASE  56
 720  FORMAT(25X,2F10.0,A1)                                             BASE  57
      READ(KARD,620,END=790)XCOORD(I),YCOORD(I),STAR                            
C  ERROR MESSAGE-"NO EOF CARD%  JOB ABORTED"
      IF((STAR.EQ.'*').AND.(XCOORD(I).EQ.0.).AND.(SIGN(1.0,XCOORD(I))   BASE  60
     1 .GT.0.)) GO TO 796                                               BASE  61
      IF((XCOORD(I).GE.XMIN).AND.(XCOORD(I).LE.XMAX).AND.               BASE  62
     1(YCOORD(I).GE.YMIN).AND.(YCOORD(I).LE.YMAX)) GO TO 717            BASE  63
      GO TO 710                                                         BASE  64
 790  WRITE(KPRINT,791)                                                 BASE  65
 791  FORMAT(1H0,24X,31HERROR...NO EOF CARD (* IN CC21)/25X,11HJOB ABORTBASE  66
     1ED)                                                               BASE  67
  
  
      REWIND IOUT                                                       BASE  68
      ENDFILE IOUT                                                      BASE  69
      GO TO 798                                                         BASE  70
  
  
C ERROR MESSAGE-"DUE TO BOUNDARY ERRORS, NO OUTPUT TAPE WAS CREATED THIS RUN. 
 796  WRITE(KPRINT,797)                                                 BASE  71
 797  FORMAT(1H0,                                                       BASE  72
     1 25X,40H*** DUE TO BOUNDARY ERRORS NO OUTPUT ***/25X,             BASE  73
     140H***   TAPE WAS CREATED THIS RUN      ***)                      BASE  74
C THIS COMPUTES THE TIME SPENT IN BASE SUBPROGRAM.
 798  CPTIM11=SECOND(CP)                                                BASE  75
      CPTIME=CPTIM11-CPTIME                                             BASE  76
      WRITE(KPRINT,799) CPTIME                                          BASE  77
 799  FORMAT(1H0,20X,'..... TIME IN BASE IS',F8.3,' .....')             BASE  78
      RETURN                                                            BASE  79
      END                                                               BASE  80
