C$NOFLOATCALLS
      SUBROUTINE READTB                                                 READTB 2
C READS INFORMATION FOR TABGEN (FOUND IN TAPE 20) AND MODIFIES IT ACCORDING TO
C THE INVERSION FACTORS GIVEN IN THE CALLING SUBROUTINE. THE TABLES ARE 
C UNDER THE CONDITIONS OF STANDARD PERCENT TEMPERATURE INVERSION FACTORS. 
      COMMON/IO/KARD,KPRINT                                             READTB 3
      COMMON/FACTI/ RINV1,RINV2,RINV3 
      COMMON/DEBUG/CHECK,REED,TABRD                                     READTB 5
      COMMON/PARM/THRESH,PENITE 
      LOGICAL CHECK,REED,TABRD                                          READTB 6
C     COMMON BLOCK TABL1 CONTAINS THE TABLES OF PROGRAM TABGEN
C     FOUND IN TAPE 20. 
C     DBV=TABLE OF DB VALUES
C     PERV=TABLE OF PERCENTAGES 
C     ENV=TABLE OF ENERGY VALUES
C     CSCF=TABLE OF CHARGE SIZE CORRECTION FACTORS
C     FON1 AND FON2 ARE USED IN F1 COMPUTATION
C     FTWO IS USED IN F2 COMPUTATION
      COMMON /TABL1/ DBV(301,9,2) , PERV(301,4,2) , ENV(1501) , 
     1 CSCF(601) ,FON1(301,4,2),FON2(301,4,2),FTWO(151,2) 
      DATA IN1/20/                                                      READTB 8
      REWIND IN1                                                        READTB 9
C THIS READS THE STANDARD PERCENT TEMPERATURE INVERSION FACTORS FROM TAPE20.
      READ (IN1,END=999) PC1,PC2,PC3
C ERROR...DB,PERCENT CURVE TABLES MISSING--PROGRAM ABORTED* 
C     L=1 IS DAY
C     L=2 IS NIGHT
C     J=1 FOCUS MAX 
C     J=2 FOCUS MEAN
C     J=3 BASE MAX
C     J=4 BASE MEAN 
C     J=5 NEG MAX 
C     J=6 NEG MEAN
C     J=7 EX NEG MAX
C     J=8 EX NEG MEAN 
C     J=9 EX NEG MIN
      DO 20 L=1,2 
      DO 20 J=1,9 
C READ DB VALUES FORM TAPE 20 INTO ARRAY DBV. 
      READ (IN1) (DBV(I,J,L),I=1,301) 
20    CONTINUE
C     L=1 IS DAY
C     F=2 IS NIGHT
C     J=1 FOCUS 
C     J=2 BASE
C     J=3 NEG 
C     J=4 EX NEG
C 
      DO 30 L=1,2 
      DO 30 J=1,4 
C READ PERCENT VALUES FROM TAPE 20 INTO ARRAY PERV. 
      READ (IN1) (PERV(I,J,L),I=1,301)
30    CONTINUE
C CSCF-CHARGE SIZE CORRECTION FACTOR. 
      READ (IN1) CSCF 
C COMPUTE CORRECTION FACTORS FOR THE INVERSION FACTORS.  DISTANCE BETWEEN 
C SOURCE AND POINT < 2 MILES.  DISTANCE> 10 MILES  2<DISTANCE<10
      R0=(RINV1+RINV2)/(PC1+PC2)                                        READTB14
      R1=RINV1/PC1                                                      READTB15
      R2= ((RINV3-PC3)/2. + PC3) /PC3 
C     CORRECT THE PERCENTAGE
      DO 100 K=1,2                                                      READTB17
      DO 100 J=1,301
      BASE=  PERV(J,2,K)
      FOCUS=  PERV(J,1,K) 
      GNEG=  PERV(J,3,K)
      EXNEG=100.-(BASE+FOCUS+GNEG)                                      READTB22
      IF(K.EQ.1) RATIO=R0                                               READTB23
C  2 MILE OR LESS        (152)                                          READTB24
C     100*ALOG10((2 MILE)*(5280 FEET/MILE) *(.3048 METER/FEET)) -199
      IF(K.EQ.2.AND.J.LT.152)RATIO=R1 
C     10 OR GREATER       (22)
      IF (K.EQ.2.AND.J.GT.222) RATIO  =R2 
C BETWEEN 2 AND 10                                                      READTB28
      IF (K.EQ.2.AND. (J.LE.222.AND. J.GE.152)) 
     1  RATIO = (R2-R1) * (J-152) / 70.0  + R1
      B1=BASE*RATIO                                                     READTB30
      F1=FOCUS*RATIO                                                    READTB31
      DELB= BASE-B1 +FOCUS-F1                                           READTB32
      DELN=GNEG/(GNEG+EXNEG)*DELB                                       READTB33
      GN1=GNEG+DELN                                                     READTB34
      IF (F1.LT.0) F1=0.
      IF (B1.LT.0.) B1=0. 
      IF (GN1.LT.0.) GN1=0. 
      PERV(J,1,K)=  F1/100. 
      PERV(J,2,K) = B1/100. 
      PERV(J,3,K)= GN1/100. 
      PERV(J,4,K)= (100.-F1-B1-GN1)/100.
  100 CONTINUE                                                          READTB47
C     FONE
C F1 COMPUTATION
      TLT =10.0 / ALOG (10.0) 
      THRSH=10.**(THRESH/10.) 
      DO 50 I=1,301 
      DO 50 J=1,4 
      DO 50 K=1,2 
      RMEAN  = DBV(I,J*2,K) 
      RMAX=  DBV(I,J*2-1,K) 
      RMIN=  DBV(I,J*2+1,K) 
C     GET THE K FACTOR
C GET THE K FACTOR
      RK= (TLT*( 10.**((RMAX-RMEAN)/10.0)-1.0) - (RMAX-RMEAN))  / 
     1  ((RMEAN-RMIN) - (TLT * (1.0- 10.0**((RMIN-RMEAN)/10.0) )))
C     CASE ONE
      FON1(I,J,K)=TLT*THRSH*RK/(RK*(RMEAN-RMIN)+(RMAX-RMEAN)) 
C     CASE TWO
      FON2(I,J,K)=TLT*THRSH/(RK*(RMEAN-RMIN)+(RMAX-RMEAN))
50    CONTINUE
C     FTWO
      DO 60 I= 1,151
C     CASE 1
C F2 COMPUTATION
      FTWO(I,1)= 1.0-10. **((1-I)/100.0)
C     CASE 2
      FTWO(I,2) = 10.0**((I-1)/100.)-1.0
60    CONTINUE
C     DB TO ENERGY
      DO 70 I=1,1501
C CHANGE DB TO ENERGY.  STORE IN ARRAY ENV. 
      ENV(I) = 10.0 **((I+249) / 100.0) 
70    CONTINUE
C       CORRECT FOR NIGHT TIME VALUES 
      DO 80 I=1,301 
      DO 80 J=1,9 
C NIGHTTIME CORRECTION FACTOR.
      DBV(I,J,2) = DBV(I,J,2) + PENITE
80    CONTINUE
C LOGICAL FLAG IS TRUE AFTER THE EXECUTION OF THE SUBROUTINE
      TABRD=.TRUE.                                                      READTB48
      IF (.NOT.CHECK) RETURN
      WRITE(KPRINT,997)      DBV
      WRITE(KPRINT,996) PERV
      WRITE(KPRINT,995) CSCF
      WRITE(KPRINT,994) FON1
      WRITE(KPRINT,994) FON2
      WRITE(KPRINT,993) FTWO
      WRITE(KPRINT,992) ENV 
997   FORMAT(1H1,18(/,1H0,20(/,1H ,15F8.2),/,1H ,F8.2)) 
996   FORMAT (1H1,8(/,1H0,20(/,1H ,15F8.4),/,1H ,F8.4)) 
995   FORMAT(1H1,(15F8.2))
994   FORMAT(1H1,8(/,1H0,30(/,1H ,10E12.5),/,1H ,E12.5))
993   FORMAT(1H1,2(/,1H0,15(/,1H ,10E12.5),/,1H ,E12.5))
992   FORMAT(1H1,151(/,1H ,10E12.5))
      RETURN                                                            READTB49
 999  WRITE(KPRINT,998)                                                 READTB50
  998 FORMAT(//10X,'..... ERROR .....DB,PERCENT CURVE TABLES(TAPE20) MISREADTB51
     1SING -- PROGRAM ABORTED')                                         READTB52
      STOP                                                              READTB53
      END                                                               READTB54
