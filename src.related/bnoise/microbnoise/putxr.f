C$NOFLOATCALLS
      SUBROUTINE PUTXR(XARRAY,MDIM,NDIM,N1,N2,AID1,AID2,DAYS,XREF,IJ)   PUTXR  2
      COMMON/IO/KARD,KPRINT                                             PUTXR  3
      DIMENSION XARRAY(MDIM,NDIM),AID1(1),AID2(1),XREF(2) 
      DATA STARS/'***'/                                                 PUTXR  5
      DO 20 I=1,N1                                                      PUTXR  6
      XREF(1)=AID1(I)                                                   PUTXR  7
      K=2                                                               PUTXR  8
      DO 10 J=1,N2                                                      PUTXR  9
      IF(IJ.EQ.1)X=XARRAY(I,J)                                          PUTXR 10
      IF(IJ.EQ.2)X=XARRAY(J,I)                                          PUTXR 11
      IF(X.LE.0.)GO TO 10                                               PUTXR 12
      XREF(K)=AID2(J)                                                   PUTXR 13
      XREF(K+1)=X/DAYS                                                  PUTXR 14
      K=K+2                                                             PUTXR 15
   10 CONTINUE                                                          PUTXR 16
      K=K-1                                                             PUTXR 17
      IF(K.GT.1)GO TO 15                                                PUTXR 18
      XREF(2)=STARS                                                     PUTXR 19
      K=2                                                               PUTXR 20
 15   WRITE(KPRINT,50)(XREF(N),N=1,K)                                   PUTXR 21
   20 CONTINUE                                                          PUTXR 22
      RETURN                                                            PUTXR 23
   50 FORMAT(/4X,A3,2X,7(3X,A3,F8.1,2X)/(9X,7(3X,A3,F8.1,2X)))          PUTXR 24
      END                                                               PUTXR 25
