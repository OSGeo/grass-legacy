      SUBROUTINE TOLER(K,V,N)
      INCLUDE 'syspar.d'
C
C     THIS ROUTINE APPLIES A TOLERANCE TO A REAL ROUTINE
C
C     K IS LOCBOO VALUE
C     V(N) IS REAL ARRAY
C
      INCLUDE 'flags.d'
      REAL V(N)
      REAL X
      X = TOL
      IF(K.GT.5) X = -TOL
      IF(PCENT) GO TO 50
      DO 20 I=1,N
      V(I) = V(I) - X
   20 CONTINUE
      RETURN
   50 CONTINUE
      DO 70 I=1,N
      V(I) = V(I)*(1.-X)
   70 CONTINUE
      RETURN
      END
