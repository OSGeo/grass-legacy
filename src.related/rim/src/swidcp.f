      INTEGER FUNCTION SWIDCP(I1,I2,LEN)
      DOUBLE PRECISION I1(1), I2(1)
      N = LEN/2
      DO 10 I = 1,N
      IF (I1(I).GT.I2(I)) GO TO 20
      IF (I1(I).LT.I2(I)) GO TO 30
   10 CONTINUE
      SWIDCP = 0
      RETURN
   20 SWIDCP = -1
      RETURN
   30 SWIDCP = 1
      RETURN
      END
