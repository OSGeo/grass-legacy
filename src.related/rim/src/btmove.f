      SUBROUTINE BTMOVE(NEW,OLD,NV)
      INCLUDE 'syspar.d'
C
C  PURPOSE:   MOVE NV VALUES FROM OLD TO NEW.
C
      INCLUDE 'btbuf.d'
      INTEGER OLD
      IS = 1
      IF(NV.LT.0) IS = -1
      N = IS * NV
      DO 100 I=1,N
      IN = NEW + IS * (I - 1)
      IO = OLD + IS * (I - 1)
      VALUE(1,IN) = VALUE(1,IO)
      VALUE(2,IN) = VALUE(2,IO)
      VALUE(3,IN) = VALUE(3,IO)
  100 CONTINUE
      RETURN
      END
