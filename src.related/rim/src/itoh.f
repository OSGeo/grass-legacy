      SUBROUTINE ITOH(I,J,K)
      INCLUDE 'syspar.d'
C
C  PURPOSE:   UNPACK I AND J FROM K
C
C  I WAS MULTIPLIED BY ZHTOI
C
      I = K / ZHTOI
      J = K - (ZHTOI * I)
      RETURN
      END
