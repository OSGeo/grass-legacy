      SUBROUTINE RMTIME(TIM)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C  PURPOSE:   RETURN THE CURRENT TIME AS INTEGER (SEC FROM MIDNIGHT)
C
C  PARAMETERS:
C         TIM-----THE CURRENT TIME
C
      integer t(3)          
      CALL itime(T)  
      TIM = t(1)*3600 + t(2)*60 + t(3)
      RETURN
      END
