      SUBROUTINE DATJUL(DD,MM,YY,JUL)
      INCLUDE 'syspar.d'
C
C     CONVERT JULIAN TO D,M,Y
C
C     FROM ACM-CA 199, BY R. TANTZEN
C
      J = JUL
 
      J = J - 1721119
 
      Y = (4*J-1)/146097
      J = 4*J-1 - 146097*Y
      D = J/4
 
      J = (4*D+3)/1461
      D = 4*D+3 - 1461*J
      D = (D+4)/4
 
      M = (5*D-3)/153
      D = 5*D-3 - 153*M
      D = (D+5)/5
 
      Y = 100*Y + J
 
      IF (M.LT.10) THEN
         M = M + 3
      ELSE
         M = M - 9
         Y = Y + 1
      ENDIF
 
      DD = D
      MM = M
      YY = Y
      RETURN
      END
