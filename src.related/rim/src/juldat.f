      LOGICAL FUNCTION JULDAT(DD,MM,YY,JUL)
      INCLUDE 'syspar.d'
C
C     CONVERT D,M,Y TO JULIAN
C     RETURN <TRUE> IF D,M,Y ARE VALID
C
C     FROM ACM-CA 199, BY R. TANTZEN
C
      D = DD
      M = MM
      Y = YY
      IF (M.GT.2) THEN
         M = M - 3
      ELSE
         M = M + 9
         Y = Y - 1
      ENDIF
      C = Y/100
      YA = Y - 100*C
      JUL = (146097*C)/4 + (1461*YA)/4 +
     X  (153*M + 2)/5 + D + 1721119
 
C     TEST ARGS FOR VALIDITY
 
      CALL DATJUL(D,M,Y,JUL)
      IF (D.EQ.DD .AND. M.EQ.MM .AND. Y.EQ.YY) THEN
         JULDAT = .TRUE.
      ELSE
         JULDAT = .FALSE.
      ENDIF
      RETURN
      END
