      SUBROUTINE WHRNAM(N,ATT)
      INCLUDE 'syspar.d'
C
C     SETUP A WHERE CLAUSE TO MATCH AN ATTRIBUTE NAME
C
      INCLUDE 'ascpar.d'
      INCLUDE 'whcom.d'
      INCLUDE 'rmatts.d'
C
      NBOO = 1
      BOO(1) = WHAND
      KATTP(1) = N
      CALL HTOI(ZC,Z,KATTL(1))
      KATTY(1) = KZTEXT
      KOMTYP(1) = 2
      KOMPOS(1) = 1
      KOMLEN(1) = 1
      KOMPOT(1) = 1
      KSTRT = 0
      MAXTU = ALL9S
      LIMTU = ALL9S
      CALL ZMOVE(WHRVAL,ATT)
      WHRLEN(1) = KATTL(1)
      RETURN
      END
