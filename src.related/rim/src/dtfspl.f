      SUBROUTINE DTFSPL(L,DP,MP,ML,YP,YL,SC,DTFINT)
      INCLUDE 'syspar.d'
C
C     SPLIT A DATE-TIME FORMAT INTEGER INTO COMPONENTS
C
      INCLUDE 'flags.d'
C
C     OFFSETS FOR DATA PACKING
C
      PARAMETER (T1=12, T2=12**2, T3=12**3, T4=12**4, T5=12**5)
      PARAMETER (TA=128)
C
C     USE DEFAULT DATE SPECIFICATION IF NONE GIVEN
C
      I = DTFINT
      IF (I.EQ.0) I = KRMDTF
C
      I = I/TA
C
C     LENGTH
      L = I/T5
      I = I - L*T5
C
C     DAY
      DP = I/T4
      I = I - DP*T4
C
C     MONTH
      MP = I/T3
      I = I - MP*T3
      ML = I/T2
      I = I - ML*T2
C
C     YEAR
      YP = I/T1
      I = I - YP*T1
      YL = I
C
C     SEPERATION CHAR
      SC = MOD(DTFINT,TA)
C
      RETURN
      END
