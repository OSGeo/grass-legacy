      SUBROUTINE GETT(ASTXT,POS,ASCHR)                                  
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C                                                                       
C     RETURN THE CH'TH ASCII-CHAR FROM ASTXT                            
C                                                                       
      INTEGER ASTXT(1), POS, ASCHR                                      
C                                                                       
      DATA M7 /127/                                                     
C                                                                       
      IW = (POS+ZCW-1)/ZCW                                              
      IC = (IW*ZCW) - POS                                               
      ASCHR = IAND(ISHFT(ASTXT(IW),-IC*7),M7)                           
      RETURN                                                            
      END                                                               
