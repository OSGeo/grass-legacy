      SUBROUTINE PUTT(ASTXT,POS,ASCHR)                                  
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C                                                                       
C     PUT THE ASCII-CHAR INTO ASTXT AT POS                              
C                                                                       
      INTEGER ASTXT(1), POS, ASCHR                                      
C                                                                       
      PARAMETER (MM=2**(ZCW*7)-1)                                       
      PARAMETER (MC=2**7-1)                                             
C                                                                       
      IW = (POS+ZCW-1)/ZCW                                              
      IC = (IW*ZCW) - POS                                               
CCCC  WRITE (6,1000) IW,IC                                              
C1000  FORMAT (' IW,IC ',2I5)                                           
      ASTXT(IW) = IOR( IAND(ASTXT(IW),MM-ISHFT(MC,IC*7)),               
     1   ISHFT(IAND(ASCHR,MC),IC*7))                                    
      RETURN                                                            
      END                                                               
