c     program msed2(input,output,tape3,tape4,tape5,tape6,tape7,tape8)   
      dimension qarray(200,70),sdaray(10,70),qt(70)                     
      data iread,iwrit1,iwrit2/7,3,4/                                   
      open(7,file='tape7')
      open(8,file='tape8')
      open(3,file='tape3')
      open(4,file='tape4')
      open(13,file='tape13')
      open(14,file='tape14')
      rewind 7
      rewind 8
      rewind 3
      rewind 4
      rewind 13
      rewind 14
   10 continue                                                          
      read(iread,100) nu,num1                                           
  100 format(2i10)                                                      
      num=num1-1                                                        
      do 105 i=1,nu                                                     
        read(iread,101)nsed                                             
  101 format(10x,i10)                                                   
          read(iread,102)(sdaray(j,i),j=1,nsed)                         
  102 format(4g20.10)                                                   
          write(iwrit2,102)(sdaray(j,i),j=1,nsed)                       
          read(iread,102)qt(i)                                          
          read(iread,102)(qarray(j,i),j=1,num)                          
  105 continue                                                          
      write(iwrit1,102)(qt(i),i=1,nu)                                   
      do 110 j=1,num                                                    
         write(iwrit1,102)(qarray(j,i),i=1,nu)                          
  110 continue                                                          
      if(iread.eq.8) go to 200                                          
      iread=8                                                           
      iwrit1=14                                                         
      iwrit2=13                                                         
      go to 10                                                          
  200 continue                                                          
      close(7)
      close(8)
      close(3)
      close(4)
      close(13)
      close(14)
      end                                                               
