      dimension qdum(200),itype(75),title(20),iseg(75),yield(10,3),     
     +tyield(3)                                                         
     +,iprint(75)                                                       
      common/sed/dcoeff,dpow,dof,chdof,taock,nsed,psi(10),dmbi(10)      
c      ---  initialize variables.                                       
      open(1,file='tape1')
      open(2,file='tape2')
      open(7,file='tape7')
      open(8,file='tape8')
      open(15,file='tape15')
      rewind 1
      rewind 2
      rewind 7
      rewind 8
      rewind 15
      tup = 0.0                                                         
      tin = 0.0                                                         
      tout = 0.0                                                        
      trvol=0.0                                                         
      tvintr = 0.0                                                      
      tarea = 0.0                                                       
      do 101 i=1,200                                                    
        qdum(i)=0.0                                                     
  101 continue                                                          
      read(2,4000)(title(i),i=1,20)                                     
 4000 format(20a4)                                                      
      print 4001,(title(i),i=1,20)                                      
 4001 format(1h ,' title: ',20a4)                                       
c      ---  read in the time increment and final time for the           
c      ---  hydrograph.                                                 
      read(2,4002)dtim,ftim                                             
 4002 format(2f10.0)                                                    
c                                                                       
c      ---  read number of  planes                                      
c      ---  (npl), number of small watersheds (nws)                     
      read(2,1000)npl,nws                                               
 1000 format(3i10)                                                      
c      ---  read in type array to idenitfy the type of units.           
      nump=npl+nws                                                      
      do 104 i=1,nump                                                   
        read(2,1000)iseg(i),itype(i),iprint(i)                          
  104 continue                                                          
c      --- convert times to seconds                                     
      dtim=dtim*60.                                                     
      ftim=ftim*60.                                                     
c      ---  calculate number of increments in hydrograph.               
      num=ifix(ftim/dtim)+1                                             
      if (nump.ne.1) go to 10                                           
c      ---  if no channels need to be routed then use only              
c      ---  the small watershed model (anawat).                         
      call anawat(1,dtim,ftim,qdum,tvintr,tarea,trvol,yield,tyield,     
     +qt,num)                                                           
c      ---  convert totals from cfs to ac-ft units.                     
      tvintr=tvintr/43560.                                              
      tarea=tarea/43560.                                                
      trvol=trvol/43560.                                                
      call out(iseg(k1),yield(1,3),qdum,num,qt,tarea,tvintr,nsed,dtim,  
     +dmbi,tyield(3),trvol)                                             
      close(1)
      close(2)
      close(7)
      close(8)
      stop                                                              
c                                                                       
   10 continue                                                          
c                                                                       
      write(8,6000)nws,num                                              
      write(7,6000)npl,num                                              
 6000 format(6i10)                                                      
c                                                                       
        do 109 k1=1,nump                                                
          do 110 k2=1,num                                               
            qdum(k2)=0.0                                                
  110 continue                                                          
          t1=0.                                                         
          t2=0.                                                         
          t3=0.                                                         
c                                                                       
          k2 = itype(iseg(k1))                                          
          goto (11,12),k2                                               
c      ---  this is the one plane case.                                 
   11 call anawat(iseg(k1),dtim,ftim,qdum,t1,t2,t3,                     
     +yield,tyield,qt,num)                                              
          write(7,6000) iseg(k1),nsed                                   
          write(7,6005)(yield(k3,1),k3=1,nsed)                          
         write(7,6005)qt                                                
          write(7,6005)(qdum(k3),k3=2,num)                              
          if(iprint(iseg(k1)).lt.0) go to 109                           
          t1=t1/43560.                                                  
          t2=t2/43560.                                                  
          t3=t3/43560.                                                  
          call out(iseg(k1),yield,qdum,num,qt,t2,t1,nsed,               
     +dtim,dmbi,tyield,t3)                                              
          go to 109                                                     
c      ---  this is the subwatershed (2-planes ? 1 channel)             
c      ---  case.                                                       
   12 call anawat(iseg(k1),dtim,ftim,qdum,t1,t2,t3,                     
     +yield,tyield,qt,num)                                              
         write(8,6000) iseg(k1),nsed                                    
          write(8,6005)(yield(k3,3),k3=1,nsed)                          
          write(8,6005) qt                                              
          write(8,6005)(qdum(k3),k3=2,num)                              
 6005 format(4g20.10)                                                   
          if(iprint(k1).lt.0) go to 109                                 
          t1=t1/43560.                                                  
          t2=t2/43560.                                                  
          t3=t3/43560.                                                  
          call out(iseg(k1),yield(1,3),qdum,num,qt,t2,t1,nsed,dtim,dmbi,
     +tyield(3),t3)                                                     
  109 continue                                                          
          rewind 8                                                      
         rewind 7                                                       
      close(1)
      close(2)
      close(7)
      close(8)
      end                                                               
c                                                                       
c     **************************************************************    
c                                                                       
      subroutine anawat(ifile,dtim,ftim,qout,tvintr,tarea,trvol,        
     +yield,tyield,qt,num)                                              
c                                                                       
c      ---  parameter definitions                                       
c             ifile  = subunit number.                                  
c             dtim   = time increment for hydrograph.                   
c             ftim   = final time of hydrograph.                        
c             qout   = array of the resulting discharges.               
c             tvintr = total volume of interception in ft**3.           
c             tarea  = total area in ft**2.                             
c             trvol  = total volume of rainfall in ft**3.               
c                                                                       
      dimension plngth(3),slope(3),rainod(200),raint(200),              
     +exces(200),excest(200),rain(200),outime(200),rt(200),             
     +qlt(200),qltime(200),ql(200,2),qout(200)                          
     +,tyield(3),yield(10,3)                                            
      common/cover/grncov(2),cancov(2),vg(2),vc(2),fimp(2),sloose(2)    
      common/depth/y(200)                                               
      common/sed/dcoeff,dpow,dof,chdof,taock,nsed,psi(10),dmbi(10)      
c      --- initialize variables.                                        
      check=0.0                                                         
      trvol=0.0                                                         
      tarea = 0.0                                                       
      tvintr = 0.0                                                      
      icheck = 0                                                        
      err = 0.0                                                         
      tyield(1)=0.0                                                     
      tyield(2)=0.0                                                     
      tyield(3)=0.0                                                     
      do 101 i=1,200                                                    
        ql(i,1)=0.0                                                     
        ql(i,2)=0.0                                                     
        qout(i) = 0.0                                                   
        qlt(i) = 0.0                                                    
        qltime(i) = 0.0                                                 
      y(i)=0.0                                                          
  101 continue                                                          
      do 313 i=1,10                                                     
      yield(i,1)=0.0                                                    
      yield(i,2)=0.0                                                    
      yield(i,3)=0.0                                                    
  313 continue                                                          
c      ---  input all data for anawat from tape1.                       
      call input(iplane,rainod,raint,nrain,plngth,slope,                
     +t,adw,c,xn,a1,b1)                                                 
      nr=nrain                                                          
c      ---  calculate viscosity and correct hydraulic conductivity      
c      ---  for temperature.                                            
      call temp(t,visco, iplane)                                        
c      ---  calculate total inches of rainfall.                         
      trinch=rainod(1)*raint(1)                                         
      do 100 i=2,nrain                                                  
        trinch=rainod(i)*(raint(i)-raint(i-1))+trinch                   
  100 continue                                                          
c      ---  the following loop calculates the excess of each            
c      ---  plane and routs the excess on each plane.                   
      do 102 i=1,iplane                                                 
       qt = 0.0                                                         
c      --- calculate area of plane (area).                              
        area=plngth(i)*plngth(3)                                        
        tarea=tarea+area                                                
c      ---  calculate rainfall volume.                                  
        trvol=trvol+area*trinch/12.                                     
c      ---  initialize rain array equal to rainod array.                
        nrain=nr                                                        
        do 103 j=1,nrain                                                
          rain(j)=rainod(j)                                             
          rt(j)=raint(j)                                                
  103   continue                                                        
        do 113 j=1,200                                                  
          excest(j)=0.0                                                 
          exces(j)=0.0                                                  
  113 continue                                                          
c      ---  calculate the interception                                  
        call intrcp(err,rain,rt,nrain,vintr,i)                          
        tvintr=tvintr+vintr*area/12.                                    
        if(err.eq.0.0) go to 10                                         
c      --- print known variables to trace possible error.               
        print 992,ifile,i                                               
  992 format(/1h ,'trace back information'/1h ,'unit number= ',i3       
     +,'  i= ',i2/1h ,'(note:  i=1 means left plane, i=2 means '        
     +,'right plane)')                                                  
        err=0.0                                                         
        go to 50                                                        
c      --- calculate the infiltration.                                  
   10 call cutoff(err,exces,excest,nex,rain,rt,nrain,                   
     +  i,ftim)                                                         
        if(err.eq.0.0.or.fimp(i).gt.0.0) go to 20                       
c      ---  print known variables to trace possible error.              
        print 992,ifile,i                                               
        err=0.0                                                         
c      ---  for the case when the total rainfall is completely          
c      ---  intercepted and/or infiltrated.                             
   50 if(iplane.eq.1) return                                            
        icheck=icheck+1                                                 
        if(icheck.eq.2) return                                          
        do 104 it=1,200                                                 
          ql(it,i)=0.0                                                  
  104 continue                                                          
        go to 102                                                       
c      ---  convert units of excess rainfall in/min to ft/sec           
c      ---  and units of time from min. to sec.                         
   20 nnex=nex-1                                                        
         do 105 j=2,nnex                                                
      exces(j)=(exces(j)*(1.-fimp(i))+rain(j-1)*fimp(i))/720.           
          excest(j)=excest(j)*60.                                       
c     print 666,exces(j),rain(j-1),fimp(i)                              
  666 format(3g20.10)                                                   
  105 continue                                                          
      exces(nex)=exces(nex)*(1.-fimp(i))/720.                           
      excest(nex)=excest(nex)*60.                                       
c      ---  define b. b is used in the equation q=a*y**b.               
c      ---  b=3.0 for the case of overland flow.                        
        b=3.0                                                           
c      ---  define a. a is used in the equation q=a*y**b.               
c      ---  for overland flow a is a function of the ground             
c      ---  cover, slope, and viscosity of water.                       
        r=100.+(adw-100.)*(grncov(i)/100.)**2.                          
        g=32.174                                                        
        a=8.*slope(i)*g/(r*visco)                                       
c      ---  call the forward routing procedure.                         
        call forwrd(a,b,plngth(i),exces,excest,nex,outime,tstop,ftim    
     +,check)                                                           
c     write(6,200)i                                                     
  200 format(' i= ',i10)                                                
        if(iplane.eq.2) go to 60                                        
        call back(a,b,plngth(i),dtim,tstop,outime,excest,exces,         
     +  qout,nex,check)                                                 
      call plane(slope(i),plngth(i),raint,rainod,r,i,qout,yield(1,1),   
     +tyield(1),dtim,ftim,visco,nr)                                     
      qt=0.0                                                            
      do 201 jj=2,num                                                   
         qt=qt+(qout(jj)+qout(jj-1))/2.                                 
  201 continue                                                          
      qt=qt*plngth(3)*dtim/43560.                                       
      do 202 jj=1,nsed                                                  
          yield(jj,1)=yield(jj,1)/plngth(3)                             
  202 continue                                                          
        return                                                          
   60 call back(a,b,plngth(i),dtim,tstop,outime,excest,                 
     +  exces,ql(1,i),nex,check)                                        
        call plane(slope(i),plngth,raint,rainod,r,i,ql(1,i),yield(1,i)  
     +,                                                                 
     +   tyield(i),dtim,ftim,visco,nr)                                  
  102 continue                                                          
c      ---  to rout the channel call qlat to sum the lateral            
c      ---  inflows.                                                    
       nq=ifix(ftim/dtim)                                               
      call qlat(ql,qlt,qltime,dtim,nq)                                  
      if(a1.ne.0.0) go to 215                                           
c      ---  define b.  b is used in the equation q=a*y**b.              
c      ---  for a channel with a triangular cross section               
c      ---  b=1.25.                                                     
      b1=0.5                                                            
c      ---  define a.  a is used in the equation q=a*y**b.              
c      ---  for a channel a is a function of the channel                
c      ---  geometry (a1), channel slope (slope(3)), and the            
c      ---  darcey-weisbach resistance factor (resist).                 
      a1 = ((2./((1./slope(1))+(1./slope(2))))**0.5)*                   
     +((1.+(1./slope(1)**2))**0.5+(1.+(1./slope(2)**2))**0.5)           
      b2 = 0.5
      a2 = sqrt(2./slope(1)+2./slope(2))
  215 continue                                                          
      call resist(c,xn,a1,b1,a,b,slope(3))                              
c      ---  rout channel.                                               
       check=1.                                                         
      call forwrd(a,b,plngth(3),qlt,qltime,nq,outime,tstop,ftim         
     +,check)                                                           
      nz=3                                                              
c     write(6,200)nz                                                    
c     write(6,500)dtim,tstop                                            
  500 format(' dtim=',f10.2,/ 'tstop= ',f10.2)                          
c     write(6,505)(outime(i),qltime(i),qlt(i),i=1,nq)                   
  505 format(2(f15.0,5x),f10.6)                                         
      call back(a,b,plngth(3),dtim,tstop,outime,qltime,qlt,             
     +qout,nq,check)                                                    
      call chnl(slope,yield,tyield(3),visco,dtim,                       
     +ftim,qout,a1,b1,a2,b2,plngth)                                           
      qt=0.0                                                            
      do 205 jj=2,num                                                   
      qt =qt+(qout(jj-1)+qout(jj))/2.                                   
  205 continue                                                          
      qt=qt*dtim/43560.                                                 
  109 continue                                                          
      return                                                            
      end                                                               
c                                                                       
c     **************************************************************    
c                                                                       
      subroutine input(iplane,rainod,raint,nrain,                       
     +plngth,slope,t,adw,c,xn,a1,b1)                                    
c      ---  this subroutine reads watershed data for the                
c      ---  subroutine anawat.  data is read from the file              
c      ---  with unit number = 1.                                       
c      ---  parameter definitions.                                      
c             iplane = indicator of whether 1-plane or 2-planes         
c                      and 1 channel is in this particular water-       
c                      shed.                                            
c             rainod=array of rainfall intensities for the storm.       
c             raint  = array of final times corresponding to the        
c                      rainfall intensities.                            
c             nrain  = number of rainfall increments (intensities).     
c             plngth= array of plane ? channel lengths.                 
c             slope  = array of plane? channel slopes.                  
c             t      = temperature of water.                            
c                                                                       
      dimension rainod(200),raint(200),plngth(3),slope(3)               
     +,p(11),d(11),dpres(3),pimp(2)                                     
      common/cover/grncov(2),cancov(2),vg(2),vc(2),fimp(2),sloose(2)    
      common/soil/wet k(2),poros(2),save(2),sw(2),si(2),plasi(3),cohm(3)
      common/sed/dcoeff,dpow,dof,chdof,taock,nsed,psi(10),dmbi(10)      
      common/var1/titl                                                  
      dimension titl(3)                                                 
c      ---  read number of planes.                                      
c                                                                       
c   ************ note -- the format numbered 1000 is not the same as    
c                        the documentation states.  as it now stands,   
c                        the column for iplane must be moved 2 spaces   
c                        to the left.       jpd                         
      read(1,1000)(titl(i),i=1,3),iplane                                
 1000 format(//3a4,i10)                                                 
c      ---  for each plane read hydraulic conductivity,                 
c      ---  soil porosity, initial soil saturation, final               
c      ---  soil saturation and average suction                         
      do 100 i=1,2                                                      
        read(1,2000)wet k(i),poros(i),si(i),sw(i),save(i),plasi(i),     
     +  cohm(i)                                                         
 2000 format(7f10.0)                                                    
c      ---  change hydraulic conductivity to units of in/min.           
        wet k(i)= wet k(i)/60.                                          
  100 continue                                                          
c      ---  read in canopy and ground cover data for each plane.        
      read(1,3000)(cancov(i),vc(i),grncov(i),vg(i),                     
     + pimp(i),sloose(i),i=1,2)                                         
      fimp(1)=pimp(1)/100.                                              
      fimp(2)=pimp(2)/100.                                              
 3000 format(6f10.0)                                                    
c      ---  read in slope and length of each plane.                     
      read(1,4000)(slope(i),plngth(i),dpres(i),i=1,2)                   
      read(1,4000) slope(3),plngth(3)                                   
 4000 format(3f10.0)                                                    
      plngth(1)=plngth(1)*(1.-dpres(1))                                 
      plngth(2)=plngth(2)*(1.-dpres(2))                                 
c                                                                       
      read(1,5000)nrain,t,xn,c,a1,b1,adw,cohm(3)                        
 5000 format(i10,7f10.0)                                                
c      ---  read in storm rainfall data.                                
      do 200 i=1,nrain                                                  
        read(1,6000) rainod(i),raint(i)                                 
 6000 format(2f10.0)                                                    
c      ---  convert intensity from in/hr to in/min.                     
        rainod(i)=rainod(i)/60.                                         
  200 continue                                                          
      read(1,1010)dcoeff,dpow,dof,chdof,taock,plasi(3),nsed             
 1010 format(6f10.0,i10)                                                
      do 210 i=1,nsed                                                   
        read(1,6000)d(i),p(i)                                           
  210 continue                                                          
      nsed=nsed-1                                                       
      do 37 i=1,nsed                                                    
        psi(i)=abs(p(i+1)-p(i))                                         
        dmbi(i)=(d(i+1)*d(i))**.5/304.8                                 
   37 continue                                                          
      return                                                            
      end                                                               
c                                                                       
c     **************************************************************    
c                                                                       
      subroutine temp(t,visco,iplane)                                   
c      ---  this subroutine corrects the viscosity and                  
c      ---  hydraulic conductivity for temperature variations           
c      ---  from the assumed temperature of 68 degrees (f).             
c      ---  parameter defintions.                                       
c             t      = temperature in degrees f.                        
c             visco  = kinematic viscosity (ft**2/sec)                  
c             iplane = number of planes.                                
c                                                                       
      common/soil/wet k(2),poros(2),save(2),sw(2),si(2),plasi(3),cohm(3)
      dimension te(10),v(10)                                            
      data te/32.,40.,50.,60.,68.,80.,90.,100.,120.,140./,              
     +v/1.93,1.66,1.41,1.22,1.09,0.930,0.826,0.739,0.609,               
     +0.514/                                                            
c      ---  calculate new viscosity by interpolation.                   
      do 100 i=1,10                                                     
        if(te(i).lt.t) go to 100                                        
        fac1=(t-te(i-1))/(te(i)-te(i-1))                                
        visco=v(i-1)+fac1*(v(i)-v(i-1))                                 
        go to 10                                                        
  100 continue                                                          
c      ---  adjust the hydraulic conductivity.                          
   10 fac2=visco/1.09                                                   
      do 101 j=1,iplane                                                 
        wet k(j)=wet k(j)/fac2                                          
  101 continue                                                          
      visco=visco*.00001                                                
      return                                                            
      end                                                               
      subroutine intrcp(err,rain,raint,nrain,vintr,ipl)                 
c      ---  this subroutine determines the volume of                    
c      ---  intercepted rainfall. interception depends                  
c      ---  on the percentage of the ground that is                     
c      ---  covered by (cancov) and ground (grncov), and                
c      ---  their respective water holding capacities(vc,vg).           
c      ---  total intercepted volume = vintr.                           
c      ---  parameter definitions.                                      
c             err    = error index.                                     
c             rain   = array of rainfall intensities.                   
c             raint  = array of final times for each rainfall           
c                      intensity.                                       
c             nrain  = number of rainfall intensities.                  
c             vintr  = total amount of rainfall intercepted in inches.  
c             ipl    = indicator of which plane the excess is           
c                      being calculated.                                
c                                                                       
      dimension r old(200),rain(nrain),raint(nrain),rtold(200)          
      common/cover/grncov(2),cancov(2),vg(2),vc(2),fimp(2),sloose(2)    
c      ---  the r old is an array which temporarily stores the          
c      ---  storm intensities. operations are done on the r old         
c      ---  array to obtain an array which equals the total             
c      ---  rainfall minus the rainfall that is lost by                 
c      ---  interception.                                               
c      ---  initialize r old array equal to rain array.                 
      do 100 i=1,nrain                                                  
        r old(i)= rain(i)                                               
        rtold(i)=raint(i)                                               
  100 continue                                                          
c      ---  calculate total intercepted volume of rainfall.             
      vgc=grncov(ipl)*vg(ipl)/100.                                      
      vcc=cancov(ipl)*vc(ipl)/100.                                      
      vintr =vgc+vcc                                                    
      vrain = 0.                                                        
c      ---  the rainfall lost to interception is subtracted             
c      ---  from the total rainfall.                                    
      do 101 i=1,nrain                                                  
        if(i.eq.1) go to 10                                             
        vrain =vrain+r old(i)*(raint(i)-raint(i-1))                     
        go to 11                                                        
   10 vrain = vrain+r old(1)*raint(1)                                   
   11 if(vrain.gt.vintr) go to 12                                       
        rain(i) = 0.                                                    
  101 continue                                                          
      go to 13                                                          
   12 drv =vrain-vintr                                                  
      dt=drv/rain(i)                                                    
      if(i.eq.1) go to 14                                               
      raint(i-1)=raint(i)-dt                                            
      return                                                            
   14 rain(1)=0.0                                                       
      raint(1)=raint(1)-dt                                              
      go to 15                                                          
c      ---  print warning if total rainfall is less than                
c      ---  total intercepted volume of rainfall.                       
   13 err=1.0                                                           
      print 1000,vrain                                                  
 1000 format(//' the entire volume of rainfall ',f10.3,                 
     +' inches,'/' has been absorbed by interception.'/)                
      go to 16                                                          
c      ---  reset rain array equal to r old array.                      
   15 continue                                                          
      nrain=nrain+1                                                     
       do 102 i=2,nrain                                                 
        rain(i)=r old(i-1)                                              
        raint(i)=rtold(i-1)                                             
  102 continue                                                          
   16 return                                                            
      end                                                               
c                                                                       
c     ****************************************************************  
c                                                                       
      subroutine cutoff(err,exces,excest,nex,rain,raint,nrain,          
     +ipl,ftim)                                                         
c      ---  this subroutine calculates the excess rainfall ?            
c      ---  infiltrated rainfall based on the green-ampt equation.      
c      ---  this is a continued infiltration model ( i.e. infiltration  
c      ---  continues after the end of the rainfall).                   
c                                                                       
c      ---  parameter definitions.                                      
c             err    = error index.                                     
c             exces  = array contianing excess rainfall                 
c                      intensity values.                                
c             excest = array contianing the final times for             
c                      each excess interval.                            
c             nex    = number of excess intervals.                      
c             rain   = array of rainfall intensities.                   
c             raint  = array of final times for each rainfall           
c                      intensity.                                       
c             nrain  = number of rainfall intervals.                    
c             ipl    = indicator of which plane the excess              
c                      is being calculated.                             
c             ftim   = final time of hydrograph.                        
c                                                                       
      dimension rain(nrain),raint(nrain),exces(200),excest(200)         
      common/soil/wet k(2),poros(2),save(2),sw(2),si(2),plasi(3),cohm(3)
      common/cover/grncov(2),cancov(2),vg(2),vc(2),fimp(2),sloose(2)    
c      ---  initialize variables.                                       
      itp=0                                                             
      fo1 = 0.                                                          
      fo2 = 0.                                                          
      t = 0.                                                            
      ftimm=ftim/60.                                                    
      exces(1) = 0.                                                     
      excest(1) = 0.                                                    
      gamma=save(ipl)*poros(ipl)*(sw(ipl)-si(ipl))                      
c      ---  calculate number of excess increments.                      
      nex=nrain+1                                                       
c      ---  the following loop iterates excess increments.              
      do 105 i=1,nrain                                                  
c      ---  calculate the rainfall time interval--dtm.                  
        if(i.eq.1) go to 11                                             
        dtm=raint(i)-raint(i-1)                                         
        go to 12                                                        
   11 dtm=raint(1)                                                      
   12 t=t+dtm                                                           
c      ---  calculate the potential infiltrated volume.                 
        delf = df(fo1,wet k(ipl),gamma,dtm)                             
c      ---  compute the potential average infiltration rate.            
        fd = delf/dtm                                                   
        r=rain(i)                                                       
c      ---  compare the rainfall intensity and the average              
c      ---  potential infiltration rate. if the rainfall                
c      ---  intensity is greater than the infiltraion rate              
c      ---  then calculate excess. if the rainfall intensity is         
c      ---  less than or equal to the infiltration rate then            
c      ---  the excess is zero.                                         
        if(fd.le.r) go to 10                                            
        fd1s =r                                                         
        exces(i+1)= 0.0                                                 
        excest(i+1) =t                                                  
        go to 25                                                        
   10 itp=1                                                             
        fd1s=fd                                                         
        exces(i+1)=r-fd1s                                               
        excest(i+1)=t                                                   
   25 fo1 = fo1+fd1s*dtm                                                
        fo2=fo1                                                         
  105 continue                                                          
      excest(nex+1)=1.e20                                               
      exces(nex+1)=-wet k(ipl)                                          
      nex=nex+1                                                         
c      ---  print warning if there is no excess.                        
      if(fimp(ipl).gt.0.0.or.itp.ne.0) return                           
   17 print 10000                                                       
10000 format(/' cutoff finds no rainfall excess.'/                      
     +' no routing will be attempted. control returned to'/             
     +' anawat.'/)                                                      
      err=2.0                                                           
      return                                                            
      end                                                               
c                                                                       
c     ****************************************************************  
c                                                                       
      function df(f,wk,head,dt)                                         
c      ---  explicit approximation for green-ampt infiltration          
c      ---  model determines the potential infiltration                 
c      ---  volume during time increment.                               
c                                                                       
c      ---  parameter definitions.                                      
c             f      = accumulated infiltrated volume in inches.        
c             wk     = hydraulic conductivity.                          
c             head   = av. suction*porosity*(final-initial saturation)  
c             dt     = time increment in minutes.                       
c                                                                       
      a=1.                                                              
      b=2.*f-wk*dt                                                      
      c=2.*wk*(head+f)*dt                                               
      df=(-b+sqrt(b*b+4.*a*c))/(2.*a)                                   
      return                                                            
      end                                                               
c                                                                       
c     ****************************************************************  
      subroutine integr(t1,t2,ans,qin,timein,n)                         
c      ---  subroutine integr integrtes the excess                      
c      ---  rainfall (or inflow in the case of a channel)               
c      ---  histogram between an initial time (t1)                      
c      ---  and a final time (t2).                                      
c                                                                       
c      ---  parameter definitions.                                      
c             t1     = initial time of characteristic.                  
c             t2     = final time of characteristic.                    
c             ans    = area between t1 and t2 of in array.              
c             qin    = array being integrated.                          
c             timein = array of final times corresponding to the        
c                      qin array.                                       
c             n      = number of elements in qin and timein arrays.     
c                                                                       
      dimension qin(200),timein(200)                                    
c      ---  initialize dummy variable which is used                     
c      ---  to store intermediate answers.                              
c     print 500                                                         
  500 format(/)                                                         
      cum =0.                                                           
c      ---  find initial time increment.                                
      do 100 i=2,n                                                      
      ii=i                                                              
        if(t1.lt.timein(i)) go to 55                                    
  100 continue                                                          
   55 int=ii                                                            
c      ---  find final time increment.                                  
      do 101 i=int,n                                                    
        if(t2.ge.timein(i)) go to 101                                   
        ifin=i                                                          
        go to 40                                                        
  101 continue                                                          
      ifin=n                                                            
c      ---  find area inbetween initial and final                       
c      ---  time increments.                                            
c     40 print 501,int,ifin,t1,t2                                       
   40 continue                                                          
  501 format(2i5,2g15.7)                                                
       do 102 j=int,ifin                                                
      cumpre=cum                                                        
        cum=cum+(timein(j)-timein(j-1))*qin(j)                          
      if(j.eq.ifin) cum=cumpre+qin(j)*(t2-timein(j-1))                  
c     print 502,cum,timein(j),timein(j-1),qin(j),j                      
  502 format(4g15.7,i5)                                                 
  102 continue                                                          
c      ---  correct intermediate answer by subtracting                  
c      ---  the areas which should not be included.                     
      if(t1.eq.0.0) go to 80                                            
      cum=cum-(t1-timein(int-1))*qin(int)                               
c     print 503,cum,-1                                                  
  503 format(g20.10,i5)                                                 
c     80 print 503,cum,-2                                               
   80 continue                                                          
c      ---  set value of final answer.                                  
       ans=cum                                                          
      return                                                            
      end                                                               
c                                                                       
c     ****************************************************************  
c                                                                       
      subroutine forwrd(a,b,d,qin,timein,n,outime,tstop,ftim            
     +,check)                                                           
      dimension qin(200),timein(200),outime(200)                        
c     print 510                                                         
  510 format(//)                                                        
      do 10 k=1,200                                                     
         outime(k)=0.0                                                  
   10 continue                                                          
      ifinal=n-1                                                        
      iifinl=ifinal-1                                                   
      do 200 k=1,iifinl                                                 
         kk=k                                                           
         xpre=0.                                                        
         x=0.                                                           
         depth=0.                                                       
         dx=0.                                                          
         do 100 i=k,ifinal                                              
            dt=timein(i+1)-timein(i)                                    
            if(qin(i+1).eq.0.0) go to 50                                
            if(i.eq.ifinal) go to 80                                    
            depth=depth+qin(i+1)*dt                                     
            dx=(a/qin(i+1))*(depth**b-(depth-qin(i+1)*dt)**b)           
            xpre=x                                                      
      x=x+dx                                                            
c     print 500,x,-2.                                                   
      go to 60                                                          
   50    dx=a*b*(depth**(b-1.))*dt                                      
            xpre=x                                                      
            x=x+dx                                                      
   60    if(x.lt.d) go to 100                                           
            x=xpre                                                      
            depth=depth-qin(i+1)*dt                                     
            if(qin(i+1).eq.0.0) go to 70                                
            delt=(((d-x)*qin(i+1)/a+depth**b)**(1./b)-depth)/qin(i+1)   
            outime(k)=delt+timein(i)                                    
            if(outime(k).lt.ftim) go to 200                             
            tstop=ftim                                                  
            return                                                      
   70    outime(k)=(d-x)/(a*b*(depth**(b-1.)))+timein(i)                
            if(outime(k).lt.ftim) go to 200                             
            tstop=ftim                                                  
            return                                                      
   80    xmax=x-(a*depth**b)/qin(i+1)                                   
c     print 500,xmax,x,d                                                
  500 format(3g20.10,2i5)                                               
c     print 500,depth,qin(i+1),-3.                                      
            if(xmax.lt.d) go to 300                                     
            delt=(((d-x)*qin(i+1)/a+depth**b)**(1./b)-depth)/qin(i+1)   
            outime(k)=timein(i)+delt                                    
            if(outime(k).lt.ftim) go to 200                             
            tstop=ftim                                                  
            return                                                      
  100 continue                                                          
      if(check.eq.0.0.and.qin(n).lt.0.0) go to 200                      
      if(depth.eq.0.0) go to 199                                        
      outime(k)=timein(ifinal)+(d-x)/(a*b*(depth**(b-1.)))              
      if(outime(k).lt.ftim) go to 200                                   
      tstop=ftim                                                        
      return                                                            
  199 outime(k)=1.e30                                                   
      tstop=ftim                                                        
      return                                                            
  200 continue                                                          
      if(check.eq.1.0) go to 310                                        
      if(qin(n).eq.0.0) go to 310                                       
      kk=ifinal                                                         
  300 continue                                                          
      call stop(a,b,d,qin,timein,n,outime,tstop,ftim,kk,check)          
      return                                                            
  310 outime(ifinal)=1.e30                                              
      tstop=ftim                                                        
      return                                                            
      end                                                               
      subroutine stop(a,b,d,qin,timein,n,outime,tstop,ftim,k,check)     
      dimension qin(200),timein(200),outime(200)                        
c     print 1000,(qin(i),timein(i),i=1,n)                               
 1000 format(3g20.10)                                                   
      ifinal=n-1                                                        
      int=k-1                                                           
      if(k.gt.1) go to 200                                              
      depth=0.0                                                         
      do 100 i=2,ifinal                                                 
         depth=depth+qin(i)*(timein(i)-timein(i-1))                     
  100 continue                                                          
      outime(1)=timein(ifinal)-depth/qin(n)                             
      tstop=outime(1)                                                   
      if(outime(1).gt.ftim) tstop=ftim                                  
c     print 1000,outime(1),tstop                                        
      return                                                            
  200 continue                                                          
      dtt=(timein(k)-timein(int))/2.                                    
      time1=timein(int)+dtt                                             
      iifinl=ifinal-1                                                   
      if(check.eq.1.0) iifinl=ifinal                                    
  300 continue                                                          
      depth=0.                                                          
      x=0.                                                              
      do 400 i=int,iifinl                                               
         ii=i                                                           
         if(i.ne.int) go to 410                                         
         dt=timein(k)-time1                                             
         go to 420                                                      
  410 dt=timein(i+1)-timein(i)                                          
  420 if(qin(i+1).eq.0.0) go to 430                                     
         depth=depth+qin(i+1)*dt                                        
         dx=(a/qin(i+1))*(depth**b-(depth-qin(i+1)*dt)**b)              
         x=x+dx                                                         
         if(x.ge.d) go to 435                                           
         go to 400                                                      
  430 dx=a*b*(depth**(b-1.))*dt                                         
         x=x+dx                                                         
         if(x.ge.d) go to 435                                           
  400 continue                                                          
      if(check.eq.1.0) go to 435                                        
      xmax=x-(a*depth**b)/qin(n)                                        
      if(xmax.lt.d) go to 440                                           
      con=(xmax-d)/d                                                    
      if(con.lt.0.05) go to 500                                         
  435 dtt=dtt/2.                                                        
      time1=time1+dtt                                                   
c     print 1000,time1,x                                                
      if(check.eq.0.0) go to 300                                        
      depth=depth-qin(ii+1)*dt                                          
      if(qin(ii+1).eq.0.0) go to 438                                    
      delt=(((d-x)*qin(ii+1)/a+depth**b)**(1./b)-depth)/qin(ii+1)       
      go to 439                                                         
  438 outime(k)=(d-x)/(a*b*(depth**(b-1.)))+timein(ii)                  
      outime(k)=timein(ii)+delt                                         
c     print 1000,outime(k),timein(ii),depth                             
  439 if(outime(k).lt.ftim) go to 300                                   
      tstop=ftim                                                        
      go to 550                                                         
  440 dtt=dtt/2.                                                        
      time1=time1-dtt                                                   
c     print 1000,x,time1                                                
      go to 300                                                         
  500 delt=(((d-x)*qin(n)/a+depth**b)**(1./b)-depth)/qin(n)             
      outime(k)=timein(ii+1)+delt                                       
      if(ii.eq.int)outime(k)=time1+delt                                 
      tstop=outime(k)                                                   
      if(tstop.gt.ftim)tstop=ftim                                       
  550 dumq=qin(k)                                                       
      n=n+1                                                             
      do 600 i=k,n                                                      
      dum1=timein(i)                                                    
         timein(i)=time1                                                
         time1=dum1                                                     
         dum2=qin(i)                                                    
         qin(i)=dumq                                                    
         dumq=dum2                                                      
  600 continue                                                          
c     print 1000,(timein(i),qin(i),outime(i),i=1,n)                     
      return                                                            
      end                                                               
            subroutine back (al,bet,len,dtim,tstop,tl,t,q,qout,nq,check)
c            this subroutine uses the subdivided solution domain        
c            as supplied by subroutine forwrd to calculate the          
c            time of origin of characteristics corresponding to         
c            an arbitrarily selected time on the downstream             
c            boundary.  in short, back calculates characteristics       
c            in the upstream direction.  this allows the discharge      
c            to be known at convenient time intervals thereby           
c            facilitating the formation of the lateral inflow to        
c            the channel and allowing greater ease in interfacing       
c            this water routing simulation with other watershed         
c            process models.                                            
c            subroutine back proceeds as follows:                       
c            it first determines the location of the time of interest   
c            on the downstream boundary with respect to the             
c            charateristics supplied by subroutine forwrd.  next,       
c            the routine formulates f(to) and its first two             
c            derivatives as shown in eqs 3-15 to 3-17 in the            
c            anawat report text.  finally, this result is iterated      
c            until a suitably accurate estimate of the time of          
c            origin is calculate using a second order newtons method.   
c                                                                       
c            parameter definitions                                      
c              al     = a in q=a*y**b.                                  
c              bet    = b in q=a*y**b.                                  
c              len    = slope length (ft).                              
c              dtim   = time increment for hydrograph (sec).            
c              ftim   = ending time of hydrograph (sec).                
c              tl     = array of final times for the characteristic     
c                       lines found in forwrd subroutine (sec).         
c              t      = time array for inflow (sec).                    
c              q      = inflow array (ft/sec or cfs/ft).                
c              qout   = outflow discharges (cfs/ft or cfs).             
c              nq     = number of increments in inflow.                 
c              tstop  = time when runoff stops.                         
c                                                                       
            dimension             tl(200)     , t(200)      , q(200)    
     +  ,                                                               
     +              cumq(200)   , qout(200)                             
            common /depth/        y(200)                                
            real                  len                                   
c            one time initialization of loop parameters and times.      
            epq = 1.e - 5                                               
            if (check.eq.1.) epq = 1.e - 5 * len                        
            niter = 10                                                  
            ep = 0.0001                                                 
            time = 0.                                                   
            b1 = bet - 1.                                               
            b2 = bet - 2.                                               
            b3 = bet - 3.                                               
            y(1) = 0.0                                                  
c     print 500,al,bet,tstop                                            
c     print 501,(t(i),tl(i),q(i),i,i=1,nq)                              
  501 format(3g20.10,i5)                                                
c            calculate cumlative inflow array.                          
            tot = q(1) * t(1)                                           
            cumq(1) = tot                                               
      test=0.0                                                          
            do 100 i = 2,200                                            
               tot = tot + q(i) * (t(i) - t(i - 1))                     
               cumq(i) = tot                                            
               y(i) = 0.0                                               
  100 continue                                                          
            i = 1                                                       
  110 if (tl(i).ne.tl(i + 1)) go to 120                                 
            i = i + 1                                                   
            go to 110                                                   
c            this loop iterates final times from 0.0 to ftim. for       
c            each final time an initial time (test) is calculated.      
  120 do 300 j = 2,200                                                  
      tpre=test                                                         
               time = time + dtim                                       
c     print 500,time,tstop                                              
  500 format(3g20.10,2i5)                                               
               if (time.gt.tstop) go to 310                             
c            this section is used to calculate the discharge for        
c            all times less than tl(1).  for these times it is not      
c            necessary go calculate the upstream time of the characteris
c            since all of these characteristics begin at t=0.           
c     print 500,time,tl(1),tstop                                        
               if (time.gt.tl(1)) go to 130                             
               call integr (0.0,time,depth,q,t,nq)                      
               qout(j) = al * depth *  * bet                            
               y(j) = depth                                             
               go to 300                                                
  130    ik = i                                                         
c            find the bounds for the characteristic line.               
               do 140 ij = ik,200                                       
                  if (time.le.tl(ij + 1)) go to 150                     
                  i = i + 1                                             
  140    continue                                                       
  150    l2 = i                                                         
c            if the lateral inflow intensity is equal to zero,          
c            it is necessary to skip up to the next characteristic.     
               if (q(l2 + 1).ne.0.) go to 160                           
               i = i + 1                                                
               go to 150                                                
  160    call nindex(time,l,nq,t)                                       
               m = l - l2 - 1                                           
c            the first guess of the time of origin to be calculated     
c            (test) is the average of the time of origin of             
c            the lower bounding characteristic, and the smaller of      
c            the time origin of the upper bounding characteristic       
c            and time.                                                
               tup = t(i + 1)                                           
      tdn=t(i)                                                          
      if(tpre.gt.tdn)tdn=tpre                                           
               if (tup.gt.time) tup = time                              
               test = (tdn + tup)/2.                                    
c            this is an analytical solution using a third order         
c            newton approximation to solve for the characteristic line. 
               do 281 j1 = 1,niter                                      
                  ff =  - len/(al * bet)                                
                  fto = ff                                              
                  tlast = test                                          
                  if (m.eq.0) go to 210                                 
                  c1 = q(l2 + 1) * t(l2 + 1) - q(l2 + 1) * test         
                  fto = fto + (1./(q(l2 + 1) * bet)) * (c1 *  * bet)    
                  fdto =  - (c1 *  * b1)                                
                  fd2to = q(l2 + 1) * b1 * (c1 *  * b2)                 
                  if (m.eq.1) go to 190                                 
                  do 180 j2 = 2,m                                       
                     jj = l2 + j2                                       
                     a =  - q(jj) * t(jj - 1) + cumq(jj - 1) - cumq(l2) 
     ++ q(l2                                                            
     +                + 1) * t(l2)                                      
                     if (q(jj).ne.0.) go to 170                         
                     c1=-q(l2+1)*test+a                                 
                     cdt=t(jj)-t(jj-1)                                  
                     fto=fto+cdt*(c1**b1)                               
                     fdto=fdto+b1*(-q(l2+1))*cdt*(c1**b2)               
                     fd2to=fd2to+b1*b2*q(l2+1)*q(l2+1)*cdt*(c1**b3)     
                     go to 180                                          
  170          c1 = q(jj) * t(jj) - q(l2 + 1) * test + a                
                     c2 = q(jj) * t(jj - 1) - q(l2 + 1) * test + a      
                     fto = fto + (1./(bet * q(jj))) * ((c1 *  * bet) - (
     +c2 *                                                              
     +               * bet))                                            
                     fdto = fdto + ( - q(l2 + 1)/q(jj)) * ((c1 *  * b1) 
     +- (c2                                                             
     +               *  * b1))                                          
                     fd2to = fd2to + (q(l2 + 1) * q(l2 + 1)/q(jj)) * b1 
     +* ((c1                                                            
     +                *  * b2) - (c2 *  * b2))                          
  180       continue                                                    
  190       a =  - q(l) * t(l - 1) + cumq(l - 1) - cumq(l2) + q(l2      
     + + 1)                                                             
     +            * t(l2)                                               
                  if (q(l).ne.0.) go to 200                             
                  c1 = a - q(l2 + 1) * test                             
                  cdt = time - t(l - 1)                                 
                  fto = fto + cdt * (c1 *  * b1)                        
                  fdto = fdto + cdt * b1 * ( - q(l2 + 1)) * (c1 *  * b2)
                  fd2to = fd2to + cdt * b1 * b2 * q(l2 + 1) * q(l2 + 1) 
     +* (c1                                                             
     +            *  * b3)                                              
                  go to 220                                             
  200       c1 = q(l) * time - q(l2 + 1) * test + a                     
                  c2 = q(l) * t(l - 1) - q(l2 + 1) * test + a           
                  if (c1.lt.0.0.or.c2.lt.0.0) go to 260                 
                  fto = fto + (1./(q(l) * bet)) * ((c1 *  * bet) - (c2 *
     +  * bet))                                                         
                  fdto = fdto + ( - q(l2 + 1)/(q(l))) * ((c1 *  * b1) - 
     +(c2 *  * b1))                                                     
                  fd2to = fd2to + ((q(l2 + 1) * q(l2 + 1))/q(l)) * b1 * 
     +((c1 *  *b2) - (c2 * * b2))                                       
                  go to 220                                             
  210       test = time - ((len/(al * (q(l) *  * b1))) *  * (1./bet))   
                  go to 290                                             
c            test for convergence.  if test is successful, the          
c            iterations are complete.  the program then proceeds to     
c            the next time increment.  if not, the program uses         
c            the solution to truncated  taylor)s series to calculate    
c            a new trail value of test.                                 
  220       if (abs(fto/ff).le.ep) go to 290                            
                  b = (2. * fdto/fd2to) - (2. * test)                   
                  c = (2./fd2to) * (fto - fdto * test + 0.5 * test *    
     +test * fd2to)                                                     
                  d = b * b - 4. * c                                    
                  if (d.lt.0.) go to 230                                
                  dtest = 0.5 * sqrt(d)                                 
                  go to 240                                             
c            the new estimate of test must fall inside the region       
c            of the solution domain bounded by the characteristics      
c            selected above.  if it does not, then an estimate must     
c            be repicked inside that region.  this procedure may        
c            result in the rejection of the estimate provided by        
c            the newton)s method.  and the substitution of a new        
c            new average value as the new guess of test.                
  230       test = test - fto/fdto                                      
c     print 500,test,fto,fdto,1                                         
                  if (test.lt.tup.and.test.ge.tdn) go to 280            
                  go to 250                                             
  240       dtest = 0.5 * (sqrt(b * b - 4. * c))                        
                  if (abs(fto/ff).le.ep) go to 290                      
                  test = 0.5 * ( - b) - dtest                           
c     print 500,test,dtest,b,2                                          
                  if (test.lt.tup.and.test.gt.tdn) go to 241            
                  test = 0.5 * ( - b) + dtest                           
c     print 500,test,dtest,b,3                                          
                  if (test.lt.tup.and.test.ge.tdn) go to 280            
                  test = tlast                                          
c     print 500,test,tlast,b,4                                          
                  go to 230                                             
  241 testb=.5*(-b)+dtest                                               
      if(testb.gt.tup.or.testb.lt.tdn) go to 280                        
      testc=tlast-fto/fdto                                              
      test1=abs(testc-test)                                             
      test2=abs(testc-testb)                                            
      if(test1.gt.test2)test=testb                                      
      if(test.lt.tup.and.test.gt.tdn) go to 280                         
      test=tlast                                                        
      go to 230                                                         
  250       if (test.ge.tup) go to 270                                  
  260       test = (tlast + tdn)/2.                                     
c     print 500,test,tlast,tdn,5                                        
                  go to 280                                             
  270       test = (tlast + tup)/2.                                     
c     280 print 500,fto,fdto,test,j1                                    
  280 continue                                                          
  281    continue                                                       
c            the values of depth (of cross-sectional area for channels) 
c            and discharge are now calculated. a test is made to determi
c            if the discharge is negligibly small or if the preselected 
c            duration of the hydrograph has been exceeded. if so the    
c            routine returns to anawat.                                 
c     290 print 500,fto,ff,test,niter,-1                                
  290 continue                                                          
            call integr (test,time,depth,q,t,nq)                        
               qout(j) = al * depth *  * bet                            
               y(j) = depth                                             
               np = nq                                                  
               if (check.gt.0.) np = nq + 1                             
               if (time.ge.t(nq - 1).and.qout(j).le.epq) go to 310      
  300 continue                                                          
  310 continue                                                          
            return                                                      
            end                                                         
c                                                                       
c     ***************************************************************   
c                                                                       
      subroutine nindex(time,l,nq,t)                                     
c      ---  this subroutine locates which inflow time increment (l)     
c      ---  contians a given time (time).                               
c      ---  parameter definitions.                                      
c             time   = time of interest.                                
c             l      = index of t array contianing the  time.         
c             nq     = number of inflow time increments.                
c             t      = array of inflow time increments (sec).           
c                                                                       
      dimension t(200)                                                  
      do 100 i=1,nq                                                     
        l=i                                                             
        if(time.le.t(i)) go to 10                                       
  100 continue                                                          
      l=nq+1                                                            
   10 return                                                            
      end                                                               
c                                                                       
c     ****************************************************************  
c                                                                       
      subroutine qlat(ql,qlt,qltime,dtim,kf)                            
c      ---  this subroutine used only in the 2 planes and               
c      ---  1 channel case.  it totals the lateral inflow               
c      ---  into the channel.                                           
c                                                                       
c      ---  parameter definitions.                                      
c             ql     = double dimension array containing the            
c                      outflows from each channel.                      
c             qlt    = array of the total lateral inflow.               
c             qltime = array of the times for the total lateral         
c                      inflow (qlt array).                              
c             dtim   = time increment used (sec).                       
c             kf     = number of elements in time array.                
c                                                                       
      dimension ql(200,2),qlt(200),qltime(200)                          
c      ---  caluculate the total average inflow over a given time       
c      ---  period.                                                     
      qlt(1)=0.0                                                        
      qltime(1)=0.0                                                     
      kff=kf+1                                                          
      do 105 i=2,kff                                                    
        qlt(i)=(ql(i,1)+ql(i,2)+ql(i-1,1)+ql(i-1,2))/2.                 
        qltime(i)=qltime(i-1)+dtim                                      
  105 continue                                                          
      kff=kff+1                                                         
      kf=kff                                                            
      qlt(kff)=0.0                                                      
      qltime(kff)=qltime(kff-1)+1.e30                                   
      return                                                            
      end                                                               
           subroutine resist(c,xn,a1,b1,alp,bet,slp)                    
c      ---  this subroutine calculates the parameters a and             
c      ---  b in the equation q=a*area**b.                              
c                                                                       
           if(c.ne.0.0) go to 102                                       
c      ---  this is for the mannings resistance.                        
           bet=(5.-2.*b1)/3.                                            
           alp=((slp*2.21)/(xn*xn*a1**(4./3.)))**.5                     
           return                                                       
c      ---  this is for the chezy resistance.                           
  102 bet=(3.-b1)/2.                                                    
           alp=(slp*c*c/a1)**.5                                         
           return                                                       
           end                                                          
      subroutine chnl(slope,yield,tyield,visco,dtim,                    
     +ftim,q,a1,b1,a2,b2,plngth)                                              
      dimension q(200),yield(10,3),sedlat(10),sedq(10),tsedq(10)        
     +,slope(3),plngth(3)                                               
      common/depth/y(200)                                               
      common/sed/dcoeff,dpow,dof,chdof,taock,nsed,psi(10),dmbi(10)      
      common/soil/wet k(2),poros(2),save(2),sw(2),si(2),plasi(3),cohm(3)
c       this subroutine calculates the amount of sediment generated     
c        in the channel unit of a watershed due to the inflow from      
c       from the planes.                                                
c       **************************************************************  
c       *                                                               
c       *     user written externals:  power                            
c       *                                                               
c       *************************************************************   
c       the total sediment inflow frim the planes is calculated         
c       (tsdlat). also the  inflow for each size fraction is de-        
c       termined(selat).  the total transport capacity for each         
c       size(tsedq) is initialized.                                     
333   format(' entering chnl')                                          
      tsdlat=0.0                                                        
      do 201 ised=1,nsed                                                
        sedlat(ised)=yield(ised,1)+yield(ised,2)                        
        tsdlat=tsdlat+sedlat(ised)                                      
        tsedq(ised)=0.0                                                 
  201 continue                                                          
           ssedq=0.0                                                    
           dcoh=0.0                                                     
           t=0.0                                                        
c       loop 110 calculates the transport capacity for all the size     
c       fractions for each calculated discharge.                        
           do 110 ij=2,200                                              
               t=t+dtim                                                 
               if(t.gt.ftim) go to 121                                  
               if (q(ij).eq.0.0) go to 110                              
c       this section calculates the channel cross-                      
c       sectional area and top width.                                   
               char=y(ij)                                               
      wper=a1*char**b1                                                  
      bper=a2*char**b2
      hyrad=char/wper                                                   
      if(plasi(3).le.0.0)go to 303                                      
      taoo=62.4*slope(3)*hyrad                                          
      tcoh=0.0022*plasi(3)**.82                                         
      erate=cohm(3)*(taoo/tcoh-1.)*wper                                 
      if(erate.lt.0.0)erate=0.0                                         
      dcoh=dcoh+erate                                                   
c       the mean velocity of the channel flow.                          
  303          vmean=q(ij)/char                                         
c                                                                       
               sumps = 0.                                               
       fmax = 1.5/(1.69+alog10(2.*hyrad/dmbi(nsed)))**2
      if (fmax .gt. 0.1) fmax = 0.1
      if (fmax .lt. 0.01) fmax = 0.01
      fmin = 0.5 * fmax
c       the 111 loop calculates the bedload and suspended               
c       load transport for each size fraction.                          
               do 111 ised=1,nsed                                       
                  dmb=dmbi(ised)                                        
                  ps=psi(ised)                                          
c       critical shear stress:                                          
      fdivr=1.69+2.*alog10(2.*hyrad/dmb)                                
      if(fdivr.eq.0.0) fdivr=0.0000001                                  
      f=1.5/(fdivr)**2                                                  
      if(f.gt.fmax)f=fmax                                               
      if(f.lt.fmin) f=fmin                                            
      tao=.2425*f*vmean*vmean                                           
                  taoc=102.96*taock*dmb                                 
c       check to see if excess shear exists                             
                  if (tao.gt.taoc.and.q(ij).gt.0.) go to 106            
                  sedq(ised)=0.                                         
                  susp = 0.                                             
                  go to 108                                             
c       bedload calculation using the meyer-peter,mueller equation      
  106       sedq(ised)=(12.85/1.392)*(tao-taoc)**(1.5)                  
c       shear velocity:                                                 
                  sv=sqrt(62.4*hyrad*slope(3)/1.938)                    
c       fall velocity calculation                                       
                  fvb=(sqrt(36.064*dmb**3.+36.*visco**2.)-6.*visco)/dmb 
                  if (dmb.le.0.0002) fvb=2.9517*dmb**2./visco           
c       calculate parameters for einstiens suspended                    
c       sediment load approximation.                                    
                  zr=fvb/(0.4*sv)                                       
      ar=2.*dmbi(nsed)/hyrad                                            
                  bmv=2.5+vmean/sv                                      
                  if (ar.gt.0.9) go to 107                 
c       evaluate j1 and j2.                                             
                  call power (zr,ar,fj,sj,1.0e-2)                       
                  p=ar**(zr-1.)/(11.6*(1.-ar)**zr)                      
                  susp=p*(bmv*fj+2.5*sj)                                
      supmax=1./ar                                                      
      if(susp.gt.supmax)susp=supmax                                     
                  if (susp.le.0.0) susp=0.0                             
                  go to 108                                             
  107        susp=0.                                                    
  108        continue                                                   
c       calculate the total sediment transport capacity for this        
c       size fraction(tsedq). update transporting capacity for          
c       for this size fraction at this time increment for suspen-       
c       ded sediment. update the total transport rate  for this         
c       time increment.                                                 
                  sedq(ised)=(1.+susp)*sedq(ised)*bper*ps          
                  tsedq(ised)=tsedq(ised)+sedq(ised)*dtim               
                  sumps=sumps+sedq(ised)                                
  111        continue                                                   
c       calculate the total sediment transport capacity                 
c       for all sizes.                                                  
               ssedq=ssedq+sumps*dtim                                   
  110     continue                                                      
  121 continue                                                          
c       calculation of channel flow detachment .                        
      dcoh=dcoh*dtim*plngth(3)                                          
           da=chdof*(ssedq-tsdlat)                                      
           if (da.lt.0.0) da=0.0                                        
      if(plasi(3).le.0.0)go to 304                                      
      if(da.gt.dcoh)da=dcoh                                             
  304      tyield=0.0                                                   
c       redistribute the available sediment in proportion to the        
c       parent material then compute the yield for each size.           
           do 220 ised=1,nsed                                           
               sedav=sedlat(ised)+da*psi(ised)                          
               yield(ised,3)=tsedq(ised)                                
               if(tsedq(ised).gt.sedav)yield(ised,3)=sedav              
               tyield=tyield+yield(ised,3)                              
  220 continue                                                          
           return                                                       
           end                                                          
c                                                                       
c     **************************************************************    
c                                                                       
      subroutine plane(slope,plngth,rt,rain,r,i,ql,yield,tyield,        
     +dtim,ftim,visco,nrain)                                            
      dimension plngth(3),rain(200),rt(200),ql(200),yield(10),          
     +sedq(10),tsedq(10)                                                
      common/sed/dcoeff,dpow,dof,chdof,taock,nsed,psi(10),dmbi(10)      
      common/soil/wet k(2),poros(2),save(2),sw(2),si(2),plasi(3),cohm(3)
      common/cover/grncov(2),cancov(2),vg(2),vc(2),fimp(2),sloose(2)    
c       subroutine plane calculates the sediment yield by size          
c       for the plane units.                                            
c       ****************************************************************
c       *                                                               
c       *     user written externals:   power                           
c       *                                                               
c       ****************************************************************
c       calculate decimal percent of ground and canopy cover.           
           cg=grncov(i)/100.                                            
           cc=cancov(i)/100.                                            
      repose=.84                                                        
c       initialize variables.                                           
      dcoh=0.0                                                          
           tranca=0.0                                                   
      fi=fimp(i)                                                        
           sumsed=0.0                                                   
           t=0.0                                                        
           do 13 ij=1,nsed                                              
               sedq(ij)=0.0                                             
               tsedq(ij)=0.0                                            
   13 continue                                                          
c       calculate the amount of sediment detached from rainfall.        
      ar1=1.-cg-fi+cg*fi                                                
      ar2=(1.0-cc)*ar1
           darea=ar2*plngth(3)*plngth(i)                                
           wtfac=darea*(1.-poros(i))*2.65*62.4/12.                      
            rtpre=0.                                                    
           do 200 j=1,nrain                                             
               dq=dcoeff*(rain(j)*60.)**dpow                            
               sumsed=sumsed+dq*(rt(j)-rtpre)/60.                       
               rtpre=rt(j)                                              
  200 continue                                                          
           sumsed=(sumsed+sloose(1))*wtfac                              
c       loop 106 calculates the transporting capacity for the           
c       calculated discharge at each time increment for each            
c       size.                                                           
           do 106 ij=2,200                                              
              qq=ql(ij)                                                 
              t=t+dtim                                                  
              if(t.gt.ftim)go to 108                                    
              if (qq.le.0.) go to 106                                   
c       calculate the depth of flow and mean velocity                   
              depth=(qq*r*visco/(257.6*slope))**(1./3.)                 
              vmean=qq/depth                                            
c       effective shear stress:                                         
              tao=.24225*45.*visco*qq/(depth**2.)                       
      tcoh=0.0022*plasi(i)**0.82                                        
      if(plasi(i).le.0.0)go to 99                                       
      erate=cohm(i)*(tao/tcoh-1.)                                       
      if(erate.lt.0.)erate=0.                                           
      dcoh=dcoh+erate                                                   
   99         sumps=0.0                                                 
      sk=cos(atan(slope))*sqrt(1.-(slope/repose)**2)                    
c       loop 107 calculates the suspended and bedload sediment          
c       transport capacity for each sediment size.                      
              do 107 ised=1,nsed                                        
                 dmb=dmbi(ised)                                         
                 ps=psi(ised)*ar1                                       
c       critical shear stress:                                          
                 taoc=102.96*taock*dmb*sk                               
                 if (tao.gt.taoc) go to 103                             
                 sedq(ised)=0.                                          
                 susp=0.                                                
                 go to 105                                              
c       calculate the bedload transport by meyer-peter, mueller         
c       equation.                                                       
  103       sedq(ised)=(12.85/1.392)*(tao-taoc)**(1.5)                  
c       shear velocity:                                                 
             sv=sqrt(62.4*depth*slope/1.938)                            
c       fall velocity:                                                  
                 fvb=(sqrt(36.064*dmb**3.+36.*visco**2.)-6.*visco)/dmb  
                 if (dmb.le.0.0002) fvb=2.9517*dmb**2./visco            
c       calculate the parametrs for the einstien suspended              
c       sediment load approximation.                                    
                 zr=fvb/(0.4*sv)                                        
                 ar=2.*dmbi(nsed)/depth                                 
      if(ar.lt..04)ar=.04                                               
                 bmv=2.5+vmean/sv                                       
                 if (zr.gt.5.5.or.ar.gt.0.5) go to 104                  
c       evaluate j1 and j2.                                             
                 call power (zr,ar,fj,sj,1.0e-2)                        
                 p=ar**(zr-1.)/(11.6*(1.-ar)**zr)                       
                 susp=p*(bmv*fj+2.5*sj)                                 
      supmax=1./ar                                                      
      if(susp.gt.supmax)susp=supmax                                     
                 if (susp.lt.0.) susp=0.                                
                 go to 105                                              
  104       susp=0.                                                     
  105       continue                                                    
c       update the sediment transport capacity for  the                 
c       suspended load  for this size. also update the total            
c       transport capacity for this size and the capacty for            
c       all sizes  at this time interval.                               
                 sedq(ised)=(1.+susp)*sedq(ised)*ps                     
                 tsedq(ised)=tsedq(ised)+sedq(ised)*plngth(3)*dtim      
                 sumps=sumps+sedq(ised)                                 
  107       continue                                                    
c       update the total sediment transport capacity for all sizes.     
              tranca=tranca+sumps*dtim*plngth(3)                        
  106    continue                                                       
  108 continue                                                          
c       calculate the detached sediment from channel flow.              
      dcoh=dcoh*plngth(i)*dtim*plngth(3)                                
           da=dof*(tranca-sumsed)                                       
           if (da.lt.0.0) da=0.0                                        
      if(plasi(i).le.0.0)go to 303                                      
      if(da.gt.dcoh)da=dcoh                                             
  303      sumsed=sumsed+da                                             
c       this loop calculates the sediment yield for the plane.          
c       first the available sediment is redistributed in pro-           
c       portion to the parent material, then the necessary com-         
c       parisions are made.                                             
           tyield=0.0                                                   
           do 210 ised=1,nsed                                           
               sedav=psi(ised)*sumsed                                   
               yield(ised)=tsedq(ised)                                  
               if(tsedq(ised).gt.sedav)yield(ised)=sedav                
               tyield=tyield+yield(ised)                                
  210 continue                                                          
           return                                                       
           end                                                          
           subroutine power (z,a,xj1,xj2,conv)                          
c       this subroutine evaluates the j1 and j2 integrals in the        
c       einstein approximation.                                         
c       xj1 and xj2 equal the j1 and j2 integral respectivly            
c       n=order of approximation +1                                     
c       conv= convergence criterion                                     
           n=1                                                          
           xj1=0.                                                       
           xj2=0.                                                       
           alg=alog(a)                                                  
           c=1.                                                         
           d=-z                                                         
           e=d+1.                                                       
           fn=1.                                                        
           aex=a**e                                                     
           go to 102                                                    
  101 n=n+1                                                             
           c=c*d/fn                                                     
           d=e                                                          
           e=d+1.                                                       
           fn=float(n)                                                  
           aex=a**e                                                     
  102 if (abs(e).le.0.001) go to 103                                    
           xj1=xj1+c*(1.-aex)/e                                         
           xj2=xj2+c*((aex-1.)/e**2-aex*alg/e)                          
           go to 104                                                    
  103 xj1=xj1-c*alg                                                     
           xj2=xj2-0.5*c*alg**2                                         
  104 if (n.eq.1) go to 105                                             
           cj1=abs(1.-fj1/xj1)                                          
           cj2=abs(1.-fj2/xj2)                                          
           if (cj1.le.conv.and.cj2.le.conv) return                      
  105 fj1=xj1                                                           
           fj2=xj2                                                      
           go to 101                                                    
           end                                                          
c                                                                       
c     ***************************************************************   
c     ***************************************************************   
c     ***************************************************************   
c                                                                       
c     **************************************************************    
c                                                                       
      subroutine out(iseg,yield,q,num,qt,tarea,tvintr,nsed,dtim,        
     +dmbi,tyield,trvol)                                                
      dimension yield(10),q(201),dmbi(10),titl(3)                       
      common/var1/titl                                                  
      print 99                                                          
      write(15,99)
   99 format(//////////)                                                
      print 100,iseg,(titl(i),i=1,3)                                    
      write(15,100)iseg,(titl(i),i=1,3)
  100 format(1x,'the results for unit number: ',i3,'(',3a4,')')         
      print 101,tarea,trvol,tvintr,qt,tyield                            
      write(15,101)tarea,trvol,tvintr,qt,tyield
  101 format(1x,' the total area in acres: ',g12.5/,                    
     +' the total rainfall in acre feet: ',g12.5,/,                     
     +' the total intercepted volume in acre feet: ',g12.5,/,           
     +' the total discharge in acre feet: ',g12.5,/,                    
     +' the total sediment yield in pounds: ',g12.5///)                 
      print 110                                                         
      write(15,110)
  110 format(' (note: the  following values are per foot width',        
     +' if the unit is a plane.)')                                      
      print 102                                                         
      write(15,102)
  102 format(/1x,'the sediment yield by sizes'/                         
     +'  size in mm       pounds')                                      
      do 909 ised = 1,nsed
  909 dmbi(ised) = dmbi(ised) * 304.8
      print 103,(dmbi(ised),yield(ised),ised=1,nsed)                    
      write(15,103)(dmbi(ised),yield(ised),ised=1,nsed)
  103 format(1x,g12.5,4x,g12.5)                                         
      print 104                                                         
      write(15,104)
  104 format(////1x,' final hydrograph'//9x,'time'                      
     +,9x,'discharge'/8x,'(min.)',9x,'(cfs)')                           
      do 106 i=2,num                                                    
        time=(i-1)*dtim/60.                                             
        print 105, time,q(i)                                            
        write(15,105)time,q(i)
  105 format (7x,f8.2,6x,f10.4)                                         
  106 continue                                                          
      return                                                            
      end                                                               
