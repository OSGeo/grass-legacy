      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var6/ num,sin(35,5),tsedo(35,10),qtout(35),time(200)      
     1,tddep(35,5),itype(35)                                            
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
c     dimension qsed(10),pppm(10),scour(35)
      open(3,file='tape3')
      open(4,file='tape4')
      open(13,file='tape13')
      open(14,file='tape14')
      open(11,file='tape11')
      open(12,file='tape12')
      open(7,file='tape7')
      open(8,file='tape8')
      open(9,file='tape9')
      open(10,file='tape10')
      open(16,file='tape16')
      rewind 3
      rewind 4
      rewind 7
      rewind 8
      rewind 9
      rewind 10
      rewind 11
      rewind 12
      rewind 13
      rewind 14
      rewind 16
c     call get1                                                         
c100  write(6,1000)                                                     
c     call redin(inx)                                                   
c   calls input procedure. if the program is running on a non-cms       
c   system, or exec2 assemble is not available, decomment the next lines
c1    continue
c     write(6,*) ' enter option:'                                       
c     read(5,*,end=1,err=1) inx                                         
c     goto(10,20,30,99) inx                                             
c     goto 100                                                          
c10   call intd1                                                        
c     goto 100                                                          
c20   call dout1                                                        
c     goto 100                                                          
c30   call dout1                                                        
c     rewind 9                                                          
c     rewind 4                                                          
c     rewind 3                                                          
c     rewind 10                                                         
c     rewind 11                                                         
c     rewind 12                                                         
c     rewind 13                                                         
c     rewind 14                                                         
      call data1                                                        
      call temp                                                         
      call initl                                                        
      write(11,900)                                                     
      call route                                                        
c110  continue
c     write(6,1001)                                                     
c     read(5 ,*,end=110,err=110) ix                                     
c     if(ix.eq.1) call prtplt(int(num),qout,time(num))                  
c     rewind 3                                                          
c     rewind 4                                                          
c     rewind 7                                                          
c     rewind 8                                                          
c     rewind 9                                                          
c     rewind 10                                                         
c     rewind 11                                                         
c     rewind 12                                                         
c     rewind 13                                                         
c     rewind 14                                                         
c     call get1                                                         
c     goto 100                                                          
c99   continue
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      stop
900   format(1x,' iseg        time        discharge    sediment',       
     *         /'           (minutes)       (cfs)        (ppm)')        
c1000 format(5x,'msed3 runtime menu',/,                                 
c    *5x,'1) computational sequence and physical characteristics',/,    
c    *5x,'2) write to file',/,                                          
c    *5x,'3) run the channel model',/,5x,'4) stop',/)                   
c1001 format(5x,'do you want a plot of the outflow hydrograph',         
c    *//,5x,'1) yes  2) no       (default is no)')                      
      end                                                               
c                                                                       
      subroutine data1                                                  
      dimension title(20)                                               
      dimension pf(35,11),d(35,11)                                      
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var5/ visco(35),t(35)                                     
      common /var6/ num,sin(35,5),tsedo(35,10),qtout(35),time(200)      
     1,tddep(35,5),itype(35)                                            
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      common / var15/ qws(35),qpl(70),qtws(35),qtpl(70),sedpl(70,10),   
     1sedws(35,10),gbows(35,10),gbopl(70,10),qcon(35),gbocon(35,10)     
      common/var16/ xn(35),c(35)                                        
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
      common /var18/ plasi(35),cohm(35)                                 
c     input and output title                                            
      read(9,110)(title(ii),ii=1,18)                                    
  110 format(20a4)                                                      
      write(11,115,err=1)(title(ii),ii=1,18)                            
  115 format(' title: ',20a4)                                           
      read(9,150)dtim,ftim                                              
      num=ifix(ftim/dtim+.00001)                                        
         read(9,140)npl,nws,ncon,nres,nch,nsed                          
      imax=20                                                           
      ndx=5                                                             
  140 format(8i10)                                                      
          mm=nsed-1                                                     
          nseg=nch+nres                                                 
        do 2436 i=1,nseg                                                
          read(9,145) iseg(i),(iws(i,j),j=1,3),(ipl(i,j),j=1,2),       
     1    icon(i),(iup(i,j),j=1,3)                                     
2436    continue                                                        
  145 format (10i5)                                                     
       eps=.01                                                          
       dt=dtim                                                          
  150 format(8f10.0)                                                    
          do 200 i=1,nseg                                               
            read(10,160) itype(i)                                       
  160         format(//i10)                                             
             if(itype(i).eq.2) go to 186                                
              read(10,150) slen(i),slop1(i),wet k(i),poros(i),          
     +        si(i),sw(i),suc(i),plasi(i)                               
              read(10,150) xn(i),c(i),t(i),cohm(i)                      
              read(10,150) a1(i),b1(i),a2(i),b2(i),agb(i),bex(i)        
     1        ,adf(i),delts(i)                                          
              do 180 j=1,nsed                                           
                  read(10,150)d(i,j),pf(i,j)                            
  180 continue                                                          
      go to 188                                                         
  186 read(10,150) vcap(i),vitl(i),sarea(i),poros(i)                    
      vcap(i)=vcap(i)*43560.                                            
      vitl(i)=vitl(i)*43560.                                            
      sarea(i)=sarea(i)*43560.                                          
      do 187 j=1,nsed                                                   
            read(10,150) d(i,j)                                         
  187 continue                                                          
  188 continue                                                          
          do 185 j=1,mm                                                 
              if(itype(i).eq.2) go to 184                               
              pb0(i,j)=pf(i,j+1)-pf(i,j)                                
  184         dnb(i,j)=(d(i,j+1)*d(i,j))**.5                            
  185 continue                                                          
  200 continue                                                          
          do 210 i=1,nseg                                               
  210 continue                                                          
      if(npl.eq.0) go to 213                                            
      do 211 i=1,npl                                                    
      read(4,300)(sedpl(i,jj),jj=1,mm)                                  
  211 continue                                                          
  300 format(4g20.10)                                                   
  213 continue                                                          
      if(nws.eq.0) go to 214                                            
      do 212 i=1,nws                                                    
      read(13,300)(sedws(i,jj),jj=1,mm)                                 
  212 continue                                                          
  214 continue                                                          
      if(npl.ge.1)read(3,300)(qtpl(i),i=1,npl)                          
      if(nws.ge.1)read(14,300)(qtws(i),i=1,nws)                         
      return                                                            
1     stop 'error on write title'                                       
      end                                                               
c                                                                       
      subroutine initl                                                  
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var5/ visco(35),t(35)                                     
      common /var6/ num,sin(35,5),tsedo(35,10),qtout(35),time(200)      
     1,tddep(35,5),itype(35)                                            
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
c     initialize entire watershed                                       
      itcom1=num                                                        
      do 100 m=1,mm                                                     
      egb(m)=0.                                                         
      gbc(m)=0.                                                         
      zbl(m)=0.                                                         
      btem(m)=0.                                                        
  100 continue                                                          
      do 300 n=1,nseg                                                   
      dx(n)=slen(n)/5.                                                  
      qin(n)=0.0                                                        
      ifull(n)=0                                                        
      tsedep(n)=0.0                                                     
      if(itype(n).eq.2) vt(n)=vitl(n)                                   
      q(n)=0.                                                           
      if(itype(n).eq.1)wet k(n)=wet k(n)/43200.                         
      qtout(n)=0.                                                       
      qdum(n)=0.                                                        
      do 300 j=1,ndx                                                    
      gbupin(n,m)=0.0                                                   
      a(n,j)=0.                                                         
      qp(n,j)=0.                                                        
      sin(n,j)=0.                                                       
      if(itype(n).eq.1)slop(n,j)=slop1(n)                               
      tddep(n,j)=0.                                                     
      do 300 m=1,mm                                                     
      gbo(n,m)=0.                                                       
      tsedo(n,m)=0.                                                     
      sb(n,j,m)=0.                                                      
      rb(n,j,m)=0.                                                      
  300 continue                                                          
      do 106 n=1,nseg                                                   
      do 104 m=1,mm                                                     
      dmb(n,m)=dnb(n,m)/304.8                                           
      vfall(n,m)=(sqrt(36.064*dmb(n,m)**3+36.*visco(n)**2)-6.*visco(n)) 
     1    /dmb(n,m)                                                     
      if (dmb(n,m) .gt. 0.0002) go to 104                               
      vfall(n,m)=2.9517*dmb(n,m)**2/visco(n)                            
  104 continue                                                          
       dmax(n)=dmb(n,mm)                                                
       porb(n)=1.-poros(n)                                              
  106 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine temp                                                   
c ---  this subroutine corrects the viscosity and                       
c ---  hydraulic conductivity for temperature variations                
c ---  from the assumed temperature of 68 degrees (f).                  
c ---  parameter defintions.                                            
c        t      = temperature in degrees f.                             
c        visco  = kinematic viscosity (ft**2/sec)                       
c        iplane = number of planes.                                     
c                                                                       
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var5/ visco(35),t(35)                                     
      dimension te(10),v(10)                                            
      data te/32.,40.,50.,60.,68.,80.,90.,100.,120.,140./,              
     1v/1.93,1.66,1.41,1.22,1.09,0.930,0.826,0.739,0.609,               
     10.514/                                                            
c ---  calculate new viscosity by interpolation.                        
      do 110 k=1,nseg                                                   
      do 100 i=1,10                                                     
        if(te(i).lt.t(k)) go to 100                                     
        fac1=(t(k)-te(i-1))/(te(i)-te(i-1))                             
        visco(k)=v(i-1)+fac1*(v(i)-v(i-1))                              
        go to 110                                                       
  100 continue                                                          
  110 continue                                                          
c ---  adjust the hydraulic conductivity.                               
      do 101 k=1,nseg                                                   
   10   fac2=visco(k)/1.09                                              
        wet k(k)=wet k(k)/fac2                                          
        visco(k)=visco(k)*.00001                                        
  101 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine cease (k,j,dtx,qup,qe,area)                            
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      area=0.                                                           
            qe=0.                                                       
      do 360 m=1,mm                                                     
      egb(m)=gbup(m)*dtx+btem(m)*a(k,j)+blat(m)                         
      rb(k,j,m)=0.                                                      
  360 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine out (k,qsed,qup,ppm,pppm,it,itcom1,dts)                
c     this subroutine handles the output of the results.                
c     only the results at the downstream boundary are                   
c     written out. the other results are written to tape11              
c     after each time interval.                                         
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var6/ num,sin(35,5),tsedo(35,10),qtout(35),time(200)      
     1,tddep(35,5),itype(35)                                            
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
      dimension qsed(10),pppm(10),scour(35)                             
          qtout(k)=qtout(k)+(qdum(k)+qup)*.5*dts/43560.                 
          write(11,200,err=1)k,time(it),qup,ppm                         
          write(11,210,err=1)(pppm(m),m=1,mm)                           
  210     format(1x,' ppm by sizes : ',5(1x,f8.0)/16x,5(1x,f8.0))       
  200 format(3x,i2,5x,f8.2,5x,f10.2,5x,f10.2)                           
      do 300 m=1,mm                                                     
          tsedo(k,m)=tsedo(k,m)+qsed(m)                                 
  300 continue                                                          
      qdum(k)=qup                                                       
      if(k.ne.nseg.or.it.ne.itcom1) return                              
      write(11,350,err=1)                                               
  350 format(//,' the following are sediment yields by size fractions', 
     1     /,' with the smallest size first and the largest last.',/)   
      do 360 j=1,k                                                      
          write(11,351,err=1) j                                         
          write(11,352,err=1)(tsedo(j,m),m=1,mm)                        
          if(itype(j).eq.2) go to 360                                   
          scour(j)=0.0                                                  
          do 359 jj=1,ndx                                               
              scour(j)=scour(j)+tddep(j,jj)/ndx                         
  359 continue                                                          
  360 continue                                                          
  351 format(/,' the yields for unit ',i2,' in pounds.')                
  352 format(5(1x,g15.7)/5(1x,g15.7))                                   
      write(11,370,err=1)                                               
  370 format(//,' the following results display the amount of sediment' 
     1      ,/,' deposited or removed from the channels and reservoirs.'
     2      ,/,' in the case of a channel the number represents the '   
     3      ,/,' depth of degradation or aggradation. for a resevoir it'
     4      ,/,' represents the volume of sediment deposited with por-' 
     5      ,/,' osity taken into account.',/)                          
      do 390 j=1,k                                                      
          if(itype(j).eq.1) write(11,381,err=1)j,scour(j)               
          if(itype(j).eq.2) write(11,382,err=1)j,tsedep(j)              
  390 continue                                                          
  381 format(1x,' for channel no. ',i2,' the change in elevation =',    
     1g15.7,' feet.')                                                   
  382 format(' for reservoir no. ',i2,' the stored sediment =',g15.7    
     1,' ac-ft.')                                                       
      write(11,400,err=1)                                               
  400 format(//' the following are the water yields for each unit.'//)  
      write(11,401,err=1)(j,qtout(j),j=1,nseg)                          
  401 format(/' the total water yield for unit ',i2,' is:',g15.7,       
     1        'ac-ft.')                                                 
      print 500,qtout(nseg)                                             
      write(16,500)qtout(nseg)
  500 format(10(/),' the results for the downstream outlet'             
     1            ,' of the watershed:',                                
     2           //' the total runoff in acre feet:',g12.5,             
     3           //' the sediment yield by sizes:'                      
     4           //'   size in mm       pounds')                        
      do 909 m=1,mm
  909 dmb(nseg,m) = dmb(nseg,m) * 304.8
      print 510,(dmb(nseg,m),tsedo(nseg,m),m=1,mm)                      
      write(16,510)(dmb(nseg,m),tsedo(nseg,m),m=1,mm)
  510 format(1x,g12.5,4x,g12.5)                                         
      print 520                                                         
      write(16,520)
  520 format(//' final hydrograph'//9x,'time',9x,'discharge'            
     1/8x,'(min.)',9x,'(cfs)')                                          
      print 530,(time(i),qout(i),i=1,itcom1)                            
      write(16,530)(time(i),qout(i),i=1,itcom1)
  530 format(7x,f8.2,6x,f10.4)                                          
      close(16)
      return                                                            
1     stop 2                                                            
      end                                                               
c                                                                       
      subroutine chinl(k,sin,qup,dts,area,qlatp)                              
c ---  this subroutine calculates channel infiltration.                 
c ---  parmeter definitions.                                            
c        si     = initial saturation of channel soil.                   
c        sw     = wetted saturation of channel soil.                    
c        suc    = average suction of channel soil (inches).             
c        poros  = porosity of soil.                                     
c        wet k  = hydraulic conductivity of channel soil (in/hr).       
c        sin    = accumalated infiltrated volume (ft**3).               
c        qup    = upstream flow (cfs).                                  
c        qlat   = lateral inflow (cfs/ft).                              
c        dtim   = length of time interval (sec).                        
c        area   = previous cross sectional area (ft**2).                
c                                                                       
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
c ---  calculate channel top width and depth of flow.                   
      tw=a2(k)*area**b2(k)                                              
      d=area/tw                                                         
c ---  calculate potential infiltration.                                
      al=(suc(k)/12.+d)*(sw(k)-si(k))*poros(k)                          
      c1=2.*sin-wet k(k)*dts                                            
      c2=8.*wet k(k)*dts*(al+sin)                                       
      segl=slen(k)/float(ndx)                                           
      delf=((-c1+(c1**2+c2)**.5)/2.)*segl*tw                            
      if(delf.eq.0.0) return                                            
c ---  calculate volume of available water.                             
      delv=(qlat*segl+qup)*dts                                          
c ---  compare potential with available.                                
      if(delf.ge.delv)go to 30                                          
c ---  this is the case that the total potential is                     
c ---  infiltrated.                                                     
      sin = sin + delf/(segl*tw)                                        
      qlatp=qlat-delf/(segl*dts)                                         
      return                                                            
c ---  this is the case that the all the available                      
c ---  water is infiltrated.                                            
   30 sin = sin + delv/(segl*tw)                                        
      qlatp=0.                                                           
      qup=0.                                                            
      return                                                            
      end                                                               
c                                                                       
      subroutine wrout (k,j,dtx,alp,bet,qup,qe)                         
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
c     set up a-q relationship                                           
            qprx=qp(k,j)                                                
      qave=0.5*(qup+qprx)                                               
            bem=bet-1.                                                  
            ben=bem-1.                                                  
            albet=alp*bet                                               
            albem=alp*bet*bem                                           
            dtxa=dtx+alp                                                
            error=eps*asum                                              
c     linear scheme to find the first approximation                     
            iter=0                                                      
            if (qave.le.1.0e-5) go to 114                               
            daq=albet*qave**bem                                         
            qe=(alat+dtx*qup+daq*qprx)/(dtx+daq)                        
            go to 115                                                   
  114       qe=asum/dtxa                                                
c     nonlinear scheme to refine the solution                           
  115       iter=iter+1                                                 
            aest=dtx*qe+alp*qe**bet                                     
            adev=asum-aest                                              
            if (abs(adev).le.error) go to 120                           
            if (iter.lt.imax) go to 116                                 
            stop                                                        
  116       fder=dtx+albet*qe**bem                                      
            sder=albem*qe**ben                                          
            bb=fder/sder                                                
            sc=2.*adev/sder                                             
            stem=bb*bb+sc                                               
            if (stem.ge.0.) go to 117                                   
            qe=qe+adev/fder                                             
            go to 115                                                   
  117       stem=sqrt(stem)                                             
            if (adev.gt.0.) go to 119                                   
            etem=bb+stem                                                
            qe=qe-etem                                                  
            if (qe.gt.0.) go to 115                                     
  118       etem=0.5*etem                                               
            qe=qe+etem                                                  
            if (qe.gt.0.) go to 115                                     
            go to 118                                                   
  119       x1=qe-bb-stem                                               
            x2=qe-bb+stem                                               
            ad1=abs(asum-dtx*x1-alp*x1**bet)                            
            ad2=abs(asum-dtx*x2-alp*x2**bet)                            
            qe=x1                                                       
            if (ad1.gt.ad2) qe=x2                                       
            go to 115                                                   
  120 return                                                            
      end                                                               
c                                                                       
      subroutine resis(xn,c,a1,b1,slp,alp,bet)                          
c this subroutine calculates the parameters a and                       
c b in the equation area=a*q**b.                                        
      if(c.ne.0.0) go to 102                                            
c      ---  this is for the mannings resistance.                        
  101  bet=(3.)/(5.-2.*b1)                                              
      alp=(a1**(4./3.)*xn**2./(2.21*slp))**(3./(10.-4.*b1))             
      return                                                            
c      ---  this is for the chezy resistance.                           
  102 bet=2./(3.-b1)                                                    
      alp=(a1/(c**2.*slp))**(bet/2.)                                    
      return                                                            
      end                                                               
c                                                                       
      subroutine pertg(k)                                               
      dimension zb(100)                                                 
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      ssum=0.                                                           
      do 100 m=1,mm                                                     
      zb(m)=zbl(m)                                                      
      ssum=ssum+zb(m)                                                   
  100 continue                                                          
      if(ssum.le.0.0) go to 105                                         
      do 104 m=1,mm                                                     
      pb1(k,m)=zb(m)/ssum                                               
  104 continue                                                          
      return                                                            
  105 continue                                                          
      do 106 m=1,mm                                                     
        pb1(k,m)=pb0(k,m)                                               
  106 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine transp (k,j,bper,slp,qe,area,alp,bet,dc,dcoh)          
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var5/ visco(35),t(35)                                     
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      common /var18/ plasi(35),cohm(35)                                 
c     determination of flow conditions, such as hydraulic depth, mean   
c     velocity, and boundary shear stress                               
            area=alp*qe**bet                                            
            weper=cpr*area**epr                                         
            hyrad=area/weper                                            
           bper=a2(k)*area**b2(k)                                       
            vmean=qe/area                                               
      tau=62.4*hyrad*slp                                                
      if(plasi(k).le.0.0)go to 33                                       
      tcoh=0.0022*plasi(k)**0.82                                        
      erate=cohm(k)*(tau/tcoh-1.)/(62.4*2.65)                           
      if(erate.lt.0.0)erate=0.0                                         
      dcoh=erate*dt*60.                                                 
   33 sv=sqrt(tau/1.9379)                                               
            bmv=2.5+vmean/sv                                            
      dc=0.0                                                            
      icheck=0                                                          
      fmax=1.5/(1.69+2.*alog10(2.*hyrad/dmb(k,mm)))**2                  
      if(fmax.gt.0.10) fmax=0.10                                        
      if(fmax.lt.0.01) fmax=0.01                                        
      fmin=0.50*fmax                                                    
  124 do 204 m=1,mm                                                     
       f=1.5/(1.69+2.*alog10(2.*hyrad/dmb(k,m)))**2                     
       if(f.gt.fmax)f=fmax                                              
      if(f.lt.fmin)f=fmin                                               
      tao=.2425*f*vmean*vmean                                           
      ttem=tao-102.96*delts(k)*dmb(k,m)                                 
c     bed material load routing                                         
      if(ttem.le.0.) go to 127                                          
 1210 continue                                                          
c     determination of ratio of suspended bed material load             
      fvb=vfall(k,m)                                                    
            zr=fvb/(0.4*sv)                                             
      ar=2.0*dmax(k)/hyrad                                              
      supmax=1./ar                                                      
      if( ar .gt. 0.9) go to 125                                        
            call power (zr,ar,fj,sj,1.0e-2)                             
            p=ar**(zr-1.)/(11.6*(1.-ar)**zr)                            
            susp=p*(bmv*fj+2.5*sj)                                      
            if (susp.lt.0.) susp=0.                                     
            if(susp.gt.supmax) susp=supmax                              
            go to 126                                                   
  125       susp=0.                                                     
c     determination of transporting capacity of bed material load       
  126 gbc(m)=(1.+susp)*bper*agb(k)*ttem**bex(k)                        
      rb(k,j,m)=gbc(m)*pb1(k,m)/qe                                      
      ttmpre=ttem                                                       
      m1=m                                                              
      if(m.ge.mm)dc=1.1*dmb(k,m)                                        
      go to 204                                                         
  127 rb(k,j,m)=0.                                                      
      gbc(m)=0.0                                                        
      icheck=icheck+1                                                   
      if(icheck.gt.1) go to 204                                         
      if(m.le.1) go to 204                                              
      dc=dmb(k,m1)+ttmpre*(dmb(k,m)-dmb(k,m1))                          
     +/(ttmpre-ttem)                                                    
  204 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine power (z,a,xj1,xj2,conv)                               
c     this subroutine evaluate j1 and j2 integrals                      
c     notations                                                         
c     xj1 = value of j1 integral                                        
c     xj2 = value of j2 integral                                        
c     n = order of approximation + 1                                    
c     conv = convergence criterion                                      
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
      subroutine srout (k,j,dtx,bper,area,qe,dc,qconc,dcoh)             
      dimension zbnew(10)                                               
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var5/ visco(35),t(35)                                     
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      common /var18/ plasi(35),cohm(35)                                 
c     check the availability of bed material load                       
      narm=1                                                            
      parm=1.0                                                          
      sgbup=0.                                                          
      srbe=0.                                                           
      sbtem=0.                                                          
      sblat=0.                                                          
      if(zsum.lt.0.2) go to 50                                          
      parm=0.0                                                          
      do 10 m=1,mm                                                      
       m1=mm-m+1                                                        
       parm=parm+pb1(1,m1)                                              
       if(pb1(1,m1).le.0.0) go to 10                                    
       narm=m1                                                          
       go to 20                                                         
   10 continue                                                          
   20 continue                                                          
   50 do 101 m=narm,mm                                                  
      sgbup=sgbup+gbup(m)                                               
      srbe=srbe+rb(k,j,m)                                               
      sbtem=sbtem+btem(m)                                               
  101 sblat=sblat+blat(m)                                               
      segb=(sgbup-srbe*qe)*dtx-srbe*area+sbtem*a(k,j)+sblat             
      if(segb .ge. 0.) go to 102                                        
      ebtem=segb+zsum*bper                                              
      if(ebtem .ge. 0.) go to 102                                       
      dep=-adf(k)*ebtem/(bper*parm)                                     
      if(dc.gt.dmb(k,m)) go to 103                                      
      if(dc.le.dmb(k,1))go to 102                                       
      pc0=pb0(k,mm)                                                     
      pc1=pb1(k,mm)                                                     
      mmm=mm-1                                                          
      do 100 m=1,mmm                                                    
          m1=mm-m                                                       
          pc0=pc0+pb0(k,m1)                                             
          pc1=pc1+pb1(k,m1)                                             
          if(dc.lt.dmb(k,m1)) go to 100                                 
          fac=(dc-dmb(k,m1))/(dmb(k,m1+1)-dmb(k,m1))                    
          pc0=pc0-pb0(k,m1)*fac                                         
          pc1=pc1-pb1(k,m1)*fac                                         
          if(pc1.gt.0.0) go to 97                                       
          depmax=zsum                                                   
          go to 98                                                      
   97     depmax=2.*dc/pc1                                              
   98     if(depmax.lt.zsum) go to 102                                  
          if(pc0.le.0.0) go to 103                                      
          deptem=(2.*dc-zsum*pc1)/pc0                                   
          go to 99                                                      
  100 continue                                                          
      go to 102                                                         
   99 if(dep.gt.deptem) dep=deptem                                      
      go to 103                                                         
  102 dep=0.0                                                           
  103 continue                                                          
      if(plasi(k).le.0.0)go to 104                                      
      if(dep.gt.dcoh)dep=dcoh                                           
  104 zsum=zsum+dep                                                     
      sum=zsum                                                          
       if(sum.le.0.0) sum=segb                                          
      if(sum.le.0.0)sum=.005                                            
c     determination of availability for next time step                  
      do 206 m=1,mm                                                     
      zbl(m)=zbl(m)+dep*pb0(k,m)                                        
  206 continue                                                          
      do 306 m=1,mm                                                     
      zbnew(m)=(zbl(m)*bper+gbup(m)*dtx+btem(m)*a(k,j)+blat(m))         
     1/(bper+(gbc(m)/sum)*(dtx+area/qe))                                
  305 rb(k,j,m)=zbnew(m)*gbc(m)/(sum*qe)                                
      egb(m)=(zbnew(m)-zbl(m))*bper                                     
      if(egb(m).le.0.0) go to 306                                       
      if(qe.le.5.0) go to 306                                           
      fdep=.5*vfall(k,m)*bper*dx(k)/qe                                  
      if(fdep.gt.1.0) go to 306                                         
      rbn=(1.-fdep)*(gbup(m)+blat(m)/dtx)/qconc                         
      if(rbn.le.rb(k,j,m)) go to 306                                    
      rbmax=.99*(gbup(m)*dtx+a(k,j)*btem(m)+blat(m))/(qe*dtx+area)      
      if(rbn.gt.rbmax)rbn=rbmax                                         
      rb(k,j,m)=rbn                                                     
      egb(m)=(gbup(m)-rb(k,j,m)*qe)*dtx-area*rb(k,j,m)                  
     1     +a(k,j)*btem(m)+blat(m)                                      
  306 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine route                                                  
      dimension qsed(10),pppm(10)                                       
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var4/ agb(35),bex(35),adf(35),delts(35)                   
      common /var5/ visco(35),t(35)                                     
      common /var6/ num,sin(35,5),tsedo(35,10),qtout(35),time(200)      
     1,tddep(35,5),itype(35)                                            
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      common/var16/ xn(35),c(35)                                        
      dts=dt*60.                                                        
      dtn=dts*float(ndx)                                                
      itcom1=num                                                        
      do 100 it=1,itcom1                                                
      if(nws.ge.1.or.npl.ge.1)call data2                                
      time(it)=float(it)*dt                                             
c     compute at time it (t+dt)                                         
      do 200 i=1,nseg                                                   
         k=iseg(i)                                                      
          if(itype(k).eq.1) go to 305                                   
          call res(k,qup,dts)                                           
      q(k)=qup                                                          
          go to 311                                                     
  305 continue                                                          
      cpr=a1(k)                                                         
      epr=b1(k)                                                         
         dtx=dtn/slen(k)                                                
      call uplat (k,dts,slen(k),qup)                                    
      do 300 j=1,ndx                                                    
      bper=a2(k)                                                        
      zsum=0.                                                           
      slp=slop(k,j)                                                     
      do 310 m=1,mm                                                     
      zbl(m)=sb(k,j,m)                                                  
      zsum=zsum+zbl(m)                                                  
      btem(m)=rb(k,j,m)                                                 
  310 continue                                                          
      qconc=qup+qlat*slen(k)/ndx                                        
      qlatp = qlat
      if(a(k,j).le.1.0e-04) go to 500                                   
      call chinl(k,sin(k,j),qup,dts,a(k,j),qlatp)                             
  500 alat=qlatp*dts                                                     
      asum=alat+a(k,j)+dtx*qup                                          
c     determine the coefficient and the exponent in a-q relation        
      call resis(xn(k),c(k),a1(k),b1(k),slp,alp,bet)                    
      if(asum.le. 1.0e-05) go to 320                                    
      call wrout (k,j,dtx,alp,bet,qup,qe)                               
      if(qe.le.0.01) go to 320                                          
      call pertg(k)                                                     
 3425 format(2i5)                                                       
      call transp (k,j,bper,slp,qe,area,alp,bet,dc,dcoh)                
      call srout (k,j,dtx,bper,area,qe,dc,qconc,dcoh)                   
      go to 350                                                         
  320 call cease (k,j,dtx,qup,qe,area)                                  
  350 a(k,j)=alp*qe**bet                                                
            qp(k,j)=qe                                                  
            qup=qe                                                      
      sumv=0.                                                           
      do 390 m=1,mm                                                     
      gbup(m)=rb(k,j,m)*qe                                              
      sb(k,j,m)=zbl(m)+egb(m)/bper                                      
      gbo(k,m)=gbup(m)                                                  
      sumv=sumv+egb(m)                                                  
  390 continue                                                          
      ddep(j)=sumv/(porb(k)*bper)                                       
      tddep(k,j)=tddep(k,j)+ddep(j)                                     
  300 continue                                                          
         q(k)=qup                                                       
  311     continue                                                      
      if(k.eq.nseg)qout(it)=qup                                         
      gbosum=0.0                                                        
      do 433 m=1,mm                                                     
         gbosum=gbosum+gbo(k,m)*2.65                                    
  433 continue                                                          
      ppm=0.                                                            
      do 312 m=1,mm                                                     
          qsed(m)=gbo(k,m)*165.4*dts                                    
          pppm(m)=0.0                                                   
          if(qup.gt.0.0)pppm(m)=gbo(k,m)*2650000./(qup+gbosum)          
          ppm=ppm+pppm(m)                                               
  312 continue                                                          
      call out(k,qsed,qup,ppm,pppm,it,itcom1,dts)                       
  200 continue                                                          
  100 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine data2                                                  
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common / var15/ qws(35),qpl(70),qtws(35),qtpl(70),sedpl(70,10),   
     1sedws(35,10),gbows(35,10),gbopl(70,10),qcon(35),gbocon(35,10)     
      if(nws.le.0) go to 120                                            
      read(14,100)(qws(i),i=1,nws)                                      
      do 110 i=1,nws                                                    
  100     format(4g20.10)                                               
          do 110 jj=1,mm                                                
              gbows(i,jj)=0.0                                           
             if(qtws(i).le.0.0) go to 110                               
              gbows(i,jj)=sedws(i,jj)*qws(i)/(qtws(i)*43560.            
     1        *165.4)                                                   
  110 continue                                                          
  120 continue                                                          
      if(npl.le.0) return                                               
      read(3,100)(qpl(i),i=1,npl)                                       
      do 200 i=1,npl                                                    
          do 200 jj=1,mm                                                
              gbopl(i,jj)=0.0                                           
             if(qtpl(i).le.0.0) go to 200                               
              gbopl(i,jj)=sedpl(i,jj)*qpl(i)/(qtpl(i)*43560.*165.4)     
  200 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine res(k,qup,dts)                                         
      dimension gbupav(10),qinpre(35),gbupre(35,10)                     
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var2/ seg(35),slen(35),slop1(35),   dmb(35,10),           
     1pb0(35,10),dnb(35,10),wet k(35),poros(35),                        
     2si(35),sw(35),suc(35),vfall(35,10),sarea(35)                      
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
      qinpre(k)=qin(k)                                                  
      call upres(k,qup)                                                 
      qin(k)=qup                                                        
      qav=(qinpre(k)+qin(k))/2.                                         
      do 100 m=1,mm                                                     
          gbupre(k,m)=gbupin(k,m)                                       
          gbupin(k,m)=gbup(m)                                           
          gbupav(m)=(gbup(m)+gbupre(k,m))/2.                            
  100 continue                                                          
      vin=qav*dts                                                       
      vt(k)=vt(k)+vin                                                   
      if(vt(k).lt.vcap(k)) go to 400                                    
      ifull(k)=ifull(k)+1                                               
      qup=qav                                                           
      if(ifull(k).ge.2) go to 200                                       
      qup=(vt(k)-vcap(k))/dts                                           
  200 continue                                                          
      vovfl=1.5*qup/sarea(k)                                            
      if(vovfl.le.0.0) go to 400                                        
      do 300 m=1,mm                                                     
          trpef=vfall(k,m)/vovfl                                        
          if(trpef.gt.1.0)trpef=1.0                                     
          gbo(k,m)=(1.-trpef)*gbupav(m)*qup/qav                         
          seddep=(gbupav(m)-gbo(k,m))*dts                               
          tsedep(k)=tsedep(k)+seddep/(43560.*porb(k))                   
  300 continue                                                          
      return                                                            
  400 continue                                                          
      qup=0.0                                                           
      do 500 m=1,mm                                                     
          gbo(k,m)=0.0                                                  
          seddep=(gbupav(m)-gbo(k,m))*dts                               
          tsedep(k)=tsedep(k)+seddep/(43560.*porb(k))                   
  500 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine upres(k,qup)                                           
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common / var15/ qws(35),qpl(70),qtws(35),qtpl(70),sedpl(70,10),   
     1sedws(35,10),gbows(35,10),gbopl(70,10),qcon(35),gbocon(35,10)     
      common /var17/ tsedep(35),vcap(35),vitl(35),vt(35)                
     1,gbupin(35,10),qin(35),ifull(35)                                  
      qup=0.                                                            
       do 101 m=1,mm                                                    
          gbup(m)=0.0                                                   
  101 continue                                                          
      if(icon(k).eq.0) go to 107                                        
      jj=icon(k)                                                        
      read(12,200)qcon(jj)                                              
      qup=qup+qcon(jj)                                                  
      read(12,210)(gbocon(jj,m),m=1,mm)                                 
      do 106 m=1,mm                                                     
            gbup(m)=gbup(m)+gbocon(jj,m)                                
  106 continue                                                          
  107 continue                                                          
  200 format(/f10.0)                                                    
  210 format(8f10.0)                                                    
      do 100 j=1,3                                                      
          if(iup(k,j).eq.0) go to105                                    
         jj=iup(k,j)                                                    
          qup=qup+q(jj)                                                 
         do 100 m=1,mm                                                  
           gbup(m)=gbup(m)+gbo(jj,m)                                    
  100 continue                                                          
  105 continue                                                          
      do 110 j=1,3                                                      
          if(iws(k,j).eq.0) go to 115                                   
          jj=iws(k,j)                                                   
          qup=qup+qws(jj)                                               
          do 110 m=1,mm                                                 
            gbup(m)=gbup(m)+gbows(jj,m)                                 
  110 continue                                                          
  115 continue                                                          
      return                                                            
      end                                                               
c                                                                       
      subroutine uplat (k,dts,slen,qup)                                 
      common /var1/ nseg,nws,npl,ncon,ndx,imax,mm,qout(200),qdum(35)    
      common /var3/ iseg(35),iws(35,3),ipl(35,2),iup(35,3),icon(35)     
      common /var7/ egb(35),gbc(35),zbl(35),gbup(35),gblat(35),blat(35),
     1btem(35),sb(35,5,10),rb(35,5,10),slop(35,5),ddep(5)               
      common /var8/ a(35,10),q(100),qp(35,10),pb1(35,10),gbo(35,10)     
      common /var9/ porb(35),zsum,asum,eps,dt,dx(50),dmax(35)           
      common /var10/cpr,epr,alat,qlat,a1(35),b1(35),a2(35),b2(35)       
      common / var15/ qws(35),qpl(70),qtws(35),qtpl(70),sedpl(70,10),   
     1sedws(35,10),gbows(35,10),gbopl(70,10),qcon(35),gbocon(35,10)     
         qup=0.                                                         
         qlat=0.                                                        
      alat=0.                                                           
c     determine the upstream inflow rate                                
      do 11 i=1,mm                                                      
      blat(i)=0.                                                        
      gbup(i)=0.                                                        
      gblat(i)=0.                                                       
   11 continue                                                          
      if(icon(k).eq.0) go to 107                                        
      jj=icon(k)                                                        
      read(12,200)qcon(jj)                                              
      qup=qup+qcon(jj)                                                  
      read(12,210)(gbocon(jj,m),m=1,mm)                                 
      do 106 m=1,mm                                                     
            gbup(m)=gbup(m)+gbocon(jj,m)                                
  106 continue                                                          
  107 continue                                                          
      do 109 j=1, 3                                                     
      if (iup(k,j) .eq. 0) go to 109                                    
            jj=iup(k,j)                                                 
            qup=qup+q(jj)
      do 108 m=1,mm                                                     
      gbup(m)=gbup(m)+gbo(jj,m)                                         
  108 continue                                                          
  109 continue                                                          
      do 110 j=1,3                                                      
          if(iws(k,j).eq.0) go to 111                                   
          jj=iws(k,j)                                                   
          qup=qup+qws(jj)                                               
          do 110 m=1,mm                                                 
             gbup(m)=gbup(m)+gbows(jj,m)                                
  110 continue                                                          
  111 continue                                                          
c     determine the lateral inflow rate                                 
      Do 113 j=1, 2                                                     
      if(ipl(k,j).eq.0) go to 113                                       
            jj=ipl(k,j)                                                 
            qlat=qlat+qpl(jj)                                           
      do 112 i=1,mm                                                     
      gblat(i)=gblat(i)+gbopl(jj,i)*slen                                
      blat(i)=gblat(i)*dts                                              
  112 continue                                                          
  113 continue                                                          
  200 format(/f10.0)                                                    
  210 format(8f10.0)
      return                                                            
      end                                                               
