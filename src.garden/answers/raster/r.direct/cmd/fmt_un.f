

            INTEGER*2 i(20000)
            INTEGER*4 m,n(20000)
	    CHARACTER*160 tfile,t1,t2,t3
 	    read(*,'(a)') tfile
            open(11,file=tfile,status='old')
	    read(11,'(a)') t1
	    read(11,'(a)') t2
	    read(11,'(a)') t3
c           open(10,file='zap123.txt',status='old')
            open(10,file=t1,status='old')
	    read(10,*)nl,ns
c          open(8,file='zap123.dat',status='new',form='unformatted',recl=ns*2,access='direct')
c           open(9,file='t.dat',status='old')
	    open(9,file=t2,status='old')
	    open(8,file=t3,status='new',form='unformatted',recl=ns*2,access='direct')
	    do 20 m = 1,nl
 	    do 21 m2 = 1,ns
 21         read(9,*)n(m2)
	    do 10 m1=1,ns
 10		i(m1)=n(m1)
 20	    write(8,rec=m)(i(ii),ii=1,ns)
c          write(8,*)
	    stop
	    end
