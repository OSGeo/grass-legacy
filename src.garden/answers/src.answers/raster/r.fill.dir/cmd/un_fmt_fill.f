

            INTEGER*2 i(20000)
            INTEGER*4 m,n(20000)
	    CHARACTER*160 tfile,t1,t2,t3
	    read(*,'(a)') tfile
	    open(11,file=tfile,status='old')
	    read(11,'(a)') t1
	    read(11,'(a)') t2
	    read(11,'(a)') t3
            open(9,file=t1,status='unknown')
	    read(9,*)nl,ns
	    open(8,file=t2,status='unknown',form='unformatted',recl=NS*2,access='direct')
            open(10,file=t3,status='unknown')
	    do 30 m = 1,nl
	    read(8,rec=m)(i(ii),ii=1,ns)
	    do 35 m1=1,ns
 35         n(m1)=i(m1)
 30         write(10,*)(n(ii),ii=1,ns)
	    stop
	    end
