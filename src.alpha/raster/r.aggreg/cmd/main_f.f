	program raggreg

	external main_c

	character*200 fargv(0:255)
	integer*4 fargc, iargc, i

c----  Anzahl der Kommandozeilenargumente ------	
	fargc = iargc()

c----  Einlesen der Kommandozeilenargumente ------

	do i=0,fargc
	  call getarg( i, fargv(i) )
	enddo


	call main_c ( (fargc+1), fargv)

	end
