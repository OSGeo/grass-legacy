c
c	Dummy fortran library to demonstrate and test Calcomp
c	call simulations
c
c plot initialization
	subroutine plots(x, y, dev)
	write(6,9900) x, y, dev
9900	format('plot start, x, y, dev:',2g10.4,i8)
	return
	end
c
c draw and terminate
	subroutine plot(x, y, mode)
	common /fact/ fm
	if (mode .eq. 999) goto 30
	if (mode .eq. 2) goto 10
	write(6, 9900)
9900	format('move to:')
	goto 20
9901	format('draw to:')
10	write(6,9901)
20	x = x * fm
	y = y * fm
	write(6, 9902) x, y
9902	format(2f10.3)
	return
30	write(6, 9903)
9903	format('plot done')
	return
	end
c
c set factor
	subroutine factor(f)
	common /fact/ fm
	fm = f
	write(6, 9900) fm
9900	format('factor set to:',e10.5)
	return
	end
c
c set newpen
	subroutine newpen(ipen)
	write(6,9900) ipen
9900	format('select pen:',i6)
	return
	end
