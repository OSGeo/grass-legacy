c
c purpose: to reduce the number of points repreenting a line by
c	selecting points along it based on the principle of
c	minimum offset ... (method 2)
c
c parameters:
c	x, y	- vectors of length 'l' representing abscissa
c		and ordinates of a line
c	l	- length of coordinate vectors
c	dmini	- minimum offset tolerance for the reduction
c		defaults to a toleration value (?)
c	ipntr	- returned 'ip' long integer vector of pointers
c		to values of x-y selected
c	ip	- number of selected values in 'ipntr'
c
c remarks: the input variables x,y,l and dmini anre not altered.
c	The reduced or caricatured result is represented bu ipntr,
c	a vector of pointers to selected points of the original line.
c
c method: the method is the one of minimum offset. This version stacks
c	the location of previously isolated points at the end of
c	ipntr thereby saving time on each iteration.
c
c Douglas, D.H. & Peucker, T.K., 1973, Algorithm fo the reduction
c	of the number of points required to represent a digitized
c	line or its caricature: Can. Cartographer, v. 10, no. 2,
c	pp. 112-122.
c Not too sure of the above. GIE.
c
	subroutine red2(x, y, l, dmini, ipntr, ip)
	dimension x(1),y(1)
	integer ipntr(1)
	toler = 1.e-6
	dmin = dmini
	if (dmin .lt. toler) dmin = .005
	ip = 1
	np = l
	ipntr(ip) = 1
	n = 1
	ipntr(np) = l
1	m = ipntr(np)
	dy = y(m) - y(n)
	dx = x(m) - x(n)
	d = sqrt(dy * dy + dx * dx)
	dmax = 0.
	i = n
2	if (d .gt. toler) go to 9
c	in the event two points of a tested segment are within
c	a distance toler of each other
6	i = i + 1
	if (i .ge. m) go to 3
	d1 = x(n) - x(i)
	d2 = y(n) - y(i)
	di = sqrt(d1 * d1 + d2 * d2)
	if (di .le. dmax) go to 6
	dmax = di
	nm = i
	go to 6
c	tested segment tends to vertical
9	if (abs(dx) .gt. abs(dy)) go to 7
	dxdy = dx/dy
	ynd = dxdy * y(n)
15	i = i + 1
	if (i .ge. m) go to 18
	di = x(i) - x(n) + ynd - dxdy * y(i)
	di = abs(di)
	if (di .le. dmax) go to 15
	dmax = di
	nm = i
	go to 15
18	dmax = abs(dmax * dy / d)
	go to 3
c	tested segment tends to horizontal
7	dydx = dy / dx
	xnd = dydx * x(n)
5	i = i + 1
	if (i .ge. m) go to 8
	di = y(i) - y(n) + xnd - dydx * x(i)
	di = abs(di)
	if (di .le. dmax) go to 5
	dmax = di
	nm = i
	go to 5
8	dmax = abs(dmax * dx / d)
3	if (dmax .lt. dmin) go to 4
c	add to the stack the location of this point
	np = np - 1
	ipntr(np) = nm
	go to 1
c	add this point to the pointer list and remove its
c	reference from the stack.
4	ip = ip + 1
	ipntr(ip) = m
	np = np + 1
	if (m .ge. l) return
	n = m
	go to 1
	end
