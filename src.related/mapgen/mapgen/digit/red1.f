c
	subroutine red1(x, y, np, dtol, ind, nind)
	dimension x(1), y(1)
	integer ind(1)
c
c	Eliminates unnecessary points from a digitized map.
c	Programmed by Kenneth J. Lanfear, December, 1982
c	Modified by Roger W. Bowen, January, 1985
c	Modified by G. I. Evenden 12/29/85
c
c	Arguments:
c		x,y	x and y coordinates of a line
c		np	Number of x,y pairs in the line. np will be
c			modified to reflect any reduction.
c		dtol	Tolerance, in x,y units. A good starting point
c			is about 1/4 of the smallest visible increment
c			on the map. The larger the tolerance, the more
c			points will be removed.
c		ind	vector of indicies to x/y of values to be
c			retained.
c		nind	number of 'ind' indicies
c
c	Ref.: Douglas, D., and Peucker, T., 1973, Algorithms for the
c	   reduction of the number of points required to represent
c	   a digitized line or its caricature. The Canadian Cartographer
c	   Vol. 10 No. 2, Dec. 1973, pp. 112-122. (the first method is
c	   used.)
c
c	Note on timing: The time required for this algorithm
c	is proportional to the number of points squared.
c	This is not usually a problem on a mainframe computer
c	such as Amdahl, but microcomputers will have difficulty
c	with lines of 100 points or more. If speed becomes a
c	problem, break long lines into several smaller lines.
c
c
	real m
c
c	initialization
	i1 = 1
	i2 = np
	do 10 i = 2,np
10		ind(i) = 0
	ind(1) = 1
	ind(np) = 1
c
c
c	test for maximum distance from the line between i1 and i2
c	get the parameters of the line
120	if (x(i1).eq.x(i2)) go to 210
	if (y(i1).eq.y(i2)) go to 220
	m = (y(i2) - y(i1)) / (x(i2) - x(i1))
	b = y(i1) - m * x(i1)
	denom = sqrt ( m*m + 1.0)
	go to 240
210	if (y(i1).eq.y(i2)) go to 230
	m = 1.0
	b = -x(i1)
	denom = 1.0
	go to 240
230	m = 0.0
	b = 0.0
	denom = 1.0
	go to 240
220	m = 0.0
	b = y(i1)
	denom = 1.0
240	dtest = dtol * denom
c
c	examine each point until one is greater than dtest away
	ia = i1 + 1
	ib = i2 - 1
	do 100 i = ia,ib
	if(abs (m * x(i) - y(i) + b).le.dtest) go to 100
c	set a new floater
	i2 = i2 - 1
	if ((i1+1).ne.i2) go to 120
	go to 140
100	continue
c
c	within tolerance limits
c	floater becomes the new anchor
140	if (i2.eq.np) go to 300
	ind(i2) = 1
	i1 = i2
	i2 = np
	if (i1.ne.(np-1)) go to 120
c
c	finished data reduction
c	all those points with ind equal .false. are to be retained
c
300	nind = 1
	do 310 i = 2, np
		if (ind(i) .eq. 0) goto 310
		nind = nind + 1
		ind(nind) = i
310	continue
	return
	end
