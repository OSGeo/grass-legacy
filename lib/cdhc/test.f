	program main
	logical a,b,c
	a=.true.
	b=.true.
	c=.false.
        if (a.or.b.and.c) print *, '1'
        if ((a.or.b).and.c) print *, '2'
        if (a.or.(b.and.c)) print *, '3'
	end
