# include <stdio.h>

line (x1, y1, x2, y2)
{ printf ("line (%dx, %dy, %dx, %dy)\n", x1, y1, x2, y2) ; }

space (l, b, r, t)
{ printf ("space (%dl, %db, %dr, %dt)\n", l, b, r, t) ; }

linemod (s)
{ printf ("linemod (%s)\n", s) ; }

move (x, y)
{ printf ("move (%dx, %dy)\n", x, y) ; }

erase ()
{ printf ("erase()\n") ; }

openpl ()
{ printf ("openpl()\n") ; }

closepl ()
{ printf ("closepl()\n") ; }
