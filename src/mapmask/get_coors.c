/* %W% %G% */
#include "mapmask.h"

get_coords(sides)
    int *sides;
{
    char coordask[50];
    int i,j,n,xy_index,line,start,limit,pages;
    float *north,*east;
    extern double *Ux,*Uy;

    *sides = 0;

    V_clear();
    V_line(3, "     You have chosen to define your area of interest ");
    V_line(4, "   using a polygon with individually defined corner pts.");
    V_line(6, "     Please enter the number of the option you want:");
    V_line(8, "     0 exit");
    V_line(9, "     1 triangle  (any 3 sided figure)");
    V_line(10,"     2 rectangle (any 4 sided figure)");
    V_line(11,"     3 pentagon  (any 5 sided figure)");
    V_line(12,"     4 hexagon   (any 6 sided figure)");
    V_line(13,"     5 septagon  (any 7 sided figure)");
    V_line(14,"     6 octagon   (any 8 sided figure)");
    V_line(15,"     7 polygon with more than 8 sides");
    V_line(18,"     Which option do you want? (0-7)");
    V_ques(sides,'i',18,38,1);

    V_call();
    V_clear();

    if(*sides == 0)
    {
	fprintf(stderr,"EXITING at user request");
	sleep(5);
	exit(-1);
    }

    *sides += 2;
    n = *sides;

    if(*sides < 3) return;

    if(*sides < 9)
    {
	V_line(5,
"           E                                                                 E"
);
	V_const(&window.west,'d',5,1,10);
	V_const(&window.east,'d',5,67,10);
	V_line(6,
"           N                                                                 N"
);
	V_const(&window.north,'d',6,0,10);
	V_const(&window.north,'d',6,66,10);
	V_line(19,
"           E                                                                 E"
);
	V_const(&window.west,'d',19,1,10);
	V_const(&window.south,'d',20,0,10);
	V_line(20,
"           N                                                                 N"
);
	V_const(&window.east,'d',19,67,10);
	V_const(&window.south,'d',20,66,10);

	V_line(1,"		  Enter UTM coordinates at corners of polygon");
	V_line(2,"	     Easting coordinates first, Northing coordinates second");
	V_line(3,"		  UTM window limits shown in corners of screen");
	V_line(4,
"=============================================================================="
);
	V_line(21,
"=============================================================================="
);

	switch (*sides) 
	{
	case 3: vask3(); break;
	case 4: vask4(); break;
	case 5: vask5(); break;
	case 6: vask6(); break;
	case 7: vask7(); break;
	case 8: vask8(); break;
	}

    }
    else
    {
	xy_index = 0;
	line =  8;
	start = 0;

	*sides = 0;
	V_line(10,"     You have chosen to define your area of interest ");
	V_line(11,"	 using a polygon with more than 8 sides.");
	V_line(13,"     Please enter the number of sides");
	V_ques(sides,'i',13,39,2);

	V_call();
	V_clear();

	if(*sides == 0)
	{
	    fprintf(stderr,"EXITING at user request");
	    sleep(5);
	    exit(-1);
	}

	north = (float *)G_calloc(*sides,4);
	east  = (float *)G_calloc(*sides,4);

	if(*sides < 10)
	    limit = *sides - 1;
	else
	    limit = 9;

	V_line(1,"		  Corner 1 can be any corner at your discretion.");
	V_line(2,"	Other corners, however, must follow in clockwise, sequential order.");

	pages = *sides / 10 + 1;
	for(i = 1; i <= pages; i++)
	{
	    V_line(3,"		  UTM window limits shown in corners of screen");
	    V_line(4,
"=============================================================================="
);
	    V_line(5,
"           E                                                                 E"
);
	    V_const(&window.west,'d',5,1,10);
	    V_const(&window.east,'d',5,67,10);
	    V_line(6,
"           N                                                                 N"
);
	    V_const(&window.north,'d',6,0,10);
	    V_const(&window.north,'d',6,66,10);
	    V_line(19,
"           E                                                                 E"
);
	    V_const(&window.west,'d',19,1,10);
	    V_const(&window.east,'d',19,67,10);
	    V_line(20,
"           N                                                                 N"
);
	    V_const(&window.south,'d',20,0,10);
	    V_const(&window.south,'d',20,66,10);

	    V_line(22,
"=============================================================================="
);

	    vask9_up(start,limit);
	    for(j = start; j <= limit; j++)
	    {
		V_ques((east+xy_index),'f',line,30,10);
		V_ques((north+xy_index),'f',line,53,10);

		line++;
		xy_index++;
	    }

	    start += 10;
	    if(*sides > (limit + 10))
		limit += 10;
	    else
		limit  = *sides - 1;

	    line =  8;

	    V_call();
	    V_clear();
	}
	for(i = 0; i < *sides; i++)
	{
	    *(Ux+i) = *(east+i);
	    *(Uy+i) = *(north+i);
	}
    }
}
