struct windows
{
	char *name ;
	float bot, top, left, right ;
} ;

#ifdef MAIN
struct windows windows[] =
	{
	{"loc", 94., 100.,   0.,  49.},
	{"nam", 87.,  93.,   0.,  49.},
	{"coo", 87., 100.,  50.,  86.},
	{"ref", 87., 100.,  87., 100.},
	{"leg", 20.,  86.,   0.,  30.},
	{"map",  0.,  86.,  31., 100.},
	{"lo1",  0.,  19.,   0.,  15.},
	{"lo2",  0.,  19.,  16.,  30.},
	{"lo3",  0.,  19.,   0.,  30.}
	} ;
#else
extern struct windows windows[] ;
#endif /* MAIN */

#define LOC	windows[0]
#define NAM	windows[1]
#define COO	windows[2]
#define REF	windows[3]
#define LEG	windows[4]
#define MAP	windows[5]
#define LO1	windows[6]
#define LO2	windows[7]
#define LO3	windows[8]
