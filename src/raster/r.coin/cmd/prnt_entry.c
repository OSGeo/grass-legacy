#include "coin.h"

#define F_CTOK(C)	((double)(C))/1000000.0
#define F_CTOM(C)	F_CTOK(C) *   0.3861
#define F_CTOA(C)	F_CTOK(C) * 247.1000
#define F_CTOH(C)	F_CTOK(C) * 100.0000

#define F_CTOP(C,R) ((int)R) ? (double)C / (double)R * 100.0 : 0.0
#define F_CTOX(C,R) ((int)R) ? (double)C / (double)R * 100.0 : 0.0
#define F_CTOY(C,R) ((int)R) ? (double)C / (double)R * 100.0 : 0.0

print_entry(Conformat,count,area)
    char Conformat;
    long count;
    double area;
{
    long total_count;
    double total_area;

    switch (Conformat)
    {
	case 'a': 
		print_area(F_CTOA(area));
		break;
	case 'h': 
		print_area(F_CTOH(area));
		break;
	case 'k': 
		print_area(F_CTOK(area));
		break;
	case 'm': 
		print_area(F_CTOM(area));
		break;
	case 'p': 
		print_percent(F_CTOP(area, window_area));
		break;
	case 'x':
		col_total(Cndex,1,&total_count,&total_area);
		print_percent(F_CTOX(area,total_area)); 
		break;
	case 'y':
		row_total(Rndex,1,&total_count,&total_area);
		print_percent(F_CTOY(area,total_area));
		break;
	default:  
		fprintf(dumpfile," %9ld |", count);
		break;
    }
}

print_area (value)
    double value;
{
    char buf[20];

    format_double (value, buf, 9);
    fprintf(dumpfile," %9s |",buf);
}

print_percent (value)
    double value;
{
    fprintf(dumpfile," %9.2lf |",value);
}
