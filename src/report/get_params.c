#include "param.h"

get_params()
{
	int i,n;

V_clear();

V_line( 1,
" The 'MAP LAYER CATEGORY REPORT' presents the categories of a map layer.");
V_line( 2,
" You may choose to have the relative area covered by each category reported");
V_line( 3,
" in one or more units (below).  If you do not choose a unit of measure,");
V_line( 4,
" only the category names and corresponding cell values will be reported.");
V_line( 6,
"please put an 'x' next to those report parameters you wish to have included");
V_line( 8,"            # cells");
V_line( 9,"            % cover (including no data areas)");
V_line(10,"            % cover (excluding no data areas)");
V_line(11,"            cover measured in acres");
V_line(12,"            cover measured in hectares");
V_line(13,"            cover measured in square kilometers");
V_line(14,"            cover measured in square miles");

	for(i = 0; i < 7; i++){
		param[i][0] = 0;
		V_ques(param[i],'s',i+8,9,2);
	}

	V_intrpt_ok();
	if(!V_call())
		exit(0);

	n = 0;
	for(i = 0; i < 7; i++)
	    if(param[i][0])
		n++;

	return(n);
}
