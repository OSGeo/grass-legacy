/**************************************************************************/
/***									***/
/***                          disp_graph()				***/
/***  		Displays graph of parameter values with scale.		***/
/***               Jo Wood,Dept. of Geography, 4th December 1995	***/
/***									***/
/**************************************************************************/

#include "raster.h"
#include "display.h"
#include "param.h"
#include "local_proto.h"


int disp_graph (
    int scr_x,
    int scr_y,			/* Mouse screen coords.		*/
    int *param_ptr,	  		/* Array storing paramtr values */
    int mwsize				/* Maximum window size.		*/
)
{

    /*------------------------------------------------------------------*/
    /*        			INITIALISE			 	*/
    /*------------------------------------------------------------------*/

    int   bot,top,lef,rit,		/* Window boundaries.		*/
	  x,y,				/* Plotting coorinates.		*/
	  n,				/* Number of samples.		*/

	  yaxis,yinc,			/* Y axis label counter and inc.*/

	  size,				/* Local window sizes.		*/

	  param,			/* Parameter value.		*/
	  pmin=99999,
	  pmax=-99999;			/* Range of parameter values.	*/

    char  text[128];			/* Text buffer for labels.	*/

    double mean   = 0.0,		/* Distribution measures.	*/
	   stdev  = 0.0,
	   entrop = 0.0,
	   fp[6];			/* Feature probabilities.	*/

    for (size=0; size<6; size++)	/* Initialise feature probs.	*/
	fp[size] = 0.0;

    n = (wsize-1)/2;			/* Number of samples.		*/


    /*------------------------------------------------------------------*/
    /*        		HIGHLIGHT CURRENT SELECTION		 	*/
    /*------------------------------------------------------------------*/

    R_standard_color(D_translate_color("red"));
    plot_cross(scr_x,scr_y);


    /*------------------------------------------------------------------*/
    /*			    SETUP GRAPH WINDOW				*/
    /*------------------------------------------------------------------*/

    D_set_cur_wind(graph_frame);
    D_get_screen_window(&top,&bot,&lef,&rit);
    D_set_clip_window(top,bot,lef,rit);

    top +=2;				/* Allow small outer border.	*/
    bot -=2;
    lef +=2;
    rit -=2;

    R_standard_color(D_translate_color("white"));
    D_erase_window();


    /*------------------------------------------------------------------*/
    /*			    	LABEL GRAPH				*/
    /*------------------------------------------------------------------*/

    R_standard_color(D_translate_color("black"));
    R_font("romanc");
    R_text_size((rit-lef)/25,(bot-top)/25);
    R_move_abs(lef,top + (bot-top)/25);

    switch(mparam)
    {
	case(ELEV):
	    sprintf(text,"Elevation");
	    break;
	case(SLOPE):
	    sprintf(text,"Slope");
	    break;
	case(ASPECT):
	    sprintf(text,"Aspect");
	    break;
	case(PROFC):
	    sprintf(text,"Profile Curvature");
	    break;
	case(PLANC):
	    sprintf(text,"Plan Curvature");
	    break;
	case(LONGC):
	    sprintf(text,"Longitudinal Curviture");
	    break;
	case(CROSC):
	    sprintf(text,"Cross-sectional Curvature");
	    break;
	case(MINIC):
	    sprintf(text,"Minimum Curvature");
	    break;
	case(MAXIC):
	    sprintf(text,"Maximum Curvature");
	    break;
	case(FEATURE):
	    sprintf(text,"Feature Classification");
	    break;
	case(RESID):
	    sprintf(text,"Quadratic Residuals");
	    break;
	case(MORAN):
	    sprintf(text,"Local Moran");
	    break;
	case(GEARY):
	    sprintf(text,"Local Geary");
	    break;
	default:
	    fprintf(stderr,"Error: Unknown terrain parameter.\n");
	    exit();
    }	
    R_text(text);
    if (text_out)
	fprintf(stdout, "%s\n",text);

    lef += (rit-lef)/10;
    top += (bot-top)/10;

    D_move_abs(lef,bot);
    D_cont_abs(rit,bot);
    D_move_abs(lef,bot);
    D_cont_abs(lef,top);

    R_text_size((rit-lef)/40,(bot-top)/25);

	
    /*------------------------------------------------------------------*/
    /*			    	LABEL AXES				*/
    /*------------------------------------------------------------------*/

    if (mparam==FEATURE)
    {
	pmin=0;
	pmax=NUM_CATS-1;

	R_move_abs(lef-(rit-lef)/10,bot - FLAT*(bot-top)/6);
	R_text("Planr");

	R_move_abs(lef-(rit-lef)/10,bot - PIT*(bot-top)/6);
	R_text("Pit");

	R_move_abs(lef-(rit-lef)/10,bot - CHANNEL*(bot-top)/6);
	R_text("Chanl");

	R_move_abs(lef-(rit-lef)/10,bot - PASS*(bot-top)/6);
	R_text("Pass");

	R_move_abs(lef-(rit-lef)/10,bot - RIDGE*(bot-top)/6);
	R_text("Ridge");

	R_move_abs(lef-(rit-lef)/10,bot - PEAK*(bot-top)/6);
	R_text("Peak");
    }
    else
    {			 /* Get parameter range and graph scaling.	*/
    	for (size=3; size<=wsize; size += 2)
    	{
	    param = *(param_ptr + (size-3)/2);

	    if (param < pmin)
	    	pmin = param;

	    if (param > pmax)
	    	pmax = param;

	    if (fix_axis)
	    {
		if (mparam == SLOPE)
		{
		    pmin=0;
		    pmax=90;
		}
		else
		    if (mparam == MORAN)
		    {
			pmin=-1000;
			pmax= 1000;
		    }
	    }
	}

    	yinc = (int)ceil((pmax-pmin)/10.0);

    	for (yaxis = pmin; yaxis <=pmax; yaxis++)
	    if (yaxis%yinc == 0)
	    {
	    	R_move_abs(lef-(rit-lef)/10,bot - (yaxis-pmin)*(bot-top)/
								(1+pmax-pmin));
	    	sprintf(text,"%d",yaxis);
    	    	R_text(text);
	    	plot_cross(lef,bot- (yaxis-pmin)*(bot-top)/(1 + pmax-pmin));
	    }
     }

    /*------------------------------------------------------------------*/
    /*		PLOT GRAPH VALUES AND CALCULATE PRIMARY STATS		*/
    /*------------------------------------------------------------------*/


    param = *(param_ptr + (size-3)/2);
    D_move_abs(lef,bot - (param-pmin)*(bot-top)/(1+ pmax-pmin));

    for (size=3; size<=wsize; size += 2)
    {
	param = *(param_ptr + (size-3)/2);
	if (fix_axis)
	    x = lef + (size-3)*(rit-lef)/(mwsize-1);
	else
	    x = lef + (size-3)*(rit-lef)/(wsize-1);

	y = bot - (param-pmin)*(bot-top)/(1+pmax-pmin);

	if (mparam==FEATURE)
	{
	    plot_box(x,y,top,bot,lef,rit,param);
	    fp[param]++;
	}
	else
	{
	    D_cont_abs(x,y);
	    plot_cross(x,y);
	    mean += param;
	}

	if (text_out)
	    fprintf(stdout, "%d\t%d\n",size,param);
    }

    /*------------------------------------------------------------------*/
    /*		CALULATE AND DISPLAY SECONDARY STATISTICS		*/
    /*------------------------------------------------------------------*/

    if (mparam == FEATURE)
    {
    	for (size=0; size<6; size++)	/* Rescale feature probabilities*/
	{
	    fp[size] /= n;
	    if (fp[size] != 0.0)
	    	entrop += -fp[size]*log(fp[size]);
	}
	fprintf(stdout, "Scaled Entropy\t%.3f\n", entrop/EMAX);
    }
    else
    {
    	mean /= n;

    	for (size=3; size<=wsize; size += 2)
    	{
	    param = *(param_ptr + (size-3)/2);
	    stdev += (mean-param)*(mean-param);
    	}
	stdev = sqrt(stdev/n);

	fprintf(stdout, "Mean\t%.3f\nStdev\t%.3f\n",mean,stdev);
    }


    /*------------------------------------------------------------------*/
    /*		      CLOSE DOWN AND RESET TO DEM WINDOW		*/
    /*------------------------------------------------------------------*/


    R_flush();
    D_set_cur_wind(dem_frame);
    D_set_clip_window_to_map_window();

    return 0;
}

/******************************************************************/
/***			 Cross Plotting function. 		***/
/******************************************************************/


int plot_cross (
    int scr_x,
    int scr_y		/* Centre of cross to plot.	*/
)
{
    D_move_abs(scr_x-2,scr_y-2);
    D_cont_rel(5,5);
    D_move_abs(scr_x-2,scr_y+2);
    D_cont_rel(5,-5);
    D_move_abs(scr_x,scr_y);
    return 0;
}


/******************************************************************/
/***			 Box Plotting function. 		***/
/******************************************************************/

int plot_box (
    int scr_x,
    int scr_y,		/* Centre of box to plot.	*/
    int top,
    int bot,
    int lef,
    int rit,	/* Graph boundaries.		*/
    int feature		/* Morphometric feature.	*/
)
{
    int n;
    n = (wsize-1)/2;		/* Number of samples.		*/

    switch (feature)
    {
	case FLAT:
    	    R_standard_color(D_translate_color("gray"));
	    break;
	case PIT:
    	    R_standard_color(D_translate_color("black"));
	    break;
	case CHANNEL:
    	    R_standard_color(D_translate_color("blue"));
	    break;
	case PASS:
    	    R_standard_color(D_translate_color("green"));
	    break;
	case RIDGE:
    	    R_standard_color(D_translate_color("yellow"));
	    break;
	case PEAK:
    	    R_standard_color(D_translate_color("red"));
	    break;
	default:
	    fprintf(stderr,"Error: Unknown terrain feature.\n");
	    exit();
    }

    R_box_abs(scr_x,scr_y-(bot-top)/10,scr_x+(rit-lef)/n,scr_y);

    R_standard_color(D_translate_color("black"));
    D_move_abs(scr_x,scr_y);
    D_cont_abs(scr_x,scr_y-(bot-top)/10);
    D_cont_abs(scr_x+(rit-lef)/n,scr_y-(bot-top)/10);
    D_cont_abs(scr_x+(rit-lef)/n,scr_y);
    D_cont_abs(scr_x,scr_y);

    return 0;
}
