/*********************** Documentation Start ***********************
 
NAME:
 
 ****************************************************
 *                                                  *
 *  CHANTR55 procedure        created  6/5/92    mak *
 *                                                  *
 ****************************************************
 
This procedure calculates peak flow in loop3.
 
 
SYNOPSIS
**********************************************************************/


/*********************************************************************/
/* Modified 03/21/95 by John Witte to print out cell time of concen- */
/* tration values for our debugging session.... -JW                  */
/*     Frank G.   Fred T.   Bob Y.   Kevin B.   John W.              */
/*********************************************************************/
 
 
/* Included header files */
#ifdef _DOS
 
 #include <stdio.h>
 #include <math.h>
 #include <stdlib.h>
 #include <string.h>
 #include <float.h>
 #include "input.h"
 #include "binary.h"
 #include "debugflg.h"


#else

#include <stdio.h>
#include <math.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"


#endif

/* Included external structures */

extern int             debuginfo;
extern INITIAL_INFOPTR initialptr;
extern COLUMN_INFOPTR *columndata;
extern int	      hydro_info;

extern HYDRO_ROUTE_REC_PTR hydro_route;

extern FLAGS_BASIC     bflags;
extern FLAGS_ROUTINE   rflags;
extern FLAGS_TABLE     tflags;
extern HYDRO_TABLE     htable;

#ifdef _UNIX_K_AND_R

void chantr55(col_num, length, area, temp_cn, drainage_area, peak_flow)

     int col_num;
     float length;
     float area;
     float temp_cn;
     float drainage_area;
     float *peak_flow;

#else

 void chantr55(int col_num, float length, float area,
	      float temp_cn, float drainage_area, float *peak_flow)

#endif

{


/* LOCAL VARIABLES */

SOURCEPTR cells_in = columndata[col_num]->sourcelist;
float sheet_flow_length;
float shallow_length = 0.0;
float channel_length =0.0;
float velocity_shallow = 0.0;
float r = 0.0;
float channel_velocity = 0.0;
float temp = 0.0;
float temp1 = 0.0;
float temp2 = 0.0;
float temp3 = 0.0;
float calc_length = 0.0;
float total_time = 0.0;
float cell_time = 0.0;
float initial_abstraction = 0.0;
float iap = 0.0;
float c0,c1,c2;
float qu = 0.0;
float q = 0.0;
float s = 0.0;
int   counter = 0;

 
/************************************************************
DESCRIPTION:
 
 This routine calculates the time of channelized flow for a cell.  The
 overland and shallow flow are from the primary cell that is on the
 longest flow path.
 
RETURNS:
 
peak flow
 
NOTES:
 
   set up the constants for the co,c1,c2 coefficients.
   These coefficients were found in the SCS publication
   entitled Urban Hydrology for Small Watersheds.  The first
   column is co, the second c1, and the third is c3.
 
 
DATA STORES:
 
HISTORY:
 
DATE      BUG #  PROG    Desc.
 
08/28/92         MAK     Created procedure
 
SEE ALSO:
cellcalc, loop1tr5, tr55prim
 
 
*********************** Documentation End *****************************/
 

 
 
 
 /* Use rectangular calculations for width depth */
 
 
 if(initialptr->geomorphic_calc == NONGEOMORPHIC)  /* user selected non-geomorphic */
    {

     if (bflags.tr_55)
	puts("Function Wsdtr55 , Non-Geomorphic");


     calc_length = length;

    }

 else
   {
 
     calc_length = columndata[col_num]->channel->tr55_length;

   }

 channel_length = calc_length;

 if (((columndata[col_num]->channel->channel_indicator != 0) &&
       (columndata[col_num]->soil->soil_type != 5)           &&
        (channel_length <= 0.0))                             ||
     (channel_length < 0.0))
   {
#ifdef _DOS
    fprintf (stderr,"ERROR: Channel segment length in column %d < 0.0\n",col_num);
#endif
    exit(1);
   }


 if(columndata[col_num]->sourcelist == NULL)
   {

    if(calc_length >= 150 )
      {
       sheet_flow_length = 150;
      }
    else
      {
       sheet_flow_length = calc_length;
       if(sheet_flow_length <0.0)
	 sheet_flow_length=0.0;
      }

    if(calc_length >= 300)
      {
       shallow_length = 150;
      }
    else
      {
       shallow_length = (calc_length-150);
       if(shallow_length < 0.0)
	shallow_length = 0.0;
      }

   channel_length = calc_length-sheet_flow_length-shallow_length;

   columndata[col_num]->runoff->time_overland = .007*
	pow((columndata[col_num]->overland_mannings *
	sheet_flow_length),0.8) / (pow(initialptr->storm_rainfall,0.5)
	* pow((columndata[col_num]->slope->average_land_slope/100),0.4));




   /* Check here to see what to use if the cell is a water cell */

   velocity_shallow = 16.1345*pow((columndata[col_num]->slope->
       average_land_slope/100),0.5);

   if(velocity_shallow>2.0)
      velocity_shallow=2.0;

   if(hydro_info)
     hydro_route->velocity_shallow=velocity_shallow;

   columndata[col_num]->runoff->time_shallow = shallow_length/
			    (3600 * velocity_shallow);


  } /* end calculations on primary cell */

/*  if((columndata[col_num]->channel->channel_indicator == 0) ||
	(columndata[col_num]->soil->soil_type == 5))
      {
       channel_velocity = 1.0;
      }

    else */
      {
   if((columndata[col_num]->channel->channel_indicator == 0) ||
	 (columndata[col_num]->soil->soil_type == 5))

	  r = (columndata[col_num]->channel->width *
	   columndata[col_num]->channel->max_depth) /
	   (columndata[col_num]->channel->width +
	   (columndata[col_num]->channel->max_depth * 2));
       else
	  r = (columndata[col_num]->channel->width *
	   columndata[col_num]->channel->depth) /
	   (columndata[col_num]->channel->width +
	   (columndata[col_num]->channel->depth * 2));


       channel_velocity=(1.49*pow(r,0.66666666666)*
	    pow((columndata[col_num]->channel->channel_slope/100)
	    ,0.5))/columndata[col_num]->channel->channel_mannings;


 
      }
 
       columndata[col_num]->runoff->time_concentrated=
	       channel_length /
	       (3600*channel_velocity);
 
    if (bflags.tr_55)
	{
          fprintf (stderr,"Column %d:\n",col_num);
	  fprintf (stderr,"INPUT: Width %f  Depth %f  Chan. slope %f Chan. mannings %f \n",
		columndata[col_num]->channel->width,
		columndata[col_num]->channel->depth,
		columndata[col_num]->channel->channel_slope,
		columndata[col_num]->channel->channel_mannings);
	  fprintf (stderr,"       Channel Length %f \n",calc_length);
	  fprintf (stderr,"INTER: hydraulic radius %f chan velocity %f \n",
		 r,channel_velocity);
	}
 
 
  /* calculate the total time of the flows through the cell */
 
   if(columndata[col_num]->sourcelist == NULL)
 
      total_time=columndata[col_num]->runoff->time_overland +
	       columndata[col_num]->runoff->time_shallow +
	       columndata[col_num]->runoff->time_concentrated;
 
   else
      total_time = columndata[col_num]->runoff->time_previous +
		   columndata[col_num]->runoff->time_concentrated;
 
 
/* Added this to make change according to Fred */
 
   cell_time = total_time;
 
 
/*   if((columndata[col_num]->channel->channel_indicator != 0) &&
	 (columndata[col_num]->soil->soil_type != 5))
 
    {
     if(total_time >10.0)
       {
	fprintf (stderr,"Total time of flow has exceeded the 10 hour limit imposed by TR55\n");
	exit(1);
       }
    } */
 
 
   if (bflags.tr_55)
     {
       fprintf (stderr,"overland time %f hours",columndata[col_num]->runoff->time_overland);
       fprintf (stderr,"Shallow time %f hours",columndata[col_num]->runoff->time_shallow);
       fprintf (stderr,"total time %f hours\n",total_time);
       fprintf (stderr,"conc. time %f hours\n",columndata[col_num]->runoff->time_concentrated);
     }
 
   columndata[col_num]->runoff->total_time=total_time;
   columndata[col_num]->runoff->time_previous = total_time;
 
 
   initial_abstraction=(200/(temp_cn/area))-2;

   iap=initial_abstraction/initialptr->storm_rainfall;

   if(hydro_info)
     hydro_route->iap=iap;
   if(bflags.tr_55)
     fprintf (stderr,"IAP value= %f",iap);

   if(iap< 0.1)
     iap=0.1;

   if(iap>0.5)
     iap=0.5;

   if ( (strcmp(initialptr->storm_type, "I")==0) ||
       (strcmp(initialptr->storm_type, "1")==0)   )
    {
	c0=2.3426-(.0103*iap)-(2.7333*iap*iap);
	c1=-0.4022-(1.4456*iap)+(4.2539*iap*iap);
	c2=0.1113-(0.0402/iap)+(0.00000401806/(iap*iap));

	/* Changed by Fred 3/23/95
          c0=2.3426-(.0103*iap)-(2.7333*pow(iap,2.0));
	  c1=-0.4022-(1.4456*iap)+(4.2539*pow(iap,2.0));
	  c2=0.1113-(0.0402/iap)+(0.00000401806/pow(iap,2.0));
        */
    }



  else if((strcmp(initialptr->storm_type, "IA")==0) ||
     (strcmp(initialptr->storm_type, "Ia")==0) ||
     (strcmp(initialptr->storm_type, "1A")==0) ||
     (strcmp(initialptr->storm_type, "1a")==0)    )
 
 
   {
     c0=1.3405+(0.1566/iap)-(0.0087/(iap*iap));
     c1=0.1155-(0.1201/iap)+(0.0077/(iap*iap));
     c2=-0.2784+(0.1517*iap)-(1.9078*iap*iap);

   }
 
 
  else if((strcmp(initialptr->storm_type, "II")==0) ||
     (strcmp(initialptr->storm_type, "2" )==0)    )

 
    {
	c0= 2.5273+(0.4759*iap)-(2.2349*iap*iap);
	c1=-0.5584-(0.7081*iap)+(1.5555*iap*iap);
        c2=-0.3321+(0.6135*iap)+(0.0106/iap);

    }
 
 
  else
 
    {
      c0=2.4577+(0.3301*iap)-(1.7740*iap*iap);
      c1=-0.4627-(0.7397*iap)+(1.8622*iap*iap);
      c2=-0.187+(0.1799*iap);
    }
 
 
  if(bflags.tr_55)
     fprintf (stderr,"coeff. 1= %f coeff 2= %f coeff 3= %f",c0,c1,c2);
 
 
   if(cells_in != NULL)
    {
      temp1=c0;
      temp2=(c1*log10(cell_time));
      temp3=(c2*pow(log10(cell_time),2.0));
 
      temp=temp1+temp2+temp3;

    }
 
   else
    {
       temp1=c0;
       temp2=(c1*log10(total_time));
       temp3=(c2*pow(log10(total_time),2.0));
 
       temp=temp1+temp2+temp3;
     }
 
 
  qu=pow(10.0,temp);
 
 
  s=(1000/(temp_cn/area))-10;
 
 
  q=(pow((initialptr->storm_rainfall-(0.2*s)),2.0))/(initialptr->storm_rainfall
     +(0.8*s));
 
  if(bflags.tr_55)
    fprintf (stderr,"\n retention factor= %f runoff vol= %f",s,q);
 
 
  *peak_flow=qu*(drainage_area/640)*q;



  if (tflags.hydro_table)
   {
    if(columndata[col_num]->sourcelist == NULL) /* If Primary...  */
     {
      htable.ht[col_num].ol_length         = sheet_flow_length;
      htable.ht[col_num].ol_travel_time    = columndata[col_num]->runoff->time_overland;

      htable.ht[col_num].sc_length         = shallow_length;
      htable.ht[col_num].sc_velocity       = velocity_shallow;
      htable.ht[col_num].sc_travel_time    = columndata[col_num]->runoff->time_shallow;
     }

    else                                        /* NON-Primary... */
     {
      htable.ht[col_num].ol_length         = 0.0;
      htable.ht[col_num].ol_travel_time    = 0.0;

      htable.ht[col_num].sc_length         = 0.0;
      htable.ht[col_num].sc_velocity       = 0.0;
      htable.ht[col_num].sc_travel_time    = 0.0;
     }

    htable.ht[col_num].ch_top_width      = columndata[col_num]->channel->width;
    htable.ht[col_num].ch_depth_bankfull = columndata[col_num]->channel->depth;
    htable.ht[col_num].ch_seg_length     = channel_length;
    htable.ht[col_num].ch_velocity       = channel_velocity;
    htable.ht[col_num].ch_travel_time    = columndata[col_num]->runoff->time_concentrated;
    htable.ht[col_num].ch_time_of_conc   = total_time;
   }


  if(hydro_info)
   {
    hydro_route->channel_length=length;
    hydro_route->cn_area=area;
    hydro_route->tr55_length=columndata[col_num]->channel->tr55_length;
    hydro_route->source_list=columndata[col_num]->sourcelist;
    hydro_route->slope_length=sheet_flow_length;
    hydro_route->shallow_length=shallow_length;
    hydro_route->overland_mannings=columndata[col_num]->overland_mannings;
    hydro_route->storm_rainfall=initialptr->storm_rainfall;
    hydro_route->average_land_slope=
	    columndata[col_num]->slope->average_land_slope;
    hydro_route->time_overland=columndata[col_num]->runoff->time_overland;
    hydro_route->velocity_shallow_after=velocity_shallow;
    hydro_route->time_shallow=columndata[col_num]->runoff->time_shallow;
    hydro_route->channel_indicator=
	    columndata[col_num]->channel->channel_indicator;
    hydro_route->soil_type=columndata[col_num]->soil->soil_type;
    hydro_route->channel_width=columndata[col_num]->channel->width;
    hydro_route->max_depth=columndata[col_num]->channel->max_depth;
    hydro_route->r=r;
    hydro_route->channel_depth=columndata[col_num]->channel->depth;
    hydro_route->channel_velocity=channel_velocity;
    hydro_route->time_concentrated=
	    columndata[col_num]->runoff->time_concentrated;
    hydro_route->total_time=total_time;
    hydro_route->time_previous=columndata[col_num]->runoff->time_previous;
    hydro_route->temp_cn=temp_cn;
    hydro_route->initial_abstraction=initial_abstraction;
    hydro_route->iap_after=iap;
    for(counter=1;counter<=4; counter++)
	  hydro_route->storm_type[counter]=initialptr->storm_type[counter];
    hydro_route->c0=c0;
    hydro_route->c1=c1;
    hydro_route->c2=c2;
    hydro_route->temp1=temp1;
    hydro_route->temp2=temp2;
    hydro_route->temp3=temp3;
    hydro_route->temp=temp;
    hydro_route->qu=qu;
    hydro_route->s=s;
    hydro_route->q=q;
    hydro_route->peak_flow=*peak_flow;
   }
/*  Added this line for verification -JW 03/21/95     (SUPP!) */
/*
  fprintf (stderr,"Col: %d  Tot: %f  ",col_num,total_time);
  fprintf (stderr,"IAP: %f C0: %f C1: %f C2: %f Qu: %f DA: %f \n",iap,c0,c1,c2,qu,drainage_area); 
  fprintf (stderr,"T1: %f T2: %f T3: %f temp: %f", temp1, temp2, temp3, temp);
*/
  if (bflags.tr_55)
     fprintf (stderr,"peak flow %f ft/sec\n",*peak_flow);
}
