#include "v.bubble.h"

int bubbling(SITE_XYZ *bsite, int nsites, struct Map_info *map, double units)
{
    int i1,i2,i3;
    double max=0,min=0;
    double radfrac,theta;
    double radius,e,n;
    double max_x=0,min_x=0,max_y=0,min_y=0;
    double x[361],y[361];
    struct line_pnts *pnts;
    struct dig_head *local_head;
    char today[20];
    char input[1024], output[1024];
    
    pnts=Vect_new_line_struct();
    
    for (i1=0;i1<nsites;i1++) {
	if (bsite[i1].z>max) max=bsite[i1].z;
	if (min) {
	    if (bsite[i1].z<min) min=bsite[i1].z;
	} else {
	    min=bsite[i1].z;
	}
    }
   
	
    printf("Found minimum: %f\n      maximum: %f\n",min,max);
    radfrac=((double) units/(max-min)) ;/*radius fraction*/
    /* Calculate "theta". "theta" is the value in radians of 1 degree of
     * angle. 
     */
    theta = ((double)2.0 * (double)M_PI) / (double)360.0;
    i1=0;
    
    while (i1<nsites) {
       	for (i2=0;i2<=360;i2++) {
       	    
	    radius=((double)radfrac*(bsite[i1].z-min));
	    e =  radius * cos( (double)(i2) * theta );
            n =  radius * sin( (double)(i2) * theta );
            
	    x[i2] = bsite[i1].x + e;
            y[i2] = bsite[i1].y + n;
    
    	    if (!max_x) {
                max_x = x[i2];
                min_x = x[i2];
                max_y = y[i2];
                min_y = y[i2];
            }
    
    	    if (x[i2] > max_x) max_x = x[i2];
            if (x[i2] < min_x) min_x = x[i2];
            if (y[i2] > max_y) max_y = y[i2];
            if (y[i2] < min_y) min_y = y[i2];
        }
        Vect_copy_xy_to_pnts(pnts,x,y,(int)361);
        Vect_write_line(map,AREA,pnts);
        i1++;
    }
    /* Initialize "dig_head" structure "local_head" with vector info. */
  local_head = (struct dig_head *) G_malloc (sizeof(struct dig_head));
  Date(today);
  strcpy(local_head->organization,"");
  strcpy(local_head->date,today);
  strcpy(local_head->your_name,"");
  sprintf(local_head->map_name,
"File created by \"%s\".", G_program_name() );
  strcpy(local_head->source_date,"");
  /* "orig_scale" arbitrarily set to 24000 */
  local_head->orig_scale = (long) 24000;
  sprintf(local_head->line_3,
"Center(s) of circle(s) from \"%s\".",
          input );
  local_head->plani_zone = G_zone();
  local_head->W = min_x;
  local_head->E = max_x;
  local_head->S = min_y;
  local_head->N = max_y;
  /* "digit_thresh" arbitrarily set to 0 */
  local_head->digit_thresh = 0.0;
  /* "map_thresh" arbitrarily set to 0 */
  local_head->map_thresh = 0.0;
  /* Copy contents of "local_head" to "map.head". */
  Vect_copy_head_data(local_head,&map->head);
  /* "Vect_close" will write the header for the vector file. */
  Vect_close(map);
 
    
    
    return i1;
}
