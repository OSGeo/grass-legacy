				/********************************/
				/* 	r.le.pixel/texture.c	*/
		                /*                              */
				/*		2.1		*/
				/*				*/
		                /*       07/05/94 version       */
  		                /*                              */
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/

#include "r.le.pixel.h"


					/* set the maximum number of categories
					   that can be in the input map */

#define NO_OF_CATEGORIES cats.num

					/* set external pointers and routines */

extern struct CHOICE *choice;
extern int g_scale, g_unit;

					/* define a structure used to read in
					   info about categories in the map */

struct Categories cats;
 
char *G_get_cat();

					/* declare a counter for the number
					   of pixels with non-zero values
					   (total) in cal_edge and a flag for
					   pixels with 0 attribute in 
					   cal_divers */

int total, zero;




					/* MOVING WINDOW ANALYSIS DRIVER */

void   mv_texture(nrows, ncols, buf, value, index, rich, cnt, cntwhole)
int  **buf, nrows, ncols, **value, index, *rich, cnt, cntwhole;
{
  int           lc, *atts, *edgeatts, k; 
  register int  i, j;
  double    	attr[4], diver[4], edge[4], tex[5], **weight, **edgemat;

					/* set the contents of the arrays used
					   used to stored the results of the
					   diversity, texture, and edge 
					   calculations to zero */

  attr[0] = attr[1] = attr[2] = attr[3] = 0;
  diver[0] = diver[1] = diver[2] = diver[3] = 0;
  tex[0] = tex[1] = tex[2] = tex[3] = tex[4] = 0;
  edge[0] = edge[1] = edge[2] = edge[3] = 0;


					/*************************/
					/* The weight matrix in  */
					/* "r.le.para/weight"    */
					/* must have the follow- */
					/* ing format: a, b, c   */
					/*    = category values  */
					/*			 */
					/*      a     b   c    	 */
					/* a	0.0 0.1 0.1	 */
					/* b 	0.1 0.1 0.1      */
					/* c 	0.2 0.2 0.3 	 */
					/*************************/

					/* If juxtaposition is to be 
					   calculated, then dynamically 
					   allocate memory for the atts
					   array and the weight matrix */

  if(choice->jux[0]){
     atts = (int *)G_calloc(cntwhole, sizeof(int));
     weight = (double **)G_calloc(cntwhole, sizeof(double *));
     for(i = 0; i < cntwhole; i++)
        weight[i] = (double *)G_calloc(cntwhole, sizeof(double));

					/* read in the weight matrix */
      
     read_weight(cntwhole, atts, weight);
     total = 0;
  }


					/*************************/
					/* The edge matrix in    */
					/* "r.le.para/edge"      */
					/* must have the follow- */
					/* ing format: a, b, c   */
					/*    = category values  */
					/*			 */
					/*      a b c    	 */
					/* a	0 1 1	         */
					/* b 	1 0 1            */
					/* c 	1 1 0 	         */
					/*************************/

					/* If edge by type is to be 
					   calculated, then dynamically 
					   allocate memory for the edgeatts
					   array and the edgemat matrix */


  if(choice->edg[2]){
     edgeatts = (int *)G_malloc(cntwhole*sizeof(int));
     edgemat = (double **)G_malloc(cntwhole*sizeof(double *));
     for(i = 0; i < cntwhole; i++)
        edgemat[i] = (double *)G_malloc(cntwhole*sizeof(double));

					/* read in the edge matrix */
      
     read_edge(cntwhole, edgeatts, edgemat);
  }

  					/* main calculation loop.  Do the
					   calculations for each pixel in
					   the sampling area */

  for(i=1; i<nrows+1; i++) {
     for(j=1; j<ncols+1; j++) {

					/* find the sequence number of the
					   attribute in the richness array */

        lc = check_order(buf[i][j], rich); 

					/* based on the choices made, call
					   the appropriate calc. routine */
 
	if (choice->att[0])
	   cal_att(buf, i, j, nrows, ncols, attr);

        if (choice->te2[0])
           cal_tex(buf, i, j, nrows, ncols, lc, rich, cnt, tex);
 
        if (choice->edg[0] || choice->jux[0]) 
           cal_edge(buf, i, j, nrows, ncols, lc, edge, cntwhole,
		    atts, weight, edgeatts, edgemat, 0, 0);

      	if (choice->div[0]) 
           cal_divers(buf, i, j, nrows, ncols, lc, cnt, diver);
 	
     }
  }

  if (choice->jux[0]) {
     free(atts);
     for(i = 0; i < cntwhole; i++)
        free(weight[i]);
     free(weight);
  }

  if (choice->edg[2]) {
     free(edgeatts);
     for (i = 0; i < cntwhole; i++)
	free(edgemat[i]);
     free(edgemat);
  }   
  					/* put the calculated value for the
					   selected measure into the 
					   corresponding pixel in the output
					   map */

  if (choice->att[0]) {				/* ATTRIBUTE MEASURES */
     if (choice->att[1])
        value[index][0] = 10.0*attr[0];			/* Mean */
     if (choice->att[2])
        value[index][1] = 10.0*attr[1];			/* St. dev. */
     if (choice->att[3])
        value[index][2] = attr[2];			/* Min. */
     if (choice->att[4])
        value[index][3] = attr[3];			/* Max. */
  }

  if (choice->div[0]) {				/* DIVERSITY MEASURES */
     if (choice->div[1])
	value[index][4] = diver[0];			/* Richness X 1 */
     if (choice->div[2])
        value[index][5] = 100.0*diver[1];		/* Shannon X 100 */
     if (choice->div[3]) 
        value[index][6] = 100.0*diver[2];		/* Dominance X 100 */
     if (choice->div[4]) 
        value[index][7] = 100.0*diver[3];		/* Inv. Simpson X 100 */
  }

  if (choice->te2) {   				/* TEXTURE MEASURES */
     if (choice->te2[1])				/* Contagion X 100 */
        value[index][8] = 100.0*tex[0];
     if (choice->te2[2])				/* ASM X 1000 */
        value[index][9] = 1000.0*tex[1];
     if (choice->te2[3])				/* IDM X 1000 */
        value[index][10] = 1000.0*tex[2];
     if (choice->te2[4])				/* Entropy X 100 */
        value[index][11] = fabs(100.0*tex[3]);
     if (choice->te2[5])				/* Contrast X 10 */
        value[index][12] = 10.0*tex[4];  
  }
 
  if (choice->jux[0]) {				/* JUXTA. MEASURES */
     if (choice->jux[1])
        value[index][13] = 1000.0*edge[0];		/* Mean jux. X 1000 */
     if (choice->jux[2])	
        value[index][14]= 1000.0*edge[2];		/* St.dv. jux. X 1000 */
  } 

  if (choice->edg[0]) {				/* EDGE MEASURES */
     if (choice->edg[1])
        value[index][15] = edge[1];			/* Sum of edges X 1 */
     if (choice->edg[2])
	value[index][16] = edge[3];			/* Sum of edges 
							   by type X 1 */
  }
}




					/* WHOLE MAP, UNITS, OR REGIONS 
					   DRIVER */

void   df_texture(nrows, ncols, buf, rich, cnt, cntwhole)
int  **buf, nrows, ncols, *rich, cnt, cntwhole;
{
  FILE   	*fp0, *fp1, *fp2, *fp3, *fp4, *fp5;											
  int           lc, k, m, fd, *atts, *edgeatts, *edge1, *edge2, fc; 
  register int  i, j;
  double    	attr[4], diver[4], edge[4], tex[5], **weight, **edgemat;
  CELL		*zscor_buf, *edge_buf, **edgemap;


  struct Range ranges ;
  int    total_no_of_cells ;

					/* set the contents of the arrays used
					   used to stored the results of the
					   diversity, texture, and edge 
					   calculations to zero */

  attr[0] = attr[1] = attr[2] = attr[3] = 0;
  diver[0] = diver[1] = diver[2] = diver[3] = 0;
  edge[0] = edge[1] = edge[2] = edge[3] = 0;
  tex[0] = tex[1] = tex[2] = tex[3] = tex[4] = 0;

					/*************************/
					/* The weight matrix in  */
					/* "r.le.para/weight"    */
					/* must have the follow- */
					/* ing format: a, b, c   */
					/*    = category values  */
					/*			 */
					/*      a     b   c    	 */
					/* a	0.0 0.1 0.1	 */
					/* b 	0.1 0.1 0.1      */
					/* c 	0.2 0.2 0.3 	 */
					/*************************/

					/* If juxtaposition is to be 
					   calculated, then dynamically 
					   allocate memory for the atts
					   array and the weight matrix */

  if(choice->jux[0]){
     atts = (int *)G_calloc(cntwhole, sizeof(int));
     weight = (double **)G_calloc(cntwhole, sizeof(double *));
     for(i = 0; i < cntwhole; i++)
        weight[i] = (double *)G_calloc(cntwhole, sizeof(double));

					/* read in the weight matrix */
      
     read_weight(cntwhole, atts, weight);
     total = 0;
  }


					/*************************/
					/* The edge matrix in    */
					/* "r.le.para/edge"      */
					/* must have the follow- */
					/* ing format: a, b, c   */
					/*    = category values  */
					/*			 */
					/*      a b c    	 */
					/* a	0 1 1	         */
					/* b 	1 0 1            */
					/* c 	1 1 0 	         */
					/*************************/

					/* If edge by type is to be 
					   calculated, then dynamically 
					   allocate memory for the edgeatts
					   array and the edgemat matrix */


  if(choice->edg[2]){
     edgeatts = (int *)G_calloc(cntwhole, sizeof(int));
     edgemat = (double **)G_calloc(cntwhole, sizeof(double *));
     for(i = 0; i < cntwhole; i++)
        edgemat[i] = (double *)G_calloc(cntwhole, sizeof(double));

					/* read in the edge matrix */
      
     read_edge(cntwhole, edgeatts, edgemat);
  }


                      			/* dynamically allocate storage for the
                                   	   buffer that will hold the map of the
                                   	   edges */

  if (choice->edgemap) {
     edgemap = (CELL **)G_calloc(nrows + 3, sizeof(CELL *));
     for(i = 0; i < nrows + 3; i++)
        edgemap[i] = (CELL *)G_calloc(ncols + 3, sizeof(CELL));
  }

  					/* main calculation loop.  Do the
					   calculations for each pixel in
					   the map */

  for(i = 1; i < nrows+1; i++) {
     for(j = 1; j < ncols+1; j++) {

					/* find the sequence number of the
					   attribute in the richness array */
   
        lc = check_order(buf[i][j], rich); 

					/* based on the choices made, call
					   the appropriate calc. routine */
 	
	if (choice->att[0])
	   cal_att(buf, i, j, nrows, ncols, attr); 

        if (choice->div[0]) 
           cal_divers(buf, i, j, nrows, ncols, lc, cnt, diver);

        if (choice->jux[0] || choice->edg[0]) {
	   edge1 = edge2 = 0;
           cal_edge(buf, i, j, nrows, ncols, lc, edge, cntwhole,
		    atts, weight, edgeatts, edgemat, &edge1, &edge2);
	   if (choice->edgemap) {
	      if (edge1) {
	         *(*(edgemap + i) + j) = *(*(buf + i) + j);
		 *(*(edgemap + i + 1) + j) = *(*(buf + i + 1) + j);
	      }
	      if (edge2) {
	         *(*(edgemap + i) + j) = *(*(buf + i) + j);
		 *(*(edgemap + i) + j + 1) = *(*(buf + i) + j + 1);
	      }
	   }
	}

        if (choice->te2[0])
           cal_tex(buf, i, j, nrows, ncols, lc, rich, cnt, tex);
     }
  }

  if (choice->jux[0]) {
     free(atts);
     for(i = 0; i < cntwhole; i++)
        free(weight[i]);
     free(weight);
  }

  if (choice->edg[2]) {
     free(edgeatts);
     for (i = 0; i < cntwhole; i++)
	free(edgemat[i]);
     free(edgemat);
  }   

					/* if the edge map was requested */

  if (choice->edgemap) {
     fc = G_open_cell_new("edge");
     edge_buf = G_allocate_cell_buf();
     for (i = 1; i < nrows + 1; i++) {
	G_zero_cell_buf(edge_buf);
        for(j = 1; j < ncols + 1; j++) {
	   if (edgemap[i][j])
	      *(edge_buf + j - 1) = edgemap[i][j];
        }
        G_put_map_row(fc,edge_buf);
     }
     free(edge_buf);
     G_close_cell(fc);
     for(i = 0; i < nrows + 3; i++)
        free(edgemap[i]);
     free(edgemap);      
  }

					/* if the zscore map was requested */

  if (choice->z) {
     fd = G_open_cell_new("zscores");
     zscor_buf = G_allocate_cell_buf();
     for (i = 1; i < nrows + 1; i++) {
        G_zero_cell_buf(zscor_buf);
        for(j = 1; j < ncols + 1; j++) {
           if (attr[1] > 0.0)
	      if (buf[i][j]) {
                 *(zscor_buf + j - 1) = 100*((double)(buf[i][j] - 
		    attr[0])/attr[1]);
	      }
        }
        G_put_map_row(fd,zscor_buf);
     }
     free(zscor_buf);
     G_close_cell(fd);
  }

					/* open the output files and 
					   save the calculated values */

  if(choice->att[0]) {
     fp0 = fopen0("r.le.out/b1-4.out", "a"); 
     fprintf(fp0, "%6d%6d", g_scale, g_unit);
     fprintf(fp0,"%10.3f%10.3f%10.3f%10.3f\n", 				
        attr[0], attr[1], attr[2], attr[3]);
     fclose(fp0);
  } 

  if(choice->div[0]) {
     fp1 = fopen0("r.le.out/d1-4.out", "a"); 
     fprintf(fp1, "%6d%6d", g_scale, g_unit);
     fprintf(fp1,"%10.3f%10.3f%10.3f%10.3f\n", 				
        diver[0], diver[1], diver[2], diver[3]);
     fclose(fp1);
  } 

  if(choice->te2[0]) {
     fp2 = fopen0("r.le.out/t1-5.out", "a");
     fprintf(fp2, "%6d%6d", g_scale, g_unit);
     fprintf(fp2,"%10.3f%10.3f%10.3f%10.3f%10.3f\n",  
        tex[0], tex[1], tex[2], tex[3], tex[4]);
     fclose(fp2);
  } 

  if(choice->jux[0]) {
     fp3 = fopen0("r.le.out/j1-2.out", "a");
     fprintf(fp3, "%6d%6d", g_scale, g_unit);
     fprintf(fp3,"%10.3f%10.3f\n", edge[0], edge[2] );				
     fclose(fp3);
  }

  if(choice->edg[1]) {
     fp4 = fopen0("r.le.out/e1.out", "a");
     fprintf(fp4, "%6d%6d", g_scale, g_unit);
     fprintf(fp4,"%10.3f\n", edge[1]);
     fclose(fp4);	
  }

  if(choice->edg[2]) {
     fp5 = fopen0("r.le.out/e2.out", "a");
     fprintf(fp5, "%6d%6d", g_scale, g_unit);
     fprintf(fp5,"%10.3f\n", edge[3]);
     fclose(fp5);	
  }

}





					/* ATTRIBUTE CALC. */

void   cal_att(buf, i0, j0, nr, nc, attr)
int    **buf, i0, j0, nr, nc;
double *attr;
{
          
  static int    count;
  static int	mini, maxi;
  double    	mean, stdv;
  static double sum, sum2;

  if (i0==1 && j0 == 1) {
     sum = 0.0;
     sum2 = 0.0;
     count = 0;
     maxi = 0;
     mini = BIG;
     mean = 0.0;
     stdv = 0.0;
  }

  if(buf[i0][j0] != 0) 
     count++; 
  sum += buf[i0][j0];
  sum2 += buf[i0][j0] * buf[i0][j0];

  if (buf[i0][j0] > maxi) 
     maxi = buf[i0][j0];

  if (buf[i0][j0] < mini) 
     mini = buf[i0][j0];

/*printf("i=%d j=%d att=%d count=%d sum=%15.3f sum2=%15.3f\n",i0,j0,buf[i0][j0],count,sum,sum2);
*/
 
  if(i0 == nr && j0 == nc) {

					/* calc. b1 = mean pixel attr. */

     attr[0] = mean = sum/count;


					/* calc. b2 = st. dev. pixel attr. */

/*printf("mean=%7.3f sum2=%7.3f 1stterm=%7.3f 2ndterm=%7.3f\n",mean, sum2,sum2/count, mean*mean);
*/

     stdv = sum2/count -  mean*mean;
     if (stdv > 0)
	attr[1] = sqrt(stdv);


					/* calc. b3 = min. pixel attr. */

     attr[2] = mini;


					/* calc. b4 = max. pixel attr. */

     attr[3] = maxi;
  }
}




					/* DIVERSITY CALC. */

void   cal_divers(buf, i0, j0, nr, nc, lc, cnt, diver)
int    **buf, nr, nc, i0, j0, lc, cnt;
double *diver;
{
  int           tot; 
  static int    *density;
  register int  i, k=0;
  double    	p, entr;

					/* if this is the first pixel, 
					   dynamically allocate memory for
					   the density array */

  if(1 == i0 && 1 == j0) {  
     density = (int *)G_calloc(cnt, sizeof(int));
     zero = 0;
  }

					/* if the pixel has a non-zero 
					   attribute */

  if (buf[i0][j0] != 0) 

 					/* increment the density count for
					   the right element of the density
					   array */

     density[lc] ++;

					/* if the pixel has a 0 attribute
					   set the zero flag to 1 */

  else if (buf[i0][j0] == 0)
     zero = 1;
					/* if this is the last pixel in the
					   sampling area, do the calc. */

  if (i0 == nr && j0 == nc) {

					/* initialize the counter for the
					   total number of pixels */

     tot = 0;


					/* case 1: there is a pixel with
					   attribute 0 */

     if (zero) {

        diver[0] = cnt-1;            	/* richness */
        if (cnt > 1)
           entr = log((double)(cnt-1));
	else
	   entr = 0.0;

					/* calculate the total # of pixels */

        for(i = 1; i < cnt; i++)
	   tot = tot + density[i];

        for(i = 1; i < cnt; i++){
           if(p = density[i] / (double)(tot))
              diver[1] += -(p*log(p));  	/* Shannon */
              diver[3] += p*p;
        }
        diver[2] = entr - diver[1];     	/* dominance */
     }


					/* case 2: there is not a pixel with
					   attribute 0 */
	
     else if (!zero) {
        diver[0] = cnt;              		/* richness */
        if (cnt > 1)
           entr = log((double)(cnt));
	else
	   entr = 0.0;

					/* calculate the total # of pixels */

        for(i = 0; i < cnt; i++)
	   tot = tot + density[i];

        for(i=0; i<cnt; i++){
           if(p = density[i] / (double)(tot))
              diver[1] += -(p*log(p));  	/* Shannon */
              diver[3] += p*p;
        }
        diver[2] = entr - diver[1];     	/* dominance */
     }

     diver[3] = 1/diver[3];          		/* recip. Simpson */
     free(density);

  }

}







					/* TEXTURE CALC. */

void   cal_tex(buf, i0, j0, nr, nc, lc, rich, cnt, tex)
int     **buf, i0, j0, nr, nc, lc, *rich, cnt ;
double  *tex;
{
  FILE   	*fp;
  int           r, center, ln, tot, loop_cnt;
  double	p;
  static int    **GLCM; 
  register int  i, j;

  int  GLCM_sum = 0 ;

  					/* setup the GLCM matrix */

  if( (i0 == 1 && j0 == 1) ){
     GLCM = (int **)G_calloc(cnt, sizeof(int *));
     for(i = 0; i < cnt; i++)
        GLCM[i] = (int *)G_calloc(cnt, sizeof(int));
  }

  					/* calculate the GLCM, using
					   the appropriate one of the
					   seven texture methods 
					   (parameter te1) */

  if(i0 > 1) {
     if(choice->tex == 3 || choice->tex == 5 || choice->tex == 7){
         if(buf[i0-1][j0]) {
           ln = check_order(buf[i0-1][j0], rich); 
           GLCM[lc][ln] ++; 
         }
     } 

     if(j0 > 1 && 
	(choice->tex == 4 || choice->tex == 6 || choice->tex == 7)){
           if(buf[i0-1][j0-1]) {
              ln = check_order(buf[i0-1][j0-1], rich); 
              GLCM[lc][ln] ++;
           }
     }

     if(j0 < nc &&
        (choice->tex == 2 || choice->tex == 6 || choice->tex == 7)){
           if(buf[i0-1][j0+1]) {
              ln = check_order(buf[i0-1][j0+1], rich); 
              GLCM[lc][ln] ++;
           }
     }
  }

  if(i0 < nr) {
     if(choice->tex == 3 || choice->tex == 5 || choice->tex == 7){
        if(buf[i0+1][j0]) {
           ln = check_order(buf[i0+1][j0], rich); 
           GLCM[lc][ln] ++;
        }
     } 

     if(j0 > 1 &&
        (choice->tex == 2 || choice->tex == 6 || choice->tex == 7)){
           if(buf[i0+1][j0-1]) {
              ln = check_order(buf[i0+1][j0-1], rich); 
              GLCM[lc][ln] ++;
           }
     }

     if(j0 < nc &&
        (choice->tex == 4 || choice->tex == 6 || choice->tex == 7)){
           if(buf[i0+1][j0+1]) {
              ln = check_order(buf[i0+1][j0+1], rich); 
              GLCM[lc][ln] ++;
           }
     }
  }

  if(j0 > 1 &&
     (choice->tex == 1 || choice->tex == 5 || choice->tex == 7)){
           if(buf[i0][j0-1]) {
              ln = check_order(buf[i0][j0-1], rich); 
              GLCM[lc][ln]++;
           }
  }
  if(j0 < nc &&
     (choice->tex == 1 || choice->tex == 5 || choice->tex == 7)){
           if(buf[i0][j0+1]) {
              ln = check_order(buf[i0][j0+1], rich); 
              GLCM[lc][ln]++;
           }
  }

 					/* once the end of the buffer
					   has been reached do the 
					   calculations */


  if(i0 == nr && j0 == nc) {    

					/* find zeroes in the richness
					   array and increment loop_cnt
					   for each zero */

       loop_cnt = cnt ;
       for (i = 0; i < cnt; i++) 
          if (rich[i]==0) 
             loop_cnt++;  

					/* if there is not a 0 category */
  
       if (loop_cnt == cnt) {
          for(i = 0; i < loop_cnt; i++)
             for(j = 0; j < loop_cnt; j++)
                GLCM_sum = GLCM_sum + GLCM[i][j];
       }

					/* if there is a 0 category */

       else {
          for(i = 1; i < cnt; i++)
             for(j = 1; j < cnt; j++) 
                GLCM_sum = GLCM_sum + GLCM[i][j];
       }

       r = GLCM_sum ;


    					/* for each category, compute the 
					   Pij and the measures **/

					/* if there is not a 0 attribute */

    if (loop_cnt == cnt ) {          
       for(i = 0; i < loop_cnt; i++)
          for(j = 0; j < loop_cnt; j++)
             if( p = GLCM[i][j] / (double)(r)){
	         tex[3] += p*log(p);             
	         tex[1] += p*p;                  /* ASM */
	         tex[2] += p/(1+ (i-j)*(i-j));   /* IDM */
	         tex[4] += p*(i-j)*(i-j);        /* Contrast */
	     }
    }

					/* if there is a 0 attribute */

    else {                            
       for(i = 1; i < cnt; i++)
          for(j = 1; j < cnt; j++)
             if( p = GLCM[i][j] / (double)(r)){
	         tex[3] += p*log(p);             
	         tex[1] += p*p;                  /* ASM */
	         tex[2] += p/(1+ (i-j)*(i-j));   /* IDM */
	         tex[4] += p*(i-j)*(i-j);        /* Contrast */
	     }
    }

    tex[3] = -1.0*tex[3];	  		 /* Entropy */
    tex[0] = 2*log((double)(cnt)) - tex[3];     /* Contagion */


    for(i = 0; i < cnt; i++)
       free(GLCM[i]);
    free(GLCM);
  }
}



					/* EDGE, JUXTAPOSITION CALC. */

void   cal_edge(buf, i0, j0, nr, nc, lc, edge, cntwhole,
		atts, weight, edgeatts, edgemat, edge1, edge2)
int     **buf, i0, j0, nr, nc, lc, cntwhole, *atts, *edgeatts,
	*edge1, *edge2;
double  *edge, **weight, **edgemat;
{
  FILE           *fp;
  int		 ln, lr, cnt, fr, to;
  double 	 juxta, sum, stdv;
  register int   i, j;
  static double  sum2 = 0;  


/* VARIABLES:
	atts = 		an array of the attributes; read from weight file.
	countwhole =	richness (total number of attributes)
	ln =		sequence number, for the neighbor pixel, in the atts 
			   array
	lc =		sequence number for an attribute in the richness 
			   array (NO LONGER USED)
	lr =		sequence number, for the current pixel, in the atts
			   array
	edge[0] =	running total of juxtaposition values
	edge[1] =	sum of edges
	edge[2] = 	st. dev. juxtaposition
	edge[3] = 	sum of edges by type
*/

					/* initialize variables */

  sum = cnt = 0;

					/* juxtaposition calc. step 1: 
					   each 4 line loop checks first
					   that the pixel has non-zero
					   attribute, second finds the
					   sequence number for the 
					   pixel's attribute in the 
					   attribute array, third sums
					   the quantity and weight, and
					   fourth sums the correct cnt */

					/* if this pixel has a non-zero
					   attribute do the calculations */


  if(buf[i0][j0]) {

					/* increment the count of non-zero
					   pixels */


     total ++;

					/* if this pixel is not on the
					   edge of the map in the first
					   row, and if juxt. was chosen */
				   
     if(i0 > 1 && choice->jux[0]) {


					/* neighbor 1: i0-1,j0 */

        if(buf[i0-1][j0]) {
	   lr = find_loc(cntwhole, atts, buf[i0][j0]);
           ln = find_loc(cntwhole, atts, buf[i0-1][j0]);
           sum += 2*weight[lr][ln];
           cnt += 2;
        }


					/* if this pixel is not on the
					   edge of the map in the first 
					   col */

        if(j0 > 1){

					/* neighbor 2: i0-1,j0-1 */

           if(buf[i0-1][j0-1]) {
	      lr = find_loc(cntwhole, atts, buf[i0][j0]);
	      ln = find_loc(cntwhole, atts, buf[i0-1][j0-1]); 
              sum += weight[lr][ln];
              cnt ++;	
           }
        }


					/* if this pixel is not on the
					   edge of the map in the last
					   col */

        if(j0 < nc){

					/* neighbor 3: i0-1,j0+1 */

           if (buf[i0-1][j0+1]) {
	      lr = find_loc(cntwhole, atts, buf[i0][j0]);
	      ln = find_loc(cntwhole, atts, buf[i0-1][j0+1]);
              sum += weight[lr][ln];
              cnt ++;
           }
        }
     }

					/* if this pixel is not on the
					   edge of the map in the last
					   row and if juxta. was chosen */

     if(i0 < nr) {
        if(choice->jux[0]){

					/* neighbor 4: i0+1,j0 */

            if(buf[i0+1][j0]) {
	       lr = find_loc(cntwhole, atts, buf[i0][j0]);
               ln = find_loc(cntwhole, atts, buf[i0+1][j0]);
               sum += 2*weight[lr][ln];
               cnt += 2;
            }

					/* if this pixel is not on the
					   edge of the map in the first
					   col */

            if(j0 > 1){

					/* neighbor 5: i0+1,j0-1 */

               if(buf[i0+1][j0-1]) {
	          lr = find_loc(cntwhole, atts, buf[i0][j0]);
                  ln = find_loc(cntwhole, atts, buf[i0+1][j0-1]);
                  sum += weight[lr][ln];
                  cnt ++;
               }
            }

					/* if this pixel is not on the
					   edge of the map in the last
					   col */

            if(j0 < nc){

					/* neighbor 6: i0+1,j0+1 */

               if(buf[i0+1][j0+1]) {
	          lr = find_loc(cntwhole, atts, buf[i0][j0]);
	          ln = find_loc(cntwhole, atts, buf[i0+1][j0+1]);
                  sum += weight[lr][ln];
	          cnt ++;
               } 
            }
        }
					/* if the i0+1, j0 pixel is different,
					   and both pixels have non-zero
					   attributes */

        if(choice->edg[0] && buf[i0][j0] != buf[i0+1][j0] &&
	   buf[i0][j0] && buf[i0+1][j0]) {

 					/* then increment edge[1] to 
					   indicate that an edge has been 
					   found if sum of edges requested */

	   if (choice->edg[1])
	         edge[1]++;

					/* then increment edge[2] to 
					   indicate that an edge has been
					   found if the edge is one of the
					   types requested and if sum of 
					   edges by type requested */

	   if (choice->edg[2]) {
	         fr = find_edge(cntwhole, edgeatts, buf[i0][j0]);
                 to = find_edge(cntwhole, edgeatts, buf[i0+1][j0]);
	         if (edgemat[fr][to]) {
	            edge[3]++;
		    if (choice->edgemap)
		       *edge1 = 1;
		 }
	   }
	}                                  
     }


					/* regardless which row this pixel
					   is in (even if edge row); if 
					   this pixel is not on the edge
					   of the map in the first col, and
					   if juxta. was chosen */

     if(j0 > 1 && choice->jux[0]){

					/* neighbor 7: i0,j0-1 */

        if (buf[i0][j0-1]) {
 	   lr = find_loc(cntwhole, atts, buf[i0][j0]);
           ln = find_loc(cntwhole, atts, buf[i0][j0-1]);
           sum += 2*weight[lr][ln];
           cnt += 2;
        }
     }

					/* regardless which row this pixel
					   is in (even if edge row); if 
					   this pixel is not on the edge
					   of the map in the last col, and
					   if juxta. was chosen */


     if(j0 < nc){
        if(choice->jux[0]){

					/* neighbor 8: i0,j0+1 */

           if(buf[i0][j0+1]) {
 	      lr = find_loc(cntwhole, atts, buf[i0][j0]);
              ln = find_loc(cntwhole, atts, buf[i0][j0+1]);
              sum += 2*weight[lr][ln]; 
              cnt += 2;
           }
        }


 					/* if the i0, j0+1 pixel is different,
					   and both pixels have non-zero
					   attributes */

        if(choice->edg[0] && buf[i0][j0] != buf[i0][j0+1] &&
	   buf[i0][j0] && buf[i0][j0+1]) {

 					/* then increment edge[1] to 
					   indicate that an edge has been 
					   found if sum of edges requested */

	   if (choice->edg[1])
                 edge[1]++;

					/* then increment edge[2] to 
					   indicate that an edge has been
					   found if the edge is one of the
					   types requested and if sum of 
					   edges by type requested */

	   if (choice->edg[2]) {
	         fr = find_edge(cntwhole, edgeatts, buf[i0][j0]);
                 to = find_edge(cntwhole, edgeatts, buf[i0][j0+1]);
	         if (edgemat[fr][to]) {
	            edge[3]++;
		    if (choice->edgemap)
		       *edge2 = 1;
		 }
           }
	}
     }
  }

					/* calculate juxtaposition and 
					   add it to the running total
					   in edge[0] */

  if(choice->jux[0]) {
     if (cnt)
        juxta = sum / cnt;
     else 
        juxta = 0.0;
     edge[0] += juxta;
     sum2 += juxta*juxta;
  }


					/* if this is the last pixel in 
					   the sampling area and juxta-
					   position was chosen */

  if(choice->jux[0] && i0 == nr && j0 == nc) {

     edge[0] /= total;

     stdv = (double)sum2/total - edge[0]*edge[0];
     if(stdv > 0) 
        edge[2] = sqrt(stdv);
      
     sum2 = 0;
  }
}




					/* READ THE WEIGHT FILE */

void  read_weight(rich, atts, weight, attcnt)
double  **weight;
int     rich, atts[], *attcnt;
{
  FILE         *fp;
  register int i, j;
  float        tmp;

					/* open the weight file */

  fp = fopen2("r.le.para/weight", "r");

					/* read the attributes into 
					   the atts array */

  for(i = 0; i < rich; i++) {
     fscanf(fp, "%d", atts+i);
  }
  while(fgetc(fp) != '\n')
     if (fgetc(fp) != ' ') {
        printf("\n");
        printf("   *************************************************\n");
        printf("    The weight file (r.le.para/weight) is incorrect \n");
        printf("       since more/less than the %d attributes found \n",rich);
        printf("       in the map are listed in the weight file     \n");
        printf("   *************************************************\n");
        exit(1);
     }
					/* read the weights, skipping
					   the first column, which 
					   contains the attribute again */

  for(i = 0; i < rich; i++){
     fscanf(fp, "%d", &tmp);
     for(j = 0; j < i; j++){
	weight[i][j] = weight[j][i];
     }
     for(j = 0; j < rich; j++){ 
  	fscanf(fp, "%f", &tmp);
	weight[i][j] = tmp;
     }
     while(fgetc(fp) != '\n');
  }
  fclose(fp);
}






					/* READ THE EDGE FILE */

void  read_edge(rich, atts, edge)
double  **edge;
int     rich, atts[];
{
  FILE         *fp;
  register int i, j;
  float        tmp;

					/* open the edge file */

  fp = fopen3("r.le.para/edge", "r");

					/* read the attributes into 
					   the atts array */

  for(i = 0; i < rich; i++)
     fscanf(fp, "%d", atts+i);
  while(fgetc(fp) != '\n')
     if (fgetc(fp) != ' ') {
        printf("\n");
        printf("   *************************************************\n");
        printf("    The edge file (r.le.para/edge) is incorrect     \n");
        printf("       since more/less than the %d attributes found \n",rich);
        printf("       in the map are listed in the edge file       \n");
        printf("   *************************************************\n");
        exit(1);
     }

					/* read the edge weights, skipping
					   the first column, which 
					   contains the attribute again */

  for(i = 0; i < rich; i++){
     fscanf(fp, "%d", &tmp);
     for(j = 0; j < i; j++){
	edge[i][j] = edge[j][i];
     }
     for(j = 0; j < rich; j++){ 
  	fscanf(fp, "%f", &tmp);
	edge[i][j] = tmp;
     }
     while(fgetc(fp) != '\n');
  }
  fclose(fp);
}






					/* FIND THE SEQUENCE NUMBER FOR
					   AN ATTRIBUTE IN THE ATTRIBUTE
					   ARRAY WHICH IS READ FROM THE
					   WEIGHT FILE */

int  find_loc(rich, atts, test)
int  rich, atts[], test;
{
  register int i;
  
  G_sleep_on_error(0);
  for(i = 0; i < rich; i++) {
     if(test == atts[i]) return i;
  }
  G_fatal_error("The weight file in r.le.para is incorrect, exit\n");  
}



					/* FIND THE SEQUENCE NUMBER FOR
					   AN ATTRIBUTE IN THE ATTRIBUTE
					   ARRAY WHICH IS READ FROM THE
					   EDGE FILE */

int  find_edge(rich, atts, test)
int  rich, atts[], test;
{
  register int i;
  
  G_sleep_on_error(0);
  for(i = 0; i < rich; i++) {
     if(test == atts[i]) return i;
  }
  G_fatal_error("The edge file in r.le.para is incorrect, exit\n");
  
}




					/* FIND THE SEQUENCE NO. OF AN
					   ATTRIBUTE IN THE RICHNESS ARRAY */

int  check_order(att, rich)
int  att, *rich;
{
  int i=0;

  if (att == 0) return 0;
  while(att != rich[i]) i++;
  return i;
}





