	                 	/********************************/
			        /* 	r.le.dist/dist.c	*/
                                /*                              */
				/*		2.1		*/
				/*				*/
	                        /*       07/15/94 version       */
        	                /*                              */
				/*      Programmer: Baker   	*/
				/*      Univ. of Wyoming 	*/
			        /********************************/



#include "r.le.dist.h"

extern struct CHOICE    *choice;
extern int 	        total_patches, ntype, m1, m2, m3, nrst, 
			adj, non_skip, NN, n_scale, n_unit;
extern float            **recl_tb, **from_to, *dist_CC, *dist_CE, *dist_EE;
int		        type_coh, total=0, total1=0, total2=0, total_ft=0;
FILE			*outfile;


					/* MOVING WINDOW DRIVER */

void mv_dist (patch_list, value, index)
PATCH  	*patch_list;
int	**value;
int	index;
{
  PATCH     	 *tmp = patch_list;
  int            i,j;
 
  if (!total_patches) return;

					/* for all the patches on the list,
					   call run_dist_mv with the right
					   parameters */


  while(tmp){

     if (adj)
        run_dist_mv(patch_list, tmp, -1, choice->mn[0], choice->mn[1], 
		    0, value, index);

     else if (nrst){

	if (m1)				/* nearest same gp */
  	   run_dist_mv(patch_list, tmp, 1, choice->mn[2], 
		       choice->mn[3], choice->mn[4], value, index);
	else if (m2)			/* nearest diff gp */
  	   run_dist_mv(patch_list, tmp, 2, choice->mn[5], choice->mn[6],
	               0, value, index);
	else if (m3)			/* nearest specific gp */
  	   run_dist_mv(patch_list, tmp, 3, choice->mn[7], 
		       choice->mn[8], choice->mn[9], value, index);
     }
 
     tmp = tmp->next;
  }

  					/* release the memory used for the
					   patch list */

  while(patch_list){
       tmp = patch_list;
       patch_list = patch_list->next;
       free(tmp->row);  
       free(tmp->col);
       free(tmp);
  }
  total_patches = 0;
  return;

} 




					/* GIVEN THE PARAMETERS, CALL THE
					   CORRECT MOVING WINDOW CALC.
					   ROUTINE */

void   run_dist_mv(patch_list, tmp, meth, v1, v2, v3, value, index)
PATCH         *patch_list, *tmp;
int           meth, v1, v2, v3, **value, index;
{
  register int  i;
  float		cc=v1, ce=v2, ee=v3, *cc1=NULL, *ce1=NULL;
  int		n, neighbor=0;

  type_coh = 0;



/* choice->mm[0] =	mean distance (n1)
   choice->mm[1] = 	st. dev. distance (n2)
   choice->mm[2] = 	mean distance in gp 1 (n3)
   choice->mm[3] = 	st. dev. distance in gp 1 (n4)
   choice->mm[4] = 	no. of distances in dist. class 1 (n5)
   choice->mm[5] = 	no. of distances in dist. class 1 & gp 1 (n6)	
*/

				/* if di2 = n3, n4, or n6 (by gp
				   measures) */

  if ((choice->mm[2] || choice->mm[3] || choice->mm[5])) {

				/* if m0-m6 */

     if (!choice->mn[7] && !choice->mn[8] && !choice->mn[9] && 
        in_group(tmp->att, recl_tb[0]))
        type_coh = 1;

				/* if m7-m9 */

     else if (choice->mn[7] || choice->mn[8] || choice->mn[9] && 
	*(*(from_to + 0) + 1) == (recl_coh(tmp->att) + 1))
	type_coh = 1;
  }

  if (meth > 0) { 		/* if a nearest neighbor method (m2-m9) */

     cal_nrst(patch_list, tmp, &cc, &ce, &ee, meth, &neighbor);

     if (v1)
        do_calc_mv(cc, dist_CC, NULL, neighbor, 0);
     else if (v2)
        do_calc_mv(ce, dist_CE, NULL, neighbor, 0);
     else if (v3)
        do_calc_mv(ee, dist_EE, NULL, neighbor, 0);
  } 

  else {			/* if an adjacent method (m0-m1) */
     neighbor = 1;
     if (v1)
       cc1 = (float *)G_calloc(500, sizeof(float));
     else
       ce1 = (float *)G_calloc(500, sizeof(float));

     cal_adj(patch_list, tmp, cc1, ce1, &n);
 
     for(i = 0; i < n; i++) {
        if(v1)
           do_calc_mv(cc1[i], dist_CC, NULL, neighbor, 0);
        else if(v2)
           do_calc_mv(ce1[i], dist_CE, NULL, neighbor, 0);
     }

     if (v1)
       free(cc1);  
     else
       free(ce1);
  }

					/* if the last patch is done, 
					   calculate the measurement value */


  if (tmp->next == NULLPTR)  
     do_calc_mv(0.0, NULL, value, neighbor, index);

}





					/* CALCULATE THE CHOSEN MOVING
					   WINDOW DISTANCE MEASURES */


void  do_calc_mv(d, dist_cl, value, neighbor, index)
int     **value, neighbor, index;
float   *dist_cl, d;
{
  static double sum1=0, sum12 = 0, sum2 = 0, sum22 = 0, stdv = 0, 
	 den1 = 0, den2 = 0;
  float        m;

/* choice->mm[0] =	mean distance (n1)
   choice->mm[1] = 	st. dev. distance (n2)
   choice->mm[2] = 	mean distance in gp 1 (n3)
   choice->mm[3] = 	st. dev. distance in gp 1 (n4)
   choice->mm[4] = 	no. of distances in dist. class 1 (n5)
   choice->mm[5] = 	no. of distances in dist. class 1 & gp 1 (n6)
	
   d =			distance
   sum =		sum of the distances
   sum2 =		sum of the distances squared
   den =		number of distances
			or number of distances by gp
   total = 		number of distances

*/

					 /* if last patch is not done */

  if (!value) {  

					/* calc. n1=mn. dist. in gp1 & 
					   n2=s.d. dist. in gp1 */
	
     if ((choice->mm[0] || choice->mm[1]) && neighbor) { 
	total1 ++;
	sum1 += d;
        if (choice->mm[1]) 			
	   sum12 += d*d;
     } 

					/* calc. n3=mn. dist. in gp1 & 
					   n4=s.d. dist. in gp1 */

     if ((choice->mm[2] || choice->mm[3]) && type_coh && neighbor) {
	total2 ++;
	sum2 += d; 
	if (choice->mm[3])
	   sum22 += d*d; 
     } 

					/* calc. n5=no. dist. in dist.
					   class1 */

     if (choice->mm[4] && d < dist_cl[2] && d >= dist_cl[1] && neighbor)
	den1 ++;

					/* calc. n6=no. dist. in dist. 
					   class1 & gp1 */

     if (choice->mm[5] && d < dist_cl[2] && d >= dist_cl[1] && type_coh
	&& neighbor)	
	den2 ++;


  } 

					/* if all the patches are done */

  else {    
					/* calc. n1=mn. dist. */
	
     if (choice->mm[0]) {
        if (total1)
           value[index][0] = 10 * (double)sum1/total1;
        else
	   value[index][0] = 0;
     }

					/* calc. n2=s.d. dist. */

     if (choice->mm[1]) {
        m=0;
        if (total1) {
           m = (double)(sum1/total1);
	   stdv = (double)(sum12/total1) - m*m;
           if (stdv > 0.0)
              value[index][1] = 100 * (sqrt((double)stdv));
	   else 
	      value[index][1] = 0;
        }
        else value[index][1] = 0;
     }

					/* calc. n3=mn. dist. by gp. */
	
     if (choice->mm[2]) {
        if (total2)
           value[index][2] = 10 * (double)sum2/total2;
        else
	   value[index][2] = 0;
     }

					/* calc. n4=s.d. dist. by gp. */

     if (choice->mm[3]) {
        m=0;
        if (total2) {
           m = (double)(sum2/total2);
	   stdv = (double)(sum22/total2) - m*m;
           if (stdv > 0.0)
              value[index][3] = 100 * (sqrt((double)stdv));
	   else 
	      value[index][3] = 0;
        }
        else 
	   value[index][3] = 0;
     }

					/* calc. n5=no. dist. in class1 */ 
 
     if (choice->mm[4])
	value[index][4] = den1;

		       			/* calc. n6=no. dist. in class1 & gp1 */

     if (choice->mm[5])
	value[index][5] = den2;

     sum1 = sum2 = sum12 = sum22 = den1 = den2 = total1 = total2 = 0;
  }
} 





					/* DRIVER FOR WHOLE MAP, UNITS, 
					   AND REGIONS CALCULATIONS */

void  df_dist (patch_list)
PATCH *patch_list;
{
  PATCH         *tmp = patch_list;
  register int  i;
  int		*dens, *dens1;
  struct VALUE  VL1_cc, VL1_ce, VL1_ee, 
		VL2_cc, VL2_ce, 
		VL3_cc, VL3_ce, VL3_ee, 
		Vcc_adj, Vce_adj;
  char		path[30];


					/* if an output list of individual
					   patch measures has been requested,
					   then open the file in r.le.out */

  if(strcmp(choice->out,"") && choice->wrum != 'm') {
     sprintf(path, "r.le.out/%s", choice->out);
     outfile = fopen0(path, "a");
     if(!strcmp(choice->out, "head"))
        fprintf(outfile, "            FM PATCH   Center     Edge    TO PATCH   Center     Edge       DIST-\nscale unit  num  att  row  col  row  col  num  att  row  col  row  col     ANCE\n");
  }


  if(!total_patches) return;
  dens = (int *)G_calloc(25, sizeof(int));
  dens1 = (int *)G_calloc(25, sizeof(int));

  while(tmp){

					 /* if di2=n3, n4, n6 tally by gp */


     if (choice->mm[2] || choice->mm[3] || choice->mm[5]){        
        type_coh = recl_coh(tmp->att);
	if (choice->mm[2] || choice->mm[3]) {
	   dens[type_coh] ++;
	   dens1[type_coh] ++;
	}
     }


     if (choice->mn[0] || choice->mn[1] ) {


					/* if di1=m0: ea. patch to all adj.
					   neighbors CC */

        if (choice->mn[0]) 
           run_dist(patch_list, tmp, -1, choice->mn[0], 0, 0, &Vcc_adj, 
		   &Vce_adj, NULL, dens1);

					/* if di1=m1: ea. patch to all adj.
					   neighbors CE */

        if (choice->mn[1]) 
           run_dist(patch_list, tmp, -1, 0, choice->mn[1], 0, &Vcc_adj, 
		   &Vce_adj, NULL, dens1);
     }

     if (nrst){

	if(m1) {

					/* if di1=m2: ea. patch to nearest
					   of same gp CC */

  	   if (choice->mn[2])
	      run_dist(patch_list, tmp, 1, choice->mn[2], 0, 0, &VL1_cc, 
	             &VL1_ce, &VL1_ee, dens);

					/* if di1=m3: ea. patch to nearest
					   of same gp CE */

  	   if (choice->mn[3])
	      run_dist(patch_list, tmp, 1, 0, choice->mn[3], 0, &VL1_cc, 
		     &VL1_ce, &VL1_ee, dens);

					/* if di1=m4: ea. patch to nearest
					   of same gp EE */

  	   if (choice->mn[4])
	      run_dist(patch_list, tmp, 1, 0, 0, choice->mn[4], &VL1_cc, 
		     &VL1_ce, &VL1_ee, dens);
        }

	if(m2) {
					/* if di1=m5: ea. patch to nearest
					   of diff. gp CC */

  	   if (choice->mn[5])
  	      run_dist(patch_list, tmp, 2, choice->mn[5], 0, 0, &VL2_cc, 
		       &VL2_ce, NULL, dens);

					/* if di1=m6: ea. patch to nearest
					   of diff. gp CE */

  	   if (choice->mn[6]) 
 	      run_dist(patch_list, tmp, 2, 0, choice->mn[6], 0, &VL2_cc,
		       &VL2_ce, NULL, dens);

        }

	if (m3) {

					/* if di1=m7: patches of 1 gp to
					   nearest of spec. gp CC */

  	   if (choice->mn[7]) 
	      run_dist(patch_list, tmp, 3, choice->mn[7], 0, 0, &VL3_cc,
		       &VL3_ce, &VL3_ee, dens);

					/* if di1=m8: patches of 1 gp to
					   nearest of spec. gp CE */

  	   if (choice->mn[8])
	      run_dist(patch_list, tmp, 3, 0, choice->mn[8], 0, &VL3_cc,
		       &VL3_ce, &VL3_ee, dens);

					/* if di1=m9: patches of 1 gp to
					   nearest of spec. gp EE */

  	   if (choice->mn[9])
	      run_dist(patch_list, tmp, 3, 0, 0, choice->mn[9], &VL3_cc,
		       &VL3_ce, &VL3_ee, dens);
        }
     }
     tmp = tmp->next;
  }

  fclose(outfile);
  free(dens);
  free(dens1);

  					/* release the memory used for the 
					   patch list */

  while(patch_list){
       tmp = patch_list;
       patch_list = patch_list->next;
       free(tmp->row);
       free(tmp->col);
       free(tmp);
  }
  total_patches = total = total_ft = 0;


} 





					/* RUN THE PROPER DISTANCE MODULE */

void run_dist(patch_list, tmp, meth, v1, v2, v3, Vcc, Vce, Vee, dens)
PATCH         *patch_list, *tmp;
int           meth, v1, v2, v3, *dens;
struct VALUE  *Vcc, *Vce, *Vee;   
{
  register int  i;
  float		cc=v1, ce=v2, ee=v3, *cc1=NULL, *ce1=NULL;
  int		n, neighbor=0;


  if (tmp->num == 1){
     if (v1)  setup_v(Vcc);
     if (v2)  setup_v(Vce);
     if (v3)  setup_v(Vee);
  }

  if (meth > 0){
     cal_nrst(patch_list, tmp, &cc, &ce, &ee, meth, &neighbor);

     if (v1)
         do_calc(cc, type_coh, dist_CC, Vcc, neighbor);
     if (v2)
         do_calc(ce, type_coh, dist_CE, Vce, neighbor);
     if (v3)
         do_calc(ee, type_coh, dist_EE, Vee, neighbor);
  }

  else {
     neighbor = 1;
     if (v1)
        cc1 = (float *)G_calloc(500, sizeof(float));
     if (v2)	
	ce1 = (float *)G_calloc(500, sizeof(float));

     cal_adj(patch_list, tmp, cc1, ce1, &n);
     for(i = 0; i < n; i++) {
         if (v1)
             do_calc(cc1[i], type_coh, dist_CC, Vcc, neighbor);
         if (v2)
             do_calc(ce1[i], type_coh, dist_CE, Vce, neighbor);
     } 
     dens[type_coh] += n-1;

     if (v1)
	free(cc1);   
     if (v2)
	free(ce1);
  }

  if (tmp->next == NULLPTR) 
     save_dist(v1, v2, v3, Vcc, Vce, Vee, dens, meth);

} 





					/* SETUP THE V STRUCTURE TO SAVE
					   THE TALLIES OF DISTANCES BY
					   DISTANCE CLASS */

void  setup_v(V)
struct VALUE *V;
{
  register int i, j;

  for(i=0; i<25; i++){
    V->den[i] = V->sum1[i] = V->sum21[i] = 0;
    for(j=0; j<25; j++)
	V->den2[i][j] = 0;
  }
  V->sum = V->sum2 = 0;
}





					/* CALCULATE THE SELECTED MEASURES */

void  do_calc(d, type_coh, dist_cl, V, neighbor)
int          type_coh, neighbor;
float        *dist_cl, d;
struct VALUE *V;
{
  int   d_coh;

  if ((choice->mm[0] || choice->mm[1]) && neighbor){
     V->sum += d;
     V->sum2 += d*d;
  }

  if ((choice->mm[2] || choice->mm[3]) && neighbor){			
     V->sum1[type_coh] += d;
     V->sum21[type_coh] += d*d;
  }

  if ((choice->mm[4] || choice->mm[5]) && neighbor){
     if (0 > (d_coh = index_coh(d, dist_cl))) return;
     if (choice->mm[4])
        V->den[d_coh] ++;
     if (choice->mm[5])
	if(type_coh < 0) 
	   return;
	else  
	   V->den2[type_coh][d_coh] ++;
  }
}






					/* DRIVER FOR CALCULATING DISTANCE
					   USING METHODS m2-m9 */

void  cal_nrst(patch_list, tmp, cc, ce, ee, meth, neighbor)
PATCH  *patch_list, *tmp;
float  *cc, *ce, *ee;
int    meth, *neighbor;
{
  register int     i,j;
  PATCH            *p_list = patch_list;
  int		   cnt=0;
  struct NPL       *npl;



/* VARIABLES:
	patch_list	=	the complete set of all patches contained
				   in the sampling area & their characteristics
	tmp		=	a pointer to a single member of the patch
				   list; this is the patch that is the "from"
				   patch in nearest neighbor calculations
	cc		=	passed in as 1 when center-center measurement
				   is desired, but is returned as the actual
				   cc distance
	ce		=	passed in as 1 when center-edge measurement
				   is desired, but is returned as the actual
				   ce distance
	ee		=	passed in as 1 when edge-edge measurement
				   is desired, but is returned as the actual
				   ee distance
	meth		=	1 = m2-m4; 2 = m5-m6; 3 = m7-m9 for parameter
				   di1.
	neighbor	=	a counter for the number of possible nearest
				   neighbor patches
*/



  npl = (struct NPL *)G_calloc(NN, sizeof(struct NPL));
  for(i=0; i<NN; i++)
     npl[i].d = BIG;

					/* go through the patch list looking
					   for patches that are nearest
					   neighbors */
				
  while(p_list){
     if (tmp->num != p_list->num ){  
        if (meth != 3 || 
	   (meth == 3 && *(*(from_to + 0) + 1) == (recl_coh(tmp->att) + 1)))
           srch_nrst(tmp, p_list, npl, &cnt, meth);  
     }
     p_list = p_list->next;
  }


  if (meth == 3 && *(*(from_to + 0) + 1) == (recl_coh(tmp->att) + 1)) 
     total_ft ++;

					/* if there are any nearest
					   neighbors, then measure distance */

  if (cnt>0)
     cal_dist(tmp, npl, cnt, cc, ce, ee);
					/* if there are no nearest
					   neighbors, then set the distance
					   to 0 */

  else if (cnt==0) {
     if(*cc) *cc = 0.0;
     if(*ce) *ce = 0.0;
     if(*ee) *ee = 0.0;
  }

  *neighbor = cnt;
  free(npl);  
}





					/* CALCULATE DISTANCES FOR THE
					   SELECTED MEASURES */

void  cal_dist(tmp, npl, cnt, cc, ce, ee)
PATCH      *tmp;
struct NPL *npl;
int        cnt;
float      *cc, *ce, *ee;
{
  register int  i, j, k;
  double        dt, d1, d2, d3, dis, row_diff, col_diff;
  int     	n1, n2=tmp->npts, dn1, dn2, num, att, r0, c0, r1, c1,
		fm_edge_row, fm_edge_col, to_edge_row, to_edge_col, 
		fm_cent_col, fm_cent_row, to_cent_col, to_cent_row,
		to_c_row, to_c_col;

  if(cnt < 1) return;
  dn2 = skip_int(n2);
  d1 = d2 = d3 = BIG;
  fm_cent_col = tmp->c_col;
  fm_cent_row = tmp->c_row;

					/* for all the patches in the nearest
					   neighbor array (total=NN) */

  for(i = 0; i < cnt; i++){ 
     if(*cc){  				/* if center-center */
        to_c_col = npl[i].p->c_col;
        to_c_row = npl[i].p->c_row;
        dt = ((to_c_row - fm_cent_row)*(to_c_row - fm_cent_row)) + 
	     ((to_c_col - fm_cent_col)*(to_c_col - fm_cent_col));

        if(d1 > dt) {
	   d1 = dt;
	   num = npl[i].p->num;
	   att = npl[i].p->att;
	   to_cent_row = to_c_row;
	   to_cent_col = to_c_col;
	}	
     }
     if(*ce || *ee){ 
	n1 = npl[i].p->npts;
	dn1 = skip_int(n1);
        for(j=0; j< n1; j+=dn1){
	    r1 = npl[i].p->row[j];
	    c1 = npl[i].p->col[j];

					/* if center-edge */

	    if(*ce){
		row_diff = (fabs((double)fm_cent_row - r1) - 0.5);
		col_diff = (fabs((double)fm_cent_col - c1) - 0.5);
		if(row_diff<0.0) row_diff = 0.0;
		if(col_diff<0.0) col_diff = 0.0;
		dt = row_diff*row_diff + col_diff*col_diff; 
		if(d2 > dt) {
		   d2 = dt;		
	   	   num = npl[i].p->num;
	   	   att = npl[i].p->att;
		   to_edge_row = r1;
		   to_edge_col = c1;
		}
	    }

					/* if edge-edge */

	    if(*ee){ 	      		
                for(k=0; k< n2; k+= dn2){
                    r0 = tmp->row[k];   
		    c0 = tmp->col[k];
		    if(r0==r1)
		       dt = (abs(c0 - c1) - 1)*(abs(c0 - c1) - 1);
		    else if(c0==c1)
		       dt = (abs(r0 - r1) - 1)*(abs(r0 - r1) - 1);
		    else if(c0!=c1 && r0!=r1)
		       dt = (abs(c0 - c1) - 1)*(abs(c0 - c1) - 1) + 
			    (abs(r0 - r1) - 1)*(abs(r0 - r1) - 1);
		    if(d3 > dt) {
		       d3 = dt;
	   	       num = npl[i].p->num;
	   	       att = npl[i].p->att;
		       fm_edge_row = r0;
		       fm_edge_col = c0;
		       to_edge_row = r1;
		       to_edge_col = c1;
		    }
		}
	    }
	}
     }  
  }



  					/* get the square root distance */
  if (*cc) {
     if (d1 && d1 < BIG)
        dis = *cc = sqrt(d1);
     else
	dis = *cc = 0.0;
     fm_edge_row = 0;
     fm_edge_col = 0;
     to_edge_row = 0;
     to_edge_col = 0;
  }

  if (*ce) {
     if (d2 > 0 && d2 < BIG)
        dis = *ce =  sqrt(d2);
     else
	dis = *ce = 0.0;
     to_cent_row = 0;
     to_cent_col = 0;
     fm_edge_row = 0;
     fm_edge_col = 0;
  }

  if (*ee) {
     if(d3 > 0 && d3 < BIG)
        dis = *ee =  sqrt(d3);
     else
	dis = *ee = 0.0;
     fm_cent_row = 0;
     fm_cent_col = 0;
     to_cent_row = 0;
     to_cent_col = 0;
  }


					/* if a file of individual distance
					   measures was requested */

  if (strcmp(choice->out,"") && choice->wrum != 'm')
     fprintf(outfile, "%5d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %8.2f\n", n_scale, n_unit, tmp->num, tmp->att, fm_cent_row, fm_cent_col, fm_edge_row, fm_edge_col, num, att, to_cent_row, to_cent_col, to_edge_row, to_edge_col, dis );

}





					/* DETERMINE IF AND HOW TO SKIP,
					   BASED ON THE USER'S INPUT */

int  skip_int(n)
int n;
{
  if (non_skip <=0) return 1;
  else n -= non_skip;

  if (n > K4) return 5;
  else if(n > K3) return 4;
  else if(n > K2) return 3;
  else if(n > K1) return 2;
  else 		  return 1;
}






					/* GET THE ARRAY OF NEAREST CANDIDATE
					   PATCHES FROM THE PATCH LIST */

void  srch_nrst(p_cur, p_nb, npl, cnt, method)
PATCH      *p_cur, *p_nb;
struct NPL *npl;
int        *cnt, method;
{
  int  gp1, gp2, d, dp=0, ip=0;
  register int i;

/* gp1 is the cohort of the current patch 
   gp2 is the cohort of each patch on the patch list
*/

  gp1 = recl_coh(p_cur->att) + 1;
  gp2 = recl_coh(p_nb->att) + 1;
 

  if (method != 3) {
     if(0 > gp1 || 0 > gp2)  
	return;
     else if (method == 1 && gp1 != gp2)
	return;
     else if (method == 2 && gp1 == gp2)
	return;   
  }

  else if (*(*(from_to + 1) + 1) != gp2)
     return;

  					/* calculate the distance of bounding 
					   box used to locate neighbors */

  d = cal_box_d(p_cur, p_nb);
					/* if less than NN, put into the
					   candidate array */

  if(*cnt < NN){  
     npl[*cnt].d = d;
     npl[*cnt].p = p_nb;
     ++ *cnt;
  }

					/* otherwise replace the one of 
					   bigger distance */

 
  else { 
     for(i=0; i<NN; i++)
        if(dp < npl[i].d){
	   dp = npl[i].d;   ip = i;
        }
     if(d < dp){
        npl[ip].p = p_nb;
	npl[ip].d = d;
     }  
  }
}






					/* CALCULATE THE DISTANCE BETWEEN 
					   BOUNDING BOXES OF 2 PATCHES */

int  cal_box_d(p1, p2)
PATCH *p1, *p2;
{
  int           d, e1=p1->e, s1=p1->s, w1=p1->w, n1=p1->n,
		e2=p2->e, s2=p2->s, w2=p2->w, n2=p2->n;
  register int  i;

  if(s1 <= n2){
	if(w2 > e1) d = n2-s1 + w2-e1;
	else if(e2 < w1) d = (n2-s1)*(n2-s1) + (w1-e2)*(w1-e2);
	else d = (n2 - s1)*(n2-s1);
  } 
  else if(s2 <= n1){
	if(w2 > e1) d = (w2-e1)*(w2-e1) + (n1-s2)*(n1-s2);
	else if(e2 < w1) d = (w1-e2)*(w1-e2) + (n1-s2)*(n1-s2);
	else d = (n1 - s2)*(n1-s2);
  } 
  else if(w1 > e2)
	d = (w1 - e2)*(w1-e2);
  else if(w2 > e1)
	d = (w2 - e1)*(w2-e1);
  else d = 0;
  return d;
}





					/* DRIVER FOR CALCULATING DISTANCES
					   USING METHODS m0-m1 */

void  cal_adj(patch_list, tmp, cc, ce, n)
PATCH  *patch_list, *tmp;
float  *ce, *cc;
int    *n;
{
  static int       **buf, nr;
  struct Cell_head wind;
  register int     i;
  int		   r, c;
  PATCH            *p_list = patch_list;

  if(tmp->num == 1){
     					/* setup the adj-buffer before 
					   searching */

     G_get_set_window(&wind);
     nr = wind.rows;
     buf = (int **)G_calloc(nr+2, sizeof(int *));
     for(i=0; i<nr+2; i++)
        buf[i] = (int *)G_calloc(wind.cols+2, sizeof(int));

     while(p_list){

					 /* put all the boundary points 
					    into the adj-buffer */

  
	for(i=0; i<p_list->npts; i++){
	   r = p_list->row[i];
	   c = p_list->col[i];
	   buf[r][c] = p_list->num;
	}
	p_list = p_list->next;
     }
  }
  
  srch_bdry(patch_list, tmp, buf, cc, ce, n);  

					 /* release buffer after all done */

 
  if( tmp->next == NULLPTR) {  
     for(i = 0; i < nr + 2; i++) 
        free(buf[i]);
     free(buf); 
  }
}





					/* SEARCH ALONG A BOUNDARY TO FIND
					   THE ADJACENT PATCHES */

void   srch_bdry(patch_list, tmp, buf, cc, ce, nn)
PATCH  *patch_list, *tmp;
int    **buf, *nn;
float  *cc, *ce;
{
  register int  i, j;
  int		*nb, cnt=0, n=tmp->npts, dn, 
		fm_cent_col, fm_cent_row, fm_edge_col, fm_edge_row,
		to_cent_col, to_cent_row, to_edge_col, to_edge_row,
		num, att;
  float  	dcc=0, dce=0, dis;
  PATCH 	*p_nb;
  struct NB     *nbs;

  nbs = (struct NB *)G_malloc(500*sizeof(struct NB));

					/* for each pt find the adjacent
					   patches */
  
  dn = skip_int(n);
  for(i = 0; i < n; i+=dn){
     nb = (int *)G_calloc(8, sizeof(int));
     check_nbs(tmp->num, tmp->row[i], tmp->col[i], buf, nb);
     set_nb_list(nb, tmp->row[i], tmp->col[i], tmp->c_row, tmp->c_col, 
        nbs, &cnt);
     free(nb);
  }

					/* calculate distances */

  for(i = 0; i < cnt; i++){

					/* if center-center */

    if(cc){
	p_nb = get_patch(patch_list, nbs[i].num);
	if ((p_nb->c_row - tmp->c_row)*(p_nb->c_row - tmp->c_row) + 
	    (p_nb->c_col - tmp->c_col)*(p_nb->c_col - tmp->c_col))
   	   cc[i] = sqrt((p_nb->c_row - tmp->c_row)*(p_nb->c_row - tmp->c_row) + 
	      (p_nb->c_col - tmp->c_col)*(p_nb->c_col - tmp->c_col));
	else cc[i] = 0.0;
	fm_edge_col = 0;
	fm_edge_row = 0;
	fm_cent_col = tmp->c_col;
	fm_cent_row = tmp->c_row;
	to_edge_col = 0;
	to_edge_row = 0;
	to_cent_col = p_nb->c_col;
	to_cent_row = p_nb->c_row;
	dis = cc[i];
	num = p_nb->num;
	att = p_nb->att;
    }

					/* if center-edge */

    if (ce) {
	p_nb = get_patch(patch_list, nbs[i].num);
	if (nbs[i].d > 0)
           ce[i] = sqrt(nbs[i].d);
	else ce[i] = 0.0;
	fm_cent_col = tmp->c_col;
	fm_cent_row = tmp->c_row;
	fm_edge_col = 0;
	fm_edge_row = 0;
	to_cent_col = 0;
	to_cent_row = 0;
	to_edge_col = nbs[i].c;
	to_edge_row = nbs[i].r;
	dis = ce[i];
	num = p_nb->num;
	att = p_nb->att;
    }

					/* if a file of individual distance
					   measures was requested */

    if (strcmp(choice->out,"") && choice->wrum != 'm')
       fprintf(outfile, "%5d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %8.2f\n", n_scale, n_unit, tmp->num, tmp->att, fm_cent_row, fm_cent_col, fm_edge_row, fm_edge_col, num, att, to_cent_row, to_cent_col, to_edge_row, to_edge_col, dis);


  }

  total += cnt; 
  *nn = cnt;
  free(nbs);

}




					/* USE SEQUENTIAL NUMBER TO GET THE
					   PATCH FROM THE PATCH LIST */

PATCH *get_patch(patch_list, n)
PATCH *patch_list;
int n;
{
  PATCH *tmp = patch_list;

  while(tmp && tmp->num != n)
    tmp = tmp->next;
  if(!tmp) {
     printf("\n");
     printf("   *******************************************************\n");
     printf("    No adjacent patches could be located, so r.le.dist    \n");
     printf("    cannot complete the requested analysis.               \n");
     printf("   *******************************************************\n");
     exit(1);
  }
  return tmp;
}





					/* CALCULATE THE ADJACENT NEIGHBOR
					   ARRAY */

void   set_nb_list(nb, ii, jj, ci, cj, nbs, cnt)
int       *nb, *cnt, ii, jj;
double    ci, cj;
struct NB nbs[];
{
  register int i=0, j;
  float        x, y, d;
  int  	       i0 = -1, j0 = 0;

  for(j = 0; j < 8; j++){
     if (!nb[j]) goto next;

     for(i = 0; i < *cnt; i++)
        if (nb[j] == nbs[i].num) break;

     if (i >= *cnt){
        nbs[i].num = nb[j];
        ++ *cnt;
        if (choice->mn[1]){
	   x = fabs(ci - (ii + i0)) - 0.5;
	   y = fabs(cj - (jj + j0)) - 0.5;
           if (x < 0) x = 0.0;
           if (y < 0) y = 0.0;
	   nbs[i].d = x*x + y*y;
	   nbs[i].r = ii + i0;
	   nbs[i].c = jj + j0;
        }     
     } 
     else if (choice->mn[1]) {
        x = fabs(ci - (ii + i0)) - 0.5;   
	y = fabs(cj - (jj + j0)) - 0.5;
        if (x < 0) x = 0.0;
        if (y < 0) y = 0.0;
        if ((d = x*x + y*y) < nbs[i].d) {
	   nbs[i].d = d;
	   nbs[i].r = ii + i0;
	   nbs[i].c = jj + j0;
	} 	    
     }

next:   
     clockwise(&i0, &j0);
 
  }   
}





					/* CHECK ADJACENT NEIGHBORS OF A
					   BOUNDARY PT TO FIND A PATCH WHOSE
					   PATCH NUMBER IS DIFFERENT FROM THAT
					   OF THE BOUNDARY PT.  THEN THIS IS
					   AN ADJACENT PATCH */

void  check_nbs(center, cr, cc, buf, nb)
int   center, cr, cc, **buf, *nb;
{
int	i;
  if(buf[cr-1][cc] && buf[cr-1][cc] != center)
     nb[0] = buf[cr-1][cc];
  
  if(buf[cr-1][cc+1] && buf[cr-1][cc+1] != center)
     nb[1] = buf[cr-1][cc+1];
 
  if(buf[cr][cc+1] && buf[cr][cc+1] != center)
     nb[2] = buf[cr][cc+1];
 
  if(buf[cr+1][cc+1] && buf[cr+1][cc+1] != center)
     nb[3] = buf[cr+1][cc+1];
 
  if(buf[cr+1][cc] && buf[cr+1][cc] != center)
     nb[4] = buf[cr+1][cc];
	 
  if(buf[cr+1][cc-1] && buf[cr+1][cc-1] != center)
     nb[5] = buf[cr+1][cc-1];

  if(buf[cr][cc-1] && buf[cr][cc-1] != center)
     nb[6] = buf[cr][cc-1];

  if(buf[cr-1][cc-1] && buf[cr-1][cc-1] != center)
     nb[7] = buf[cr-1][cc-1]; 
}






					/* SAVE THE CALCULATED DISTANCES */

void  save_dist(v1, v2, v3, Vcc, Vce, Vee, dens, meth)
int 		v1, v2, v3, *dens, meth;
struct VALUE    *Vcc, *Vce, *Vee;
{
  register int	i;
  char		*str;
  double        m;
  FILE   	*fp;
  int    	tot;

  if(meth < 0) tot = total;
  else if(meth == 3) tot = total_ft;
  else tot = total_patches;

  if(v1)
     save_dist1(Vcc, dist_CC, tot, dens);		
  if(v2)
     save_dist1(Vce, dist_CE, tot, dens);
  if(v3)
     save_dist1(Vee, dist_EE, tot, dens);

  if(choice->mm[5]){
     str = G_malloc(80);
     sprintf(str, "r.le.out/%s", "n6.out");
     fp = fopen0(str, "a");
     free(str);

     fprintf(fp, "%8d%8d\n", n_scale, n_unit);
     if(v1)
        save_dist2(fp, Vcc, ntype, (int)(dist_CC[0]-1));		
     if(v2)
        save_dist2(fp, Vce, ntype, (int)(dist_CE[0]-1));
     if(v3)
        save_dist2(fp, Vee, ntype, (int)(dist_EE[0]-1));
     fprintf(fp, "\n\n");
     fclose(fp);
  }
}





					/* SAVE THE NUMBER OF DISTANCES
					   BY DISTANCE CLASS BY GP */

void  save_dist2(fp, V, n1, n2)
FILE         *fp;
int          n1, n2;
struct VALUE *V;
{
  register int i, j;
  
  for(i=0; i<n1; i++){
     for(j=0; j<n2; j++)
					/* write n6 */

       fprintf(fp, "%7d", V->den2[i][j]);		
     fprintf(fp, "\n");
  }
  fprintf(fp, "\n");
}





					/* SAVE MEAN, ST. DEV., AND NO.
					   BY DISTANCE CLASS */

void  save_dist1(V, n, tot, dens)
struct VALUE *V;
float        *n;
int	     tot, dens[];
{
  register int i;
  double       m=0, stdv=0;
  FILE       *fp ;

  if(tot){
     m = V->sum / tot;  
     stdv = V->sum2/tot - m*m;
     if(stdv) stdv = sqrt(stdv);
     else stdv = 0;
  }

					/* open the appropriate output files */

					/* write n1 = mean distance & n2 = st.
					   dev. distance  */

 if( choice->mm[0] || choice->mm[1] ) {				
     fp = fopen0("r.le.out/n1-2.out", "a");
     fprintf(fp, "%8d%8d", n_scale, n_unit);
     fprintf(fp, "%8.2f%8.2f", m, stdv);
     fprintf(fp, "\n");
     fclose(fp);
 }

					/* write n3 = mean distance by gp */


 if(choice->mm[2]) {						 
     fp = fopen0("r.le.out/n3.out", "a");
     fprintf(fp, "%8d%8d", n_scale, n_unit);

					/* calc. mean distance by group */


     for(i=0; i<ntype; i++){					
	m = 0;
	if(dens[i]) m = V->sum1[i]/dens[i];
	fprintf(fp, "%8.2f", m);
     }
     fprintf(fp, "\n");
     fclose(fp);
 }

					/* n4 = st. dev. distance by gp  */


 if(choice->mm[3]){						
     fp = fopen0("r.le.out/n4.out", "a");
     fprintf(fp, "%8d%8d", n_scale, n_unit);

					/* calc. st. dev. distance by gp */	
     for(i=0; i<ntype; i++){					
	m = stdv = 0;
	if(dens[i]){
	     m = V->sum1[i]/dens[i];
	     if((stdv = V->sum21[i]/dens[i] - m*m) > 0)
		 stdv = sqrt(stdv);
	     else stdv = 0;
        }
	fprintf(fp, "%8.2f", stdv);
     }
     fprintf(fp, "\n");
     fclose(fp);
 }

					/* write n5 = no. of dist. by dist.
					   class */


 if(choice->mm[4]){						
     fp = fopen0("r.le.out/n5.out", "a");
     fprintf(fp, "%8d%8d", n_scale, n_unit);
     for(i=0; i < *n-1; i++)
       fprintf(fp, "%7d", V->den[i]);
     fprintf(fp, "\n");
     fclose(fp);
 }

}





					/* WHICH ATTRIBUTE GP DOES THE 
					   ATTRIBUTE BELONG TO */

int  in_group(att, group)
int   att;
float *group;
{
  register int i;

  for(i=1; i<*group; i++) {
     if(-999 == *(group+i)){
        if( *(group+i) && *(group+i-1) <= att && att <= *(group+i+1)) {
 	   return 1;
	}
	else 
	   i++;
     } 
     else if(*(group+i) == att) {
	return 1;
     }
  }
  return 0;
}




					/* WHICH INDEX COHORT DOES THE
					   ATTRIBUTE BELONG TO */


int  index_coh(att, group)
float  att;
float  *group;
{
  register int i;

  for(i=*group-1; i>=1; i--)
    if(*(group+i) <= att)
 	     return i-1;
  return -999;
}





					/* WHICH ATTRIBUTE GP DOES THE 
					   ATTRIBUTE BELONG TO */

int  recl_coh(att)
int    att;
{
  register int i,j;
  extern int   ntype;
  extern float **recl_tb;

  for(i=0; i < ntype; i++) {
    if(in_group(att, recl_tb[i])) {
        return i; 
    }
  }

  return -999;
}




					/* SQUARE ROOT DISTANCE */

double  eu_d(x1, y1, x2, y2)
double  x1, y1, x2, y2;
{
  return( sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) );
}








