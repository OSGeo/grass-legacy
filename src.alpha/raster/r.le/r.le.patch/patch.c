				/********************************/
				/*	r.le.patch/patch.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*	Version 06/15/94	*/
				/*				*/
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/
#include "r.le.patch.h"

extern struct CHOICE   *choice;
extern int 	       total_patches, ntype, n_scale, n_unit;
extern float           *shape_PA, *shape_CPA, *shape_RCC, **recl_tb, *size_cl;
FILE     	       *a5,*a6,*a1_4,*a7,*c1_4,*c5,*c6,*c7,*c8,*c9c,*c9e,*c10c,
			*c10e,*s3,*s4,*s5,*s6,*s1_2,*h3,*h4,*h5,*h6,*h1_2,*p4,
			*p5,*p6,*p1_3,*outfile;


/** run the default patch measures **/
void  df_patch (patch_list)
PATCH  *patch_list;
{
  PATCH   *tmp = patch_list, *tmp0 ;
  int     *type_dens, type_coh;
  char	  path[30];

  if(!total_patches) return;


  /***********************************************************/

  if (choice->att[1] || choice->att[2] || 
      choice->att[3] || choice->att[4]) { 
     a1_4 = fopen0("r.le.out/a1-4.out", "a");
     fprintf(a1_4, "%4d%4d", n_scale, n_unit);
  }

  if (choice->att[5]) { 
     a5 = fopen0("r.le.out/a5.out", "a");
     fprintf(a5, "%4d%4d", n_scale, n_unit);
  }

  if (choice->att[6]) { 
     a6 = fopen0("r.le.out/a6.out", "a");
     fprintf(a6, "%4d%4d", n_scale, n_unit);
  }

  if (choice->att[7]) { 
     a7 = fopen0("r.le.out/a7.out", "a");
     fprintf(a7, "%4d%4d", n_scale, n_unit);
  }

  /***********************************************************/

  if (choice->size[1] || choice->size[2]) { 
     s1_2 = fopen0("r.le.out/s1-2.out", "a");
     fprintf(s1_2, "%4d%4d", n_scale, n_unit);
  }

  if (choice->size[3]) { 
     s3 = fopen0("r.le.out/s3.out", "a");
     fprintf(s3, "%4d%4d", n_scale, n_unit);
  }

  if (choice->size[4]) { 
     s4 = fopen0("r.le.out/s4.out", "a");
     fprintf(s4, "%4d%4d", n_scale, n_unit);
  }

  if (choice->size[5]) { 
     s5 = fopen0("r.le.out/s5.out", "a");
     fprintf(s5, "%4d%4d", n_scale, n_unit);
  }

  if (choice->size[6]) { 
     s6 = fopen0("r.le.out/s6.out", "a");
     fprintf(s6, "%4d%4d", n_scale, n_unit);
  }



  /***********************************************************/

  if (choice->core[1] || choice->core[2] || 
      choice->core[3] || choice->core[4]) { 
     c1_4 = fopen0("r.le.out/c1-4.out", "a");
     fprintf(c1_4, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[5]) { 
     c5 = fopen0("r.le.out/c5.out", "a");
     fprintf(c5, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[6]) { 
     c6 = fopen0("r.le.out/c6.out", "a");
     fprintf(c6, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[7]) { 
     c7 = fopen0("r.le.out/c7.out", "a");
     fprintf(c7, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[8]) { 
     c8 = fopen0("r.le.out/c8.out", "a");
     fprintf(c8, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[9]) { 
     c9c = fopen0("r.le.out/c9c.out", "a");
     fprintf(c9c, "%4d%4d", n_scale, n_unit);
     c9e = fopen0("r.le.out/c9e.out", "a");
     fprintf(c9e, "%4d%4d", n_scale, n_unit);
  }

  if (choice->core[10]) { 
     c10c = fopen0("r.le.out/c10c.out", "a");
     fprintf(c10c, "%4d%4d", n_scale, n_unit);
     c10e = fopen0("r.le.out/c10e.out", "a");
     fprintf(c10e, "%4d%4d", n_scale, n_unit);
  }

/************************************************************/  

  if (choice->shape[1] || choice->shape[2]) { 
     h1_2 = fopen0("r.le.out/h1-2.out", "a");
     fprintf(h1_2, "%4d%4d", n_scale, n_unit);
  }

  if (choice->shape[3]) { 
     h3 = fopen0("r.le.out/h3.out", "a");
     fprintf(h3, "%4d%4d", n_scale, n_unit);
  }

  if (choice->shape[4]) { 
     h4 = fopen0("r.le.out/h4.out", "a");
     fprintf(h4, "%4d%4d", n_scale, n_unit);
  }

  if (choice->shape[5]) { 
     h5 = fopen0("r.le.out/h5.out", "a");
     fprintf(h5, "%4d%4d", n_scale, n_unit);
  }

  if (choice->shape[6]) { 
     h6 = fopen0("r.le.out/h6.out", "a");
     fprintf(h6, "%4d%4d", n_scale, n_unit);
  }


/************************************************************/  


  if (choice->perim[1] || choice->perim[2] || 
      choice->perim[3]) { 
     p1_3 = fopen0("r.le.out/p1-3.out", "a");
     fprintf(p1_3, "%4d%4d", n_scale, n_unit);
  }

  if (choice->perim[4]) { 
     p4 = fopen0("r.le.out/p4.out", "a");
     fprintf(p4, "%4d%4d", n_scale, n_unit);
  }

  if (choice->perim[5]) { 
     p5 = fopen0("r.le.out/p5.out", "a");
     fprintf(p5, "%4d%4d", n_scale, n_unit);
  }

  if (choice->perim[6]) { 
     p6 = fopen0("r.le.out/p6.out", "a");
     fprintf(p6, "%4d%4d", n_scale, n_unit);
  }

/************************************************************/  

  if(strcmp(choice->out,"") && choice->wrum != 'm') {
     sprintf(path, "r.le.out/%s", choice->out);
     outfile = fopen0(path, "a");
     if(!strcmp(choice->out, "head"))
	fprintf(outfile, "sc-  un-               center     patch     core     edge               shape index\n");
        fprintf(outfile, "ale  it    num   att  row  col     size     size     size      per    P/A   CP/A    RCC\n");
  }


/************************************************************/  


  type_dens = (int *)G_calloc(25, sizeof(int));

			/* for each patch on the patch list */

  while(tmp) {
     if((type_coh = recl_coh(tmp->att)) >= 0)
        type_dens[type_coh] ++;
     if (choice->att[0]) 
        df_att(tmp, type_coh, type_dens);
     if (choice->core[0])
        df_core(tmp, type_coh, type_dens);
     if (choice->size[0])
        df_size(tmp, type_coh, type_dens);
     if (choice->shape[0]) 
        df_shape(tmp, type_coh, type_dens);
     if (choice->perim[0]) 
        df_perim(tmp, type_coh, type_dens);
     if(choice->fract) 
        df_fract(tmp);
     if(strcmp(choice->out,"") && choice->wrum != 'm')
        fprintf(outfile, "%3d %3d %6d %5d %4.0f %4.0f %8.0f %8.0f %8.0f %8.0f %6.3f %6.3f %6.3f\n", n_scale, n_unit, tmp->num, tmp->att, tmp->c_row, tmp->c_col, tmp->area, tmp->core, tmp->edge, tmp->perim, tmp->perim/tmp->area, 0.282*tmp->perim/sqrt(tmp->area), 2.0*sqrt(tmp->area/M_PI)/tmp->long_axis);
   
     tmp0 = tmp;
     tmp = tmp->next;
     free(tmp0);
  }

  fclose(a1_4);
  fclose(a5);
  fclose(a6);
  fclose(a7);

  fclose(c1_4);
  fclose(c5);
  fclose(c6);
  fclose(c7);
  fclose(c8);
  fclose(c9c);
  fclose(c9e);
  fclose(c10c);
  fclose(c10e);

  fclose(s1_2);
  fclose(s3);
  fclose(s4);
  fclose(s5);
  fclose(s6);

  fclose(h1_2);
  fclose(h3);
  fclose(h4);
  fclose(h5);
  fclose(h6);

  fclose(p1_3);
  fclose(p4);
  fclose(p5);
  fclose(p6);

  fclose(outfile);

  free(type_dens);
  total_patches = 0;
}



/************************************/
/*  compute the attribute measures  */
/************************************/

void   df_att(tmp, type_coh, type_dens)
PATCH *tmp;
int   type_coh, *type_dens;
{
  static   int w_att=0, w_att2=0, total=0, sumx=0, sumx2=0, *density, *area;
  register int  i;


/* variables:
	w_att =      sum of (patch attributes x areas)
	w_att2 =     sum of (patch attributes x areas squared)
	t_size =     sum of all the patch areas
	sumx =       sum of all the patch attributes
	sumx2 =      sum of all the patch attributes squared
	area[] =     array of total areas by gp
	density[] =  no. of patches by gp
*/

  
  if(tmp->num == 1)
	area = (int *)G_calloc(25, sizeof(int));

  sumx += tmp->att;
  sumx2 += tmp->att * tmp->att;
  w_att += tmp->area * tmp->att;
  w_att2 += tmp->area * tmp->att * tmp->att;
  total += tmp->area;
  area[type_coh] += tmp->area;

/*printf("area[%d] = %5.0f",type_coh,tmp->area);
*/
     
  if(!tmp->next){
      save_att(w_att, w_att2, total, sumx, sumx2, type_dens, area); 
      w_att = w_att2 = total = sumx = sumx2 = 0;    
      free(area);
  }
}


/** save the attribute measures **/
void  save_att(w_att, w_att2, t_size, sum, sum2, density, area)
int   w_att, w_att2, t_size, sum, sum2, *density;
int   *area;
{
  register int	i;
  double        wm, wstdv, m, stdv;

/* variables:
     IN: from df_att
	w_att =    sum of (patch attributes x areas)
	w_att2 =   sum of (patch attributes x areas squared)
	t_size =   sum of all the patch areas
	sum =      sum of all the patch attributes
	sum2 =     sum of all the patch attributes squared
	area[] =   array of total areas by gp
	density =  no. of patches by gp
     INTERNAL:
	wm = 	   mean pixel attribute (a1)
	wstdv =	   st. dev. pixel attribute (a2)
	m =	   mean patch attribute (a3)
	stdv =	   st. dev. patch attribute (a4)
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)	
*/ 

  wm = (double)w_att / t_size;
  wstdv = (double)w_att2/t_size - (double)wm*wm;
  if(wstdv > 0) wstdv = sqrt(wstdv);
  else wstdv = 0;
  m = (double)sum / total_patches;  
  stdv = (double)sum2/total_patches - (double)m*m;
  if(stdv > 0) stdv = sqrt(stdv);
  else stdv = 0;

     			/* write a1=mn. pix. att., a2=s.d. pix. att., 
			   a3=mn. patch att., a4=s.d. patch att. */
	
  if(choice->att[1] || choice->att[2] || 
     choice->att[3] || choice->att[4]) {
     fprintf(a1_4, "  %10.2f  %10.2f  %10.2f  %10.2f\n", wm, wstdv, m, stdv);
  }

     			/* write a5 = cover by gp */

  if(choice->att[5]) {
     for(i=0; i<ntype; i++)
        fprintf(a5, " %6.4f", (double)area[i]/t_size);
     fprintf(a5, "\n");
  }

     			/* write a6 = density by gp */

  if(choice->att[6]) {
     for(i=0; i<ntype; i++)
        fprintf(a6, " %5d", *(density+i));
     fprintf(a6, "\n");
  }

			/* write a7 = total patches */

  if(choice->att[7])
     fprintf(a7, " %6d\n",total_patches);

}



/** compute the core measures **/
void   df_core(tmp, type_coh, type_dens)
PATCH *tmp;
int   type_coh, *type_dens;
{
  static   int  **density1c=NULL,  **density1e=NULL, 
		*densityc=NULL, *densitye=NULL, first=1;    
  static double mcore=0, medge=0, *mcore1=NULL,  *medge1=NULL, 
		sumc2=0, sume2=0, *sum22c, *sum22e; 	
  register int  i;
  int	        core_coh, edge_coh;
  
/* variables:
     IN:
 	type_coh =	identif. no. for ea. gp.	
	type_dens[] = 	array of no. of patches by gp ?
     INTERNAL:
	mcore =		sum of patch core sizes
	medge =		sum of patch edge sizes
	sumc2 =		sum of patch cores squared
	sume2 =		sum of patch edges squared
	mcore1[] =	array of patch cores by gp
	medge1[] =	array of patch edges by gp
	sum22c[] =	array of patch cores squared by gp
	sum22e[] =	array of patch edges squared by gp
	densityc[] =	array of no. of patches by core size class
	densitye[] =	array of no. of patches by edge size class
	density1c[] =	array of no. of patches by core size class
			by gp
	density1e[] =	array of no. of patches by edge size class
			by gp

     GLOBAL:
*/


  if(first){
    densityc = (int *)G_calloc(25, sizeof(int));
    densitye = (int *)G_calloc(25, sizeof(int));
    sum22c = (double *)G_calloc(25, sizeof(double)); 
    sum22e = (double *)G_calloc(25, sizeof(double)); 
    mcore1 = (double *)G_calloc(25, sizeof(double)); 
    medge1 = (double *)G_calloc(25, sizeof(double)); 
  }

		/* if output is by size class (c9 & c10) determine 
		   which size class the current patch is in */

  if (choice->core[9] || choice->core[10]) {
     core_coh = index_coh(tmp->core, size_cl);
     densityc[core_coh] ++;
     edge_coh = index_coh(tmp->edge, size_cl);
     densitye[edge_coh] ++;
  }

  mcore += tmp->core;
  medge += tmp->edge;
  sumc2 += tmp->core * tmp->core;
  sume2 += tmp->edge * tmp->edge;

  if(type_coh >= 0) {
      mcore1[type_coh] += tmp->core;
      medge1[type_coh] += tmp->edge;
      sum22c[type_coh] += tmp->core * tmp->core;
      sum22e[type_coh] += tmp->edge * tmp->edge;
  }

			/* if c10 */

  if(choice->core2) {
     if(first){
	density1c = (int **)G_calloc(25, sizeof(int *));
        for(i=0; i<25; i++)
 	   density1c[i] = (int *)G_calloc(25, sizeof(int));
	density1e = (int **)G_calloc(25, sizeof(int *));
        for(i=0; i<25; i++)
 	   density1e[i] = (int *)G_calloc(25, sizeof(int));
     }
     if(type_coh >= 0) {
	if (core_coh >= 0)
	   density1c[type_coh][core_coh] ++;
        if (edge_coh >= 0)
	   density1e[type_coh][edge_coh] ++;
     }
  } 

  if(first) first = 0;

  if(!tmp->next){
        save_core(sumc2, sume2, mcore, medge, mcore1, medge1, sum22c, sum22e, 
		  densityc, densitye, type_dens, density1c, density1e); 
        mcore = medge = sumc2 = sume2 = 0;
        free(densityc);
	free(densitye);
	free(sum22c);
	free(sum22e);
        free(mcore1);
	free(medge1);
	if(density1c || density1e){
           for(i=0; i<25; i++)
             free(density1c[i]);
	     free(density1e[i]);
	   free(density1c);  
	   free(density1e);
	}
	first = 1;      
  }
}




/** save the core measures **/
void  save_core(sumc2, sume2, mcore, medge, mcore1, medge1, sum22c, sum22e, 
		  densityc, densitye, type_dens, density1c, density1e)
int     *densityc, *densitye, *type_dens, **density1c, **density1e;
double  mcore, medge, *mcore1, *medge1, sumc2, sume2, *sum22c, *sum22e;
{
  register int	i, j;
  float    	tmpc, tmpe, stdvc, stdve;
  FILE   	*fpc, *fpe;


/* variables:
     IN:
	sumc2 =		sum of patch cores squared
	sume2 =		sum of patch edges squared
	mcore =		sum of patch cores
	medge =		sum of patch edges
	mcore1[] =	array of sums of patch cores by gp
	medge1[] =	array of sums of patch edges by gp
	sum22c[] =	array of patch cores squared by gp
	sum22e[] =	array of patch edges squared by gp
	densityc[] =	array of no. of patches by core size class
	densitye[] =	array of no. of patches by edge size class
	type_dens[] = 	array of no. of patches by gp ?
     INTERNAL:
	tmpc =		mean patch core (c1 & c5)
	stdvc =		st. dev. patch core (c2 & c6)
	tmpe =		mean patch edge (c3 & c7)
	stdve =		st. dev. patch edge (c4 & c8)		
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)
*/


		/* calc. & write c1=mn. core, c2=s.d. core,
		   c3=mn. edge, c4=s.d. edge */

  if(choice->core[1] || choice->core[2] || 
     choice->core[3] || choice->core[4]) {
     tmpc = mcore/total_patches;
     stdvc = sumc2/total_patches - tmpc*tmpc;
     if(stdvc > 0) stdvc = sqrt(stdvc);
     else  stdvc = 0;
     tmpe = medge/total_patches;
     stdve = sume2/total_patches - tmpe*tmpe;
     if(stdve > 0) stdve = sqrt(stdve);
     else  stdve = 0;
     fprintf(c1_4, " %11.2f %11.2f %11.2f %11.2f\n", tmpc, stdvc, 
	     tmpe, stdve);
  }

		/* calc. & write c5=mn. core by gp */

  if(choice->core[5]) {
     for(i=0; i<ntype; i++){
         if(tmpc = type_dens[i]) 
            tmpc = mcore1[i] / tmpc;
         fprintf(c5, " %11.2f", tmpc);
     }
     fprintf(c5, "\n");
  }

		/* calc. & write c6=s.d. core by gp */

  if(choice->core[6]) {
     for(i=0; i<ntype; i++){
         stdvc = 0;
         if(type_dens[i]){
            tmpc = mcore1[i] / type_dens[i];
            stdvc = sum22c[i]/type_dens[i] - tmpc*tmpc;
            if(stdvc > 0) stdvc = sqrt(stdvc);
	    else stdvc = 0;
         } 
         fprintf(c6, " %11.2f", stdvc);
     }
     fprintf(c6, "\n");
  }
    
		/* calc. & write c7=mn. edge by gp */

  if(choice->core[7]) {
     for(i=0; i<ntype; i++){
         if(tmpe = type_dens[i]) 
            tmpe = medge1[i] / tmpe;
         fprintf(c7, " %11.2f", tmpe);
     }
     fprintf(c7, "\n");
  }

		/* calc. & write c8=s.d. edge by gp */

  if(choice->core[8]) {
     for(i=0; i<ntype; i++){
         stdve = 0;
         if(type_dens[i]){
            tmpe = medge1[i] / type_dens[i];
            stdve = sum22e[i]/type_dens[i] - tmpe*tmpe;
            if(stdve > 0) stdve = sqrt(stdve);
	    else stdve = 0;
         } 
         fprintf(c8, " %11.2f", stdve);
     }
     fprintf(c6, "\n");
  }
    
		/* write c9=no. by size class */

  if (choice->core[9]) {
     for(i=0; i<size_cl[0]-1; i++)
         fprintf(c9c, " %5d", *(densityc+i));
     fprintf(c9c, "\n");
     for(i=0; i<size_cl[0]-1; i++)
         fprintf(c9e, " %5d", *(densitye+i));
     fprintf(c9c, "\n");
  }
  
  if (choice->core2){
     if (!(fpc = fopen("r.le.out/c10c.out", "a")))
        G_fatal_error("Can't write file c10c.out; do you have write permission?\n");
     if (!(fpe = fopen("r.le.out/c10e.out", "a")))
	G_fatal_error("Can't write file c10e.out; do you have write permission?\n");

		/* write c10=no. by size class by gp */

     fprintf(fpc, "%4d%4d\n", n_scale, n_unit);
     for(i=0; i<ntype; i++){
        for(j=0; j<*size_cl-1; j++) 
            fprintf(fpc, " %5d", density1c[i][j]);
	fprintf(fpc, "\n");
     }
     fprintf(fpc, "\n\n");
     fclose(fpc);
     fprintf(fpe, "%4d%4d\n", n_scale, n_unit);
     for(i=0; i<ntype; i++){
        for(j=0; j<*size_cl-1; j++)
            fprintf(fpe, " %5d", density1e[i][j]);
	fprintf(fpe, "\n");
     }
     fprintf(fpe, "\n\n");
     fclose(fpe);
  }


}



/** compute the size measures **/
void   df_size(tmp, type_coh, type_dens)
PATCH *tmp;
int   type_coh, *type_dens;
{
  static   int  **density1=NULL, *density=NULL, first=1;    
  static double msize=0, *msize1=NULL, sum2=0, *sum22; 	
  register int  i;
  int	        size_coh;
  
/* variables:
     IN:
 	type_coh =	identif. no. for ea. gp.	
	type_dens[] = 	array of no. of patches by gp ?
     INTERNAL:
	msize =		sum of patch sizes
	sum2 =		sum of patch sizes squared
	msize1[] =	array of patch sizes by gp
	sum22[] =	array of patch sizes squared by gp
	density[] =	array of no. of patches by size class
     GLOBAL:
*/


  if(first){
    density = (int *)G_calloc(25, sizeof(int));
    sum22 = (double *)G_calloc(25, sizeof(double)); 
    msize1 = (double *)G_calloc(25, sizeof(double)); 
  }

		/* if output is by size class (s5 & s6) determine which
		   size class the current patch is in */

  if (choice->size[5] || choice->size[6]) {
     size_coh = index_coh(tmp->area, size_cl);
     density[size_coh] ++;
  }

  msize += tmp->area;
  sum2 += tmp->area * tmp->area;

  if(type_coh >= 0) {
      msize1[type_coh] += tmp->area;
      sum22[type_coh] += tmp->area * tmp->area;
  }
   





  if(choice->size2) {
      if(first){
	density1 = (int **)G_calloc(25, sizeof(int *));
        for(i=0; i<25; i++)
 	       density1[i] = (int *)G_calloc(25, sizeof(int));
        }
	if(type_coh >= 0 && size_coh >= 0)
	   density1[type_coh][size_coh] ++;
  } 




  if(first) first = 0;

  if(!tmp->next){
        save_size(sum2, msize, msize1, sum22, density, type_dens, density1); 
        msize = sum2 = 0;
        free(density);
        free(msize1);
	free(sum22);
	if(density1){
           for(i=0; i<25; i++)
             free(density1[i]);
	   free(density1);  
	}
	first = 1;      
  }
}




/** save the size measures **/
void  save_size(sum2, msize, msize1, sum22, density, type_dens, density1)
int     *density, *type_dens, **density1;
double  msize, *msize1, sum2, *sum22;
{
  register int	i, j;
  float    	tmp, stdv;
  FILE   	*fp;


/* variables:
     IN:
	sum2 =		sum of patch sizes squared
	msize =		sum of patch sizes
	msize1[] =	array of sums of patch sizes by gp
	sum22[] =	array of patch sizes squared by gp
	density[] =	array of no. of patches by size class
	type_dens[] = 	array of no. of patches by gp ?
     INTERNAL:
	tmp =		mean patch size (s1 & s3)
	stdv =		st. dev. patch size (s2 & s4)		
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)

*/


		/* calc. & write s1=mn. size, s2=s.d. size */

  if(choice->size[1] || choice->size[2]) {
     tmp = msize/total_patches;
     stdv = sum2/total_patches - tmp*tmp;
     if(stdv > 0) stdv = sqrt(stdv);
     else  stdv = 0;
     fprintf(s1_2, " %11.2f %11.2f\n", tmp, stdv);
  }

		/* calc. & write s3=mn. size by gp */

  if(choice->size[3]) {
     for(i=0; i<ntype; i++){
         if(tmp = type_dens[i]) 
            tmp = msize1[i] / tmp;
         fprintf(s3, " %11.2f", tmp);
     }
     fprintf(s3, "\n");
  }

		/* calc. & write s4=s.d. size by gp */

  if(choice->size[4]) {
     for(i=0; i<ntype; i++){
         stdv = 0;
         if(type_dens[i]){
            tmp = msize1[i] / type_dens[i];
            stdv = sum22[i]/type_dens[i] - tmp*tmp;
            if(stdv > 0) stdv = sqrt(stdv);
	    else stdv = 0;
         } 
         fprintf(s4, " %11.2f", stdv);
     }
     fprintf(s4, "\n");
  }
    
		/* write s5=no. by size class */

  if(choice->size[5]) {
     for(i=0; i<size_cl[0]-1; i++)
         fprintf(s5, " %5d", *(density+i));
     fprintf(s5, "\n");
  }
  

  if(choice->size2){
    if(!(fp = fopen("r.le.out/s6.out", "a")))
	G_fatal_error("Can't write file s6.out; do you have write permission?\n");

		/* write s6=no. by size class by gp */

    fprintf(fp, "%4d%4d\n", n_scale, n_unit);
    for(i=0; i<ntype; i++){
        for(j=0; j<*size_cl-1; j++)
            fprintf(fp, " %5d", density1[i][j]);
	fprintf(fp, "\n");
    }
    fprintf(fp, "\n\n");
    fclose(fp);
  }


}




/** compute the shape measures **/
void   df_shape(tmp, type_coh, type_dens)
PATCH *tmp;
int   type_coh, *type_dens;
{
  static   int  *den1, *den2, *den3, 
		**density1=NULL, **density2=NULL, **density3, new=1;    
  static double mshape=0, *mshape1=NULL,
		mshape_p=0, *mshape1_p, *sqr11, *sqr21, *sqr31,
		mshape_r=0, *mshape1_r, sq1=0, sq2=0, sq3=0; 	
  register int  i;
  int		shape_coh1=0, shape_coh2=0, shape_coh3=0;
  double    	shp1, shp2, shp3;


/* variables:
     INTERNAL:
	shp1 =		CPA shape index
	shp2 =		PA shape index
	shp3 =		RCC shape index
	mshape =	sum of CPA shape indices for all patches
	mshape_p =	sum of PA shape indices for all patches
	mshape_r =	sum of RCC shape indices for all patches
	mshape1[] =	array of CPA indices by gp
	mshape1_p[] =	array of PA indices by gp
	mshape1_r[] =	array of RCC indices by gp
	sq1 =		sum of CPA indices squared
	sq2 =		sum of PA indices squared
	sq3 =		sum of RCC indices squared
	sqr11[] =	array of CPA indices squared by gp	
	sqr21[] =	array of PA indices squared by gp
	sqr31[] =	array of RCC indices squared by gp	
	den1[] =	array of CPA indices by index class
	den2[] =	array of PA indices by index class
	den3[] =	array of RCC indices by index class
	density1[] = 	array of CPA indices by index class by gp
	density2[] =	array of PA indices by index class by gp
	density3[] =	array of RCC indices by index class by gp
*/
 
         
  shp1 = 0.282 * tmp->perim / sqrt(tmp->area);		  /* CPA method m2 */
  shp2 = tmp->perim / tmp->area;			  /* PA method m1 */
  shp3 = 2.0 * sqrt(tmp->area / M_PI) / tmp->long_axis;	  /* RCC method m3 */
     
  if(new) { /** setup the temporary parameters **/
      mshape1 = (double *)G_calloc(25, sizeof(double));
      mshape1_p = (double *)G_calloc(25, sizeof(double));
      mshape1_r = (double *)G_calloc(25, sizeof(double));
      sqr11 = (double *)G_calloc(25, sizeof(double));
      sqr21 = (double *)G_calloc(25, sizeof(double));
      sqr31 = (double *)G_calloc(25, sizeof(double));
      den1 = (int *)G_calloc(25, sizeof(int));
      den2 = (int *)G_calloc(25, sizeof(int));
      den3 = (int *)G_calloc(25, sizeof(int));
  } 


  mshape += shp1;
  mshape_p += shp2;
  mshape_r += shp3;
  sq1 += shp1 * shp1;
  sq2 += shp2 * shp2;
  sq3 += shp3 * shp3;

  if(type_coh >= 0){
     mshape1[type_coh] += shp1;
     mshape1_p[type_coh] += shp2;
     mshape1_r[type_coh] += shp3;
     sqr11[type_coh] += shp1*shp1;
     sqr21[type_coh] += shp2*shp2;
     sqr31[type_coh] += shp3*shp3;
  }



					/* if sh2=h5 or h6 */

  if(choice->shape[5] || choice->shape[6]) {
					/* if sh1=m1 */
     if(choice->Mx[1]) {
        if(0 <= (shape_coh2 = index_coh(shp2, shape_PA)))
	   den2[shape_coh2] ++;
     }

					/* if sh1=m2 */
     if(choice->Mx[2]) {
        if(0 <= (shape_coh1 = index_coh(shp1, shape_CPA)))
	   den1[shape_coh1] ++;
     }

					/* if sh1=m3 */
     if(choice->Mx[3]) {
        if(0 <= (shape_coh3 = index_coh(shp3, shape_RCC)))
	   den3[shape_coh3] ++;
     }
  }

					/* if sh2=h6 */

  if(choice->shape2){
     if(new) {	   
        density1 = (int **)G_calloc(25, sizeof(int *));
        density2 = (int **)G_calloc(25, sizeof(int *));
        density3 = (int **)G_calloc(25, sizeof(int *)); 
        
        for(i=0; i<25; i++)
           if(!(density1[i] = (int *)calloc(25, sizeof(int))) ||
              !(density2[i] = (int *)calloc(25, sizeof(int))) ||
	      !(density3[i] = (int *)calloc(25, sizeof(int))) ) 
	         G_fatal_error("Failure to allocate memory for sh2, exit.\n");
     }

     if(type_coh >= 0){
	   if(shape_coh1 >= 0)
	      density1[type_coh][shape_coh1] ++;
	   if(shape_coh2 >= 0)
	      density2[type_coh][shape_coh2] ++;
	   if(shape_coh3 >= 0)
	      density3[type_coh][shape_coh3] ++;
     }
  }

  if(new) new = 0;
  if(!tmp->next){  /** if all the patches are done **/
        save_shape(sq1, sq2, sq3, sqr11, sqr21, sqr31, 
                   mshape, mshape_p, mshape_r, 
		   mshape1, mshape1_p, mshape1_r, type_dens, 
		   den1, den2, den3,
		   density1, density2, density3); 
        mshape = sq1 = sq2 =sq3 = mshape_p = mshape_r = 0;
       
        free(mshape1);
        free(mshape1_p);
        free(mshape1_r);
        free(sqr11);
        free(sqr21);
	free(sqr31);
	free(den1);
	free(den2);
	free(den3);
	if(density1){
           for(i=0; i<25; i++){
              free(density1[i]);
	      free(density2[i]);  
              free(density3[i]);
	   }
	   free(density1);  
           free(density2);
	   free(density3);  
	}
	new = 1;      
  }
}

/** save the shape measures **/
void  save_shape(sq1, sq2, sq3, sqr11, sqr21, sqr31, 
                 mshape, mshape_p, mshape_r, 
		 mshape1, mshape1_p, mshape1_r, type_dens,
		 den1, den2, den3, 
		 density1, density2, density3)
int     *type_dens, *den1, *den2, *den3, 
	**density1, **density2, **density3;
double  sq1, sq2, sq3, *sqr11, *sqr21, *sqr31, mshape, *mshape1, 
        mshape_p, mshape_r, *mshape1_p, *mshape1_r;
{
  register int	i, j;
  float    	tmp, stdv;
  FILE   	*fp;

/* variables:
     IN:
	sq1 =		sum of CPA indices squared
	sq2 =		sum of PA indices squared
	sq3 =		sum of RCC indices squared
	sqr11[] =	array of CPA indices squared by gp	
	sqr21[] =	array of PA indices squared by gp
	sqr31[] =	array of RCC indices squared by gp	
	mshape =	sum of CPA shape indices for all patches
	mshape_p =	sum of PA shape indices for all patches
	mshape_r =	sum of RCC shape indices for all patches
	mshape1[] =	array of CPA indices by gp
	mshape1_p[] =	array of PA indices by gp
	mshape1_r[] =	array of RCC indices by gp
	type_dens[] = 	array of no. of patches by gp ?
	den1[] =	array of CPA indices by index class
	den2[] =	array of PA indices by index class
	den3[] =	array of RCC indices by index class
	density1[] = 	array of CPA indices by index class by gp
	density2[] =	array of PA indices by index class by gp
	density3[] =	array of RCC indices by index class by gp
     INTERNAL:
	tmp =		mean shape index (h1)		
	stdv =		st. dev. shape index (h2)		
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)	
*/


/*CALC. & WRITE h1-h5 FOR CPA INDEX (m2) */

		/* calc. & write h1=mn. shape, h2=s.d. shape for CPA index (m2) */ 

  if((choice->shape[1] || choice->shape[1]) && choice->Mx[2]) {
       tmp = mshape / total_patches;
       stdv = sq1/total_patches - tmp*tmp;
       if(stdv > 0) stdv = sqrt(stdv);
       else stdv = 0;
       fprintf(h1_2, " %12.3f %12.3f\n", tmp, stdv);
  }


		/* calc. & write h3=mn. shape by gp for CPA index (m2) */

  if(choice->shape[3] && choice->Mx[2]) {
       for(i=0; i<ntype; i++){
          if(tmp = type_dens[i]) 
             tmp = mshape1[i] / tmp;
          fprintf(h3, " %12.3f", tmp);
       }
       fprintf(h3, "\n");
  }


		/* calc. & write h4=s.d. shape by gp for CPA index (m2) */

  if(choice->shape[4] && choice->Mx[2]) {
       for(i=0; i<ntype; i++){
           stdv = 0;
           if(type_dens[i] > 1){
              tmp = mshape1[i] / type_dens[i];
              stdv = sqr11[i]/type_dens[i] - tmp*tmp;
              if(stdv > 0) stdv = sqrt(stdv);
	      else stdv = 0;
           } 
           fprintf(h4, " %12.3f", stdv);
       }
       fprintf(h4, "\n");
  }


		/* write h5=no. by shape index class for CPA index (m2) */

  if(choice->shape[5] && choice->Mx[2]) {
       for(j=0; j<*shape_CPA-1; j++)
             fprintf(h5, " %5d", den1[j]);
       fprintf(h5, "\n");
  }



  
/*CALC. & WRITE h1-h5 FOR PA INDEX (m1) */

		/* calc. & write h1=mn. shape, h2=s.d. shape for PA index (m1) */ 

  if((choice->shape[1] || choice->shape[2]) && choice->Mx[1]) {
       tmp = mshape_p / total_patches;
       stdv = sq2/total_patches - tmp*tmp;
       if(stdv > 0) stdv = sqrt(stdv);
       else stdv = 0;
       fprintf(h1_2, " %12.3f %12.3f\n", tmp, stdv);
  }


		/* calc. & write h3=mn. shape by gp for PA index (m1) */

  if(choice->shape[3] && choice->Mx[1]) {
      for(i=0; i<ntype; i++){
         if(tmp = type_dens[i]) 
            tmp = mshape1_p[i] / tmp;
         fprintf(h3, " %12.3f", tmp);
     }
     fprintf(h3, "\n");
  }


		/* calc. & write h4=s.d. shape by gp for PA index (m1) */

  if(choice->shape[4] && choice->Mx[1]) {
     for(i=0; i<ntype; i++){
         stdv = 0;
         if(type_dens[i] > 1){
             tmp = mshape1_p[i] / type_dens[i];
             stdv = sqr21[i]/type_dens[i] - tmp*tmp;
             if(stdv > 0) stdv = sqrt(stdv);
	     else stdv = 0;
         } 
         fprintf(h4, " %12.3f", stdv);
     }
     fprintf(h4, "\n");
  }


		/* write h5=no. by shape index class for PA index (m1) */

  if(choice->shape[5] && choice->Mx[1]) {
      for(j=0; j<*shape_PA-1; j++)
          fprintf(h5, " %5d", den2[j]);
      fprintf(h5, "\n");
  }


  

/*CALC. & WRITE h1-h5 FOR RCC INDEX (m3) */

		/*    calc. & write h1=mn. shape, h2=s.d. shape for RCC index (m3) */ 
     
  if((choice->shape[1] || choice->shape[2]) && choice->Mx[3]) {
     tmp = mshape_r / total_patches;
     stdv = sq3/total_patches - tmp*tmp;
     if(stdv > 0) stdv = sqrt(stdv);
     else stdv = 0;
     fprintf(h1_2, " %6.3f %6.3f\n", tmp, stdv);
  }



		/*    calc. & write h3=mn. shape by gp for RCC index (m3) */

  if(choice->shape[3] && choice->Mx[3]) {
     for(i=0; i<ntype; i++){
         if(tmp = type_dens[i]) 
            tmp = mshape1_r[i] / tmp;
         fprintf(h3, " %6.3f", tmp);
     }
     fprintf(h3, "\n");
  }



		/*    calc. & write h4=s.d. shape by gp for RCC index (m3) */

  if(choice->shape[4] && choice->Mx[3]) {
     for(i=0; i<ntype; i++){
         stdv = 0;
         if(type_dens[i] > 1){
             tmp = mshape1_r[i] / type_dens[i];
             stdv  = sqr31[i]/type_dens[i] - tmp*tmp;   
             if(stdv > 0) stdv = sqrt(stdv);  
	     else stdv = 0;
         }
         fprintf(h4, " %6.3f", stdv);
     }
     fprintf(h4, "\n");
  }



		/*    write h5=no. by shape index class for RCC index (m3) */

  if(choice->shape[5] && choice->Mx[3]) {
     for(j=0; j<*shape_RCC-1; j++)
           fprintf(h5, " %5d", den3[j]);
     fprintf(h5, "\n");
  }
  



/* CALC. & WRITE h6 = NO. IN EA. SHAPE INDEX CLASS BY GP */

   if(choice->shape[6]) {

        if(density1){
          if(!(fp = fopen("r.le.out/h6.out", "a")))
	      G_fatal_error("Can't write file h6.out; do you have write permission?\n");

          fprintf(fp, "%4d%4d\n", n_scale, n_unit);

          if (choice->Mx[2]) {
               for(i=0; i<ntype; i++){
                   for(j=0; j<*shape_CPA-1; j++)
                       fprintf(fp, " %5d", density1[i][j]);
	           fprintf(fp, "\n");
               }
               fprintf(fp, "\n");
          }

          if (choice->Mx[1]) {
              for(i=0; i<ntype; i++){
                  for(j=0; j<*shape_PA-1; j++)
                      fprintf(fp, " %5d", density2[i][j]);
	          fprintf(fp, "\n");
              }
              fprintf(fp, "\n");
          }

          if (choice->Mx[3]) {
              for(i=0; i<ntype; i++){
                  for(j=0; j<*shape_RCC-1; j++)
                     fprintf(fp, " %5d", density3[i][j]);
                  fprintf(fp, "\n");
              }
              fprintf(fp, "\n");
          }

          fclose(fp);
        }
   }

}


/** compute the fractal dimension **/
void  df_fract(tmp)
PATCH *tmp;
{
  static float  *p, *area, sum = 0, sum2 = 0;
  float       	a, b, siga, sigb, chi2, R2, mean;
  static int    cnt=0, first = 1;
  FILE		*fp;
  
  if(first){
     p = (float *)calloc(total_patches, sizeof(float));
     area = (float *)calloc(total_patches, sizeof(float));
     first = 0;
  }

  if (tmp->perim) {
     p[cnt] = log(tmp->perim);
     area[cnt] = log(tmp->area);
     sum += p[cnt];  
     sum2 += p[cnt]*p[cnt];
     cnt ++;
  }

  if(!tmp->next){
     fp = fopen0("r.le.out/f1.out", "a");
     if (cnt > 5) {
        fit(area, p, cnt, &a, &b, &siga, &sigb, &chi2);
        if(cnt > 1)     
           R2 = 1 - chi2 / (sum2 -  sum*sum/cnt);
        else R2 = 0;
     }
     else {
	b = 0;
	R2 = 0;
	sigb = 0;
     }
     fprintf(fp, "%4d%4d %6.3f %6.3f %6.3f\n", 
                  n_scale, n_unit, 2.0*b, R2, sigb);
     fclose(fp);
     free(area);
     free(p);
     cnt = sum = sum2 = 0;
     first = 1;
  }
}


/** compute the linear regression of log perim vs log area **/
void  fit(x, y, ndata, a, b, siga, sigb, chi2)
float *x, *y, *a, *b, *siga, *sigb, *chi2;
int   ndata;
{
  register int i;
  float 	wt, t, sxoss, sx=0, sy=0, st2=0, ss, sigdat = 0;

  *b = 0;
  for(i=0; i<ndata; i++) {
	sx += x[i];
	sy += y[i];
  }
  ss = ndata;
  sxoss = sx/ss;
  for (i=0; i<ndata; i++) {
     t = x[i] - sxoss;
     st2 += t*t;
     *b += t*y[i];
  }

  if(st2 > 0)  *b /= st2;
  else *b = 0;
  *a = (sy-sx*(*b)) / ss;
  *siga = sqrt((1.0 + sx*sx/(ss*st2))/ss);
  if(st2 > 0)  *sigb = sqrt(1.0/st2);
  else *sigb = 0;

  *chi2 = 0;
  for(i=0; i<ndata; i++) 	
	*chi2 += (y[i] - *a - *b*x[i])*(y[i] - *a - *b*x[i]);
  if(ndata > 2)
     sigdat = sqrt((*chi2)/(ndata-2));
  *siga *= sigdat;
  *sigb *= sigdat;
}



/** compute & save the perimeter measures **/
void   df_perim(tmp, type_coh, type_dens)
PATCH *tmp;
int   type_coh, *type_dens;
{
  static   float  perim=0, *perim1, sum2=0, *sum21, first=1;   
  register int  i;
  double        mean, stdv;
  
  if(first){
     perim1 = (float *)G_calloc(25, sizeof(float)); 
     sum21 = (float *)G_calloc(25, sizeof(float)); 
     first = 0;
  }


/* variables:
     IN: 
	tmp =	   
 	type_coh =	identif. no. for ea. gp.	
	type_dens[] = 	array of no. of patches by gp ?
     INTERNAL:
	perim =		sum of perimeters (p1)
	sum2 = 		sum of perimeters squared
	perim1[] = 	array of sum of perims by gp (pp4)
	sum21[] =	array of sum of perims squared by gp
	mean = 		mean perim. (p2)
	stdv =		st. dev. perim. (p3)
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)
*/
	

  perim += tmp->perim;
  sum2 += tmp->perim*tmp->perim;
  if(type_coh >= 0) { 
     perim1[type_coh] += tmp->perim;
     sum21[type_coh] += tmp->perim*tmp->perim;
  }
	
  if( !tmp->next) {  /** save the perimeter measures **/

     mean = perim/total_patches;
     stdv = sum2/total_patches - mean*mean;
     if(stdv > 0) stdv = sqrt(stdv);
     else  stdv = 0;


		/* write p1=sum per., p2=mn. per.,p3=s.d. per. */

  if(choice->perim[1] || choice->perim[2] || choice->perim[3])
     fprintf(p1_3, " %11.2f %11.2f %11.2f\n", perim, mean, stdv);


		/* write p4=sum per. by gp */

  if(choice->perim[4]) {
     for(i=0; i<ntype; i++)
	fprintf(p4, " %11.2f", perim1[i]);
     fputs("\n", p4);
  }


		/* write p5=mn. per. by gp */

  if(choice->perim[5]) {
     for(i=0; i<ntype; i++){ 
	if(type_dens[i])
           mean = perim1[i]/type_dens[i];
        else  mean = 0;
	fprintf(p5, " %11.2f", mean);
     }
     fputs("\n", p5);
  }


		/* calc. & write p6=s.d. per. by gp */

  if(choice->perim[6]) {
     for(i=0; i<ntype; i++){
	stdv = 0;
	if(type_dens[i]){
	   mean = perim1[i]/type_dens[i];
	   stdv = sum21[i]/type_dens[i] - mean*mean;
	   if(stdv > 0) stdv = sqrt(stdv);
  	   else stdv = 0;
        } 
	fprintf(p6, " %11.2f", stdv);
     }
     fputs("\n", p6);
  }

  free(perim1);  
  free(sum21);  
  first = 1;
  perim = sum2 = 0;

  }     
}





/** run the moving window patch measures **/
void mv_patch (patch_list, value, index)
PATCH 	*patch_list;
int 	**value;
int	index;
{
  PATCH     	 *tmp = patch_list, *tmp0;
 
  if(!total_patches) return;
  while(tmp){  
     if(choice->att[0])		
	m_att(tmp, value, index);
     if(choice->size[0])
	m_size(tmp, value, index);
     if(choice->core[0])
	m_core(tmp, value, index);
     if(choice->shape[0] && choice->Mx[1]) 		
	m_shape(tmp, 1, value, index);
     if(choice->shape[0] && choice->Mx[2]) 	
	m_shape(tmp, 2, value, index);
     if(choice->shape[0] && choice->Mx[3]) 
	m_shape(tmp, 3, value, index);
     if(choice->fract)			
	m_fractal(tmp, value, index);
     if(choice->perim[0])		
     	m_perim(tmp, value, index);

     tmp0 = tmp;
     tmp = tmp->next;
     free(tmp0);
  }

  total_patches = 0;
}




/** check if att is in the "group" **/
int  in_group(att, group)
int   att;
float *group;
{
  register int i;

  for(i=1; i<*group; i++)
    if(-999 == *(group+i)){
	if( *(group+i) && *(group+i-1) <= att && att <= *(group+i+1))
 	     return 1;
	else i++;
    } else if(*(group+i) == att)
	return 1;

  return 0;
}


/**  determine which index class att belongs to **/
int  index_coh(att, group)
double   att;
float    *group;
{
  register int i;

  for(i=*group-1; i>=1; i--) {
    if(*(group+i) <= att)
 	     return i-1;
  }
  return -999;
}


/**  determine which group att belongs to **/
int  recl_coh(att)
int    att;
{
  register int i;
  extern int   ntype;
  extern float **recl_tb;

  for(i=0; i < ntype; i++)
    if(in_group(att, recl_tb[i]))
        return i;

  return -999;
}



/** moving-window attribute measures **/
void 	m_att(tmp, value, index)
PATCH *tmp;
int   **value, index;

{
  static   int  sum1 = 0, sum12 = 0, sum2 = 0, sum22 = 0, sum32 = 0,
		total1 = 0, total2 = 0, density = 0;
  double        mean, stdv;

/* choice->att 	1=mean pixel attrib. (a1)
	      	2=st. dev. pixel attrib. (a2) 
		3=mean patch attrib. (a3)
		4=st. dev. patch attrib. (a4)
	      	5=cover in gp 1 (a5)
		6=density in gp 1 (a6)
		7=total density (a7)         

  variables:
     INTERNAL:
	sum1 =		sum of patch area x patch attrib.
	sum2 = 		sum of patch attributes
	total =		sum of patch areas
	sum2 =		sum of patch area x patch attrib. squared
			or sum of patch attrib. squared
			or sum of patch areas	
	density =	no. of patches in gp 1
	value =		output value for selected att measure
     GLOBAL:
	total_patches = tot. no. patches in sampling area (fm. trace.c)	
*/

 
  if (choice->att[1] || choice->att[2]){
     sum1 += tmp->area * tmp->att;
     total1 += tmp->area;
     if(choice->att[2])
        sum12 += tmp->area * tmp->att * tmp->att;
  }
 
  if (choice->att[3] || choice->att[4]){
     sum2 += tmp->att;
     if(choice->att[4])
        sum22 += tmp->att * tmp->att;
  }
 
  if (choice->att[5]) {
     total2 += tmp->area;
     if(in_group(tmp->att, recl_tb[0]))
	sum32 += tmp->area; 
  }

  if (choice->att[6]) {
     if(in_group(tmp->att, recl_tb[0]))
        density++;
  }



  if(!tmp->next){

		/* calc. a1=mn. pixel attrib. */
				
     if (choice->att[1] && total1) {
        value[index][0] = 10.0 * sum1/total1;
     }

		/* calc. a2=s.d. pixel attrib. */
			
     if (choice->att[2] && total1){
	mean = (double)sum1/total1;
        stdv = (double)sum12/total1 - mean*mean;
	if(stdv > 0) 
           value[index][1] = 10.0 * sqrt(stdv);		
     } 

		/* calc. a3=mn. patch attrib. */

     if (choice->att[3] && total_patches) {
	value[index][2] = 10.0 * sum2/total_patches;
     }

		/* calc. a4=s.d. patch attrib. */

     if (choice->att[4] && total_patches){
	mean = (double)sum2/total_patches;
        stdv = (double)sum22/total_patches - mean*mean;
	if(stdv > 0) 
	   value[index][3] = 10.0 * sqrt(stdv);	
     }

		/* calc. a5=cover in gp 1 */

     if (choice->att[5] && total2) {
	value[index][4] = 1000.0 * sum32/total2;
     }

		/* calc. a6=density in gp 1 */

     if (choice->att[6]) {
	value[index][5] = density;
     }

		/* calc. a7=total density */

     if (choice->att[7])
	value[index][6] = total_patches;

     total1 = total2 = density = sum1 = sum12 = 0;
     sum2 = sum22 = sum32 = 0;      
  }

}



/** moving-window size measures **/
void    m_size(tmp, value, index)
PATCH *tmp;
int   **value, index;

{
  static   int  sum1 = 0, sum12 = 0, sum2 = 0, sum22 = 0,
		density1 = 0, density2 = 0, density3 = 0;
  double        mean, stdv;


/* choice->size == 1 - mean patch size (s1)
		   2 - st. dev. patch size (s2)
		   3 - mean patch size by gp 1 (s3)
		   4 - st. dev. patch size by gp 1 (s4)
		   5 - no. by size class 1 (s5)
		   6 - no. by size class 1 by gp 1 (s6)
   variables:
     INTERNAL:
	sum =		sum of patch sizes
			or sum of patch sizes in gp 1
	sum2 =		sum of patch sizes squared
			or sum of patch sizes in gp 1
	density =	no. of patches in gp 1
*/
 

 
  if (choice->size[1] || choice->size[2]){   
     sum1 += tmp->area;     
     if(choice->size[2])
          sum12 += tmp->area * tmp->area;
  }
 
  if (choice->size[3] || choice->size[4]){
     if(in_group(tmp->att, recl_tb[0])) {
        density1 ++;
        sum2 += tmp->area;      
        if(choice->size[4])
           sum22 += tmp->area * tmp->area;
     }
  } 


  if (choice->size[5] && tmp->area < size_cl[2]) {
     density2 ++;
  }


  if (choice->size[6] && tmp->area < size_cl[2] && 
     in_group(tmp->att, recl_tb[0])) {
     density3 ++;	  
  }




  if (!tmp->next){

		/* calc. s1=mn. patch size */

     if (choice->size[1] && total_patches) {
        value[index][7] = 10.0 * sum1/total_patches;
     }


		/* calc. s2=s.d. patch size */

     if (choice->size[2] && total_patches) {
	mean = (double)sum1/total_patches;
        stdv = (double)sum12/total_patches - mean*mean;
	if(stdv > 0) 
           value[index][8] = 10.0 * sqrt(stdv);	
     } 


		/* calc. s3=mn. patch size by gp 1 */

     if (choice->size[3] && density1) {
	value[index][9] = 10.0 * sum2/density1;
     }


		/* calc. s4=s.d. patch size by gp 1 */

     if (choice->size[4] && density1 > 1){
	mean = (double)sum2/density1;
        stdv = (double)sum22/density1 - mean*mean;
	if(stdv > 0)  
	   value[index][10] = 10.0 * sqrt(stdv);	  	
     } 


		/* calc. s5=no. by size class 1 */

     if (choice->size[5]) {
	value[index][11] = density2;  
     }

		/* calc. s6=no. by size class 1 by gp 1*/

     if (choice->size[6]) {
	value[index][12] = density3;  
     }

     density1 = density2 = density3 = 0;
     sum1 = sum12 = sum2 = sum22 = 0; 
  }
}




/** moving-window core measures **/
void    m_core(tmp, value, index)
PATCH *tmp;
int   **value, index;

{
  static   int  sum1c = 0, sum1e = 0, sum2c = 0, sum2e = 0,
		sum12c = 0, sum12e = 0, sum22c = 0, sum22e = 0,
		density1c = 0, density1e = 0, 
		density2c = 0, density2e = 0,
		density3c = 0, density3e = 0;
  double        meanc, stdvc, meane, stdve;


/* choice->core == 1 - mean core size (c1)
		   2 - st. dev. core size (c2)
		   3 - mean edge size (c3)
		   4 - st. dev. edge size (c4)
		   5 - mean core size by gp (c5)
		   6 - st. dev. core size by gp (c6)
		   7 - mean edge size by gp (c7)
		   8 - st. dev. edge size by gp (c8)
		   9 - no. by size class (c9)
		  10 - no. by size class by gp (c10)
   variables:
     INTERNAL:
	sumc =		sum of core sizes
			or sum of core sizes in gp 1
	sum2c =		sum of core sizes squared
			or sum of core sizes in gp 1
	sume =		sum of edge sizes
			or sum of edge sizes in gp 1
	sum2e =		sum of edge sizes squared
			or sum of edge sizes in gp 1
	densityc =	no. of cores in gp 1
	densitye =	no. of edges in gp 1
*/
 

 
  if (choice->core[1] || choice->core[2]){   
     sum1c += tmp->core;  
     if(choice->core[2])
          sum12c += tmp->core * tmp->core;
  }
 
  if (choice->core[3] || choice->core[4]){   
     sum1e += tmp->edge;  
     if(choice->core[4])
          sum12e += tmp->edge * tmp->edge;
  }
 
  if (choice->core[5] || choice->core[6] ||
      choice->core[7] || choice->core[8])
     if(in_group(tmp->att, recl_tb[0])) {
	if (choice->core[5] || choice->core[6]) {
	   density1c ++;
           sum2c += tmp->core;      
           if(choice->core[6])
              sum22c += tmp->core * tmp->core;
        }
	if (choice->core[7] || choice->core[8]) {
	   density1e ++;
           sum2e += tmp->edge;      
           if(choice->core[8])
              sum22e += tmp->edge * tmp->edge;
        }
     }

  if (choice->core[9]) {
     if (tmp->core < size_cl[2])
          density2c ++;
     if (tmp->edge < size_cl[2])
	  density2e ++;
  }

  if (choice->core[10]) {
     if (tmp->core < size_cl[2] && in_group(tmp->att, recl_tb[0]))
          density3c ++;
     if (tmp->edge < size_cl[2] && in_group(tmp->att, recl_tb[0]))
          density3e ++;	  
  }




  if (!tmp->next){

				/* calc. c1=mn. core size */

     if (choice->core[1] && total_patches) {
        value[index][13] = 10.0 * sum1c/total_patches;
     }

				/* calc. c2=s.d. core size */

     if (choice->core[2] && total_patches){
	meanc = (double)sum1c/total_patches;
        stdvc = (double)sum12c/total_patches - meanc*meanc;
	if(stdvc > 0) 
	   value[index][14] = 10.0 * sqrt(stdvc);	
     } 

				/* calc. c3=mn. edge size */

     if (choice->core[3] && total_patches) {
        value[index][15] = 10.0 * sum1e/total_patches;
     }

				/* calc. c4=s.d. edge size */

     if (choice->core[4] && total_patches){
	meane = (double)sum1e/total_patches;
        stdve = (double)sum12e/total_patches - meane*meane;
	if(stdve > 0) 
	   value[index][16] = 10.0 * sqrt(stdve);	
     } 

				/* calc. c5=mn. core size by gp 1 */

     if (choice->core[5] && density1c) {
	value[index][17] = 10.0 * sum2c/density1c;
     }


				/* calc. c6=s.d. core size by gp 1 */

     if (choice->core[6] && density1c > 1){
	meanc = (double)sum2c/density1c;
        stdvc = (double)sum22c/density1c - meanc*meanc;
	if(stdvc > 0)  
	   value[index][18] = 10.0 * sqrt(stdvc);	  	
     } 


				/* calc. c7=mn. edge size by gp 1 */

     if (choice->core[7] && density1e) {
	value[index][19] = 10.0 * sum2e/density1e;
     }

				/* calc. c8=s.d. edge size by gp 1 */

     if (choice->core[8] && density1e > 1){
	meane = (double)sum2e/density1e;
        stdve = (double)sum22e/density1e - meane*meane;
	if(stdve > 0)  
	   value[index][20] = 10.0 * sqrt(stdve);	  	
     } 

				/* calc. c9=no. by size class 1 */

     if (choice->core[9]) {
	value[index][21] = density2c;  
     }

		       		/* calc. c10=no. by size class 1 by gp 1 */

     if (choice->core[10]) {
	value[index][22] = density3c;  
     }

     density1c = density1e = 0;
     density2c = density2e = 0;
     density3c = density3e = 0;
     sum1c = sum1e = sum2c = sum2e = 0;
     sum12c = sum12e = sum22c = sum22e = 0;
  }
}





/** moving-window shape measures **/
void	m_shape(tmp, way, value, index)
PATCH *tmp;
int   way, **value, index;
{
  static double  sum1 = 0, sum12 = 0, sum2 = 0, sum22 = 0;
  static int     density1 = 0, density2 = 0, density3 = 0;
  double         mean, shp, stdv;

/* choice->shape	1 = mean patch shape (h1)
			2 = st.dev. patch shape (h2)
			3 = mean patch shape by gp 1 (h3)
			4 = st.dev. shape by gp 1 (h4)
			5 = number by shape class 1 (h5)
			6 = number by shape class 1 by gp 1 (h6)
   variables:
     IN:
	way =		shape index choice (see shp)
     INTERNAL:
	shp = 		P/A (way=1)
			CPA (way=2)
			RCC (way=3)
	sum =		sum of shape indices or
			sum of shape indices in gp 1
	sum2 =		sum of shape indices squared
			sum of shape indices squared in gp 1			
*/


 
  if(way == 1) { 	
      shp = tmp->perim/tmp->area;
  }
  else if (way == 2) {	
      shp = (0.282 * tmp->perim)/sqrt(tmp->area);
  }
  else {	
      shp = 2 * sqrt(tmp->area/M_PI)/tmp->long_axis;
  }
    
  if (choice->shape[1] || choice->shape[2]) { 
     sum1 += shp;      
     if(choice->shape[2])
        sum12 += shp * shp;
  }
 
  if (choice->shape[3] || choice->shape[4]) {
     if (in_group(tmp->att, recl_tb[0])) {
        density1 ++;
        sum2 += shp;      
        if(choice->shape[4])
           sum22 += shp * shp;
     }
  }
 

  if (choice->shape[5] || choice->shape[6])
     if ((way == 1 && (shp < shape_PA[2] && shp >= shape_PA[1])) ||
	 (way == 2 && (shp < shape_CPA[2] && shp >= shape_CPA[1])) ||
	 (way == 3 && (shp < shape_RCC[2] && shp >= shape_RCC[1]))) {
	  if (choice->shape[5])
             density2 ++;
          else if (in_group(tmp->att, recl_tb[0]))
             density3 ++;	  
     }

  if (!tmp->next){

		/* calc. h1=mn. patch shape */
	
     if (choice->shape[1] && total_patches)				
        value[index][23] = 1000.0 * sum1/total_patches;

		/* calc. h2=s.d. patch shape */

     if (choice->shape[2] && total_patches > 1) {
	mean = sum1/total_patches;
	stdv = sum12/total_patches - mean*mean;
	if(stdv > 0)  
	   value[index][24] = 1000.0 * sqrt(stdv);
     } 

		/* calc. h3=mn. patch shape in gp 1 */

     if (choice->shape[3] && density1)
	value[index][25] = 1000.0 * sum2/density1;

		/* calc. h4=s.d. patch shape in gp 1 */

     if (choice->shape[4] && density1 > 1) {
	mean = sum2/density1;
	stdv = sum22/density1 - mean*mean;	
	if(stdv > 0)  
	   value[index][26] = 1000.0* sqrt(stdv);
     }

		/* calc. h5=no. in shape index class 1 */

     if (choice->shape[5])
	value[index][27] = density2;  

		/* calc. h6=no. in shape index class 1 & gp 1 */

     if (choice->shape[6])
	value[index][28] = density3;  

     density1 = density2 = density3 = sum1 = sum12 = 0;
     sum2 = sum22 = 0;      
  }
}



/** compute the fractal dimension for moving-window analysis **/
void  m_fractal(tmp, value,index)
PATCH *tmp;
int   **value, index;
{
  static float  *p, *area;
  float       	a, b, siga, sigb, chi2;
  static int    cnt=0, first=1;
  FILE		*fp;
  
  if(first){
     p = (float *)G_calloc(total_patches, sizeof(float));
     area = (float *)G_calloc(total_patches, sizeof(float));
     first = 0;
  }

  if (tmp->perim) {
     p[cnt] = log(tmp->perim);
     area[cnt] = log(tmp->area);
     cnt ++;
  }

  if(!tmp->next){
     if (cnt > 5) {
        fit(area, p, cnt, &a, &b, &siga, &sigb, &chi2);
        value[index][29] = 200 * b;
     }
     else value[index][29] = 0;
     free(area);  
     free(p);
     cnt = 0;
     first = 1;
  }
}



/** moving-window perimeter measures **/
void   	m_perim(tmp, value, index)
PATCH *tmp;
int   **value, index;
{
  static   int  sum1 = 0, density = 0, sum12 = 0, sum2 = 0, sum22 = 0;
  double        mean, stdv;

/* choice->perim == 1 - sum of perimeters (p1)
		    2 - mean perimeter (p2)
		    3 - st. dev. perimeters (p3)
		    4 - sum of perimeters in gp 1 (p4)
		    5 - mean perimeter in gp 1 (p5)
		    6 - st. dev. perimeters in gp 1 (p6)
   variables:
     INTERNAL:
	sum =		sum of patch perimeters
			or sum of patch perimeters in gp 1
	sum2 =		sum of patch perimeters squared
			or sum of patch perimeters in gp 1
	density =	no. of patches in gp 1
*/


  
  if (choice->perim[1] || choice->perim[2] || choice->perim[3]) {
        sum1 += tmp->perim;    
        if(choice->perim[3])
	    sum12 += tmp->perim * tmp->perim;  
  } 

  if (choice->perim[4] || choice->perim[5] || choice->perim[6]) {
     if(in_group(tmp->att, recl_tb[0])){  
        sum2 += tmp->perim;      
        if(choice->perim[5] || choice->perim[6]){
	   density ++;
	   if(choice->perim[6])
	      sum22 += tmp->perim * tmp->perim;
	}
     }
  }  

  if(!tmp->next){

		/* calc. p1=sum of perims. */

     if (choice->perim[1])
        value[index][30] = sum1;

		/* calc. p2=mn. perim. */

     if (choice->perim[2] && total_patches)
	   value[index][31] = 10.0 * sum1/total_patches;

		/* calc. p3=s.d. perim. */

     if(choice->perim[3] && total_patches > 1){
	mean = (double)sum1/total_patches;	   
        stdv = (double)sum12/total_patches - mean*mean;
	if(stdv > 0) 
	   value[index][32] = 10.0 * sqrt(stdv);	          	
     } 

		/* calc. p4=sum of perims. in gp 1 */

     if (choice->perim[4])
        value[index][33] = sum2;

		/* calc. p5=mn. perim. in gp 1 */

     if(choice->perim[5] && density)
	value[index][34] =  10.0 * sum2/density;

		/* calc. p6=s.d. perims. in gp 1 */

     if(choice->perim[6] && density > 1){
	mean = (double)sum2/density;
	stdv = (double)sum22/density - mean*mean;
	if(stdv > 0) 
	   value[index][35] = 10.0 * sqrt(stdv);	   	
     } 

     density = sum1 = sum12 = sum2 = sum22 = 0;      
  }
}



double  eu_d(x1, y1, x2, y2)
double  x1, y1, x2, y2;
{
  return( sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) );
}






