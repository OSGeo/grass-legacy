#include "segment.h"
#include "gis.h"
#define         BUG              1
#define         BB               0
#define         ONE              1.570796327
#define         TWO              3.141592654
#define         THREE            4.71238898
#define         FOUR             6.283185307
#define         PI_BY_FOUR       0.785398163
#define         PI_BY_TWO        1.570796327
#define         COLOR_SHIFT      155.0
#define         COLOR_MAX        255.0



struct point {
        double theta;
        double beta;
        int ii;
        int jj;
        struct point *next;
        struct point *back;
        };

         
struct window *los(ptr)
        struct window *ptr;
 
{
        extern double m_dist,ob_elev;
        extern char displayed_map[], out_layer[];
        int  X,p,xmax,ymax,k1,k2,nrows,ncols,srows,scols;
        int nsegments=16,a,b,len,row;
        extern int iv,jv;
        double t1,t2,tan(),atof();
        char  *old_mapset;
        char  *value;
        char *search_mapset, *current_mapset;
        struct Cell_head cellhd;
        extern struct Cell_head window;
        extern char buf[];
        int new,old,in_fd,out_fd;
        struct Colors colors;
	struct Range range;
        double factor,fabs(),decide_color_range(),max_vert_angle=0.0;
        struct point *heads[16],*segment(),*free_list(),*mover;
 
        CELL *cell, data, zv;
        SEGMENT seg_in, seg_out;
        
        
        /* messages     */
        if(iv <0 || jv <0)
        {
        sprintf(buf," Please specify observer location before running analysis");
        message(ptr);
        return(ptr);
        }
        if(m_dist == 0.0)
        {
  sprintf(buf," Please specify maximum distance for los analysis.");
        message(ptr);
        return(ptr);
        }
        if(ob_elev == 0.0)
        {
  sprintf(buf," Please specify observer's elevation above ground.");
        message(ptr);
        return(ptr);
        }
        if(out_layer[0] == '\0')
        {
  sprintf(buf," Please specify the name of the outputmap. ");
        message(ptr);
        return(ptr);
        }
        
         
 
        current_mapset= G_mapset();
 
        search_mapset = "";
 
        old_mapset = G_find_cell (displayed_map, search_mapset);
    if (old_mapset == NULL)
    {
        sprintf (buf, "%s - not found", displayed_map);
        G_fatal_error (buf);
        exit(1);
    }
    if (G_legal_filename(out_layer) < 0)
    {
        sprintf (buf, "%s - illegal name", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
    if (G_find_cell (out_layer, current_mapset))
    {
        sprintf (buf, "%s - already exits. can't overwrite", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
 

if (G_get_cellhd (displayed_map, old_mapset, &cellhd) < 0)
    {
        sprintf (buf, "%s in %s - can't read cell header", displayed_map, old_mapset);
        G_fatal_error (buf);
        exit(1);
    }
 
        nrows = G_window_rows();
        ncols = G_window_cols();
 
        /* printf("\n nrows=%d, ncols=%d\n",nrows,ncols); */
 
        len = sizeof(CELL);
        srows = nrows/5 + 1;
        scols = ncols/5 + 1;
 
         
    cell = G_allocate_cell_buf();
 
    old = G_open_cell_old (displayed_map, old_mapset);
    if (old < 0)
    {
        char buf[200];
        sprintf (buf, "%s in %s - can't open cell file", displayed_map, old_mapset);
        G_fatal_error (buf);
        exit(1);
    }
 
    new = G_open_cell_new (out_layer);
    if (new < 0)
    {
        sprintf (buf, "%s - can't create cell file", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
 
        in_fd = creat("in_seg_file",0666);
        segment_format(in_fd,nrows,ncols,srows,scols,len);
        close(in_fd);
 
        out_fd = creat("out_seg_file",0666);
        segment_format(out_fd,nrows,ncols,srows,scols,len);
        close(out_fd);
 

        in_fd = open("in_seg_file",2);
        segment_init(&seg_in,in_fd,4);
 
        out_fd = open("out_seg_file",2);
        segment_init(&seg_out,out_fd,4);
 
        for(row = 0; row < nrows; row++)
        {
        if (G_get_map_row (old,cell,row) < 0)
        exit(1);
 
        segment_put_row(&seg_in,cell,row);
        }
 
 
 
        /* printf("\n ns_res=%lf   ew_res=%lf  rows=%d  cols=%d\n",window.ns_res,window.ew_res,window.rows,window.cols); */
        /* printf("\n iv=%d   jv=%d \n",iv,jv); */
 
  /* with that we have completed all the initializations, declarations,*/
  /* and setup. The boring part....
     */
 
 
        value = (char *) &zv;
        segment_get(&seg_in,value,iv,jv);
 
        zv += ob_elev;
        for(X=1; X <= nsegments; X++){
 
        k1= 1- (X-1)/8 * 2;     /* sign on y */
if(X>4 && X<13)k2= -1; else k2=1; /* sign on x */
 
if(X==1 || X==4 || X==5 || X==8 || X==9 || X==12 || X==13 || X==16)
 
        { t1= 0.0;      t2= 0.5;}
else    {t1=  0.5;      t2= 1.0;}
 
 
if(X==1 || X==2 || X==7 || X==8 || X==9 || X==10 || X==15 || X==16)
 
        p = 0;
 else   p = 1;
 
 a= ((ncols-1)*(k2+1)/2 - k2 * jv);
 b= (1-k1)/2*(nrows-1) + k1*iv;

                                
        if(p==0){ xmax=a; ymax=b;}
        else    { xmax=b; ymax=a;}
        /* printf("\n xmax=%d, ymax=%d",xmax,ymax); */
 
  heads[X-1]= segment(X,xmax,ymax,t1,t2,p,k1,k2,zv,ncols,&seg_in,&seg_out,iv,jv);
 
}
 
        for(X=1; X <= nsegments; X++){
 
        mover = heads[X-1];
        
        while(mover != NULL)
        {
        if(fabs(mover->beta) > max_vert_angle)
        max_vert_angle = fabs(mover->beta);
        mover = mover->next;
        }
 
                                }
 
        factor = decide_color_range(max_vert_angle*57.3);
 
        for(X=1; X <= nsegments; X++){

        mark_visible_points(heads[X-1],&seg_out,ncols,iv,jv,factor);
        /* free_list(heads[X-1]); */
                }
 
 
         data = 40;
        value = (char *) &data;
        segment_put(&seg_out,value,iv,jv);
 
 
        segment_flush(&seg_out);
 
        for(row=0; row< nrows; row++)
        {
        segment_get_row(&seg_out,cell,row);
         
        if(G_put_map_row(new,cell,row) < 0)
        {
        exit(1);
        }
        }
         
        segment_release(&seg_in);
        segment_release(&seg_out);
        close(in_fd);
        close(out_fd);
        G_close_cell(old);
        G_close_cell(new);
        system("rm in_seg_file");
        system("rm out_seg_file");
 
         /*      creating colours for output map         */
	G_read_range (out_layer, current_mapset, &range);
        G_make_color_wave(&colors,range.pmin, range.pmax);
        G_write_colors (out_layer,current_mapset,&colors);

 
        /* reinitialization of global variables */
        iv = -1;
        jv = -1;
        m_dist = 0.0;
        ob_elev = 0.0;
        out_layer[0] = '\0';
        
        return(ptr);
 
 
}    /*  END OF MAIN PROGRAM */
 
struct point *segment(X,xmax,ymax,t1,t2,p,k1,k2,zv,ncols,seg_in_p,seg_out_p,iv,jv)
 
        int xmax,ymax,p,k1,k2,zv,ncols,X,iv,jv;
        double t1,t2;
        SEGMENT *seg_in_p, *seg_out_p;
 
{
        int s1,s2,less,x,y,xa,ya,xt,yt;
        struct point *head = NULL, *present_point, *make_list();
        int q;
        struct point *free_list();
        struct point *hidden_point_elimination();
 
        q = 1 + (X-1)/4;
 
        if(t1 != 0){ less= ymax/t1 + 0.99;
                         xmax= (xmax<=less)?xmax:less;
                        }
 
        for(x=xmax ; x > 0; x--)
        {
        s1= x * t1 + 0.9;
        s2= x * t2 ;
        s2= (s2<=ymax)?s2:ymax;
 

        for(y= s2; y>= s1; y--)
        {
 
        if(p==0) { xt= x; yt = y;}
         else    { yt= x; xt = y;}
 
        xa = k2*xt ;  ya = k1*yt;
 
        head = make_list(head,ya,xa,seg_in_p,ncols,zv,q,iv,jv);
 
 
        }
        }  /* end of outer loop */
         
 
 
if(head != NULL){

                  
         head->back = NULL;
        present_point = head;
        while(present_point->next != NULL){
 
        present_point->next->back = present_point;
        present_point = present_point->next;

        }                                    
 
 
 
head= hidden_point_elimination(head,zv,ncols,seg_in_p,seg_out_p,q,k1,k2,iv,jv);
 
}
        return(head);
 
}       /* END OF FUNCTION SEGMENT */
 
 
double decide_color_range(angle)
        double angle;
{
        int i;

        i = angle + 0.99;
        return((COLOR_MAX - COLOR_SHIFT)/i);
}
 
 
 
 
struct point *make_list(head,y,x,seg_in_p,ncols,zv,q,iv,jv)
 
        int y,x,ncols,zv,q,iv,jv;
        struct point *head;
        SEGMENT *seg_in_p;
 
{
        double del_x,del_y,dist,sqrt(),angle,atan(),incl;
        CELL zp;
        char *value;
        struct point *make_point();
        static struct point  *present_point;
 
        del_x= abs(x)  ;
        del_y= abs(y)  ;

        dist = sqrt(del_x*del_x +del_y*del_y) * window.ns_res;
 
        if(dist > m_dist) return(head);

        if(del_x==0) angle=ONE;         
        else angle= atan(del_y/del_x);

        switch(q){
                case 1: break;
                case 2: angle = TWO - angle; break;
                case 3: angle = TWO + angle; break;
                case 4: angle = FOUR - angle; break;
                default: break;
                }
 
        value = (char *) &zp;
        segment_get(seg_in_p,value,iv-y,x+jv);
        incl= atan((zp-zv)/dist);
 
 
if(head== NULL){      /* first point ? */
 
        head= make_point(dist,angle,incl,y,x);
        present_point = head;
 
}
 
else {present_point->next = make_point(dist,angle,incl,y,x);
        present_point = present_point->next;
}
        return(head);
        }  /* END OF FUNCTION MAKE_LIST  */
 
 
 struct point *make_point(dist,angle,incl,i,j)
        double dist,angle,incl;
        int i,j;
 
{       struct point *new_point;
        char *malloc();
 
        new_point = (struct point *)malloc(sizeof(struct point));
 
        new_point->theta= angle;
        new_point->beta= incl;
        new_point->ii= i;
        new_point->jj= j;
        new_point->next= NULL;
 
        return(new_point);
 
        }
 
struct point *hidden_point_elimination(head,zv,ncols,seg_in_p,seg_out_p,q,k1,k2,iv,jv)
 
        struct point *head;
        SEGMENT *seg_in_p, *seg_out_p;
        int zv,ncols,q,k1,k2,iv,jv;
 
{
        struct point *p_ptr, *c_ptr;
        double fabs();
        int uu,vv;
        double angle_a,angle_b,ngle(),next_beta_a,next_beta_b,i_beta;
        int xxx,yyy,yyya,xxxa,xxxb,yyyb,abs(),zp;
        double del_x,del_y,dist,atan(),sqrt(),beta(),next_beta,angle;
        struct point *delete();
 
 
        uu = (k1 + k2)/2;
        vv = (k1 - k2)/2;
 
        for(c_ptr= head->next; c_ptr != NULL; c_ptr= c_ptr->next){
 
        if(c_ptr->ii == 0 || c_ptr->jj == 0){
                        xxxa = c_ptr->jj - vv;
                        yyya = c_ptr->ii + uu;

                        xxxb = c_ptr->jj + uu; 
                        yyyb= c_ptr->ii + vv;
                                         
}
else{
 
                         xxxa = c_ptr->jj - uu;
                         yyya = c_ptr->ii - vv;
                                         
                         xxxb = c_ptr->jj + vv;
                        yyyb = c_ptr->ii - uu;
                                         
}
 
        angle_a = ngle(xxxa,yyya,q);
        angle_b = ngle(xxxb,yyyb,q);
 
        next_beta_a = beta(xxxa,yyya,zv,seg_in_p,ncols,iv,jv);
        next_beta_b = beta(xxxb,yyyb,zv,seg_in_p,ncols,iv,jv);
 

for(p_ptr=head;p_ptr!=c_ptr;p_ptr=p_ptr->next){

if(c_ptr->beta <= p_ptr->beta);
else{   /* zero else */
 
                        
        if(p_ptr->theta == c_ptr->theta){
                head= delete(p_ptr,head,ncols,seg_out_p,iv,jv);
                }
 
else{

      
 
 
        if(p_ptr->theta > c_ptr->theta)
        {xxx = xxxa; yyy = yyya;
        next_beta = next_beta_a;
        angle = angle_a;}
 
 else   {xxx = xxxb; yyy = yyyb;
        next_beta = next_beta_b;
        angle = angle_b;}
 
if(fabs(c_ptr->theta-p_ptr->theta)< fabs(c_ptr-> theta - angle)) { 
 
        if(p_ptr->ii == yyy && p_ptr->jj == xxx){
        }
 
   else{        /* second else */
 
 
 
if(p_ptr->beta < next_beta){
                        head= delete(p_ptr,head,ncols,seg_out_p,iv,jv);
                        }
 
else{   /* fourth else */
 
                /* INTERPOLATION */
i_beta= c_ptr->beta + (p_ptr->theta - c_ptr->theta)/(angle - c_ptr->theta)                                         * (next_beta - c_ptr->beta);
 
if(p_ptr->beta < i_beta){
                        head= delete(p_ptr,head,ncols,seg_out_p,iv,jv);
                        }
 
}       /* fourth else       */  
}       /* second else       */  
}       /* first else        */  
}       /* zero else         */  
}       /* end of check      */  
 
}       /* end of inner loop */
 
}       /* end of outer loop */
        return(head);
}       /* END OF FUNCTION HIDDEN_POINT_ELIMINATION */
 
 
 
double ngle(x,y,q)
 
        int x,y,q;
 
{       double del_x,del_y,atan(),angle;
        int abs();
 
        del_x = abs(x) ;
        del_y = abs(y);
 
        if(del_x == 0.0) angle = ONE;
        else angle = atan(del_y/del_x) ;
 
        switch(q){
 
        case 1 : break;
        case 2 : angle = TWO - angle; break;
        case 3 : angle = TWO + angle; break;
        case 4 : angle = FOUR - angle; break;
        default : break;
 
        }       /* end of switch */
 
        return(angle);
 
}       /* END OF FUNCTION ANGLE */
double beta(x,y,zv,seg_in_p,ncols,iv,jv)
 
        int x,y,zv,ncols,iv,jv;
        SEGMENT *seg_in_p;
 
 
{       double  del_x, del_y,dist;
        int abs();
        CELL zp;
        char *value;

                     
        del_x = abs(x) ;
        del_y = abs(y) ;
 
        dist = sqrt(del_x * del_x + del_y * del_y) * window.ns_res;
 
        value = (char *) &zp;
        segment_get(seg_in_p,value,iv-y,x+jv);
        return(atan((zp-zv)/dist));
}
 
 
 
struct point *delete(p,h,ncols,seg_out_p,iv,jv)
         
        struct point *p,*h;
        int ncols,iv,jv;
        SEGMENT *seg_out_p;
 
{
        CELL data;
        char *value;
 
        data = 18;
        value = (char *) &data;
 
        segment_put(seg_out_p,value,iv- p->ii, p->jj + jv);
 
        if(p==h)   /*  first one ?  */
        {
                p->next->back = NULL;
                h = p->next;
                free(p);
                return(h);
        }
 
                         /*  otherwise  */
                p->next->back = p->back;
                p->back->next = p->next;
                free(p);
                return(h);
 
        }      /*  END OF DELETE FUNCTION  */
 
 
 
 
mark_visible_points(head,seg_out_p,ncols,iv,jv,factor)
 
        SEGMENT *seg_out_p;
        struct point *head;
        int ncols,iv,jv;
        double factor;
 
{
        struct point *pt;
        double p_slope;
        double fabs();
        CELL data ;
        char *value;

        value = (char *) &data;
        pt =  head;
 
        while(pt != NULL)   /* loop till end of list */
 
        {
 
        data = (CELL ) (pt->beta * 57.3 * factor + COLOR_SHIFT);
        value = (char *) &data;
 
        segment_put(seg_out_p,value, iv - pt->ii, pt->jj + jv);
 
     pt= pt->next; /* next visible point */
 
                }
}
 
 
 
struct point *free_list(head)
 
        struct point *head;
 
{       struct point *present_point = head, *temporary;
 
        while(present_point != NULL)
        { temporary= present_point;
        present_point = present_point->next;
         free(temporary);
        }
}
        /* END OF FUNCTION FREE_LIST  */

