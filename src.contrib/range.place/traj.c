#include "segment.h"
#include "gis.h"
#define MAIN
#include "point.h"

#define         GRAVITY         9.86
#define         ONE              90.0
#define         TWO              180.0
#define         THREE            270.0
#define         FOUR             360.0
#define         PI_BY_FOUR       0.785398163
#define         PI_BY_TWO        1.570796327
#define         PI               3.141592654

CELL zv;           /* absolute elevation of the gun        */
extern char buf[];
extern struct Cell_head window;

extern int ifir,jfir;              /* map array coors of firing pt.*/
 int nrows,ncols;
extern double high_angle,low_angle;
extern double azimuth1,azimuth2;
extern double vel_initial;
extern char displayed_map[];
extern char out_layer[];
extern double ob_elev;
 
double tent_max_dist;   /* tentative max range of fall  */
 
  int total;      /* total points picked up in first list */
 
 
 
struct window *traj(ptr)
        struct window *ptr;
 
{
        char *current_mapset,
        *search_mapset,
        *old_mapset;    /* points to the mapset of the elev_layer
*/
   
        CELL *cell,        /* pointer to CELL i/o buffer */
                   data;
 
        /* structure to hold the header info of the elevation layer */
         struct Cell_head cellhd;
 
        int srows,scols,        /* rows and cols in every segment
*/
            len;                /* size of a cell entry (bytes)
*/
        int stash_away(),set_default_options();
 
        int new                 /* fd for output overlay        */        ,old,                   /* fd for elevation overlay     */        in_fd,                  /* fd for segmented elev layer  */        out_fd;                 /* fd for segmented output layer*/ 
        int row;                /* row counter used in looping  */ 
 
        SEGMENT seg_in, seg_out;
 
        struct Colors colors;
	struct Range range;
        char *value;
 
        int i,j;        /* looping variables.(0,0) at left top  */ 
        struct new_point *make_lis(),*sort_list(),*prune_list(),
                *head, *c_ptr, *present_point;
 
        double dtheta;

                       
 
 
        /* name of current mapset       */
        current_mapset= G_mapset();
 
 
 
        /* read in database window parameters   */
        if (G_get_window (&window) < 0)
    {
        sprintf (buf, "can't read current database window parameters");
        G_fatal_error (buf);
        exit(1);
    }
 
 
 
        /* check if the elevation overlay exists        */
        search_mapset = "";
 
        old_mapset = G_find_cell (displayed_map, search_mapset);
    if (old_mapset == NULL)
    {
        sprintf (buf, "%s - not found", displayed_map);
        G_fatal_error (buf);
        exit(1);
    }
 
 
        /* check if the output layer has a legal filename       */        if (G_legal_filename(out_layer) < 0)
    {
        sprintf (buf, "%s - illegal name", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
 
 
        /* check if the output layer already exists in the      */        /* user's current mapset                                */
        if (G_find_cell (out_layer,current_mapset))
    {
        sprintf (buf, "%s - already exits. can't overwrite", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
 
 
        /* read the cell header into the appropriate structure  */        if (G_get_cellhd (displayed_map, old_mapset, &cellhd) < 0)    {
        sprintf (buf, "%s in %s - can't read cell header", displayed_map, old_mapset);
        G_fatal_error (buf);
        exit(1);
    }
 
        /* calculate rows and columns of the database window    */        nrows = G_window_rows();
        ncols = G_window_cols();
        /* printf("\n nrows=%d, ncols=%d\n",nrows,ncols); */
 
        len = sizeof(CELL);
 
        /* decide the rows and columns in every segment         */        srows = nrows/5 + 1;
        scols = ncols/5 + 1;
 
 
        /* allocation of buffer for row i/o     */
        cell = G_allocate_cell_buf();

                                           
 
        /* open elevation layer for reading     */
    old = G_open_cell_old (displayed_map, old_mapset);
    if (old < 0)
    {
        char buf[200];
        sprintf (buf, "%s in %s - can't open cell file", displayed_map, old_mapset);
        G_fatal_error (buf);
        exit(1);
    }

      
        /* creating and opening of output layer for writing     */    new = G_open_cell_new (out_layer);
    if (new < 0)
    {
        sprintf (buf, "%s - can't create cell file", out_layer);
        G_fatal_error (buf);
        exit(1);
    }
 
        /* creating and formatting of the segmented elev overlay */
        in_fd = creat("in_seg_file",0666);
        segment_format(in_fd,nrows,ncols,srows,scols,len);
        close(in_fd);
 
        /* creating and formatting of the segmented output overlay */
        out_fd = creat("out_seg_file",0666);
        segment_format(out_fd,nrows,ncols,srows,scols,len);
        close(out_fd);
 
         
        in_fd = open("in_seg_file",2);
        segment_init(&seg_in,in_fd,4);
 
        out_fd = open("out_seg_file",2);
        segment_init(&seg_out,out_fd,4);

         
        /* conversion of gun angles from degrees to radians     */        low_angle = low_angle * PI / 180.0;
        high_angle = high_angle * PI / 180.0;
 
 
        /* reading of data from the original elev overlay and   */        /* writing into the segmented file format               */        for(row = 0; row < nrows; row++)
        {
        if (G_get_map_row (old,cell,row) < 0)
        exit(1);

        segment_put_row(&seg_in,cell,row);
        }

          

        /* printf("\n ns_res=%lf   ew_res=%lf  rows=%d  cols=%d\n",window.ns_res,window.ew_res,window.rows,window.cols);
        */
 
 
        /* calculate the absolute elevation of the gun  */
        value = (char *) &zv;
        segment_get(&seg_in,value,ifir,jfir);
        zv += ob_elev;
 
        /*core to be filled in  */
 
        /* tentative maximum range of fall to limit points      */        /* to be picked up int the preliminary list             */        tent_max_dist = 1.1 * vel_initial * vel_initial / GRAVITY;        
	/* printf("\n tent_max_dist = %lf", tent_max_dist); */
 
        head = NULL;
        total = 0;
 
        for(i=0; i<nrows; i++){         /* loop over rows       */                for(j=0; j<ncols; j++){ /* loop over columns    */ 
        if(i==ifir && j==jfir);
        else
                head = make_lis(head,ifir-i,j-jfir,&seg_in);
                                        }
                                }

        head = sort_list(total,head);

        /* printf("\n je viens");       */ 
 
 
        for(c_ptr=head->next; c_ptr !=NULL; c_ptr= c_ptr->next) {
 
        dtheta= window.ns_res *180.0/PI/c_ptr->r;
 
for(present_point=head;present_point!=c_ptr;present_point=present_point->next){
 
 if (c_ptr->l_traj >=  present_point->l_traj)   /* chance of blocking */
  check(present_point,c_ptr,head,dtheta,&seg_in);
        }
        }       /* end of outer for     */
         
 
 
 
 
 
        mark_impact_points(head,&seg_out);
 
 
 
 
 
 
 
        /* end of core to be filled in  */
 
 
 
        data = 55;
        value = (char *) &data;
        segment_put(&seg_out,value,ifir,jfir);


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

        return(ptr);
 
}       /* END OF MAIN PROGRAM  */
check(p_ptr,c_ptr,head,dtheta,seg_in_p)
 
struct new_point *p_ptr,*c_ptr,*head;
double dtheta;
SEGMENT *seg_in_p;
 
{       double angle;
        int iii,jjj,ii,jj,abs();
        double del_x,del_y,dist,fabs(),atan(),sqrt();
 
        double higher,lower;    /* low and high angles of impact
 */
        double i_higher,i_lower;
        double A,B,a,b,c,
                disc;   /* discriminant in the quadratic equation
 */
 
        struct new_point *delet();
 
 
 
/*      out of horizontal angle checking range  */
if(fabs(c_ptr->theta - p_ptr->theta) > dtheta) {
                /* printf("\n dhteta= %lf",dtheta); */
                return;
                        }
 
/* printf("\n check "); */
 
/*if directly behind blocking pt, farther point's l_traj blocked*/if(p_ptr->theta == c_ptr->theta){
 
/* printf("\nbplt=%lf, bpht=%lf, cplt=%lf, cpht=%lf\n",
                c_ptr->l_traj,c_ptr->h_traj,p_ptr->l_traj,p_ptr->h_traj); */
if(obscured(c_ptr->l_traj,c_ptr->h_traj,p_ptr->l_traj,p_ptr->h_traj))
        {head = delet(p_ptr,head);
         /* printf("\n deleted");*/}
                return;
                } 
        ii = ifir - c_ptr->ii;
        jj = jfir + c_ptr->jj;
if((c_ptr->theta < 90.0) && (c_ptr->theta!=0.0)){
        if(p_ptr->theta < c_ptr->theta){iii= ii +1;
                                        jjj= jj;
                                        }
                                else   {iii= ii;
                                        jjj= jj -1;
                                        }
                        }
 
else if(c_ptr->theta>90.0 && c_ptr->theta<180.0){
        if(p_ptr->theta < c_ptr->theta){iii= ii;
                                        jjj= jj+ 1;
                                        }
                                else   {iii= ii +1;
                                        jjj= jj;
                                        }
                        }
 
else if(c_ptr->theta>180.0 && c_ptr->theta<270.0){
        if(p_ptr->theta < c_ptr->theta){iii= ii -1;
                                        jjj= jj;
                                        }
                                else   {iii= ii;
                                        jjj= jj +1;
                                        }
                        }
 
else if(c_ptr->theta > 270.0){
        if(p_ptr->theta < c_ptr->theta){iii= ii;
                                        jjj= jj -1;
                                        }
                                else    {iii= ii -1;
                                        jjj = jj;
                                        }
                        }

else if(c_ptr->theta == 0.0){ iii= ii -1;
                              jjj= jj;
                              }

else if(c_ptr->theta == 90.0){iii= ii;
                if(p_ptr->theta < c_ptr->theta) jjj= jj +1;
                                        else    jjj= jj -1;
                                        }
else if(c_ptr->theta == 180.0){jjj= jj;
                if(p_ptr->theta < c_ptr->theta) iii= ii -1;
                                        else    iii= ii +1;
                                        }

else if(c_ptr->theta == 270.0){iii= ii;   
                if(p_ptr->theta < c_ptr->theta) jjj= jj -1;
                                        else    jjj= jj +1;
                                        }
 
        del_x = (double ) abs(jfir-jjj) ;
        del_y = (double ) abs(ifir-iii) ;
 
        if(del_x == 0.0) angle = 90.0;
        else angle = atan(del_y/del_x) * 180.0/PI;
 
        if(iii<=ifir && jjj>=jfir);                 /* first quadrant */        else if(iii<=ifir && jjj<jfir)              /* second quadrant */
                angle= 180.0 - angle;
        else if(iii>ifir && jjj<= jfir)             /* third quadrant  */
                angle= 180.0 + angle;
        else angle = 360.0 - angle;             /* fourth quadrant */
 
if(p_ptr->ii==iii && p_ptr->jj==jjj) {/*printf("\n one");*/return;}
if(fabs(c_ptr->theta - angle) <=  fabs(c_ptr->theta - p_ptr->theta))                                      {/*printf("\n two");*/  return;}
 
dist = sqrt(del_x * del_x + del_y * del_y) * window.ns_res;

        find_ABabcdisc(&A,&B,&a,&b,&c,&disc,seg_in_p,dist,iii,jjj);
        find_angles(&lower,&higher,&disc,&a,&b);
 

 
                /* INTERPOLATION */

i_lower= c_ptr->l_traj + (p_ptr->theta - c_ptr->theta)/(angle - c_ptr->theta)                                         * (lower - c_ptr->l_traj);
 
i_higher= c_ptr->h_traj + (p_ptr->theta - c_ptr->theta)/(angle - c_ptr->theta)                                         * (higher - c_ptr->h_traj);

               
        /* printf("\n i_lower=%lf, i_higher=%lf",i_lower,i_higher); */

if(i_lower >= p_ptr->l_traj) {                                   
/* printf("\nbplt=%lf, bpht=%lf, cplt=%lf, cpht=%lf\n",
                i_lower,i_higher,p_ptr->l_traj,p_ptr->h_traj); */
        if(obscured(i_lower,i_higher,p_ptr->l_traj,p_ptr->h_traj))        {head = delet(p_ptr,head);
                /* printf("\n deleted");*/}
                        return;
                        }

                          
                        } /*  END OF CHECK  */
 

obscured(bp_ltraj,bp_htraj,cp_ltraj,cp_htraj)

        double bp_ltraj,bp_htraj,cp_ltraj,cp_htraj;
{
 
        /* printf("\n obscure check"); */
        /* check if the farther point can be hit by the high traj
*/
        if(cp_htraj > high_angle) return (1);
 
  /* check if the ltraj of bp can block the htraj of the farther pt.  */
        if(bp_ltraj > cp_htraj) return (1);
 
        /* high trajectory check        */
        if(bp_htraj > high_angle)
                return (0);     /* bp cannot obscure    */
 
        else{
                if(bp_htraj <= cp_htraj) return (1); /* obscured */
                else return (0);
                }
 
}       /* END OF OBSCURED? CHECK       */
                 
 
 
struct new_point *make_lis(head,y,x,seg_in_p)
 
        struct new_point *head;
        SEGMENT *seg_in_p;
        int x,y;
 
{
        double del_x,del_y,dist,sqrt(),angle,atan();
        struct new_point *present_point, *make_poin();
        double higher,lower;    /* low and high angles of impact
 */
        double A,B,a,b,c,
                disc;   /* discriminant in the quadratic equation
 */
 
 
 
        del_x= (double) abs(x)  ;
        del_y= (double) abs(y)  ;

        dist = sqrt(del_x*del_x +del_y*del_y) * window.ns_res;
 
        if(dist > tent_max_dist) return(head);
 
        if(del_x==0) angle=ONE;
        else angle= atan(del_y/del_x) * 180.0/ PI;
 
        if(x>=0 && y>=0);
        else if(x<=0 && y>=0) angle = TWO - angle;
        else if(x<=0 && y<=0) angle = TWO + angle;
        else angle = FOUR - angle;
 
        if(angle<azimuth1 || angle>azimuth2)
        return(head);   /* out of horizontal angle range        */ 
 
        find_ABabcdisc(&A,&B,&a,&b,&c,&disc,seg_in_p,dist,ifir-y,x+jfir);
 
        if(disc < 0.0) return(head);    /* unhittable   */
 
        find_angles(&lower,&higher,&disc,&a,&b);
 
        if(higher> high_angle  && lower< low_angle)
        return(head);   /* unhittable due to gun parameters     */ 
        /* printf("\n lower=%lf,  higher=%lf",lower*180.0/3.142,higher*180/3.142); */
 
        total++;
 
        if(head== NULL){      /* first point ? */

        head= make_poin(dist,angle,y,x,lower,higher);
        present_point = head;

        }                     

else {present_point->next = make_poin(dist,angle,y,x,lower,higher);
        present_point = present_point->next;
        }
        return(head);
         
                      
        }  /* END OF FUNCTION MAKE_LIST  */

 
struct new_point *make_poin(dist,angle,i,j,lower,higher)
        double dist,angle,lower,higher;
        int i,j;
 
{       struct new_point *new_point;
        char *malloc();
 
        new_point = (struct new_point *)malloc(sizeof(struct new_point));
 
        new_point->theta= angle;
        new_point->r    = dist;
        new_point->ii= i;
        new_point->jj= j;
        new_point->l_traj = lower;
        new_point->h_traj = higher;
        new_point->next= NULL;

        return(new_point);     

        }                  

          
 
mark_impact_points(head,seg_out_p)
 
        SEGMENT *seg_out_p;
        struct new_point *head;
 
{
        struct new_point *pt;
        CELL data = 250;
        char *value;

        value = (char *) &data;
        pt =  head;
 
        while(pt != NULL)   /* loop till end of list */
 
        {
 
        segment_put(seg_out_p,value, ifir - pt->ii, pt->jj + jfir);
 
     pt= pt->next; /* next impact point */
 
                }
}
 
 
 
 
find_ABabcdisc(A,B,a,b,c,disc,seg_in_p,dist,ii,jj)
        double *A,*B,*a,*b,*c,*disc,dist;
        SEGMENT *seg_in_p;
        int ii,jj;
 
{
        CELL zp;   /* the "y" for any point        */
        char *value;
 
 
        value = (char *) &zp;
        segment_get(seg_in_p,value,ii,jj);
        *A = (zp - zv)/dist;
        *B = GRAVITY * dist / 2.0 /vel_initial/ vel_initial;
 
        *a = *A * *A + 1.0;
        *b = 2.0* *A* *B - 1.0;
        *c = *B * *B;

        *disc = *b* *b - 4.0 * *a * *c;
}

  
find_angles(low,high,disc,a,b)
        double *low,*high,*disc,*a,*b;
{
        double sqrt(),acos();
 
        *disc = sqrt(*disc);
        *a = 2.0 * *a;

        *high= acos(sqrt((-(*b) - *disc)/(*a)));
        *low = acos(sqrt((-(*b) + *disc)/(*a)));

}                            
struct new_point *sort_list(total,head)
 
        int total;
        struct new_point *head;
 
{
        struct new_point **ptr, *temporary, *present_point ;
        int count, cut, cou;
        char *malloc();
 
        /* printf("\n zero"); */
 
ptr= (struct new_point **)malloc(total*sizeof(struct new_point *));
        /* printf("\n zeroq"); */
present_point = head;
 
for(count=0;count<total;count++){
        *(ptr+count)= present_point;
        present_point= present_point->next;
        }   /*  Initialization of pointers  */
 
        /* printf("\n one"); */
 
for(cut=1;cut<total;cut++){
        temporary= *(ptr+cut);
        for(cou=cut;
        cou>0
        && (*(ptr+cou-1))->r < temporary->r;
        cou--){
        *(ptr+cou)= *(ptr+cou-1);
        *(ptr+cou-1)= temporary;
        }
        *(ptr+cou)= temporary;
        }

          
       /*  printf("\n two"); */
        /*  reordering of list  */

for(count =0,head = present_point = *ptr;
        count< (total-1); count++){
        present_point->next = *(ptr+count+1);
        present_point = present_point->next;
        }

       /* if(count != 0) */ present_point->next = NULL;
        free(ptr);

                   
        /* printf("\n three"); */
        /* ASSIGNMENT OF BACK POINTERS */
 
        head->back = NULL;
        present_point = head;

        while(present_point->next != NULL){

        present_point->next->back = present_point;
        present_point = present_point->next;

        }                                    

          
        return(head);
         
        }       /* END OF FUNCTION SORT_LIST */

                                
 
struct new_point *delet(p,h)
         
        struct new_point *p,*h;
 
{
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
 

