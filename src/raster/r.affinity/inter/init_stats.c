
#include "global.h"

int get_info()

{
int count,i,yes;
CELL cellv;
int row,col;
int nrows, ncols;
int band;

    vals = (CELL *) G_malloc (Ref.nfiles*MAX_GREYLEVAL* sizeof (CELL));
    if(vals==NULL) {
       fprintf (stderr, "No memory!");
       exit(1);
     }

    nrows = G_window_rows();
    ncols = G_window_cols();
    fprintf (stderr, "\n rows: %d, cols: %d",nrows,ncols);
    
    for(i=0;i<Ref.nfiles;i++) pixelnum[i]=0.0;  
  
    count=0;
    for (row = 0; row < nrows; row++)
      {
	    if (G_get_map_row (t_fd, t_cell, row) < 0) exit(1);
            for (col=0;col<ncols;col++){
               cellv=t_cell[col];
               if (cellv==0) continue;
               yes=0;
               for(i=0;i<count;i++)     
                    if(cluster_val[i]==cellv) {
                       yes=1;
                       break;
                      }
               if (!yes) 
                  {cluster_val[count]=cellv;  count++; }
               if (count== MAX_CLUSTERS) {
                     fprintf (stderr, "Too many clusters!");
                     close_files();
                     exit(1);
                  }
              
             }
      }

    clusternum=count;
    fprintf (stderr, "\n cluster number: %d",clusternum);

    for(band=0;band<Ref.nfiles;band++){
      count=0;
      for (row = 0; row < nrows; row++)
      {
	    if (G_get_map_row (cellfd[band], cell[band], row) < 0) exit(1);
            for (col=0;col<ncols;col++){
               cellv=cell[band][col];
               if (cellv==0) {
                  pixelnum[band]++;
                  continue;
                  }
               for(i=0;i<count;i++)     if(vals[band*MAX_GREYLEVAL+i]==cellv)  break;
               if (i==count) 
                  {vals[band*MAX_GREYLEVAL+count]=cellv;  count++; }
               if (count== MAX_GREYLEVAL) {
                     fprintf (stderr, "Too many values!");
                     close_files();
                     free(vals);
                     exit(1);
                  }
              
             }
      }

      val_num[band]=count;
      fprintf (stderr, "\n number of different values in layer %d: %d",band,count);
      
    }

  
 return 0;

}


/* allocate memory for stats */
int alloc_stats()

{ 
    
    int nomemory=0;
    char *name, *mapset;
    int i;
   

   /* get the variable needed */
   get_info();
   /* allocate memory for stats  */
   switch ( Ref.nfiles){

     case 7:
        statsb6=(struct Node_stats *) malloc(clusternum*val_num[6]*sizeof(struct Node_stats));
        if (statsb6==NULL ) nomemory=1;

     case 6:
  	statsb5=(struct Node_stats *) malloc(clusternum*val_num[5]*sizeof(struct Node_stats));
        if (statsb5==NULL ) nomemory=1;
     
     case 5:  
        statsb4=(struct Node_stats *) malloc(clusternum*val_num[4]*sizeof(struct Node_stats));
        if (statsb4==NULL ) nomemory=1;

     case 4:
   	statsb3=(struct Node_stats *) malloc(clusternum*val_num[3]*sizeof(struct Node_stats));
        if (statsb3==NULL ) nomemory=1;

     case 3:
   	statsb2=(struct Node_stats *) malloc(clusternum*val_num[2]*sizeof(struct Node_stats));

        if (statsb2==NULL ) nomemory=1;

     case 2:
   	statsb1=(struct Node_stats *) malloc(clusternum*val_num[1]*sizeof(struct Node_stats));
        if (statsb1==NULL ) nomemory=1;

     case 1:
   	statsb0=(struct Node_stats *) malloc(clusternum*val_num[0]*sizeof(struct Node_stats));
        if (statsb0==NULL ) nomemory=1;
  }

   
 if (nomemory==1 )
     {
		fprintf (stderr, "Not enough memory!");
                close_files();
                free(vals);
		exit(1);
     }

  return 0;

}


/* initialize the stats  */
init_stats()

{

int rn;
int i,tempi;
 
 switch ( Ref.nfiles){

    case 7:
                
      for (rn=0;rn<clusternum*val_num[6];rn++)
        {
        tempi=rn%val_num[6];
        statsb6[rn].greyv=vals[6*MAX_GREYLEVAL+tempi];
        statsb6[rn].count_c=0;
        statsb6[rn].count_all=0;
        }
        
    case 6:

                
      for (rn=0;rn<clusternum*val_num[5];rn++)
        {
        tempi=rn%val_num[5];
        statsb5[rn].greyv=vals[5*MAX_GREYLEVAL+tempi];
        statsb5[rn].count_c=0;
        statsb5[rn].count_all=0;
        }


    case 5:

                 
      for (rn=0;rn<clusternum*val_num[4];rn++)
        {
        tempi=rn%val_num[4];
        statsb4[rn].greyv=vals[4*MAX_GREYLEVAL+tempi];
        statsb4[rn].count_c=0;
        statsb4[rn].count_all=0;
        }
        

    case 4:

      for (rn=0;rn<clusternum*val_num[3];rn++)
        {
        tempi=rn%val_num[3];
        statsb3[rn].greyv=vals[3*MAX_GREYLEVAL+tempi];
        statsb3[rn].count_c=0;
        statsb3[rn].count_all=0;
        }
        
    case 3:

               
      for (rn=0;rn<clusternum*val_num[2];rn++)
        {
        tempi=rn%val_num[2];
        statsb2[rn].greyv=vals[2*MAX_GREYLEVAL+tempi];
        statsb2[rn].count_c=0;
        statsb2[rn].count_all=0;
        }
        
    case 2:

             
      for (rn=0;rn<clusternum*val_num[1];rn++)
        {
        tempi=rn%val_num[1];
        statsb1[rn].greyv=vals[1*MAX_GREYLEVAL+tempi];
        statsb1[rn].count_c=0;
        statsb1[rn].count_all=0;
        }
        

    case 1:

              
      for (rn=0;rn<clusternum*val_num[0];rn++)
        {
        tempi=rn%val_num[0];
        statsb0[rn].greyv=vals[0*MAX_GREYLEVAL+tempi];
        statsb0[rn].count_c=0;
        statsb0[rn].count_all=0;
        }
        

  }
 
free(vals);

}


 /*free memory for stats */
int free_stats()

{
  

     switch ( Ref.nfiles){

     case 7:
        free(statsb6);

     case 6:
        free(statsb5);

     case 5:  
        free(statsb4);

     case 4:
        free(statsb3);

     case 3:
        free(statsb2);

     case 2:
        free(statsb1);

     case 1:
        free(statsb0);
  
      }
   
   return 0;
 
}


                       
                        
                  
