#include "global.h"


/* statistic of image */
int image_stats(int nrows,int ncols)

{
    int band;
           
    CELL cellv,cv;  /* appended by Tian */
    int celln,cn;  /* appended by Tian */
    int rn;     /* appended by Tian */
    int row;
    int i;

   

   /* generate the statistic count for every value and every band  */
    for (row = 0; row < nrows; row++)
    {
        /* get one line from the training map */
        if (G_get_map_row (t_fd, t_cell, row) < 0)
		exit1();
        /* get one line from each file  */
	for (band = 0; band < Ref.nfiles; band++)
	  
            if (G_get_map_row (cellfd[band], cell[band], row) < 0)
		exit1();
        
        /* count the number for differrent greyleval  */
        for (band = 0; band < Ref.nfiles; band++)
          
           switch (band) {


             case 0:
                
                    for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];
                       if(cellv==0) continue;
                                            
                       for(i=0;i<val_num[0];i++)
                          if (cellv==statsb0[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb0[rn*val_num[0]+i].count_all++;
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb0[cn*val_num[0]+i].count_c++;
                      
                      }

                    break;

              case 1:

                  for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];

                        if(cellv==0) continue;                                         
                       for(i=0;i<val_num[1];i++)
                          if (cellv==statsb1[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb1[rn*val_num[1]+i].count_all++;
                       
                       /* which class */
                       if (cv==0) continue;

                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb1[cn*val_num[1]+i].count_c++;
                      
                      }
                    break;

              case 2:

                  for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];
                       if(cellv==0) continue;
                       
                       for(i=0;i<val_num[2];i++)
                          if (cellv==statsb2[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb2[rn*val_num[2]+i].count_all++;
                       
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb2[cn*val_num[2]+i].count_c++;
                      
                      }
                    break;

              case 3:
                  for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];

                       if(cellv==0) continue;                       
                       for(i=0;i<val_num[3];i++)
                          if (cellv==statsb3[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb3[rn*val_num[3]+i].count_all++;
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb3[cn*val_num[3]+i].count_c++;
                      
                      }

                    break;

              case 4:

                    for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];
                       if(cellv==0) continue;                       
                       for(i=0;i<val_num[4];i++)
                          if (cellv==statsb4[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb4[rn*val_num[4]+i].count_all++;
                       
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb4[cn*val_num[4]+i].count_c++;
                      
                      }
                    break;

              case 5:

                    for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];

                       if(cellv==0) continue;                       
                       for(i=0;i<val_num[5];i++)
                          if (cellv==statsb5[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb5[rn*val_num[5]+i].count_all++;
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb5[cn*val_num[5]+i].count_c++;
                      
                      }
                    break;

              case 6:

                    for (celln=0;celln<ncols;celln++)
                       {

                       cellv=cell[band][celln];
                       cv=t_cell[celln];
                       if(cellv==0) continue;       
                       
                       for(i=0;i<val_num[6];i++)
                          if (cellv==statsb6[i].greyv) break;
                       
                       for (rn=0;rn<clusternum;rn++)
                         statsb6[rn*val_num[6]+i].count_all++;
                       
                       
                       /* which class */
                       if (cv==0) continue;
                       for(cn=0;cn<clusternum;cn++) 
                           if(cluster_val[cn]==cv) break;

                       statsb6[cn*val_num[6]+i].count_c++;
                      
                      }
                    break;


             }        
 
     }

   
 
 }
