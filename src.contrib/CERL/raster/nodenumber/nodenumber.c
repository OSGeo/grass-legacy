#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>

#define DIM 380

#define random(A) (A*rand()/RAND_MAX)

main()
{
   FILE *in_fptr;
   FILE *in_basin_fptr;
   FILE *in_dat_fptr;
   FILE *out_fptr;
   int i,j,k,l,m,rowdex,coldex,prevdir;
   int chanmat[DIM][DIM],endrow[DIM],endcol[DIM];
   int nodemat[DIM][DIM];
   int rows,cols,work,itmp;
   int numstreams;
   int occur;
   int chan[50],depend[50],count;
   char northc[10],southc[10],eastc[10],westc[10],rowc[10],colc[10];
   long north,south,east,west;
   int outrow, outcol;
   int startrow[30],startcol[30];
   int nx1[30],nodenum,lnow;
   


   if((in_fptr=fopen("yak_links.asc","r"))==NULL)
      {fprintf (stdout,"Can't open goodwin.links\n");return(0);}

   if((out_fptr=fopen("yak_nodes.asc","w"))==NULL)
      {fprintf (stdout,"Can't open goodwin.nodes\n");return(0);}

   fprintf (stdout,"files are open\n");

   fscanf(in_fptr,"%6s %ld",northc,&north);
   fscanf(in_fptr,"%6s %ld",southc,&south);
   fscanf(in_fptr,"%5s %ld",eastc,&east);
   fscanf(in_fptr,"%5s %ld",westc,&west);
   fscanf(in_fptr,"%5s %d",rowc,&rows);
   fscanf(in_fptr,"%5s %d",colc,&cols);
  


   for(j=0;j<rows;j++)
      {
      for(i=0;i<cols;i++)
         {
         fscanf(in_fptr,"%d",&chanmat[i][j]);
         nx1[chanmat[i][j]]++;
         }
      }
   fprintf (stdout,"done reading data\n");

   /* these are the c notation coordinates of the outlet */
   endrow[11]=43;
   endcol[11]=10;

   fprintf (stdout,"numstreams=%d\n",chanmat[10][43]);
   numstreams=chanmat[endcol[11]][endrow[11]];
  /*  nodemat[endcol[11]][endrow[11]]=nx1[11]; */
   

   for(count=numstreams;count>=1;count--)
      {
      fprintf (stdout,"working on stream %d, nx1[%d]=%d \n",count,count,nx1[count]);
      work=1;

      occur=0;
      prevdir=0;
      nodenum=nx1[count];
      rowdex=endrow[count];
      coldex=endcol[count];
      nodemat[endcol[count]][endrow[count]]=nx1[count];
 
      /* if(count==numstreams) nodenum=nx1[count]-1; */
      nodenum=nx1[count]-1;
      while(work==1)
         {
         /* search for channel of desired link number, excluding previous one */
         if((chanmat[coldex][rowdex-1]==count) &&
             (prevdir!=1))
            {
            rowdex--;
            nodemat[coldex][rowdex]=nodenum;
            nodenum--;
            prevdir=3;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((chanmat[coldex][rowdex+1]==count) &&
             (prevdir!=3))
            {
            rowdex++;
            nodemat[coldex][rowdex]=nodenum;
            nodenum--;
            prevdir=1;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((chanmat[coldex-1][rowdex]==count) &&
             (prevdir!=2))
            {
            coldex--;
            nodemat[coldex][rowdex]=nodenum;
            nodenum--;
            prevdir=4;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((chanmat[coldex+1][rowdex]==count) &&
             (prevdir!=4))
            {
            coldex++;
            nodemat[coldex][rowdex]=nodenum;
            nodenum--;
            prevdir=2;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         else
            {
            lnow=chanmat[coldex][rowdex-1];
            if((lnow!=count)&&(lnow!=0))
               {
               endrow[lnow]=rowdex-1;
               endcol[lnow]=coldex;
               }
            lnow=chanmat[coldex][rowdex+1];
            if((lnow!=count)&&(lnow!=0))
               {
               endrow[lnow]=rowdex+1;
               endcol[lnow]=coldex;
               }
            lnow=chanmat[coldex-1][rowdex];
            if((lnow!=count)&&(lnow!=0))
               {
               endrow[lnow]=rowdex;
               endcol[lnow]=coldex-1;
               }
            lnow=chanmat[coldex+1][rowdex];
            if((lnow!=count)&&(lnow!=0))
               {
               endrow[lnow]=rowdex;
               endcol[lnow]=coldex+1;
               }             
            work=0;  /* this is the end of the current channel */
            startrow[count]=rowdex;
            startcol[count]=coldex;
            }
         }
      }
   fprintf (stdout,"Writing output file\n");
   fprintf(out_fptr,"%6s %ld\n",northc,north);
   fprintf(out_fptr,"%6s %ld\n",southc,south);
   fprintf(out_fptr,"%5s %ld\n",eastc,east);
   fprintf(out_fptr,"%5s %ld\n",westc,west);
   fprintf(out_fptr,"%5s %d\n",rowc,rows);
   fprintf(out_fptr,"%5s %d\n",colc,cols);
   for(j=0;j<rows;j++)
      {
      for(i=0;i<cols;i++)
         {
         fprintf(out_fptr,"%2d ",nodemat[i][j]);
         }
      fprintf(out_fptr,"\n");
      }
   fprintf (stdout,"done writing data\n");
   return(0);
}
  
