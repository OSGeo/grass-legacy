/************************************************************************
 program node+con.c

   Written by Fred L. Ogden, Ph.D., May 1996.

   This program may be used, distributed and modified freely provided that
   reference is given to the original author in all publications, reports,
   and manuals produced using this program.

   The purpose of this program is to greatly simplify the task of setting
   up channel routing data sets for CASC2D.

INPUT:

-  ascii GRASS elevation map. (smoothed DEM preferable)
-  ascii GRASS links map. (reclassification of r.watershed streams map)
-  ascii file containing needed run parameters and the cross section 
      properties for each link.  At the present time, this program only
      supports trapezoidal channel cross sections (link type 1).  This input
      file should therefore contain, the following parameters:

   **************PARAMETER LIST******************************************
   * slope of the most downstream node  L/L                             *
   * kinetic energy correction factor (1.0 typ.)                        *
   * friction slope weighing coeff. (0.5 typ)                           *
   * implicit finite difference weighting coeff (0.55 min, 1.0 max      *
   * length of each channel node, (typ. equal to raster grid size (m)   *
   * base flow in 1st ord. channels (cms)                               *


 for each numbered link which appears in the link map:

      link_#  link_type mannings_n  bottom_width  channel_depth  side_slope

      See the r.hydro.CASC2D reference manual for details regarding the
      meaning of these variables.

OUTPUT:

-  ascii GRASS nodes map.
-  ascii channel input file with smoothed thalweg elevation profiles.
-  ascii file which contains the change in thalweg elevation at each link/node
      due to thalweg profile smoothing.

NEEDED MODIFICATIONS:
-  add dynamic memory allocation.
-  support for link types 2,5,6,8,9,10,11 
-  support for floating point GRASS elevation map.

IDEA FOR IMPLEMENTATION AS A GRASS COMMAND:

g.node+con  linkmap=_mapname_  elev_map=_mapname_  x_sect_file=_filename_
            nodemap=_mapname_  chan_file=_filename_ diff_file=_filename_

******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>

#define ROWS 400    /* array dimension for maximum number of rows in maps */
#define COLS 400    /* array dimension for maximum number of cols in maps */
#define LINKS 100   /* array dimension for maximum number of links */
#define NODES 200   /* array dimension for maximum number of nodes*/



main()
{
   FILE *in_elev_fptr;
   FILE *in_link_fptr;
   FILE *in_xsect_fptr;
   FILE *out_node_fptr;
   FILE *out_fptr;
   FILE *out_diff_fptr;

   long nx1[LINKS],backdep[LINKS];
   float elevmap[ROWS][COLS];
   float bel[ROWS][COLS];
   float orbel[ROWS][COLS];
   long linkmap[ROWS][COLS];
   long nodemap[ROWS][COLS];
   long endrow[LINKS],endcol[LINKS];
   long startrow[LINKS],startcol[LINKS];

   long i,j,k,l,m,zeros,itmp;
   long ltmp;
   long idex,jdex;
   long depend[LINKS][3];
   long ndep[LINKS];
   long startx,starty,prevdir,found;
   long ltype[LINKS];
   long maxnodes;
   long it,workflag;
   float mtmp,ctmp,btmp,stmp;
   float qmin,chan_dep[LINKS];
   float uslope,newel;
   char northc[10],southc[10],eastc[10],westc[10],rowc[10],colc[10];
   long north,south,east,west;
   long count,work,occur,nodenum,rowdex,coldex,lnow;
   float gr,sout,tol,alpha,beta,theta,dx,tl,Qa[LINKS],mann[LINKS],bot[LINKS];
   float sslope[LINKS];
   float z[LINKS],outelev,deltay,deltax;

   long order1,numlinks,iitmp,old,start,stop,left,right,top,bottom;
   long sortlist[5],big,medium,small,tiny;
   long orig[5];
  
   long rows1,cols1,rows2,cols2,rows,cols;
   float ftmp;

    /************************/
   /* OPEN THE INPUT FILES */
  /************************/

   if((in_elev_fptr=fopen("elev.smooth","r"))==NULL)
      {fprintf (stdout,"Can't open elev.smooth\n");return(0);}

   if((in_link_fptr=fopen("yak_links.asc","r"))==NULL)
      {fprintf (stdout,"Can't open yak_links.asc\n");return(0);}

   if((in_xsect_fptr=fopen("xsect.dat","r"))==NULL)
      {fprintf (stdout,"Can't open xsect.dat\n");return(0);}

    /************************/
   /* OPEN THE OUTPUT FILES */
  /************************/

   if((out_node_fptr=fopen("yak_nodes.asc","w"))==NULL)
      {fprintf (stdout,"Can't open yak_nodes.asc\n");return(0);}

   if((out_fptr=fopen("yak_trap.dat","w"))==NULL)
      {fprintf (stdout,"Can't open yak_trap.dat\n");return(0);}

   if((out_diff_fptr=fopen("diff.out","w"))==NULL)
      {fprintf (stdout,"Can't open diff.out\n");return(0);}

   /**************HARD-CODED PARAMETER LIST*******************/
   /* These are used in writing the channel data file.       */
   /**********************************************************/
   gr=9.81;       /* gravity                              */
   tol=0.000001;  /* tolerance in calc. normal depth      */
   tl=2400.0;     /* a bogus number to be removed later   */
  
    /************************************************/
   /* READ THE HEADERS OF THE TWO INPUT GRASS MAPS */
  /************************************************/

   fscanf(in_elev_fptr,"%6s %ld",northc,&north);
   fscanf(in_elev_fptr,"%6s %ld",southc,&south);
   fscanf(in_elev_fptr,"%5s %ld",eastc,&east);
   fscanf(in_elev_fptr,"%5s %ld",westc,&west);
   fscanf(in_elev_fptr,"%5s %ld",rowc,&rows1);
   fscanf(in_elev_fptr,"%5s %ld",colc,&cols1);

   fscanf(in_link_fptr,"%6s %ld",northc,&north);
   fscanf(in_link_fptr,"%6s %ld",southc,&south);
   fscanf(in_link_fptr,"%5s %ld",eastc,&east);
   fscanf(in_link_fptr,"%5s %ld",westc,&west);
   fscanf(in_link_fptr,"%5s %ld",rowc,&rows2);
   fscanf(in_link_fptr,"%5s %ld",colc,&cols2);

   if((rows1!=rows2)||(cols1!=cols2))
      {
      fprintf (stdout,"Number of rows or columns in elev and links map are different\n");
      fprintf (stdout,"Program exiting.\n");
      exit(0);
      }
   rows=rows1;
   cols=cols1;

    /******************************************/
   /* write the header for the new links map */
  /******************************************/

   fprintf(out_node_fptr,"%6s %ld\n",northc,north);
   fprintf(out_node_fptr,"%6s %ld\n",southc,south);
   fprintf(out_node_fptr,"%5s %ld\n",eastc,east);
   fprintf(out_node_fptr,"%5s %ld\n",westc,west);
   fprintf(out_node_fptr,"%5s %ld\n",rowc,rows);
   fprintf(out_node_fptr,"%5s %ld\n",colc,cols);

   fprintf (stdout,"rows=%ld cols=%ld\n",rows,cols);
   deltax=(float)(east-west)/(float)cols;

   for(i=1;i<LINKS;i++)
      {
      nx1[i]=0;
      endrow[i]=0;
      endcol[i]=0;
      }

    /************************************/
   /* READ THE LINK AND ELEVATION MAPS */
  /************************************/

   numlinks=0;
   for(i=1;i<=rows;i++)
      {
      for(j=1;j<=cols;j++)
         {
         fscanf(in_link_fptr,"%ld",&itmp);
         linkmap[i][j]=itmp;
         nx1[itmp]++;
         if(itmp>numlinks) numlinks=itmp;
         fscanf(in_elev_fptr,"%ld",&itmp);
         elevmap[i][j]=(float)itmp;
         }
      }
   fprintf (stdout,"numlinks=%ld\n",numlinks);

    /************************/
   /* Find the longest link */
  /************************/

   maxnodes=0;
   for(j=1;j<=numlinks;j++)
      {
      if(nx1[j]>maxnodes) maxnodes=nx1[j];
      }
   maxnodes+=3;  /* some fudge for memory allocation */ 

    /***********************************************************************/
   /* read in the params & x-section properties for each link from a file */
  /***********************************************************************/

   fscanf(in_xsect_fptr,"%f",&sout);
   fscanf(in_xsect_fptr,"%f",&alpha);
   fscanf(in_xsect_fptr,"%f",&beta);
   fscanf(in_xsect_fptr,"%f",&theta);
   fscanf(in_xsect_fptr,"%f",&dx);
   fscanf(in_xsect_fptr,"%f",&qmin);

   if(dx!=deltax) 
     fprintf (stdout,"Warning, channel node length different from raster grid size.\n");

   for(i=1;i<=numlinks;i++)
      {
      fscanf(in_xsect_fptr,"%ld %ld %f %f %f %f",&itmp,&ltmp,&mtmp,&btmp,&ctmp,&stmp);
      ltype[i]=ltmp;
      mann[i]=mtmp;
      bot[i]=btmp;
      chan_dep[i]=ctmp;
      sslope[i]=stmp;
      }

    /***********************************************************************/
   /* find the ends of links, these are 1st order streams or the outlet   */
  /***********************************************************************/
   for(i=1;i<=rows;i++)
      {
      for(j=1;j<=cols;j++)
         {
         if(linkmap[i][j]!=0)
            {
            /* find the end (beginning) channel grids */
            zeros=0;
            if(linkmap[i][j+1]==0) zeros++;
            if(linkmap[i][j-1]==0) zeros++;
            if(linkmap[i+1][j]==0) zeros++;
            if(linkmap[i-1][j]==0) zeros++;
            if(zeros==4)
               {
               fprintf (stdout,"Problem, one node channel at row=%ld col=%ld\n",i,j);
               fprintf (stdout,"      %4ld   \n",linkmap[i-1][j]);
               fprintf (stdout," %4ld %4ld %4ld\n",linkmap[i][j-1],linkmap[i][j],linkmap[i][j+1]);
               fprintf (stdout,"      %4ld   \n",linkmap[i+1][j]);
              
               exit(0);
               }
            if(zeros==3) 
               {
               endcol[linkmap[i][j]]=j;  /* col coord. of terminal link */
               endrow[linkmap[i][j]]=i;  /* row coord. of terminal link */
               }
            }
         }
      }

   fprintf (stdout,"outlet is located at col %ld  row %ld\n",
                               endcol[numlinks],endrow[numlinks]);

   /***************************************************************/
   /* since we know the outlet and nx1, let's build the node map  */
   /* BEGINNING OF NODENUMBER.C   this program searches the net-  */
   /* work to identify and number all nodes.                      */
   /***************************************************************/

   for(count=numlinks;count>=1;count--)
      {
      fprintf (stdout,"working on stream %d, nx1[%d]=%d \n",count,count,nx1[count]);
      work=1;

      occur=0;
      prevdir=0;
      nodenum=nx1[count];
      rowdex=endrow[count];
      coldex=endcol[count];
      nodemap[endrow[count]][endcol[count]]=nx1[count];
 
      /* if(count==numstreams) nodenum=nx1[count]-1; */
      nodenum=nx1[count]-1;
      while(work==1)
         {
         /* search for channel of desired link number, excluding previous one */
         if((linkmap[rowdex][coldex-1]==count) &&
             (prevdir!=1))
            {
            coldex--;
            nodemap[rowdex][coldex]=nodenum;
            nodenum--;
            prevdir=3;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((linkmap[rowdex][coldex+1]==count) &&
             (prevdir!=3))
            {
            coldex++;
            nodemap[rowdex][coldex]=nodenum;
            nodenum--;
            prevdir=1;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((linkmap[rowdex-1][coldex]==count) &&
             (prevdir!=2))
            {
            rowdex--;
            nodemap[rowdex][coldex]=nodenum;
            nodenum--;
            prevdir=4;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         if((linkmap[rowdex+1][coldex]==count) &&
             (prevdir!=4))
            {
            rowdex++;
            nodemap[rowdex][coldex]=nodenum;
            nodenum--;
            prevdir=2;
            fprintf (stdout,"found node %3d in link %3d\n",nodenum,count);
            continue;
            }
         else
            {
            lnow=linkmap[rowdex][coldex-1];
            if((lnow!=count)&&(lnow!=0))
               {
               endcol[lnow]=coldex-1;
               endrow[lnow]=rowdex;
               }
            lnow=linkmap[rowdex][coldex+1];
            if((lnow!=count)&&(lnow!=0))
               {
               endcol[lnow]=coldex+1;
               endrow[lnow]=rowdex;
               }
            lnow=linkmap[rowdex-1][coldex];
            if((lnow!=count)&&(lnow!=0))
               {
               endcol[lnow]=coldex;
               endrow[lnow]=rowdex-1;
               }
            lnow=linkmap[rowdex+1][coldex];
            if((lnow!=count)&&(lnow!=0))
               {
               endcol[lnow]=coldex;
               endrow[lnow]=rowdex+1;
               }             
            work=0;  /* this is the end of the current channel */
            startrow[count]=rowdex;
            startcol[count]=coldex;
            }
         }
      }
   /******************************/
  /* write the node map         */
 /******************************/
   for(i=1;i<=rows;i++)
      {
      for(j=1;j<=cols;j++)
         {
         fprintf(out_node_fptr,"%3ld ",nodemap[i][j]);
         }
      fprintf(out_node_fptr,"\n");
      }
   fprintf (stdout,"done writing node map\n");
  
   /***************************************************************/
   /* The next section of code searches the network to determine  */
   /* the network connectivity information required at the        */
   /* beginning of the channel input data file.                   */
   /***************************************************************/

   /* Find the location of the first node in each link */
   for(i=1;i<=rows;i++)
      {
      for(j=1;j<=cols;j++)
         {
         if(linkmap[i][j]!=0)
            {
            if(nodemap[i][j]==1)
               {
               startrow[linkmap[i][j]]=i;
               startcol[linkmap[i][j]]=j;
               fprintf (stdout,"link %ld starts at row %ld col %ld\n",
                       linkmap[i][j],i,j);
               }
            bel[nodemap[i][j]][linkmap[i][j]]=elevmap[i][j];
            orbel[nodemap[i][j]][linkmap[i][j]]=elevmap[i][j];
            }
         }
      }


   /* search for the dependencies */
   for(k=1;k<=numlinks;k++)
      {
      j=startcol[k];
      i=startrow[k];
      zeros=0;
      right=linkmap[i][j+1];
      left=linkmap[i][j-1];
      bottom=linkmap[i+1][j];
      top=linkmap[i-1][j];

      if((right!=0)&&  (nodemap[i][j+1]==nx1[right])    ) zeros++;
      if((left!=0)&&   (nodemap[i][j-1]==nx1[left])     ) zeros++;
      if((bottom!=0)&& (nodemap[i+1][j]==nx1[bottom])   ) zeros++;
      if((top!=0)&&    (nodemap[i-1][j]==nx1[top])      ) zeros++;


      if(zeros==4)
         {
         fprintf (stdout,"Problem, one node channel at row=%ld col=%ld\n",i,j);
         fprintf (stdout,"      %4ld   \n",linkmap[i-1][j]);
         fprintf (stdout," %4ld %4ld %4ld\n",linkmap[i][j-1],linkmap[i][j],linkmap[i][j+1]);
         fprintf (stdout,"      %4ld   \n",linkmap[i+1][j]);
         exit(0);
         }

      if(zeros==3)
         {
         /* this is the beginning of a first order link */
         fprintf (stdout,"first order link %ld found\n",k);
         continue;
         }
      /* if it makes it to here, then this is a channel junction. */
      sortlist[1]=top;
      sortlist[2]=bottom;
      sortlist[3]=left;
      sortlist[4]=right;
 
      orig[1]=top;
      orig[2]=bottom;
      orig[3]=left;
      orig[4]=right;

      big=0;
      small=1000;
      medium=0;
      for(;;)
         {
         for(jdex=2;jdex<=4;jdex++)
            {
            itmp=sortlist[jdex];
            for(idex=jdex-1;idex>=1;idex--)
               {
               if(sortlist[idex]<=itmp) break;
               sortlist[idex+1]=sortlist[idex];
               }
            sortlist[idex+1]=itmp;
            }
         break;
         }
      tiny=sortlist[1];
      small=sortlist[2];
      medium=sortlist[3];
      big=sortlist[4];

      /* this is not a first order link */
      backdep[medium]=big;
      backdep[small]=big;
      depend[big][1]=medium;
      depend[big][2]=small;
   
      /* fprintf (stdout,"k=%ld\n",k);
      fprintf (stdout,"orig[1]= %6ld   big=    %ld\n",orig[1],big);
      fprintf (stdout,"orig[2]= %6ld   medium= %ld\n",orig[2],medium);
      fprintf (stdout,"orig[3]= %6ld   small=  %ld\n",orig[3],small);
      fprintf (stdout,"orig[4]= %6ld   tiny=   %ld\n",orig[4],tiny);
      getchar();  */
      }

   for(k=1;k<=numlinks;k++)
      {
      ndep[k]=0;
      if(depend[k][1]!=0) ndep[k]++;
      if(depend[k][2]!=0) ndep[k]++;
      }
       
   for(k=1;k<=numlinks;k++)
      {
      fprintf (stdout,"%4ld %4ld %4ld %4ld %4ld\n",k,ndep[k],depend[k][1],depend[k][2],
                                     backdep[k]);
      }
      
      
   /* add on to the bel array for all links except the most downstream */
   for(j=1;j<=numlinks;j++)
      {
      if(j<numlinks)
         {
         nx1[j]+=1;
         bel[nx1[j]][j]=bel[1][backdep[j]];
         orbel[nx1[j]][j]=bel[1][backdep[j]];
         }
      }

   /*************************************************************************/
   /*  Begin channel talweg profile filtering - NEVER ADD!!!                */
   /*************************************************************************/
   /*** THIS SECTION OF CODE REMOVES LARGE REGIONS OF ADVERSE SLOPE BY
    *** CUTTING DOWN HILLS WHICH HAVE TWO LOWER UPSTREAM NODES, IN A BIG
    *** WAY.
    ***/

   for(j=1;j<=numlinks;j++) /* do each link individually */
      {
      for(i=(nx1[j]-1);i>3;i--)  /* check for obvious high spots in the profile */
         {
         if((bel[i-2][j]<bel[i][j])&&(bel[i-1][j]<bel[i][j]))  /* probably a high spot */
            {
            if(bel[i-2][j]>bel[i-1][j]) /* downward slope at upstream two nodes */
               {
               uslope=(bel[i-2][j]-bel[i-1][j])/deltax;
               uslope*=0.5;  /* use half of upstream slope as a new est. */
               bel[i][j]=bel[i-1][j]-uslope*deltax; 
               continue;
               }
            if(bel[i-2][j]<bel[i-1][j]) /* adverse slope at upstream two nodes */
               {
               /*  is i-3 > i-2 (normal slope)??? */
               if(bel[i-3][j]>bel[i-2][j])
                 {                    /* base guess off bel[i-2] */
                 uslope=1.0*(bel[i-3][j]-bel[i-2][j])/deltax;
                 bel[i][j]=bel[i-2][j]-uslope*(2.0*deltax);
                 continue;
                 }
               /* if not do something, bel[i] must be less than bel[i-3] */
               else bel[i][j]=bel[i-3][j]-0.25*deltax/100.0;
               }
            }
         }

      /*** THIS SECTION OF CODE REMOVES SOLITARY HIGH SPOTS IN THE PROFILE BY
       *** ITERATING
       ***/

/***
 *    it=0;
 *    for(;;)
 *       {
 *       it++;
 *       fprintf (stdout,"doing iteration #%d\n",it);
 *       workflag=0;
 *       for(i=2;i<=nx1[j]-1;i++)
 *          {
 *          if(bel[i][j]>bel[i-1][j])
 *             {
 *             bel[i][j]=0.5*(bel[i+1][j]+bel[i-1][j]);
 *             workflag++;
 *             }
 *          }
 *       if(it==5000) workflag=0;
 *       if(workflag==0) break;
 *       }
 ***/

      /*** NOW SMOOTH THE PROFILE BY A LITTLE THREE POINT MOVING AVERAGING
       *** WITH WEIGHTS DETERMINED BY THE DISTANCES TO ADJACENT POINTS
       ***/
      for(k=1;k<=10;k++)
         {
         for(i=2;i<nx1[j];i++)
            {
            newel=(bel[i-1][j]+bel[i][j]+bel[i+1][j])/3.0; 
            if(newel<=bel[i][j]) bel[i][j]=newel;
            /* bel[i][j]=0.5*(bel[i-1][j]+bel[i+1][j]); */
            }
         }
      }


   /*************************************************************************/
   /*  End channel talweg profile filtering                                 */
   /*************************************************************************/

    /***************************************/
   /* FINALLY write the channel data file */
  /***************************************/


   fprintf(out_fptr,"%4.2f\n",gr);
   fprintf(out_fptr,"1.0  1.0  1.0\n");
   fprintf(out_fptr,"%7.2f\n",(float)(east-west)/(float)cols);
   fprintf(out_fptr,"30.0  24000.0\n");
   fprintf(out_fptr,"%5.2f\n",qmin);
   fprintf(out_fptr,"%5ld\n",numlinks);
   fprintf(out_fptr,"%5ld\n",maxnodes);


   for(j=1;j<=numlinks;j++)
      {
      fprintf(out_fptr,"%4ld %4ld %4ld %4ld %4ld %4ld\n",j,ltype[j],ndep[j],
                                     depend[j][1],depend[j][2],backdep[j]);
      }


   for(j=1;j<=numlinks;j++)
      {
      fprintf(out_fptr,"%4ld %4ld\n",j,nx1[j]);
      for(k=1;k<nx1[j];k++)
         {
         fprintf(out_fptr,"%6.3f %7.3f %7.3f %7.3f %9.3f\n",mann[j],bot[j],chan_dep[j],
                                                            sslope[j],bel[k][j]);
         }
      if(j<numlinks) /* add the node for the junction */
         {
         k=backdep[j];
         if(ndep[k]==1)  /* this is a single-channel jct. so x-sect should be the same */
            {
            fprintf(out_fptr,"%6.3f %7.3f %7.3f %7.3f %9.3f\n",mann[k],bot[k],chan_dep[k],
                                                            sslope[k],bel[nx1[j]][j]);
            }
         else
            {
            fprintf(out_fptr,"%6.3f %7.3f %7.3f %7.3f %9.3f\n",mann[j],bot[j],chan_dep[j],
                                                               sslope[j],bel[nx1[j]][j]);
            }
         }
      else
         {
         fprintf(out_fptr,"%6.3f %7.3f %7.3f %7.3f %9.3f\n",mann[j],bot[j],chan_dep[j],
                                                            sslope[j],bel[nx1[j]][j]);
         }
      }

   fprintf(out_fptr,"\n");

   it=0;
   for(j=1;j<=numlinks;j++)
      {
      for(i=1;i<=nx1[j];i++)
         {
         fprintf(out_diff_fptr,"%3ld %f\n",it,orbel[i][j]-bel[i][j]);
         it++;
         }
      }
     
   return(0);
}
