#include<stdio.h>
#include "define.h"
#include "global.h"

print_ele()   {
    char string[80];

    int adjust;
    int ee;
    int nn;

    float factor;

    FILE *str;

/*
 *  Make sure units are consistent:
 */
    factor = 1.0;
    if(convert_GIS_units) {
	if(units_space == ENGLISH)
	    factor = 1.0/FT_TO_M;
	else if(units_space == METRIC)
	    factor = FT_TO_M;
	else {
	    printf("\n ERROR: Units not specified correctly in print_ele");
	    exit(0);
	}
    }
/*
 *  Prompt the user for the KINEROS file.
 */
    printf("\n");
    printf("\n");
    printf("\n Enter the output file (KINEROS-input) name: \n ");
    scanf("%s",string);

    str = open_file(string,"w");
/*
 *  Adjust the number of elements for gaps in element numbers.
 */
    adjust = 0;
    for(nn=1;nn<=num_ele;nn++) {
	if(element[nn].order <= 0) {
	    adjust = adjust + 1;
	    printf("\n Element %d NOT USED",nn);
	}
    }
    printf("\n Gaps from the absence of %d elements.",adjust);
    printf("\n Number of elements was %d and is now %d.",num_ele,num_ele-adjust);


    fprintf(str,  "#**** KINEROS Parameter Input File*****");
    fprintf(str,"\n#");
    fprintf(str,"\n#**********************************");
    fprintf(str,"\n#***********  S Y S T E M  ********");
    fprintf(str,"\n#**********************************");
    fprintf(str,"\n#*    NELE     NRES    NPART     CLEN     TFIN     DELT    THETA     TEMP");
    fprintf(str,"\n       %3d",(num_ele-adjust));
    fprintf(str,"      %3d",nres);
    fprintf(str,"      %3d",npart); 
    fprintf(str," %8.1f",char_length*factor);
    fprintf(str," %8.1f",tfin);
    fprintf(str," %8.1f",dt);
    fprintf(str," %8.3f",theta);
    fprintf(str," %8.1f",temp);
    fprintf(str,"\n#");
    fprintf(str,"\n#***********************************");
    fprintf(str,"\n#********* O P T I O N S ***********");
    fprintf(str,"\n#***********************************");
    fprintf(str,"\n#  NTIME NUNITS  NEROS");
    fprintf(str,"\n      %d      %d      %d",units_time,units_space,erosion);
    fprintf(str,"\n#");
    fprintf(str,"\n#***********************************************");
    fprintf(str,"\n#****   C O M P U T A T I O N   O R D E R   ****");
    fprintf(str,"\n#***********************************************");
    fprintf(str,"\n#  There must be NELE elements in the list. NLOG ");
    fprintf(str,"\n#  must be sequential. ELEMENT NUM. need not be.");
    fprintf(str,"\n#");
    fprintf(str,"\n#  COMP. ORDER    ELEMENT");
    fprintf(str,"\n#     (NLOG)      NUM. (J)");
    fprintf(str,"\n#     ------      --------");

    for(nn=1;nn<=num_ele;nn++) {
        for(ee=1;ee<=num_ele;ee++) {
	    if(element[ee].order == nn) {
                fprintf(str,"\n     %3d           %3d", nn, ee);
                printf("\n num_ele = %3d    order=%3d  element=%3d",num_ele, nn, ee);
	    }
	}
    }

    fprintf(str,"\n#");
    fprintf(str,"\n#***********************************************");
    fprintf(str,"\n#******   E L E M E N T - W I S E   I N F O  ***");
    fprintf(str,"\n#***********************************************");
    fprintf(str,"\n#    There must be NELE sets of the ELEMENT-WISE  prompts and data ");
    fprintf(str,"\n#    records; duplicate records from * to * for each element.  The ");
    fprintf(str,"\n#    elements may be entered in any order.");

    for(ee=1;ee<=num_ele;ee++) { 
	if(element[ee].order > 0) {
            fprintf(str,"\n#* ");
            fprintf(str,"\n# J      NU      NR       NL      NC1      NC2    NCASE   NPRINT     NPNT      NRP");
            fprintf(str,"\n");
            fprintf(str,"%3d",ee);
            fprintf(str,"    %3d",element[ee].plane[0]);
            fprintf(str,"      %3d",element[ee].plane[1]);
            fprintf(str,"      %3d",element[ee].plane[2]);
            fprintf(str,"      %3d",element[ee].trib[0]);
            fprintf(str,"      %3d",element[ee].trib[1]);
            fprintf(str,"       %1d ",element[ee].chan_shape);
            fprintf(str,"       %1d ",element[ee].print);
            fprintf(str,"       %1d ",element[ee].pond);
            fprintf(str,"       %1d ",element[ee].print_rain);
            fprintf(str,"\n#---------------------------------------------------------------------------------");
            fprintf(str,"\n#       XL        W        S       ZR       ZL       BW     DIAM       R1       R2");
            fprintf(str,"\n ");
            fprintf(str," %8.1f",element[ee].length*factor);
            fprintf(str," %8.1f",element[ee].width*factor);
            fprintf(str," %8.5f",element[ee].slope);
            fprintf(str," %8.5f",element[ee].bank[0]);
            fprintf(str," %8.5f",element[ee].bank[1]);
            fprintf(str," %8.1f",element[ee].stream_width*factor);
            fprintf(str," %8.1f",element[ee].diameter*factor);
            fprintf(str," %8.4f",element[ee].mannings_n);
            fprintf(str," %8.4f",element[ee].laminar_k);
            fprintf(str,"\n#---------------------------------------------------------------------------------");
            fprintf(str,"\n#     FMIN        G      POR       SI     SMAX      ROC     RECS    DINTR");
            fprintf(str,"\n ");
            fprintf(str," %8.4f",element[ee].fmin);
            fprintf(str," %8.4f",element[ee].G);
            fprintf(str," %8.4f",element[ee].porosity);
            fprintf(str," %8.4f",element[ee].Sint);
            fprintf(str," %8.4f",element[ee].Smax);
            fprintf(str," %8.4f",element[ee].Rock);
            fprintf(str," %8.4f",element[ee].recess);
            fprintf(str," %8.4f",element[ee].intercept);
	
            fprintf(str,"\n#---------------------------------------------------------------------------------");
            fprintf(str,"\n#      LAW       CF       CG       CH    CO-CS      D50     RHOS     PAVE   SIGMAS");
    
            fprintf(str,"\n ");
            fprintf(str,"        %1d",element[ee].res_law);
            fprintf(str," %8.4f",element[ee].Cf);
            fprintf(str," %8.4f",element[ee].Cg);
            fprintf(str," %8.4f",element[ee].Ch);
            fprintf(str," %8.4f",element[ee].Co);
            fprintf(str," %8.4f",element[ee].d50);
            fprintf(str," %8.4f",element[ee].rho_s);
            fprintf(str," %8.4f",element[ee].pave);
            fprintf(str," %8.4f",element[ee].sigma_s);
            fprintf(str,"\n#*");
        }
    }
    fclose(str);
}
