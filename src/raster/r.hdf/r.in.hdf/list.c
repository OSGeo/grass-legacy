
/*
Written by Bill Brown, August 1994
*/


#include "df.h"
#include "hdf.h"


#define LISTSIZE 60
#define MAXLEN 80

/* if you have hdf version 3.3, you can uncomment this
#define HDF_LIB3_3
*/

list_hdf_objects(fn, tag)
char *fn;
uint16 tag;
{
int i, nlabels, num, startpos=1, len, status;
uint16 reflist[LISTSIZE];
char labellist[MAXLEN*LISTSIZE + 1], *label_str, plural;

/* Apparently not used much
int label_l, unit_l, format_l, coord_l;
char *label_s, *unit_s, *format_s, *coord_s;
*/
    show_hdf_fileinfo(fn);
    switch(tag){

	case DFTAG_NDG:

#ifdef HDF_LIB3_3
	    num = DFSDndatasets(fn);
#else
	    num = DFSDnumber(fn);   /* obsolete */
#endif

	    plural = num == 1? '\0': 'S';
	    printf("\n\"%s\" CONTAINS %d NUMERIC DATA GROUP%c:\n\n", 
		fn, num, plural);
	    break;

	case DFTAG_RIG:

	    num = DFR8nimages(fn);
	    plural = num == 1? '\0': 'S';
	    printf("\n\"%s\" CONTAINS %d 8-BIT IMAGE%c:\n\n", 
		fn, num, plural);
	    break;

    }

    nlabels = DFANlablist( fn, tag, reflist, labellist, LISTSIZE,
	      MAXLEN, startpos);

    if(nlabels == FAIL){
	fprintf(stderr,"Unable to read label information for \"%s\"\n", fn);
	return(-1);
    }


    for(i=0; i<nlabels; i++){

	printf("Reference Number: %d\tLabel: %s\n", reflist[i],
	      labellist+(i*MAXLEN));

/* Apparently not used much
	DFSDreadref(fn, reflist[i]);
	DFSDgetdatalen(&label_l, &unit_l, &format_l, &coord_l);
	label_s = (char *) HDgetspace((uint32) label_l + 1);
	unit_s = (char *) HDgetspace((uint32) unit_l + 1);
	format_s = (char *) HDgetspace((uint32) format_l + 1);
	coord_s = (char *) HDgetspace((uint32) coord_l + 1);
	DFSDgetdatastrs(label_s, unit_s, format_s, coord_s);
	printf("Data Label: %s\n", label_s);
	printf("Data Unit: %s\n", unit_s);
	printf("Data Format: %s\n", format_s);
	printf("Data Coordinate System: %s\n", coord_s);
	HDfreespace(label_s);
	HDfreespace(unit_s);
	HDfreespace(format_s);
	HDfreespace(coord_s);
*/

	/* read in all of the annotations */
	len = DFANgetdesclen(fn, tag, reflist[i]);
	if(len != FAIL) {
	    label_str = (char *) HDgetspace((uint32) len + 1);
	    status = DFANgetdesc(fn, tag, reflist[i], label_str, len + 1);
	    label_str[len] = '\0';
	    if(status == FAIL)
		printf("Unable to read description\n");
	    else
		printf("Description: %s\n", label_str);
	    HDfreespace(label_str);
	}
	
    }

}

#define MAXDESCLEN 2047
#define FIRST      1
#define NOTFIRST   0

show_hdf_fileinfo (fn)
char *fn;
{
    int32 fileid;
    int   ret,length;
    char  inlabel[MAXLEN+1],indescr[MAXDESCLEN+1];

    printf("\nFILE INFO FOR: \"%s\"", fn);
    /* open file to read file IDs and file description */
    fileid = Hopen(fn, DFACC_READ, 0);

    /*  read all file IDs from file */
    length = DFANgetfidlen(fileid, FIRST);
    while (length >= 0) {
        ret = DFANgetfid(fileid,inlabel, MAXLEN, NOTFIRST);
        printf("\nLabel: %s", inlabel);
        length = DFANgetfidlen(fileid, NOTFIRST);
    }

    /* read description length and description from file */
    length = DFANgetfdslen(fileid, FIRST);
    ret = DFANgetfds(fileid,indescr, MAXDESCLEN, FIRST);
    if(ret >= 0)
	printf("\n\nDescription: \n%s", indescr);

    printf("\n");

    Hclose(fileid);
}





