/*
Written by Bill Brown, August 1994
*/


#include "df.h"
#include "hdf.h"
#include "gis.h"

/* used these from gis.h:
    MAXEDLINES   (50)
    RECORD_LEN   (80) 
*/

do_history(rast, hdfname, tag, ref)
char *rast, *hdfname;
uint16 tag, ref;
{
struct History  hist;
char *mapset, msg[1024];


    if(rast) {
	mapset = G_find_file("cell",rast,"");
	if(mapset==NULL) {
	    sprintf(msg,"update history: file [%s] not found", rast);
	    G_warning(msg);
	}

	G_short_history(rast, "raster", &hist);

	sprintf(hist.datsrc_1,"hdf file \"%s\"", hdfname);
	get_info_for_hist(hdfname, tag, ref, hist.datsrc_2, 
			    hist.edhist, &(hist.edlinecnt));

	G_write_history (rast, &hist);
    }
}


get_info_for_hist(fn, tag, ref, label, lines, nlineret)
char *fn;
uint16 tag;
uint16 ref;
char *label, lines[MAXEDLINES][RECORD_LEN];
int *nlineret;  /* actual number of lines returned */
{
    int32 fileid;
    int   ret, len, dsdlen, fdlen;
    char buf[RECORD_LEN+1];
    char *fdesc, *dsdesc;

    fileid = Hopen(fn, DFACC_READ, 0);

    /*  read first file ID from file, put in label */
    len = DFANgetfidlen(fileid, 1);
    if (len >= 0) {
        ret = DFANgetfid(fileid, buf, RECORD_LEN, 1);
	if(ret != FAIL){
	    buf[len] = '\0';
	    strcpy(label, buf);
	}
	else
	    strcpy(label, "");
    }
    else
	strcpy(label, "");

    /* read first description length and description from file */
    fdlen = DFANgetfdslen(fileid, 1);
    if (fdlen >= 0) {
	fdesc = (char *) HDgetspace((uint32) fdlen + 1);
	ret = DFANgetfds(fileid, fdesc, fdlen+1, 1);
	fdesc[fdlen] = '\0';
	if(ret == FAIL){
	    HDfreespace(fdesc);
	    fdesc = NULL;
	}
    }
    else
	fdesc = NULL;

    Hclose(fileid);

    /* read in the data set annotation */
    dsdlen = DFANgetdesclen(fn, tag, ref);
    if(dsdlen >= 0) {
	dsdesc = (char *) HDgetspace((uint32) dsdlen + 1);
	ret = DFANgetdesc(fn, tag, ref, dsdesc, dsdlen + 1);
	dsdesc[dsdlen] = '\0';
	if(ret == FAIL){
	    HDfreespace(dsdesc);
	    dsdesc = NULL;
	}
    }
    else
	fdesc = NULL;

    /* chop them up & put in lines 
     * copy up to RECORD_LEN chars or a newline, whichever comes first
     * for each line in the history.
     * I assume here that specific annotation info is more important
     * than general file info!! (annotation is listed first) 
    */	

    {
    int done, numlines=0, i, last;

	strcpy(lines[numlines], "IMPORT COMMAND:"); numlines++;
	strncpy(lines[numlines], G_recreate_command(), RECORD_LEN);
	lines[numlines][RECORD_LEN-1] = '\0';
	numlines++;
	strcpy(lines[numlines], ""); numlines++;

	if(dsdesc){
	    strcpy(lines[numlines], "DATA SET ANNOTATION:"); numlines++;
	    last = done = 0;
	    while(numlines < MAXEDLINES && !done){
		for(i=0; last+i < dsdlen && i < (RECORD_LEN-1) && 
			dsdesc[last+i] != '\0' && dsdesc[last+i] != '\n'; i++){
		    lines[numlines][i] = dsdesc[last+i];
		}
		if(i){
		    lines[numlines][i] = '\0';
		    last += i;
		    numlines++;
		}
		else 
		    last += 1;
		if(last>=dsdlen) done = 1;
	    }
	}
	
	if(fdesc && numlines < (MAXEDLINES-2)){
	    strcpy(lines[numlines], ""); numlines++;
	    strcpy(lines[numlines], "HDF FILE DESCRIPTION:"); numlines++;
	    last = done = 0;
	    while(numlines < MAXEDLINES && !done){
		for(i=0; last+i < fdlen && i < (RECORD_LEN-1) && 
			fdesc[last+i] != '\0' && fdesc[last+i] != '\n'; i++){
		    lines[numlines][i] = fdesc[last+i];
		}
		if(i){
		    lines[numlines][i] = '\0';
		    last += i;
		    numlines++;
		}
		else 
		    last += 1;
		if(last>=fdlen) done = 1;
	    }
	}

	*nlineret = numlines;
    }

    HDfreespace(fdesc);
    HDfreespace(dsdesc);

}


