/*	
*		shape disassemble routines
*		Alex Shevlakov, sixote@yahoo.com, 03/2000
 * Revision 1.1.1.1  2000/03/14 12:35:59  zapoved
 * 
*/
static char rcsid[] = 
  "$Id$";
  
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>

char*	my_readline(FILE *fp,int rev) {

int k;
char buf1[128]="";
char buf2[128]="";
char bufc[256]="";
char *ptr1, *ptr2;
char *ptrc;

	if ((k=fscanf(fp,"%[ 0-9.A-Z]",&buf1))) {

		ptr1=buf1;
		if (!feof(fp)) 
			fseek(fp,1,SEEK_CUR);
			if ((k=fscanf(fp,"%[ 0-9.A-Z]",&buf2))) {
				ptr2=buf2;
				if (!feof(fp)) 
					fseek(fp,-strlen(buf2),SEEK_CUR);
			if (rev)
				snprintf(bufc,256,"%s\n%s\n",ptr2,ptr1);
			else
				snprintf(bufc,256,"%s\n%s\n",ptr1,ptr2);
				
			ptrc=bufc;
			
			return ptrc;
			}
		return NULL;

	}
	else
		return NULL;
		
		
	
}

int leave_single_lines( FILE *ifp, FILE *ofp) {

char *fst, *secn, *p;
char tmp_buf[256]="";
char tbuf1[256]="";
char tbuf2[256]="";
char hbuf1[128]="";
char hbuf2[128]="";
char gbuf[256]="";
int k, still, match;
long sv_pos = 0;

char	nch[8] = "";
static char keep_ch[8] = "";
static int num;


char *to_where;
long fn_pos = 0;

	fst="init";
	secn="init";
	

		match = 0;
		

		fseek(ifp,0,SEEK_END);
		fn_pos=ftell(ifp);
		fseek(ifp,0,SEEK_SET);
				
		while(!feof(ifp) && (fst)) { 
			
			still=0;
			
			fst = my_readline(ifp,1);
			snprintf(tbuf1,256,fst);
			
			fseek(ifp,sv_pos,SEEK_SET);
			fst = my_readline(ifp,0);
			snprintf(tmp_buf,256,fst);

			sscanf(tmp_buf,"%6c",&nch);
			if (nch[5] == '\n') {
				nch[5] = '\0';
				gbuf[0] = '\0';
				sprintf(gbuf,"%s\n",nch);
				still++;
				num = atoi(nch);
				strcpy(keep_ch,nch);
				fprintf(stderr,"    Checking arc %d\n",num);
			}
			
			fst=tmp_buf;
			
			while (*fst != '\n') fst++;
			++fst;
			snprintf(hbuf2,256,fst);
			
			sscanf(tmp_buf,"%[ 0-9.A-Z]",&hbuf1);
			
			if (!strncmp(hbuf2,"END",3)) { 		
				still++;
				if (!strncmp(hbuf1,"END",3)) {		/*exiting*/
					
					fprintf(ofp,"END\n");		
					break;
					
				}
			}
			
			if (!strncmp(hbuf1,"END",3)) { 		
				still++;
			}
							
			sv_pos=ftell(ifp);
			
		if (!still) {

/*	this stuff is SLOW
		
			while(!feof(ifp) && (secn)) {
				
			   
				secn = my_readline(ifp,0);
				if (secn) 
					snprintf(tbuf2,256,secn);			
					
				
				if (!(k=strncmp(tbuf1,tbuf2,strlen(tbuf2)))) {
				
					if (!strlen(gbuf)) 
						fprintf(ofp,"END\n");
					gbuf[0] = '\0';
					sprintf(gbuf,"%s\n",keep_ch);
					match++;
					break;	
			  	}
				else 
					match = 0;
			  	
			}			
*/

			
		   if (fn_pos-sv_pos) {

			to_where = (char*)malloc((fn_pos-sv_pos) * sizeof(char));
			fread(to_where,1,fn_pos-sv_pos,ifp);

			if (strstr(to_where,tbuf1)) {

				if (!strlen(gbuf)) 
						fprintf(ofp,"END\n");
					gbuf[0] = '\0';
					sprintf(gbuf,"%s\n",keep_ch);
					match++;
				}
				else 
					match = 0;
			free (to_where);
		   }		
		}
			
			if ( strlen(gbuf) && !match) {
				if (!still) {	
					fprintf(ofp,"%s",gbuf);
					fprintf(ofp,"%s",tmp_buf);
					gbuf[0] = '\0';
					match = 0;
				}
			}
			else 
				if (!match && strncmp(hbuf1,"END",3)) fprintf(ofp,"%s",hbuf2);
				
			fseek(ifp,sv_pos,SEEK_SET);
		}

	return 0;
}


int split_arcs_to_nodes( FILE *ifp, FILE *ofp) {

char *fst, *secn, *p;
char tmp_buf[256]="";
char tbuf1[256]="";
char tbuf2[256]="";
char hbuf1[128]="";
char hbuf2[128]="";
char gbuf[256]="";
int k, still;
long sv_pos = 0;

char	nch[8] = "";
static char keep_ch[8] = "";
static int num;
static int third_line;

char *to_where;

	fst="init";
	secn="init";
	

		third_line = 0;
		

		while(!feof(ifp) && (fst)) { 
			
			still=0;
			
			fst = my_readline(ifp,1);
			snprintf(tbuf1,256,fst);
			
			fseek(ifp,sv_pos,SEEK_SET);
			fst = my_readline(ifp,0);
			snprintf(tmp_buf,256,fst);
	
			sscanf(tmp_buf,"%6c",&nch);
			if (nch[5] == '\n') {
				nch[5] = '\0';
				gbuf[0] = '\0';
				sprintf(gbuf,"%s\n",nch);
				still++;
				num = atoi(nch);
				strcpy(keep_ch,nch);
				fprintf(stderr,"    Splitting arc %d\n",num);
			}
			

			fst=tmp_buf;
			
			while (*fst != '\n') fst++;
			++fst;
			snprintf(hbuf2,256,fst);
			
			sscanf(tmp_buf,"%[ 0-9.A-Z]",&hbuf1);
			
			if (!strncmp(hbuf2,"END",3)) { 		
				still++;
				if (!strncmp(hbuf1,"END",3)) {		/*exiting*/
					
					fprintf(ofp,"END\n");		
					break;
					
				}
			}
			
			if (!strncmp(hbuf1,"END",3)) { 		
				still++;
			}
							
			sv_pos=ftell(ifp);
			
		if (!still) {
			
			rewind(ifp);
/*	this stuff is SLOW
			while((sv_pos > ftell(ifp)+33) && (secn)) {
				
			   	
				secn = my_readline(ifp,0);
				if (secn) 
					snprintf(tbuf2,256,secn);			
					
				
				if (!(k=strncmp(tbuf2,hbuf1,strlen(hbuf1)))) {				

					if (!third_line) fprintf(ofp,"END\n");
					gbuf[0] = '\0';
					sprintf(gbuf,"%s\n",keep_ch);
					match++;
					break;	
			  	}
				else 
					match = 0;
			  	
			}			
*/


			to_where = (char*)malloc(sv_pos * sizeof(char));
			fread(to_where,1,sv_pos-(strlen(hbuf1))+1,ifp);

			if (strstr(to_where,hbuf1)) {

				if (!third_line) 
					fprintf(ofp,"END\n");
					gbuf[0] = '\0';
					sprintf(gbuf,"%s\n",keep_ch);
			}

			free (to_where);
		
		}
			
			if ( strlen(gbuf)) {
				if (!still) {
				
					fprintf(ofp,"%s",gbuf);
					fprintf(ofp,"%s",tmp_buf);
					gbuf[0] = '\0';
				}
			}
			else {

				if (strncmp(hbuf1,"END",3)) fprintf(ofp,"%s",hbuf2);
			}
				
			fseek(ifp,sv_pos,SEEK_SET);
			third_line = still;
		}

	return 0;
}

