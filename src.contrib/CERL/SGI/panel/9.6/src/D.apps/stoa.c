/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <stdio.h>
#include <gl.h>
#include <panel.h>

Boolean
tread(fd, buf, len)
int fd;
char *buf;
int len;
{
  int c;

  if ((c=read(0,buf,len))!=len) {
    (void) printf("<end of input>\n");
    return FALSE;
  }
  return TRUE;
}   

main() 
{
  int c, i;
  Actuator a;
  short actid;
  int msgtype, datasize, delay;
  static union {
    int i;
    float f;
  } buf[9];
  char data[16386];

  for (;;) {
    if (!tread(0,&msgtype,sizeof(msgtype))) exit(1);
    
  redo:
    switch (msgtype) {
    case PNL_MT_MOUSE:
      if (!tread(0,buf,sizeof(buf))) exit(1);
      
      pnl_cp=(Panel *)buf[0].i;
      pnl_ca=(Actuator *)buf[1].i;
      pnl_x=buf[2].f;
      pnl_y=buf[3].f;
      pnl_justup=(Boolean)buf[4].i;
      pnl_justdown=(Boolean)buf[5].i;
      pnl_mousedown=(Boolean)buf[6].i;
      pnl_shiftkey=(Boolean)buf[7].i;
      pnl_controlkey=(Boolean)buf[8].i;
      
      (void) printf("MOUSE: %d %d %f %f %d %d %d\n", 
		    pnl_cp,
		    pnl_ca,
		    pnl_x,
		    pnl_y,
		    pnl_justup,
		    pnl_justdown,
		    pnl_mousedown,
		    pnl_shiftkey,
		    pnl_controlkey);
      break;
    case PNL_MT_STATE:
      tread(0,&actid,sizeof(short));
      tread(0,&a,sizeof(a));
      tread(0,&datasize,sizeof(int));
      tread(0,data,datasize);

      (void) printf("STATE: %d %d %f\n", actid, a.type, a.val);
      break;
    case PNL_MT_DELAY:
      tread(0,&delay,sizeof(delay));

      (void) printf("DELAY: %d\n", delay);
      break;
    default:
      (void) printf("unrecognized message type\n");
      for (i=1;;i++) {
	if (!tread(0,&msgtype,sizeof(msgtype))) exit(1);
	if (  msgtype==PNL_MT_MOUSE
	    ||msgtype==PNL_MT_STATE
	    ||msgtype==PNL_MT_DELAY) break;
      }
      printf("skipped %d non-tokens\n", i);
      goto redo;
    }
  }
}


