#include "xgdisp.h"

int Flash = 0;
static int FlashWhich = XGD_ORIGINAL;
static unsigned int red, green, blue;
Pixel Pix;
XtIntervalId timeoutID;

XtTimerCallbackProc
#ifdef _NO_PROTO
HighlightFlash(cli, id)
     Pixel        cli;
     XtIntervalId *id;
#else
HighlightFlash(Pixel cli, XtIntervalId *id)
#endif
{
  XColor col;

  
  if (Flash){
    col.pixel = cli;
    Pix = cli;
    XQueryColor(Global.display, Global.cmap, &col);
    if (FlashWhich == XGD_ORIGINAL){
      XColor hlight;

      hlight.pixel = Global.highlight;
      XQueryColor(Global.display, Global.cmap, &hlight);
      
      red = col.red; green = col.green; blue = col.blue;
      col.red = hlight.red; col.green = hlight.green; col.blue = hlight.blue;
      FlashWhich = XGD_HIGHLIGHT;
    } else {
      FlashWhich = XGD_ORIGINAL;
      col.red = red;
      col.green = green;
      col.blue = blue;
    }
    
    XStoreColor(Global.display, Global.cmap, &col);
    timeoutID = XtAppAddTimeOut(Global.appContext, 1000L, HighlightFlash, cli);
  }
}

void
#ifdef _NO_PROTO
StartFlash()
#else
StartFlash(void)
#endif
{
  Flash = 1;
}

void
#ifdef _NO_PROTO
EndFlash()
#else
EndFlash(void)
#endif
{
  XColor col;

  if (!Flash)
    return;

  XtRemoveTimeOut(timeoutID);
  col.pixel = Pix;
  XQueryColor(Global.display, Global.cmap, &col);
  col.red = red;
  col.green = green;
  col.blue = blue;
  XStoreColor(Global.display, Global.cmap, &col);
  Flash = 0;
  FlashWhich = XGD_ORIGINAL;
}

