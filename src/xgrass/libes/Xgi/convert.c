#include <Xm/XmP.h>
#include "Pixel.h"
#include "color.h"

int xmUseVersion;

#ifdef _NO_PROTO
static void _XgCvtStringToScaleTypeMask ();
#else
static void _XgCvtStringToScaleTypeMask ( XrmValue * args, Cardinal   num_args,
XrmValue * from_val, XrmValue * to_val);
#endif

XgRegisterScaleTypeConverter()
{
   static Boolean registered = False;
   static XrmQuark unitQ;

   if (!registered)
   {
      xmUseVersion = XmVersion;

#ifdef notdef
      unitQ = XrmStringToQuark(XmNunitType);

      /*
       * Because the quark value can't be loaded statically,
       * we have to compute it and load it here -- just before
       * installing the converters that use it.
       */

      resIndConvertArgs[0].address_id = (caddr_t) unitQ;
#endif /* notdef */
      XtAddConverter (XmRString, XmRScaleTypeMask,
                       _XgCvtStringToScaleTypeMask, NULL, 0);


   }

   registered = True;

}

/* ARGSUSED */
static void 
#ifdef _NO_PROTO
_XgCvtStringToScaleTypeMask (args, num_args, from_val, to_val)
XrmValue * args;
Cardinal   num_args;
XrmValue * from_val;
XrmValue * to_val;
#else 
_XgCvtStringToScaleTypeMask ( XrmValue * args, Cardinal   num_args,
XrmValue * from_val, XrmValue * to_val)
#endif

{
   char * in_str = (char *) (from_val->addr);
   static unsigned char i;

   to_val->size = sizeof (unsigned char);
   to_val->addr = (caddr_t) &i;

   if (!strcmp (in_str, "rgb"))
      i = XgRGB ;
   else if (!strcmp (in_str, "hsv"))
      i = XgHSV ;
   else if (!strcmp (in_str, "cmy"))
      i = XgCMY ;
   else
   {
      to_val->size = 0;
      to_val->addr = NULL;
      XtStringConversionWarning ((char *)from_val->addr, XmRScaleTypeMask);
   }
}

