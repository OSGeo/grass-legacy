#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
XgdUpdateSiteStd(sinfo, icontype, size, width, color)
     XgdSiteInfo *sinfo;
     int icontype;
     int size;
     int width;
     Pixel color;
#else
XgdUpdateSiteStd(XgdSiteInfo *sinfo, int icontype, int size,
		 int width, Pixel color)
#endif
{
     XgdSetStdSiteIconType(sinfo, icontype);
     XgdSetStdSiteSize(sinfo, size);
     XgdSetStdSiteWidth(sinfo, width);
     XgdSetStdSiteColor(sinfo, color);
}

void 
#ifdef _NO_PROTO
XgdSetStdSiteIconType(sinfo, icontype)
XgdSiteInfo *sinfo;
int icontype;
#else
XgdSetStdSiteIconType(XgdSiteInfo *sinfo, int icontype)
#endif
{
    sinfo->Site.def.icontype = icontype;
}

void 
#ifdef _NO_PROTO
XgdSetStdSiteSize(sinfo, size)
XgdSiteInfo *sinfo;
int size;
#else
XgdSetStdSiteSize(XgdSiteInfo *sinfo, int size)
#endif
{
    sinfo->Site.def.size = size;
}

void
#ifdef _NO_PROTO
XgdSetStdSiteWidth(sinfo, width)
XgdSiteInfo *sinfo;
int width;
#else
XgdSetStdSiteWidth(XgdSiteInfo *sinfo, int width)
#endif
{
    sinfo->Site.def.width = width;
}

void
#ifdef _NO_PROTO
XgdSetStdSiteColor(sinfo, color)
XgdSiteInfo *sinfo;
Pixel color;
#else
XgdSetStdSiteColor(XgdSiteInfo *sinfo, Pixel color)
#endif
{
    sinfo->Site.def.color = color;
}

