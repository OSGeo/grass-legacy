
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

/************ Function: setxhair ******************************
 *
 * This function sets the initial position of the cross-hairs and
 * saves the pixels under them.
 */

setxhair(x, y)
int x, y;
{
    register int i, index;
    
    /* set mouse coordinates and compute endpoints */
    mouse.float_x = mouse.old_x = x;
    mouse.float_y = mouse.old_y = y;
    mse_x2 = mouse.float_x + 12;
    mse_x1 = mse_x2 - 25;
    if (mse_x1 < 0) mse_x1 = 0;
    if (mse_x2 > H_RES - 1) mse_x2 = H_RES - 1;
    mse_y2 = mouse.float_y + 12;
    mse_y1 = mse_y2 - 25;
    if (mse_y1 < 0) mse_y1 = 0;
    if (mse_y2 > V_RES - 1) mse_y2 = V_RES - 1;
    
    /* do horizontal cross-hair */
    video.total_offset = H_RES * mouse.float_y + mse_x1;
    index = 0;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++, index++)
    {	if (i == mouse.float_x) continue;
	CHECK_SEG();
	hor_pix[index] = *(graphics_base + video.segment.offset);
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
    
    /* do vertical cross-hair */
    video.total_offset = H_RES * mse_y1 + mouse.float_x;
    index = 0;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES, index++)
    {	if (i == mouse.float_y) continue;
	CHECK_SEG();
	ver_pix[index] = *(graphics_base + video.segment.offset);
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
}



/***************** Function: xhair *****************************
 *
 * This function erases the cross-hair at the previous location and, if
 * flag > 0, moves the cross_hair to x,y.
 */

xhair(x, y, flag)
int x, y, flag;
{
    register int i, index;
    
    /* erase horizontal cross-hair */
    video.total_offset = H_RES * mouse.old_y + mse_x1;
    index = 0;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++, index++)
    {	if (i == mouse.old_x) continue;
	CHECK_SEG();
	*(graphics_base + video.segment.offset) = hor_pix[index];
    }
    
    /* erase vertical cross-hair */
    video.total_offset = H_RES * mse_y1 + mouse.old_x;
    index = 0;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES, index++)
    {	if (i == mouse.old_y) continue;
	CHECK_SEG();
	*(graphics_base + video.segment.offset) = ver_pix[index];
    }

  if (flag)
  {
    
    /* save the coordinates and compute new endpoints */
    mouse.old_x = mouse.float_x = x;
    mouse.old_y = mouse.float_y = y;
    mse_x2 = mouse.float_x + 12;
    mse_x1 = mse_x2 - 25;
    if (mse_x1 < 0) mse_x1 = 0;
    if (mse_x2 > H_RES - 1) mse_x2 = H_RES - 1;
    mse_y2 = mouse.float_y + 12;
    mse_y1 = mse_y2 - 25;
    if (mse_y1 < 0) mse_y1 = 0;
    if (mse_y2 > V_RES - 1) mse_y2 = V_RES - 1;
    
    /* do the new horizontal cross-hair */
    video.total_offset = H_RES * mouse.float_y + mse_x1;
    index = 0;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++, index++)
    {	if (i == mouse.float_x) continue;
	CHECK_SEG();
	hor_pix[index] = *(graphics_base + video.segment.offset);
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
    
    /* do the new vertical cross-hair */
    video.total_offset = H_RES * mse_y1 + mouse.float_x;
    index = 0;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES, index++)
    {	if (i == mouse.float_y) continue;
	CHECK_SEG();
	ver_pix[index] = *(graphics_base + video.segment.offset);
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
  }
}


/*************** Function: setrbox ****************************
 *
 * This function sets the initial position of the rubber box and
 * saves the pixels under it.
 */

setrbox(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    register int i;
    register unsigned char *ptr;
    
    /* set mouse coordinates and compute endpoints */
    if (x2 >= x1)
    {	mse_x1 = x1;
    	mse_x2 = x2;
    }
    else
    {	mse_x1 = x2;
    	mse_x2 = x1;
    }
    if (y2 >= y1)
    {	mse_y1 = y1;
    	mse_y2 = y2;
    }
    else
    {	mse_y1 = y2;
    	mse_y2 = y1;
    }
    mouse.fixed_x = x1;
    mouse.fixed_y = y1;
    mouse.float_x = x2;
    mouse.float_y = y2;
    
    /* save top of box */
    mse_voff = H_RES * mse_y1 + mse_x1;
    video.total_offset = mse_voff;
    ptr = box_top;
    for (i = mse_x1; ; i++, video.total_offset++)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
	if (i == mse_x2) break;
    }
    
    /* save right side of box */
    ptr = box_rt;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
    }
    
    /* save left side of box */
    video.total_offset = mse_voff;
    ptr = box_lt;
    for (i = mse_y1; ; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
	if (i == mse_y2) break;
    }
    
    /* save bottom of box */
    ptr = box_bot;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
    }
    
    /* draw top of box */
    video.total_offset = mse_voff;
    for (i = mse_x1; ; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
	if (i == mse_x2) break;
    }
    
    /* draw right side of box */
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
    
    /* draw left side of box */
    video.total_offset = mse_voff;
    for (i = mse_y1; ; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
	if (i == mse_y2) break;
    }
    
    /* draw bottom of box */
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
    
}


/**************** Function: rubbox ***************************
 *
 * This function erases the rubber box at the previous location and,
 * if flag > 0, moves the floating corner to x,y.
 */

rubbox(x, y, flag)
int x, y, flag;
{
    register int i;
    unsigned char *ptr;
    
    /* erase top of old box */
    video.total_offset = mse_voff;
    ptr = box_top;
    for (i = mse_x1; ; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = *ptr++;
	if (i == mse_x2) break;
    }
    
    /* erase right side of old box */
    ptr = box_rt;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = *ptr++;
    }
    
    /* erase left side of old box */
    video.total_offset = mse_voff;
    ptr = box_lt;
    for (i = mse_y1; ; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = *ptr++;
	if (i == mse_y2) break;
    }
    
    /* erase bottom of old box */
    ptr = box_bot;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = *ptr++;
    }
    
  if (flag)
  {

    /* save the coordinates and compute new endpoints */
    mouse.float_x = x;
    mouse.float_y = y;
    if (mouse.float_x < mouse.fixed_x) 
    {	mse_x1 = mouse.float_x;
    	mse_x2 = mouse.fixed_x;
    }
    else 
    {	mse_x1 = mouse.fixed_x;
    	mse_x2 = mouse.float_x;
    }
    if (mouse.float_y < mouse.fixed_y) 
    {	mse_y1 = mouse.float_y;
    	mse_y2 = mouse.fixed_y;
    }
    else 
    {	mse_y1 = mouse.fixed_y;
    	mse_y2 = mouse.float_y;
    }
    
    /* save top of new box */
    mse_voff = H_RES * mse_y1 + mse_x1;
    video.total_offset = mse_voff;
    ptr = box_top;
    for (i = mse_x1; ; i++, video.total_offset++)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
	if (i == mse_x2) break;
    }
    
    /* save right side of new box */
    ptr = box_rt;
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
    }
    
    /* save left side of new box */
    video.total_offset = mse_voff;
    ptr = box_lt;
    for (i = mse_y1; ; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
	if (i == mse_y2) break;
    }
    
    /* save bottom of new box */
    ptr = box_bot;
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++)
    {	CHECK_SEG();
	*ptr++ = *(graphics_base + video.segment.offset);
    }
    
    /* draw top of new box */
    video.total_offset = mse_voff;
    for (i = mse_x1; ; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
	if (i == mse_x2) break;
    }
    
    /* draw right side of new box */
    for (i = mse_y1; i <= mse_y2; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
    
    /* draw left side of new box */
    video.total_offset = mse_voff;
    for (i = mse_y1; ; i++, video.total_offset += H_RES)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
	if (i == mse_y2) break;
    }
    
    /* draw bottom of new box */
    for (i = mse_x1; i <= mse_x2; i++, video.total_offset++)
    {	CHECK_SEG();
	*(graphics_base + video.segment.offset) = (i & 1) ? 0 : 215;
    }
  }
}


/*********** Function: setrline ****************************
 *
 * This function sets the initial position of the rubber line and
 * saves the pixels under it.
 */

setrline(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    register int dx, dy;
    
    /* get endpoints */
    mse_x1 = x1;
    mse_y1 = y1;
    mse_x2 = x2;
    mse_y2 = y2;
    
    /* save pixels under line and then set them to white */
    mse_voff = H_RES * mse_y1 + mse_x1;
    video.total_offset = mse_voff;
    dx = mse_x2 - mse_x1;
    if (dx > 0) mse_xinc = 1;
    else
    {	mse_xinc = -1;
    	dx = -dx;
    }
    dy = mse_y2 - mse_y1;
    if (dy > 0) mse_yinc = H_RES;
    else
    {	mse_yinc = -H_RES;
    	dy = -dy;
    }
    mse_old_dx = dx;
    mse_old_dy = dy;
    mse_pix_ptr = mse_pixels;
    if (dx > dy)
    {	mse_errinc = dy<<1;
    	mse_errdec = mse_errinc - (dx<<1);
	mse_err = mse_errinc - dx;
	for (dx++; ; dx--)
    	{   CHECK_SEG();
	    *mse_pix_ptr++ = *(graphics_base + video.segment.offset);
	    *(graphics_base + video.segment.offset) = (dx & 1) ? 0 : 215;
	    if (dx == 1) break;
	    video.total_offset += mse_xinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_yinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
    else
    {	mse_errinc = dx<<1;
    	mse_errdec = mse_errinc - (dy<<1);
	mse_err = mse_errinc - dy;
	for (dy++; ; dy--)
    	{   CHECK_SEG();
	    *mse_pix_ptr++ = *(graphics_base + video.segment.offset);
	    *(graphics_base + video.segment.offset) = (dy & 1) ? 0 : 215;
	    if (dy == 1) break;
	    video.total_offset += mse_yinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_xinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
}


/***************** Function: rubline **************************
 *
 * This function erases the rubber line at the previous location and, if
 * flag > 0, moves the floating end to x, y.
 */

rubline(x, y, flag)
int x, y, flag;
{
    register int  dx, dy;    
    
    /* erase the old line */
    video.total_offset = mse_voff;
    mse_pix_ptr = mse_pixels;
    dx = mse_old_dx;
    dy = mse_old_dy;
    if (dx > dy)
    {	for (dx++; ; dx--)
    	{   CHECK_SEG();
	    *(graphics_base + video.segment.offset) = *mse_pix_ptr++;
	    if (dx == 1) break;
	    video.total_offset += mse_xinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_yinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
    else
    {	for (dy++; ; dy--)
    	{   CHECK_SEG();
	    *(graphics_base + video.segment.offset) = *mse_pix_ptr++;
	    if (dy == 1) break;
	    video.total_offset += mse_yinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_xinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
    
  if (flag)
  {

    /* get new endpoints */
    mse_x2 = x;
    mse_y2 = y;
    
    /* save pixels under new line and then set them to white */
    mse_voff = H_RES * mse_y1 + mse_x1;
    video.total_offset = mse_voff;
    dx = mse_x2 - mse_x1;
    if (dx > 0) mse_xinc = 1;
    else
    {	mse_xinc = -1;
    	dx = -dx;
    }
    dy = mse_y2 - mse_y1;
    if (dy > 0) mse_yinc = H_RES;
    else
    {	mse_yinc = -H_RES;
    	dy = -dy;
    }
    mse_old_dx = dx;
    mse_old_dy = dy;
    mse_pix_ptr = mse_pixels;
    if (dx > dy)
    {	mse_errinc = dy<<1;
    	mse_errdec = mse_errinc - (dx<<1);
	mse_err = mse_errinc - dx;
	for (dx++; ; dx--)
    	{   CHECK_SEG();
	    *mse_pix_ptr++ = *(graphics_base + video.segment.offset);
	    *(graphics_base + video.segment.offset) = (dx & 1) ? 0 : 215;
	    if (dx == 1) break;
	    video.total_offset += mse_xinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_yinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
    else
    {	mse_errinc = dx<<1;
    	mse_errdec = mse_errinc - (dy<<1);
	mse_err = mse_errinc - dy;
	for (dy++; ; dy--)
    	{   CHECK_SEG();
	    *mse_pix_ptr++ = *(graphics_base + video.segment.offset);
	    *(graphics_base + video.segment.offset) = (dy & 1) ? 0 : 215;
	    if (dy == 1) break;
	    video.total_offset += mse_yinc;
	    if (mse_err < 0) mse_err += mse_errinc;
	    else
	    {	video.total_offset += mse_xinc;
	    	mse_err += mse_errdec;
	    }
	}
    }
  }
}
