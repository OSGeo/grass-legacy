/* This file contains the source code for the
 * AT&T VDC600 card DAC registers.
 *
 * Author: Paul W. Carlson    Jan. 1990
 */

/*======== system include files ===============================*/
#include <sys/types.h>
#include <sys/sysmacros.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/user.h>
#include <sys/errno.h>
#include <sys/immu.h>

/*======== driver include files ===============================*/
#include "palettes.h"
 
/*================ video register defines ======================*/
#define DAC_MASK	0x03C6	/* Palette mask register */
#define DAC_READ	0x03C7	/* Palette read register */
#define DAC_WRITE	0x03C8	/* Palette write register */
#define DAC_DATA	0x03C9	/* Palette data register */
#define INP_STAT_1	0x03DA	/* Input status register one */

/*================ macros ======================================*/
#define WRITE_DAC(DAC_REG, RED, GRN, BLU) \
{ \
    		while (!(inb(INP_STAT_1) & 8)); \
    		while (inb(INP_STAT_1) & 8); \
    		outb(DAC_WRITE, (unsigned char)DAC_REG); \
    		outb(DAC_DATA,  (unsigned char)RED); \
    		outb(DAC_DATA,  (unsigned char)GRN); \
    		outb(DAC_DATA,  (unsigned char)BLU); \
}

/*================ global variables ============================*/
static struct dac_struct {
    int arg1;
    int arg2;
    int arg3;
    int arg4;
} dac_args;

unsigned char dac_red[217];
unsigned char dac_grn[217];
unsigned char dac_blu[217];

int dac_busy;

static int *dac_argp;

/**************** Function: dac_open *************************************
 *
 * This function opens the driver.
 */
dac_open(dev, flag, otyp)
dev_t dev;
int flag, otyp;
{
    int off;

    /* Check if already open */
    if (dac_busy)
    {	u.u_error = EBUSY;
	return;
    }
    else dac_busy = 1;
}


/************* Function: dac_ioctl ********************************
 *
 * This function does all the DAC stuff.
 */
dac_ioctl(dev, cmd, argp, mode)
dev_t dev;
int cmd, *argp, mode;
{
    dac_argp = argp;
    copyin(dac_argp, &dac_args, sizeof(struct dac_struct));
    switch (cmd & 63)
    {
	case 1: /* DAC write */
		WRITE_DAC(dac_args.arg1, dac_args.arg2, 
			  dac_args.arg3, dac_args.arg4);
		break;

	case 2: /* DAC read */

    		outb(DAC_READ, (unsigned char)dac_args.arg1);
    		dac_args.arg2 = inb(DAC_DATA);
    		dac_args.arg3 = inb(DAC_DATA);
    		dac_args.arg4 = inb(DAC_DATA);
    		copyout(&dac_args, dac_argp, sizeof(struct dac_struct));
		break;

	case 3: /* This function selects one of four palettes.
 		 * 	1 - six-level RGB
 		 *	2 - continuous gray scale
 		 *	3 - red, green, blue color ramp
 		 *	4 - color wave
 		 * All the palettes have 217 colors, 
		 * with black in 0 and white in 216.		*/
    
    		switch (dac_args.arg1)
    		{   case 1: 
			    bcopy(rgb_red, dac_red, 217);
			    bcopy(rgb_grn, dac_grn, 217);
			    bcopy(rgb_blu, dac_blu, 217);
			    mov_palette();
			    break;
		    case 2:
			    bcopy(grey_red, dac_red, 217);
			    bcopy(grey_grn, dac_grn, 217);
			    bcopy(grey_blu, dac_blu, 217);
			    mov_palette();
			    break;
		    case 3: 
			    bcopy(ramp_red, dac_red, 217);
			    bcopy(ramp_grn, dac_grn, 217);
			    bcopy(ramp_blu, dac_blu, 217);
			    mov_palette();
			    break;
		    case 4: 
			    bcopy(wave_red, dac_red, 217);
			    bcopy(wave_grn, dac_grn, 217);
			    bcopy(wave_blu, dac_blu, 217);
			    mov_palette();
			    break;
    		}
		break;

	case 4: shift_dac();
	        mov_palette();
		break;
	    }
    }


/************* Function: dac_close ********************************
 *
 * This function closes the driver.
 */
dac_close(dev, flag, otyp)
dev_t dev;
int flag, otyp;
{
    /* Set flag to not busy */
    dac_busy = 0;
}
