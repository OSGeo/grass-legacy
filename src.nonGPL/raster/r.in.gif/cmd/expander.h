/*----------------------------------------------------------------------*/
/* Copyright (c) 1987							*/
/* by CompuServe Inc., Columbus, Ohio.  All Rights Reserved		*/
/* EXPANDER.H can be copied and distributed freely for any		*/
/* non-commercial purposes. EXPANDER.H can only be incorporated		*/
/* into commercial software with the permission of CompuServe Inc.	*/
/*----------------------------------------------------------------------*/

short Expand_Data();
/*
 * Function:
 *	Decompress a LZW compressed data stream.
 *
 * Inputs:
 *	get_byte_routine - address of the caller's "get_byte" routine.
 *	put_byte_routine - address of the caller's "put_byte" routine.
 *
 * Returns:
 *	0	OK
 *	-1	expected end-of-file
 *	-2	cannot allocate memory
 *	-3	bad "min_code_size"
 *	< -3	error status from the get_byte or put_byte routine
 */
