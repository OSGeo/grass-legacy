/*----------------------------------------------------------------------*/
/* Copyright (c) 1987							*/
/* by CompuServe Inc., Columbus, Ohio.  All Rights Reserved		*/
/* EXPANDER.C can be copied and distributed freely for any		*/
/* non-commercial purposes. EXPANDER.C can only be incorporated		*/
/* into commercial software with the permission of CompuServe Inc.	*/
/*----------------------------------------------------------------------*/

/*
 * ABSTRACT:
 *	The compression algorithm builds a string translation table that maps
 *	substrings from the input string into fixed-length codes.  These codes
 *	are used by the expansion algorithm to rebuild the compressor's table
 *	and reconstruct the original data stream.  In it's simplest form, the
 *	algorithm can be stated as:
 *
 *		"if <w>k is in the table, then <w> is in the table"
 *
 *	<w> is a code which represents a string in the table.  When a new
 *	character k is read in, the table is searched for <w>k.  If this
 *	combination is found, <w> is set to the code for that combination
 *	and the next character is read in.  Otherwise, this combination is
 *	added to the table, the code <w> is written to the output stream and
 *	<w> is set to k.
 *
 *	The expansion algorithm builds an identical table by parsing each
 *	received code into a prefix string and suffix character.  The suffix
 *	character is pushed onto the stack and the prefix string translated
 *	again until it is a single character.  This completes the expansion.
 *	The expanded code is then output by popping the stack and a new entry
 *	is made in the table.
 *
 *	The algorithm used here has one additional feature.  The output codes
 *	are variable length.  They start at a specified number of bits.  Once
 *	the number of codes exceeds the current code size, the number of bits
 *	in the code is incremented.  When the table is completely full, a
 *	clear code is transmitted for the expander and the table is reset.
 *	This program uses a maximum code size of 12 bits for a total of 4096
 *	codes.
 *
 *	The expander realizes that the code size is changing when it's table
 *	size reaches the maximum for the current code size.  At this point,
 *	the code size in increased.  Remember that the expander's table is
 *	identical to the compressor's table at any point in the original data
 *	stream.
 *
 *	The compressed data stream is structured as follows:
 *		first byte denoting the minimum code size
 *		one or more counted byte strings. The first byte contains the
 *		length of the string. A null string denotes "end of data"
 *
 *	This format permits a compressed data stream to be embedded within a
 *	non-compressed context.
 *
 * AUTHOR: Steve Wilhite
 *
 * REVISION HISTORY:
 *
 */

#include <malloc.h>
#include <setjmp.h>

#include "expander.h"

/* Function prototypes: */

void init_table();

short read_code();

#define null_ptr	0
#define largest_code	4095

jmp_buf recover;

struct code_entry
    {
    short prefix;			/* prefix code */
    char suffix;			/* suffix character */
    char stack;
    };

static short code_size;
static short clear_code;
static short eof_code;
static short first_free;
static short bit_offset;
static short byte_offset, bits_left;
static short max_code;
static short free_code;
static short old_code;
static short input_code;
static short code;
static short suffix_char;
static short final_char;
static short bytes_unread;
static unsigned char code_buffer[64];
static struct code_entry *code_table;
static short (*get_byte)();
static short (*put_byte)();

static short mask[12] =
    {0x001, 0x003, 0x007, 0x00F,
     0x01F, 0x03F, 0x07F, 0x0FF,
     0x1FF, 0x3FF, 0x7FF, 0xFFF};

void init_table(min_code_size)
 short	min_code_size;
    {
    code_size = min_code_size + 1;
    clear_code = 1 << min_code_size;
    eof_code = clear_code + 1;
    first_free = clear_code + 2;
    free_code = first_free;
    max_code = 1 << code_size;
    }

short read_code()
    {
    short
	bytes_to_move,
	i,
	ch;
    long
	temp;

    byte_offset = bit_offset >> 3;
    bits_left = bit_offset & 7;

    if (byte_offset >= 61)
	{
	bytes_to_move = 64 - byte_offset;

	for (i = 0; i < bytes_to_move; i++)
	    code_buffer[i] = code_buffer[byte_offset + i];

	while (i < 64)
	    {
	    if (bytes_unread == 0)
		{
		/* Get the length of the next record. A zero-length record
		 * denotes "end of data".
		 */

		bytes_unread = (*get_byte)();

		if (bytes_unread < 1)
		    if (bytes_unread == 0)	/* end of data */
			break;
		    else
			longjmp(recover, bytes_unread);
		}

	    ch = (*get_byte)();

	    if (ch < 0)
		longjmp(recover, ch);

	    code_buffer[i++] = (unsigned char)(ch);
	    bytes_unread--;
	    }

	bit_offset = bits_left;
	byte_offset = 0;
	}

    bit_offset += code_size;
    temp = (long) code_buffer[byte_offset]
	| (long) code_buffer[byte_offset + 1] << 8
	| (long) code_buffer[byte_offset + 2] << 16;

    if (bits_left > 0)
	temp >>= (long) bits_left;

    return (short)(temp) & mask[code_size - 1];
    }

short Expand_Data(get_byte_routine, put_byte_routine)
		  short (*get_byte_routine)();
		  short (*put_byte_routine)();
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
    {
    short sp;				/* stack ptr */
    short status;
    short min_code_size;

    get_byte = get_byte_routine;
    put_byte = put_byte_routine;

    code_table = (struct code_entry *)
	malloc(sizeof(struct code_entry)*(largest_code + 1));

    if (code_table == null_ptr)
	return -2;

    status = setjmp(recover);

    if (status != 0)
	{
	free((char *) code_table);
	return status;
	}

    /* Get the minimum code size (2 to 9) */

    min_code_size = (*get_byte)();

    if (min_code_size < 0)
	longjmp(recover, min_code_size);
    else if (min_code_size < 2 || min_code_size > 9)
	longjmp(recover, -3);

    init_table(min_code_size);
    sp = 0;
    bit_offset = 64*8;			/* force "read_code" to start a new */
    bytes_unread = 0;			/* record */

    while ((code = read_code()) != eof_code)
	{
	if (code == clear_code)
	    {
	    init_table(min_code_size);
	    code = read_code();
	    old_code = code;
	    suffix_char = code;
	    final_char = code;
	    if ((status = (*put_byte)(suffix_char)) != 0)
		longjmp(recover, status);
	    }
	else
	    {
	    input_code = code;

	    if (code >= free_code)
		{
		code = old_code;
    		code_table[sp++].stack = (char) final_char;
		}

	    while (code >= first_free)
		{
		code_table[sp++].stack = code_table[code].suffix;
		code = code_table[code].prefix;
		}

	    final_char = code;
	    suffix_char = code;
	    code_table[sp++].stack = (char) final_char;

	    while (sp > 0)
		if ((status = (*put_byte)(code_table[--sp].stack)) != 0)
		    longjmp(recover, status);

	    code_table[free_code].suffix = (char) suffix_char;
	    code_table[free_code].prefix = old_code;
	    free_code++;
	    old_code = input_code;

	    if (free_code >= max_code)
		if (code_size < 12)
		    {
		    code_size++;
		    max_code <<= 1;
		    }
	    }
	}

    free((char *) code_table);
    return 0;
    }
