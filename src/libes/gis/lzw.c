/**********************************************************************
 *  
 *  void
 *  G_lzw_set_bits (bits)
 *  
 *       int bits;
 *  
 *  sets the maximum number of bits used for compression. this number
 *  determines the achievable compression ratio. usually, but not
 *  necessarily, more bits result in a better compression ratio. the
 *  tradeoff for more bits is a larger internal buffer. the buffer 
 *  size for compression is apprx. (2 x 2^bits x sizeof (2 x int + char)),
 *  for expansion it is (2^bits x sizeof (2 x int + char)). this sounds
 *  worse than it actually is, since the implementation is adaptive,
 *  it starts with 9 bits and increases the bits used only
 *  if neccessary. This implies that for any data 2^bits will never
 *  exceed twice the size of the size of the data (except for very
 *  small data, of course).
 *  
 *  note: internally the range of bits is clamped to [9,20].
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_max_used_bits ()
 *  
 *  returns: the maximum number of bits used for the latest compression
 *           or expansion. 
 *  
 *           0 ... if no compression was performed.
 *  
 **********************************************************************
 *  
 *  int 
 *  G_lzw_compress (src, dst, nofBytes)
 *  
 *       unsigned char *src, *dst;
 *       int nofBytes;
 *  
 *  compresses the first "nofBytes" of data in "src" and copies 
 *  the compressed data into "dst". dst is assumed to be of size
 *  nofBytes. in case the compressed size exceeds the uncompressed
 *  size G_lzw_compress () interrupts the compresion process.
 *  
 *  returns: >= 0, the size of the compressed data.
 *             -1, if the size of the compressed data exceeds nofBytes.
 *             -2 if there was a problem allocating buffers.
 *  
 *  note: in the case that -1 is returned the data in dst is not defined.
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_expand (src, dst, nofBytes)
 *  
 *       unsigned char *src, *dst;
 *       int nofBytes;
 *  
 *  expands data in "src" and stores the uncompressed data in "dst".
 *  it is assumed that the data in src has been compressed with 
 *  G_lzw_compress (). "nofBytes" specifies the size of "dst".
 *  
 *  returns: >= 0, the size of the expanded data.
 *             -1, if the size of dst is too small.
 *             -2 if there was a problem allocating buffers.
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_compress_count_only_array (src, nofBytes)
 *  
 *       unsigned char *src;
 *       int nofBytes;
 *  
 *  computes the size of the first "nofBytes" of "src" if they would
 *  be compressed with G_lzw_compress ().
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_compress_count_only_file (src, nofBytes)
 *  
 *       int src;
 *       int nofBytes;
 *  
 *  computes the size of the next "nofBytes" of the binary file "src" 
 *  if they would be compressed with G_lzw_compress ().
 *  
 *  note: the file pointer of src is reset to the initial position
 *        upon completion.
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_write (dst, src, nofBytes)
 *  
 *       int dst;
 *       unsigned char *src;
 *       int nofBytes;
 *  
 *  compresses the first "nofBytes" of data in "src" and copies 
 *  the compressed onto file "dst". in case the compressed size would 
 *  exceed the uncompressed size, G_lzw_write () copies the uncompressed 
 *  data onto "dst". the first byte written determines the maximum number
 *  of "bits" used for the compression. in the case of the compressed
 *  size exceeding the uncompressed size this byte is set to 0. note,
 *  that this implies that the number of bytes written onto dst might
 *  exceed nofBytes by one.
 *  
 *  returns: >= 0, the size of the compressed data.
 *             -1, if the size of the compressed data exceeds nofBytes.
 *             -2 if there was a problem allocating buffers.
 *  
 *  note: the file pointer for dst need not be in position 0, and
 *        there can be data succeeding the current portion of
 *        compressed data on dst. the end of the current portion of
 *        compressed data is determined by a special termination code 
 *        written by G_lzw_write ().
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_read (src, dst, nofBytes)
 *  
 *       int src;
 *       unsigned char *dst;
 *       int nofBytes;
 *  
 *  expands data in file "src" and stores the uncompressed data in "dst".
 *  it is assumed that the data in src has been compressed with 
 *  G_lzw_write (). "nofBytes" specifies the size of "dst". the first
 *  byte read determines if the remaining data has to be expanded or
 *  only copied onto dst. (see G_lzw_write ()).
 *  
 *  returns: -2 if there was a problem allocating buffers.
 *           -1 if the size of dst is too small.
 *           otherwise, the size of the uncompressed data.
 *  
 *  note: the file pointer for src need not be in position 0, and
 *        there can be data succeeding the current portion of
 *        compressed data on src. the end of the current portion of
 *        compressed data is determined by a special termination code 
 *        written by G_lzw_write ().
 *  
 *  note: no assumption can be made for the file pointer of "src"
 *        after the execution of this function. 
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_read2 (src, dst, nofBytes, buffSize)
 *  
 *       int src;
 *       unsigned char *dst;
 *       int nofBytes, buffSize;
 *  
 *  expands data in file "src" and stores the uncompressed data in "dst".
 *  it is assumed that the data in src has been compressed with 
 *  G_lzw_write (). "nofBytes" specifies the size of "dst". the first
 *  byte read determines if the remaining data has to be expanded or
 *  only copied onto dst. if the data has to be expanded "buffSize"
 *  specifies how many bytes the read operation should read in one chunk.
 *  if this number is larger then BUFFER_SIZE then BUFFER_SIZE is used
 *  instead. (see G_lzw_read ()).
 *  
 *  return values and remarks are those of G_lzw_read.
 *  
 **********************************************************************
 *
 *  int
 *  G_lzw_nof_read_bytes ()
 *  
 *  returns the number of bytes lzw uses to store the compressed version 
 *  of the data which was read from file in the latest execution of 
 *  G_lzw_read (). 
 *  
 *  note: has to be used immediately after a G_lzw_read (), before the
 *        execution of any other G_lzw_* () function.
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_transfer_compress (src, dst, nofBytes)
 *  
 *       int src, dst;
 *       int nofBytes;
 *  
 *  the functionality is identical to G_lzw_write (), except that "src"
 *  is a binary file.
 *  
 *  returns: >= 0, the size of the compressed data.
 *             -1, if the size of the compressed data exceeds nofBytes.
 *             -2 if there was a problem allocating buffers.
 *  
 *  note: reads the data in "src" twice; once to figure out the 
 *        compressed size, and once to do the actual compression.
 *  
 *  note: no assumption can be made for the file pointer of "src"
 *        after the execution of this function. 
 *  
 **********************************************************************
 *  
 *  int
 *  G_lzw_transfer_expand (src, dst, nofBytes)
 *  
 *       int src, dst;
 *       int nofBytes;
 *  
 *  the functionality is identical to G_lzw_write (), except that "dst"
 *  is a binary file.
 *  
 *  returns: >= 0, the size of the expanded data.
 *             -1, if the size of dst is too small.
 *             -2 if there was a problem allocating buffers.
 *  
 *  note: no assumption can be made for the file pointer of "src"
 *        after the execution of this function. 
 *  
 **********************************************************************/

/*--------------------------------------------------------------------------*/

/* 
 this code for the lzw compression algorithm has been adapted from a
 demonstration code for PCs provided by Mark R. Nelson. for a discussion
 of the lzw algorithm see:

      T. A.Welch, A Technique for High Performance Data Compression, 
      IEEE Computer, 17(6), June 1984, pp. 8-19.

 this implementation follows the algorithm in the paper closely. the main
 deviation is that the number of bits used for the codes is dynamic. that is
 we start we the smallest possible number of bits, 9, and increase it by
 1 every time we run out of codes that can be described with the current 
 number of bits. an increase in bits is indicated by the INCREASE_BITS_CODE.
 in the expasnion phase it would be possible to determine when a bit increase
 happens even without the INCREASE_BITS_CODE. however, such a scheme can not
 be used, since in the compression phase the maximum number of bits is 
 arbitrily limited. due to this dynamic method, we need an array of
 hash-tables in the decoding phase. every hash-table in this array stores
 the codes for a certain number of bits. the size of each table is such
 that no more than 50% of the table can be filled. in order to save space
 the tables are allocated only if needed. to search for an item in the array
 of tables, we start in the smallest table that can possibly contain the code.
 if in this table we cannot find the item we proceed to the next table until
 the largest table has been visited.

*/

/*--------------------------------------------------------------------------*/

#include <gis.h>
#include <sys/types.h>
#include <unistd.h>

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

typedef struct {

  unsigned int bits, hashing_shift, maxCode, table_size;
  int *code;
  unsigned int *codeOfPrefix;
  unsigned char *suffixChar;

} hashTableType;

/*--------------------------------------------------------------------------*/

typedef struct {

  unsigned int bits, maxCode, table_size;

} decodeTableType;

/*--------------------------------------------------------------------------*/

unsigned char *decode_buffer; 
static int bit_count;
static unsigned int bit_buffer = 0;

static int maxAllocatedBits = 0, minAllocatedBits;
static int maxUsedBits;
static int maxBits = 0;
static hashTableType *h[21];
static decodeTableType *d[21];
static hashTableType *current;
static decodeTableType *currentD;

static int (*lzwGetChar) (), (*lzwPutChar) ();

#define CURRENT (h[maxAllocatedBits])
#define CURRENTD (d[maxAllocatedBits])

#define LZW_MIN_BITS 9   /* must be at least 9 */
#define LZW_MAX_BITS 20
#define LZW_DEFAULT_NOFBITS 16

static int lzwNofBits = LZW_DEFAULT_NOFBITS;

unsigned int *codeOfPrefix;
unsigned char *suffixChar;

#define END_OF_DATA_CODE (1 << (LZW_MIN_BITS - 1))
#define INCREASE_BITS_CODE (END_OF_DATA_CODE + 1)
#define FIRST_CODE (INCREASE_BITS_CODE + 1)

/*--------------------------------------------------------------------------*/

static void
lzw_set_bits (bits)

     int bits;

{
#define MIN(a,b) (a < b ? a : b)
#define MAX(a,b) (a > b ? a : b)

  lzwNofBits = MIN (LZW_MAX_BITS, MAX (LZW_MIN_BITS, bits));
}

/*--------------------------------------------------------------------------*/

static void *
lzw_malloc (nBytes)

     int nBytes;

{
  void *tmp;

  tmp = (void *) G_malloc (nBytes);
  if (tmp == NULL) G_fatal_error ("lzw_malloc: can't allocate memory");

  return tmp;
}

/*--------------------------------------------------------------------------*/

static void *
lzw_realloc (buffer, nBytes)

     char *buffer;
     int nBytes;

{
  void *tmp;

  tmp = (void *) G_realloc (buffer, nBytes);
  if (tmp == NULL) G_fatal_error ("lzw_realloc: can't allocate memory");

  return tmp;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                           /* COMPRESSION */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static hashTableType *
lzw_alloc_hashtable (nofBits)

     int nofBits;

{
  hashTableType *tmp;
  int i;

  static int prime_size [21] = {0,1,2,3,4,5,6,7,8, /* dummies */
				587, 1171, 2131, 5021, 9029, 18041, 36551, 
			        72221, 150011, 300233, 598127,1103981};

  tmp = (hashTableType *) lzw_malloc (sizeof (hashTableType));

  tmp->bits = nofBits;
  tmp->hashing_shift = nofBits - 8;
  tmp->maxCode = (1 << nofBits) - 1;
  tmp->table_size = prime_size [nofBits];

  tmp->code = (int *) lzw_malloc (tmp->table_size * sizeof (int));
  tmp->codeOfPrefix = (unsigned int *)
                          lzw_malloc (tmp->table_size * sizeof (unsigned int));
  tmp->suffixChar = (unsigned char *) 
                          lzw_malloc (tmp->table_size * sizeof (unsigned char));

  for (i = 0; i < tmp->table_size; i++) tmp->code[i] = -1;

  return tmp;
}

/*--------------------------------------------------------------------------*/

static hashTableType *
lzw_increase_hashtable ()

{
  if (maxAllocatedBits >= maxBits) return (hashTableType *) NULL;

  maxAllocatedBits++;

  return (current = CURRENT = lzw_alloc_hashtable (maxAllocatedBits));
}

/*--------------------------------------------------------------------------*/

static int
lzw_init_compress ()

{
  minAllocatedBits = maxAllocatedBits = LZW_MIN_BITS;

  if (current = CURRENT = lzw_alloc_hashtable (maxAllocatedBits)) {

    bit_count = 0;
    bit_buffer = 0;
    
    maxBits = lzwNofBits;

    return 1;
  } 
    
  return 0;
}

/*--------------------------------------------------------------------------*/

static void
lzw_free_hashtable (
     hashTableType * ht)

{
  G_free (ht->code);
  G_free (ht->codeOfPrefix);
  G_free (ht->suffixChar);
  G_free (ht);
}  

/*--------------------------------------------------------------------------*/

static void
lzw_cleanup_compress ()

{
  int i;

  if (! maxAllocatedBits) return;

  for (i = minAllocatedBits; i <= maxAllocatedBits; i++)
    lzw_free_hashtable (h[i]);

  maxUsedBits = maxAllocatedBits;
  maxAllocatedBits = 0;
}

/*--------------------------------------------------------------------------*/

static int
lzw_get_index (code)

     unsigned int code;

{
  register unsigned int l, index;

  l = code >> minAllocatedBits;
  index = minAllocatedBits;

  while (l) {
    index++;
    l >>= 1;
  }

  return index;
}

/*--------------------------------------------------------------------------*/

/* buffer output of the code */

static int
lzw_putCodeBuffered (count, code)

     unsigned int code;
     int *count;

{
  bit_buffer |= (unsigned int) code << (32 - current->bits - bit_count);
  bit_count += current->bits;

  while (bit_count >= 8) {
    if (! lzwPutChar (bit_buffer >> 24)) return 0;
    (*count)++;
    bit_buffer <<= 8;
    bit_count -= 8;
  }

  return 1;
}

/*--------------------------------------------------------------------------*/

/* flush the remainder of the code */

static int
lzw_flushCodeBuffer (count)

     int *count;
{
  if (bit_count == 0) return 1;

  if (! lzwPutChar (bit_buffer >> 24)) return 0;
  bit_buffer <<= 8;
  (*count)++;

  return 1;
}

/*--------------------------------------------------------------------------*/

/* tries to find entry <prefixCode, character> in hashtable "ht".
   the index returned either points to such an entry, or to an
   unused space in the hashtable. */

static int
lzw_findPrefixInTable (ht, prefixCode, character)

     hashTableType *ht;
     unsigned int prefixCode;
     unsigned int character;

{
  int index, offset;

  offset = 
    ((index = (character << ht->hashing_shift) ^ prefixCode) == 0 ?
     1 : ht->table_size - index);

  while (1) {
    if ((ht->code[index] == -1) ||
	(ht->codeOfPrefix[index] == prefixCode && 
	 ht->suffixChar[index] == character)) return index;

    index -= offset;
    if (index < 0) index += ht->table_size;
  }
}

/*--------------------------------------------------------------------------*/

/* tries to find <prefixCode, character> in any of the hashtables. search 
   starts with the smallest hashtable which can possibly contain the entry.
   returns in <ht, index> a hashtable and the index which points to the
   entry <prefixCode, character> in that table. or returns a pointer to
   the largest hashtable and a pointer to an unused space in the hashtable. */

static void
lzw_findPrefix (prefixCode, character, ht, index)

     unsigned int prefixCode;
     unsigned int character;
     hashTableType **ht;
     int *index;

{
  int i;

  i = lzw_get_index (prefixCode);
  while ((i <= maxAllocatedBits) &&
	 (h[i]->code[*index = 
	  lzw_findPrefixInTable (h[i], prefixCode, character)] == -1))
    i++;

  *ht = h[(i <= maxAllocatedBits ? i : maxAllocatedBits)];
}

/*--------------------------------------------------------------------------*/

/* keeps compressing data provided by "getChar" until "getChar" indicates
   end of data (returns 0). the compressed data is output via function
   "putChar". when all the data is compressed a END_OF_DATA_CODE is
   written and the outputstream is flushed with a "0". every time the
   number of bits increases a new hashtable is created, and the 
   INCREASE_BITS_CODE is written. */

static int
lzw_compress (getChar, putChar)

     int (* getChar) (), (*putChar) ();

{
  unsigned int character, prefixCode, nextCode;
  int lzwSize, index;
  hashTableType *ht;

  if (! lzw_init_compress ()) return -2;

  lzwGetChar = getChar;
  lzwPutChar = putChar;

  if (! getChar (&prefixCode)) return 0;

  nextCode = FIRST_CODE;
  lzwSize = 0;

  while (getChar (&character)) {
    lzw_findPrefix (prefixCode, character, &ht, &index); 

    if (ht->code[index] != -1)              
      prefixCode = ht->code[index];          
    else {             
      if (nextCode <= ht->maxCode) {
	ht->code[index] = nextCode++;
	ht->codeOfPrefix[index] = prefixCode;
	ht->suffixChar[index] = character;
      } else
	if (maxAllocatedBits < maxBits) {
	  if (! lzw_putCodeBuffered (&lzwSize, INCREASE_BITS_CODE)) return -1;
	  ht = lzw_increase_hashtable ();
	  index = lzw_findPrefixInTable (ht, prefixCode, character);
	  ht->code[index] = nextCode++;
	  ht->codeOfPrefix[index] = prefixCode;
	  ht->suffixChar[index] = character;
	}
      if (! lzw_putCodeBuffered (&lzwSize, prefixCode)) return -1;
      prefixCode = character;
    }                       
  }

  if (! lzw_putCodeBuffered (&lzwSize, prefixCode)) return -1;
  if (! lzw_putCodeBuffered (&lzwSize, END_OF_DATA_CODE)) return -1;
  if (! lzw_flushCodeBuffer (&lzwSize)) return -1;

  lzw_cleanup_compress ();

  return lzwSize;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                            /* EXPANSION */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static decodeTableType *
lzw_alloc_decodetable (nofBits)

     int nofBits;

{
  decodeTableType *tmp;

  tmp = (decodeTableType *) lzw_malloc (sizeof (decodeTableType));
  
  tmp->bits = nofBits;
  tmp->table_size = 1 << nofBits;
  tmp->maxCode = (1 << nofBits) - 1;
  
  if (nofBits == minAllocatedBits) {
    codeOfPrefix = (unsigned int *)
      lzw_malloc (tmp->table_size * sizeof (unsigned int));
    suffixChar = (unsigned char *) 
      lzw_malloc (tmp->table_size * sizeof (unsigned char));
    decode_buffer = (unsigned char *)
      lzw_malloc (tmp->table_size * sizeof (unsigned char *));
  } else {
    codeOfPrefix = (unsigned int *)
      lzw_realloc ((char *)codeOfPrefix, tmp->table_size *
		   sizeof (unsigned int));
    suffixChar = (unsigned char *) 
      lzw_realloc ((char *)suffixChar, tmp->table_size * 
		   sizeof (unsigned char));
    decode_buffer = (unsigned char *)
      lzw_realloc ((char *)decode_buffer, tmp->table_size * 
		   sizeof (unsigned char *));
  }

  return tmp;
}

/*--------------------------------------------------------------------------*/

static decodeTableType *
lzw_increase_decodetable ()

{
  maxAllocatedBits++;
  return (currentD = CURRENTD = lzw_alloc_decodetable (maxAllocatedBits));
}

/*--------------------------------------------------------------------------*/

static int
lzw_init_expand ()

{
  minAllocatedBits = maxAllocatedBits = LZW_MIN_BITS;

  if (currentD = CURRENTD = lzw_alloc_decodetable (maxAllocatedBits)) {

    bit_count = 0;
    bit_buffer = 0;
    
    return 1;
  } 
    
  return 0;
}

/*--------------------------------------------------------------------------*/

static void
lzw_cleanup_expand ()

{
  int i;

  if (! maxAllocatedBits) return;

  G_free (codeOfPrefix);
  G_free (suffixChar);
  G_free (decode_buffer);

  for (i = minAllocatedBits; i <= maxAllocatedBits; i++) G_free (d[i]);

  maxUsedBits = maxAllocatedBits;
  maxAllocatedBits = 0;
}

/*--------------------------------------------------------------------------*/

/* bufferes read via "getChar". returns units of the required number of 
   bits. */

static unsigned int
lzw_getCode ()

{
  unsigned int code, elt;

  while (bit_count < currentD->bits) {
    if (! lzwGetChar (&elt)) 
      G_fatal_error ("lzw_expand: end of input encountered prematurely\n");
    bit_buffer |= elt << (24 - bit_count);
    bit_count += 8;
  }

  code = bit_buffer >> (32 - currentD->bits);
  bit_buffer <<= currentD->bits;
  bit_count -= currentD->bits;

  return code;
}

/*--------------------------------------------------------------------------*/

/* decodes "code" by following the code pointers and appending the 
   corresponding characters to "buffer". */

unsigned char *
lzw_decode (buffer, code)

     unsigned char *buffer;
     unsigned int code;

{
  while (code > 255) {
    *buffer++ = suffixChar[code];
    code = codeOfPrefix[code];
  }

  *buffer= (unsigned char) code;

  return (buffer);
}

/*--------------------------------------------------------------------------*/

/* keeps expanding data provided by "getChar" until the END_OF_DATA_CODE
   is encountered. the uncompressed data is output via function "putChar". */


int
lzw_expand (getChar, putChar)

     int (* getChar) (), (*putChar) ();

{
  unsigned int newCode, oldCode, nextCode;
  unsigned int character, nofBytes;
  unsigned char *string;

  if (! lzw_init_expand ()) return -2;

  lzwGetChar = getChar;
  lzwPutChar = putChar;

  nextCode = FIRST_CODE;
  nofBytes = 0;

  oldCode = lzw_getCode ();
  character = oldCode;          
  if (! lzwPutChar (oldCode)) return -1;
  nofBytes++;

  while ((newCode = lzw_getCode ()) != END_OF_DATA_CODE) {

    if (newCode >= nextCode) {
      *decode_buffer = character;
      string = lzw_decode (decode_buffer + 1, oldCode);
    } else 
      if (newCode == INCREASE_BITS_CODE) {
	lzw_increase_decodetable ();
	continue;
      } else
	string = lzw_decode (decode_buffer, newCode);

    character = *string;
    while (string >= decode_buffer) {
      if (! lzwPutChar ((unsigned int) *string--)) return -1; 
      nofBytes++;
    }

    if (nextCode <= currentD->maxCode) {
      codeOfPrefix[nextCode] = oldCode;
      suffixChar[nextCode] = character;
      nextCode++;
    }

    oldCode = newCode;
  }

  lzw_cleanup_expand ();
  return nofBytes;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                          /* ARRAY I/O SUPPORT */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static unsigned char *inputArray, *inputLast;

static int
lzw_read_from_array (character)

     unsigned int *character;

{
  if (inputArray <= inputLast) {
    *character = (unsigned int) *inputArray++;
    return 1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

static void
lzw_init_read_from_array (a, nofChars)

     unsigned char *a;
     int nofChars;

{
  inputArray = a;
  inputLast = &(a[nofChars - 1]);
}

/*--------------------------------------------------------------------------*/

static unsigned char *outputArray, *outputLast;

static int
lzw_write_to_array (character)

     unsigned int character;

{
  if (outputArray <= outputLast) {
    *outputArray++ = (unsigned char) character;
    return 1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

static void
lzw_init_write_to_array (a, nofChars)

     unsigned char *a;
     int nofChars;

{
  outputArray = a;
  outputLast = &(a[nofChars - 1]);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                          /* FILE I/O SUPPORT */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define BUFFER_SIZE 2048

static unsigned char inputbuffer[BUFFER_SIZE];
static int inputPos, inputSize;
static int inputFile;
static int readTotal = 0;
static int readFromFileBlockSize;

static int
lzw_read_from_file (character)

     unsigned int *character;

{
  if (inputPos == inputSize) {
    inputSize = read (inputFile, inputbuffer, readFromFileBlockSize);
    if (! inputSize) return inputPos = inputSize = 0;
    inputPos = 0;
  }
  
  readTotal++;
  *character = inputbuffer[inputPos++];
  return 1;
}

/*--------------------------------------------------------------------------*/

static void
lzw_init_read_from_file (fp, maxNofBytes)

     int fp, maxNofBytes;

{
  inputFile = fp;
  inputPos = inputSize = 0;
  readTotal = 1;
  readFromFileBlockSize = MIN (maxNofBytes, BUFFER_SIZE);
}

/*--------------------------------------------------------------------------*/

static int outputFile;
static unsigned char outputbuffer[BUFFER_SIZE];
static int outputPos;

/*--------------------------------------------------------------------------*/

static void
lzw_flush_output ()

{
  if (outputPos) write (outputFile, outputbuffer, 
			(BUFFER_SIZE < outputPos ? BUFFER_SIZE : outputPos));

  outputPos = 0;
}

/*--------------------------------------------------------------------------*/
static int
lzw_write_to_file (character)

     unsigned int character;

{
  outputbuffer [outputPos++] = character;
  if (outputPos == BUFFER_SIZE) lzw_flush_output ();

  return 1;
}

/*--------------------------------------------------------------------------*/

static void
lzw_init_write_to_file (fp)

     int fp;

{
  outputFile = fp;
  outputPos = 0;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                        /* COUNT-ONLY O SUPPORT */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int
lzw_write_to_null (character)

     unsigned int character;

{
  return 1;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

                        /* EXPORTED FUNCTIONS */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

int
G_lzw_nof_read_bytes ()

{
  return readTotal;
}

/*--------------------------------------------------------------------------*/

int
G_lzw_max_used_bits ()

{
  return maxUsedBits;
}

/*--------------------------------------------------------------------------*/

void
G_lzw_set_bits (bits)

     int bits;

{
  lzw_set_bits (bits);
}

/*--------------------------------------------------------------------------*/

int
G_lzw_compress (src, dst, nofBytes)

     unsigned char *src, *dst;
     int nofBytes;

{
  lzw_init_read_from_array (src, nofBytes);
  lzw_init_write_to_array (dst, nofBytes);
  return lzw_compress (lzw_read_from_array, lzw_write_to_array);
}

/*--------------------------------------------------------------------------*/

int
G_lzw_expand (src, dst, nofBytes)

     unsigned char *src, *dst;
     int nofBytes;

{
  lzw_init_read_from_array (src, nofBytes);
  lzw_init_write_to_array (dst, nofBytes);
  return lzw_expand (lzw_read_from_array, lzw_write_to_array);
}

/*--------------------------------------------------------------------------*/

int
G_lzw_compress_count_only_array (src, nofBytes)

     unsigned char *src;
     int nofBytes;

{
  lzw_init_read_from_array (src, nofBytes);
  return lzw_compress (lzw_read_from_array, lzw_write_to_null);
}

/*--------------------------------------------------------------------------*/

int
G_lzw_compress_count_only_file (src)

     int src;

{
  int size;
  long location;

  location = lseek (src, 0L, SEEK_CUR);

  lzw_init_read_from_file (src, BUFFER_SIZE);
  size = lzw_compress (lzw_read_from_file, lzw_write_to_null);

  lseek (src, location, SEEK_SET);

  return size;
}

/*--------------------------------------------------------------------------*/

int
G_lzw_write (dst, src, nofBytes)

     int dst;
     unsigned char *src;
     int nofBytes;

{
  int i;
  unsigned char tmp;

  if (nofBytes == 0) {
    tmp = maxUsedBits = 0;
    if (write (dst, &tmp, 1) != 1) return -1;
    return 1;
  }

  if (G_lzw_compress_count_only_array (src, nofBytes) < nofBytes) {
    tmp = G_lzw_max_used_bits ();
    write (dst, &tmp, 1);
    lzw_init_read_from_array (src, nofBytes);
    lzw_init_write_to_file (dst);
    i = lzw_compress (lzw_read_from_array, lzw_write_to_file);
    if (i < 0) return i;
    lzw_flush_output ();
    return i + 1;
  } else {
    tmp = maxUsedBits = 0;
    if (write (dst, &tmp, 1) != 1) return -1;
    if (write (dst, src, nofBytes) != nofBytes) return -1;
    return nofBytes + 1;
  }
}

/*--------------------------------------------------------------------------*/

int
G_lzw_write_noCompress (
     int dst,
     unsigned char *src,
     int nofBytes)

{
  unsigned char tmp;

  if (nofBytes == 0) {
    tmp = maxUsedBits = 0;
    if (write (dst, &tmp, 1) != 1) return -1;
    return 1;
  }

  tmp = maxUsedBits = 0;
  if (write (dst, &tmp, 1) != 1) return -1;
  if (write (dst, src, nofBytes) != nofBytes) return -1;
  return nofBytes + 1;
}

/*--------------------------------------------------------------------------*/

int
G_lzw_test_status (src)

     int src;

{
  char infoByte;

  read (src, &infoByte, 1);

  return infoByte != 0;
}

/*--------------------------------------------------------------------------*/

int
G_lzw_read2 (src, dst, nofBytes, buffSize)

     int src;
     unsigned char *dst;
     int nofBytes, buffSize;

{
  char infoByte;

  read (src, &infoByte, 1);

  if (infoByte) {
    lzw_init_read_from_file (src, buffSize);
    lzw_init_write_to_array (dst, nofBytes);
    return lzw_expand (lzw_read_from_file, lzw_write_to_array);
  } else {
    maxUsedBits = 0;
    if (nofBytes!= 0) read (src, dst, nofBytes);
    readTotal = nofBytes + 1;
    return nofBytes;
  }
}

/*--------------------------------------------------------------------------*/

int
G_lzw_read (src, dst, nofBytes)

     int src;
     unsigned char *dst;
     int nofBytes;

{
  return G_lzw_read2 (src, dst, nofBytes, nofBytes);
}

/*--------------------------------------------------------------------------*/

int 
G_lzw_transfer_compress (src, dst, nofBytes)

     int src, dst;
     int nofBytes;

{  
  int c, i;
  unsigned char tmp;

  if (G_lzw_compress_count_only_file (src) < nofBytes) {
    tmp = G_lzw_max_used_bits ();
    write (dst, &tmp, 1);
    lzw_init_read_from_file (src, nofBytes);
    lzw_init_write_to_file (dst);
    i = lzw_compress (lzw_read_from_file, lzw_write_to_file);
    if (i < 0) return i;
    lzw_flush_output ();
    return i + 1;
  } else {
    tmp = maxUsedBits = 0;
    write (dst, &tmp, 1);
    for (i = 0; i < nofBytes; i++) {
      read (src, &c, 1);
      write (dst, &c, 1);
    }
    return nofBytes + 1;
  }
}

/*--------------------------------------------------------------------------*/

int 
G_lzw_transfer_expand (src, dst, nofBytes)

     int src, dst;
     int nofBytes;

{
  int c, i;
  char infoByte;

  read (src, &infoByte, 1);

  if (infoByte) {
    lzw_init_read_from_file (src, nofBytes);
    lzw_init_write_to_file (dst);
    i = lzw_expand (lzw_read_from_file, lzw_write_to_file);
    lzw_flush_output ();
    return i;
  } else {
    maxUsedBits = 0;
    for (i = 0; i < nofBytes; i++) {
      read (src, &c, 1);
      write (dst, &c, 1);
    }
    return nofBytes;
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
