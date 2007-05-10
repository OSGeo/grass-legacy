struct GFONT_CAP
{   
    char *name;     /**< Short name for this font face */
    char *longname; /**< Descriptive name for the font face */
    char *path;     /**< Full path to the file containing this font face */
    int index;      /**< Index within the file of this font face */
    int type;       /**< Type of this font face (currently stroke or freeype) */
    char *encoding; /**< Encoding to be used with this font face. */
};

#define GFONT_STROKE 0
#define GFONT_FREETYPE 1

#ifdef G_MKFONTCAP_MAIN
#  define G_MKFONTCAP_GLOBAL
#else
#  define G_MKFONTCAP_GLOBAL extern
#endif

G_MKFONTCAP_GLOBAL char **searchdirs;
G_MKFONTCAP_GLOBAL int numsearchdirs;

G_MKFONTCAP_GLOBAL struct GFONT_CAP *fontcap;
G_MKFONTCAP_GLOBAL int totalfonts;
G_MKFONTCAP_GLOBAL int maxfonts;

/* freetype_fonts.c */
void find_freetype_fonts(void);

/* stroke_fonts.c */
void find_stroke_fonts(void);
