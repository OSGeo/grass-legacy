
struct siteslegend 
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct siteslegend siteslegend ;
#else
    extern struct siteslegend siteslegend;
#endif


