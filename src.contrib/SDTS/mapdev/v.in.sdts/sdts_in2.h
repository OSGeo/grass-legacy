struct att_ff_file
{
  char *fname;
  int processed;
};

struct att_ff_info
{
   int n_ap_files;
   int n_as_files;
   int n_ff_files;
   struct att_ff_file *AP, *AS, *FF;
};


