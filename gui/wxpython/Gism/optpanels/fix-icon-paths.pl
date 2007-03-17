#!/usr/bin/perl

## Use this to change absolute icon paths in glade-generated code to portable stuff
## use like this: perl this_file glade-emitted.py > portable-equivalent.py

# This is the hardcoded path in current (200703) svn version
$glade_path = q|/Applications/Grass/GRASS.app/Contents/Resources/etc/gui/icons/|; 

# Be careful below: make sure os is imported but don't do it twice
$prelude = <<ENDP;
if os.getenv("GRASS_ICONPATH"):
    icons = os.environ["GRASS_ICONPATH"]
    if icons[-1] != '/':
        icons.append('/')
else:
    icons = os.getenv("GISBASE") + "/etc/gui/icons/"
ENDP

$has_import_os = 0;
$in_headers = 1;
while(<>){
  /import.*\bos\b/ and $has_import_os = 1;
  /^(class|def)/ and do{ 
    print "import os\n" if !$has_import_os;
    print $prelude if $in_headers;
    $in_headers = 0;
    };
  s|"$glade_path|icons+"|o;
  print;
}

# --Daniel Calvelo 200703
