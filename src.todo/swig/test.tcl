catch { load ./libgis.so libgis}
catch { load ./libgis.dll libgis}

set mod [_G_define_module]
set get [_G_define_option]
puts $mod
puts $get
