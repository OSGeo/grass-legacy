proc file_listbox {path} {
	global file_name
	toplevel .files
	wm geometry .files +100+100
	wm title .files files
	
	frame .files.frame0 \
	    -borderwidth {2} \
	    -relief {flat}

	listbox .files.frame0.listbox \
	    -relief {sunken} \
	    -geometry 20x10 \
	    -yscrollcommand {.files.frame0.vscrollbar set}

	  scrollbar .files.frame0.vscrollbar \
	    -command {.files.frame0.listbox yview}
	 button .files.close -text "Close" -command {destroy .files}

	  pack append .files.frame0 \
	    .files.frame0.listbox { left expand fill } \
	    .files.frame0.vscrollbar { right fill }
	  pack .files.close
	  pack .files.frame0

	  cd $path
	  foreach i [exec ls -a [exec pwd]] {
     	     if { [string compare $i "."] != 0 && [string compare $i ".."] != 0 } {
	        .files.frame0.listbox insert end $i
	     }
	  }

	
	bind .files.frame0.listbox <Double-ButtonPress-1> {
	        set file_name [selection get]
		destroy .files
	}
tkwait window .files
return $file_name
}
set dd [file_listbox {.}]
puts $dd
exit