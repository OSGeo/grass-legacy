lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1 
#package require http

set formpath $env(GISBASE)/etc/form/ 
source $formpath/html_library.tcl

set submit_result ""
set submit_msg ""
set html ""

set nb [NoteBook .nb]
$nb configure -width 300 -height 500
pack .nb -fill both -expand yes

proc create_submit_msg { formid  }  {
    global submit_result submit_msg formf

    destroy $formf($formid).sbw 
    destroy $formf($formid).sbt

    if { $submit_result == 1 } { set color "green" } else { set color "red" }
    set sbw [ScrolledWindow $formf($formid).sbw -relief sunken -borderwidth 2]
    set sbt [text $formf($formid).sbt -height 3 -width 20 -foreground $color ]
    pack $sbw $sbt -fill x
    $sbw setwidget $sbt
    $sbt insert end $submit_msg
    $sbt configure -state disabled
}

proc add_form { formid title } {
    global nb formf html

    set form($formid) [$nb insert end $formid -text $title]
    $nb raise $formid
    set formf($formid) [ frame $form($formid).frm ]
    set formsw($formid) [ScrolledWindow $formf($formid).sw -relief sunken -borderwidth 2]
    set formtxt($formid) [ text $formf($formid).txt -height 5 -width 20 ]
    pack $formf($formid) $formsw($formid) $formtxt($formid) -fill both -expand yes
    $formsw($formid) setwidget $formtxt($formid)
    HMinit_win $formtxt($formid)
    HMparse_html $html "HMrender $formtxt($formid)"
    $formtxt($formid) configure -state disabled
}

proc clear_nb { }  {
    global submit_msg
 
    set submit_msg ""
  
    foreach frm [ .nb pages ] {
        .nb delete $frm
    }
}

proc HMsubmit_form {win param query} {
    global submit_result submit_msg

    regexp -- {\.nb\.f(.+)\.frm\.txt} $win r formid 
    #puts "win = $win formid = $formid"

    reset_values
    foreach {col val} $query {
        #puts "$col : $val" 
        set_value $col $val 
    }

    submit $formid
    #puts "result = $submit_result msg = $submit_msg" 
    create_submit_msg $formid   
}

bind . <Destroy> { if { "%W" == "."} { close_form } }
