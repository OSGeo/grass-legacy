proc editor { texte  } {

#   if {[catch "open $file_name r" textBoxInFile]} {
#     return
#   }

#  set textBoxMessage [read $textBoxInFile]
#  close $textBoxInFile

   toplevel .editor

   wm geometry .editor +50+50
   wm title .editor { editor }
   
   frame .editor.frame1 
   frame .editor.frame2

   pack append .editor \
          .editor.frame1 { expand } \
          .editor.frame2 { expand }

   text .editor.frame1.texte  \
        -relief sunken \
        -borderwidth 2 \
        -yscrollcommand { .editor.frame1.sb1 set }

   .editor.frame1.texte insert 0.0  $texte

   scrollbar .editor.frame1.sb1 \
       -orient vertical \
       -command { .editor.frame1.texte yview }  

   pack append .editor.frame1 \
       .editor.frame1.sb1 { filly right } \
       .editor.frame1.texte { left expand fill }

   button .editor.frame2.saveas \
       -text { Save as }
     
   button .editor.frame2.print \
       -text { Print }

   button .editor.frame2.quit \
       -text { Quit } \
       -command { destroy .editor }

   pack append .editor.frame2 \
          .editor.frame2.saveas { left } \
          .editor.frame2.print { left } \
          .editor.frame2.quit { left } 

   grab .editor

   tkwait window .editor
} 

