package provide pkgLogenMenu 1.0

namespace eval LogenMenu {
 
}


# -------
# procedure to initialise menu section
# -------
proc LogenMenu::procGUI_Menu {} {
    frame .frmMenu
    
    # top level menu bar
       
    menubutton .frmMenu.mnuFile -text "File" \
       -menu .frmMenu.mnuFile.m -underline 0 
       
    menubutton .frmMenu.mnuAnalyse -text "Analyse" \
       -menu .frmMenu.mnuAnalyse.m
       
    menubutton .frmMenu.mnuSettings -text "Settings" \
       -menu .frmMenu.mnuSettings.m
       
    menubutton .frmMenu.mnuOptions -text "View" \
       -menu .frmMenu.mnuOptions.m


    menubutton .frmMenu.mnuAbout -text "About" \
       -menu .frmMenu.mnuAbout.m -underline 0 
       
    # -------------------- append to the menu bar
    pack append .frmMenu \
      .frmMenu.mnuFile {left frame w} \
      .frmMenu.mnuAnalyse {left frame w} \
      .frmMenu.mnuSettings {left frame w} \
      .frmMenu.mnuOptions {left frame w} \
      .frmMenu.mnuAbout {right frame w}
    
  
    # -------------------- file menu
    menu .frmMenu.mnuFile.m -tearoff 0
    .frmMenu.mnuFile.m add command -label "Open..." -command procOpenFile -accelerator Meta+o
    .frmMenu.mnuFile.m add command -label "Reopen" -command procReOpenFile -state disabled
    .frmMenu.mnuFile.m add command -label "Save" -command procSaveAnn
    .frmMenu.mnuFile.m add sep
    .frmMenu.mnuFile.m add command -label "Quit" -command {destroy .} -accelerator Meta+q
    
   bind . <Meta-o> {procOpenFile}
   bind . <Meta-r> {procReOpenFile}
   bind . <Meta-q> {destroy .}

    # -------------------- Analyse menu
    menu .frmMenu.mnuAnalyse.m -tearoff 0
   .frmMenu.mnuAnalyse.m add command -label "Do Simple BTA" \
      -command procSimpleBTA -state disabled
   .frmMenu.mnuAnalyse.m add command -label "Check Filter Declarations" \
       -command procMSVAnalysisOnSource -state disabled
   .frmMenu.mnuAnalyse.m add command -label "(Re)do BTA" \
      -command procFARAnalysis -state disabled
    .frmMenu.mnuAnalyse.m add sep
   .frmMenu.mnuAnalyse.m add command -label "Run Specialised Program for Query" \
       -command procRunSpecProgram -state disabled
   

         
         
    # -------------------- Settings menu
    menu .frmMenu.mnuSettings.m -tearoff 0
    .frmMenu.mnuSettings.m add command -label "Preferences..." \
          -command {procSetPreferences "general"} -underline 0
   .frmMenu.mnuSettings.m add cascade -label "Font" \
        -menu .frmMenu.mnuSettings.m.mnuFont

    global useNewAnn
    #default new annotations to on from now on
    set useNewAnn 1
   .frmMenu.mnuSettings.m add checkbutton -label "New Annotations" \
         -variable useNewAnn -offvalue 0 -onvalue 1

    
    .frmMenu.mnuSettings.m add sep
    .frmMenu.mnuSettings.m add command -label "Clear Memo Table Now" \
	-command {prolog "delete_memo_table('$strFilename')"}
 

    #font menu
    menu .frmMenu.mnuSettings.m.mnuFont -tearoff 0
    
    global chosefontval
    foreach font {"Courier 10" "Courier 12" "Courier 14" "Courier 16" "Courier 18"} {
	.frmMenu.mnuSettings.m.mnuFont  add radiobutton -label "$font"  -variable chosefontval -value $font \
	       -command "changeFont {$font}" 
    }
    
	
         
         
    # -------------------- VIEW menu
    global viewVal
    set viewVal 0
    menu .frmMenu.mnuOptions.m -tearoff 0
   .frmMenu.mnuOptions.m add radiobutton -label "Prolog Source" -command {procShowSourceFile} \
        -variable viewVal -value 1 -state disabled
   # .frmMenu.mnuOptions.m add radiobutton -label "Annotation" -command {procShowAnnFile} \
        -variable viewVal -value 2 -state disabled
   .frmMenu.mnuOptions.m add radiobutton -label "Generating Extension" -command {procShowGenexFile} \
        -variable viewVal -value 2 -state disabled
   .frmMenu.mnuOptions.m add radiobutton -label "Memo Table" -command {procShowMemoFile} \
        -variable viewVal -value 3 -state disabled
    
    # -------------------- About menu
    menu .frmMenu.mnuAbout.m -tearoff 0
    .frmMenu.mnuAbout.m add command -label "About Logen" -command "procAboutLogen" -underline 0
    .frmMenu.mnuAbout.m add command -label "Check for Updates" -command "procCheckForUpdates" -underline 0
   

   

    # ----- fix the menu bar
   # steve pack append . .frmMenu {top fillx}

}