# ----
# Package section
# ---
lappend auto_path "[file dirname [info script]]/tklib/"

package require pkgLogenDialog
package require pkgLogenToolbar
package require pkgLogenParser
package require pkgLogenMenu
package require pkgProlog

# ------------------------------------------------------------
# GUI Section
# ------------------------------------------------------------

global version
set version 0.9.0





proc procQuit {} {
   puts "Quitting"
   prolog logen_preferences:save_preferences('logen_preferences.pl')
}

proc procEdit {} {
    global sourceFrame    
    global sourceEdit

    procCheckIfSaved
    .frmBar.filebar.bSaveAnn configure -state disable
    .frmBar.filebar.bSavePl configure -state normal

    set sourceEdit 1

    .frmBar.editbar.bEdit configure -state disable
    $sourceFrame.text configure -state normal
}

proc myprolog {call} {
    
    prolog "mycall($call, ERR)"    
    set Err $prolog_variables(ERR)
    if { $Err != "ok" } { 
	LogenDialog::Dialog_Error "Prolog Exception" "$Err" {"Ok" ok}
    }
    puts "returned $Err"
    

}



proc procZoom {zoomin} {
    global chosefontval
    set size [lindex [split $chosefontval] 1]
    if { $zoomin } {
	set size [expr {$size + 2}]
    } else {
	set size [expr {$size - 2}]
	
    }
    changeFont "Courier $size"
}




proc procSetStatusText { text } {
    global strStatus
    set strStatus $text
}

proc procEnableItemsAfterSpecialisation {} {
    #puts "spec done"
    # enable Open Destination File
    
	.frmMenu.mnuOptions.m entryconfigure 2 -state normal

	.frmSpec.b3 configure -state normal
}

proc procEnableItemsAfterOpeningFile {} {
    global strFilename
	# enable ReOpen and Specialise Button
	set fileTail [file tail $strFilename]
	.frmMenu.mnuFile.m entryconfigure 1 -state normal \
	   -label "Reopen $fileTail"
    
        .frmBar.filebar.bReopen configure -state normal
	.frmSpec.b3 configure -state disabled
	.frmSpec.b2 configure -state disabled
	.frmSpec.b1 configure -state normal
	
	# enable View menu items and indicate that we view the source file
	global viewVal
	set viewVal 1
	.frmMenu.mnuOptions.m entryconfigure 0 -state normal
	.frmMenu.mnuOptions.m entryconfigure 1 -state disabled
	.frmMenu.mnuOptions.m entryconfigure 2 -state disabled

	.frmMenu.mnuAnalyse.m entryconfigure 0 -state normal


    #reset destination frame
    
    global destFrame

    $destFrame.text configure -state normal  
   $destFrame.text delete 0.0 end    
    $destFrame.text configure -state disabled
}

proc procEnableItemsAfterRunCogen  {} {
    #puts "cogen done"

	.frmSpec.b1 configure -state disabled
	.frmSpec.b2 configure -state normal
	
	.frmMenu.mnuOptions.m entryconfigure 1 -state normal
	.frmMenu.mnuOptions.m entryconfigure 2 -state disabled
}


# -------


proc .top.frmSource args {
    if [regexp {^(ins|del).*} [lindex $args 0]] { procCodeModified }
    uplevel ..top.frmSource $args
}




# -------
# procedure to initialise source code section
# -------
proc procGUI_Source {} {
    global sourceFrame
    global filterFrame
    global destFrame
    global bModified
    global sourceEdit
    
    set bModified 0
    set sourceEdit 0
    # ------- code source frames
  
   
    frame .split 
    frame .split.top 
    frame .split.bottom 
    frame .top 

    set sourceFrame [frame .top.frmSource -borderwidth .1c -relief groove]
    # reset modified 
    scrollbar $sourceFrame.scrolly -command "$sourceFrame.text yview"
    scrollbar $sourceFrame.scrollx -command "$sourceFrame.text xview" -orient h   
    text $sourceFrame.text -yscroll "$sourceFrame.scrolly set" -xscroll "$sourceFrame.scrollx set" -setgrid 1  -state disabled -wrap no -height 0 -width 0


    pack $sourceFrame.scrolly -side right -fill y
    pack $sourceFrame.scrollx -side bottom -fill x
    pack $sourceFrame.text -expand 1 -fill both
    
    #filter Window next to main source window
    set filterFrame [frame .top.frmFilter -borderwidth .1c -relief groove]
    scrollbar $filterFrame.scrolly -command "$filterFrame.text yview"
    scrollbar $filterFrame.scrollx -command "$filterFrame.text xview" -orient h
    text $filterFrame.text -yscroll "$filterFrame.scrolly set" -xscroll "$filterFrame.scrollx set" \
        -setgrid 1 -width 0 -wrap no -height 0

    
    #  proxy all insert calls to the filter frame to keep modified state saved
    rename $filterFrame.text .$filterFrame.text
    proc $filterFrame.text args {
        if [regexp {^(ins|del).*} [lindex $args 0]] { procCodeModified }
	uplevel ..top.frmFilter.text $args
    }

    pack $filterFrame.scrolly -side right -fill y
    pack $filterFrame.scrollx -side bottom -fill x
    pack $filterFrame.text -expand 1 -fill both 

    
    # set up a row to enter specialisation goal and specialise button
    set f [frame .frmSpec]

    button $f.b1 -text "Run Cogen" -default active -state disabled -command {procRunCogen}
    button $f.b2 -text Specialise -default active -state disabled -command {procSpecialise}
    button $f.b3 -text Run -default active -state disabled -command {procRunSpecProgram}
    button $f.b4 -text "Filter Propagation" -default active -state normal -command {procFilterProp}
    button $f.bBTA -text "BTA" -default active -state normal -command {procAutoBTA}

    pack [label $f.label -text {Goal: }] -side left
    pack [entry $f.entry] -side left -fill x -expand true -padx 5
    pack $f.b1 -padx 5 -side right 
    pack $f.b2 -padx 5 -side right
    pack $f.b3 -padx 5 -side right
    pack $f.bBTA -padx 5 -side right
    pack $f.b4 -padx 5 -side right


    prolog "tcltk_get_preference(last_spec_query,PrefSpec)"
    set spec_query $prolog_variables(PrefSpec)
    $f.entry insert 0 "$spec_query"
    
    
    # ------- code source frames
    set destFrame [frame .frmDest -borderwidth .1c -relief groove]
    scrollbar $destFrame.scrolly -command "$destFrame.text yview"
    scrollbar $destFrame.scrollx -command "$destFrame.text xview" -orient h
    text $destFrame.text -yscroll "$destFrame.scrolly set" -xscroll "$destFrame.scrollx set" \
        -setgrid 1 -state disabled -height 0 -width 0
    
    pack $destFrame.scrolly -side right -fill y
    pack $destFrame.scrollx -side bottom -fill x
    pack $destFrame.text -expand 1 -fill both


    #set font to monospaced font (windows defaults to times else)
    #set chosenfont "Courier 10"
    #changeFont $chosenfont
 

    # ------ statusbar
    global strStatus
    
    frame .statusBar
    label .statusBar.lab -text "  " -relief sunken -bd 1 -anchor w -textvariable strStatus
    label .statusBar.foo -width 8 -relief sunken -bd 1 -anchor w 
    
    
    pack .statusBar.lab -side left -padx 2 -expand yes -fill both
    pack .statusBar.foo -side left -padx 2
    

    
    
}

proc changeFontColour {colour} {
    global sourceFrame
    global destFrame
    global filterFrame

    
#    $sourceFrame.text configure -foreground "darkgray"
}

proc changeFont {chosenfont} {
    global sourceFrame
    global filterFrame
    global destFrame
    global chosefontval

   $sourceFrame.text configure -font $chosenfont 
    $filterFrame.text configure -font $chosenfont 
    $destFrame.text configure -font $chosenfont 
    set chosefontval $chosenfont
    prolog "set_preference(font_size,'$chosenfont')"
}

proc procAutoBTA {} {
    global strFilename
    global sourceFrame
    
    $sourceFrame.text tag remove unsafe 1.0 end

    puts "Automatic Binding Time Analysis..."
    #set unsafe [exec "./bta_binsolve" "$strFilename"]
    set unsafe [exec "sicstus" "-l" "bta_driver.pl" "--goal" "auto_bta('$strFilename'),halt." 2> /dev/null]
    puts $unsafe   
    set unsafe [split [string trim $unsafe {[]}] ","]
    puts $unsafe   
    if { [llength $unsafe] == 0 } {
	puts "Program Terminates!"
    } else {
	LogenParser::procFormatSource $unsafe $sourceFrame

    }
    
   
}

proc procFilterProp {} {
    global filterFrame
    global strAnnFilename

    set FileName "temp"
 #   cd "/Users/mv/cvs_server/soton/cogen2/logen_source"
    set result [exec ./bta/auto_bta "$strAnnFilename" ./bta/logenbta.pl "$FileName"]

   $filterFrame.text delete 0.0 end
    # open file
    set fid [open $FileName r]
    $filterFrame.text insert 0.0 [read $fid]    
    close $fid    
    
    
    # save and load file to make pretty colours
    procSaveAnn
    procReOpenFile
    


}


proc proc_setup_ann_tags {} {
    global sourceFrame
    global filterFrame

    global annotation_tags
    global annotation_colour
    global annotation_menu
    global useNewAnn
    global last_annotation


    # ANY NEW ANNOTATIONS MUST BE LISTED HERE FOR SAVING/LOADING TO WORK
    # All annotation tags should be defined here
    set annotation_tags [list unfold memo call rescall if resif mcall ucall unknown]
    
    # the colour of the menu item and annotated text
    set annotation_colour(unfold) "darkgreen"
    set annotation_colour(ucall) "darkgreen"
    set annotation_colour(call) "darkgreen"
    set annotation_colour(if) "darkgreen"
    set annotation_colour(rescall) "darkred"
    set annotation_colour(mcall) "darkred"
    set annotation_colour(memo) "darkred"
    set annotation_colour(resif) "darkred"
    set annotation_colour(unknown) "blue"
    set last_annotation ""




    #the menu to display on click - grouping of types
    set annotation_menu(unfold) "call"
    set annotation_menu(call) "call"
    set annotation_menu(memo) "call"
    set annotation_menu(rescall) "call"
    set annotation_menu(ucall) "call"
    set annotation_menu(mcall) "call"
    set annotation_menu(unknown) "call"

    set annotation_menu(if) "if"
    set annotation_menu(resif) "if"

    #create all the needed menus
    foreach menu_name [array names annotation_menu] {
	menu $sourceFrame.text.$menu_name -tearoff 0	
    }
    
    #setup the tags
    foreach ann_tag $annotation_tags {
	$sourceFrame.text tag configure $ann_tag -foreground $annotation_colour($ann_tag)
	$sourceFrame.text tag bind $ann_tag <Button-1> {
	    global sourceEdit
	    if { !$sourceEdit } {
		proc_mouse_over %X %Y %x %y	true
	    }
	}

	$sourceFrame.text tag bind $ann_tag <Motion> {	
	    global sourceEdit
	    if { !$sourceEdit } {
		proc_mouse_over %X %Y %x %y	false
	    }
	}

	$sourceFrame.text tag bind $ann_tag <Leave> {
	    # do something on leaving an annotation?
	    $sourceFrame.text tag remove hilite 1.0 end
	    set last_annotation ""
	    procSetStatusText ""
	}


	
	$sourceFrame.text.$annotation_menu($ann_tag) add command -label $ann_tag -foreground $annotation_colour($ann_tag) -command "procChangeAnn $ann_tag" -underline 0 
    }


    #syntax tags

    
    foreach Frame "$sourceFrame $filterFrame .frmDest" {
	$Frame.text tag configure comment -foreground "hotpink1"
	$Frame.text tag configure head -foreground "blue"
	$Frame.text tag configure unsafe -background "red"

	$Frame.text tag configure list -foreground "purple"

    }

    #if you click on the head annotation in the filter text widget it
    #should put the goal into the specialise entry
    $filterFrame.text tag bind head <Button-1> {
	set curRange [Text_CurrentRange $filterFrame.text head [$filterFrame.text index @%x,%y]]
	
	  .frmSpec.entry delete 0 end 
        .frmSpec.entry insert 0 [$filterFrame.text get [lindex $curRange 0] [lindex $curRange 1]]

    }


    $sourceFrame.text tag configure hilite -background "grey"    
   $sourceFrame.text tag configure errorTag -background "red"    

    $sourceFrame.text tag configure unknown -background "black" -foreground "white"

    $sourceFrame.text tag configure hide_nf -background "yellow"    
}


#from Book:
proc Text_CurrentRange { t tag mark} {
    set range [$t tag prevrange $tag $mark]
    set end [lindex $range 1]
    if {[llength $range] == 0  || [$t compare $end < $mark]} {
	#at very begining
	set range [$t tag nextrange $tag $mark]
	if {[llength $range] == 0 || [$t compare $mark < [lindex $range 0]]} {
	    return {}
	}
    }
    return $range

}

#remove old annotation tag and replace it with a new one
proc procChangeAnn { newann } {
    global sourceFrame
    global strSelectedAnn 


    #set the modified flag, text has changed

    procCodeModified

    set ann [lindex $strSelectedAnn 0]
    set ann_start [lindex $strSelectedAnn 1]
    set ann_end [lindex $strSelectedAnn 2]   
    $sourceFrame.text tag remove $ann $ann_start $ann_end
    $sourceFrame.text tag add $newann $ann_start $ann_end


}


#get the current annotation with start and end info
#tagname start end
proc get_annotation_at_index {index} {
    global annotation_tags
    global sourceFrame 
    set curann [$sourceFrame.text tag names $index ]
        
    # We have list of annotations is one of them a valid ann?
    foreach ann $curann {
	if { [lsearch -exact $annotation_tags $ann] >= 0} {   
	    # we have a valid index...
	    return "$ann [get_tag_position $ann $index ]"
	}	  	
    }
    
    # no valid annotation found
    return ""
}

#get start and end of the tag ann at index
proc get_tag_position { ann index } {
    global sourceFrame

    set newindex [$sourceFrame.text index "$index + 1 char"]
    return [$sourceFrame.text tag prevrange $ann $newindex ]
}

# A mouse over event on one of our annotations, identify the annotation 
proc proc_mouse_over {X  Y x y ShowMenu} {   
    global annotation_menu       
    global sourceFrame
    global last_annotation

    set pos_index  [$sourceFrame.text index @$x,$y ]     
    set curann [get_annotation_at_index $pos_index]
    
    #ensure we have a valid annotation
    if { $curann == "" } {
	#no annotation found
	#puts "Cant decide on annotation here??"
	return
    }
        
    set ann [lindex $curann 0]
    set ann_start [lindex $curann 1]
    set ann_end [lindex $curann 2]

    if { $last_annotation != "" } {
	#remove last annotation	
	$sourceFrame.text tag remove hilite 1.0 end
    }
    $sourceFrame.text tag add hilite $ann_start $ann_end
    set last_annotation $curann
    
    procSetStatusText "$ann - $ann_start -> $ann_end"

    # show menu if button was pressed
    if { $ShowMenu } {
	global strSelectedAnn 
	set strSelectedAnn $curann
	tk_popup $sourceFrame.text.$annotation_menu($ann) $X $Y    
    }
}


# -------
# procedure to initialise main GUI
# -------
proc procInitGUI {} {
    global sourceFrame
    global filterFrame
    global examplesPath
    global destFrame
   
    global version

    prolog "tcltk_get_preference(examples_path,PATH)"
    set examplesPath $prolog_variables(PATH)
    #set examplesPath $::env(logen_examples_directory)
    
    

    wm title . "LOGEN $version"
   
    wm iconname . "Logen"

    LogenMenu::procGUI_Menu

    LogenToolbar::procToolbar
    procGUI_Source

    
    grid columnconfigure . 0 -weight 1
    grid rowconfigure . 0 -minsize 5m
    grid rowconfigure . 1 -minsize 5m
    grid rowconfigure . 2 -weight 1
    grid rowconfigure . 3 -minsize 5m
    
    grid .frmMenu -row 0 -sticky news
    grid .frmBar -row 1 -stick news
    grid .split -row 2 -stick news
    grid .statusBar -row 3 -sticky ew
    
    

    #topPane will contain Source, Filter and Spec bar
    grid columnconfigure .split.top 0 -weight 1 
    grid rowconfigure .split.top 0 -weight 1
    grid rowconfigure .split.top 1 -minsize 10m    
    grid rowconfigure .split.top 2 -minsize 1m    

    grid .top -row 0 -sticky news -in .split.top
    grid .frmSpec -row 1 -sticky news -in .split.top

    #dest pane

    grid columnconfigure .split.bottom 0 -weight 1
    grid rowconfigure .split.bottom 0 -minsize 1m
    grid rowconfigure .split.bottom 1 -weight 1

    grid .frmDest -row 1 -sticky news -in .split.bottom
    


# arrange the frames *new*

    Pane_Create $sourceFrame $filterFrame -percent 0.75 -orient horizontal -in .top
    Pane_Create .split.top .split.bottom -percent 0.6 -orient vertical -in .split


   
    wm geometry . 80x50 


    
    # now main pain is topPane, DestPane and status bar



    



    
    #new using grid...
#    grid columnconfigure . 0 -minsize 60p -weight 5
#    grid columnconfigure . 1 -minsize 20p -weight 2
    
    #menu bar row
#    grid rowconfigure . 0 -minsize 5m
    #SOurce and Filter    
#    grid rowconfigure . 1 -weight 1 -minsize 10m
#    grid rowconfigure . 2 -minsize 10m
#    grid rowconfigure . 3 -weight 1 -minsize 10m
    #statusbar
#    grid rowconfigure . 4 -minsize 5m

#    grid .frmMenu -row 0 -column 0 -columnspan 2 -sticky news

#    grid .top -column 0 -row 1 -sticky news -columnspan 2
#    grid .frmSpec -column 0 -row 2 -sticky news -columnspan 2
#    grid $destFrame -column 0 -columnspan 2 -row 3 -sticky news
#    grid .statusBar -column 0 -row 4 -columnspan 2 -stick news

    
    #pack .bottom -side bottom -expand yes
    #pack .top -side top  -expand yes
 
    #pack .statusBar -fill both -side bottom
    #pack .frmSpec .frmDest -fill both -side top
    
    #pack $filterFrame -side right -fill both -expand yes
    #pack $sourceFrame -side left -fill both -expand yes

  
    procAboutLogen

}

# -------
# procedure to open a file, and then execute it on prolog
# -------
proc procOpenFile {} {
    global strFilename
    global examplesPath
    
    set oldFile $strFilename
    procCheckIfSaved
    
    #set examplesPath $::env(logen_examples_directory)
    

    set types {
    	{"Prolog Files"		{.pl}	}
    	{"XSB Files"		{.P}	}
    	{"BIM Files"		{.pro}	}
    	{"Ecce Files"		{.xce}	}
	    {"All files"		*}
    }
    # show the dialog box
    set strFilename [tk_getOpenFile -filetypes $types -initialdir $examplesPath -parent . ]
    if {$strFilename != ""} {
	#remember the directory for next time
	set examplesPath [file dirname $strFilename]
        procLoadFile
    } else {
	# if the filename is empty then use old one
	set strFilename $oldFile
    }
}


proc procLoadFile {} {
    global strFilename strGxFilename strAnnFilename
    global useNewAnn
    global destFrame
    set rootName [file rootname $strFilename]
    append app2Name $rootName ".pl.gx"
    
    set strGxFilename $app2Name
    
    global version
    if [file exists $strFilename] {
	        wm title . "LOGEN $version: \[[file tail $strFilename]\]"
            # the file exists, so show it.

	if { $useNewAnn } {
	    # and this one for the new...
	    procShowAnnotatedSourceCode $strFilename
	    set strAnnFilename $strFilename
	    append strAnnFilename ".ann"
	    

	    $destFrame.text configure -state normal
	    $destFrame.text delete 0.0 end
	    $destFrame.text configure -state disabled

	} else {
	    # use this line for older annotations
	    procShowSourceCode $strFilename
	}
        
	
	
            
             procEnableItemsAfterOpeningFile
    
        } else {
	        tk_messageBox -parent . -icon error \
        		-message "File $strFilename does not exist."

        }
    
}

proc procShowSourceFile {} {
#    global strFilename strGxFilename
#    procShowSourceCode $strFilename
}
proc procShowGenexFile {} {
    global strFilename  strGxFilename
    procShowDestCode $strGxFilename
}
proc procShowMemoFile {} {
    global specRootName
    set memoName {}
    append memoName $specRootName ".memo"
    procShowDestCode $memoName
}

proc procShowFile { filename} {
    if [file exists $filename] {
        procShowSourceCode $filename
    
        } else {
	        tk_messageBox -parent . -icon error \
        		-message "File $filename does not exist."
        }
}


proc procSaveCode {} {
    global sourceFrame
    global strFilename
    global sourceEdit


    if { $sourceEdit } {
	
	set text [$sourceFrame.text get 1.0 end]
	#output to file here instead
	set fid [open $strFilename w]
	puts -nonewline $fid $text    
	close $fid
	
	set ans [LogenParser::parseProlog $sourceFrame  $strFilename]

	if { $ans == 0 } {

	    #dont ReOpenFile if theres a parse error
	    return
	}
    }    
    set sourceEdit 0

    .frmBar.editbar.bEdit configure -state normal 
    .frmBar.filebar.bSavePl configure -state disable
    procReOpenFile
    
    procCodeModified
    #we must save the ann file to have any actual effect on specialisation
   ## However its a pain when its not correctly syntax... procSaveAnn 
}

proc procBackup { strFilename } {
    # this proc for incremental backup of all saved annotation files
    # you may get the idea that I don't always trust logen to not 
    # destroy my file.... and experience would agree (this should be 
    # in preferences)

    set backupdir [file join [file dirname $strFilename] backup]
    if {[file exists $backupdir] == 0} {
	file mkdir $backupdir
    }
    
    append backupname [file tail [file rootname $strFilename]] [clock seconds] ".pl.ann"

    set annfile [append foo $strFilename ".ann"] 
    puts $annfile
    if {[file exists $annfile] == 1} {
	file copy $annfile [file join $backupdir $backupname]
    }
    
    

 

}

proc procSaveAnn {} {
    global sourceFrame
    global filterFrame
    global strFilename
    global bModified
    
    # before doing anything we make a backup
    procBackup $strFilename
    
    set annotations ""
    
   set hidenf_ranges [$sourceFrame.text tag ranges hide_nf]
   set block_index 0
   set start_block [lindex $hidenf_ranges [expr {$block_index *2} ]]
   set end_block [lindex $hidenf_ranges [expr {$block_index *2+1} ]]

   puts "HideNf: $hidenf_ranges" 
   puts "start: $start_block, $end_block" 

   set i 1.0
   set ann [getNextAnnotation $i]

   while {$ann != ""} {
       puts "ann:$ann"
       set i [lindex $ann 2]  
      puts "S: $start_block I: $i"
      puts "if $i >= $start_block"
       if {$start_block != "" && [$sourceFrame.text compare $i >= $start_block] } {
	   puts "true"
	   append annotations "start_hide_nf -1 -1 "
	   set block_index [expr {$block_index + 1}]
	   set start_block [lindex $hidenf_ranges [expr {$block_index *2} ]]
       }
       puts "false"

       append annotations $ann " "

       if {$end_block != "" && [$sourceFrame.text compare $i >= $end_block] } { 
	   append annotations "end_hide_nf -1 -1 "
   	   set end_block [lindex $hidenf_ranges [expr {$block_index *2+1} ]]
       }
      

       
      
       set ann [getNextAnnotation $i]
   }

    if {$end_block != "" && [$sourceFrame.text compare $i >= $end_block] } { 
	   append annotations "end_hide_nf -1 -1 "
   	   set end_block [lindex $hidenf_ranges [expr {$block_index *2+1} ]]
    }

    puts "ALL Ann: $annotations"
    set filters [$filterFrame.text get 1.0 end]
    
   
   prolog "save_ann('$strFilename','$annotations', '$filters')"                 
    set bModified 0

    # hope everything worked as we now reload the file
   procReOpenFile
}
#get the next valid ann from position index
# returns ANN Start End
proc getNextAnnotation {index} {
    global sourceFrame
    global annotation_tags


    
    set nextStart [$sourceFrame.text index end]
    set start ""

    # get first occurence of next annotation
    foreach tag $annotation_tags {
	set i [$sourceFrame.text tag nextrange $tag $index]	
	set curStart [lindex $i 0]
	
	if { $curStart != "" } {
	    if { [$sourceFrame.text compare $curStart < $nextStart] } {
		set nextStart $curStart
		set start [lindex $i 0]
	    }
	}
    }


    if {$start == ""} {
	return ""
    }

    return [get_annotation_at_index $start]
}

# ---------------------------------------------- reload
proc procReOpenFile {} {
    global strFilename
    if {$strFilename != ""} {
	procCheckIfSaved
        procLoadFile
    }
}

# ---------------------------------------------- load destination
proc procOpenDestFile {} {
    global strFilename
    if {$strFilename != ""} {
        #set rootName [file rootname $strFilename]
        set specName {}
        append specName $strFilename ".xce"
        set strFilename $specName
        procLoadFile
    }
}



# -------
# procedure to reset the animation
# -------

proc procSimpleBTA {} {
    global strFilename
    #set rootName [file rootname $strFilename]
    set annName {}
    #append annName $rootName ".ann"
    append annName $strFilename ".ann"

    puts "Simple BTA: $strFilename $annName"
    if [prolog "annotateFile('$strFilename','$annName')"] {
#	        tk_messageBox -parent . -icon error \
        		-message "Annotated to $annName. Open this file to use it."
	         procShowSourceCode $strFilename
    } else {
	        tk_messageBox -parent . -icon error \
        		-message "Could not annotate $strFilename to $annName."
    }
}

proc procCodeModified {} {
    global bModified 
    set bModified 1
    
    .frmBar.filebar.bSaveAnn configure -state normal
    # reset buttons if you are modifing text
    procEnableItemsAfterOpeningFile
    
}

proc procCheckIfSaved {} {
    global bModified
    #puts "Modified flag: $bModified" 
    if { $bModified } {
	global sourceFrame
	set ans [LogenDialog::Dialog_Error "Save?" "Changes are not saved.\nSave now?" \
		     {"Save" save} {"Cancel" cancel}]

#	set ans [tk_messageBox -default yes -message "Source File is not saved.  Save now?" -title "Warning" -type yesno -icon warning]
	
	if { $ans == "save" } {
	    puts "Saving"
	    procSaveAnn
	}
    }    
}

proc procRunCogen {} {
    global strFilename strGxFilename strAnnFilename useNewAnn 
    # prolog "run_cogen_and_save_wo_expansion('$strAnnFilename','$strGxFilename')"
    
    procCheckIfSaved

    if { $useNewAnn } {
	prolog "run_cogen_and_save_wo_expansion('$strAnnFilename','$strGxFilename')"
    } else {
	prolog "run_cogen_and_save_wo_expansion('$strFilename','$strGxFilename')"
    }

    procShowDestCode $strGxFilename
    prolog "load_gx_wo_expansion('$strGxFilename')"
    procEnableItemsAfterRunCogen
    procShowErrors

}



proc procSpecialise {} {
    global strFilename strAnnFilename strGxFilename
    set rootName [file rootname $strFilename]
    global specRootName
#    set specRootName {}
     set specRootName  $strFilename

#    append specRootName $strFilename ".ann"
    set goal [.frmSpec.entry get]
    if [prolog "atom_ok_to_specialise($goal)"] {
#      puts "Specialising $strFilename for $goal"

#    NEW MODULE SPEC STUFF --Steve
#      prolog "run_gx_and_save_wo_expansion($goal,'$specRootName')"
	
	prolog "moduledriver:rebuild_everything"
	prolog "moduledriver:request_pattern('$specRootName', $goal)"	
	prolog "moduledriver:specialize"
	
      prolog "set_preference(last_spec_query,$goal)"
      # prolog "do_nothing($goal,'$specName')"
      # prolog "run_gx($goal)"
#      puts "done"
    } else {
	LogenDialog::Dialog_Error "Invalid Specialisation Goal" "The call $goal has no filter declaration.\n\nPlease check specialisation goal and add filter declaration if needed." {"Ok" ok}

    }
    set specName {}

    append specName [file rootname $specRootName] ".spec"

    if { [file exists $specName] } {
	procShowDestCode $specName
	procEnableItemsAfterSpecialisation
    } else {
	LogenDialog::Dialog_Error "Specialisation Failed" "No code was created bny the specialiser, please investigate the prolog error messages"

    }

    procShowErrors
}

proc procRunSpecProgram {} {
    global specRootName
    set memoName {}
    append memoName $specRootName ".memo"
    set goal [.frmSpec.entry get]
    prolog "tcltk_run_spec_program($goal,'$memoName')"
    procShowErrors
}


# ------------------------------------------------------------
# Functional  Section
# ------------------------------------------------------------
# -------
# procedure to open a file, and put it on the source code
# -------
proc procShowSourceCode {sFileName} {
    global sourceFrame

    $sourceFrame.text configure -state normal
    $sourceFrame.text delete 0.0 end
    # open file
    set fid [open $sFileName r]
    $sourceFrame.text insert 0.0 [read $fid]    
    close $fid

    set PARSE [LogenParser::parseProlog $sourceFrame  $sFileName]
    $sourceFrame.text configure -state disabled    

    #puts "procShowSourceCode returns $PARSE"
    return $PARSE
}

proc procLoadFilters {sFilters} {
    global filterFrame

    $filterFrame.text configure -state normal
    $filterFrame.text delete 0.0 end

    $filterFrame.text insert 0.0 $sFilters    
    # $filterFrame.text configure -state disabled    
}

proc procShowAnnotatedSourceCode {sFile} {    
    global sourceFrame
    global filterFrame
    global bModified 

    if { ! [procShowSourceCode $sFile] } {
	# parsing must have failed
	# An error message would have been given by parser	
	LogenDialog::Dialog_Error "Parse Error" "Unable to annotate code." {"Ok" ok}
        return
    }

    if { ! [file exists "$sFile.ann"] } {
	puts "Ann File Does not exist"

	set ans [LogenDialog::Dialog_Error "Annotations file not found" "Do you wish to run simple BTA?\n\nAlternatively all calls will be annotated as 'unknown'." {"Simple bta" simple} {"Mark Unknown" unknown}]

	
	if { $ans == "simple"} {
	    procSimpleBTA
	} else {
	    set fid [open "$sFile.ann" w]
	    puts $fid "%empty ann file"
	    close $fid
	}
	

    }
    prolog "request_ann('$sFile',  Format, Syntax, Filter, FilSyntax)"
    set lFormat $prolog_variables(Format)
    set lSyntax $prolog_variables(Syntax)
    set lFilter $prolog_variables(Filter)
    set lFilterSyntax $prolog_variables(FilSyntax)
    
    procLoadFilters $lFilter
    procShowSourceCode $sFile

#removed need for format, now done seperately in parser
    LogenParser::procFormatSource $lFormat $sourceFrame
    LogenParser::procFormatSource $lFilterSyntax $filterFrame
    $filterFrame.text configure -state normal

    
    set bModified 0
    .frmBar.filebar.bSaveAnn configure -state disable
    
}







proc procShowDestCode {sFileName} {
    global destFrame
    $destFrame.text configure -state normal
    $destFrame.text delete 0.0 end
    # open file
    set fid [open $sFileName r]
    $destFrame.text insert 0.0 [read $fid]    
    close $fid
    $destFrame.text configure -state disabled
    
    # return -1 if parsing failed
    return [LogenParser::parseProlog .frmDest  $sFileName]
}




# A simple Dialog
# ---------------

proc Dialog_Create {top title args} {
	global dialog
	if [winfo exists $top] {
		switch -- [wm state $top] {
			normal {
				# Raise a buried window
				raise $top
			}
			withdrawn -
			iconified {
				# Open and restore geometry
				wm deiconify $top
				catch {wm geometry $top $dialog(geo,$top)}
			}
		}
		return 0
	} else {
		eval {toplevel $top} $args
		wm title $top $title
		return 1
	}
}
proc Dialog_Wait {top varName {focus {}}} {
    upvar $varName var
    
    # Poke the variable if the user nukes the window
    bind $top <Destroy> [list set $varName $var]

    # Grab focus for the dialog
    if {[string length $focus] == 0} {
	set focus $top
    }
    set old [focus -displayof $top]
    focus $focus
    catch {tkwait visibility $top}
    catch {grab $top}

    set y [expr {[winfo rooty .] + [winfo height .]/3 }]
    set x [expr {[winfo rootx .] + [winfo width .]/4 }]
    wm geometry $top "+$x+$y"

	# Wait for the dialog to complete
	tkwait variable $varName
	catch {grab release $top}
	focus $old
}
proc Dialog_Dismiss {top} {
	global dialog
	# Save current size and position
	catch {
		# window may have been deleted
		set dialog(geo,$top) [wm geometry $top]
		wm withdraw $top
	}
}


#
# Example 33-2
# A simple dialog.
#

proc Dialog_Prompt { string } {
	global prompt
	destroy .prompt
	set f .prompt
	if [Dialog_Create $f "Prompt" -borderwidth 10] {
		message $f.msg -text $string -aspect 1000
		entry $f.entry -textvariable prompt(result)
		set b [frame $f.buttons]
		pack $f.msg $f.entry $f.buttons -side top -fill x
		pack $f.entry -pady 5
		button $b.ok -text OK -command {set prompt(ok) 1}
		button $b.cancel -text Cancel \
			-command {set prompt(ok) 0}
		pack $b.ok -side left
		pack $b.cancel -side right
		bind $f.entry <Return> {set prompt(ok) 1 ; break}
		bind $f.entry <Control-c> {set prompt(ok) 0 ; break}
	}
	set prompt(ok) 0
	Dialog_Wait $f prompt(ok) $f.entry
	Dialog_Dismiss $f
	if {$prompt(ok)} {
		return $prompt(result)
	} else {
		return {}
	}
}

# ------------------------------------------------------------

proc Create_Settings_Dialog {} {
   toplevel .settingsDlg -borderwidth 10
   
    # set up a row to enter specialisation goal and specialise button
    set f [frame .settingsDlg]
    pack [label $f.label -text {Goal: }] -side left
    pack [entry $f.entry] -side left -fill x -expand true
    button $f.b1 -text Specialise -default active -command {procSpecialise}
    pack $f.b1 -padx 10 -side right
    $f.entry insert 0 {nont(a,X,Y)}
}

# ------------------------------------------------------------

proc procAboutLogen {} {
    global sourceFrame


    #procShowSourceCode "../logen_readme.txt"
    set sFileName "../logen_readme.txt"
    
    $sourceFrame.text configure -state normal
    $sourceFrame.text delete 0.0 end
    # open file
    set fid [open $sFileName r]
    $sourceFrame.text insert 0.0 [read $fid]    
    close $fid

    


    image create photo logen_img -format gif -fil "./icons/Logen.gif"
    $sourceFrame.text configure -state normal
    $sourceFrame.text insert 1.0 "\n"
    $sourceFrame.text image create 1.0 -image logen_img
    $sourceFrame.text configure -state disable
    

}
proc procCheckForUpdates {} {
    global version
    set prob_update_url "http://www.ecs.soton.ac.uk/~mal/systems/logen_Download/"
    package require http
    puts "Opening http connection"
    set token [::http::geturl "$prob_update_url/current_version.txt"]
    set data [::http::data $token]
    puts "data $data"
    ::http::cleanup $token
    set webversion [lindex [split $data] 0]
    puts "Webversion $webversion"
    if {![string is digit [string index $webversion 0]]} {
           tk_messageBox -message "Error connecting to $prob_update_url. Message: $data."
    } elseif {$webversion!=$version} {
           global tcl_platform
           set webdate [lindex [split $data] 1]
           if {$tcl_platform(platform)!="unix"} {
               tk_messageBox -message "Version $webversion of Logen is available as of $webdate.\nGo to $prob_update_url to obtain the update."
           } else {
             tk_messageBox -message "Version $webversion of Logen is available as of $webdate.\nI will try to open $prob_update_url in browser."
             exec open $prob_update_url
           }
    } else {
           tk_messageBox -message "Your version of Logen ($version) is up-to-date."
   }
}
# ------------------------------------------------------------
# MAIN PROGRAM
# ------------------------------------------------------------
# -------
# procedure initialise everythings
# -------
proc procMainInit {} {
    # initialise GUI
    prolog logen_preferences:init_and_load_preferences('logen_preferences.pl')
    procInitGUI
    
    prolog "tcltk_get_preference(font_size,PrefFont)"
    set chosenfont $prolog_variables(PrefFont)
    changeFont $chosenfont
    
    proc_setup_ann_tags
    
    # initialise prolog
    prolog tcltk_initialise
    
    
    procShowErrors

    global strFilename
    set strFilename ""
}

proc Pane_Create {f1 f2 args} {            
    # Map optional arguments into array values   
    set t(-orient) vertical    
    set t(-percent) 0.5   
    set t(-in) [winfo parent $f1]
    array set t $args

    # Keep state in an array associated with the master frame
    set master $t(-in)
    upvar #0 Pane$master pane
    array set pane [array get t]

    # Create the grip and set placement attributes that
    # will not change. A thin divider line is achieved by
    # making the two frames one pixel smaller in the
    # adjustable dimension and making the main frame black.
    
    set pane(1) $f1
    set pane(2) $f2
    set pane(grip) [frame $master.grip -background gray50 \
			-width 10 -height 10 -bd 1 -relief raised \
			-cursor crosshair]
    if {[string match vert* $pane(-orient)]} {
	set pane(D) Y;# Adjust boundary in Y direction
	place $pane(1) -in $master -x 0 -rely 0.0 -anchor nw \
	    -relwidth 1.0 -height -1
	place $pane(2) -in $master -x 0 -rely 1.0 -anchor sw \
	    -relwidth 1.0 -height -1
	place $pane(grip) -in $master -anchor c -relx 0.8
    } else {
	set pane(D) X ;# Adjust boundary in X direction
	place $pane(1) -in $master -relx 0.0 -y 0 -anchor nw \
	    -relheight 1.0 -width -1
	place $pane(2) -in $master -relx 1.0 -y 0 -anchor ne \
	    -relheight 1.0 -width -1
	place $pane(grip) -in $master -anchor c -rely 0.8
    }
    $master configure -background black
    
    # Set up bindings for resize, <Configure>, and 
    # for dragging the grip.
    
    bind $master <Configure> [list PaneGeometry $master]
    bind $pane(grip) <ButtonPress-1> \
	[list PaneDrag $master %$pane(D)]
    bind $pane(grip) <B1-Motion> \
	[list PaneDrag $master %$pane(D)]
    bind $pane(grip) <ButtonRelease-1> \
	[list PaneStop $master]
    
    # Do the initial layout    
    PaneGeometry $master
}


proc Pane_Create {f1 f2 args} {            
    # Map optional arguments into array values   
    set t(-orient) vertical    
    set t(-percent) 0.5   
    set t(-in) [winfo parent $f1]
    array set t $args

    # Keep state in an array associated with the master frame
    set master $t(-in)
    upvar #0 Pane$master pane
    array set pane [array get t]

    # Create the grip and set placement attributes that
    # will not change. A thin divider line is achieved by
    # making the two frames one pixel smaller in the
    # adjustable dimension and making the main frame black.
    
    set pane(1) $f1
    set pane(2) $f2
    set pane(grip) [frame $master.grip -background gray50 \
			-width 10 -height 10 -bd 1 -relief raised \
			-cursor crosshair]
    if {[string match vert* $pane(-orient)]} {
	set pane(D) Y;# Adjust boundary in Y direction
	place $pane(1) -in $master -x 0 -rely 0.0 -anchor nw \
	    -relwidth 1.0 -height -1
	place $pane(2) -in $master -x 0 -rely 1.0 -anchor sw \
	    -relwidth 1.0 -height -1
	place $pane(grip) -in $master -anchor c -relx 0.8
    } else {
	set pane(D) X ;# Adjust boundary in X direction
	place $pane(1) -in $master -relx 0.0 -y 0 -anchor nw \
	    -relheight 1.0 -width -1
	place $pane(2) -in $master -relx 1.0 -y 0 -anchor ne \
	    -relheight 1.0 -width -1
	place $pane(grip) -in $master -anchor c -rely 0.8
    }
    $master configure -background black
    
    # Set up bindings for resize, <Configure>, and 
    # for dragging the grip.
    
    bind $master <Configure> [list PaneGeometry $master]
    bind $pane(grip) <ButtonPress-1> \
	[list PaneDrag $master %$pane(D)]
    bind $pane(grip) <B1-Motion> \
	[list PaneDrag $master %$pane(D)]
    bind $pane(grip) <ButtonRelease-1> \
	[list PaneStop $master]
    
    # Do the initial layout    
    PaneGeometry $master
}

proc PaneGeometry {master} {
    upvar #0 Pane$master pane
    if {$pane(D) == "X"} {
	place $pane(1) -relwidth $pane(-percent)
	place $pane(2) -relwidth [expr 1.0 - $pane(-percent)]
	place $pane(grip) -relx $pane(-percent)
	set pane(size) [winfo width $master]
    } else {
	place $pane(1) -relheight $pane(-percent)
	place $pane(2) -relheight [expr 1.0 - $pane(-percent)]
	place $pane(grip) -rely $pane(-percent)
	set pane(size) [winfo height $master]
    }
}
proc PaneTest {{p .p} {orient vert}} {
    catch {destroy $p}
    frame $p -width 200 -height 200
    label $p.1 -bg blue -text foo
    label $p.2 -bg green -text bar
    pack $p -expand true -fill both
    pack propagate $p off
    Pane_Create $p.1 $p.2 -in $p -orient $orient -percent 0.3
}

proc PaneDrag {master D} {
    upvar #0 Pane$master pane
    if [info exists pane(lastD)] {
	set delta [expr double($pane(lastD) - $D) \
		       / $pane(size)]
	set pane(-percent) [expr $pane(-percent) - $delta]
	if {$pane(-percent) < 0.0} {
	    set pane(-percent) 0.0
	} elseif {$pane(-percent) > 1.0} {
	    set pane(-percent) 1.0
	}
	PaneGeometry $master
	}
    set pane(lastD) $D
}
proc PaneStop {master} {
    upvar #0 Pane$master pane
    catch {unset pane(lastD)}
}


# ---------------------------------------------------------------
# Show List
# ---------------------------------------------------------------

proc procShowList {Result Msg1 Msg2} {
    global ok
	destroy .invariant 
	# destroy added for Mac TclTk
    set f .invariant
	if [Dialog_Create $f $Msg1 -borderwidth 10] {
		message $f.msg -text $Msg2 -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        scrollbar $f.frmSource.scrollx -command "$f.frmSource.text xview" -orient h
        text $f.frmSource.text -yscroll "$f.frmSource.scrolly set" -xscroll "$f.frmSource.scrollx set" \
         -setgrid 1 -height 18 -state disabled
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.scrollx -side bottom -fill x
        pack $f.frmSource.text -expand 1 -fill both
    
		set b [frame $f.buttons]
		pack $f.msg $f.frmSource $f.buttons -side top -fill x
		pack $f.frmSource -pady 5
		button $b.ok -text Done -command {set ok 1}
		pack $b.ok -side right
		
		bind $f.frmSource <Return> {set ok 1 ; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}
    
    
    $f.frmSource.text configure -state normal
    $f.frmSource.text delete 0.0 end
    
    foreach i $Result {
       $f.frmSource.text insert end $i
       $f.frmSource.text insert end "\n"
    }    
    
    $f.frmSource.text configure -state disabled
    
    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
}

proc procShowErrors {} {
   if [prolog get_all_errors(Res)] {
	set Result [split [string trimright $prolog_variables(Res) {;}] \;]
    
    procShowList $Result "Error Messages" "The following errors occured:"
   }
}


# ---------------------------------------------------------------
# PREFERENCES
# ---------------------------------------------------------------

proc procLoadPreferences {} {
  prolog logen_preferences:load_preferences('logen_preferences.pl')
  procShowErrors
}

proc procSetPreferences {prefcategory} {
    global curprefcategory
    set curprefcategory $prefcategory
	prolog logen_preferences:backup_preferences
    global ok
    destroy .trace
    set f .trace
	if [Dialog_Create $f "View/Edit Preferences" -borderwidth 10] {
		message $f.msg -text "List of Preferences" -aspect 1000
        # ------- code source frames
        frame $f.frmSource -borderwidth .1c -relief groove
        scrollbar $f.frmSource.scrolly -command "$f.frmSource.text yview"
        listbox $f.frmSource.text -yscroll "$f.frmSource.scrolly set" \
         -setgrid 1 -height 10 -width 60 -bg white
        bind $f.frmSource.text <ButtonRelease-1> {procPerformPrefOption}
        pack $f.frmSource.scrolly -side right -fill y
        pack $f.frmSource.text -expand 1 -fill both
    
		set b [frame $f.buttons]
	    set c [frame $f.checks]
		pack $f.msg $f.frmSource $f.checks $f.buttons -side top -fill x
		pack $f.frmSource -pady 5
		global lastiOption boolPrefVal
		set lastiOption 0
		set boolPrefVal 0
		radiobutton $c.bool -text True -variable boolPrefVal  -value 1 -state normal
		radiobutton $c.bool2 -text False -variable boolPrefVal  -value 0 -state normal
		message $c.valmsg -text "Value: " -aspect 1000
		global textPrefVal
		set textPrefVal 0
		entry $c.val -textvariable textPrefVal -width 40 -state normal
		button $c.pick -text Pick -command {procPickValue} -state normal

		pack $c.valmsg $c.val $c.bool $c.bool2 $c.pick -side left
		button $b.ok -text Finished -command {set ok 1; procPerformSetPrefOption}
		button $b.cancel -text Cancel -command {set ok 2}
		button $b.resetall -text "Reset all to defaults" -command {procResetPrefToDefaults}
		pack $b.ok $b.cancel $b.resetall -side right
		
		bind $f.frmSource <Return> {set ok 1 ; procPerformSetPrefOption; break}
		bind $f.frmSource <Control-c> {set ok 1 ; break}
	}
    
	global prolog_variables
	if { [prolog get_preferences_list($prefcategory,Res)] } {
	    set Result [split [string trimright $prolog_variables(Res) {;}] \;]
	} else {
	    puts "error loading preferences"
	    return
	}
	
    foreach i $Result {
       $f.frmSource.text insert end $i
    }    
    
    # $f.frmSource.text configure -state normal
    
    set ok 0
	Dialog_Wait $f ok $f.frmSource
	Dialog_Dismiss $f
	if {$ok==1} {
	  prolog logen_preferences:save_preferences('logen_preferences.pl')
	} else {
	  prolog logen_preferences:revert_preferences
	}
	# procInsertOptions
}
proc procResetPrefToDefaults {} {
    global curprefcategory
    procPerformSetPrefOption
    prolog reset_to_defaults($curprefcategory)
    procPerformGetPrefOption
}
proc procPerformPrefOption {} {
    global curprefcategory
    procPerformSetPrefOption
    procPerformGetPrefOption
}
proc procPickValue {} {
    global curprefcategory
    global lastiOption textPrefVal boolPrefVal 
    if {$lastiOption > 0} {
        prolog get_ith_preference_type($curprefcategory,$lastiOption,Type)
        prolog get_ith_preference_value($curprefcategory,$lastiOption,Val)
        set curtype $prolog_variables(Type)
        set curval $prolog_variables(Val)
        puts "curtype $curtype"
        if {$curtype == "rgb_color"} {
            set colres [tk_chooseColor -initialcolor $curval -parent .trace]
            if {$colres != ""} {
              puts "color: $colres"
              set textPrefVal $colres
            } else {
              puts "no color"
            }
        } else {
          puts "You cannot pick for this type"
        }
    }
}
proc procPerformGetPrefOption {} {
    global curprefcategory
    global lastiOption
    set iOption [.trace.frmSource.text curselection]
    if {$iOption != ""} {
        incr iOption
        prolog get_ith_preference_value($curprefcategory,$iOption,Val)
        set lastiOption $iOption
        global textPrefVal
        global boolPrefVal
        set textPrefVal $prolog_variables(Val)

	#reset entry
	pack forget .trace.checks.val .trace.checks.bool .trace.checks.bool2 .trace.checks.pick
        
	if [prolog get_ith_preference_type($curprefcategory,$iOption,bool)] {
           if {$textPrefVal=="true"} {set boolPrefVal 1} else {set boolPrefVal 0}
            # set boolPrefVal [expr $textPrefVal=="true"]
           #.trace.checks.val configure -state disabled
           #.trace.checks.bool configure -state normal
           #.trace.checks.bool2 configure -state normal
           #.trace.checks.pick configure -state disabled
	    
	   pack forget .trace.checks.val .trace.checks.bool .trace.checks.bool2 .trace.checks.pick
           pack .trace.checks.bool .trace.checks.bool2 -side left
	   

	    
           #.trace.checks.bool configure -state normal
           #.trace.checks.bool2 configure -state normal

	    
           
        } else {
            set boolPrefVal 2
	    pack .trace.checks.val
 #          .trace.checks.val configure -state normal
 #          .trace.checks.bool configure -state disabled
 #          .trace.checks.bool2 configure -state disabled
            if [prolog get_ith_preference_type($curprefcategory,$iOption,rgb_color)] {
		pack .trace.checks.val
               
            } else {
               #
           }
        }
    }
}
proc procPerformSetPrefOption {} {
    global curprefcategory
    global lastiOption textPrefVal boolPrefVal 
    if {$lastiOption>0} {
        prolog get_ith_preference_type($curprefcategory,$lastiOption,PType)
        set preftype $prolog_variables(PType)
        if {$preftype=="bool"} {
           if {$boolPrefVal == 1} {
              prolog set_ith_preference_value($curprefcategory,$lastiOption,true)
            } else {
              prolog set_ith_preference_value($curprefcategory,$lastiOption,false)
            }
        } elseif {$preftype=="string" || $preftype=="rgb_color"} {
           prolog set_ith_preference_value($curprefcategory,$lastiOption,'$textPrefVal')
        } else {
           prolog set_ith_preference_value($curprefcategory,$lastiOption,$textPrefVal)
        }
   }
}

# ---------------------------------------------------------------




#prolog print(a).

#prolog "logen_preferences:init_and_load_preferences('logen_preferences.pl')."
#prolog init_and_load_preferences('logen_preferences.pl')


prolog "ensure_loaded('sicstus.pl'),ensure_loaded('logen_main.pl')."

proc test {} {
#    global prolog_variables
    prolog "X=2"
    puts $prolog_variables(X)
}

#test


procMainInit

