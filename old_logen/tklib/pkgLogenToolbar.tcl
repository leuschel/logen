package provide pkgLogenToolbar 1.0

namespace eval LogenToolbar {
 #   variable dialog
}

proc LogenToolbar::createButton { name text image command state} {

    if { [file exists "$image"] } {
	image create photo $name.img -format gif -fil "$image"
	button "$name" -text $text -image "$name.img" -command "$command" -state "$state"
    } else {
	button "$name" -text $text -command "$command" -state "$state"
	tk_messageBox -parent . -icon error \
	    -message "Icon $image does not exist."	
    }
    bind $name <Enter> "procSetStatusText {$text}"
    bind $name <Leave> {procSetStatusText ""}
}


proc LogenToolbar::procToolbar {} {
    frame .frmBar
    
    frame .frmBar.filebar
    frame .frmBar.aboutbar
    frame .frmBar.fontbar
    frame .frmBar.editbar
    frame .frmBar.options





    #filebar
    createButton .frmBar.filebar.bNew "New"  "./icons/New24.gif" {puts [LogenDialog::Dialog_Error "Save?" "Changes are not saved.\nSave now?" {"Save" save} {"Cancel" cancel}]} normal
    createButton .frmBar.filebar.bOpen "Open File"  "./icons/Open24.gif" {procOpenFile} normal
    createButton .frmBar.filebar.bReopen "ReOpen Current  File"  "./icons/Redo24.gif" {procReOpenFile} disable
    createButton .frmBar.filebar.bSaveAnn "Save the Annotations"  "./icons/SaveAnn24.gif" {procSaveAnn} disable
    createButton .frmBar.filebar.bSavePl "Save the Source Code"  "./icons/SavePL24.gif" {procSaveCode} disable

    #help bar
    createButton .frmBar.aboutbar.bAbout "About Window"  "./icons/Information24.gif" {procAboutLogen} normal

    #zoom bar
    createButton .frmBar.fontbar.bZoomIn "Increase Font"  "./icons/ZoomIn24.gif" {procZoom 1} normal
    createButton .frmBar.fontbar.bZoomOut "Decrease Font"  "./icons/ZoomOut24.gif" {procZoom 0} normal
    createButton .frmBar.fontbar.bError "View Error Log"  "./icons/Log24.gif" {procShowErrors} normal

    createButton .frmBar.editbar.bEdit "Edit Code"  "./icons/Edit24.gif" {procEdit} normal
    createButton .frmBar.options.bPref "Preferences..."  "./icons/Preferences24.gif" {procSetPreferences "general"} normal




  




    grid rowconfigure .frmBar 0 -minsize 1m 
    grid rowconfigure .frmBar 1 -weight 1
#    pack .frmBar.open .frmBar.save -side left -fill y
    
    grid columnconfigure .frmBar 0 -minsize 5m
    grid columnconfigure .frmBar 1 -minsize 3m
    grid columnconfigure .frmBar 2 -minsize 3m
    grid columnconfigure .frmBar 3 -minsize 3m
    grid columnconfigure .frmBar 4 -minsize 3m
    grid columnconfigure .frmBar 5 -minsize 3m
    grid columnconfigure .frmBar 6 -minsize 3m
    grid columnconfigure .frmBar 7 -weight 1
    grid columnconfigure .frmBar 8 -minsize 5m


   # pack frmBar.filebar.open 
    pack .frmBar.filebar.bNew .frmBar.filebar.bOpen .frmBar.filebar.bReopen .frmBar.filebar.bSavePl .frmBar.filebar.bSaveAnn -side left -padx 0 -pady 0 -fill x
    pack .frmBar.aboutbar.bAbout -side left
    pack .frmBar.fontbar.bZoomOut .frmBar.fontbar.bZoomIn .frmBar.fontbar.bError -side left
    pack .frmBar.editbar.bEdit -side left
    pack .frmBar.options.bPref -side left

    grid .frmBar.filebar -row 1 -column 0
    grid .frmBar.fontbar -row 1 -column 2
    grid .frmBar.editbar -row 1 -column 4
    grid .frmBar.options -row 1 -column 6
    grid .frmBar.aboutbar -row 1 -column 8


   bind .frmBar <Destroy> {procQuit}

}