!**********************************************************************
!** bbappconf.bb: Style file for bbappconf when using Blackbox           **
!**********************************************************************

!Uncomment what you need
!Number of windows to configure
!bbappconf.numberOf.configs:  3

! For now only classhints (name and/or class) can be used to select window
! You can use clashint name and/or classhint class,
! if both are specified both should match.
! To find classhint use xprop: WM_CLASS(string) = "name", "class"
! To set only the mainwindow of licq sticky and decorless :
!bbappconf.1.classHint.class:	licq
!bbappconf.1.classHint.name:	MainWindow
!bbappconf.1.Stick:	true
!bbappconf.1.decorless:	true
!bbappconf.1.maxVert:	false	
!bbappconf.1.maxHoriz:  false
!bbappconf.1.startOnWorkspace:	

! start xterm with "xterm -name workspace2"
!bbappconf.2.classHint.class:	XTerm
!bbappconf.2.classHint.name:	Workspace2
!bbappconf.2.Stick:		true
!bbappconf.2.decorless:	true
!bbappconf.2.startOnWorkspace:	2

!bbappconf.3.classHint.class:	XTerm
!bbappconf.3.classHint.name:	Workspace3
!bbappconf.3.startOnWorkspace:	3
!bbappconf.1.maxVert:	true
!bbappconf.1.maxHoriz:  true

! ** auto reconfigure when config-file has changed **
!bbappconf.autoConfig:                False
! ** number of seconds between checks **
!bbappconf.autoConfig.checkTimeout:   10

!** position of bbappconf window **
bbappconf.position:               +0-0

! *************************************************************
! ** By default the values below this point are taken from   **
! ** the Blackbox style.                                     **
! ** If you want to override something just uncomment it     **
! ** and change the value                                    **
! *************************************************************

!** set bevelwidth
!bbappconf.bevelWidth:             4

!** define frame style **
!bbappconf.frame:                 Raised Gradient Vertical Bevel1
!bbappconf.frame.color:           slategrey
!bbappconf.frame.colorTo:         darkslategrey

!** define pager-desktop window style **
!bbappconf.desktop:               Sunken Gradient Vertical Bevel1
!bbappconf.desktop.color:         slategrey
!bbappconf.desktop.colorTo:	darkslategrey

!** define pager-window style **
!bbappconf.window:                Raised Gradient Vertical Bevel1
!bbappconf.window.color:          slategrey
!bbappconf.window.colorTo:        darkslategrey

!** define pager-window style **
!bbappconf.window.focus:          Raised Gradient Vertical Bevel1
!bbappconf.window.focus.color:    rgb:c/9/6
!bbappconf.window.focus.colorTo:  rgb:8/6/4

!bbappconf.active.window.borderColor:	lightgrey
!bbappconf.inactive.window.borderColor:	black
!bbappconf.active.desktop.borderColor:	lightgrey
