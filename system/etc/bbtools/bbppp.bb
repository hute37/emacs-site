!**********************************************************************
!** bbppp.bb: Style file for bbppp when using Blackbox	     	     **
!**********************************************************************

bbtools.colorsPerChannel:	4
bbtools.imageDither:		False

! ** auto reconfigure when config-file has changed **
bbppp.autoConfig:		false

!** time (in seconds) between checks **
!bppp.checkDelay:		1

!** position of bbppp window **
bbppp.position: 		+0-0
!bbppp.bevelWidth:		4
!** size between button corner and light
!bbppp.button.bevelWidth:	4

!** place bbdate in the slit **
!bbppp.withdrawn:		True
!** bevelWidth when in the slit **
!bbppp.withdrawn.bevelWidth:	0

!** don't show frame **
!bbppp.shape:			True

!** bbppp on top of other windows **
!bbppp.raised:			True

! font which determines the height (compatible with blackbox)
!bbppp.heightBy.font: -*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*
!** label font **
bbppp.labelFont:     -*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*

!** show the time-label **
!bbppp.show.label:	True

!** show seconds in time-label
!bbppp.show.showsecs:   True

!** show the throughput label **
!bbppp.show.thruput:	False

!** button style
! other selection is "compact"
! compact button style is 1 button (the switch) containing all 3 lights
!bbppp.buttonstyle:	normal

!** orientation of widgets within window
! other selection is "vertical"
!bbppp.orientation:	horizontal

!** define frame style **
!bbppp.frame:			Raised Gradient Vertical Bevel1
!bbppp.frame.color: 		slategrey
!bbppp.frame.colorTo:		darkslategrey

!** define label style **
!bbppp.transparent:		True
!bbppp.label:			Sunken Gradient Vertical Bevel1
!bbppp.label.color:	 	slategrey
!bbppp.label.colorTo:		darkslategrey

!** text color label **
!bbppp.textColor:		lightgrey

!** define button style **
!bbppp.button:			Raised Gradient Diagonal Bevel1
!bbppp.button.color:	 	rgb:c/9/6
!bbppp.button.colorTo:		rgb:8/6/4

!** define button style **
!bbppp.button.pressed:		Sunken Gradient Diagonal Bevel1
!bbppp.button.pressed.color: 	rgb:c/9/6
!bbppp.button.pressed.colorTo:	rgb:8/6/4

!** first light indicates the status of the ppp-link **
bbppp.switchColor.on:		green2
bbppp.switchColor.off:		black
bbppp.switchColor.switching:	yellow

!** second light indicates tx **
bbppp.txColor.on:		green2
bbppp.txColor.off:		black

!** third light indicates rx **
bbppp.rxColor.on:		green2
bbppp.rxColor.off:		black

!** commands to set up ppp link **
bbppp.startCommand:		pon
bbppp.stopCommand:		poff
!bbppp.resumeCommand:		pon
