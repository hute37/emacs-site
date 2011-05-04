!****************************************************************************
!**   bbsload.bb -- Style and configuration file for bbsload               **
!****************************************************************************

!** Auto reconfigure when .blackboxrc changes
bbsload.autoConfig:  False

!****************** How and where to display ********************

!** Screen position to start at
bbsload.position:			+0-0

!** place bbdate in the slit **
!bbsload.withdrawn:			True

!** don't show frame **
!bbsload.shape:				True

!** delay between updates
!bbsload.checkDelay:			5

!bbsload.bevelWidth:			4

!** How many bars to draw
bbsload.gauge.numberOf.bars:		6

!** show seperate lines in gauge
!bbsload.gauge.tickmarks:               True

! Name of your network device
bbsload.netcard.name:			eth0
! Speed of your network device (KBits/sec)
! NOTE: you may wish to enter an estimated
!       maximum bandwidth as no-one ever
!       gets the full speed of their card
!       unless they are on a LAN
bbsload.netcard.speed:			10000

!****************** What and how to display *****************************
!** gauges and labels are displayed in this sequence **

!** if true the labels are shown below the gauges 
!bbsload.show.vertical:                 False
bbsload.show.1m.gauge:       		False
bbsload.show.5m.gauge:      		False
bbsload.show.15m.gauge:      		False
bbsload.show.memUsed.gauge:		True
bbsload.show.swapUsed.gauge:		False
bbsload.show.totalUsed.gauge:		False
bbsload.show.cpuUser.gauge:		True
bbsload.show.cpuNice.gauge:		False
bbsload.show.cpuSystem.gauge:		False
bbsload.show.cpuIdle.gauge:		False
bbsload.show.networkin.gauge:           True
bbsload.show.networkout.gauge:          False

! WARNING: Do not use more than one label
!          as they are currently broken
bbsload.show.runningCounter.label:	False
bbsload.show.taskCounter.label: 	False
bbsload.show.1m.label:       		False
bbsload.show.5m.label:      		False
bbsload.show.15m.label:      		False
bbsload.show.memUsed.label:		False
bbsload.show.swapUsed.label:		False
bbsload.show.totalUsed.label:		False
bbsload.show.cpuUser.label:		False
bbsload.show.cpuNice.label:		False
bbsload.show.cpuSystem.label:		False
bbsload.show.cpuIdle.label:		False
bbsload.show.networkin.label:		False
bbsload.show.networkout.label:		False

!** define active gauge style
bbsload.gauge.active:      		Vertical Gradient Flat Bevel2
bbsload.gauge.active.color:      	red1
bbsload.gauge.active.colorTo:    	green

!** define inactive gauge style
bbsload.gauge.inactive:      		Solid Flat Bevel2
bbsload.gauge.inactive.color:    	grey18
bbsload.gauge.inactive.colorTo:  	grey18

!** define background gauge style
!bbsload.gauge.background:      	Solid Flat Bevel2
!bbsload.gauge.background.color:    	grey18
!bbsload.gauge.background.colorTo:  	grey18

!** define the colors in stead of gradient
!bbsload.gauge.multiColor:		False

!** define the colors for the segment of the gauge
!bbsload.gauge.segment.1.color:		green
!bbsload.gauge.segment.2.color:		green
!bbsload.gauge.segment.3.color:		yellow
!bbsload.gauge.segment.4.color:		yellow
!bbsload.gauge.segment.5.color:		red1
!bbsload.gauge.segment.6.color:		red1

!** define maximum load
!bbsload.gauge.maximum.load:		1.0

!** define load-level of the different segments
!** max load is ignored
!bbsload.gauge.segment.1.scale:		0.1
!bbsload.gauge.segment.2.scale:		0.2
!bbsload.gauge.segment.3.scale:		0.4
!bbsload.gauge.segment.4.scale:		0.6
!bbsload.gauge.segment.5.scale:		0.8
!bbsload.gauge.segment.6.scale:		1.0


!** bbsload on top of other windows?
bbsload.raised:  True

!** pixel width of the graph area
!bbsload.gauge.width:		20

!** number of pixels the bars decrease in width each level
!** on left and right side
!bbsload.gauge.left.stepWidth:	2
!bbsload.gauge.right.stepWidth:	0

! *************************************************************
! ** By default the values below this point are taken from   **
! ** the Blackbox style.                                     **
! ** If you want to override something just uncomment it     **
! ** and change the value                                    **
! *************************************************************

!***************** Define display style *************************

!** define frame style
!bbsload.frame:          Raised Gradient Vertical   Bevel1
!bbsload.frame.color:    gray45
!bbsload.frame.colorTo:  gray21

!** define label style
!bbsload.label:          Sunken Gradient Vertical   Bevel1
!bbsload.label.color:    gray45
!bbsload.label.colorTo:  gray21

!************ fonts and text colors ******************************

!** label font
!mailtool.labelFont:     -*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*

!** font which determines the height (compatible with blackbox)
!mailtool.heightBy.font: -*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*

!** text colors
!bbsload.taskCounter.label.textColor:	lightgrey
!bbsload.runningCounter.label.textColor:	lightgrey
!bbsload.1m.label.textColor:		lightgrey
!bbsload.5m.label.textColor:		lightgrey
!bbsload.15m.label.textColor:		lightgrey
!bbsload.memUsed.label.textColor:	lightgrey
!bbsload.swapUsed.label.textColor:	lightgrey
!bbsload.totalUsed.label.textColor:	lightgrey
!bbsload.seperator.counter.textColor:	lightgrey
