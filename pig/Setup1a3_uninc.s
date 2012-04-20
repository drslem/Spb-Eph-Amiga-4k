; Spaceballs vs Ephidrena
;
; Systemoppsett
; c2p
; texturegenerator
; kamera + rotasjon
; singenerator
; glow
;
;



_PREVIEW	=	0
_SOUND		=	01
_TIMING		=	01

	movem.l	d0-a6,-(sp)
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	lea	tsjippmem,a5
	jsr	_codeStart
	movem.l	(sp)+,d0-a6
	rts

;	incdir	dh3:Code/Include/
;	include	AllOfIt_lvo.i
;	include	hardware/hardware.i		; Some hardware registers
;	include	hardware/dmabits.i		; The bits for DMAcon
;	include	hardware/intbits.i		; The bits for Intena

INTF_SETCLR	=	32768
INTF_VERTB	=	32
INTF_INTEN	=	16384
INTF_EXTER	=	8192
INTF_AUD0	=	128

DMAF_SETCLR	=	32768
DMAF_COPPER	=	128
DMAF_RASTER	=	256
DMAF_BLITTER	=	64
DMAF_MASTER	=	512
DMAF_AUDIO	=	15

CUSTOM		=	$dff000
intenar		=	$1c
intena		=	$9a
dmaconr		=	$02
dmacon		=	$96

	gb_copinit=38
	ExecBase=4
dmaset	equ	DMAF_SETCLR!DMAF_COPPER!DMAF_RASTER!DMAF_BLITTER!DMAF_MASTER!DMAF_AUDIO
	IFNE	_SOUND
intset	equ	INTF_SETCLR!INTF_VERTB!INTF_INTEN!INTF_EXTER!INTF_AUD0
	ENDC
	IFEQ	_SOUND
intset	equ	INTF_SETCLR!INTF_VERTB!INTF_INTEN!INTF_EXTER
	ENDC
		section	kukostluktermerkelig,code
_codeStart
		lea	CopperPtr(pc),a4
		move.l	a5,(a4)

		add.w	#ScrnAd-CopperPtr,a4
		add.l	#(_Scrn1-tsjippmem),a5
		move.l	a5,(a4)+
		add.l	#(_Scrn2-_Scrn1),a5
		move.l	a5,(a4)+
;		lea	Chunk1(pc),a0
		move.l	a4,a0
		add.l	#(Chunk1-WChuAd),a0
		move.l	a0,(a4)+		WChuAd
		add.l	#(Chunk2-Chunk1),a0
		move.l	a0,(a4)+		ChuAd

		lea	CopperPtr(pc),a6
		bsr.w	prekalkulator	precalc i årevis...

		move.l	$4.w,a6
		lea	superv(pc),a5		;68010++ instruksjon kommer her
;		jsr	_LVOSupervisor(a6)	;go supervisor
		jsr	-30(a6)

		Lea	CUSTOM,a5		 Offset to hardware registers
		move.w	intenar(a5),-(sp)
		move.w	dmaconr(a5),-(sp)
		move.w	#$7fff,intena(a5)	Clear all interrupts
		move.w	#$7fff,dmacon(a5)	Clear all DMA channels

	IFNE	_SOUND
		move.l  $70(a0),-(sp) 		;old_lev4
		lea	audio_interrupt(pc),a4
		move.l  a4,$70(a0)
	ENDC
		Move.l	$6c(a0),-(sp)		Save old interrupt adress
		lea	Interrupt(pc),a4
		Move.l	a4,$6c(a0)		Store NEW interrupt adress
		move.l	a0,-(sp)		VBR


	IFNE	_SOUND
		include	_fist_music_code.i
	ENDC

;		lea	CUSTOM,a5
	IFEQ	_SOUND
		lea	$dff000+$a0+$40,a4
	ENDC
		move.w	#dmaset,dmacon-$a0-$40(a4)	Set the DMA channels we need
		move.w	#intset,intena-$a0-$40(a4)	Set the int. channels we need

	IFNE	_SOUND
.dd		cmp.b	#2,lyd_play(a6)
		blo.s	.dd
	ENDC

XSTRT	=	129
XSTOP	=	129+320-8
YSTRT	=	44+32
YSTOP	=	44+32+192-2
HSTRT	=	129
WIDTH	=	320


		lea	CopperPtr(pc),a6		konstant i a6
		bsr.w	ChgS	a0: neste copperlinje, d2 adresse til hamscr

		move.l	d2,a1
		lea	(640*96/8)(a1),a2
		move.w	#7680-1,d0
.Aa		move.b	#$77,(a1)+		HAMmask
		move.b	#$cc,(a2)+
		dbra	d0,.Aa
	
		lea	clist(pc),a1
		moveq	#((clistE-clist)/4)-1,d2
.Aa2		move.l	(a1)+,(a0)+
		dbra	d2,.Aa2

*		move.l	CopperPtr(pc),$080(a5)
		move.l	(a6),$080-$a0-$40(a4)
		move.w	d0,$088-$a0-$40(a4)
;		move.w	#0,$180-$a0-$40(a4)
		clr.w	$180-$a0-$40(a4)
;----------------------------------------------------------------------

LogObe	=	0
LogThe	=	5*6*1
LogFis	=	5*6*2
LogPig	=	5*6*3
LogSpb	=	5*6*4
LogEph	=	5*6*5
LogSlu	=	5*6*6
LogLoa	=	5*6*7

CenObe	=	(256*33*4)+(64*4)
CenThe1	=	(256*49*4)+(50*4)
CenThe2	=	(256*49*4)+(54*4)
CenFis1	=	(256*49*4)+(82*4)
CenFis2	=	(256*22*4)+(56*4)
CenFis3	=	(256*80*4)+(06*4)
CenPig1	=	(256*49*4)+(86*4)
CenPig2	=	(256*22*4)+(84*4)
CenPig3	=	(256*80*4)+(34*4)
CenSpb	=	(256*49*4)+(40*4)
CenEph	=	(256*49*4)+(46*4)
CenSlu	=	(256*49*4)+(56*4)
CenLoa	=	(256*49*4)+(44*4)



MainLoopStarterSanneligHer

;	clr.w	VB2

;	move.w	#0,-(sp)
	move.l	#0,-(sp)

.V1	lea	VB(pc),a0
	move.w	(a0),d1
	clr.w	(a0)
.V2	move.w	(a0),d0
	beq.s	.V2
	addq.l	#2,a0
	move.w	(a0)+,d7	vb2
	move.w	d1,(a0)		legger den i se

	IFNE	_SOUND
;	lea	lyd_fastdata+lyd_play-CopperPtr(a6),a4
	move.l	a6,a4
	add.l	#lyd_fastdata+lyd_play-CopperPtr,a4
	ENDC

;--------------------------------

*	bra.s	.ass

;	lea	Anm1,a0
;	move.w	(sp),d0
;	and.l	#$0f,d0
;	mulu.l	#160*96*4,d0
;	add.l	d0,a0
;	move.l	WChuAd(pc),a1
;	move.w	#(160*96)-1,d0
;.asa	move.l	(a0)+,(a1)+
;	dbra	d0,.asa
;	add.w	#1,(sp)

	IFNE	_TIMING

	cmp.b	#6,(a4)
	bge.s	.part2
	bsr.w	NormCl

;--- logoer ------------------------------

	cmp.b	#4,(a4)
	beq.w	.ja1a
	cmp.b	#3,(a4)
	bge.s	.ikke1a
.ja1a
	lea	Logo+LogObe(pc),a0
	move.l	#CenObe,d0
	bsr.w	SetL
	bra.s	.siste1

.ikke1a
	cmp.b	#4,(a4)
	bge.s	.ikke1c
	lea	Logo+LogThe(pc),a0
	move.l	#CenThe1,d0
	bsr.w	SetL

	lea	Logo+LogFis(pc),a0
	move.l	#CenFis1,d0
	bsr.w	SetL
	bra.s	.siste1
.ikke1c
;	cmp.b	#6,(a4)
;	bge.s	.part2

	lea	Logo+LogThe(pc),a0
	move.l	#CenThe2,d0
	bsr.w	SetL

	lea	Logo+LogPig(pc),a0
	move.l	#CenPig1,d0
	bsr.w	SetL
.siste1
	bsr.w	Glow
	bra.w	.nomorepartsblink

;---------------------------------------
.part2	cmp.b	#10,(a4)
	bge.w	.part3

	bsr.w	NormCl

	lea	Snurr3d2(pc),a1
	lea	KubCor(pc),a0
	move.w	#400*2,d1		amp
	move.w	(sp),d0			ang
.preA
	moveq	#22-1,d5
.A	move.l	a0,a2
	bsr.w	GetSC
	move.w	#(8*1)-1,d4
.Aa
	move.w	(a2)+,(a1)
	add.w	d3,(a1)+
	move.w	(a2)+,(a1)
	sub.w	d2,(a1)+
	move.w	(a2)+,(a1)
	add.w	d2,(a1)+

	dbra	d4,.Aa
	add.w	#29,d0
	dbra	d5,.A

	move.w	se(pc),d0
	add.w	d0,(sp)

	moveq	#0,d0
	move.b	(a4),d0
	lsl.w	#7,d0
	add.w	#157,d0

;	move.w	(sp),d0

	move.w	#1200*2,d1
	bsr.w	GetSC

	lea	Cam(pc),a4
	move.w	#600,(a4)+
	movem.w	d2/d3,(a4)

;	move.w	d2,Cam+2
;	move.w	#600,Cam
;	move.w	d3,Cam+4

	lea	Snurr3d2(pc),a4
	move.w	#(8*22*1)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex2-CopperPtr,a2
	move.w	#8-3,d6		punkter per obj
	moveq	#(22*1)-1,d7			antall obj
	bsr.w	Smuff

	bsr.w	Glow

	bra.w	.nomoreparts

;--------------------------------------

.part3	cmp.b	#18,(a4)
	bge.w	.part4

	moveq	#0,d7
	bsr.w	NormCl

	bsr.w	evilvector

	bsr.w	Glow

	bra.w	.nomorepartsblink

;---------------------------------------
.part4	cmp.b	#23,(a4)
	bge.s	.part5

	moveq	#0,d7
	bsr.w	NormCl

	move.l	a6,a0
	add.l	#Anm2-CopperPtr,a0
	moveq	#0,d2
	moveq	#0,d3

	move.w	timeflip-CopperPtr(a6),d1
	and.w	#$03,d1
	beq.s	.L4a

	move.w	#160*4*2,d2
	moveq	#1,d3		go left
	cmp.w	#$01,d1
	beq.s	.L4a

	moveq	#0,d2
	add.l	#(160*4*95),a0
	cmp.w	#$02,d1
	beq.s	.L4a

	moveq	#0,d3
	move.w	#-160*4*2,d2

.L4a
	move.w	(sp),d0
	bsr.w	SetAnm
	addq.w	#1,(sp)

	cmp.b	#22,(a4)
	bne.s	.L4b
	lea	Logo+LogObe(pc),a0
	move.l	#CenObe,d0
	bsr.w	SetL
	bsr.w	Glow
	bra.w	.nomorepartsblink
.L4b
	bsr.w	Glow
	bra.w	.nomoreparts

;---------------------------------------

.part5	cmp.b	#27,(a4)
	bge.w	.part6

;	moveq	#0,d7
	bsr.w	NormCl

	move.w	#233,d4		vinkeloffset
	move.w	#22,a0		radiusdelta
	move.w	#(200/1)-1,d7	cordn
	bsr.w	SnurrCor

	move.w	(sp),d0
	move.w	#2200*2,d1
	bsr.w	GetSC

	move.w	se(pc),d0
	add.w	d0,(sp)

;	move.w	(sp),CamTilt

	move.w	timeflip-CopperPtr(a6),d1
	btst	#0,d1
	beq.s	.ss
	exg	d2,d3
.ss
	lea	Cam(pc),a4
	move.w	d2,(a4)
	sub.w	d3,(a4)+
	move.w	d3,(a4)+
	move.w	d2,(a4)

;	move.w	d2,Cam+4
;	move.w	d2,Cam
;	sub.w	d3,Cam
;	move.w	d3,Cam+2

;	lea	Snurr3d(pc),a4
	move.l	a6,a4
	add.l	#Snurr3d-CopperPtr,a4
	move.w	#(200)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex2-CopperPtr,a2
	move.w	#200-3,d6		punkter per obj
	moveq	#1-1,d7			antall obj
	bsr.w	Smuff

	lea	Logo+LogFis(pc),a0
	move.l	#CenFis2,d0
	bsr.w	SetL
	lea	Logo+LogPig(pc),a0
	move.l	#CenPig2,d0
	bsr.w	SetL

	bsr.w	Glow

	bra.w	.nomorepartsblink


;---------------------------------------
.part6
	cmp.b	#29,(a4)
	bge.w	.part7

	moveq	#0,d7
	bsr.w	NormCl

	cmp.b	#28,(a4)
	bge.s	.part6b
	lea	Logo+LogEph(pc),a0
	move.l	#CenEph,d0
	bsr.w	SetL
	bra.s	.part6c
.part6b
	lea	Logo+LogSpb(pc),a0
	move.l	#CenSpb,d0
	bsr.w	SetL


.part6c	bsr.w	Glow
	bra.w	.nomorepartsblink

;---------------------------------------
.part7
	cmp.b	#39,(a4)
	bge.s	.part8

	moveq	#0,d7
	bsr.w	NormCl

	move.l	a6,a0
	add.l	#Anm1-CopperPtr,a0
	moveq	#0,d2
	moveq	#0,d3

	move.w	timeflip-CopperPtr(a6),d1
	and.w	#$03,d1
	beq.s	.L7a2

	move.w	#160*4*2,d2
	moveq	#1,d3		go left
	cmp.w	#$01,d1
	beq.s	.L7a2

	moveq	#0,d2
	add.l	#(160*4*95),a0
	cmp.w	#$02,d1
	beq.s	.L7a2

	moveq	#0,d3
	move.w	#-160*4*2,d2

.L7a2
	move.w	2(sp),d0
	bsr.w	SetAnm
	addq.w	#1,2(sp)

	cmp.b	#31,(a4)
	blo.s	.L7a3
	bsr.w	evilvector
.L7a3

	bsr.w	Glow
	bra.w	.nomoreparts
;---------------------------------------

.part8	cmp.b	#45,(a4)
	beq.w	ikkenoemere


	bsr.w	NormCl


	cmp.b	#42,(a4)
	bge.s	.L8a

	move.l	a6,a0
	add.l	#Anm2-CopperPtr,a0
	moveq	#0,d2
	moveq	#0,d3

	move.w	2(sp),d0
	bsr.w	SetAnm
	addq.w	#1,2(sp)


	cmp.b	#40,(a4)
	blo.s	.L8a2

	move.w	(sp),d4
	and.w	#511,d4
	move.w	#38,a0		radiusdelta
	move.w	#(300/1)-1,d7	cordn
	bsr.w	SnurrCor

	move.w	#192,d0
	move.w	#3200*2,d1
	bsr.w	GetSC

	addq.w	#1,(sp)

	lea	Cam(pc),a4
	move.w	d2,(a4)+
	move.w	d3,(a4)+
	move.w	d2,(a4)


;	lea	Snurr3d(pc),a4
	move.l	a6,a4
	add.l	#Snurr3d-CopperPtr,a4
	move.w	#(300)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex2-CopperPtr,a2
	move.w	#300-3,d6		punkter per obj
	moveq	#1-1,d7			antall obj
	bsr.w	Smuff
	bra.s	.L8a3
.L8a
	move.w	#$666,2(sp)
.L8a2
	move.w	#0,(sp)
.L8a3
	lea	Logo+LogFis(pc),a0
	move.l	#CenFis3,d0
	bsr.w	SetL
	lea	Logo+LogPig(pc),a0
	move.l	#CenPig3,d0
	bsr.w	SetL

;	cmp.b	#42,(a4)
;	blo.s	.L8b

	cmp.w	#$666,2(sp)
	bne.s	.L8b

	bsr.w	Glow
	bra.s	.nomorepartsblink
.L8b
	bsr.w	Glow
	bra.s	.nomoreparts
;---------------------------------------
	ENDC		timing

*.ass
*	moveq	#0,d0
*	tst.w	bla
*	beq.s	.as1
*	move.w	#0,bla
*	move.l	#$3f3f3f3f,d0
*.as1
*	move.l	WChuAd,a0
*	move.w	#(160*96)-1,d1
*.as	move.l	d0,(a0)+
*	dbra	d1,.as



.nomorepartsblink
	bsr.w	FlippIt
.nomoreparts
	move.l	WChuAd(pc),a0
	move.l	ScrnAd(pc),a1
	bsr.w	C2P0604K
	bsr.w	ChgS


	btst	#06,$bfe001
	bne.w	.V1
;----------------------------------------------------------------------
ikkenoemere

		addq.l	#4,sp

Backtosystem	Lea	CUSTOM,a5		 Offset to hardware registers

		Move.w	#$7fff,intena(a5)	 Clear all interrupts
		Move.w	#$7fff,dmacon(a5)	 Clear all DMA channels

		move.l	(sp)+,a0
		move.l	(sp)+,$6c(a0)
	IFNE	_SOUND
		move.l	(sp)+,$70(a0)
	ENDC
		move.w	(sp)+,d0
		move.w	(sp)+,d1
		or.w	#$8000,d0
		or.w	#$8000,d1
		move.w	d0,dmacon(a5)
		move.w	d1,intena(a5)

CloseDown	move.l	ExecBase.w,a6		ufin måte å finne gfxbase på
		move.l	156(a6),a6
		move.l	gb_copinit(a6),$080(a5)	 Kick it into life
		move.w	d0,$088(a5)
		rts
*bla	dc.w	0

ChgS	*move.l	CopperPtr(pc),a0
	move.l	(a6),a0
	lea	ScrnAd(pc),a1
	move.l	(a1),d0
	move.l	4(a1),d1
	move.l	d1,(a1)+
	move.l	d0,(a1)+
	move.l	(a1),d2
	move.l	4(a1),d5
	move.l	d2,(a1)+	wchuad
	move.l	d5,(a1)+	chuad
	move.l	a0,d2
	add.l	#_HamScr-tsjippmem,d2	hamscr
	move.l	d2,d5
	move.w	#$00e0,d3
	moveq	#2-1,d4
	bsr.s	.ChgSl
	move.l	d0,d5
	moveq	#6-1,d4

.ChgSl	swap	d5
	move.w	d3,(a0)+
	move.w	d5,(a0)+
	addq	#2,d3
	swap	d5
	move.w	d3,(a0)+
	move.w	d5,(a0)+
	addq	#2,d3
	add.l	#7680,d5
	dbra	d4,.ChgSl
	rts

FlippIt	move.l	WChuAd(pc),a0
	move.w	#(160*100*4)-1,d0
	not.w	Flipp-CopperPtr(a6)
	beq.s	.e
.Aa	move.b	(a0),d1
	lsr.b	d1
	move.b	d1,(a0)+
;	lsr.w	(a0)+
	dbra	d0,.Aa
.e	rts
;----------------------------------------------------
evilvector

;
;	sp er forandret pga bsr
;
	lea	Snurr3d2(pc),a1
	lea	KubCor(pc),a0
	move.w	#600*2,d1		amp
	move.w	4(sp),d0			ang
	moveq	#6-1,d7
.preA2
	moveq	#6-1,d5
.A2	move.l	a0,a2
	bsr.w	GetSC
	move.w	#(8*1)-1,d4
.Aa2
	move.w	(a2)+,(a1)
	add.w	d2,(a1)+
	move.w	(a2)+,(a1)
	add.w	d2,(a1)+
	move.w	(a2)+,(a1)
	sub.w	d3,(a1)+

	dbra	d4,.Aa2
	add.w	#170,d0
	dbra	d5,.A2

	add.w	#67,d0
	sub.w	#160,d1
	dbra	d7,.preA2

	move.w	se(pc),d0
	add.w	d0,4(sp)

	move.w	#910,d4
	move.w	#777,d0
	cmp.b	#17,(a4)
	beq.s	.L3a
	move.w	timeflip-CopperPtr(a6),d1
	btst	#0,d1
	beq.s	.L3b
	move.w	#666,d4
.L3b
	lsl.w	#7,d1
	add.w	d1,d0
.L3a

	move.w	#600*2,d1
	bsr.w	GetSC

	lea	Cam(pc),a4
	move.w	d4,(a4)+
	move.w	d3,(a4)+
	move.w	d2,(a4)


	lea	Snurr3d2(pc),a4
	move.w	#(8*6*6)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex1-CopperPtr,a2
	move.w	#8-3,d6		punkter per obj
	moveq	#(6*6)-1,d7			antall obj
	bsr.w	Smuff

	rts
;----------------------------------------------------
; Flytte inn fra wbuf
;----------------------------------------------------
NormCl	;lea	WBuf+(256*16*4)+(48*4),a0
	move.l	a6,a0
	add.l	#WBuf+(256*16*4)+(48*4)-CopperPtr,a0
;	move.l	WChuAd(pc),a1

	move.l	a6,a1
	add.l	#WBuf2-CopperPtr,a1

	moveq	#97-1,d0
	moveq	#0,d2
.ClrLa	move.w	#160-1,d1
.ClrLb

;	move.l	(a0),(a1)+
;	move.l	d2,(a0)+

	move.l	(a0),(a1)+
	tst.w	d7
	beq.s	.sss
	move.l	(a0),d2
	lsr.l	d2
	and.l	#$3f3f3f3f,d2
.sss	move.l	d2,(a0)+
	dbra	d1,.ClrLb
	lea	((256-160)*4)(a0),a0
	dbra	d0,.ClrLa
	rts

;----------------------------------------------------
SetAnm	move.l	a6,a1
	add.l	#WBuf+(256*16*4)+(48*4)-CopperPtr,a1
	and.l	#$0f,d0
	mulu.l	#160*96*4,d0
	add.l	d0,a0

	moveq	#96-1,d0
	tst.w	d3
	bne.s	.B
.Aa	move.w	#160-1,d1
.Ab	move.l	(a0)+,(a1)+
	dbra	d1,.Ab
	lea	((256-160)*4)(a1),a1
	lea	(a0,d2.w),a0
	dbra	d0,.Aa
	rts

.B	add.l	#160*4,a0
.Ba	move.w	#160-1,d1
.Bb	move.l	-(a0),(a1)+
	dbra	d1,.Bb
	lea	((256-160)*4)(a1),a1
	lea	(a0,d2.w),a0
	dbra	d0,.Ba
	rts

;----------------------------------------------------
SetL
;	move.l	WChuAd(pc),a1

	move.l	a6,a1
;	add.l	#WBuf2-CopperPtr,a1
	add.l	#WBuf+(256*16*4)+(48*4)-CopperPtr,a1


;	add.l	#(160*42*4)+(40*4),a1
	add.l	d0,a1
	move.l	#$363c3f3f,d7
	moveq	#6-1,d0
.Aa	moveq	#5-1,d1
.Ab	move.b	(a0)+,d2
	moveq	#8-1,d3
.Ac	btst	d3,d2
	beq.s	.Ad
	move.l	d7,(a1)
	move.l	d7,4(a1)
	move.l	d7,(256*4)(a1)
	move.l	d7,(257*4)(a1)
.Ad	addq.l	#4*2,a1
	dbra	d3,.Ac
	dbra	d1,.Ab
	lea	(512-80)*4(a1),a1
	dbra	d0,.Aa
	rts

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

SnurrCorN	=	600

; a0 : radius-delta
; d4 : vinkeloffset
; d7 : (cordn/2)-1

SnurrCor
;	lea	Snurr3d(pc),a1
	move.l	a6,a1
	add.l	#Snurr3d-CopperPtr,a1

;	move.w	#(SnurrCorN/2)-1,d7
	move.w	#-400,d6		startZ
	moveq	#0,d0
	move.w	#500,d5			startRad
;	move.w	#90,d4			vinkeloffset
.Aa	move.w	d5,d1
	bsr.w	GetSC
	move.w	d2,(a1)+
	move.w	d3,(a1)+
	move.w	d6,(a1)+
	sub.w	#90,d1
	bsr.w	GetSC
	move.w	d2,(a1)+
	move.w	d3,(a1)+
	move.w	d6,(a1)+
	addq	#4,d6			Zdelta
	sub.w	a0,d5
;	subq	#4,d5			radDelta
	add.w	d4,d0			vinkelDelta
	dbra	d7,.Aa
	rts
;----------------------------------------------------
Glow
;	lea	WBuf+(256*16*4)+(48*4),a0
	move.l	a6,a0
	add.l	#WBuf+(256*16*4)+(48*4)-CopperPtr,a0
;	lea	Chunk,a3
	move.l	WChuAd(pc),a3
	move.l	#$3f3f3f3f,d2
	moveq	#(160/4)-1,d0
.Aa	moveq	#(96/4)-1,d1
	move.l	a3,a1
	lea	(256*4*4)(a0),a2
	move.l	(a0),d3		0
.Ab	move.l	(a2),d4		4
	move.l	d3,(a1)
	move.l	d4,d5
	lea	(160*4)(a1),a1		
	add.l	d3,d5
	lsr.l	d5		2
	add.l	d5,d3
	lsr.l	d3		1	
	lea	(256*4*4)(a2),a2
	and.l	d2,d3
	move.l	d3,(a1)			1
	and.l	d2,d5
	lea	(160*4)(a1),a1	
	move.l	d5,(a1)			2
	add.l	d4,d5
	lsr.l	d5
	lea	(160*4)(a1),a1
	and.l	d2,d5
	move.l	d5,(a1)			3
	lea	(160*4)(a1),a1
	move.l	d4,d3
	dbra	d1,.Ab
	lea	(4*4)(a3),a3
	lea	(4*4)(a0),a0
	dbra	d0,.Aa

	lea	-(160*4)(a3),a3
	lea	(4*4)(a3),a0

;	lea	WBuf2,a4
	move.l	a6,a4
	add.l	#WBuf2-CopperPtr,a4
	move.l	a3,a5
	move.w	#((160/4)*95)-1,d0
.Ba	move.l	(a3),d1	0

	move.l	(a4)+,(a3)
	add.l	d1,(a3)+
	move.l	(a0),d3		4
	lea	(4*4)(a0),a0
	move.l	d1,d4
	add.l	d3,d4
	lsr.l	d4		2
	add.l	d4,d1
	lsr.l	d1		1
	and.l	d2,d1

	move.l	(a4)+,(a3)
	add.l	d1,(a3)+		1
	and.l	d2,d4

	move.l	(a4)+,(a3)
	add.l	d4,(a3)+		2
	add.l	d3,d4
	lsr.l	d4
	and.l	d2,d4

	move.l	(a4)+,(a3)
	add.l	d4,(a3)+		3
	dbra	d0,.Ba

	move.w	#(160*96)-1,d0
.Ca	move.l	(a5),d1
	lsr.l	d1
	and.l	d2,d1
	move.l	d1,(a5)+
	dbra	d0,.Ca

;	lea	WBuf2,a0
;	move.l	WChuAd(pc),a1
;	move.w	#(160*96)-1,d0
;.ad	move.l	(a0)+,(a1)+
;	dbra	d0,.ad



	rts
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Sjølvæste vektortegnelurifaksproduktet!
;
; a2 : texture
; d6 : vertices per object-3
; d7 : number of objects-1

Smuff	lea	rotatedcords(pc),a0
	lea	TStruct(pc),a5
.A	move.w	d6,-(sp)
.Aa
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	moveq	#0,d5

	movem.w	(a0)+,d0-d5
	subq.l	#8,a0

	move.l	a5,a3
	addq.l	#4,a5
	movem.l	d6-d7/a2/a5-a6,-(sp)
	bsr.w	Tri
	movem.l	(sp)+,d6-d7/a2/a5-a6

	dbra	d6,.Aa
	move.w	(sp)+,d6
	addq.l	#8,a0
	dbra	d7,.A
	rts

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Tri
*	lea	TStruct(pc),a3
	lea	4(a3),a4
	lea	4(a4),a5

;	lea	WBuf,a1
	move.l	a6,a1
	add.l	#WBuf-CopperPtr,a1
	cmp.w	d1,d3
	bge.s	.YselA
	exg	d1,d3
	exg	d0,d2
	exg	a3,a4
.YselA	cmp.w	d3,d5
	bge.s	.YselB
	exg	d3,d5
	exg	d2,d4
	exg	a4,a5
.YselB	cmp.w	d1,d3
	bge.s	.YselC
	exg	d1,d3
	exg	d0,d2
	exg	a3,a4
.YselC

	move.l	(a3),d7
	add.l	d2,d7
	sub.l	d0,d7
	move.l	d3,d6
	sub.l	d1,d6
	lsl.l	#8,d6
	add.l	d6,d7
	and.l	#(256*256)-1,d7
	move.l	d7,(a4)

	move.l	(a3),d7
	add.l	d4,d7
	sub.l	d0,d7
	move.l	d5,d6
	sub.l	d1,d6
	lsl.l	#8,d6
	add.l	d6,d7
	and.l	#(256*256)-1,d7
	move.l	d7,(a5)

	move.l	(a3),d7
;	move.l	TexAdr(pc),a3
;	lea	(a3,d7.l*4),a3
	lea	(a2,d7.l*4),a3

	neg	d0
	lea	(a3,d0.w*4),a3
	neg	d0

	move.l	d2,a5
	sub.w	d0,d2	x1-x0
	sub.w	d0,d4	x2-x0
	move.w	d4,d6
	sub.w	d2,d6	x2-x1
	sub.w	d1,d3	y1-y0
	beq.s	.Aa
	swap	d2
	divsl.l	d3,d2	delta x0.x1
.Aa	sub.w	d1,d5	y2-y0
	beq.w	TriE	alt er flatt
	swap	d4
	divsl.l	d5,d4	delta x0.x2
	sub.w	d3,d5	y2-y1
	beq.s	.Ab
	swap	d6
	divsl.l	d5,d6	delta x1.x2
.Ab	lsl.l	#8,d1
	lsl.l	#2,d1
	add.l	d1,a1
	move.l	d0,d1	x0
	move.l	d6,a6	delta x1.x2

	subq.w	#1,d3
	bmi.s	.FlatTop
	bsr.s	.Ba

.FlatTop
	move.l	a5,d1
	move.l	a6,d2	delta x1.x2
	subq.w	#1,d5
	bmi.s	TriE	flat bunn
	move.w	d5,d3

.Ba	move.w	d0,d6
	move.w	d1,d7
	cmp.w	d6,d7
	bge.s	.Bb
	exg	d6,d7
.Bb	lea	(a1,d6.w*4),a2
	lea	(a3,d6.w*4),a4
	sub.w	d6,d7
	beq.s	.Bd
.Bc

	move.b	#4,-(sp)
.Bc0
	move.b	(a4)+,d6
	add.b	d6,(a2)
	cmp.b	#$3f,(a2)+
	ble.s	.Bc1
	move.b	#$3f,-1(a2)
.Bc1	sub.b	#1,(sp)
	bne.s	.Bc0
	move.b	(sp)+,d6

	subq.w	#1,d7
	bne.s	.Bc
.Bd	lea	(256*4)(a1),a1
	lea	(256*4)(a3),a3
	swap	d0
	swap	d1
	add.l	d4,d0
	add.l	d2,d1
	swap	d0
	swap	d1
	dbra	d3,.Ba


TriE	rts


*--------------------------------------------------------------------
; d0 : angle
; d1 : amplitude/2
;
; d2 : y
; d3 : x
GetSC	and.w	#1023,d0
	move.w	d1,d2
	move.w	d1,d3
	lea	SinTab(pc),a5
	muls.w	(a5,d0.w*4),d2
	lea	CosTab(pc),a5
	muls.w	(a5,d0.w*4),d3
	swap	d2
	swap	d3
	ext.l	d2
	ext.l	d3
	rts
*--------------------------------------------------------------------

yx	=	-18
yy	=	-16
yz	=	-14
zx	=	-12
zy	=	-10
zz	=	-8
xx	=	-6
xy	=	-4
xz	=	-2

;------------------------------------------------
; Kamera
;---------------------------------------------
; a4 : coords
; a5 : cordn-1

FixCam

	lea	Cam(pc),a0

	lea	Matrx(pc),a3

	movem.w	(a0)+,d0-d2

	ext.l	d0
	ext.l	d1
	ext.l	d2

	fmove.l	d0,fp0
	fmove.l	d1,fp1
	fmove.l	d2,fp2
	muls.l	d0,d0
	muls.l	d1,d1
	muls.l	d2,d2
	add.l	d2,d0
	add.l	d1,d0
	fmove.l	d0,fp3
	fsqrt	fp3
	fmove.w	fp3,(Depth-Matrx)(a3)
	fdiv	fp3,fp0		Nx
	fdiv	fp3,fp1		Ny
	fdiv	fp3,fp2		Nz
	fmove.w	CamTilt(pc),fp3
	fmovecr	#0,fp4		pi
	fmul	fp4,fp3
	fmove.l	#512,fp4
	fdiv	fp4,fp3
	fsincos	fp3,fp4:fp5

	fmove	fp4,fp3
	fmul	fp0,fp3	NxDx
	fmove	fp5,fp6
	fmul	fp1,fp6	NyDy
	fadd	fp6,fp3	NxDx+NyDy(+NzDz=+0) = N.D
	fmove	fp0,fp6	Nx
	fmul	fp3,fp6
	fsub	fp6,fp4	Vx
	fmove	fp1,fp6
	fmul	fp3,fp6
	fsub	fp6,fp5	Vy
	fmove	fp2,fp6
	fmul	fp3,fp6
	fneg	fp6	Vz

	fmove.l	#128*256,fp7
	fmul	fp7,fp0
	fmul	fp7,fp1
	fmul	fp7,fp2
	fmul	fp7,fp4
	fmul	fp7,fp5
	fmul	fp7,fp6
	
	fmove.w	fp4,d4
	fmove.w	fp5,d5
	fmove.w	fp6,d6
	fmove.w	fp0,d0
	fmove.w	fp1,d1
	fmove.w	fp2,d2

	move.w	d4,(a3)+	ax
	move.w	d5,(a3)+	ay
	move.w	d6,(a3)+	az
	move.w	d0,(a3)+	bx
	move.w	d1,(a3)+	by
	move.w	d2,(a3)+	bz

	move.w	d5,d3
	muls.w	d2,d3
	add.l	d3,d3
	swap	d3
	move.w	d6,d7
	muls.w	d1,d7
	add.l	d7,d7
	swap	d7
	sub.w	d7,d3
	move.w	d3,(a3)+		Ux
	move.w	d6,d3
	muls.w	d0,d3
	add.l	d3,d3
	swap	d3
	move.w	d4,d7
	muls.w	d2,d7
	add.l	d7,d7
	swap	d7
	sub.w	d7,d3
	move.w	d3,(a3)+
	muls.w	d1,d4
	add.l	d4,d4
	swap	d4
	muls.w	d0,d5
	add.l	d5,d5
	swap	d5
	sub.w	d5,d4
	move.w	d4,(a3)+

WorldRot
.DoRot

;	move.l	cord3dAd(pc),a0		allerede i a4
	move.l	a4,a0
	lea	rotatedcords(pc),a1
	lea	TStruct(pc),a2
	move.w	a5,d3		antall cords -1
.Aa
;	move.w	(a0)+,d0	x
;	move.w	(a0)+,d1	y
;	move.w	(a0)+,d2	z
	movem.w	(a0)+,d0-d2
	move.w	xx(a3),d4
	move.w	xy(a3),d5
	move.w	xz(a3),d6
	muls.w	d0,d4
	muls.w	d1,d5
	muls.w	d2,d6
	add.l	d5,d4
	add.l	d6,d4		nyX
	add.l	d4,d4
	swap	d4

	move.w	yx(a3),d5
	move.w	yy(a3),d6
	move.w	yz(a3),d7
	muls.w	d0,d5
	muls.w	d1,d6
	muls.w	d2,d7
	add.l	d6,d5
	add.l	d7,d5		nyY
	add.l	d5,d5
	swap	d5

	muls.w	zx(a3),d0
	muls.w	zy(a3),d1
	muls.w	zz(a3),d2
	add.l	d0,d2
	add.l	d1,d2		nyZ
	add.l	d2,d2
	swap	d2

	muls	#300,d4
	muls	#300,d5
	add.w	Depth(pc),d2
	beq.s	.duh
	divs	d2,d4
	divs	d2,d5
.duh	add.w	#128,d4
	bpl.s	.clipXa		noget tvilsom xclip
	moveq	#0,d4
.clipXa	cmp.w	#255,d4
	blo.s	.clipXb
	move.w	#255,d4
.clipXb	add.w	#64,d5
	bpl.s	.clipYa
	move	#0,d5
.clipYa	cmp.w	#127,d5
	ble.s	.clipYb
	move.w	#127,d5
.clipYb
	move.w	d4,(a1)+	x
	move.w	d5,(a1)+	y
;	tst.w	NewStencils-CopperPtr(a6)
;	bne.s	.noStenc
	ext.l	d5
	lsl.l	#8,d5
	move.b	d4,d5
	move.l	d5,(a2)+
.noStenc
	dbra	d3,.Aa
;	st	NewStencils-CopperPtr(a6)
	rts
;----------------------------------------------
prekalkulator

        lea     SinTab(pc),a0
        
        moveq   #0,d0          ;a=0
        move.l  #12500000,d1   ;b=d
       
        move    #2047,d7
.slop1
        move.l  d0,(a0)+       ;store a 
        move.l  d0,d2           
        muls.l  #-161708,d3:d2 ;a*d^2
        add.l   d1,d0          ;a'=a+b  
        add.l   d3,d1          ;b'=b+a*d^2      
        dbf     d7,.slop1

;-----------------------------------------------------
; Steinmap
;-----------------------------------------------------
Stone	move.l	a6,a0
	add.l	#Map-CopperPtr,a0
	move.l	a0,a1
	move.l	#(256*512),d3
	move.l	d3,d7
	move.l	#$4af58c31,d0
	move.l	#$df76ec5a,d1
	addx.l	d0,d1
.Aa	move.b	-256(a0),d2
	add.b	-1(a0),d2
	lsr	d2
	eor.w	d0,d1
	addx.l	d0,d1
	ror.l	d1
	addx.l	d0,d0
	addx.l	d1,d0
	exg.l	d0,d1
	cmp.w	#$2000,d1
	bge.s	.Aab
	cmp.b	#$f,d2
	beq.s	.Aa
	addq	#01,d2
	bra.s	.Ad
.Aab	cmp.b	#$0,d2
	beq.s	.Aa
	subq	#01,d2
.Ad	move.b	d2,(a0)+
	subq.l	#01,d3
	bne.s	.Aa

	move.l	d7,d0	(256*512)
	move.l	a1,a0	Map
.Ae	lsl.w	(a0)
	lsl.w	(a0)+
	subq.l	#2,d0
	bne.s	.Ae

;-----------------------------------------------------
; Textures
;-----------------------------------------------------

	IFNE	_PREVIEW
	rts
*	bra.w	PrecAnm
	ENDC

	moveq.l	#1,d4		pixelwidth
	moveq	#2-1,d2		passes
	move.l	a1,a3		sourcedest
	bsr.w	Mix\.MultiSmuut	picsiz i d7!

	move.l	#(256*17)+11,d6	for litt variasjon i texturingen

	lea	Pal1(pc),a2
	move.l	a6,a3
	add.l	#Tex1-CopperPtr,a3	dest
	add.l	d6,a1
	move.l	a1,a0
;	add.l	#(256*76)+31,a0		source
	bsr.w	MapPal			picsiz*2 i d7!

	lea	Pal2(pc),a2
	move.l	a6,a3
	add.l	#Tex2-CopperPtr,a3	dest
	add.l	d6,a1
	move.l	a1,a0
;	add.l	#(256*76)+31,a0		source
	bsr.w	MapPal			picsiz*2 i d7!

	lea	Pal3(pc),a2
	move.l	a6,a3
	add.l	#Tex3-CopperPtr,a3	dest
	add.l	d6,a1
	move.l	a1,a0
;	add.l	#(256*76)+31,a0		source
	bsr.w	MapPal			picsiz*2 i d7!

*	lea	Pal4(pc),a2
*	move.l	a6,a3
*	add.l	#Tex4-CopperPtr,a3	dest
*	add.l	d6,a1
*	move.l	a1,a0
*;	add.l	#(256*76)+31,a0		source
*	bsr.w	MapPal			picsiz*2 i d7!

	move.l	a1,a5		org-stone
	lsr.l	d7		256*256

*	move.l	a6,a0
*	add.l	#Tex1-CopperPtr,a0
*	move.l	a6,a1
*	add.l	#Tex4-CopperPtr,a1
*	move.l	a5,a2
*	move.l	a6,a3
*	add.l	#Tex5-CopperPtr,a3	dest	1&4
*	bsr.w	Mix

;	move.l	a6,a0
;	add.l	#Tex2-CopperPtr,a0
;	move.l	a6,a1
;	add.l	#Tex3-CopperPtr,a1
;	move.l	a5,a2
;	move.l	a6,a3
;	add.l	#Tex4-CopperPtr,a3	dest	2&3
;	bsr.w	Mix

	move.l	a6,a0
	add.l	#Tex1-CopperPtr,a0
	move.l	a6,a1
	add.l	#Tex3-CopperPtr,a1
	move.l	a5,a2
	move.l	a6,a3
	add.l	#Tex3-CopperPtr,a3	dest	1&3
	bsr.w	Mix

	move.l	a6,a0
	add.l	#Tex1-CopperPtr,a0
	move.l	a6,a1
	add.l	#Tex2-CopperPtr,a1
	move.l	a5,a2
	move.l	a6,a3
	add.l	#Tex2-CopperPtr,a3	dest	1&2
	bsr.w	Mix
;-----------------------------------------------------------------
PrecAnm

	move.l	WChuAd(pc),-(sp)
	move.l	a6,d0
	add.l	#Anm2-(160*96*4)-CopperPtr,d0
	move.l	d0,(WChuAd-CopperPtr)(a6)

	move.l	#$00000011,-(sp)

.precanmA
	moveq	#0,d7
	bsr.w	NormCl

	lea	Snurr3d2(pc),a1
	lea	KubCor(pc),a0
	move.w	#300*2,d1		amp
	move.w	(sp),d0			ang
	moveq	#2-1,d7
.prepreA
	move.w	#-440,d6
.preA
	moveq	#4-1,d5
.A	move.l	a0,a2
	bsr.w	GetSC
	move.w	#(8*1)-1,d4
.Aa
	move.w	(a2)+,2(a1)

	move.w	(a2)+,(a1)
	add.w	d2,(a1)+

	add.w	d3,(a1)+

	move.w	(a2)+,(a1)
	add.w	d6,(a1)+

	dbra	d4,.Aa
	add.w	#256,d0
	dbra	d5,.A

	add.w	#16,d0
	add.w	#110,d6
	cmp.w	#500,d6
	blt.s	.preA

	add.w	#32,d0
	dbra	d7,.prepreA

	add.w	#16,(sp)
	move.w	#392,d0
	move.w	#1200*2,d1
	bsr.w	GetSC

;	move.w	d2,Cam
;	move.w	#1200,Cam+2
;	move.w	d3,Cam+4

	lea	Cam(pc),a4
	move.w	d2,(a4)+
	move.w	#1200,(a4)+
	move.w	d3,(a4)



	lea	Snurr3d2(pc),a4
	move.w	#(8*1*4*9)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex2-CopperPtr,a2
	move.w	#8-3,d6		punkter per obj
	moveq	#(4*1*9)-1,d7			antall obj
	bsr.w	Smuff

	move.l	a6,a4
	add.l	#Snurr3d2+(8*1*6*4*9)-CopperPtr,a4
	move.w	#(8*1*4*9)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex3-CopperPtr,a2
	move.w	#8-3,d6		punkter per obj
	moveq	#(4*1*9)-1,d7			antall obj
	bsr.w	Smuff

	bsr.w	Glow

	add.l	#160*96*4,(WChuAd-CopperPtr)(a6)
	sub.w	#01,2(sp)
	bne.w	.precanmA

	sub.l	#160*96*4*33,(WChuAd-CopperPtr)(a6)
	move.w	#$11,2(sp)
precanm2

	moveq	#0,d7
	bsr.w	NormCl

	move.w	#80,d4		vinkeloffset
	move.w	#6,a0		radiusdelta
	move.w	#(300/2)-1,d7	cordn
	bsr.w	SnurrCor

	lea	Snurr3d2(pc),a1
	lea	(Snurr3d-Snurr3d2)(a1),a0
	move.w	#800*2,d1		amp
	move.w	(sp),d0			ang
	moveq	#2-1,d6
.preA
	moveq	#4-1,d5
.A	move.l	a0,a2
	bsr.w	GetSC
	move.w	#300-1,d4
.Aa
	move.w	(a2)+,2(a1)
	move.w	(a2)+,(a1)
	add.w	d2,(a1)+
	add.w	d3,(a1)+
	move.w	(a2)+,(a1)+

	dbra	d4,.Aa
	add.w	#256,d0
	dbra	d5,.A

	add.w	#128,d0
	move.w	#600*2,d1
	dbra	d6,.preA

	add.w	#16,(sp)
	move.w	#-392,d0
	move.w	#2200*2,d1
	bsr.w	GetSC

	lea	Cam(pc),a4
	move.w	d2,(a4)+
	move.w	#800,(a4)+
	move.w	d3,(a4)

;	move.w	d2,Cam
;	move.w	#800,Cam+2
;	move.w	d3,Cam+4

	lea	Snurr3d2(pc),a4
	move.w	#(300*4)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex2-CopperPtr,a2
	move.w	#300-3,d6		punkter per obj
	moveq	#4-1,d7			antall obj
	bsr.w	Smuff

	move.l	a6,a4
	add.l	#Snurr3d2+(300*6*4)-CopperPtr,a4
	move.w	#(300*4)-1,a5
	bsr.w	FixCam
	move.l	a6,a2
	add.l	#Tex3-CopperPtr,a2
	move.w	#300-3,d6		punkter per obj
	moveq	#4-1,d7			antall obj
	bsr.w	Smuff

	bsr.w	Glow

	add.l	#160*96*4,(WChuAd-CopperPtr)(a6)
	sub.w	#01,2(sp)
	bne.w	precanm2


	addq.l	#4,sp
	move.l	(sp)+,(WChuAd-CopperPtr)(a6)

	moveq	#0,d7
	bsr.w	NormCl

	rts
;-----------------------------------------------------------------

;-----------------------------------------------------------------
;

MapPal	move.l	d7,d0
	lsr.l	d0
	moveq	#0,d4
.Ae2	move.b	(a0)+,d4
	lsr.b	#2,d4
	move.l	(a2,d4.l*4),(a3)+
	subq.l	#1,d0
	bne.s	.Ae2
	rts

Mix
	; a0: source1	a1: source2	a2: mask	a3:dest
	;
	add.l	d6,a5	change offset in original mask
	move.l	a3,d5
	moveq	#0,d2
	moveq	#0,d3
	moveq	#-1,d0		size = 256*256
.Aa	move.b	(a2)+,d3
	bsr.s	.Ac
	bsr.s	.Ac
	bsr.s	.Ac
	move.b	d2,(a3)+

	addq.l	#1,a0
	addq.l	#1,a1
	dbra	d0,.Aa

	moveq.l	#4,d4		pixelwidth
	moveq	#2-1,d2		passes
	move.l	d5,a3

.MultiSmuut
	move.l	d7,d3	d7=bildesiz
	muls.l	d4,d3	pixelwidth
	move.l	a3,a4	buffer
.Cc
	move.l	d4,d5
	lsl.l	#8,d5
	move.b	(a4,d4.l),d0
	add.b	(a4,d5.l),d0
	neg.l	d4
	neg.l	d5
	add.b	(a4,d4.l),d0
	add.b	(a4,d5.l),d0
	neg.l	d4

	lsr.b	#2,d0
	move.b	d0,(a4)+
	subq.l	#1,d3
	bne.s	.Cc
	dbra	d2,Mix\.MultiSmuut
	rts

.Ac	move.b	(a0)+,d2
	sub.b	(a1),d2
	ext.w	d2
	muls.w	d3,d2
	divs.w	#63,d2
	add.b	(a1)+,d2
	move.b	d2,(a3)+
	rts
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
superv		movec	vbr,a0
		rte

Interrupt	movem.l	d0-a6,-(sp)
		lea	CUSTOM,a5

		lea	VB(pc),a0
		addq.w	#1,(a0)+
		addq.w	#1,(a0)

		cmp.w	#51,(a0)	VB2
		bne.s	.rass
		clr.w	(a0)
		addq.w	#1,4(a0)
.rass

*	cmp.w	#51,VB2
*	bne.s	.rass
*	move.w	#0,VB2
*	add.w	#1,bla
*.rass

		move.w	#$070,$9c(a5)	sletter alle level tre...
*		move.w	#$070,$dff09c
		movem.l	(sp)+,d0-a6
		rte

	IFNE	_SOUND
; audio_interrupt
		cnop 0,4
initchannel
		 move.l a1,(a4)+ 
		 move.l #((65536/2)<<16)!lyd_speed,(a4)+ 
		 move.w #63,(a4)+ ;$a9(a5,d2.l)
		 add.w  #$10-$a,a4 
		 rts
audio_interrupt
		 movem.l  d0-d7/a0-a6,-(sp)
;		 lea    lyd_fastdata(pc),a6
		lea CopperPtr(pc),a6
		add.l #lyd_fastdata-CopperPtr,a6

		 move.l audiobase(a6),a4
		 move.w #%0000000010000000,$09c-$a0(a4)
		 move.w #%0000000010000000,$09c-$a0(a4)

		 move.l lyd_playlistptr(a6),a0
;		 tst.b  lyd_play(a6)
;		 beq.b  .noplay
		add.b	#1,lyd_play(a6)
		lea	VB2(pc),a3
		clr.w	(a3)
		addq.w	#1,4(a3)

*	clr.w	VB2
*	add.w	#1,bla
.startedplaying
		 move.w (a0)+,d0
		 move.l a0,lyd_playlistptr(a6)
.kanalonloop
		 move.w d0,d1
		 and.l  #$f,d1
		 swap   d1
		 move.l lyd_chn1ptr(a6),a1
		 add.l  d1,a1
		 bsr    initchannel
		 lsr.w  #4,d0
		 bne.b  .kanalonloop
.noplay
		 movem.l  (sp)+,d0-d7/a0-a6
		 rte
	ENDC
;------------------------------------------------------
clist		dc.w	$0100,$8a11
		dc.w	$0102,$0000
		dc.w	$0104,$0000
		dc.w	$008e,XSTRT+(YSTRT*256)
		dc.w	$0090,(XSTOP-256)+(YSTOP-256)*256
		dc.w	$0092,(HSTRT/2-8)
		dc.w	$0094,(HSTRT/2-8)+(8*((WIDTH/16)-1))
		dc.w	$0108,-88
		dc.w	$010a,-8
		dc.w	$01fc,$4003
		dc.w	$ffff,$fffe
clistE


;------------------------------------------------
;------------------------------------------------
Pal1	incbin	/dest/data/1b
Pal2	incbin	/dest/data/2b
Pal3	incbin	/dest/data/3b

KubCor	dc.w	-044,-044,-044
	dc.w	-044,0044,-044
	dc.w	0044,-044,-044
	dc.w	0044,0044,-044
	dc.w	0044,-044,0044
	dc.w	0044,0044,0044
	dc.w	-044,-044,0044
	dc.w	-044,0044,0044

;	dc.w	0044,-044,-044
;	dc.w	0044,-044,0044
;	dc.w	-044,-044,-044
;	dc.w	-044,-044,0044
;	dc.w	-044,0044,-044
;	dc.w	-044,0044,0044
;	dc.w	0044,0044,-044
;	dc.w	0044,0044,0044

Logo	dc.l	$ecea0000,$00aa8a00,$0000acee,$000000aa,$82000000,$ecee0000
	dc.l	$00000000,$0000eae0,$0000004a,$80000000,$4ae00000,$004e8000
	dc.l	$00004ae0,$00000000,$00000000,$ebb80000,$008a1000,$0000eb90
	dc.l	$00000088,$90000000,$8b900000,$00800000,$0000eb80,$000000aa
	dc.l	$00000000,$aa800000,$00ea8000,$00008b80,$00000080,$00000000
	dc.l	$eeeeece8,$8e8aa88a,$a888eaa8,$eca88e2e,$e88ae882,$28aeecae
	dc.l	$eee00000,$0000eeab,$3bbb808a,$aaaa2a80,$eaaaabaa,$808eeab2
	dc.l	$2b8088ab,$2baa80e0,$00000000

;	dc.l	$e8aaaa00,$0088aeea,$0000e8aa
;	dc.l	$ae000028,$aaa20000,$2eeaae00,$00e00000,$00008eec,$eeeee08a
;	dc.l	$aa8aaaa0,$8aaaeaaa,$a08aea8c,$cac08eac,$eaaea0e0,$00000000

bredde	=	80
hoyde	=	96
		include	/c2psmall6bplFIX.s
	IFNE	_SOUND
		include	_fist_music_data.i
	ENDC
_codeEnd

;--------------------------------------------------------------------
;----- Fastmem-buffer -----------------------------------------------
CopperPtr	dc.l	0
VB		dc.w	0		Vertical Blanking Timer
VB2		dc.w	0
se		dc.w	0
timeflip	dc.w	0
ScrnAd		dc.l	0
ShowAd		dc.l	0
WChuAd		dc.l	0
ChuAd		dc.l	0

Flipp		dc.w	0
NewStencils	dc.w	0
Depth		dc.w	00
Matrx		ds.w	3*3
CamTilt		dc.w	0
Cam
worldposx	dc.w	0
worldposy	dc.w	0
worldposz	dc.w	0
SinTab		ds.l	2048
CosTab		=	SinTab+(256*4)
MaxCorN		=	600*4
rotatedcords	ds.w	2*MaxCorN
TStruct		ds.w	2*MaxCorN
Snurr3d2	ds.w	MaxCorN*3
Snurr3d		ds.w	600*3

	IFNE	_SOUND
lyd_fastdata    ds.b lyd_sizeofvar+(65536*10*2)
	ENDC


Chunk1		ds.l	160*96		RGBB buf
Chunk2		ds.l	160*96
WBuf2		ds.l	160*96
		ds.l	160
WBuf		ds.l	256*128

		ds.b	256
Map		ds.b	256*512		steinmap
		ds.b	256

	IFEQ	_PREVIEW
Tex1		ds.l	256*256
Tex2		ds.l	256*256
Tex3		ds.l	256*256
Tex4		ds.l	256*256
	ELSE
Tex1	incbin	!prev_tex
Tex2	=	Tex1+256*256*4
Tex3	=	Tex2+256*256*4
Tex4	=	Tex3+256*256*4
	ENDC

		ds.l	160*96
	IFEQ	_PREVIEW
Anm1		ds.l	160*96*16
Anm2		ds.l	160*96*16
	ELSE
Anm1	incbin	!prev_anm
Anm2	=	Anm1+(160*96*4*16)
	ENDC

Anm2E
_fastmemEnd
;---------------------------------------------------------------------
;----- Chipmem-buffer ------------------------------------------------
		section	pungkugle,bss_c
tsjippmem
_Bpls		ds.w	8*4
_Coppr		ds.w	11*2
		ds.w	2		8 byte allign
		cnop	0,8
_Scrn1		ds.w	8000*8
_Scrn2		ds.w	8000*8
_HamScr		ds.w	8000*2
	ds.w	10240*10
lyd_chipdata    ds.b    10*65536
tsjippmemE

	PRINTT
	PRINTT	"CodeSize: "
	PRINTV	_codeEnd-_codeStart
	PRINTT	"FastmemSize: "
	PRINTV	_fastmemEnd-_codeStart
	PRINTT	"ChipmemSize: "
	PRINTV	tsjippmemE-tsjippmem

