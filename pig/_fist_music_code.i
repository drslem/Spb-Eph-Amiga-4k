;INTB_VERTB equ 5
lyd_hoyestespornr equ 7
lyd_speed   equ 220 ;15 ;23
wavebytelength equ 256
engine equ 1
song equ 1
;debug equ 0
;		section fesbikkje,code_f

;//s Variables, pointers etc.
					RSRESET
sin                 rs.w    wavebytelength
*yo                  rs.w    wavebytelength
triangle            rs.w    wavebytelength
square              rs.w    wavebytelength
lyd_trommeseighet   rs.l    1
lyd_destpitch       rs.l    1
lyd_oldpitch        rs.l    1
lyd_vent270         rs.l    1
lyd_phaser          rs.l    1
lyd_chorus          rs.l    1
lyd_notelengde      rs.l    1
lyd_playlistptr     rs.l    1
lyd_type            rs.b    1  ; obs.. lyd_type1 og lyd_type2 må ikke flyttes
lyd_type2           rs.b    1
					rs.w    1  ; dummy word for å spare en shift lenger nedi her
lyd_play            rs.w    1
lyd_vari            rs.l    1
audiobase           rs.l    1
lyd_chn1ptr         rs.l    1
lyd_sparechannel    rs.l    1
lyd_sizeofvar       rs      0

;//e

;		include "dev:include/myincludes"

Main_start

Take_system
; save old DMACONR and INTENAR + set bits for restore
;		move.w  $dff01c,d0
;		ori.w   #$c000,d0
;		move.w  d0,-(sp) ;old_intena

; audio interrupt setup
;		lea     super(pc),a5
;		move.l  4.w,a6
;		jsr     _lvosupervisor(a6)

;		move.l  $70(a6),-(sp) ;old_lev4
;		move.l  #audio_interrupt,$70(a6)
;		move.l  a6,-(sp) ; vbr_addr

;-------------------------------------------------------
;-------------------------------------------------------

;		lea     lyd_fastdata(pc),a6
	lea	CopperPtr(pc),a6
	add.l	#lyd_fastdata-CopperPtr,a6
		lea     lyd_sizeofvar(a6),a5
		move.l  a5,lyd_sparechannel(a6)
;		move.l  #lyd_chipdata,lyd_chn1ptr(a6)
		move.l	CopperPtr(pc),d7
		add.l	#lyd_chipdata-tsjippmem,d7
		move.l	d7,lyd_chn1ptr(a6)
;//e
;//s Business

;//s Waveforms
calc_waveforms
		move.l    a6,a3
		moveq     #(wavebytelength/2)-1,d7
.cw0_180                                   ;d0=X
		move.w  d7,d1
		moveq   #-128,d4
		add.w   d1,d4
		add.w   d1,d4
		sub.w   #63,d1
		muls.w  d1,d1           ;d1=(X-64)^2
		moveq   #127,d2   ; peak of squarewave
		asr.l   #7-2,d1   ; *1/32
		sub.w   #$80,d1
		move.w  d2,square(a3)
		move.w  d4,triangle(a3)
		move.w  d1,(a3)+
		not.w   d1
		not.w   d2
*        neg.w   d4
		move.w  d1,(wavebytelength/2)*2-2(a3) ; sin
		move.w  d2,(wavebytelength/2)*2-2+square(a3)
		move.w  d4,(wavebytelength/2)*2-2+triangle(a3)
		dbf     d7,.cw0_180
;//e
;//s parsing
sound_engine
		 lea     lyd_taubern(pc),a0 ; song
; Parse sound data
.nestekanal
		moveq   #0,d0
		move.b  (a0)+,d0  ;kanal
		cmp.b   #31,d0
		beq.w     .exit
		move.w  (a0)+,lyd_type(a6) ; henter inn til lydtype1 og 2
		swap    d0
		move.l  lyd_sparechannel(a6),a1
		add.l   d0,a1
.record
		move.l  a1,a3
		moveq   #0,d7
		moveq   #0,d6
		move.b  (a0),d7 ; kanalnr eller lengde og posisjon (L,P)
		cmp.b   #32,d7  ; varsku denne
		blo.b   .nestekanal
		addq.w  #1,a0
		;d7=LLLPPPPP
		moveq   #11,d5
		lsl.l   d5,d7
		;LLL PPPPP000 00000000
		move.w  d7,d6 ; d6=16x0 PPPPP000 00000000
		; hvis ting går på ræva, så husk på at du gambler på at øverste
		; wordet i d6 er 0x0000
		add.l   d6,a3 ; legger til to ganger pga wordsize
		add.l   d6,a3
		clr.w   d7    ; d7=00000000 00000LLL 000...0
		lsr.l   #5,d7 ; d7=000LLL00 00000000 (L*2^10)
		move.l  d7,lyd_notelengde(a6)
.okidoki
		moveq   #0,d0
		moveq   #0,d4
		moveq   #0,d5
		moveq   #0,d6
		move.b  lyd_type(a6),d0
		move.b  d0,d1
		move.b  d0,d2
		and.b   #$7<<1,d0   ; mask out waveform nr bits
		and.w   #$7<<4,d2   ; mask out chorus bits
		move.w  d2,lyd_chorus(a6)
		lsl.w   #8,d0
*        mulu.l  #wavebytelength,d0
		lea     (a6,d0.w),a2
		move.b  (a0)+,d4 ; note
		btst    #0,d1    ; instrument type
		bne.b   .synth2
;//e
;//s tromme
		moveq   #0,d1
		move.b  lyd_type2(a6),d1
	;   lsl.w   #2,d1       ; * 1024
		move.w  d1,a4       ; a4 = amount of frequency decay per period

		and.b   #$1f,d4     ; notedata, initial frequency
		lsl.l   #8,d4
		lsl.l   #4,d4
		*swap    d4          ; <<16
		move.l  #63<<8,d2  ; lyd_vent270(a6)
		clr.l   d1          ; t parameter in sin(t)
.calc_tromme
		subq.w  #1,d5
		bpl.b   .ikke90eller270grader

		; after one period, the frequency is decremented
		sub.l   d4,d1
		bpl.b   .continuewithsamefrequency
		add.l   #256<<8,d1 ; period of 256 16:16 format
		bsr.b   .decay_frequency
.continuewithsamefrequency
		move.l  d1,d3 ; t
		moveq   #0,d0
		move.b  -1(a0),d0 ; variation
		lsr.b   #5,d0
		mulu.l  d0,d3 ; vt
		lsr.w   #8,d3

		move.w  (a2,d3.w*2),d3  ; d3 = sound data from wave
		sub.l   d4,d2 ; lyd_vent270(a6)
		bpl.b   .ikke90eller270grader
		add.l   #128<<8,d2 ;lyd_vent270(a6)
		bsr.b   .ifgradergreie
.ikke90eller270grader
		add.w   d3,(a3)+
		subq.l  #2,d7
		bne.b  .calc_tromme
		bra.w  .record

.ifgradergreie
		addq.w  #1,d6 ; d6 = width of clip area
		move.w  d6,d5
*        lsr.w   #1,d5
.decay_frequency
		sub.l   a4,d4
*        bpl.b   .frequencynotbelowzero
*        moveq   #0,d4
.frequencynotbelowzero
		rts
;//e
;//s Skala
.c equ 214
*.ch equ 202
*.d equ 190
*.dh equ 180
*.e equ 170
*.f equ 160
*.fh equ 151
.g equ 143
.gh equ 135
*.a equ 127
.ah equ 120
*.b equ 113

.lyd_skala
			 dc.b .c
*             dc.b .ch
*             dc.b .d
*             dc.b .dh
*             dc.b .e
*             dc.b .f
*             dc.b .fh
			 dc.b .g
			 dc.b .gh
*             dc.b .a
			 dc.b .ah
*             dc.b .b

		even
;//e
;//s Synth
.synth2
		move.b  d4,d3
		and.b   #$7,d4  ; mask out notebits
		lsr.b   #3,d3
		move.b  .lyd_skala-*(pc,d4.l),d4
.synth
		move.l  #256*65536/3,d2
		divs.l  d4,d2   ; pitch
		move.b  d3,d4
		and.b   #$07,d3 ;oktav
		lsl.l   d3,d2   ; sett rett oktav
		lsr.b   #3,d4
		beq.b   .noprepitch
		move.l  d2,d3
		lsl.l   #3,d3
		lsl.l   d4,d3
		move.l  d3,lyd_oldpitch(a6)
.noprepitch
		; d2 = destpitch
*        move.l  d0,d2
		move.l  lyd_oldpitch(a6),d0
.nextlayer
*        moveq   #0,d1
.mekklyd
		;modulo
		add.l   d0,d1
		move.l  d1,d4
		swap    d4
		and.w   #$ff,d4
		move.w  (a2,d4.w*2),d3
		add.w   d3,(a3)+ ; add into channel

		move.b  lyd_type2(a6),d3
		lsr.b   #1,d3
		moveq   #127,d4
		asl.l   d3,d4

		cmp.l   d2,d0 ;lyd_destpitch(a6),d0 ;d4
		bge.b   .pitchenerlavere
		neg.l   d4
.pitchenerlavere
		sub.l   d4,d0
.ikkemerepitchbend
		subq.w  #2,d7
		bne.b   .mekklyd

		sub.b   #1<<4,lyd_chorus+1(a6)
		beq.b   .ferdigmednote
		move.l  lyd_notelengde(a6),d7
		sub.l   d7,a3
		lsl.l   #1,d0
		bra.b   .nextlayer
.ferdigmednote
		move.l  d0,lyd_oldpitch(a6)
		bra.w   .record
.exit
;//e
;//s Post process
		move.l  lyd_sparechannel(a6),a4
		move.l  lyd_chn1ptr(a6),a1
		moveq   #lyd_hoyestespornr,d0 ;hoestesppornr
.echochannels
		moveq   #0,d4
		moveq   #0,d7
		moveq   #0,d3
		moveq   #0,d6 ; for filter
		move.b  (a0)+,d4 ;echo decay
		move.b  (a0)+,d3 ;echo lengde
		lsl.w   #8,d3
		move.l  a4,a3
.echo
		move.w  d6,d5
		sub.w   (a4)+,d5
		asr.w   #1,d5
		sub.w   d5,d6
		move.w  d6,d2
		ext.l   d2
		muls.l  d4,d2
		asr.l   #8,d2
		add.w   d2,(a3,d3.l*2) ;,d2 ;,(a3,d3.l*2) ;,(a3,d3.l*2)
		addq.w  #1,d3
		subq.w  #1,d7
		bne.b   .echo

;a3=start av 16bits kanal
;a1=start av 8bits kanal lyd_chn1ptr
		moveq   #0,d1
		moveq   #0,d6
		move.b  (a0)+,d1 ; phaser frequency
		move.l  d1,a5
		move.b  (a0)+,d6 ; loop point
		mulu.w  #8192/2,d6 ;
		subq.l  #1,d6
		moveq   #0,d1
		sub.l   a2,a2
.konverter
		add.l   a5,a2
		move.l  a2,d2
		lsr.w   #8,d2
		moveq   #0,d4
		move.w  (a6,d2.w*2),d4
		asr.w   #1,d4
		add.w   d1,d4 ;a3
		move.w  (a3,d4.l*2),d5  ; vibrato signal
		add.w   (a3,d1.l*2),d5  ; + normal signal = phaser
		addq.l  #1,d1
		and.l   d6,d1
		asr.w   #1,d5
.abs
		moveq   #-128,d3
		cmp.w   d3,d5
		bge.b   .okia
		move.w  d3,d5
.okia
		not.w   d3
		cmp.w   d3,d5
		ble.b   .okia2
		move.w  d3,d5
.okia2
		move.b  d5,(a1)+ ;8bit inn i lydsporet
		subq.w  #1,d7
		bne.b   .konverter
		dbra     d0,.echochannels
;//e
;//s Hardware
		move.l  a0,lyd_playlistptr(a6)
; init hardware
		bset.b  #1,$bfe001
		move.l  #$dff0a0,audiobase(a6)
		move.l  audiobase(a6),a4
		moveq   #3,d2
.initsoundhw
		bsr.w   initchannel
		dbf     d2,.initsoundhw

;		move.w  $02-$a0-$40(a4),d0
;		bset    #15,d0
;		move.w  d0,-(sp) ;old_dmacon
;		move.w  #$7fff,$096-$a0-$40(a4)
;		move.w  #%1100000010000000,$09a-$a0-$40(a4)
;		move.w  #%1000001111001111,$096-$a0-$40(a4) ; start dma
;		move.b  #1,lyd_play(a6)
;//e
;//s main
;waitloop
;		IFEQ    debug
;		btst    #2,$dff016
;		bne.b   waitloop
;		ENDC

;//s restore system

;Restore_system
;		move.l  audiobase(a6),a5
;		move.w  #$7fff,d0
;		move.w  d0,$9a-$a0(a5)      ;restore registers
;		move.w  d0,$96-$a0(a5)
;		move.w  (sp)+,$96-$a0(a5)   ;old_dmacon,$dff096
;		move.l  (sp)+,a6        ;vbr_addr ;,a6
;		move.l  (sp)+,$70(a6)   ;old lev4
;		move.w  (sp)+,$9a-$a0(a5)   ;old_intena,$dff09a
;		rts
;		cnop    0,4
;super   movec   vbr,a6
;		rte
;//e
;//s audio_interrupt
;		cnop 0,4
;initchannel
;		 move.l a1,(a4)+
;		 move.l #((65536/2)<<16)!lyd_speed,(a4)+
;		 move.w #63,(a4)+ ;$a9(a5,d2.l)
;		 addq.w #$10-$a,a4
;		 rts

;audio_interrupt
;		 movem.l  d0-d7/a0-a6,-(sp)
;		 lea    lyd_fastdata,a6

;		 move.l audiobase(a6),a4
;		 move.w #%0000000010000000,$09c-$a0(a4)
;		 move.w #%0000000010000000,$09c-$a0(a4)

;		 move.l lyd_playlistptr(a6),a0
;		 tst.b  lyd_play(a6)
;		 beq.b  .noplay
;.startedplaying
;		 move.w (a0)+,d0
;		 move.l a0,lyd_playlistptr(a6)
;.kanalonloop
;		 move.w d0,d1
;		 and.l  #$f,d1
;		 swap   d1
;		 move.l lyd_chn1ptr(a6),a1
;		 add.l  d1,a1
;		 bsr.s  initchannel
;		 lsr.w  #4,d0
;		 bne.b  .kanalonloop
;.noplay
;		 movem.l  (sp)+,d0-d7/a0-a6
;		 rte
;//e

;//e

;//e
