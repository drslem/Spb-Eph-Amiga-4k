
;INTB_VERTB equ 5
lyd_hoyestespornr equ 8
lyd_speed   equ 265
wavebytelength equ 360+90

;		section init,code_f


;//s Variables, pointers etc.
					RSRESET
sin                 rs.w    wavebytelength
fuckedup            rs.w    wavebytelength
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
audiobase           rs.l    1
lyd_chn1ptr         rs.l    1
lyd_sparechannel    rs.l    1
lyd_sizeofvar       rs      0

;//e


;	include	include:AllOfIt_lvo.i

Main_start

Take_system
; save old DMACONR and INTENAR + set bits for restore
;		move.w  $dff01c,d0
;		ori.w   #$c000,d0
;		move.w  d0,-(sp) ;old_intena

; audio interrupt setup
;		lea     super(pc),a5
;		move.l  4.w,a6
;		jsr     _LVOSupervisor(a6)

;		move.l  $70(a6),-(sp) ;old_lev4
;		move.l  #audio_interrupt,$70(a6)
;		move.l  a6,-(sp) ; vbr_addr

;-------------------------------------------------------
;-------------------------------------------------------

		lea     lyd_fastdata(pc),a6
		lea     lyd_sizeofvar(a6),a5
		move.l  a5,lyd_sparechannel(a6)
;		move.l  #lyd_chipdata,lyd_chn1ptr(a6)
		move.l	CopperPtr(pc),d7
		add.l	#lyd_chipdata-tsjippmem,d7
		move.l	d7,lyd_chn1ptr(a6)
		move.l  #$dff0a0,audiobase(a6)

calc_waveforms
		move.l    a6,a3
		move.w    #180-1,d7
		move.l    #-364*90,d5
		move.l    #360/2,d6 ;364,d6
.cw0_180                                   ;d0=X
		move.w    d7,d1
		muls.w    d1,d1           ;d1=X^2

		muls.w    #-4,d1    ; -4x²
		move.w    d7,d3
		muls.w    #724,d3   ; + 724x
		add.l     d3,d1     ; sine
		move.w    #16384>>8,d2 ; peak of squarewave
		add.l     d6,d5
		asr.l     #8,d1

		move.l    d5,d4
		asr.l     #8,d4
		move.w    d4,triangle(a3)
		move.w    d2,square(a3)
		move.w    d1,(a3)+
		move.w    d1,360*2-2(a3) ; OBS DETTE Skriver over deler av fuckedup

		neg.w     d1
		neg.w     d2
		move.w    d1,180*2-2(a3) ; sin
		move.w    d2,180*2-2+square(a3)
		move.w    #32760,180*2-2+fuckedup(a3)
		neg.w     d4
		move.w    d4,180*2-2+triangle(a3)
		dbf       d7,.cw0_180


sound_engine
		 lea     lyd_sagakrakken(pc),a0 ; song
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
		mulu.l  #wavebytelength,d0
		lea     (a6,d0.l),a2
		move.b  (a0)+,d4 ; note
		btst    #0,d1    ; instrument type
		bne.b   .synth2

; tromme
		moveq   #0,d1
		move.w  lyd_type2(a6),d1
		lsl.w   #2,d1 ; * 1024 triks
		move.w  d1,a4 ;lyd_trommeseighet(a6)
		and.b   #$1f,d4
		swap    d4
		move.w  #90,lyd_vent270(a6)
		move.l  #360<<16,d2
.cal_tromme
		subq.w  #1,d5
		bpl.b   .ikke90eller270grader
		sub.l   d4,d1
		bpl.b   .not01
		add.l   d2,d1 ;#360*65536,d1
		bsr.b   .dcay_rutine
.not01
		move.l  d1,d3
		moveq   #0,d0
		move.b  -1(a0),d0
		lsr.b   #5,d0
		mulu.l  d0,d3

		; modulo 360
		divu.l  d2,d0:d3 ;#360*65536,d0:d3
		swap    d0
		move.w  (a2,d0.w*2),d3  ; d3 = sound data from wave
		sub.l   d4,lyd_vent270(a6)
		bpl.b   .ikke90eller270grader
		move.w  #180,lyd_vent270(a6)
		bsr.b   .ifgradergreie
.ikke90eller270grader
		add.w   d3,(a3)+
		subq.l  #2,d7
		bne.b  .cal_tromme
		bra.w  .record

.ifgradergreie
		addq.w  #1,d6
		move.w  d6,d5
.dcay_rutine
		sub.l   a4,d4
		bpl.b   .ikkegrader
		moveq   #0,d4
.ikkegrader
		rts

.lyd_skala   dc.b 214,202,190,180,170,160,151,143,135,127,120,113

.synth2
		move.b  d4,d3
		and.b   #$f,d4  ; mask out notebits
		lsr.b   #4,d3
		move.b  .lyd_skala-*(pc,d4.l),d4
.synth
		move.l  #360*65536/2,d0
		divs.l  d4,d0
		move.b  d3,d4
		bclr    #3,d3
		lsl.l   d3,d0 ; oktav
		btst    #3,d4
		bne.b   .ferdigmednote ; set pitch
		; d2 = destpitch
		move.l  d0,d2
		move.l  lyd_oldpitch(a6),d0
.mekklyd
		;modulo
		moveq   #0,d4
		add.l   d0,d1
		exg.l   d1,d4
		divu.l  #360*65536,d1:d4
		move.l  d1,d4
		swap    d4
		move.w  (a2,d4.w*2),d3
		add.w   d3,(a3)+ ; add into channel

		move.b  lyd_type2(a6),d3
		btst    #0,d3 ;lyd_type2(a6)
		beq.b   .nocut
		and.b   #$3,-2(a3)
.nocut
		lsr.b   #1,d3
		move.l  #1000,d4
		asl.l   d3,d4

		cmp.l   d2,d0 ;lyd_destpitch(a6),d0 ;d4
		bge.b   .pitchenerlavere
		neg.l   d4
.pitchenerlavere
		sub.l   d4,d0
.ikkemerepitchbend
		subq.w  #2,d7
		bne.b   .mekklyd

		sub.w   #1<<4,lyd_chorus(a6)
		beq.b   .ferdigmednote
		move.l  lyd_notelengde(a6),d7
		sub.l   d7,a3
		move.l  d0,d1 ; hmm ?
		lsl.l   #1,d0
		bra.b   .mekklyd
.ferdigmednote
		move.l  d0,lyd_oldpitch(a6)
		bra.w   .record

.exit

; echo
		move.l  lyd_sparechannel(a6),a3
		move.l  lyd_chn1ptr(a6),a1
		moveq   #lyd_hoyestespornr,d0 ;hoestesppornr
.echochannels
		moveq   #0,d4
		moveq   #0,d7
		moveq   #0,d1
		moveq   #0,d3
		move.b  (a0)+,d4 ;echo decay
		move.b  (a0)+,d3 ;echo lengde
		asl.w   #8,d3
		move.l  a3,a4
.echo
		move.w  (a4)+,d2
		ext.l   d2
		muls.w  d4,d2
		asr.l   #8,d2
		add.w   d2,(a3,d3.l*2)
.finnmax
		move.w  (a3,d3.l*2),d2
		bpl.b   .absval
		neg.w   d2
.absval     
		cmp.w   d2,d1
		bge.b   .ikkestorre
		move.w  d2,d1
.ikkestorre
		addq.w  #1,d3
		subq.w  #1,d7
		bne.b   .echo

;d1=maxvolum
;a3=start av 16bits kanal
;a1=start av 8bits kanal lyd_chn1ptr
		moveq   #-127,d3
		moveq   #127,d4
.konverter
		move.w  (a3)+,d5
		ext.l   d5
		asl.l   #8,d5

		divs.l  d1,d5
		cmp.l   d3,d5
		bge.b   .okia
		move.l  d3,d5
.okia
		cmp.l   d4,d5
		ble.b   .okia2
		move.l  d4,d5
.okia2
		move.b  d5,(a1)+ ;8bit inn i lydsporet

		sub.w   #1,d7
		bne.b   .konverter
		dbra     d0,.echochannels

		move.l  a0,lyd_playlistptr(a6)
; init hardware
		bset.b  #1,$bfe001
		move.l  audiobase(a6),a4
		moveq   #3,d2
.initsoundhw
		bsr     initchannel
		dbf     d2,.initsoundhw

;		move.w  $02-$a0-$40(a4),d0
;		bset    #15,d0
;		move.w  d0,-(sp) ;old_dmacon
;		move.w  #$7fff,$096-$a0-$40(a4)
;		move.w  #%1100000010000000,$09a-$a0-$40(a4)
;		move.w  #%1000001111001111,$096-$a0-$40(a4) ; start dma

;		move.b  #1,lyd_play(a6)
;waitloop
;		btst    #2,$dff016
;		bne.b   waitloop
;//s  restore system

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




; audio_interrupt
;		cnop 0,4
;initchannel
;		 move.l a1,(a4)+ 
;		 move.l #((65536/2)<<16)!lyd_speed,(a4)+ 
;		 move.w #63,(a4)+ ;$a9(a5,d2.l)
;		 add.w  #$10-$a,a4 
;		 rts
;audio_interrupt
;		 movem.l  d0-d7/a0-a6,-(sp)
;		 lea    lyd_fastdata,a6
;
;		 move.l audiobase(a6),a4
;		 move.w #%0000000010000000,$09c-$a0(a4)
;		 move.w #%0000000010000000,$09c-$a0(a4)
;
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
;		 bsr    initchannel
;		 lsr.w  #4,d0
;		 bne.b  .kanalonloop
;.noplay
;		 movem.l  (sp)+,d0-d7/a0-a6
;		 rte

;-------------------------------------------------------------
;		section fast,bss_f
;lyd_fastdata    ds.b lyd_sizeofvar+(65536*10*2)

;		section chip,bss_c
;lyd_chipdata    ds.b    10*65536


