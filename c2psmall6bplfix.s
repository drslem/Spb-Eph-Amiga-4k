;6bpl c2p for 4k.
;max. 320*256 screen.
;SLOW AND NOT OPTIMIZED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;CODE Not TESTED
;Coded by sp^ctz (Rune Stensland) 2002
;76 bytes (inc rts). a0 pointer to chunky buffer a1 pointer to bitplane.

; small fixes by Slummy in 2003

;bredde	=	40
;hoyde	=	200
;c2p
C2P0604K:
aaa
	lea	(bredde*hoyde*2)(a1),a1
;        move.l  #bredde*hoyde/4,a3
	lea	(bredde*hoyde/4).w,a3
.loop3:
        move.w  #8,a2
.loop2:
        moveq.l #4-1,d7
        move.l  (a0)+,d0 ;32bit
.loop
        lsl.l   #2,d0
        add.l   d0,d0   ;23456000
        addx.l  d1,d1   ;00000001
        add.l   d0,d0   ;34560001
        addx.l  d2,d2   ;00000002
        add.l   d0,d0   ;45600000
        addx.l  d3,d3   ;00000003
        add.l   d0,d0   ;56000000
        addx.l  d4,d4   ;00000004
        add.l   d0,d0   ;60000000
        addx.l  d5,d5   ;00000005
        add.l   d0,d0   ;00000000
        addx.l  d6,d6   ;00000006

        dbf     d7,.loop

        subq.l  #1,a2
        move.l  a2,d7
        bne.b   .loop2

;	move.l  d1,bredde*hoyde*5/1(a1)
;	move.l  d2,bredde*hoyde*4/1(a1)
;        move.l  d3,bredde*hoyde*3/1(a1)
;        move.l  d4,bredde*hoyde*2/1(a1)
;        move.l  d5,bredde*hoyde*1/1(a1)
;        move.l  d6,(a1)+


	move.l  d1,bredde*hoyde*3(a1)		;5
	move.l  d2,bredde*hoyde*2(a1)		;4
        move.l  d3,bredde*hoyde*1(a1)		;3
        move.l  d5,-(bredde*hoyde*1)(a1)	;1
        move.l  d6,-(bredde*hoyde*2)(a1)	;0
        move.l  d4,(a1)+			;2

        subq.l  #1,a3
        move.l  a3,d7
        bne.b   .loop3
bbb
	rts
