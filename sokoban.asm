include "gbhw.inc"

include "dma.inc"		; allows us to use dma_Copy2HRAM macro
include "sprite.inc"		; gives us spr_* macros to modify all sprites

;-------------- INTERRUPT VECTORS ------------------------
; specific memory addresses are called when a hardware interrupt triggers

SECTION "Game Data",ROM0
GAME_TILE_DATA:
DB $fb,$44,$fb,$44,$ff,$38,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$03,$01,$0a,$00,$0f
DB $00,$00,$00,$00,$00,$38,$38,$e6,$fe,$c1,$d2,$3f,$12,$ff,$3f,$c0
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$80,$00,$80

; Vertical-blank triggers each time the screen finishes drawing. Video-RAM
; (VRAM) is only available during VBLANK. So this is when updating OAM /
; sprites is executed.
SECTION "Vblank", ROM0[$0040]
	JP	DMA_ROUTINE

SECTION "LCDC", ROM0[$0048]
	reti

SECTION "Timer", ROM0[$0050]
	reti

SECTION "Serial", ROM0[$0058]
	reti

SECTION "Joypad", ROM0[$0060]
	reti
;----------- END INTERRUPT VECTORS -------------------

SECTION "ROM_entry_point", ROM0[$0100]	; ROM is given control from boot here
	nop
	jp	code_begins

;------------- BEGIN ROM HEADER ----------------
; The gameboy reads this info (before handing control over to ROM)
SECTION "rom header", ROM0[$0104]
	NINTENDO_LOGO
	ROM_HEADER	"macro and dma  "

; here's where you can include additional .asm modules
include "memory.asm"

	SpriteAttr	player1	; declare "player1" as a sprite
	SpriteAttr	player2

code_begins:
	di	; disable interrupts
	ld	SP, $FFFF	; set stack to top of HRAM

	dma_Copy2HRAM	; sets up routine from dma.inc that updates sprites

	ld	a, IEF_VBLANK	; --
	ld	[rIE], a	; Set only Vblank interrupt flag
	ei			; enable interrupts. Only vblank will trigger

	ld	a,0		;
	ldh	[rLCDC],a	;turn off LCD

	call LOAD_TILES
	call LOAD_MAP
	ld	a,%11100100	;load a normal palette up 11 10 01 00 - dark->light
	ldh	[rBGP],a	;load the palette
	ldh	[rOBP0],a	;load the palette

	ld	a, [rLCDC]	; fetch LCD Config. (Each bit is a flag)
	or	LCDCF_OBJON	; enable sprites through "OBJects ON" flag
	or	LCDCF_OBJ16	; enable 8bit wide sprites (vs. 16-bit wide)
	or	%10010001
	ld	[rLCDC], a	; save LCD Config. Sprites are now visible.


	; see where we declare "player1" as a sprite-variable above
	; set X=20, Y=10, Tile=$19, Flags=0
	PutSpriteXAddr	player1, 16 * 5 ; * 6
	PutSpriteYAddr	player1, 16 * 4 ; * 5
	sprite_PutTile	player1, $02
	sprite_PutFlags	player1, $00

	PutSpriteXAddr	player2, 16 * 5 + 8
	PutSpriteYAddr	player2, 16 * 4
	sprite_PutTile	player2, $02
	sprite_PutFlags	player2, $20

	ld e, $16
.loop
	halt	; halts cpu until interrupt triggers (vblank)
 	; by halting, we ensure that .loop only runs only each screen-refresh,
	; so only 60fps. That makes the sprite movement here manageable
	nop

	; Check whether the y-coordinate % 16 is 0
	GetSpriteYAddr	player1
	push af
	and $f
	jr z, .pop_af
	pop af
	add	a, e
	PutSpriteYAddr	player1, a
	PutSpriteYAddr	player2, a
	jp .loop

.pop_af
	pop af
.check_hor
	; Check whether the x-coordinate % 16 is 0
	GetSpriteXAddr	player1
	ld	d, a
	and $f
	jr z, .cont
	ld	a, d
	add	a, e
	PutSpriteXAddr	player1, a
	add a, 8 ; Because the second sprite is 8 pixels to the right
	PutSpriteXAddr	player2, a
	jr .loop

; From here, should check the current direction and stop the player
; from moving if there's something blocking

.cont
	call	jpad_GetKeys

	; move character if corresponding button has been pushed
	push	af	; save register A (joypad info)
	and	PADF_UP	; compare joypad info. Set NZ flag if UP bit present
	jr	z, .skip_up
	ld	e, -1
	; Begin: Collision detection (UP)
	push af
	push hl
	call GetTile
	sub a, $40
	jr nc, .no_borrow
	dec h
.no_borrow
	ld l, a ; Place lower byte of tile address into l
	ld	a, [hl]
	or a ; Set zero flag
	pop hl
	jr z, .skip_colu
	pop af
	jr .skip_up
.skip_colu
	pop af
	; End: Collision detection (UP)
	GetSpriteYAddr	player1
	dec	A
	PutSpriteYAddr	player1, a
	PutSpriteYAddr	player2, a
	jp .skip_right
.skip_up
	pop	af	; restore register A (joypad info)
	push	af	; save (again) reg. A
	and	PADF_DOWN
	jr	z, .skip_down
	ld	e, 1

	; Begin: Collision detection (DOWN)
	push af
	push hl
	call GetTile
	add a, $40
	jr nc, .nocarry
	inc h
.nocarry
	ld l, a ; Place lower byte of tile address into l
	ld	a, [hl]
	or a ; Set zero flag
	pop hl
	jr z, .skip_cold
	pop af
	jr .skip_down
.skip_cold
	pop af
	; End: Collision detection (DOWN)

	GetSpriteYAddr	player1
	inc	A
	PutSpriteYAddr	player1, a
	PutSpriteYAddr	player2, a
	jp .skip_right
.skip_down
	pop	af
	push	af
	and	PADF_LEFT
	jr	z, .skip_left
	ld	e, -1
	GetSpriteXAddr	player1
	; Begin: Collision detection (LEFT)
	push af
	push hl
	call GetTile
	sub a, 2
	jr nc, .nocarryl
	dec h
.nocarryl
	ld l, a ; Place lower byte of tile address into l
	ld	a, [hl]
	or a ; Set zero flag
	pop hl
	jr z, .skip_coll
	pop af
	jr .skip_left
.skip_coll
	pop af
	; End: Collision detection (LEFT)
	dec	A
	PutSpriteXAddr	player1, a
	add a, 8
	PutSpriteXAddr	player2, a
	jp .skip_right
.skip_left
	pop	af
	push	af
	and	PADF_RIGHT
	jr	z, .skip_right
	ld	e, 1

	GetSpriteXAddr	player1

	; Begin: Collision detection (RIGHT)
	push de
	push af
	push hl
	call GetTile
	add a, 2
	jr nc, .nocarryr
	inc h
.nocarryr
	ld l, a ; Place lower byte of tile address into l
	ld	d, h
	add	a, 2
	jr nc, .nocarry2r
	inc d
.nocarry2r
	ld e, a ; Now, DE contains the address of 2 tiles ahead
	ld	a, [hl]
	or a ; Set zero flag
	jr z, .skip_colr
	; Check whether the tile is a crate
	cp a, 4
	; If so, check tile after the crate
	jr nz, .not_crate
	ld	a, [de]
	or	a
	jr nz, .not_crate
	ld [hl], 0
	ld h, d
	ld l, e
	ld [hl], 4
	jr .skip_colr
.not_crate
	pop hl
	pop af
	pop de
	jr .skip_right
.skip_colr
	pop hl
	pop af
	; End: Collision detection (RIGHT)
	inc	A
	PutSpriteXAddr	player1, a
	add a, 8
	PutSpriteXAddr	player2, a
	pop de

.skip_right
	pop	af

	jp	.loop		; start up at top of .loop label. Repeats each vblank


jpad_GetKeys:
; Uses AF, B
; get currently pressed keys. Register A will hold keys in the following
; order: MSB --> LSB (Most Significant Bit --> Least Significant Bit)
; Down, Up, Left, Right, Start, Select, B, A
; This works by writing

	; get action buttons: A, B, Start / Select
	ld	a, JOYPAD_BUTTONS; choose bit that'll give us action button info
	ld	[rJOYPAD], a; write to joypad, telling it we'd like button info
	ld	a, [rJOYPAD]; gameboy will write (back in address) joypad info
	ld	a, [rJOYPAD]
	cpl		; take compliment
	and	$0f	; look at first 4 bits only  (lower nibble)
	swap	a	; place lower nibble into upper nibble
	ld	b, a	; store keys in b
	; get directional keys
	ld	a, JOYPAD_ARROWS
	ld	[rJOYPAD], a ; write to joypad, selecting direction keys
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]	; delay to reliablly read keys
	ld	a, [rJOYPAD]	; since we've just swapped from reading
	ld	a, [rJOYPAD]	; buttons to arrow keys
	ld	a, [rJOYPAD]
	cpl			; take compliment
	and	$0f		; keep lower nibble
	or	b		; combine action & direction keys (result in a)
	ld	b, a

	ld	a, JOYPAD_BUTTONS | JOYPAD_ARROWS
	ld	[rJOYPAD], a		; reset joypad

	ld	a, b	; register A holds result. Each bit represents a key
	ret

GetTile:
	; _SCRN + (y-$10) * 4 + (x - $8) / 8
	GetSpriteXAddr	player1
	srl a
	srl a
	srl a
	ld	l, a ; Using l as temporary storage
	GetSpriteYAddr	player1
	ld	h, $98
	sla a
	jr nc, .cont1u
	inc h
	sla a
	inc h
	jr .cont2u
.cont1u
	sla a
	jr nc, .cont2u
	inc h
.cont2u
	add a, l ; TODO: Need to investigate the probability of having carry
	ret

LOAD_TILES::
	ld	hl,TILES_DATA
	ld	de,_VRAM + 16 ; Skip the first 16 bytes to preserve the blank tile
	ld	bc,7*16
LOAD_TILES_LOOP::
	ld	a,[hl+]	;get a byte from our tiles, and increment.
	ld	[de],a	;put that byte in VRAM and
	inc	de		;increment.
	dec	bc		;bc=bc-1.
	ld	a,b		;if b or c != 0,
	or	c		;
	jr	nz,LOAD_TILES_LOOP	;then loop.
	ret			;done

LOAD_MAP::
	ld	hl,LEVEL_1	;our little map
	ld	de,_SCRN0	;where our map goes
	ld	b,18
	ld	c,$14
LOAD_MAP_LOOP::
	ld	a,[hl+]	;get a byte of the map and inc hl
	ld	[de],a	;put the byte at de
	inc	de		;duh...
	dec	c		;decrement our counter
	jr	nz,LOAD_MAP_LOOP	;and of the counter != 0 then loop
	; Add $0D to de
	ld a, e
	add a, $0C ; $20 - $13
	ld e, a
	jr nc, .TERUS
	inc d
.TERUS
	dec b
	jr z, .HABIS
	ld c, $14
	jr LOAD_MAP_LOOP
.HABIS
	ret			;done

 SECTION "Tiles", ROM0

; Start of tile array.
TILES_DATA::
DB $FF,$00,$02,$FD,$02,$FD,$02,$FD
DB $FF,$00,$20,$DF,$20,$DF,$20,$DF

DB $0F,$00,$1F,$00,$1F,$00,$1F,$00
DB $1F,$00,$1F,$06,$1F,$66,$1F,$E0
DB $1F,$E0,$0F,$F0,$00,$DF,$00,$DF
DB $00,$DF,$00,$1F,$00,$1C,$00,$1C

DB $7F,$00,$FF,$00,$F0,$0F,$F8,$07
DB $DC,$23,$CE,$31,$C7,$38,$C3,$3C
DB $C3,$3C,$C7,$38,$CE,$31,$DC,$23
DB $F8,$07,$F0,$0F,$FF,$00,$7F,$00
DB $FE,$00,$FF,$00,$0F,$F0,$1F,$E0
DB $3B,$C4,$73,$8C,$E3,$1C,$C3,$3C
DB $C3,$3C,$E3,$1C,$73,$8C,$3B,$C4
DB $1F,$E0,$0F,$F0,$FF,$00,$FE,$00

SECTION "Map", ROM0

LEVEL_1::
; DB $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
; DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
; DB $00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00
; DB $00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00
; DB $00,$00,$01,$01,$00,$00,$00,$00,$04,$06,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
; DB $00,$00,$01,$01,$00,$00,$00,$00,$05,$07,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
; DB $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DB $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$04,$06,$00,$00,$00,$00,$00,$00
DB $00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$05,$07,$00,$00,$00,$00,$00,$00
DB $00,$00,$01,$01,$00,$00,$00,$00,$04,$06,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$01,$01,$00,$00,$00,$00,$05,$07,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$04,$06,$01,$01,$00,$00
DB $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$05,$07,$01,$01,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00