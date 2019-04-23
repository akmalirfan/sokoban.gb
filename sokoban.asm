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

	SpriteAttr	copyright	; declare "copyright" as a sprite
	SpriteAttr	copyright2

code_begins:
	di	; disable interrupts
	ld	SP, $FFFF	; set stack to top of HRAM

	dma_Copy2HRAM	; sets up routine from dma.inc that updates sprites

	ld	a, IEF_VBLANK	; --
	ld	[rIE], a	; Set only Vblank interrupt flag
	ei			; enable interrupts. Only vblank will trigger

	ld	a, [rLCDC]	; fetch LCD Config. (Each bit is a flag)
	or	LCDCF_OBJON	; enable sprites through "OBJects ON" flag
	or	LCDCF_OBJ16	; enable 8bit wide sprites (vs. 16-bit wide)
	ld	[rLCDC], a	; save LCD Config. Sprites are now visible.


	; see where we declare "copyright" as a sprite-variable above
	; set X=20, Y=10, Tile=$19, Flags=0
	PutSpriteXAddr	copyright, 8
	PutSpriteYAddr	copyright, 8
	sprite_PutTile	copyright, $19
	sprite_PutFlags	copyright, $00

	PutSpriteXAddr	copyright2, 16
	PutSpriteYAddr	copyright2, 8
	sprite_PutTile	copyright2, $17
	sprite_PutFlags	copyright2, $00

	ld e, $16
.loop
	halt	; halts cpu until interrupt triggers (vblank)
 	; by halting, we ensure that .loop only runs only each screen-refresh,
	; so only 60fps. That makes the sprite movement here manageable
	nop

	; Check whether the y-coordinate % 8 is 0
	GetSpriteYAddr	copyright
	; ld	d, a
	push af
	and $7
	jr z, .pop_af
	; ld	a, d
	pop af
	add	a, e
	PutSpriteYAddr	copyright, a
	PutSpriteYAddr	copyright2, a
	jp .check_hor

.pop_af
	pop af
.check_hor
	; Check whether the x-coordinate % 8 is 0
	GetSpriteXAddr	copyright
	ld	d, a
	and $7
	jr z, .cont
	ld	a, d
	add	a, e
	PutSpriteXAddr	copyright, a
	add a, 8
	PutSpriteXAddr	copyright2, a

; From here, should check the current direction and stop the player
; from moving if there's something blocking

; Or maybe can check when the start pushing any direction.
; This way, don't have to check all directions

.cont
	call	jpad_GetKeys

	; move character if corresponding button has been pushed
	push	af	; save register A (joypad info)
	and	PADF_UP	; compare joypad info. Set NZ flag if UP bit present
	jr	z, .skip_up
	ld	e, -1
	; Begin: Collision detection (UP)
	; _SCRN + (y-$10) * 4 + (x - $8) / 8
	push af
	push hl
	push de
	GetSpriteXAddr	copyright
	; sub a, $8 ; No need because GetSpriteXAddr took care of that
	srl a
	srl a
	srl a
	ld	h, a ; Using h as temporary storage
	GetSpriteYAddr	copyright
	ld	d, $98
	sla a
	jr nc, .cont1u
	inc d
	sla a
	inc d
	jr .cont2u
.cont1u
	sla a
	jr nc, .cont2u
	inc d
.cont2u
	add a, h ; TODO: Need to investigate the probability of having carry
	sub a, $20
	ld l, a ; Place lower byte of tile address into l
	ld h, d ; Place upper byte of tile address into h
	ld	a, [hl]
	or a ; Set zero flag
	pop de
	pop hl
	jr z, .skip_colu
	pop af
	jr .skip_up
.skip_colu
	pop af
	; End: Collision detection (UP)
	GetSpriteYAddr	copyright
	dec	A
	PutSpriteYAddr	copyright, a
	PutSpriteYAddr	copyright2, a
	jp .skip_right
.skip_up
	pop	af	; restore register A (joypad info)
	push	af	; save (again) reg. A
	and	PADF_DOWN
	jr	z, .skip_down
	ld	e, 1

	; Begin: Collision detection (DOWN)
	; _SCRN + (y-$10) * 4 + (x - $8) / 8
	push af
	push hl
	push de
	GetSpriteXAddr	copyright
	; sub a, $8 ; No need because GetSpriteXAddr took care of that
	srl a
	srl a
	srl a
	ld	h, a ; Using h as temporary storage
	GetSpriteYAddr	copyright
	; sub a, $10 ; No need because GetSpriteYAddr took care of that
	add a, $10
	ld	d, $98
	sla a
	jr nc, .cont1d
	inc d
	sla a
	inc d
	jr .cont2d
.cont1d
	sla a
	jr nc, .cont2d
	inc d
.cont2d
	add a, h ; TODO: Need to investigate the probability of having carry
	ld l, a ; Place lower byte of tile address into l
	ld h, d ; Place upper byte of tile address into h
	ld	a, [hl]
	or a ; Set zero flag
	pop de
	pop hl
	jr z, .skip_cold
	pop af
	jr .skip_down
.skip_cold
	pop af
	; End: Collision detection (DOWN)

	GetSpriteYAddr	copyright
	inc	A
	PutSpriteYAddr	copyright, a
	PutSpriteYAddr	copyright2, a
	jp .skip_right
.skip_down
	pop	af
	push	af
	and	PADF_LEFT
	jr	z, .skip_left
	ld	e, -1
	GetSpriteXAddr	copyright
	; Begin: Collision detection (LEFT)
	; _SCRN + (y-$10) * 4 + (x - $8) / 8
	push af
	push hl
	push de
	; sub a, $8 ; No need because GetSpriteXAddr took care of that
	sub a, $8 ; Because we want to know the tile left to the player
	srl a
	srl a
	srl a
	ld	h, a ; Using h as temporary storage
	GetSpriteYAddr	copyright
	; sub a, $10 ; No need because GetSpriteYAddr took care of that
	ld	d, $98
	sla a
	jr nc, .cont1l
	inc d
	sla a
	inc d
	jr .cont2l
.cont1l
	sla a
	jr nc, .cont2l
	inc d
.cont2l
	add a, h ; TODO: Need to investigate the probability of having carry
	ld l, a ; Place lower byte of tile address into l
	ld h, d ; Place upper byte of tile address into h
	ld	a, [hl]
	or a ; Set zero flag
	pop de
	pop hl
	jr z, .skip_coll
	pop af
	jr .skip_left
.skip_coll
	pop af
	; End: Collision detection (LEFT)
	dec	A
	PutSpriteXAddr	copyright, a
	add a, 8
	PutSpriteXAddr	copyright2, a
	sprite_PutFlags	copyright, %00100000
	jp .skip_right
.skip_left
	pop	af
	push	af
	and	PADF_RIGHT
	jr	z, .skip_right
	ld	e, 1

	GetSpriteXAddr	copyright

	; Begin: Collision detection (RIGHT)
	; _SCRN + (y-$10) * 4 + (x - $8) / 8
	push af
	push hl
	push de
	; sub a, $8 ; No need because GetSpriteXAddr took care of that
	add a, $8 ; Because we want to know the tile right to the player
	srl a
	srl a
	srl a
	ld	h, a ; Using h as temporary storage
	GetSpriteYAddr	copyright
	; sub a, $10 ; No need because GetSpriteYAddr took care of that
	ld	d, $98
	sla a
	jr nc, .cont1
	inc d
	sla a
	inc d
	jr .cont2
.cont1
	sla a
	jr nc, .cont2
	inc d
.cont2
	add a, h ; TODO: Need to investigate the probability of having carry
	ld l, a ; Place lower byte of tile address into l
	ld h, d ; Place upper byte of tile address into h
	ld	a, [hl]
	or a ; Set zero flag
	pop de
	pop hl
	jr z, .skip_colr
	pop af
	jr .skip_right
.skip_colr
	pop af
	; End: Collision detection (RIGHT)
	inc	A
	PutSpriteXAddr	copyright, a
	add a, 8
	PutSpriteXAddr	copyright2, a
	sprite_PutFlags	copyright, $00

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