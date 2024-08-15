.IFNDEF __GAME_GFX_ASM__
.DEFINE __GAME_GFX_ASM__

.SECTION "Empty Graphic Data 1bpp" FREE
EmptyGraphic_1bpp_Data:
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@End:    
.ENDS

.SECTION "Empty Graphic Data 4bpp" FREE
EmptyGraphic_4bpp_Data:
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@End:    
.ENDS


.SECTION "Spy Graphic Data 1bpp" FREE
SpyGraphic_1bpp_Data:
; Spy is 2 tiles x 2 tiles
@UL:
    .DB %00000111
    .DB %00011111
    .DB %00001111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
@@End:
@LL:
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00000111
    .DB %00000111
    .DB %00001111
@@End:
@UR:
    .DB %11100000
    .DB %11111000
    .DB %11110000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
@@End:
@LR:
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11100000
    .DB %11100000
    .DB %11110000
@@End:
@End:

.ENDS

.SECTION "Spy Graphic Data 4bpp" FREE
SpyGraphic_4bpp_Data:
; Spy is 2 tiles x 2 tiles
@UL:
    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00001111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@LL:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00001111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@UR:
    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11110000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:

@LR:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11110000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@End:

.ENDS

.SECTION "Spy Graphic & Mask Interleaved" FREE
; This is an optimized arrangement for compositing the spy as BG tiles
; (see functions like Tile_CompositeInterleavedTileData_ToVRAM_VRAMPTRSet).
; The data is laid out as follows:
; Byte  Value
; ====  =====
;  +0   Inverted mask for row 1
;  +1   Byte for gfx row 1, plane 0
;  +2   Byte for gfx row 1, plane 1
;  +3   Byte for gfx row 1, plane 2
;  +4   Byte for gfx row 1, plane 3
;  +5   Inverted mask for row 2
;   .   .
;   .   .
;   .   .

SpyMaskAndGraphic_Data:
; Spy is 2 tiles x 2 tiles
@UL:
@@Row1:
@@@Mask:
    .db %11111000
@@@Gfx:
    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row2:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row3:
@@@Mask:
    .DB %11110000
@@@Gfx:
    .DB %00001111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row4:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row5:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row6:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row7:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@Row8:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:

@LL:
@@Row1:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row2:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row3:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row4:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row5:
@@@Mask:
    .DB %11100000
@@@Gfx:
    .DB %00011111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row6:
@@@Mask:
    .DB %11111000
@@@Gfx:
    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row7:
@@@Mask:
    .DB %11111000
@@@Gfx:
    .DB %00000111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row8:
@@@Mask:
    .DB %11110000
@@@Gfx:
    .DB %00001111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@UR:
@@Row1:
@@@Mask:
    .DB %00011111
@@@Gfx:
    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row2:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row3:
@@@Mask:
    .DB %00001111
@@@Gfx:
    .DB %11110000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row4:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row5:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row6:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row7:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row8:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:

@LR:
@@Row1:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row2:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row3:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row4:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row5:
@@@Mask:
    .DB %00000111
@@@Gfx:
    .DB %11111000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row6:
@@@Mask:
    .DB %00011111
@@@Gfx:
    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row7:
@@@Mask:
    .DB %00011111
@@@Gfx:
    .DB %11100000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@Row8:
@@@Mask:
    .DB %00001111
@@@Gfx:
    .DB %11110000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@End:
@End:
.ENDS

.SECTION "Elevator Graphic Data 1bpp" FREE

ElevatorGraphic_1bpp_Data:
@Left:
    .DB %00000011
    .DB %00000011
    .DB %00000011
    .DB %00000011
    .DB %00000011
    .DB %00000011
    .DB %00000011
    .DB %00000011
@@End:
@Right:
    .DB %11000000
    .DB %11000000
    .DB %11000000
    .DB %11000000
    .DB %11000000
    .DB %11000000
    .DB %11000000
    .DB %11000000
@@End:
@End:
.ENDS

.SECTION "Elevator Graphic Data 4bpp" FREE

ElevatorGraphic_4bpp_Data:
@Left:
    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000011
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@End:
@Right:
    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:

@End:
.ENDS


.SECTION "Floor Graphic Data 1bpp" FREE

FloorGraphic_1bpp_Data:
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %11111111
@End:

.ENDS

.SECTION "Floor Graphic Data 4bpp" FREE

FloorGraphic_4bpp_Data:
    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111111
    .DB %00000000
    .DB %00000000
    .DB %00000000

@End:

.ENDS

.SECTION "Edge Graphic Data 1bpp" FREE

EdgeGraphic_1bpp_Data:
@Left:
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
@@End:
@Right:
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
@@End:
@Left_Floor:
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %11111111
@@End:
@Right_Floor:
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %00000001
    .DB %11111111
@@End:
.ENDS

.SECTION "Edge Graphic Data 4bpp" FREE

EdgeGraphic_4bpp_Data:
@Left:
    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

@@End:
@Right:
    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@Left_Floor:
    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %10000000
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
@Right_Floor:
    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %00000001
    .DB %00000000
    .DB %00000000
    .DB %00000000

    .DB %11111111
    .DB %00000000
    .DB %00000000
    .DB %00000000
@@End:
.ENDS


.ENDIF  ; __GAME_GFX_ASM__