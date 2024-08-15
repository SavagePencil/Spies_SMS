.IFNDEF __SPRITEMANAGER_ASM__
.DEFINE __SPRITEMANAGER_ASM__

.SECTION "Sprite Manager" FREE

.INCLUDE "Utils/spritechain.asm"

.STRUCT sSpriteManager
    pChainHead          DW      ; What's the first chain?     
.ENDST

;==============================================================================
; SpriteManager_Init
; Preps the Sprite manager
; INPUTS:  HL: Sprite Chain Header
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
SpriteManager_Init:
    ld      (gSpriteManager.pChainHead), hl
    ret

.ENDS

.SECTION "Sprite Manager - Clear" FREE
;==============================================================================
; SpriteManager_Clear
; Clears all chains, starting from the head.
; INPUTS:  None
; OUTPUTS:  None
; Destroys HL, IX
;==============================================================================
SpriteManager_Clear:
    ld      hl, (gSpriteManager.pChainHead)
-:
    ld      a, l
    or      h
    ret     z           ; If NULL, we're done.

    push    hl
    pop     ix

    call    SpriteChain_Clear

    ld      l, (ix + sSpriteChainHeader.NextChain + 0)
    ld      h, (ix + sSpriteChainHeader.NextChain + 1)
    jp      -
.ENDS

.SECTION "Sprite Manager - Upload to VRAM" FREE
;==============================================================================
; SpriteManager_UploadToVRAM
; Uploads all chains, starting from the head.
; INPUTS:  None
; OUTPUTS:  D: #/sprites remaining
; Destroys A, B, C, D, H, L, IX
;==============================================================================
SpriteManager_UploadToVRAM:
    ld      hl, (gSpriteManager.pChainHead)
    call    SpriteChain_RenderChainSequence
    ret
.ENDS

.RAMSECTION "Sprite Manager Instance" SLOT 3
    gSpriteManager INSTANCEOF sSpriteManager
.ENDS


.ENDIF  ;__SPRITEMANAGER_ASM__