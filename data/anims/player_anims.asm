.IFNDEF __PLAYER_ANIMS_ASM__
.DEFINE __PLAYER_ANIMS_ASM__

.INCLUDE "modules/animation_module.asm"

.SECTION "Player Animations" FREE

PlayerAnims:
@IdleAnim:
.DSTRUCT @@IdleDef INSTANCEOF sAnimationDefinition VALUES
    Behavior            .DB ANIMATION_BEHAVIOR_PING_PONG
    NumFrames           .DB 3
    NumTilesPerFrame    .DB 4
.ENDST
; FRAME 0
.DSTRUCT @@IdleFrame0 INSTANCEOF sAnimationFrame VALUES
    FrameDuration       .DB 60
.ENDST
@@@IdleFrame0_Tiles:
    .dw 'p', 'l', 'I', '0'
@@@Done:
; FRAME 1
.DSTRUCT @@IdleFrame1 INSTANCEOF sAnimationFrame VALUES
    FrameDuration       .DB 60
.ENDST
@@@IdleFrame0_Tiles:
    .dw 'p', 'l', 'I', '1'
@@@Done:
; FRAME 2
.DSTRUCT @@IdleFrame2 INSTANCEOF sAnimationFrame VALUES
    FrameDuration       .DB 60
.ENDST
@@@IdleFrame0_Tiles:
    .dw 'p', 'l', 'I', '2'
@@@Done:

.ENDS

.ENDIF  ;__PLAYER_ANIMS_ASM__