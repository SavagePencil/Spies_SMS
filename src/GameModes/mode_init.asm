.include "Managers/modemanager.asm"
.include "Managers/vdpmanager.asm"
.include "Managers/inputmanager.asm"

.SECTION "Mode - Init" FREE

; Public
ModeInit:
.DSTRUCT @InitAppMode INSTANCEOF sApplicationMode VALUES:
    VideoInterruptJumpTarget    .DW DefaultVideoInterruptHandler    ; Called when a video interrupt (V/HBlank) occurs.
    OnActive                    .DW _ModeInit@OnActive              ; Called when this mode is made active (pushed, old one above popped, etc.)

    OnNMI                       .DW _ModeInit@DoNothing             ; Called when a non-maskable interrupt (NMI) comes in.
    OnInactive                  .DW _ModeInit@DoNothing             ; Called when this mode goes inactive (popped, new mode pushed on, etc.)
    OnUpdate                    .DW _ModeInit@DoNothing             ; Called when the application wants to update.
    OnRenderPrep                .DW _ModeInit@DoNothing             ; Called when the application is prepping things for render.
    OnEvent                     .DW _ModeInit@DoNothing             ; Called when a generic event occurs.
.ENDST

; Private
_ModeInit:

@DoNothing:
    ; Do nothing.
    ret

@OnActive:
    ; If we're being pushed on, stand everything up.

    ; Setup the VDP
    call    VDPManager_Init

    ; Setup the input manager
    call    InputManager_Init

    ; Let's go to the title screen.
    ld      de, ModeTitleScreen@TitleScreenAppMode
    call    ModeManager_SetMode

    ret

@OnRenderPrep:
    ret

.ENDS