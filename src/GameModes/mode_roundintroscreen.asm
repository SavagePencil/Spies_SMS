.INCLUDE "../data/fonts/default_font.asm"
.INCLUDE "Utils/fsm.asm"
.INCLUDE "Utils/tile_routines.asm"

.SECTION "Mode - Round Intro Screen" FREE

.STRUCT sRoundIntroScreen
    ; FSM governing the screen's state transitions.
    ScreenFSM INSTANCEOF  sFSM

    ; Independent variables unique to each state
    .UNION WaitState
        WaitTimer       DB      ; How long to wait
    .ENDU
.ENDST

; This screen has additional functions for its FSM.
.STRUCT sModeRoundIntroScreen_State
    ScreenState INSTANCEOF sState
    OnRenderPrep           DW
.ENDST

; Public
ModeRoundIntroScreen:

.DSTRUCT @RoundIntroScreenAppMode INSTANCEOF sApplicationMode VALUES:
    VideoInterruptJumpTarget    .DW _ModeRoundIntro@InterruptHandler    ; Called when a video interrupt (V/HBlank) occurs.
    OnActive                    .DW _ModeRoundIntro@OnActive            ; Called when this mode is made active (pushed, old one above popped, etc.)
    OnUpdate                    .DW _ModeRoundIntro@OnUpdate            ; Called when the application wants to update.

    OnRenderPrep                .DW _ModeRoundIntro@DoNothing           ; Called when the application is prepping things for render.
    OnNMI                       .DW _ModeRoundIntro@DoNothing           ; Called when a non-maskable interrupt (NMI) comes in.
    OnInactive                  .DW _ModeRoundIntro@DoNothing           ; Called when this mode goes inactive (popped, new mode pushed on, etc.)
    OnEvent                     .DW _ModeRoundIntro@DoNothing           ; Called when a generic event occurs.
.ENDST

; Private
_ModeRoundIntro:
@DoNothing:
    ret

@OnActive:
    ; If we're replacing what was on before us, do the full init.  Otherwise don't do anything.
    cp      MODE_MADE_ACTIVE
    jr      z, @@FullInit

    ret

@@FullInit:
    ; Turn off the display and interrupts while we do graphics things.

    di
    ; Turn off the display & VBlanks by OR'ing to the current value.
    ld      a, (gVDPManager.Registers.VideoModeControl2)
    and     $FF ~(VDP_REGISTER1_ENABLE_DISPLAY | VDP_REGISTER1_ENABLE_VBLANK)
    ld      e, VDP_COMMMAND_MASK_REGISTER1
    call    VDPManager_WriteRegisterImmediate

    ; Clear the nametable
    call ClearNameTable

    ; Upload the font.
    ld      hl, $0020   ; Dest tile index
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL
    ld      hl, DefaultFont_1bpp_DataBegin                  ; Src data
    ld      bc, DefaultFont_1bpp_DataEnd - DefaultFont_1bpp_DataBegin ; Length of data
    ld      e, $00                                          ; Palette entry for 0s in 1bpp data
    ld      d, $01                                          ; Palette entry for 1s in 1bpp data
    call    Tile_Upload1BPPWithPaletteRemaps_VRAMPtrSet

    ; Upload the palette
    ld      b, (@Palette@End - @Palette) >> 1           ; #/entries (2 bytes per entry)
    ld      hl, @Palette
-:
    ld      e, (hl)     ; Get entry
    inc     hl
    ld      c, (hl)     ; Get color value
    inc     hl
    push    hl
    call    VDPManager_SetPaletteEntryImmediate
    pop     hl
    djnz    -

    ; Start our state machine for this screen.
    ld      ix, gModeRoundIntroScreen.ScreenFSM
    ld      hl, ModeRoundIntroScreen_WaitState
    call    FSM_IX@Init

    ; We're ready to roll.  Turn on interrupts and the screen.
    ; Turn on the display, by OR'ing to the current value.
    ld      a, (gVDPManager.Registers.VideoModeControl2)
    or      VDP_REGISTER1_ENABLE_DISPLAY | VDP_REGISTER1_ENABLE_VBLANK
    ld      e, VDP_COMMMAND_MASK_REGISTER1
    call    VDPManager_WriteRegisterImmediate
    ei

    ret

@OnUpdate:
    ; Update our current state
    ld      ix, gModeRoundIntroScreen.ScreenFSM
    call    FSM_IX@OnUpdate
    ret

@InterruptHandler:
    PUSH_ALL_REGS
        in  a, (VDP_STATUS_PORT)                ; Satisfy the interrupt
        ld  ix, gModeRoundIntroScreen.ScreenFSM
        ld  de, sModeRoundIntroScreen_State.OnRenderPrep  ; Offset for our custom function
        call FSM_IX@OnEvent                        ; Call custom function
    POP_ALL_REGS
    ret

@Palette:
; BG Palette Entry 0 == color 0 (black)
.db VDP_PALETTE_BG_PALETTE_INDEX + 0, $00
; BG Palette Entry 1 == color $3F (white)
.db VDP_PALETTE_BG_PALETTE_INDEX + 1, (3 << VDP_PALETTE_RED_SHIFT) | (3 << VDP_PALETTE_GREEN_SHIFT) | (3 << VDP_PALETTE_BLUE_SHIFT)
@@End:

.ENDS

.RAMSECTION "Mode - Round Intro Screen Context" SLOT 3
    gModeRoundIntroScreen INSTANCEOF sRoundIntroScreen
.ENDS

.SECTION "Mode - Round Intro Wait State" FREE
.DEFINE ROUND_INTRO_WAIT_TIME   120

.DSTRUCT ModeRoundIntroScreen_WaitState INSTANCEOF sModeRoundIntroScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _WaitState@OnEnter
    ScreenState.OnUpdate        .DW _WaitState@OnUpdate

    ; Custom handlers
    OnRenderPrep                .DW _WaitState@OnRenderPrep
.ENDST

;******************************************************************************
; Wait State:  Displays the player and round # for a period of time
;******************************************************************************
_WaitState:

@OnEnter:
    ; Write the player's name to the screen.
    ld      hl, @Player1Str
    ld      b, @Player1Str@End - @Player1Str
    ld      a, (gGameManager.CurrPlayer)
    dec     a       ; Are we on player 1?
    jr      z, @@PrintPlayerNameStr

    ; Else, we're player 2.
    ld      hl, @Player2Str
    ld      b, @Player2Str@End - @Player2Str

@@PrintPlayerNameStr:
    ; Position should be center - len / 2.
    ld      a, b    ; Get str len
    rrca            ; Str len / 2
    ld      e, a
    ld      a, 16   ; Screen middle
    sub     e
    ld      e, a    ; E = middle - len / 2
    ld      d, 8   ; Row
    
    ld      c, $00  ; Common attributes

    call    VDP_UploadStringToNameTable
 
    ; Write the round number as a string.  We'll start with the static string "ROUND" first.
    ld      hl, @RoundStr
    ld      b, @RoundStr@End - @RoundStr
    ld      a, b
    add     a, 2    ; + space + round #
    rrca
    ld      e, a
    ld      a, 16
    sub     e
    ld      e, a    ; E = middle - len / 2
    ld      d, 11   ; Row

    ld      c, $00  ; Common attributes

    push    de      ; Save the loc.
        call VDP_UploadStringToNameTable
    pop     de      ; We want that column position!

    ; Now get the dynamic round number.
    call    GameManager_GetCurrPlayerStats  ; IY points to curr player's stats.
    ld      a, (iy + sPlayerStats.Level)
    add     a, '1'  ; A now has the ASCII char for the curr round.
    ; TODO WRITE THIS

@SetWaitParams:
    ld      a, ROUND_INTRO_WAIT_TIME
    ld      (gModeRoundIntroScreen.WaitState.WaitTimer), a

    and     a   ; Stay in state
    ret

@Player1Str:
    .db "PLAYER ONE"
@@End:

@Player2Str:
    .db "PLAYER TWO"
@@End:

@RoundStr:
    .db "ROUND"
@@End:

@OnUpdate:
    dec     (ix + sRoundIntroScreen.WaitState.WaitTimer)
    jr      nz, @@StayInSameState

    ; Move to the next mode.
    ld      de, ModeGameScreen@GameScreenAppMode
    call    ModeManager_SetMode

@@StayInSameState:
    and     a
    ret

@OnRenderPrep:
    ; Don't do anything; we already wrote the characters during init.
    and     a
    ret

.ENDS
