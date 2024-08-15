.INCLUDE "../data/fonts/default_font.asm"
.INCLUDE "Utils/fsm.asm"
.INCLUDE "Utils/tile_routines.asm"
.INCLUDE "Modules/debounce_module.asm"

.SECTION "Mode - Title Screen" FREE

.STRUCT sTitleScreen
    ; FSM governing the screen's state transitions.
    ScreenFSM INSTANCEOF  sFSM

    ; Module used to ensure the player releases buttons before we accept input from them.
    Controller1DebounceModule INSTANCEOF sDebounceModule_Instance

    ; Independent variables unique to each state.
    .UNION FadeUp
        TimingCounter   DB      ; Current counter value
        CharsRemaining  DB      ; How many characters are left?
    .NEXTU AwaitInput
        BlinkTimer      DB      ; How long before we blink?
        IsOn            DB      ; Are we currently displayed, or no?
        ButtonReleased  DB      ; Have we seen a button released yet? 
    .ENDU    
.ENDST

; This screen has additional functions for its FSM.
.STRUCT sModeTitleScreen_State
    ScreenState INSTANCEOF sState
    OnRenderPrep           DW
.ENDST

; Public:
ModeTitleScreen:

.DSTRUCT @TitleScreenAppMode INSTANCEOF sApplicationMode VALUES:
    VideoInterruptJumpTarget    .DW _ModeTitleScreen@InterruptHandler   ; Called when a video interrupt (V/HBlank) occurs.
    OnActive                    .DW _ModeTitleScreen@OnActive           ; Called when this mode is made active (pushed, old one above popped, etc.)
    OnUpdate                    .DW _ModeTitleScreen@OnUpdate           ; Called when the application wants to update.

    OnRenderPrep                .DW _ModeTitleScreen@DoNothing          ; Called when the application is prepping things for render.
    OnNMI                       .DW _ModeTitleScreen@DoNothing          ; Called when a non-maskable interrupt (NMI) comes in.
    OnInactive                  .DW _ModeTitleScreen@DoNothing          ; Called when this mode goes inactive (popped, new mode pushed on, etc.)
    OnEvent                     .DW _ModeTitleScreen@DoNothing          ; Called when a generic event occurs.
.ENDST

; The title screen waits for the player to release all buttons before accepting input.
.DSTRUCT @TitleScreen_DebounceParams INSTANCEOF sDebounceModule_Parameters VALUES
    DesiredVal  .DB CONTROLLER_JOYPAD_UP_RELEASED | CONTROLLER_JOYPAD_DOWN_RELEASED | CONTROLLER_JOYPAD_LEFT_RELEASED | CONTROLLER_JOYPAD_RIGHT_RELEASED | CONTROLLER_JOYPAD_BUTTON1_RELEASED | CONTROLLER_JOYPAD_BUTTON2_RELEASED
    Mask        .DB CONTROLLER_JOYPAD_UP_RELEASED | CONTROLLER_JOYPAD_DOWN_RELEASED | CONTROLLER_JOYPAD_LEFT_RELEASED | CONTROLLER_JOYPAD_RIGHT_RELEASED | CONTROLLER_JOYPAD_BUTTON1_RELEASED | CONTROLLER_JOYPAD_BUTTON2_RELEASED
.ENDST


; B = #/Chars to print.
@PrintTitle:
    ; Position should be center - len / 2.
    ld      e, 16 - ( ( @TitleString@End - @TitleString ) / 2 )   ; Col
    ld      d, 8    ; Row

    ld      c, $00  ; Common attributes

    ld      hl, @TitleString
    call    VDP_UploadStringToNameTable

    ret

; A != 0:  Print "Press Start", otherwise erase it.
@PrintPressStart:
    ; Position should be center - len / 2.
    ld      e, 16 - ( ( @PressStartString@End - @PressStartString ) / 2 )   ; Col
    ld      d, 20    ; Row
    
    ld      b, @PressStartString@End - @PressStartString
    ld      c, $00  ; Common attributes

    ld      hl, @PressStartString
    and     a
    jr      nz, @PrintString

    ld      hl, @PressStartStringEmpty

@PrintString:
    call    VDP_UploadStringToNameTable

    ret

@TitleString:
.DB "SPY'S DEMISE"
@@End:

@PressStartString:
.DB "PRESS START"
@@End:

@PressStartStringEmpty:
.DB "           "
@@End:

; Private:
_ModeTitleScreen:

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

    ; Default us to both joypads active.
    xor     a                               ; 0 == Port 1
    ld      b, CONTROLLER_TYPE_SMS_JOYPAD
    ld      hl, Controller_Joypad_Port1_State
    call    InputManager_SetController

    ld      a, 1                            ; 1 == Port 2
    ld      b, CONTROLLER_TYPE_SMS_JOYPAD
    ld      hl, Controller_Joypad_Port2_State
    call    InputManager_SetController

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
    ld      ix, gModeTitleScreen.ScreenFSM
    ld      hl, ModeTitleScreen_FadeUpState
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
    ; Read input.
    call    InputManager_OnUpdate

    ; Update our current state
    ld      ix, gModeTitleScreen.ScreenFSM
    call    FSM_IX@OnUpdate
    ret

@InterruptHandler:
    PUSH_ALL_REGS
        in  a, (VDP_STATUS_PORT)                ; Satisfy the interrupt
        ld  ix, gModeTitleScreen.ScreenFSM
        ld  de, sModeTitleScreen_State.OnRenderPrep ; Offset for our custom function
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

.RAMSECTION "Mode - Title Screen Context" SLOT 3
    gModeTitleScreen INSTANCEOF sTitleScreen
.ENDS


.SECTION "Mode - Title Screen FadeUp State" FREE

.DEFINE TITLESCREEN_CHAR_RATE           15      ; #/frames for each character
.DEFINE TITLESCREEN_PRESS_START_TIMER   20      ; #/frames before blinking "PRESS START"

.DSTRUCT ModeTitleScreen_FadeUpState INSTANCEOF sModeTitleScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _FadeUpState@OnEnter
    ScreenState.OnUpdate        .DW _FadeUpState@OnUpdate

    ; Custom handlers
    OnRenderPrep                .DW _FadeUpState@OnRenderPrep
.ENDST

; Private:
_FadeUpState:

@OnEnter:
    ; Set frames between chars appearing
    ld  hl, gModeTitleScreen.FadeUp.TimingCounter
    ld  (hl), TITLESCREEN_CHAR_RATE

    ; Set #/chars remaining
    ld  hl, gModeTitleScreen.FadeUp.CharsRemaining
    ld  (hl), ModeTitleScreen@TitleString@End - ModeTitleScreen@TitleString + 1

    ; No transition
    and a
    ret

@OnUpdate:
    ; Count down the current timer.
    ld  hl, gModeTitleScreen.FadeUp.TimingCounter
    dec (hl)
    jr  nz, @@NoTransition

    ; Update to next char, unless *THAT'S* done, too!
    ld  hl, gModeTitleScreen.FadeUp.CharsRemaining
    dec (hl)
    jr  z, @@NextState

    ; Still chars to come, so reset the timer between chars.
    ld  hl, gModeTitleScreen.FadeUp.TimingCounter
    ld  (hl), TITLESCREEN_CHAR_RATE
@@NoTransition:
    ; No transition
    and a
    ret

@@NextState:
    ; Transition to next state
    scf
    ld  hl, ModeTitleScreen_AwaitInputState
    ret

@OnRenderPrep:
    ; Write chars

    ; How many chars should we write?
    ld      a, ModeTitleScreen@TitleString@End - ModeTitleScreen@TitleString + 1
    ld      hl, gModeTitleScreen.FadeUp.CharsRemaining
    sub     (hl)
    ld      b, a

    ; Only call if chars to draw.  Stored in B.
    call    nz, ModeTitleScreen@PrintTitle

    and     a
    ret
.ENDS

.SECTION "Mode - Title Screen Await Input State" FREE

.DSTRUCT ModeTitleScreen_AwaitInputState INSTANCEOF sModeTitleScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _AwaitInputState@OnEnter
    ScreenState.OnUpdate        .DW _AwaitInputState@OnUpdate

    ; Custom handlers
    OnRenderPrep                .DW _AwaitInputState@OnRenderPrep
.ENDST

; Private:
_AwaitInputState:

@OnEnter:
    ; Init values local to this state.
    ld      hl, gModeTitleScreen.AwaitInput.BlinkTimer
    ld      (hl), TITLESCREEN_PRESS_START_TIMER

    ld      hl, gModeTitleScreen.AwaitInput.IsOn
    ld      (hl), $00

    ld      hl, gModeTitleScreen.AwaitInput.ButtonReleased
    ld      (hl), $00

    ; **** SETUP DEBOUNCE MODULE FOR CONTROLLER 1 ****
    ; Setup our debounce module to ensure the player releases buttons before we accept input.

    ; Set the params    
    ld      hl, ModeTitleScreen@TitleScreen_DebounceParams
    ld      (gModeTitleScreen.Controller1DebounceModule.DebounceParams), hl

    ; Get current val from Controller 1
    ld      a, (gInputManager.Controller1.Joypad.Data.CurrentButtons)
    ld      (gModeTitleScreen.Controller1DebounceModule.CurrentVal), a

    ; Init the FSM
    ld      hl, DebounceModule_WaitForDebounceState
    ld      ix, gModeTitleScreen.Controller1DebounceModule.DebounceFSM
    call    FSM_IX@Init

    and     a
    ret

@OnUpdate:
    ; Time to blink?
    ld      hl, gModeTitleScreen.AwaitInput.BlinkTimer
    dec     (hl)
    jr      nz, @@CheckInput

    ; We counted down, so blink.
    ld      hl, gModeTitleScreen.AwaitInput.BlinkTimer
    ld      (hl), TITLESCREEN_PRESS_START_TIMER

    ; Flip the visible bit
    ld      a, (gModeTitleScreen.AwaitInput.IsOn)
    xor     1
    ld      (gModeTitleScreen.AwaitInput.IsOn), a

@@CheckInput:
    ; Get current val from Controller 1
    ld      a, (gInputManager.Controller1.Joypad.Data.CurrentButtons)
    ld      (gModeTitleScreen.Controller1DebounceModule.CurrentVal), a

    ; Check to see if we've already debounced.
    ld      ix, gModeTitleScreen.Controller1DebounceModule.DebounceFSM
    call    FSM_IX@OnUpdate
    call    DebounceModule_IsDebounced
    jr      nz, @@StayInSameState    ; If NZ, that means we're not yet debounced.

    ; We're debounced.  See if either of our buttons were hit.
    ld      a, (gModeTitleScreen.Controller1DebounceModule.CurrentVal)
    ld      b, CONTROLLER_JOYPAD_BUTTON1_RELEASED | CONTROLLER_JOYPAD_BUTTON2_RELEASED
    and     b
    cp      b
    jr      z, @@StayInSameState      

    ; Move to next state
@@MoveToNextState:
    
    ld      hl, ModeTitleScreen_SetupGameState
    scf     ; Transition to next state
    ret

@@StayInSameState:
    and a
    ret


@OnRenderPrep:
    ld      a, (gModeTitleScreen.AwaitInput.IsOn)
    call    ModeTitleScreen@PrintPressStart
    and     a
    ret
.ENDS

.SECTION "Mode - Title Screen Setup Game State" FREE

.DSTRUCT ModeTitleScreen_SetupGameState INSTANCEOF sModeTitleScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _SetupGameState@OnEnter
    ScreenState.OnUpdate        .DW _SetupGameState@DoNothing

    ; Custom handlers
    OnRenderPrep                .DW _SetupGameState@DoNothing
.ENDST

_SetupGameState:

@OnEnter:
    ld      a, 1                ; #/players
    call    GameManager_Init

    ; Transition to the round intro mode.
    ld      de, ModeRoundIntroScreen@RoundIntroScreenAppMode
    call    ModeManager_SetMode

    and     a
    ret

@DoNothing:
    and     a   ; Stay in same state.
    ret


.ENDS
