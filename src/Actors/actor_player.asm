.IFNDEF __ACTOR_PLAYER_ASM__
.DEFINE __ACTOR_PLAYER_ASM__

.INCLUDE "Managers/actormanager.asm"
.INCLUDE "Utils/fsm.asm"
.INCLUDE "Utils/controller.asm"
.INCLUDE "Utils/Controllers/controller_joypad.asm"
.INCLUDE "Modules/animation_module.asm"
.INCLUDE "../data/anims/player_anims.asm"

.SECTION "Actor - Player" FREE

.ENUMID 'a'
.ENUMID ACTOR_PLAYER_SPRITE_TILE_INDEX_UL
.ENUMID ACTOR_PLAYER_SPRITE_TILE_INDEX_LL
.ENUMID ACTOR_PLAYER_SPRITE_TILE_INDEX_UR
.ENUMID ACTOR_PLAYER_SPRITE_TILE_INDEX_LR

.DEFINE ACTOR_PLAYER_MIN_X   0
.DEFINE ACTOR_PLAYER_MAX_X   176

.STRUCT sActorPlayer
    ActorInstance   INSTANCEOF sActorInstance

    ; Global variables for the player.
    XPos            DB
    YPos            DB
    Floor           DB

    ; Pointer to our joypad (ControllerData_Joypad) 
    pJoypadData     DW

    Animation       INSTANCEOF sAnimationInstance

    ; State-specific vars.
    ; TODO
.ENDST

; Parameters used when initializing the player
.STRUCT sActorPlayer_Init
    InitialXPos     DB
    InitialYPos     DB
    InitialFloor    DB
.ENDST

; Public
ActorPlayer:

.DSTRUCT @ActorPlayerClassMethods INSTANCEOF sActorClassMethods VALUES
    OnActorAdded   .DW _ActorPlayer@OnActorAdded
.ENDST

;==============================================================================
; ActorPlayer@Init
; Sets initial values for the actor
; INPUTS:  DE: Pointer to sActorPlayer memory
;          IX: Pointer to sActorPlayer_Init definition
; OUTPUTS:  None
; Destroys TODO
;==============================================================================
@Init:
    ; Get our actor into IY
    push    de
    pop     iy

    ; Setup our methods.
    ld      (iy + sActorPlayer.ActorInstance.pActorClassMethods + 0), <ActorPlayer@ActorPlayerClassMethods
    ld      (iy + sActorPlayer.ActorInstance.pActorClassMethods + 1), >ActorPlayer@ActorPlayerClassMethods

    ld      a, (ix + sActorPlayer_Init.InitialXPos)
    ld      (iy + sActorPlayer.XPos), a
    ld      a, (ix + sActorPlayer_Init.InitialYPos)
    ld      (iy + sActorPlayer.YPos), a
    ld      a, (ix + sActorPlayer_Init.InitialFloor)
    ld      (iy + sActorPlayer.Floor), a

    ; Setup our controller.
    ; TODO this should be passed in.
    ld      hl, gInputManager.Controller1.Joypad.Data
    ld      (iy + sActorPlayer.pJoypadData + 0), l
    ld      (iy + sActorPlayer.pJoypadData + 1), h

    ; Setup our state machine.
    ld      hl, _ActorPlayer@IdleState  ; Initial state
    call    FSM_DE@Init
    ret

; Private
_ActorPlayer:

.DSTRUCT @IdleState INSTANCEOF sActorState VALUES
    Base.OnEnter    .DW _ActorPlayer@Idle@OnEnter
    Base.OnUpdate   .DW _ActorPlayer@Idle@OnUpdate
    OnRenderPrep    .DW _ActorPlayer@OnRenderPrep
.ENDST

@Idle:
@@OnEnter:
    push    de
    pop     ix

    ; Prep the Idle animation routine.
    ld      bc, sActorPlayer.Animation
    add     ix, bc  ; IX points to the sAnimationInstance
    ld      de, PlayerAnims@IdleAnim@IdleDef

    call    AnimationInstance@Init

    and     a
    ret

@@OnUpdate:
    ; Get the actor into IY
    push    de
    pop     iy

    ; Check our controller.
    ld      l, (iy + sActorPlayer.pJoypadData + 0)
    ld      h, (iy + sActorPlayer.pJoypadData + 1)
    ld      de, sControllerData_Joypad.CurrentButtons
    add     hl, de
    ; Get sControllerData_Joypad.CurrentButtons
    ld      a, (hl)
    bit     CONTROLLER_JOYPAD_LEFT_BITPOS, a
    jr      z, @@@GoLeft
    bit     CONTROLLER_JOYPAD_RIGHT_BITPOS, a
    jr      z, @@@GoRight

    ; Advance the animation
    push    iy
    pop     ix
    ld      bc, sActorPlayer.Animation
    add     ix, bc  ; IX points to the sAnimationInstance
    ld      de, PlayerAnims@IdleAnim@IdleDef

    call    AnimationInstance@Update

    ; Otherwise, stay in the same state.
    and     a
    ret

@@@GoLeft:
    ld      hl, _ActorPlayer@MovingLeftState
    scf     ; Indicate state change
    ret

@@@GoRight:
    ld      hl, _ActorPlayer@MovingRightState
    scf     ; Indicate state change
    ret

.DSTRUCT @MovingLeftState INSTANCEOF sActorState VALUES
    Base.OnUpdate   .DW _ActorPlayer@MovingLeft@OnUpdate
    OnRenderPrep    .DW _ActorPlayer@OnRenderPrep

.ENDST

@MovingLeft:
@@OnUpdate:
    ; Get actor into IY
    push    de
    pop     iy

    ; Make the character move.
    ld      a, (iy + sActorPlayer.XPos)
    dec     a
    cp      ACTOR_PLAYER_MAX_X
    jp      nc, @@@GoIdle

    ; Otherwise, update the position.
    ld      (iy + sActorPlayer.XPos), a

    ; Check our controller.
    ld      l, (iy + sActorPlayer.pJoypadData + 0)
    ld      h, (iy + sActorPlayer.pJoypadData + 1)
    ld      de, sControllerData_Joypad.CurrentButtons
    add     hl, de
    ; Get sControllerData_Joypad.CurrentButtons
    ld      a, (hl)
    bit     CONTROLLER_JOYPAD_RIGHT_BITPOS, a
    jr      z, @@@GoRight

    ; Otherwise, stay in the same state.
    and     a
    ret

@@@GoIdle:
    ld      (iy + sActorPlayer.XPos), ACTOR_PLAYER_MIN_X
    ld      hl, _ActorPlayer@IdleState
    scf     ; Indicate state change
    ret

@@@GoRight:
    ld      hl, _ActorPlayer@MovingRightState
    scf     ; Indicate state change
    ret


.DSTRUCT @MovingRightState INSTANCEOF sActorState VALUES
    Base.OnUpdate   .DW _ActorPlayer@MovingRight@OnUpdate
    OnRenderPrep    .DW _ActorPlayer@OnRenderPrep
.ENDST

@MovingRight:
@@OnUpdate:
    ; Get actor into IY
    push    de
    pop     iy

    ; Make the character move.
    ld      a, (iy + sActorPlayer.XPos)
    inc     a
    cp      ACTOR_PLAYER_MAX_X
    jr      nc, @@@GoIdle

    ; Otherwise, update the position.
    ld      (iy + sActorPlayer.XPos), a

    ; Check our controller.
    ld      l, (iy + sActorPlayer.pJoypadData + 0)
    ld      h, (iy + sActorPlayer.pJoypadData + 1)
    ld      de, sControllerData_Joypad.CurrentButtons
    add     hl, de
    ; Get ControllerData_Joypad.CurrentButtons
    ld      a, (hl)
    bit     CONTROLLER_JOYPAD_LEFT_BITPOS, a
    jr      z, @@@GoLeft

    ; Otherwise, stay in the same state.
    and     a
    ret

@@@GoIdle:
    ld      (iy + sActorPlayer.XPos), ACTOR_PLAYER_MAX_X
    ld      hl, _ActorPlayer@IdleState
    scf     ; Indicate state change
    ret

@@@GoLeft:
    ld      hl, _ActorPlayer@MovingLeftState
    scf     ; Indicate state change
    ret



;==============================================================================
; _ActorPlayer@OnActorAdded
; Callback when this actor is added to the actor manager (i.e., made active).
; INPUTS:  IY: Pointer to sActorPlayer struct
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@OnActorAdded:
    ret

;==============================================================================
; _ActorPlayer@OnRenderPrep
; Callback when this actor needs to prepare for rendering.  This includes
; updating any graphics or sprites.
; INPUTS:  DE: Pointer to sActorPlayer memory
; OUTPUTS:  None
; Destroys TODO
;==============================================================================
@OnRenderPrep:
@@RenderSprites:
    ; Get actor into IY
    push    de
    pop     iy

    exx
        ; Get current animation's tile indices.
        push    iy
        pop     ix
        ld      bc', sActorPlayer.Animation
        add     ix, bc'  ; IX points to the sAnimationInstance
        ld      l', (ix + sAnimationInstance.pCurrFrame + 0)
        ld      h', (ix + sAnimationInstance.pCurrFrame + 1) ; HL points to the sAnimationFrame
.REPT _sizeof_sAnimationFrame
        inc     hl'
.ENDR
        ; Now HL' points to the first sAnimation_TileDef
    exx

    ; Render a sprite for now
    ld      ix, gModeGameScreen.PlayerSpriteChain
    SPRITE_CHAIN_PREP_ENQUEUE_CHAIN_IN_IX gModeGameScreen.PlayerChainYPos, gModeGameScreen.PlayerChainXPosTilePos

    ld      a, (iy + sActorPlayer.YPos)
    dec     a                               ; Remember that sprite Y positions are off by one in the hardware.
    ld      b, (iy + sActorPlayer.XPos)
@@@UL:
    ex      af, af'
        exx
            ld  a', (hl')
            inc hl'
            inc hl'
        exx
        ld      c, a'
    ex      af, af'
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@LL:
    add     a, 8

    ex      af, af'
        exx
            ld  a', (hl')
            inc hl'
            inc hl'
        exx
        ld      c, a'
    ex      af, af'
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@LR:
    ld      c, a    ; Temp
    ld      a, b    ; A = XPos
    add     a, 8
    ld      b, a
    ld      a, c    ; Restore YPos

    ex      af, af'
        exx
            ld  a', (hl')
            inc hl'
            inc hl'
        exx
        ld      c, a'
    ex      af, af'
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@UR:
    sub     a, 8

    ex      af, af'
        exx
            ld  a', (hl')
            inc hl'
            inc hl'
        exx
        ld      c, a'
    ex      af, af'
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@Done:

@@Done:
    and     a   ; Stay in same state.
    ret

;==============================================================================
; _ActorPlayer@OnCollision
; Callback when this actor has collided with another.
; INPUTS:  DE: Pointer to sActorPlayer struct
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@OnCollision:
    ret





.ENDS


.RAMSECTION "Actor - Player Global RAM" SLOT 3
    ;gActorPlayer_Globals INSTANCEOF sActorPlayer_Globals
.ENDS


.ENDIF ;__ACTOR_PLAYER_ASM__