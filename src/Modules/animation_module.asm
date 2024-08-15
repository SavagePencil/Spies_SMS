.IFNDEF __ANIMATION_MODULE_ASM__
.DEFINE __ANIMATION_MODULE_ASM__

.SECTION "Animation Module" FREE

; Animation behaviors
.ENUMID 0
; Animation runs, then loops back to the beginning.
.ENUMID ANIMATION_BEHAVIOR_LOOP
; Animation runs, and then holds with the same value at the end.
.ENUMID ANIMATION_BEHAVIOR_1SHOT
; Animation runs, then reverses.
.ENUMID ANIMATION_BEHAVIOR_PING_PONG

; Behavior flags
.ENUMID 0
.ENUMID ANIMATION_BEHAVIOR_FLAG_PING_PONG_FORWARD
.ENUMID ANIMATION_BEHAVIOR_FLAG_PING_PONG_BACKWARD
.ENUMID ANIMATION_BEHAVIOR_FLAG_1SHOT_AT_END

; Signals sent after the animation is updated
.ENUMID 0
; Animation started a loop, or started a ping-pong.
.ENUMID ANIMATION_SIGNAL_LOOPED
; Animation reached the end.
.ENUMID ANIMATION_SIGNAL_REACHED_END

.STRUCT sAnimationDefinition
    Behavior            DB
    NumFrames           DB
    ; #/tiles used in each frame, e.g., a 2x2 will have four.
    NumTilesPerFrame    DB
    ; Followed by a series of sAnimationFrames
.ENDST

.STRUCT sAnimationFrame
    ; How long do we stay in this frame before advancing?
    FrameDuration       DB

    ; Followed by a sequence of sAnimationDefinition.NumTilesPerFrame sAnimationFrame_TileDefs
.ENDST

.STRUCT sAnimationFrame_TileDef
    ; Where does this go in VRAM?  
    ; BGs use 0..448, while Sprites only use 0..255
    .UNION
        BGTileIndex     DW  ; 0..448
    .NEXTU
        SpriteTileIndex DB  ; 0..255
        Unused          DB
    .ENDU
.ENDST

; An instance of an animation uses one of these.
.STRUCT sAnimationInstance
    ; How much tie is left in this frame?
    TimeLeftInFrame     DB

    ; Pointer to sAnimationFrame
    pCurrFrame          DW

    ; Helper pointer
    pFirstFrame         DW

    ; Helper calculation
    BytesPerFrame       DB

    ; Which frame # we're on
    CurrFrameIndex      DB

    ; Behavior-specific state flags
    BehaviorFlags       DB

    ; We maintain a COPY of the animation definition for faster lookup.
    AnimDef INSTANCEOF  sAnimationDefinition

.ENDST

AnimationInstance:
;==============================================================================
; AnimationInstance@Init
; Sets initial values for an animation instance
; INPUTS:  IX: Pointer to sAnimationInstance memory
;          DE: Pointer to sAnimationDefinition definition
; OUTPUTS:  None
; Destroys A, BC, DE, HL, IY
;==============================================================================
@Init:
    ; COPY the sAnimationDefinition into our memory.
    push    ix
    pop     hl
    ld      bc, sAnimationInstance.AnimDef
    add     hl, bc  ; HL now points to the sAnimationDefinition inside of our instance.

    ; Execute the copy.
    ex      de, hl
    ld      bc, _sizeof_sAnimationDefinition
    ldir

    ; HL now points to the byte *after* the sAnimationDefinition, which coincidentally
    ; is the first sAnimationFrame.  Load this in as our first frame.
    ld      (ix + sAnimationInstance.pFirstFrame + 0), l
    inc     hl
    ld      (ix + sAnimationInstance.pFirstFrame + 1), h
    dec     hl

    ; How many bytes per frame?  It's _sizeof_sAnimationFrame + (#/tiles per frame * _sizeof_sAnimationFrame_TileDef)
    ld      a, _sizeof_sAnimationFrame
    ld      b, (ix + sAnimationInstance.AnimDef.NumTilesPerFrame)
-:
    add     a, _sizeof_sAnimationFrame_TileDef
    djnz    -
    ld      (ix + sAnimationInstance.BytesPerFrame), a

    ; Start at the first frame.
    xor     a
    call    @SetFrameByIndex

    ; Adjust flags based on behavior.
    ld      a, (ix + sAnimationInstance.AnimDef.Behavior)
    cp      ANIMATION_BEHAVIOR_LOOP
    jr      z, @@Behavior_Loop
    cp      ANIMATION_BEHAVIOR_1SHOT
    jr      z, @@Behavior_1Shot
    ; Else, assume it's a ping-pong.
@@Behavior_PingPong:
    ld      (ix + sAnimationInstance.BehaviorFlags), ANIMATION_BEHAVIOR_FLAG_PING_PONG_FORWARD
    ret
@@Behavior_Loop:
    ld      (ix + sAnimationInstance.BehaviorFlags), 0
    ret
@@Behavior_1Shot:
    ld      (ix + sAnimationInstance.BehaviorFlags), 0
    ret

.ENDS

.SECTION "Animation Module - Set Frame By Index" FREE
;==============================================================================
; AnimationInstance@SetFrameByIndex
; Sets up a new frame based on index in the animation.  Resets the timer.
; INPUTS:  IX: Pointer to sAnimationInstance memory
;           A: Index to switch to.
; OUTPUTS:  None
; Destroys A, DE, HL, IY
;==============================================================================
@SetFrameByIndex:
    ; Find the frame details.
    ld      l, (ix + sAnimationInstance.pFirstFrame + 0)
    ld      h, (ix + sAnimationInstance.pFirstFrame + 1)

    ld      (ix + sAnimationInstance.CurrFrameIndex), a

    and     a
    jp      z, @@FrameFound

    ; How big is each frame?
    ld      e, (ix + sAnimationInstance.BytesPerFrame)
    ld      d, 0
-:
    add     hl, de  ; Move to next frame.
    dec     a
    jp      nz, -

@@FrameFound:
    ; OK, we've got it.  HL points to the sAnimationFrame we need.
    ld      (ix + sAnimationInstance.pCurrFrame + 0), l
    ld      (ix + sAnimationInstance.pCurrFrame + 1), h

    ; Get the timing off of it.
    push    hl
    pop     iy
    ld      a, (iy + sAnimationFrame.FrameDuration)
    ld      (ix + sAnimationInstance.TimeLeftInFrame), a

    ret
.ENDS

.SECTION "Animation Module - Update" FREE
;==============================================================================
; AnimationInstance@Update
; Updates and advances the frame based on the behavior specified.
; INPUTS:  IX: Pointer to sAnimationInstance memory
; OUTPUTS:  Sets carry flag if an event happened.
;           A: Returns ANIMATION_SIGNAL_* (only if carry was set)
; Destroys A, DE, HL, IY
;==============================================================================
@Update:
    ; Don't do anything if at the end of a 1-shot.
    ld      a, (ix + sAnimationInstance.BehaviorFlags)
    cp      ANIMATION_BEHAVIOR_FLAG_1SHOT_AT_END
    jr      nz, @@NormalBehavior

    ; Clear carry and go home.
    and     a
    ret

@@NormalBehavior:
    dec     (ix + sAnimationInstance.TimeLeftInFrame)
    ret     nz  ; Get out if our timer is still going.

    ; Zero?  Then we need to advance.  If we're at the end, then we need to handle things
    ; differently.
    ld      a, (ix + sAnimationInstance.AnimDef.Behavior)
    cp      ANIMATION_BEHAVIOR_LOOP
    jr      z, @@@Behavior_Loop
    cp      ANIMATION_BEHAVIOR_1SHOT
    jr      z, @@@Behavior_1Shot
    ; Else, assume it's a ping-pong.
@@@PingPong:
    ; Are we going up, or down?
    ld      a, ANIMATION_BEHAVIOR_FLAG_PING_PONG_FORWARD
    cp      (ix + sAnimationInstance.BehaviorFlags)
    jr      z, @@@@GoingForwards
    ; Else, assume we're going backwards.
@@@@GoingBackwards:
    ; Decrement the frame.
    ld      a, (ix + sAnimationInstance.CurrFrameIndex)
    dec     a
    jp      m, @@@@@StartGoingForwards  ; If we went below zero, it's time to ping-pong!

    ; Otherwise update and stay in state.
    call    @SetFrameByIndex
    and     a
    ret
@@@@@StartGoingForwards:
    ; Flip direction.
    ld      a, 1
    call    @SetFrameByIndex

    ld      (ix + sAnimationInstance.BehaviorFlags), ANIMATION_BEHAVIOR_FLAG_PING_PONG_FORWARD

    ; Let the caller know.
    scf
    ld      a, ANIMATION_SIGNAL_LOOPED
    ret

@@@@GoingForwards:
    ; Increment the frame.
    ld      a, (ix + sAnimationInstance.CurrFrameIndex)
    inc     a
    cp      (ix + sAnimationInstance.AnimDef.NumFrames)
    jr      z, @@@@@StartGoingBackwards  ; If we went to our max, it's time to ping-pong!

    ; Otherwise update and stay in state.
    call    @SetFrameByIndex
    and     a
    ret
@@@@@StartGoingBackwards:
    ; Flip direction.
    ld      a, (ix + sAnimationInstance.AnimDef.NumFrames)
    sub     2
    call    @SetFrameByIndex

    ld      (ix + sAnimationInstance.BehaviorFlags), ANIMATION_BEHAVIOR_FLAG_PING_PONG_BACKWARD

    ; Let the caller know.
    scf
    ld      a, ANIMATION_SIGNAL_LOOPED
    ret

@@@Behavior_Loop:
    ld      a, (ix + sAnimationInstance.CurrFrameIndex)
    inc     a
    ; Are we on the last frame?
    cp      (ix + sAnimationInstance.AnimDef.NumFrames)
    jp      z, @@@@LastFrame

    ; Update the frame.
    call    @SetFrameByIndex
    and     a
    ret
@@@@LastFrame:
    ; Start over from the beginning.
    xor     a
    call    @SetFrameByIndex

    ; Let the caller know.
    scf
    ld      a, ANIMATION_SIGNAL_LOOPED
    ret

@@@Behavior_1Shot:
    ld      a, (ix + sAnimationInstance.CurrFrameIndex)
    inc     a
    ; Are we on the last frame?
    cp      (ix + sAnimationInstance.AnimDef.NumFrames)
    jp      z, @@@@LastFrame

    ; Update the frame.
    call    @SetFrameByIndex
    and     a
    ret

@@@@LastFrame:
    ld      (ix + sAnimationInstance.BehaviorFlags), ANIMATION_BEHAVIOR_FLAG_1SHOT_AT_END

    ; Let the caller know.
    scf
    ld      a, ANIMATION_SIGNAL_REACHED_END
    ret    
.ENDS

.ENDIF  ;__ANIMATION_MODULE_ASM__