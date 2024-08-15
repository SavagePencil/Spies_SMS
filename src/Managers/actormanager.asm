.IFNDEF __ACTORMANAGER_ASM__
.DEFINE __ACTORMANAGER_ASM__

.INCLUDE "Utils/fsm.asm"

.SECTION "Actor Manager" FREE

; Actor manager tracks the actors
.STRUCT sActorManager
    pActorListHead  DW   ; Ptr to first sActorInstance in the list
.ENDST

; Common callbacks for each actor, on a per-state basis.
.STRUCT sActorState
    Base                INSTANCEOF sState   ; OnEnter, OnExit, OnUpdate
    OnRenderPrep        DW
    OnCollision         DW
.ENDST

; Callbacks at the actor class level (not per-state).
.STRUCT sActorClassMethods
    OnActorAdded        DW  ; Called when the actor is added to the manager
    OnActorRemoved      DW  ; Called when removed from the manager
.ENDST

.STRUCT sActorInstance
    FSM                 INSTANCEOF sFSM ; State machine for this actor.

    pNextActor          DW              ; Ptr to next actor
    pActorClassMethods  DW              ; Actor-specific callbacks

.ENDST

; Destroys A, B, C, D, E, H, L
.MACRO ACTORMANAGER_CALL_METHOD_ON_ACTOR_IN_DE ARGS METHOD_OFFSET
    ; See if we have class methods for this actor.
    ld      hl, sActorInstance.pActorClassMethods
    add     hl, de      ; HL is now at the DE's class methods.

    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a
    or      h
    jr      z, +        ; No class methods?  We're done then.

    ; Otherwise, see if a callback exists.
    ld      bc, METHOD_OFFSET
    add     hl, bc      ; Move to method

    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a
    or      h
    call    nz, CallHL  ; Only call if we have a callback specified
+:
.ENDM

ActorManager:

;==============================================================================
; ActorManager@Init
; Preps the actor manager
; INPUTS:  None
; OUTPUTS:  None
; Destroys HL
;==============================================================================
@Init:
    ld      hl, $0000
    ld      (gActorManager.pActorListHead), hl
    ret

.ENDS

.SECTION "Actor Manager - Add Actor" FREE
;==============================================================================
; ActorManager@AddActor
; Adds an actor to the manager.  Calls OnActorAdded.
; INPUTS:  DE:  Pointer to an sActorInstance
; OUTPUTS:  None
; Destroys Everything
;==============================================================================
@AddActor:
    ; Get the actor into IY
    push    de
    pop     iy

    ; Replace the head with the current one.
    ld      hl, (gActorManager.pActorListHead)
    ld      (iy + sActorInstance.pNextActor + 0), l
    ld      (iy + sActorInstance.pNextActor + 1), h
    ld      (gActorManager.pActorListHead), de

    ACTORMANAGER_CALL_METHOD_ON_ACTOR_IN_DE sActorClassMethods.OnActorAdded
    ret
.ENDS

.SECTION "Actor Manager - Find Actor" FREE
;==============================================================================
; ActorManager@FindActor
; Attempts to find an actor from within the list.
; INPUTS:  BC:  Pointer to an sActorInstance to find
; OUTPUTS:  Sets carry flag if not found.
;           BC: Pointer to the sActorInstance to find
;           DE: 'Next' pointer that points to the actor.
; Destroys A, HL
;==============================================================================
@FindActor:
    ; Start at the head
    ld      hl, gActorManager.pActorListHead
-:
    ; Get the actor at the pointer
    ld      e, (hl)
    inc     hl
    ld      d, (hl)
    ld      a, e
    or      d
    jr      z, @@NotFound       ; Was it NULL?  If so, we didn't find it.
    dec     hl                  ; HL back to original loc
    ex      de, hl              ; HL now points to the sActorInstance being tested, DE holds the 'next' pointer.
    and     a
    sbc     hl, bc
    ret     z                   ; If these match, we found our actor.  Carry is clear.

    ; Not it.  Keep going.
    ex      de, hl              ; Back to HL holding 'next' pointer.
    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a

    .REPT sActorInstance.pNextActor
        inc hl
    .ENDR
    jp      -

@@NotFound:
    scf
    ret
.ENDS

.SECTION "Actor Manager - Remove Actor" FREE
;==============================================================================
; ActorManager@RemoveActor
; Removes an actor from the manager.  Calls OnActorRemoved.
; INPUTS:  DE:  Pointer to an sActorInstance to remove
; OUTPUTS:  None
; Destroys Everything
;==============================================================================
@RemoveActor:
    ; Move DE into BC
    ld      c, e
    ld      b, d

    call    ActorManager@FindActor
    jr      c, @@ActorNotFound      ; If carry is set, we didn't find the actor.

    push    bc
    pop     iy      ; IY now points to the sActorInstance we're looking to remove.

    ; DE holds the previous 'next' pointer that points to our actor.
    ld      a, (iy + sActorInstance.pNextActor + 0)
    ld      (de), a
    inc     de
    ld      a, (iy + sActorInstance.pNextActor + 1)
    ld      (de), a ; The previous "next" has been updated to remove this node.

    ; Put BC back into DE
    ld      e, c
    ld      d, b
    
    ; Now call the remove function for this actor.
    ACTORMANAGER_CALL_METHOD_ON_ACTOR_IN_DE sActorClassMethods.OnActorRemoved
    ret

@@ActorNotFound:
    ; Attempted to remove an actor that couldn't be found.  Could set a breakpoint or error.
    ret
.ENDS

.SECTION "Actor Manager - Call Method on List" FREE
;==============================================================================
; ActorManager@CallMethodOnList
; Calls a method on every actor in the list.
; INPUTS:  BC: sActorState Method offset to call
; OUTPUTS:  None
; Destroys EVERYTHING
;==============================================================================
@CallMethodOnList:
    ; Get at the actor at the head
    ld      de, (gActorManager.pActorListHead)
-:
    ; Is this actor NULL?
    ld      a, d
    or      e
    ret     z               ; Return if no actor.

    ; Actor exists.

    ; Preserve our offset for the next actor
    push    bc
        ; Call the function.
        call    FSM_DE@OnEvent
    pop     bc

@@NextActor:
    ; Advance DE to get the pNextActor.
.REPT sActorInstance.pNextActor
    inc     de
.ENDR
    ex      de, hl
    ; HL = sActorInstance.pNextActor
    ld      e, (hl)
    inc     hl
    ld      d, (hl)

    jp      -
.ENDS


.RAMSECTION "Actor Manager Instance" SLOT 3
    gActorManager INSTANCEOF sActorManager
.ENDS


.ENDIF  ;__ACTORMANAGER_ASM__