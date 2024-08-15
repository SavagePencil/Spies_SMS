.IFNDEF __EXECUTE_BUFFER_ASM__
.DEFINE __EXECUTE_BUFFER_ASM__

.SECTION "Execute Buffer - Init" FREE

.STRUCT sExecuteEntry
    CallbackFunction    DW      ; The callback function.  Entries requiring additional parameters can derive from this.
.ENDST

.STRUCT sExecuteBufferDescriptor
    TotalCapacity       DW      ; Capacity of the buffer, in bytes.
    SpaceRemaining      DW      ; How much do we currently have left?
    StartPos            DW      ; Where does the buffer start at?
    CurrPos             DW      ; Where is the current position in the buffer?  Starts immediately after this.
.ENDST

;==============================================================================
; ExecuteBuffer_Init
; Initializes an execute buffer.
; INPUTS:  IY: Pointer to sExecuteBufferDescriptor in memory
;          DE: Location of buffer.
;          BC: Capacity, in bytes.
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
ExecuteBuffer_Init:
    ld      (iy + sExecuteBufferDescriptor.TotalCapacity + 0), c
    ld      (iy + sExecuteBufferDescriptor.TotalCapacity + 1), b
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 0), c
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 1), b
    ld      (iy + sExecuteBufferDescriptor.StartPos + 0), e
    ld      (iy + sExecuteBufferDescriptor.StartPos + 1), d
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 0), e
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 1), d

    ret
.ENDS

.SECTION "Execute Buffer - Reset" FREE
;==============================================================================
; ExecuteBuffer_Reset
; Resets an execute buffer.
; INPUTS:  IY: Pointer to sExecuteBufferDescriptor
; OUTPUTS:  None
; Destroys HL
;==============================================================================
ExecuteBuffer_Reset:
    ld      l, (iy + sExecuteBufferDescriptor.StartPos + 0)
    ld      h, (iy + sExecuteBufferDescriptor.StartPos + 1)
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 0), l
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 1), h
    ld      l, (iy + sExecuteBufferDescriptor.TotalCapacity + 0)
    ld      h, (iy + sExecuteBufferDescriptor.TotalCapacity + 1)
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 0), l
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 1), h
    ret
.ENDS

.SECTION "Execute Buffer - Attempt Enqueue IY" FREE
;==============================================================================
; ExecuteBuffer_AttemptEnqueue_IY
; Attempts to enqueue an execute command into the buffer.
; INPUTS:  IY: Pointer to sExecuteBufferDescriptor
;          DE: Pointer to sExecuteEntry
;          BC: Size of Entry to enqueue
; OUTPUTS: Carry flag set if FAILED to enqueue.
; Destroys HL, DE, BC
;==============================================================================
ExecuteBuffer_AttemptEnqueue_IY:
    ; Will this thing fit?
    ld      l, (iy + sExecuteBufferDescriptor.SpaceRemaining + 0)
    ld      h, (iy + sExecuteBufferDescriptor.SpaceRemaining + 1)
    and     a
    sbc     hl, bc
    ret     c           ; Carry will be set

    ; OK, we can fit.
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 0), l
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 1), h

    ; Copy the data into the buffer.
    ld      l, (iy + sExecuteBufferDescriptor.CurrPos + 0)
    ld      h, (iy + sExecuteBufferDescriptor.CurrPos + 1)
    ex      de, hl
    ldir

    ; Update curr position for next person.
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 0), e
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 1), d

    and     a   ; Clear carry
    ret

.ENDS

.SECTION "Execute Buffer - Attempt Enqueue IX" FREE
;==============================================================================
; ExecuteBuffer_AttemptEnqueue_IX
; Attempts to enqueue an execute command into the buffer.
; INPUTS:  IX: Pointer to sExecuteBufferDescriptor
;          DE: Pointer to sExecuteEntry
;          BC: Size of Entry to enqueue
; OUTPUTS:  Carry flag set if FAILED to enqueue.
; Destroys HL, DE, BC
;==============================================================================
ExecuteBuffer_AttemptEnqueue_IX:
    ; Will this thing fit?
    ld      l, (ix + sExecuteBufferDescriptor.SpaceRemaining + 0)
    ld      h, (ix + sExecuteBufferDescriptor.SpaceRemaining + 1)
    and     a
    sbc     hl, bc
    ret     c           ; Carry will be set

    ; OK, we can fit.
    ld      (ix + sExecuteBufferDescriptor.SpaceRemaining + 0), l
    ld      (ix + sExecuteBufferDescriptor.SpaceRemaining + 1), h

    ; Copy the data into the buffer.
    ld      l, (ix + sExecuteBufferDescriptor.CurrPos + 0)
    ld      h, (ix + sExecuteBufferDescriptor.CurrPos + 1)
    ex      de, hl
    ldir

    ; Update curr position for next person.
    ld      (ix + sExecuteBufferDescriptor.CurrPos + 0), e
    ld      (ix + sExecuteBufferDescriptor.CurrPos + 1), d

    and     a   ; Clear carry
    ret

.ENDS


.SECTION "Execute Buffer - Attempt Reserve IY" FREE
;==============================================================================
; ExecuteBuffer_AttemptReserve_IY
; Attempts to reserve space for an execute command into the buffer.
; INPUTS:  IY: Pointer to sExecuteBufferDescriptor
;          BC: Size of Entry to reserve for
; OUTPUTS:  Carry flag set if FAILED to reserve enough space.
;           DE: Start of reserved location (if successful)
; Destroys HL, DE
;==============================================================================
ExecuteBuffer_AttemptReserve_IY:
    ; Will this thing fit?
    ld      l, (iy + sExecuteBufferDescriptor.SpaceRemaining + 0)
    ld      h, (iy + sExecuteBufferDescriptor.SpaceRemaining + 1)
    and     a
    sbc     hl, bc
    ret     c           ; Carry will be set

    ; OK, we can fit.
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 0), l
    ld      (iy + sExecuteBufferDescriptor.SpaceRemaining + 1), h

    ; Remember where we currently are.
    ld      l, (iy + sExecuteBufferDescriptor.CurrPos + 0)
    ld      h, (iy + sExecuteBufferDescriptor.CurrPos + 1)

    ; Hang onto it to give back to the caller.
    ld      e, l
    ld      d, h

    ; Move the current position pointer.
    add     hl, bc

    ; Update curr position for next person.
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 0), l
    ld      (iy + sExecuteBufferDescriptor.CurrPos + 1), h

    and     a   ; Clear carry
    ret

.ENDS

.SECTION "Execute Buffer - Attempt Reserve IX" FREE
;==============================================================================
; ExecuteBuffer_AttemptReserve_IX
; Attempts to reserve space for an execute command into the buffer.
; INPUTS:  IX: Pointer to sExecuteBufferDescriptor
;          BC: Size of Entry to reserve for
; OUTPUTS:  Carry flag set if FAILED to reserve enough space.
;           DE: Start of reserved location (if successful)
; Destroys HL, DE
;==============================================================================
ExecuteBuffer_AttemptReserve_IX:
    ; Will this thing fit?
    ld      l, (ix + sExecuteBufferDescriptor.SpaceRemaining + 0)
    ld      h, (ix + sExecuteBufferDescriptor.SpaceRemaining + 1)
    and     a
    sbc     hl, bc
    ret     c           ; Carry will be set

    ; OK, we can fit.
    ld      (ix + sExecuteBufferDescriptor.SpaceRemaining + 0), l
    ld      (ix + sExecuteBufferDescriptor.SpaceRemaining + 1), h

    ; Remember where we currently are.
    ld      l, (ix + sExecuteBufferDescriptor.CurrPos + 0)
    ld      h, (ix + sExecuteBufferDescriptor.CurrPos + 1)

    ; Hang onto it to give back to the caller.
    ld      e, l
    ld      d, h

    ; Move the current position pointer.
    add     hl, bc

    ; Update curr position for next person.
    ld      (ix + sExecuteBufferDescriptor.CurrPos + 0), l
    ld      (ix + sExecuteBufferDescriptor.CurrPos + 1), h

    and     a   ; Clear carry
    ret

.ENDS

.SECTION "Execute Buffer - Execute" FREE
;==============================================================================
; ExecuteBuffer_Execute
; Executes a buffer, starting at the top.
; INPUTS:  IY: Pointer to sExecuteBufferDescriptor
; OUTPUTS:  None
; Destroys HL, DE, BC, anything from callbacks.
;==============================================================================
ExecuteBuffer_Execute:
    ; Start at the top.
    ld      e, (iy + sExecuteBufferDescriptor.StartPos + 0)
    ld      d, (iy + sExecuteBufferDescriptor.StartPos + 1)
-:
    ; Are we done?
    ld      l, (iy + sExecuteBufferDescriptor.CurrPos + 0)
    ld      h, (iy + sExecuteBufferDescriptor.CurrPos + 1)
    and     a
    sbc     hl, de
    ret     z

    ; Get the callback
    ld      a, (de)
    ld      l, a
    inc     de
    ld      a, (de)
    ld      h, a        ; HL is the callback
    inc     de

    dec     bc          ; Remove the callback from our count
    dec     bc

    ; NOTE:  IT IS THE RESPONSIBILITY OF THE CALLER TO:
    ; * ADJUST DE TO BE PAST THE EXECUTION ENTRY
    call    CallHL

    jp      -

.ENDS

.ENDIF ; __EXECUTE_BUFFER_ASM__