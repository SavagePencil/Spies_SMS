.IFNDEF __ACTION_UPLOAD_VRAMDATA_ASM__
.DEFINE __ACTION_UPLOAD_VRAMDATA_ASM__

.INCLUDE "Modules/execute_buffer.asm"

.SECTION "Action - Upload Data to VRAM Indirect" FREE
; Uploads data to VRAM that is located elsewhere (e.g., ROM).

.STRUCT sAction_UploadVRAMData_Indirect
    ExecuteEntry    INSTANCEOF sExecuteEntry    ; Callback
    DestVRAMLoc     DW                          ; Where in VRAM will this go?
    Length          DW                          ; Length in bytes?
    pData           DW                          ; Where's the src data?
.ENDST

;==============================================================================
; Action_UploadVRAMData_Indirect
; Execute Buffer action to upload data that is pointed to into VRAM.
; INPUTS:  DE:  Start of sAction_UploadVRAMData_Indirect
; OUTPUTS:  DE: Next byte in Execute Buffer
; Destroys Everything
;==============================================================================
Action_UploadVRAMData_Indirect:
    ex  de, hl

    ; Get DestVRAM Loc
    ld  e, (hl)
    inc hl
    ld  d, (hl)
    inc hl

    ; Get Length of Data
    ld  c, (hl)
    inc hl
    ld  b, (hl)
    inc hl

    ; Get Ptr to Data
    ld  a, (hl)
    inc hl
    push    hl  ; Preserve next pos
        ld      h, (hl)
        ld      l, a

        call    VDP_UploadDataToVRAMLoc
    pop     de

    inc     de

    ret


.ENDS

.SECTION "Action - Upload VRAM Data Implicit" FREE
; Uploads data that is located here in the execute buffer (not pointed to elsewhere) to VRAM.

.STRUCT sAction_UploadVRAMData_Implicit
    ExecuteEntry    INSTANCEOF sExecuteEntry    ; Callback
    DestVRAMLoc     DW                          ; Where in VRAM will this go?
    Length          DW                          ; Length in bytes?
    ; Data goes after this, of variable size.
.ENDST

;==============================================================================
; Action_UploadVRAMData_Implicit
; Execute Buffer action to upload data in the buffer to VRAM.
; INPUTS:  DE:  Start of sAction_UploadVRAMData_Implicit
; OUTPUTS:  DE: Next byte in Execute Buffer
; Destroys Everything
;==============================================================================
Action_UploadVRAMData_Implicit:
    ex  de, hl

    ; Get DestVRAM Loc
    ld  e, (hl)
    inc hl
    ld  d, (hl)
    inc hl

    ; Get Length of Data
    ld  c, (hl)
    inc hl
    ld  b, (hl)
    inc hl

    ; HL now points to the implicit data.
    call    VDP_UploadDataToVRAMLoc

    ; HL now points to byte past end of buffer, which is perfect.  Now move it to DE
    ex      de, hl
@Done:
    ret


.ENDS

.SECTION "Action - Upload VRAM List Implicit" FREE
; Uploads data that is located here in the execute buffer (not pointed to elsewhere) to VRAM.
; Data is uploaded as a sequence of linear runs.
.STRUCT sAction_UploadVRAMList_Implicit
    ExecuteEntry        INSTANCEOF sExecuteEntry    ; Callback
    NumRuns             DB                          ; How many runs?
    ; A series of sAction_UploadVRAMList_Run go after this.
.ENDST

.STRUCT sAction_UploadVRAMList_Run
    VRAMLoc             DW                          ; Start pos of run
    RunLengthInBytes    DB                          ; #/bytes in run.

    ; Data goes after this, of variable size.
.ENDST

;==============================================================================
; Action_UploadVRAMList_Implicit
; Execute Buffer action to upload data in the buffer to VRAM.
; INPUTS:  DE:  Start of sAction_UploadVRAMList_Implicit
; OUTPUTS:  DE: Next byte in Execute Buffer
; Destroys Everything
;==============================================================================
Action_UploadVRAMList_Implicit:
    ex      de, hl

    ld      d, (hl)     ; Get #/runs
    inc     hl          ; Point to first Run

    ld      c, VDP_DATA_PORT
-:
    ; Start of sAction_UploadNameTable_Run
    ld      a, (hl)                     ; VRAM loc
    inc     hl
    out     (VDP_CONTROL_PORT), a
    ld      a, (hl)
    inc     hl
    or      VDP_COMMAND_MASK_VRAM_WRITE
    out     (VDP_CONTROL_PORT), a

    ld      b, (hl)                     ; Get run length
    inc     hl

    otir                                ; UL VRAM data

    dec     d                           ; Next run
    jp      nz, -

    ; HL now points to byte past end of buffer, which is perfect.  Now move it to DE
    ex      de, hl

    ret


.ENDS

.SECTION "Action - Upload VRAM List Indirect" FREE
; Uploads data that is located elsewhere (e.g., ROM) to VRAM.
; Data is uploaded as a sequence of linear runs.
.STRUCT sAction_UploadVRAMList_Indirect
    ExecuteEntry        INSTANCEOF sExecuteEntry    ; Callback
    pHeader             DW                          ; Pointer to sAction_UploadVRAMList_Indirect_Header 
.ENDST

.STRUCT sAction_UploadVRAMList_Indirect_Header
    NumRuns             DB                          ; How many runs?
    ; A series of sAction_UploadVRAMList_Run go after this.
.ENDST

;==============================================================================
; Action_UploadVRAMList_Indirect
; Execute Buffer action to upload data that is pointed to into VRAM.
; INPUTS:  DE:  Start of sAction_UploadVRAMList_Indirect
; OUTPUTS:  DE: Next byte in Execute Buffer
; Destroys Everything
;==============================================================================
Action_UploadVRAMList_Indirect:
    ld      a, (de)
    ld      l, a
    inc     de
    ld      a, (de)
    ld      h, a
    inc     de      ; DE points to next byte in Execute Buffer, HL holds the ptr to sAction_UploadVRAMList_Indirect_Header

    push    de
        ld      d, (hl) ; Get #/runs
        inc     hl      ; Point to first Run

        ld      c, VDP_DATA_PORT
-:
        ; Start of sAction_UploadVRAMList_Run
        ld      a, (hl) ; VRAM loc
        inc     hl
        out     (VDP_CONTROL_PORT), a
        ld      a, (hl)
        inc     hl
        or      VDP_COMMAND_MASK_VRAM_WRITE
        out     (VDP_CONTROL_PORT), a

        ld      b, (hl) ; Run length
        inc     hl

        otir

        dec     d       ; Next run
        jp  nz, -
    
    pop de
    ret
.ENDS


.ENDIF  ; __ACTION_UPLOAD_VRAMDATA_ASM__