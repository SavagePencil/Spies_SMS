.IFNDEF __ACTOR_ELEVATOR_ASM__
.DEFINE __ACTOR_ELEVATOR_ASM__

.INCLUDE "Managers/actormanager.asm"
.INCLUDE "Managers/gamemanager.asm"
.INCLUDE "Managers/tilemanager.asm"
.INCLUDE "Utils/spritechain.asm"
.INCLUDE "Utils/vdp.asm"
.INCLUDE "Actions/action_upload_vramdata.asm"

.SECTION "Actor - Elevator Base" FREE

.ENUMID 0
.ENUMID ACTOR_ELEVATOR_UL
.ENUMID ACTOR_ELEVATOR_ML
.ENUMID ACTOR_ELEVATOR_LL
.ENUMID ACTOR_ELEVATOR_UR
.ENUMID ACTOR_ELEVATOR_MR
.ENUMID ACTOR_ELEVATOR_LR
.ENUMID ACTOR_ELEVATOR_TOTAL

.DEFINE ELEVATOR_MAX_Y  175
.DEFINE ELEVATOR_MIN_Y  3
.DEFINE ELEVATOR_HEIGHT_IN_TILES    3   ; May span across more tiles than their height.
.DEFINE ELEVATOR_WIDTH_IN_TILES     2

; Global variables used for perf optimizations.
.STRUCT sActorElevator_Globals
    ; Pointer to tiles that are underneath of us.
    TempPtrToSrcBGTile_Upper    DW
    TempPtrToSrcBGTile_Middle   DW
    TempPtrToSrcBGTile_Lower    DW

    ; Pointer for where tile data will be uploaded to VRAM
    TempDestTileVRAMLoc         DW

    ; Pointer to nametable action to upload nametable values at VBLANK
    TempDestNameTableAction     DW

    ; Pointer to the nametable action payload, detailing the actual tiles to change.
    TempDestNameTablePayload    DW

    ; What's our "sprite's" offset inside the new tile?
    TempYPixelOffset            DB

    ; Pointer to the actor's nametable offset.  This is used frequently enough to warrant caching it.
    TempNameTableOffset         DW
    
.ENDST

.STRUCT sActorElevator_Base
    ActorInstance INSTANCEOF sActorInstance
    XPos            DB
    YPos            DB
    YSpeed          DB
.ENDST

; Structure used in level definitions for initialization.
.STRUCT sActorElevator_BaseInit
    InitialXPos     DB
    InitialYPos     DB
    InitialSpeed    DB
.ENDST

; Public
ActorElevator_Base:

;==============================================================================
; ActorElevator_Base@Init
; Sets initial values for the actor
; INPUTS:  IY: Pointer to sActorElevator memory
;          IX: Pointer to sActorElevator_BaseInit
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@Init:
    ; IY points to the sActorElevator_Base
    ld      a, (ix + sActorElevator_BaseInit.InitialXPos)
    ld      (iy + sActorElevator_Base.XPos), a

    ld      a, (ix + sActorElevator_BaseInit.InitialYPos)
    ld      (iy + sActorElevator_Base.YPos), a

    ld      a, (ix + sActorElevator_BaseInit.InitialSpeed)
    ld      (iy + sActorElevator_Base.YSpeed), a
    ret

;==============================================================================
; ActorElevator_Base@OnActorAdded
; Callback when this actor is added to the actor manager (i.e., made active).
; INPUTS:  IY: Pointer to sActorElevator struct
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@OnActorAdded:
    ret

;==============================================================================
; ActorElevator_Base@OnUpdate
; Callback when this actor needs to update.
; INPUTS:  DE: Pointer to sActorElevator struct
; OUTPUTS:  None
; Destroys A, IY
;==============================================================================
@OnUpdate:
    ; Get our actor into IY
    push    de
    pop     iy

    ; Move the elevator.
    ld  a, (iy + sActorElevator_Base.YPos)
    add a, (iy + sActorElevator_Base.YSpeed)
    ld  (iy + sActorElevator_Base.YPos), a

    ; Did we go over our max?
    cp  ELEVATOR_MAX_Y
    jr  nc, @@SwapDirsMax
    cp  ELEVATOR_MIN_Y
    jr  c, @@SwapDirsMin

@@MovementDone:
    and a   ; Stay in same state
    ret

@@SwapDirsMax:
    ld  (iy + sActorElevator_Base.YPos), ELEVATOR_MAX_Y
    jp  @@SwapYDir

@@SwapDirsMin:
    ld (iy + sActorElevator_Base.YPos), ELEVATOR_MIN_Y
    jp @@SwapYDir

@@SwapYDir:
    ld  a, (iy + sActorElevator_Base.YSpeed)
    neg
    ld  (iy + sActorElevator_Base.YSpeed), a
    jp  @@MovementDone

;==============================================================================
; ActorElevator_Base@OnCollision
; Callback when this actor has collided with another.
; INPUTS:  DE: Pointer to sActorElevator struct
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@OnCollision:
    ; TODO
    and     a   ; Stay in same state
    ret

.ENDS

.SECTION "Actor - Elevator Sprite" FREE

; Tile indices for our elevator sprites
.ENUMID 252
.ENUMID ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UL
.ENUMID ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LL
.ENUMID ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UR
.ENUMID ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LR

.STRUCT sActorElevatorSprite
    ; The Sprite variant keeps the base variables here.
    Base                        INSTANCEOF sActorElevator_Base
.ENDST

; Structure used in level definitions for initialization.
.STRUCT sActorElevatorSprite_Init
    Base                        INSTANCEOF sActorElevator_BaseInit
.ENDST

; Public Interface
ActorElevatorSprite:

.DSTRUCT @ActorElevatorSpriteClassMethods INSTANCEOF sActorClassMethods VALUES
    OnActorAdded                .DW ActorElevator_Base@OnActorAdded     ; Uses base
.ENDST

.DSTRUCT @ActorElevatorSpriteMovingState INSTANCEOF sActorState VALUES
    Base.OnUpdate               .DW ActorElevator_Base@OnUpdate         ; Uses base
    OnRenderPrep                .DW _ActorElevatorSprite@OnRenderPrep   ; Custom
    OnCollision                 .DW ActorElevator_Base@OnCollision      ; Uses base
.ENDST

;==============================================================================
; ActorElevatorSprite@Init
; Sets initial values for the actor
; INPUTS:  DE: Pointer to sActorElevator memory
;          IX: Pointer to sActorElevatorSprite_Init definition
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@Init:
    ; Get our actor into IY
    push    de
    pop     iy

    ; Call base class
    call    ActorElevator_Base@Init

    ; Assign the class methods
    ld      (iy + sActorElevatorSprite.Base.ActorInstance.pActorClassMethods + 0), <ActorElevatorSprite@ActorElevatorSpriteClassMethods
    ld      (iy + sActorElevatorSprite.Base.ActorInstance.pActorClassMethods + 1), >ActorElevatorSprite@ActorElevatorSpriteClassMethods

    ; Init our FSM.
    ld      hl, @ActorElevatorSpriteMovingState
    call    FSM_IY@Init

    ret

_ActorElevatorSprite:
;==============================================================================
; _ActorElevatorSprite@OnRenderPrep
; Callback when this actor needs to prepare for rendering.  This includes
; updating any graphics or sprites.
; INPUTS:  DE: Pointer to sActorElevator struct
; OUTPUTS:  None
; Destroys A, B, C, DE, HL, IX, IY.
;==============================================================================
@OnRenderPrep:
    ; Get actor into IY
    push    de
    pop     iy
@@RenderSprites:
    ; Render a sprite for now
    ld      ix, gModeGameScreen.EnemySpriteChain
    SPRITE_CHAIN_PREP_ENQUEUE_CHAIN_IN_IX gModeGameScreen.EnemyChainYPos, gModeGameScreen.EnemyChainXPosTilePos

    ld      a, (iy + sActorElevatorSprite.Base.YPos)
    dec     a                               ; Remember that sprite Y positions are off by one in the hardware.
    ld      b, (iy + sActorElevatorSprite.Base.XPos)
@@@UL:
    ld      c, ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UL
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@LL:
    add     a, 8
    ld      c, ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LL
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@LR:
    ld      c, a    ; Temp
    ld      a, b    ; A = XPos
    add     a, 8
    ld      b, a
    ld      a, c    ; Restore YPos

    ld      c, ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LR
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@UR:
    sub     a, 8

    ld      c, ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UR
    SPRITE_CHAIN_ENQUEUE_SPRITE
@@@Done:

@@Done:
    and     a       ; Stay in same state
    ret

.ENDS


.SECTION "Actor - Elevator BG" FREE
; This Elevator Actor uses BG tiles as "sprites" so that we don't have too many sprites on a line.
; It's pretty complex compared to its sprite variant, as it has to composite itself against the backdrop.


; How many tiles, including spanning across multiple pages, do we need?
.DEFINE ELEVATOR_ACTOR_BG_TOTAL_TILES_NEEDED_FOR_ALLOC   (ELEVATOR_HEIGHT_IN_TILES * ELEVATOR_WIDTH_IN_TILES * 2)

; Structure defining the Execute Buffer action when updating the nametable under the actor.
.STRUCT sActorElevatorBG_NameTablePayload
    UploadHeader        INSTANCEOF sAction_UploadVRAMList_Indirect_Header
    TopRowRunInfo       INSTANCEOF sAction_UploadVRAMList_Run
    TopRowData          INSTANCEOF sNameTableEntry ELEVATOR_WIDTH_IN_TILES
    MiddleRowRunInfo    INSTANCEOF sAction_UploadVRAMList_Run
    MiddleRowData       INSTANCEOF sNameTableEntry ELEVATOR_WIDTH_IN_TILES
    BottomRowRunInfo    INSTANCEOF sAction_UploadVRAMList_Run
    BottomRowData       INSTANCEOF sNameTableEntry ELEVATOR_WIDTH_IN_TILES
.ENDST

.STRUCT sActorElevatorBG
    ; The BG variant keeps common variables here.
    Base                        INSTANCEOF sActorElevator_Base

    ; Using a pageflip scheme, we can write to VRAM during active display to speed up compositing.
    ActivePage                  DB      ; 0 == first page, 1 == second page

    ; The Execute Buffer action used to invoke updating the nametable entries.
    ; This action is copied into the execute buffer for execution at VBLANK, 
    ; and points to its corresponding payload (below).
    ; The values differ based on which page is active.
    DestNameTableAction_Page0   INSTANCEOF sAction_UploadVRAMList_Indirect
    DestNameTableAction_Page1   INSTANCEOF sAction_UploadVRAMList_Indirect

    ; Pre-calculated nametable entries.  These are copied to the nametable during VBLANK.
    ; The values differ based on which page is active.
    DestNameTablePayload_Page0  INSTANCEOF sActorElevatorBG_NameTablePayload
    DestNameTablePayload_Page1  INSTANCEOF sActorElevatorBG_NameTablePayload

    ; Destination VRAM locs for composited tiles to go to.  These differ based on which page is active.
    DestTileVRAMLocPage0        DW
    DestTileVRAMLocPage1        DW

    ; Track the previous name table offset we *were* on.  If it changed, we have to update some dirty BG tiles.
    PreviousNameTableOffset     DW
.ENDST

; Structure used in level definitions for initialization.
.STRUCT sActorElevatorBG_Init
    Base                        INSTANCEOF sActorElevator_BaseInit

    ; What tile index should this actor use for blitting to?
    ; Range will be ELEVATOR_ACTOR_BG_TOTAL_TILES_NEEDED_FOR_ALLOC tiles long.
    StartingTileIndex           DW  ; 16-bit for 0..448
.ENDST


; Public Interface
ActorElevatorBG:

.DSTRUCT @ActorElevatorBGClassMethods INSTANCEOF sActorClassMethods VALUES
    OnActorAdded                .DW ActorElevator_Base@OnActorAdded     ; Uses base
.ENDST

.DSTRUCT @ActorElevatorBGMovingState INSTANCEOF sActorState VALUES
    Base.OnUpdate               .DW ActorElevator_Base@OnUpdate         ; Uses base
    OnRenderPrep                .DW _ActorElevatorBG@OnRenderPrep       ; Custom
    OnCollision                 .DW ActorElevator_Base@OnCollision      ; Uses base
.ENDST

;==============================================================================
; ActorElevatorBG@Init
; Sets initial values for the actor
; INPUTS:  DE: Pointer to sActorElevator memory
;          IX: Pointer to sActorElevatorBG_Init
; OUTPUTS:  None
; Destroys Nothing
;==============================================================================
@Init:
    ; Get our actor into IY
    push    de
    pop     iy

    ; Call base class
    call    ActorElevator_Base@Init

    ; Assign the methods
    ld      (iy + sActorElevatorBG.Base.ActorInstance.pActorClassMethods + 0), <ActorElevatorBG@ActorElevatorBGClassMethods
    ld      (iy + sActorElevatorBG.Base.ActorInstance.pActorClassMethods + 1), >ActorElevatorBG@ActorElevatorBGClassMethods

    ; Init our FSM
    ld      hl, @ActorElevatorBGMovingState
    call    FSM_IY@Init

    ; Get our base tile index.
    ld      l, (ix + sActorElevatorBG_Init.StartingTileIndex + 0)
    ld      h, (ix + sActorElevatorBG_Init.StartingTileIndex + 1)

    ; Calc VRAM locs for each page of tiles.
    push    hl
        ; Calc the VRAM loc for the destination tiles
        CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
        ld      (iy + sActorElevatorBG.DestTileVRAMLocPage0 + 0), l
        ld      (iy + sActorElevatorBG.DestTileVRAMLocPage0 + 1), h

        ; Second page is just past the first.
        push    de
            ld      de, ACTOR_ELEVATOR_TOTAL * _sizeof_sTile
            add     hl, de
        pop     de

        ld      (iy + sActorElevatorBG.DestTileVRAMLocPage1 + 0), l
        ld      (iy + sActorElevatorBG.DestTileVRAMLocPage1 + 1), h
    pop     hl

    ; Create actions for updating our nametable indices for each position, in each page.
    push    hl
        ; First, create the actions that will be copied into the exec buffer.  These are only a few bytes.
        ; PAGE 0
            ; The callback function.
            ld      (iy + sActorElevatorBG.DestNameTableAction_Page0.ExecuteEntry.CallbackFunction + 0), <Action_UploadVRAMList_Indirect
            ld      (iy + sActorElevatorBG.DestNameTableAction_Page0.ExecuteEntry.CallbackFunction + 1), >Action_UploadVRAMList_Indirect

            ; The header points to page 0 payload.
            push    hl
                push    iy
                pop     hl
                push    de
                    ld      de, sActorElevatorBG.DestNameTablePayload_Page0
                    add     hl, de
                    ld      (iy + sActorElevatorBG.DestNameTableAction_Page0.pHeader + 0), l
                    ld      (iy + sActorElevatorBG.DestNameTableAction_Page0.pHeader + 1), h
                pop de
            pop     hl

        ; PAGE 1
            ; The callback function.
            ld      (iy + sActorElevatorBG.DestNameTableAction_Page1.ExecuteEntry.CallbackFunction + 0), <Action_UploadVRAMList_Indirect
            ld      (iy + sActorElevatorBG.DestNameTableAction_Page1.ExecuteEntry.CallbackFunction + 1), >Action_UploadVRAMList_Indirect

            ; The header points to page 1 payload.
            push    hl
                push    iy
                pop     hl
                push    de
                    ld      de, sActorElevatorBG.DestNameTablePayload_Page1
                    add     hl, de
                    ld      (iy + sActorElevatorBG.DestNameTableAction_Page1.pHeader + 0), l
                    ld      (iy + sActorElevatorBG.DestNameTableAction_Page1.pHeader + 1), h
                pop de
            pop     hl

        ; Now create the payloads for each of the actions.
        ; HL holds our nametable value.

        ; TODO:  BG nametables should use the sprite palette.
        ;ld      a, h
        ;or      VDP_NAMETABLE_ENTRY_USE_SPRITE_PAL
        ;ld      h, a

        ; PAGE 0
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.UploadHeader + sAction_UploadVRAMList_Indirect_Header.NumRuns), ELEVATOR_HEIGHT_IN_TILES

            ; Now each run.
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.TopRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.MiddleRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.BottomRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry

            ; Our tile orders are arranged column-major, but we need them row-major for name table uploads.  So:
            ; Tile Enum     Name Table Position
            ; =========     ===================
            ;   UL (0)      Top, position 0
            ;   ML (1)      Middle, position 0
            ;   LL (2)      Bottom, position 0
            ;   UR (3)      Top, position 1
            ;   ML (4)      Middle, position 1
            ;   LR (5)      Bottom, position 1

            ; UL
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.TopRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.TopRowData.1 + 1), h
            inc     hl

            ; ML
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.MiddleRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.MiddleRowData.1 + 1), h
            inc     hl

            ; LL
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.BottomRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.BottomRowData.1 + 1), h
            inc     hl

            ; UR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.TopRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.TopRowData.2 + 1), h
            inc     hl

            ; MR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.MiddleRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.MiddleRowData.2 + 1), h
            inc     hl

            ; LR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.BottomRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page0.BottomRowData.2 + 1), h
            inc     hl

        ; PAGE 1
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.UploadHeader + sAction_UploadVRAMList_Indirect_Header.NumRuns), ELEVATOR_HEIGHT_IN_TILES

            ; Now each run.
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.TopRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.MiddleRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.BottomRowRunInfo.RunLengthInBytes), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry

            ; Our tile orders are arranged column-major, but we need them row-major for name table uploads.  So:
            ; Tile Enum     Name Table Position
            ; =========     ===================
            ;   UL (0)      Top, position 0
            ;   ML (1)      Middle, position 0
            ;   LL (2)      Bottom, position 0
            ;   UR (3)      Top, position 1
            ;   ML (4)      Middle, position 1
            ;   LR (5)      Bottom, position 1

            ; UL
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.TopRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.TopRowData.1 + 1), h
            inc     hl

            ; ML
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.MiddleRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.MiddleRowData.1 + 1), h
            inc     hl

            ; LL
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.BottomRowData.1 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.BottomRowData.1 + 1), h
            inc     hl

            ; UR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.TopRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.TopRowData.2 + 1), h
            inc     hl

            ; MR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.MiddleRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.MiddleRowData.2 + 1), h
            inc     hl

            ; LR
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.BottomRowData.2 + 0), l
            ld      (iy + sActorElevatorBG.DestNameTablePayload_Page1.BottomRowData.2 + 1), h
            inc     hl

    pop     hl

    ; Set the active page.
    ld      (iy + sActorElevatorBG.ActivePage), $00

    ret

.MACRO ACTOR_ELEVATOR_GET_BG_ENTRY ARGS X_OFFSET, Y_OFFSET
    ; Do the BG name table loc.
    ld      hl, (gActorElevator_Globals.TempNameTableOffset)
    ld      bc, gModeGameScreen.NameTableBuffer + (X_OFFSET * _sizeof_sNameTableEntry) + (Y_OFFSET * VDP_NAMETABLE_ROWSIZE_IN_BYTES)
    add     hl, bc      ; HL = Ptr to BG Name table entry
.ENDM

; Gets a pointer to the tile data at the BG entry indicated.
.MACRO ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY ARGS X_OFFSET, Y_OFFSET
    ; Find loc in BG table
    ACTOR_ELEVATOR_GET_BG_ENTRY X_OFFSET, Y_OFFSET

    ; HL is the nametable position, now get the value stored there.
    ld      e, (hl)
    inc     hl
    ld      d, (hl)     ; DE holds the nametable value for this position

    ld      hl, gTileManager.TileProvenanceTable
    .REPT _sizeof_sTileProvenance
        add     hl, de
    .ENDR

    ; Get the pointer to the data.
    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a
.ENDM

; Composites one row of tile data.  DE holds the Bottom tile data,
; HL holds pointer to interleaved data for the *inverted* mask and Top
; tile data.  See Tile_CompositeInterleavedTileData_ToVRAM_VRAMPTRSet
; for reference.
.MACRO COMPOSITE_ONE_ROW
    ; Get mask data.
    ld      b, (hl)
    inc     hl
.REPT 4
    ld      a, (de)             ; Get bottom tile
    and     b                   ; Mask it
    or      (hl)                ; Composite top tile
    out     (VDP_DATA_PORT), a  ; Out!
    inc     hl                  ; Advance mask/top ptr
    inc     de
.ENDR
.ENDM

_ActorElevatorBG:
;==============================================================================
; _ActorElevatorBG@OnRenderPrep
; Callback when this actor needs to prepare for rendering.  This includes
; updating any graphics or sprites.
; INPUTS:  DE: Pointer to sActorElevator struct
; OUTPUTS:  None
; Destroys EVERYTHING.
;==============================================================================
@OnRenderPrep:
    push    de
    pop     iy

@@UpdateActivePage:
    ; Get the correct values for which page we're on.
    ld      a, (iy + sActorElevatorBG.ActivePage)
    and     a
    jp      nz, @@@GetPage1Vals
    ; Otherwise, we're page 0.
@@@GetPage0Vals:
    push    iy
    pop     hl
    ld      bc, sActorElevatorBG.DestNameTableAction_Page0
    add     hl, bc      ; HL points to the action.

    ; Hold onto the delta between the action and its payload.
    ld      bc, sActorElevatorBG.DestNameTablePayload_Page0 - sActorElevatorBG.DestNameTableAction_Page0

    ; DE holds the dest VRAM loc
    ld      e, (iy + sActorElevatorBG.DestTileVRAMLocPage0 + 0)
    ld      d, (iy + sActorElevatorBG.DestTileVRAMLocPage0 + 1)
    jp      @@@SetPageVals
@@@GetPage1Vals:
    push    iy
    pop     hl
    ld      bc, sActorElevatorBG.DestNameTableAction_Page1
    add     hl, bc      ; HL points to the action.

    ; Hold onto the delta between the action and its payload.
    ld      bc, sActorElevatorBG.DestNameTablePayload_Page1 - sActorElevatorBG.DestNameTableAction_Page1

    ; DE holds the dest VRAM loc
    ld      e, (iy + sActorElevatorBG.DestTileVRAMLocPage1 + 0)
    ld      d, (iy + sActorElevatorBG.DestTileVRAMLocPage1 + 1)
@@@SetPageVals:
    ld      (gActorElevator_Globals.TempDestTileVRAMLoc), de
    ld      (gActorElevator_Globals.TempDestNameTableAction), hl
    add     hl, bc  ; Offset to payload
    ld      (gActorElevator_Globals.TempDestNameTablePayload), hl

    ; Swap page
    xor     $01
    ld      (iy + sActorElevatorBG.ActivePage), a
@@@Done:

@@FindNameTablePos:
    ; Figure out our map position
    ; ROW = (YPos / 8)
    ; COL = (XPos / 8)
    ; We have 32 columns per row.
    ; Each column is 2 bytes, meaning each row is 32 * 2 == 64 bytes.
    ; MapPos = (ROW * 24 * 2) + (COL * 2)
    ld      a, (iy + sActorElevatorBG.Base.YPos)
    ld      c, a            ; Hang onto the YPos for the next section to save another load.
    and     $F8             ; A = ROW * 8
    ld      l, a
    ld      h, $0
    add     hl, hl          ; HL = ROW * 16
    add     hl, hl          ; HL = ROW * 32
    add     hl, hl          ; HL = ROW * 64

    ; Now column offset.
    ld      a, (iy + sActorElevatorBG.Base.XPos)
    and     $F8             ; A = COL * 8
    srl     a
    srl     a               ; A = COL * 2
    ld      e, a
    ld      d, $0
    add     hl, de          ; HL = Offset into map

    ; Cache our Name Table offset as we'll be using that a lot, too.
    ld      (gActorElevator_Globals.TempNameTableOffset), hl
@@@Done:

@@Composite:
    ; Pre-calc our Y offset as we'll be using this a lot.
    ; In the previous section, we stored the YPos in C.  Get that now.
    ld      a, c
    and     $7      ; Mask out the pixel offset
    ld      (gActorElevator_Globals.TempYPixelOffset), a

    ; Prep VRAM based on whichever page we're targeting.
    ld      hl, (gActorElevator_Globals.TempDestTileVRAMLoc)
    SET_VRAM_WRITE_LOC_FROM_HL

@@@LeftSide:
@@@@AssembleTiles:
    ; Identify all of the BG tiles underneath of us.
    
    ; UL
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 0, 0
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Upper), hl

    ; ML
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 0, 1
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Middle), hl

    ; LL
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 0, 2
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Lower), hl
@@@@@Done:
@@@@DoComposition:
    ; Set the out port.
    ld      c, VDP_DATA_PORT

    ; Load our interleaved mask & gfx data.
    ld      de, SpyMaskAndGraphic_Data@UL

    ; Figure out where to go based on our Y position offset.
    ld      a, (gActorElevator_Globals.TempYPixelOffset)
    ADD_2A_TO_PTR @JumpTable

    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a

    call    CallHL
@@@@Done:

@@@RightSide:
@@@@AssembleTiles:
    ; Identify all of the BG tiles underneath of us.
    
    ; UR
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 1, 0
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Upper), hl

    ; MR
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 1, 1
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Middle), hl

    ; LR
    ACTOR_ELEVATOR_GET_TILE_PTR_FOR_BG_ENTRY 1, 2
    ld      (gActorElevator_Globals.TempPtrToSrcBGTile_Lower), hl
@@@@@Done:
@@@@DoComposition:
    ; Set the out port.
    ld      c, VDP_DATA_PORT

    ; Load our interleaved mask & gfx data.
    ld      de, SpyMaskAndGraphic_Data@UR

    ; Figure out where to go based on our Y position offset.
    ld      a, (gActorElevator_Globals.TempYPixelOffset)
    ADD_2A_TO_PTR @JumpTable

    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a

    call    CallHL
@@@@Done:

    ; **** END COMPOSITION ****
@@@Done:

@@CheckDirty:
    ; Did we move off of a tile boundary?  If so, we need to fix the dirty BG tiles, so include them in the exec buffer.
    ld      hl, (gActorElevator_Globals.TempNameTableOffset)
    ld      e, (iy + sActorElevatorBG.PreviousNameTableOffset + 0)
    ld      d, (iy + sActorElevatorBG.PreviousNameTableOffset + 1)
    and     a
    sbc     hl, de
    jp      z, @@@Done  ; If the same, then skip.

@@@Dirty:
    ex      af, af'  ; Preserve our sign.
        ; Reserve space in the Exec Buffer.
        ld      a', 1        ; #/runs.
        ; How much space do we need?
        ;                  Size of Execute Buffer Command     Size of each Run's header                  Size of data
        ld      bc, _sizeof_sAction_UploadVRAMList_Implicit + (_sizeof_sAction_UploadVRAMList_Run * 1) + (_sizeof_sNameTableEntry * 1 * ELEVATOR_WIDTH_IN_TILES)
        call    @@CreateNameTableExecBuffer

        ; HL points to start of buffer area.
        ex      de, hl  ; Now DE does.
    ex      af, af'  ; Restore our sign.  Are we dirty on top, or on bottom?

    ; Assume we're finding the nametable data for the row ABOVE ours.
    ld      bc, -1 * VDP_NAMETABLE_ROWSIZE_IN_BYTES
    jp      nc, @@@SubmitDirty

    ; Otherwise, it's the row beneath us.
    ld      bc, ELEVATOR_HEIGHT_IN_TILES * VDP_NAMETABLE_ROWSIZE_IN_BYTES

@@@SubmitDirty:
    ld      hl, (gActorElevator_Globals.TempNameTableOffset)
    add     hl, bc      ; HL points to offset for the row specified

    push    hl          ; Hang onto the offset.
        ; Set VRAM loc
        ld      bc, VDP_NAMETABLE_START_LOC
        add     hl, bc
        ex      de, hl
        ld      (hl), e
        inc     hl
        ld      (hl), d
        inc     hl

        ; Run length...
        ld      (hl), ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
        inc     hl

        ex      de, hl
    pop     hl  ; Get our offset again.

    ; Now get the dirty tiles with that same offset.
    ld      bc, gModeGameScreen.NameTableBuffer
    add     hl, bc      ; HL points to map's name table buffer.

.REPT ELEVATOR_WIDTH_IN_TILES * _sizeof_sNameTableEntry
    ldi                 ; Copy the source nametable data into our exec buffer.
.ENDR

@@@Done:


@@UpdateNameTableForNewBG:
    ; Now update the nametable for our NEW tile IDs.
    ; We'll have an execute buffer action that points to the action pre-defined inside this actor, based on whichever page we're on.
    ld      de, (gActorElevator_Globals.TempDestNameTableAction)
    ld      bc, _sizeof_sAction_UploadVRAMList_Indirect
    ld      ix, gModeGameScreen.VBlankExecuteBuffer

    call    ExecuteBuffer_AttemptEnqueue_IX
    jp      nc, @@@Success

    ; Failed
    halt        ; TODO

@@@Success:
    ; Fill out the destination VRAM locations for each run.
    ld      hl, VDP_NAMETABLE_START_LOC
    ld      bc, (gActorElevator_Globals.TempNameTableOffset)
    ld      (iy + sActorElevatorBG.PreviousNameTableOffset + 0), c    ; Update prev.
    ld      (iy + sActorElevatorBG.PreviousNameTableOffset + 1), b

    add     hl, bc  ; HL = VRAM loc for top row (NAMETABLE START + Offset)

    ; Figure out which nametable entries to use, based on our active page.
    ld      ix, (gActorElevator_Globals.TempDestNameTablePayload)

    ; Hold value for one VRAM row
    ld      bc, VDP_NAMETABLE_ROWSIZE_IN_BYTES  ; Add one row

    ; Top row VRAM loc
    ld      (ix + sActorElevatorBG_NameTablePayload.TopRowRunInfo.VRAMLoc + 0), l
    ld      (ix + sActorElevatorBG_NameTablePayload.TopRowRunInfo.VRAMLoc + 1), h

    ; Middle row VRAM loc
    add     hl, bc
    ld      (ix + sActorElevatorBG_NameTablePayload.MiddleRowRunInfo.VRAMLoc + 0), l
    ld      (ix + sActorElevatorBG_NameTablePayload.MiddleRowRunInfo.VRAMLoc + 1), h

    ; Bottom row VRAM loc
    add     hl, bc
    ld      (ix + sActorElevatorBG_NameTablePayload.BottomRowRunInfo.VRAMLoc + 0), l
    ld      (ix + sActorElevatorBG_NameTablePayload.BottomRowRunInfo.VRAMLoc + 1), h
@@@Done:

@@Done:
    and     a   ; Stay in same state
    ret

;==============================================================================
; ActorElevator@OnRenderPrep@CreateNameTableExecBuffer
; Creates an exec buffer entry to update nametable entries.
; INPUTS:  A: #/runs to add
;          BC: Size of exec buffer to alloc
; OUTPUTS: HL: Start of run data. 
; Destroys HL, DE, IX
;==============================================================================
@@CreateNameTableExecBuffer:
    ; Request space in the execute buffer.
    ld      ix, gModeGameScreen.VBlankExecuteBuffer

    call    ExecuteBuffer_AttemptReserve_IX
    jr      c, @@Fail

    ; DE points to the top.  Fill out the struct.
    ex      de, hl
    ld      (hl), <Action_UploadVRAMList_Implicit   ; sAction_UploadVRAMList_Implicit.ExecuteEntry.CallbackFunction + 0
    inc     hl
    ld      (hl), >Action_UploadVRAMList_Implicit   ; sAction_UploadVRAMList_Implicit.ExecuteEntry.CallbackFunction + 1
    inc     hl
    ld      (hl), a ; #/Runs
    inc     hl
@@@Done:
    ret

@@Fail:
    ; TODO FAIL
    halt

@JumpTable:
    .DW @@CompositeUnroll_0_8
    .DW @@CompositeUnroll_1_7
    .DW @@CompositeUnroll_2_6
    .DW @@CompositeUnroll_3_5
    .DW @@CompositeUnroll_4_4
    .DW @@CompositeUnroll_5_3
    .DW @@CompositeUnroll_6_2
    .DW @@CompositeUnroll_7_1

; Zero lines of original, 8 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_0_8:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; No original lines to copy over.

    ; Just composite all 8 lines.
    ex      de, hl
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; No composition.  Copy straight.
    ld      b, 8 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 1 line of original, 7 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_1_7:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; One line of original to copy over.
    ld      b, 1 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite 7 lines.
    ex      de, hl
.REPT 7
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite one line.
    COMPOSITE_ONE_ROW

    ; Copy the rest.
    ex      de, hl

    ld      b, 7 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 2 lines of original, 6 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_2_6:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 2 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 6
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 2
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 6 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 3 lines of original, 5 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_3_5:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 3 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 5
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 3
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 5 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 4 lines of original, 4 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_4_4:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 4 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 4
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 4
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 4 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 5 lines of original, 3 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_5_3:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 5 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 3
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 5
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 3 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 6 lines of original, 2 lines of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_6_2:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 6 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 2
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 6
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 2 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

; 7 lines of original, 1 line of composite.
; DE = Ptr to Interleaved Data (Mask + Gfx) for Top tile
@@CompositeUnroll_7_1:
@@@Upper:
    ; Get top Bottom tile.
    ld      hl, (gActorElevator_Globals.TempPtrToSrcBGTile_Upper)

    ; Lines of original to copy over.
    ld      b, 7 * 4    ; 4bpp
-:
    outi
    jp      nz, -

    ; Composite the remaining lines.
    ex      de, hl
.REPT 1
    COMPOSITE_ONE_ROW
.ENDR

@@@Middle:
    ; Get middle Bottom tile.
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Middle)

    ; Composite
.REPT 8
    COMPOSITE_ONE_ROW
.ENDR

@@@Lower:
    ; Get lower Bottom tile
    ld      de, (gActorElevator_Globals.TempPtrToSrcBGTile_Lower)

    ; Composite the rest of the Top tile
.REPT 7
    COMPOSITE_ONE_ROW
.ENDR

    ; Copy the rest of the bottom tile.
    ex      de, hl

    ld      b, 1 * 4    ; 4bpp
-:
    outi
    jp      nz, -
@@@Done:
    ret

.ENDS

.RAMSECTION "Actor - Elevator Global RAM" SLOT 3
    gActorElevator_Globals INSTANCEOF sActorElevator_Globals
.ENDS


.ENDIF  ;__ACTOR_ELEVATOR_ASM__