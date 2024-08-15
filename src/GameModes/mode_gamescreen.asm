.INCLUDE "Utils/fsm.asm"
.INCLUDE "Utils/vdp.asm"
.INCLUDE "Utils/tile_routines.asm"
.INCLUDE "Utils/spritechain.asm"
.INCLUDE "Managers/gamemanager.asm"
.INCLUDE "Managers/actormanager.asm"
.INCLUDE "Managers/spritemanager.asm"
.INCLUDE "Managers/tilemanager.asm"
.INCLUDE "Actors/actor_elevator.asm"
.INCLUDE "Actors/actor_player.asm"

.INCLUDE "Modules/execute_buffer.asm"

.INCLUDE "../data/fonts/default_font.asm"
.INCLUDE "../data/game graphics/game_gfx.asm"
.INCLUDE "../data/maps/map_round1.asm"




.SECTION "Mode - Game Screen" FREE

; We want to keep a cap of how many sprites are used by player and enemy.
.DEFINE GAME_SPRITES_MAX_PLAYER_SPRITES 8
; Enemy sprites are 2x2.
.DEFINE GAME_SPRITES_MAX_ENEMY_SPRITES  MAX_ELEVATORS_SPRITES * 2 * 2

; Dimensions of the play area (subset of the whole screen)
.DEFINE GAME_MAP_NUM_COLS   24
.DEFINE GAME_MAP_NUM_ROWS   24


.STRUCT sGameScreen
    ; FSM governing the screen's state transitions.
    ScreenFSM INSTANCEOF  sFSM

    ; Independent variables unique to each state
    ; TODO

    PlayerSpriteChain INSTANCEOF sSpriteChainHeader
    EnemySpriteChain INSTANCEOF sSpriteChainHeader

    PlayerChainYPos DSB GAME_SPRITES_MAX_PLAYER_SPRITES
    PlayerChainXPosTilePos DSW GAME_SPRITES_MAX_PLAYER_SPRITES

    EnemyChainYPos DSB GAME_SPRITES_MAX_ENEMY_SPRITES
    EnemyChainXPosTilePos DSW GAME_SPRITES_MAX_ENEMY_SPRITES

    NameTableBuffer INSTANCEOF sNameTableEntry VDP_NAMETABLE_NUMVISIBLEROWS * VDP_NAMETABLE_NUMCOLS

    VBlankExecuteBuffer INSTANCEOF sExecuteBufferDescriptor
    VBlankExecuteBuffer_Memory DSB 2048

.ENDST

; This screen has additional functions for its FSM.
.STRUCT sModeGameScreen_State
    ScreenState INSTANCEOF sState
    OnRenderPrep    DW              ; Capture changes for the rendering.
.ENDST

; Public
ModeGameScreen:

.DSTRUCT @GameScreenAppMode INSTANCEOF sApplicationMode VALUES:
    VideoInterruptJumpTarget    .DW _ModeGameScreen@InterruptHandlerNotReadyForVBL    ; Called when a video interrupt (V/HBlank) occurs.
    OnActive                    .DW _ModeGameScreen@OnActive            ; Called when this mode is made active (pushed, old one above popped, etc.)
    OnUpdate                    .DW _ModeGameScreen@OnUpdate            ; Called when the application wants to update.

    OnRenderPrep                .DW _ModeGameScreen@OnRenderPrep        ; Called when the application is prepping things for render.
    OnNMI                       .DW _ModeGameScreen@DoNothing           ; Called when a non-maskable interrupt (NMI) comes in.
    OnInactive                  .DW _ModeGameScreen@DoNothing           ; Called when this mode goes inactive (popped, new mode pushed on, etc.)
    OnEvent                     .DW _ModeGameScreen@DoNothing           ; Called when a generic event occurs.
.ENDST

; Private
_ModeGameScreen:

@DoNothing:
    ret

@OnActive:
    ; If we're replacing what was on before us, do the full init.  Otherwise don't do anything.
    cp      MODE_MADE_ACTIVE
    ret     nz

    ; Do the full monty.

    ; Turn off the display and interrupts while we do graphics things.
    di
    ; Turn off the display & VBlanks by OR'ing to the current value.
    ld      a, (gVDPManager.Registers.VideoModeControl2)
    and     $FF ~(VDP_REGISTER1_ENABLE_DISPLAY | VDP_REGISTER1_ENABLE_VBLANK)
    ld      e, VDP_COMMMAND_MASK_REGISTER1
    call    VDPManager_WriteRegisterImmediate

    ; Start our state machine for this screen.
    ld      ix, gModeGameScreen.ScreenFSM
    ld      hl, ModeGameScreen_FullInitState
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
    ld      de, gModeGameScreen.ScreenFSM
    call    FSM_DE@OnUpdate

    ret

@OnRenderPrep:
    ; Clear our execute buffer
    ld      iy, gModeGameScreen.VBlankExecuteBuffer
    call    ExecuteBuffer_Reset

    ; Clear our Sprite Manager
    call    SpriteManager_Clear

    ; Have the current state prepare to render.
    ld      de, gModeGameScreen.ScreenFSM
    ld      bc, sModeGameScreen_State.OnRenderPrep
    call    FSM_DE@OnEvent

    ; Now we're ready for VBL
    ld      hl, _ModeGameScreen@InterruptHandlerReadyForVBL
    ld      (gModeManager.CurrVideoInterruptJumpTarget), hl

    ret

; Called when we *ARE* ready for a VBL.
@InterruptHandlerReadyForVBL:
    PUSH_ALL_REGS
        in      a, (VDP_STATUS_PORT)                ; Satisfy the interrupt

        ; Upload the sprites
        call SpriteManager_UploadToVRAM

        ; Execute the execute buffer
        ld      iy, gModeGameScreen.VBlankExecuteBuffer
        call    ExecuteBuffer_Execute

        ld      hl, _ModeGameScreen@InterruptHandlerNotReadyForVBL
        ld      (gModeManager.CurrVideoInterruptJumpTarget), hl
    POP_ALL_REGS
    ret

; Called when we're NOT ready for a VBL
@InterruptHandlerNotReadyForVBL:
    push    af
        in      a, (VDP_STATUS_PORT)                ; Satisfy the interrupt
    pop     af
    ret
.ENDS

.RAMSECTION "Mode - Game Screen Context" SLOT 3
    gModeGameScreen INSTANCEOF sGameScreen
.ENDS

.SECTION "Mode - Game Screen Full Init State" FREE

.DSTRUCT ModeGameScreen_FullInitState INSTANCEOF sModeGameScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _FullInitState@OnEnter
    ScreenState.OnUpdate        .DW _FullInitState@OnUpdate
.ENDST



_FullInitState:

@OnEnter:
    ; Clear the nametable
    call ClearNameTable

    ; Enable the status bar on the right.
    ld      a, (gVDPManager.Registers.VideoModeControl1)
    or      VDP_REGISTER0_DISABLE_VERTICAL_STATUS_SCROLL
    ld      e, VDP_COMMMAND_MASK_REGISTER0
    call    VDPManager_WriteRegisterImmediate

@@ExecuteBuffer:
    ld      iy, gModeGameScreen.VBlankExecuteBuffer
    ld      de, gModeGameScreen.VBlankExecuteBuffer_Memory          ; Loc of buffer
    ld      bc, _sizeof_gModeGameScreen.VBlankExecuteBuffer_Memory  ; How big it is.
    call    ExecuteBuffer_Init

@@ActorManager:
    call    ActorManager@Init

@@SpriteSetup:
    push    ix
        ; Create each chain first.
        ; Player Sprite Chain
        ld      ix, gModeGameScreen.PlayerSpriteChain       ; Ptr to header
        ld      a, GAME_SPRITES_MAX_PLAYER_SPRITES          ; Max Capacity
        ld      bc, gModeGameScreen.EnemySpriteChain        ; Ptr to Next
        ld      de, gModeGameScreen.PlayerChainYPos         ; Ptr to Y Table
        ld      hl, gModeGameScreen.PlayerChainXPosTilePos  ; Ptr to XPos/Tile Table
        call    SpriteChain_Init

        ; Enemy Sprite Chain
        ld      ix, gModeGameScreen.EnemySpriteChain        ; Ptr to header
        ld      a, GAME_SPRITES_MAX_ENEMY_SPRITES           ; Max Capacity
        ld      bc, $0000                                   ; Ptr to Next
        ld      de, gModeGameScreen.EnemyChainYPos          ; Ptr to Y Table
        ld      hl, gModeGameScreen.EnemyChainXPosTilePos   ; Ptr to XPos/Tile Table
        call    SpriteChain_Init

        ; Now init the manager.
        ld      hl, gModeGameScreen.PlayerSpriteChain       ; First chain element.
        call    SpriteManager_Init
    pop     ix
@@UploadTiles:
    call    TileManager_Init

    ; Load the BG graphics
    ld      iy, Map_Round1@TileSets@LevelSet_4bpp_TileSet
    call    TileManager_LoadTileSet

    ; For the BG graphics, we want the provenance as well.
    ld      iy, Map_Round1@TileSets@LevelSet_4bpp_TileSet
    call    TileManager_LoadTileSetProvenance

    ; Load the font
    ld      iy, Map_Round1@TileSets@DefaultFont_TileSet
    call    TileManager_LoadTileSet

    ; Load the elevator sprites
    ld      iy, Map_Round1@TileSets@ElevatorSprites_4bpp_TileSet
    call    TileManager_LoadTileSet

    ; And include provenance.
    ld      iy, Map_Round1@TileSets@ElevatorSprites_4bpp_TileSet
    call    TileManager_LoadTileSetProvenance

@@LoadMap:
    ; Load the appropriate map.
    push    ix
        ; Fill the nametable buffer with zeroes first.
        ld      hl, gModeGameScreen.NameTableBuffer
        ld      bc, _sizeof_gModeGameScreen.NameTableBuffer
        ld      d, $0
-:
        ld      (hl), d
        inc     hl
        dec     bc
        ld      a, b
        or      c
        jp      nz, -

        ; Now the load the actual map.  Note that it fills only a SUBSET of the NameTableBuffer.
        ld      hl, Map_Round1@MapData
        ld      de, gModeGameScreen.NameTableBuffer

        ld      c, GAME_MAP_NUM_ROWS
@@@NewRow:
        ld      b, GAME_MAP_NUM_COLS
@@@NextMapColumn:
        push    bc
            ; Get next key.
            ld      a, (hl)

            ; Scan for it in the dictionary.
            ld      b, MAP_ROUND1_NUM_ENTRIES
            ld      ix, Map_Round1@MapDictionary

-:
            cp      (ix + sMapDictionaryEntry.Key)
            jr      z, @@@FoundIt
            
            .REPT   _sizeof_sMapDictionaryEntry
                inc     ix
            .ENDR

            djnz    -
@@@Error:
            halt

@@@FoundIt:
            ld      a, (ix + sMapDictionaryEntry.Value + 0)
            ld      (de), a
            inc     de
            ld      a, (ix + sMapDictionaryEntry.Value + 1)
            ld      (de), a
            inc     de
        pop     bc

        ; Move to next entry in map data.
        inc     hl

        ; Loop, as appropriate for current row.
        djnz    @@@NextMapColumn

        ; At the end of the row.  Are we out of rows?
        dec     c
        jr      z, @@@MapLoadDone

        ; Move to the next row.  It's the difference between the number of colums in the name table - the size of the map.
        push    hl
            ex  de, hl
            ld  de, (VDP_NAMETABLE_NUMCOLS - GAME_MAP_NUM_COLS) * _sizeof_sNameTableEntry
            add hl, de
            ex  de, hl
        pop     hl
        jp      @@@NewRow
@@@MapLoadDone:
    pop ix

@@UploadNameTable:
    ; Upload the contents of our nametable buffer to the nametable in VRAM.
    ld      bc, _sizeof_gModeGameScreen.NameTableBuffer
    ld      hl, gModeGameScreen.NameTableBuffer
    ld      de, VDP_NAMETABLE_START_LOC

    call    VDP_UploadDataToVRAMLoc

@@UploadPalette:
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

@@InitActors:
    ; Init our elevator actors.  Start with the BG ones first.
@@@BG_Elevators:
    ld      a, (Map_Round1@Elevators@BG_Elevators@Count)
    ld      (gGameManager.NumElevatorsBG), a
    and     a
    jr      z, @@@@Done

    ld      b, a            ; Count of BG elevators.
    ld      de, gGameManager.ElevatorsBG ; Actor memory.

    push    ix
        ld      ix, Map_Round1@Elevators@BG_Elevators@Definitions   ; Start at first one.
-:
        push    bc
            ; Call the constructor
            call    ActorElevatorBG@Init

            ; Add the actor to the scene
            call    ActorManager@AddActor

            ; Move to the next actor in the sequence.
            ex      de, hl
            ld      de, _sizeof_sActorElevatorBG
            add     hl, de      ; HL points to next actor in memory

            ; And next one from the level definition.
            ld      de, _sizeof_sActorElevatorBG_Init
            add     ix, de

            ex      de, hl      ; DE points to next actor in memory
        pop     bc
        djnz    -
    pop     ix
@@@@Done:

@@@Sprite_Elevators:
    ; Now the sprite-based elevators.
    ld      a, (Map_Round1@Elevators@Sprite_Elevators@Count)
    ld      (gGameManager.NumElevatorsSprites), a
    and     a
    jr      z, @@@@Done

    ld      b, a                    ; Count of Sprite elevators.
    ld      de, gGameManager.ElevatorsSprites    ; Actor memory.

    push    ix
        ld      ix, Map_Round1@Elevators@Sprite_Elevators@Definitions   ; Start at first one.
-:
        push    bc
            ; Call the constructor
            call    ActorElevatorSprite@Init

            ; Add the actor to the scene
            call    ActorManager@AddActor

            ; Move to the next actor in the sequence.
            ex      de, hl
            ld      de, _sizeof_sActorElevatorSprite
            add     hl, de      ; HL points to next actor in memory

            ; And next one from the level definition.
            ld      de, _sizeof_sActorElevatorSprite_Init
            add     ix, de

            ex      de, hl      ; DE points to next actor in memory
        pop     bc
        djnz    -
    pop     ix
@@@@Done:
@@Player:
    push    ix
        ld      ix, Map_Round1@Player@Definitions

        ld      de, gGameManager.PlayerActor

        ; Call the constructor.
        call    ActorPlayer@Init

        ; Add the actor to the scene.
        call    ActorManager@AddActor
    pop     ix
@@@Done:

@@Done:
    ; Stay in the same state UNTIL we have gone through a render phase
    and     a
    ret

@OnUpdate:
    ; Move to next state.
    ld      hl, ModeGameScreen_GameplayState
    scf
    ret

@Palette:
; BG Palette Entry 0 == black
.db VDP_PALETTE_BG_PALETTE_INDEX + 0, $00
; BG Palette Entry 1 == white
.db VDP_PALETTE_BG_PALETTE_INDEX + 1, (3 << VDP_PALETTE_RED_SHIFT) | (3 << VDP_PALETTE_GREEN_SHIFT) | (3 << VDP_PALETTE_BLUE_SHIFT)
; BG Palette Entry 2 == blue
.db VDP_PALETTE_BG_PALETTE_INDEX + 2, (3 << VDP_PALETTE_BLUE_SHIFT)
; BG Palette Entry 3 == red
.db VDP_PALETTE_BG_PALETTE_INDEX + 3, (3 << VDP_PALETTE_RED_SHIFT)

; Sprite Palette Entry 0 == black
.db VDP_PALETTE_SPRITE_PALETTE_INDEX + 0, $00
; Sprite Palette Entry 1 == white
.db VDP_PALETTE_SPRITE_PALETTE_INDEX + 1, (3 << VDP_PALETTE_RED_SHIFT) | (3 << VDP_PALETTE_GREEN_SHIFT) | (3 << VDP_PALETTE_BLUE_SHIFT)
@@End:
.ENDS


.SECTION "Mode - Game Screen Gameplay State" FREE

;******************************************************************************
; Gameplay State:  Executes the runtime gameplay.
;******************************************************************************:

.DSTRUCT ModeGameScreen_GameplayState INSTANCEOF sModeGameScreen_State VALUES
    ; Default State handlers
    ScreenState.OnEnter         .DW _GameplayState@OnEnter
    ScreenState.OnUpdate        .DW _GameplayState@OnUpdate

    ; Functions unique to this screen state
    OnRenderPrep                .DW _GameplayState@OnRenderPrep
.ENDST

; Private
_GameplayState:

@OnEnter:
    ; Stay in same state.
    and     a
    ret

@OnUpdate:
    ; Query input
    call    InputManager_OnUpdate

    ; Update actors
    ld      bc, sActorState.Base.OnUpdate
    call    ActorManager@CallMethodOnList

    ; Stay in same state.
    and     a
    ret

@OnRenderPrep:
    ld      bc, sActorState.OnRenderPrep
    call    ActorManager@CallMethodOnList

    ; Stay in same state.
    and     a
    ret

.ENDS