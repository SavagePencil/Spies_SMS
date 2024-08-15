.IFNDEF __MAP_ROUND1_ASM__
.DEFINE __MAP_ROUND1_ASM__

.INCLUDE "Modules/tileset.asm"
.INCLUDE "Actors/actor_elevator.asm"

.SECTION "Map - Round 1" FREE

.ENUMID $00
; Which tile is empty space?
.ENUMID GFX_TILE_INDEX_EMPTY

.ENUMID $20
; Where does the font begin?
.ENUMID GFX_TILE_INDEX_FONT

.ENUMID $80
; Which tile index is the floor?
.ENUMID GFX_TILE_INDEX_FLOOR

; Tile indices for empty elevator left & rights (not mirrored)
.ENUMID GFX_TILE_INDEX_ELEVATOR_LEFT
.ENUMID GFX_TILE_INDEX_ELEVATOR_RIGHT

; Tile indices for screen edge left & rights (not mirrored)
.ENUMID GFX_TILE_INDEX_EDGE_LEFT
.ENUMID GFX_TILE_INDEX_EDGE_LEFT_FLOOR
.ENUMID GFX_TILE_INDEX_EDGE_RIGHT
.ENUMID GFX_TILE_INDEX_EDGE_RIGHT_FLOOR

; Public
Map_Round1:

.STRUCT sMapDictionaryEntry
    Key     DB
    Value   DW
.ENDST

.DEFINE MAP_ROUND1_NUM_ENTRIES  8

.STRUCT sMapDictionary
    Entries INSTANCEOF sMapDictionaryEntry MAP_ROUND1_NUM_ENTRIES
.ENDST

.DSTRUCT @MapDictionary INSTANCEOF sMapDictionary VALUES
    Entries.1.Key:      .DB " "
    Entries.1.Value:    .DW GFX_TILE_INDEX_EMPTY

    Entries.2.Key:      .DB "_"
    Entries.2.Value:    .DW GFX_TILE_INDEX_FLOOR

    Entries.3.Key:      .DB "L"
    Entries.3.Value:    .DW GFX_TILE_INDEX_ELEVATOR_LEFT

    Entries.4.Key:      .DB "R"
    Entries.4.Value:    .DW GFX_TILE_INDEX_ELEVATOR_RIGHT

    Entries.5.Key:      .DB "!"
    Entries.5.Value:    .DW GFX_TILE_INDEX_EDGE_LEFT

    Entries.6.Key:      .DB "|"
    Entries.6.Value:    .DW GFX_TILE_INDEX_EDGE_RIGHT

    Entries.7.Key:      .DB "l"
    Entries.7.Value:    .DW GFX_TILE_INDEX_EDGE_LEFT_FLOOR

    Entries.8.Key:      .DB "r"
    Entries.8.Value:    .DW GFX_TILE_INDEX_EDGE_RIGHT_FLOOR
.ENDST

@MapData:
;    000000000011111111112222
;    012345678901234567890123
.DB "! LR LR LR LR LR LR LR |"  ;00
.DB "! LR LR LR LR LR LR LR |"  ;01
.DB "! LR LR LR LR LR LR LR |"  ;02
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;03
.DB "! LR LR LR LR LR LR LR |"  ;04
.DB "! LR LR LR LR LR LR LR |"  ;05
.DB "! LR LR LR LR LR LR LR |"  ;06
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;07
.DB "! LR LR LR LR LR LR LR |"  ;08
.DB "! LR LR LR LR LR LR LR |"  ;09
.DB "! LR LR LR LR LR LR LR |"  ;10
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;11
.DB "! LR LR LR LR LR LR LR |"  ;12
.DB "! LR LR LR LR LR LR LR |"  ;13
.DB "! LR LR LR LR LR LR LR |"  ;14
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;15
.DB "! LR LR LR LR LR LR LR |"  ;16
.DB "! LR LR LR LR LR LR LR |"  ;17
.DB "! LR LR LR LR LR LR LR |"  ;18
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;19
.DB "! LR LR LR LR LR LR LR |"  ;20
.DB "! LR LR LR LR LR LR LR |"  ;21
.DB "! LR LR LR LR LR LR LR |"  ;22
.DB "l_LR_LR_LR_LR_LR_LR_LR_r"  ;23
@@End

@Floor_Count:
.DB 5

@Floors:
.DB 3, 7, 11, 15, 19, 23

@Elevators:
@@BG_Elevators:
@@@Count:
    .DB 3
@@@Definitions:
.DSTRUCT @@@@BG_1 INSTANCEOF sActorElevatorBG_Init VALUES
    Base.InitialXPos    .DB 2 * 8
    Base.InitialYPos    .DB 7 * 8
    Base.InitialSpeed   .DB -1
    StartingTileIndex   .DW 448 - (ELEVATOR_ACTOR_BG_TOTAL_TILES_NEEDED_FOR_ALLOC * 1)
.ENDST

.DSTRUCT @@@@BG_2 INSTANCEOF sActorElevatorBG_Init VALUES
    Base.InitialXPos    .DB 5 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB 1
    StartingTileIndex   .DW 448 - (ELEVATOR_ACTOR_BG_TOTAL_TILES_NEEDED_FOR_ALLOC * 2)
.ENDST

.DSTRUCT @@@@BG_3 INSTANCEOF sActorElevatorBG_Init VALUES
    Base.InitialXPos    .DB 8 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB -1
    StartingTileIndex   .DW 448 - (ELEVATOR_ACTOR_BG_TOTAL_TILES_NEEDED_FOR_ALLOC * 3)
.ENDST

@@Sprite_Elevators:
@@@Count:
    .DB 4
@@@Definitions:
.DSTRUCT @@@@@Sprite_1 INSTANCEOF sActorElevatorSprite_Init VALUES
    Base.InitialXPos    .DB 11 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB 1
.ENDST

.DSTRUCT @@@@@Sprite_2 INSTANCEOF sActorElevatorSprite_Init VALUES
    Base.InitialXPos    .DB 14 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB -1
.ENDST

.DSTRUCT @@@@@Sprite_3 INSTANCEOF sActorElevatorSprite_Init VALUES
    Base.InitialXPos    .DB 17 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB 1
.ENDST

.DSTRUCT @@@@@Sprite_4 INSTANCEOF sActorElevatorSprite_Init VALUES
    Base.InitialXPos    .DB 20 * 8
    Base.InitialYPos    .DB 128
    Base.InitialSpeed   .DB -1
.ENDST

@ElevatorColumns:
;.DB 2, 5, 8, 11, 14, 17, 20
.DB 2, 5, 8, 11, 14, 17, 20

@Player:
@@Definitions:
.DSTRUCT @@@Player_1 INSTANCEOF sActorPlayer_Init VALUES
    InitialXPos         .DB 0
    InitialYPos         .DB 168
    InitialFloor        .DB 7
.ENDST

@TileSets_Count:
.DB 3

@TileSets:
.DW @@DefaultFont_TileSet
.DW @@LevelSet_4bpp_TileSet
.DW @@ElevatorSprites_4bpp_TileSet

@@DefaultFont_TileSet:
.DSTRUCT @@DefaultFont@TileSetDescriptor INSTANCEOF sTileSetDescriptor VALUES
    Flags:      .DB TILESET_FLAGS_1BPP_UNCOMPRESSED | TILESET_FLAGS_STRUCTURE_BLOCK | TILESET_FLAGS_BLOCK_HAS_DEST_INDEX | TILESET_FLAGS_HAS_1BPP_REMAP
    NumTiles:   .DW ( DefaultFont_1bpp_DataEnd - DefaultFont_1bpp_DataBegin ) >> 3

    ; 1bpp-Only Pal Remaps:
    PaletteRemap_1bpp.Pal0_Remap:   .DB 0
    PaletteRemap_1bpp.Pal1_Remap:   .DB 1
.ENDST
.DSTRUCT @@DefaultFont@FontData INSTANCEOF sTileSet_BlockEntry VALUES
    DestTileIndex:  .DW GFX_TILE_INDEX_FONT
    pData:          .DW DefaultFont_1bpp_DataBegin
.ENDST

@@LevelSet_4bpp_TileSet:
.DSTRUCT @@@TileSetDescriptor INSTANCEOF sTileSetDescriptor VALUES
    Flags:      .DB TILESET_FLAGS_4BPP_UNCOMPRESSED | TILESET_FLAGS_STRUCTURE_LIST
    NumTiles:   .DW (@@@List@End - @@@List ) / _sizeof_sTileSet_ListEntry
.ENDST
; Now each list entry
@@@List:

; Verbose way, especially if you need a label for later.
.DSTRUCT @@@@Empty INSTANCEOF sTileSet_ListEntry VALUES
    DestTileIndex:  .DW GFX_TILE_INDEX_EMPTY
    pData:          .DW EmptyGraphic_4bpp_Data
.ENDST

; Faster method.  Anonymous.
;      Idx                              Ptr
.TABLE DW,                              DW
.ROW GFX_TILE_INDEX_FLOOR,              FloorGraphic_4bpp_Data
.ROW GFX_TILE_INDEX_ELEVATOR_LEFT,      ElevatorGraphic_4bpp_Data@Left
.ROW GFX_TILE_INDEX_ELEVATOR_RIGHT,     ElevatorGraphic_4bpp_Data@Right
.ROW GFX_TILE_INDEX_EDGE_LEFT,          EdgeGraphic_4bpp_Data@Left
.ROW GFX_TILE_INDEX_EDGE_LEFT_FLOOR,    EdgeGraphic_4bpp_Data@Left_Floor
.ROW GFX_TILE_INDEX_EDGE_RIGHT,         EdgeGraphic_4bpp_Data@Right
.ROW GFX_TILE_INDEX_EDGE_RIGHT_FLOOR,   EdgeGraphic_4bpp_Data@Right_Floor

@@@@End:

@@ElevatorSprites_4bpp_TileSet:
.DSTRUCT @@@ElevatorSpriteSetDescriptor INSTANCEOF sTileSetDescriptor VALUES
    Flags:      .DB TILESET_FLAGS_4BPP_UNCOMPRESSED | TILESET_FLAGS_STRUCTURE_LIST
    NumTiles:   .DW (@@@List@End - @@@List ) / _sizeof_sTileSet_ListEntry
.ENDST
; Now each list entry
@@@List:
;      Idx                                  Ptr
.TABLE DW,                                  DW
.ROW   ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UL, SpyGraphic_4bpp_Data@UL
.ROW   ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LL, SpyGraphic_4bpp_Data@LL
.ROW   ACTOR_ELEVATOR_SPRITE_TILE_INDEX_UR, SpyGraphic_4bpp_Data@UR
.ROW   ACTOR_ELEVATOR_SPRITE_TILE_INDEX_LR, SpyGraphic_4bpp_Data@LR
@@@@End:

; Private
_Map_Round1:

.ENDS

.ENDIF  ; __MAP_ROUND1_ASM__