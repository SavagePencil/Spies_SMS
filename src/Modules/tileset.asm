.IFNDEF __TILESET_ASM__
.DEFINE __TILESET_ASM__

; What type is the data being used?
.DEFINE TILESET_FLAGS_TYPE_BIT_SHIFT            0
.DEFINE TILESET_FLAGS_TYPE_BIT_MASK             3 << TILESET_FLAGS_TYPE_BIT_SHIFT
.DEFINE TILESET_FLAGS_1BPP_UNCOMPRESSED         0 << TILESET_FLAGS_TYPE_BIT_SHIFT
.DEFINE TILESET_FLAGS_4BPP_UNCOMPRESSED         1 << TILESET_FLAGS_TYPE_BIT_SHIFT

; Is this a block of tiles, or a list of non-contiguous ones?
.DEFINE TILESET_FLAGS_STRUCTURE_SHIFT           2
.DEFINE TILESET_FLAGS_STRUCTURE_MASK            1 << TILESET_FLAGS_STRUCTURE_SHIFT
.DEFINE TILESET_FLAGS_STRUCTURE_BLOCK           0 << TILESET_FLAGS_STRUCTURE_SHIFT
.DEFINE TILESET_FLAGS_STRUCTURE_LIST            1 << TILESET_FLAGS_STRUCTURE_SHIFT

; OPTIONAL:  Does this block have a destination tile index defined, or will it come from code (blocks only)?
.DEFINE TILESET_FLAGS_BLOCK_DEST_INDEX_SHIFT    3
.DEFINE TILESET_FLAGS_BLOCK_DEST_INDEX_MASK     1 << TILESET_FLAGS_BLOCK_DEST_INDEX_SHIFT
.DEFINE TILESET_FLAGS_BLOCK_NO_DEST_INDEX       0 << TILESET_FLAGS_BLOCK_DEST_INDEX_SHIFT
.DEFINE TILESET_FLAGS_BLOCK_HAS_DEST_INDEX      1 << TILESET_FLAGS_BLOCK_DEST_INDEX_SHIFT

; OPTIONAL:  Does it have remap data (1bpp only)?
.DEFINE TILESET_FLAGS_1BPP_REMAP_SHIFT          4
.DEFINE TILESET_FLAGS_1BPP_REMAP_MASK           1 << TILESET_FLAGS_1BPP_REMAP_SHIFT
.DEFINE TILESET_FLAGS_NO_1BPP_REMAP             0 << TILESET_FLAGS_1BPP_REMAP_SHIFT
.DEFINE TILESET_FLAGS_HAS_1BPP_REMAP            1 << TILESET_FLAGS_1BPP_REMAP_SHIFT

.STRUCT sTileSetDescriptor
    Flags               DB      ; Composite of TILESET_FLAGS_*
    NumTiles            DW      ; How many tiles in this set?

    ; Type-based flag values
    .UNION PaletteRemap_1bpp
        Pal0_Remap      DB      ; Color remap for 0s in 1bpp
        Pal1_Remap      DB      ; Color remap for 1s in 1bpp
    .ENDU
.ENDST

.STRUCT sTileSet_ListEntry
    DestTileIndex   DW      ; Where is this destined?
    pData           DW      ; Where's the source data?
.ENDST

.STRUCT sTileSet_BlockEntry
    DestTileIndex   DW      ; Where is this destined?
    pData           DW      ; Where's the source data?
.ENDST

.ENDIF  ;__TILESET_ASM__