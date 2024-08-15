.IFNDEF __TILEMANAGER_ASM__
.DEFINE __TILEMANAGER_ASM__

.INCLUDE "Modules/tileset.asm"
.INCLUDE "Utils/tile_routines.asm"


.STRUCT sTileProvenance
    pTileData   DW      ; Pointer to the data
    pTileSet    DW      ; Pointer to the owning tile set
.ENDST

.DEFINE TILEMANAGER_MAX_TILE_PROVENANCE 448

.STRUCT sTileManager
    ; Track where each tile came from.
    TileProvenanceTable DSW TILEMANAGER_MAX_TILE_PROVENANCE
.ENDST


.SECTION "Tile Manager - Init" FREE

;==============================================================================
; TileManager_Init
; Initializes the tile manager.
; INPUTS:  None
; OUTPUTS:  None
; Destroys HL, BC, A
;==============================================================================
TileManager_Init:
    ; Clear the provenance table.
    ld      hl, gTileManager.TileProvenanceTable
    ld      bc, _sizeof_sTileManager.TileProvenanceTable
-:
    ld      a, b
    or      c
    ret     z

    ld      (hl), $00
    dec     bc
    jr      -


.ENDS

.SECTION "Tile Manager - Load Tile Set" FREE
;==============================================================================
; TileManager_LoadTileSet
; Loads a tile set.
; INPUTS:  IY: Pointer to sTileSetDescriptor
;          (OPTIONAL) D: Palette remap for 1bpp 1s
;          (OPTIONAL) E: Palette remap for 1bpp 0s
; OUTPUTS:  None
; Destroys Everything
;==============================================================================
TileManager_LoadTileSet:
    ; IY is the descriptor.

    ; What is the structure we're dealing with?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_STRUCTURE_LIST
    jp      z, @Block
    ; Otherwise, we're a list.
@List:
    ; What is our storage type?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_TYPE_BIT_MASK
    cp      TILESET_FLAGS_1BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_1bpp
    cp      TILESET_FLAGS_4BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_4bpp
    ; TODO:  ERROR
    halt
@@Uncompressed_1bpp:
    ; Do we have remaps specified?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_HAS_1BPP_REMAP
    jr      z, @@@Load

    ; Otherwise, load up the remaps.
    ld      e, (iy + sTileSetDescriptor.PaletteRemap_1bpp.Pal0_Remap)   ; Get the palette remap for 0s
    ld      d, (iy + sTileSetDescriptor.PaletteRemap_1bpp.Pal1_Remap)   ; Get the palette remap for 1s
@@@Load:
    ; We can optimize our routine by pre-interleaving the palette remaps.
    PRE_INTERLEAVE_1BPP_REMAP_ENTRIES_TO_E_FROM_DE

    ; Get our count of tiles in this list.
    ld      c, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      b, (iy + sTileSetDescriptor.NumTiles + 1)

    ; Point to data after descriptor.
    push    bc
        ld      bc, _sizeof_sTileSetDescriptor
        add     iy, bc  ; IY now points to the first sTileSet_ListEntry
    pop     bc
-:
    ; Are we done?
    ld      a, b
    or      c
    ret     z       ; DONE

    ; Get the dest tile position
    ld      l, (iy + sTileSet_ListEntry.DestTileIndex + 0)
    ld      h, (iy + sTileSet_ListEntry.DestTileIndex + 1)

    ; Set VRAM pointer
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL

    ; Get pointer to data
    ld      l, (iy + sTileSet_ListEntry.pData + 0)
    ld      h, (iy + sTileSet_ListEntry.pData + 1)

    push    bc
        ld      bc, _sizeof_sTile / 4    ; Size of 1bpp tile.

        call    Tile_Upload1BPPWithPaletteRemaps_VRAMPtrSet_ColorsInterleaved

        ; Advance to next tile in list.
        ld      bc, _sizeof_sTileSet_ListEntry
        add     iy, bc
    pop     bc

    dec     bc  ; One tile less
    jp      -

@@Uncompressed_4bpp:
    ; Get our count of tiles in this list.
    ld      c, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      b, (iy + sTileSetDescriptor.NumTiles + 1)

    ; Point to data after descriptor.
    ld      de, _sizeof_sTileSetDescriptor
    add     iy, de  ; IY now points to the first sTileSet_ListEntry
-:
    ; Are we done?
    ld      a, b
    or      c
    ret     z       ; DONE

    ; Get the dest tile position
    ld      l, (iy + sTileSet_ListEntry.DestTileIndex + 0)
    ld      h, (iy + sTileSet_ListEntry.DestTileIndex + 1)

    ; Get pointer to data
    ld      e, (iy + sTileSet_ListEntry.pData + 0)
    ld      d, (iy + sTileSet_ListEntry.pData + 1)

    push    bc
        ld      bc, _sizeof_sTile    ; Size of 1bpp tile.

        call    VDP_UploadTileDataToTilePos

        ; Advance to next tile in list.
        ld      bc, _sizeof_sTileSet_ListEntry
        add     iy, bc
    pop     bc

    dec     bc  ; One tile less
    jp      -

@Block:
    ; What is our storage type?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_TYPE_BIT_MASK
    cp      TILESET_FLAGS_1BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_1bpp
    cp      TILESET_FLAGS_4BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_4bpp
    ; TODO:  ERROR
    halt
@@Uncompressed_1bpp:
    ; Do we have remaps specified?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_HAS_1BPP_REMAP
    jr      z, @@@Load

    ; Otherwise, load up the remaps.
    ld      e, (iy + sTileSetDescriptor.PaletteRemap_1bpp.Pal0_Remap)   ; Get the palette remap for 0s
    ld      d, (iy + sTileSetDescriptor.PaletteRemap_1bpp.Pal1_Remap)   ; Get the palette remap for 1s
@@@Load:
    ; Find out where we're writing to.
    ld      l, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 0)
    ld      h, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 1)

    ; Set VRAM pointer
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL

    ; Size of data == #/Tiles * sizeof 1bpp tile (8 bytes)
    ; Get our #/tiles in HL because it's faster to do the math in HL than bitshifts in BC.
    ld      l, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      h, (iy + sTileSetDescriptor.NumTiles + 1)
    add     hl, hl  ; HL = #/tiles * 2
    add     hl, hl  ; HL = #/tiles * 4
    add     hl, hl  ; HL = #/tiles * 8
    ld      c, l
    ld      b, h    ; BC = #/tiles * 8

    ; Get pointer to the data.  It's just past the descriptor.
    ld      l, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 0)
    ld      h, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 1)

    call    Tile_Upload1BPPWithPaletteRemaps_VRAMPtrSet
    ret

@@Uncompressed_4bpp:
    ; Size of data == #/Tiles * sizeof tile (32 bytes)
    ld      c, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      b, (iy + sTileSetDescriptor.NumTiles + 1)
    .REPT 5
        sla c
        rl  b
    .ENDR

    ; Find out where we're writing to.
    ld      l, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 0)
    ld      h, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 1)

    ; Get pointer to the data.  It's just past the descriptor.
    ld      e, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 0)
    ld      d, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 1)

    call    VDP_UploadTileDataToTilePos
    ret

.ENDS

.SECTION "Tile Set - Attempt Find Tile in Set" FREE
;==============================================================================
; TileManager_AttemptFindTileInSet
; Loads a tile set.
; INPUTS:  IY: Pointer to sTileSetDescriptor
;          DE: Tile index to be searched for
; OUTPUTS:  Carry flag:  Set if requested tile was NOT found
;           HL points to data
; Destroys IY, BC, HL, A
;==============================================================================
TileManager_AttemptFindTileInSet:
    ; IY points to the descriptor for the set.

    ; What is the structure we're dealing with?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_STRUCTURE_LIST
    jp      z, @Block
    ; Otherwise, we're a list.

    ; FALL THROUGH

@List:
    ld      l, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      h, (iy + sTileSetDescriptor.NumTiles + 1)

    ld      bc, _sizeof_sTileSetDescriptor
    add     iy, bc                                      ; IY is start of list data.

    ld      c, l
    ld      b, h
-:
    ; Are we done yet?
    ld      a, b
    or      c
    jr      z, @@NotFound

    ; Get dest tile index.
    ld      l, (iy + sTileSet_ListEntry.DestTileIndex + 0)
    ld      h, (iy + sTileSet_ListEntry.DestTileIndex + 1)

    ; Is this our tile?
    and     a
    sbc     hl, de
    jr      z, @@FoundIt

    ; Nope.  Advance.
    .REPT _sizeof_sTileSet_ListEntry
        inc     iy
    .ENDR

    ; Decrement count
    dec     bc

    jp      -
@@NotFound:
    scf             ; Set carry for failure.
    ret

@@FoundIt:
    ; We matched!  Get the pointer to the data.
    ld      l, (iy + sTileSet_ListEntry.pData + 0)
    ld      h, (iy + sTileSet_ListEntry.pData + 1)
    and     a       ; Clear carry
    ret

@Block:
    ; See if this is within our range, first.

    ld      l, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 0)
    ld      h, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.DestTileIndex + 1)

    ex      de, hl

    and     a
    sbc     hl, de
    jr      c, @@NotFound       ; If the tile we're looking for was less than our dest, it's not present.

    ; See if our tile is within range.
    ld      e, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      d, (iy + sTileSetDescriptor.NumTiles + 1)
    dec     de                  ; Off by 1

    ex      de, hl              ; Now DE is test tile - dest for this block, HL is count of tiles - 1

    and     a
    sbc     hl, de
    jr      c, @@NotFound

    ; WE ARE IN THE BLOCK'S RANGE
    ; DE holds our tile index offset into the block.
    ld      l, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 0)
    ld      h, (iy + _sizeof_sTileSetDescriptor + sTileSet_BlockEntry.pData + 1)

    ; Data offset is dependent on the bpp of the tile data itself.
    ld      a, (iy + sTileSetDescriptor.Flags)
    and     TILESET_FLAGS_TYPE_BIT_MASK
    cp      TILESET_FLAGS_1BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_1bpp
    cp      TILESET_FLAGS_4BPP_UNCOMPRESSED
    jr      z, @@Uncompressed_4bpp
    ; TODO:  ERROR
    halt
@@NotFound:
    scf     ; Set carry flag to indicate failure.
    ret
@@Uncompressed_1bpp:
    ; HL holds start of data.  DE holds tile index offset we want.
    ; We want HL + (DE * 8) for 1bpp.
    ex      de, hl
    add     hl, hl      ; HL = offset * 2
    add     hl, hl      ; HL = offset * 4
    add     hl, hl      ; HL = offset * 8
    add     hl, de
    and     a           ; Clear carry for success
    ret
@@Uncompressed_4bpp:
    ; HL holds start of data.  DE holds tile index offset we want.
    ; We want HL + (DE * 32) for 4bpp.
    .REPT 5
        sla     e
        rl      d
    .ENDR
    add     hl, de
    and     a           ; Clear carry for success
    ret
.ENDS

.SECTION "Tile Set - Load Tile Set Provenance" FREE
;==============================================================================
; TileManager_LoadTileSetProvenance
; Loads a tile set's provenance into the Tile Manager so that we can find out
; where a given tile came from.
; INPUTS:  IY: Pointer to sTileSetDescriptor
; OUTPUTS:  None
; Destroys Everything
;==============================================================================
TileManager_LoadTileSetProvenance:
    ; IY is the descriptor.

    ; What is the structure we're dealing with?
    ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
    and     TILESET_FLAGS_STRUCTURE_LIST
    jp      z, @Block
    ; Otherwise, we're a list.
@List:
    ; Get our count of tiles in this list.
    ld      c, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      b, (iy + sTileSetDescriptor.NumTiles + 1)

    ; Save our tile set descriptor.
    push    iy
    pop     hl

    ; Point to data after descriptor.
    push    bc
        ld      bc, _sizeof_sTileSetDescriptor
        add     iy, bc  ; IY now points to the first sTileSet_ListEntry
    pop     bc

    push    ix  ; We need this for the provenance.
-:
        ; Are we done?
        ld      a, b
        or      c
        jp      z, @@Done

        ; Get the dest tile position
        ld      e, (iy + sTileSet_ListEntry.DestTileIndex + 0)
        ld      d, (iy + sTileSet_ListEntry.DestTileIndex + 1)

        ; Calculate the offset into the provenance table.
        ld      ix, gTileManager.TileProvenanceTable
        .REPT _sizeof_sTileProvenance
            add ix, de
        .ENDR
        ; IX = sTileProvenance object.
        ld      (ix + sTileProvenance.pTileSet + 0), l
        ld      (ix + sTileProvenance.pTileSet + 1), h

        ; Get pointer to data
        ld      e, (iy + sTileSet_ListEntry.pData + 0)
        ld      d, (iy + sTileSet_ListEntry.pData + 1)

        ; IX = sTileProvenance object.
        ld      (ix + sTileProvenance.pTileData + 0), e
        ld      (ix + sTileProvenance.pTileData + 1), d

        ; Advance to next tile in list.
        ld      de, _sizeof_sTileSet_ListEntry
        add     iy, de

        dec     bc  ; One tile less
        jp      -
@@Done:
    pop     ix
    ret

@Block:
    ; How many tiles have we got?
    ld      c, (iy + sTileSetDescriptor.NumTiles + 0)
    ld      b, (iy + sTileSetDescriptor.NumTiles + 1)   ; BC holds the #/tiles

    ; Keep our pointer to the tile set.
    push    iy
    pop     hl          ; HL holds ptr to tile set.

    ; Advance past the descriptor.
    ld      de, _sizeof_sTileSetDescriptor
    add     iy, de      ; IY now points to the sTileSet_BlockEntry after the descriptor

    ; Find our offset into the provenance table.
    ld      e, (iy + sTileSet_BlockEntry.DestTileIndex + 0)
    ld      d, (iy + sTileSet_BlockEntry.DestTileIndex + 1)

    push    ix
        ld      ix, gTileManager.TileProvenanceTable
        .REPT _sizeof_sTileProvenance
            add     ix, de
        .ENDR

        ; IX points to first provenance.  
        
        ; Now get the pointers to data.
        ld      e, (iy + sTileSet_BlockEntry.pData + 0)
        ld      d, (iy + sTileSet_BlockEntry.pData + 1)
        ; DE holds pointer to data.

        push    hl
        pop     iy  ; Get IY back to the tile set.

        ex      de, hl  ; HL now has the data pointer, DE has the tile set.
-:
        ld      a, b
        or      c
        jr      z, @@Done   ; DONE?

        push    bc
            ld      (ix + sTileProvenance.pTileData + 0), l
            ld      (ix + sTileProvenance.pTileData + 1), h
            ld      (ix + sTileProvenance.pTileSet + 0), e
            ld      (ix + sTileProvenance.pTileSet + 1), d

            ; Advance the data pointer based on size of each tile.
            ; What is our storage type?
            ld      a, (iy + sTileSetDescriptor.Flags)          ; Get flags
            and     TILESET_FLAGS_TYPE_BIT_MASK
            cp      TILESET_FLAGS_1BPP_UNCOMPRESSED
            jr      z, @@Uncompressed_1bpp
            cp      TILESET_FLAGS_4BPP_UNCOMPRESSED
            jr      z, @@Uncompressed_4bpp
            ; TODO:  ERROR
            halt
@@Uncompressed_1bpp:
            ld      bc, _sizeof_sTile / 4
            jp      @@DataSizeFound
@@Uncompressed_4bpp:
            ld      bc, _sizeof_sTile
            ; FALL THROUGH
@@DataSizeFound:
            ; Add our current size of tile data 
            add     hl, bc

            ; Advance our provenance pointer
            ld      bc, _sizeof_sTileProvenance
            add     ix, bc
        pop     bc

        dec     bc
        jp      -
@@Done:
    pop     ix
    ret
.ENDS




.RAMSECTION "Tile Manager Instance" SLOT 3
    gTileManager INSTANCEOF sTileManager
.ENDS


.ENDIF  ;__TILEMANAGER_ASM__