.IFNDEF __GAMEMANAGER_ASM__
.DEFINE __GAMEMANAGER_ASM__

.INCLUDE "Actors/actor_elevator.asm"
.INCLUDE "Actors/actor_player.asm"

.SECTION "Game Manager" FREE

.DEFINE GAMEMANAGER_INITIAL_LIVES 3

; Some elevators use sprites, while others use BG tiles to simulate sprites.
; The BG ones are a lot slower than the sprites, and have to be column-aligned.
.DEFINE MAX_ELEVATORS_SPRITES   4
.DEFINE MAX_ELEVATORS_BG        3

.DEFINE MAX_ELEVATORS MAX_ELEVATORS_SPRITES + MAX_ELEVATORS_BG

.STRUCT sPlayerStats
    Lives   DB
    Level   DB
    Score   DW
.ENDST

.STRUCT sGameManager
    NumPlayers  DB  ; Total #/players.  1 or 2.
    CurrPlayer  DB  ; Current player index.  1 or 2.
    Player1     INSTANCEOF sPlayerStats
    Player2     INSTANCEOF sPlayerStats

    ; GameScreen will handle initialization of these guys, but we want them
    ; accessible to other classes.
    ElevatorsSprites    INSTANCEOF sActorElevatorSprite MAX_ELEVATORS_SPRITES
    ElevatorsBG         INSTANCEOF sActorElevatorBG     MAX_ELEVATORS_BG
    PlayerActor         INSTANCEOF sActorPlayer
    NumElevatorsSprites DB
    NumElevatorsBG      DB
    NumElevators        DB  ; Sum of the above.
.ENDST

;==============================================================================
; GameManager_Init
; Sets the values for the game based on #/players
; INPUTS:  A:  #/players (1 == 1 player, 2 == 2 players)
; OUTPUTS:  None
; Destroys A, HL
;==============================================================================
GameManager_Init:
    ld  (gGameManager.NumPlayers), a
    ld  hl, gGameManager.CurrPlayer
    ld  (hl), $1                        ; Always start with player 1

    ; Init Player 1
    ld  hl, gGameManager.Player1.Lives
    ld  (hl), GAMEMANAGER_INITIAL_LIVES
    ld  hl, gGameManager.Player1.Level
    ld  (hl), $00
    ld  hl, gGameManager.Player1.Score
    ld  (hl), $0000

    ; Init Player 2
    ld  hl, gGameManager.Player2.Level
    ld  (hl), $00
    ld  hl, gGameManager.Player2.Score
    ld  (hl), $0000

    ; Do we have a player 2?  If not, set to 0 lives.
    dec a
    jr  z, @SetP2Lives    ; A == 0 will be our #/lives

    ; Otherwise, we have a P2.  Put #/lives in A.
    ld  a, GAMEMANAGER_INITIAL_LIVES
@SetP2Lives:
    ld  (gGameManager.Player2.Lives), a
    ret
.ENDS

.SECTION "GameManager - Is Game Over" FREE
;==============================================================================
; GameManager_IsGameOver
; Sets carry flag if all players are out of lives.
; INPUTS:  None
; OUTPUTS:  None
; Destroys A, HL
;==============================================================================
GameManager_IsGameOver:
    ld  a, (gGameManager.Player1.Lives)
    ld  hl, gGameManager.Player2.Lives
    add a, (hl)
    jr  z, @GameOver    ; If zero, we're done.

    and a               ; Clear carry
    ret

@GameOver:
    scf                 ; Set carry
    ret
.ENDS

.SECTION "GameManager - Get Next Player" FREE
;==============================================================================
; GameManager_GetNextPlayer
; Returns the next player whose turn it is.
; INPUTS:  None
; OUTPUTS:  A == 0:  Game Over.  
;           A == 1:  Player 1's turn is next.  
;           A == 2:  Player 2's turn is next.
; Destroys A, HL
;==============================================================================
GameManager_GetNextPlayer:
    call    GameManager_IsGameOver
    jr      nc, @CheckNumPlayers    ; If there are players, do the logic.

    ; Game over
    xor     a
    ret

@CheckNumPlayers:
    ; How many players have we got?
    ld      a, (gGameManager.NumPlayers)
    dec     a
    jr      z, @P1sTurn

    ; Otherwise, this is a 2p game.  Find out which player is currently active.
    ld      a, (gGameManager.CurrPlayer)
    dec     a
    jr      nz, @P2IsCurrent

@P1IsCurrent:
    ; P1 is current.  See if P2 has any lives left.
    ld      a, (gGameManager.Player2.Lives)
    and     a
    jr      z, @P1sTurn ; It's P1's turn if P2 has no lives.
    jr      @P2sTurn

@P2IsCurrent:
    ; P2 is current.  See if P1 has any lives left.
    ld      a, (gGameManager.Player1.Lives)
    and     a
    jr      z, @P2sTurn ; It's P2's turn if P1 has no lives.
    jr      @P1sTurn

@P1sTurn:
    ld      a, $1   ; It's player 1's turn next.
    ret

@P2sTurn:
    ld      a, $2   ; It's player 2's turn next.
    ret
.ENDS

.SECTION "GameManager - GetCurrPlayerStats" FREE
;==============================================================================
; GameManager_GetCurrPlayerStats
; Returns a pointer to the current player's sPlayerStats struct.
; INPUTS:  None
; OUTPUTS:  IY:  Points to current player's sPlayerStats object
; Destroys A, IY
;==============================================================================
GameManager_GetCurrPlayerStats:
    ld      iy, gGameManager.Player1
    ld      a, (gGameManager.CurrPlayer)
    dec     a
    ret     z

    ld      iy, gGameManager.Player2
    ret

.ENDS

.RAMSECTION "Game Manager Instance" SLOT 3
    gGameManager INSTANCEOF sGameManager
.ENDS



.ENDIF ;__GAMEMANAGER_ASM__