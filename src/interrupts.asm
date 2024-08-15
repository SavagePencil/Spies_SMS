.IFNDEF __INTERRUPTS_ASM__
.DEFINE __INTERRUPTS_ASM__

.INCLUDE "Managers/modemanager.asm"

.bank 0 slot 0
.org $0038
.section "SMSFramework Video Interrupts" FORCE
SMSFramework_VideoInterruptHandler:
    push    hl
        ld      hl, (gModeManager.CurrVideoInterruptJumpTarget)
        call    CallHL
    pop     hl
    ei
    reti
.ends

.bank 0 slot 0
.org $0066
.section "SMSFramework Non-Maskable Interrupts" FORCE
SMSFramework_NMIHandler:
    push    af
        ; Are we initialized, or did this come in while we were booting?
        ld a, (SMSFrameWork_Initialized)
        and a
        jr  z, @Restore ; Ignore it if we're not yet initialized.

        ; Pass this on to the mode handler
        push    ix
            call ModeManager_OnNMI
        pop     ix
@Restore:
    pop     af
    retn
.ENDS

.ENDIF  ;__INTERRUPTS_ASM__
