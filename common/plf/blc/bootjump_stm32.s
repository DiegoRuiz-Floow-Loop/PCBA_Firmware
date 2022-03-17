;/*******************************************************************************
; * TekPartner A/S
; * $Header$
; *******************************************************************************/

                EXPORT  boot_jump

;.syntax unified
;.cpu cortex-m4
;.thumb
 
;.global boot_jump

  PRESERVE8
  THUMB
  AREA    |.text|, CODE, READONLY
  EXPORT  boot_jump

boot_jump PROC
  ldr sp, [r0]      ; Load new stack pointer address 
  ldr pc, [r0, #4]  ; Load new program counter address 
  ENDP
  
  END
