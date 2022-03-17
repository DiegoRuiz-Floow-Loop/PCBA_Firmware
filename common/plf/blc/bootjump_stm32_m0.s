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

;; M+
boot_jump PROC
; Load new stack pointer address 
  ldr r2, [r0]      
  mov sp, r2
  ;;  ldr sp, [r0]    
; Load new program counter address 
  ldr r2, [r0, #4]  
  ;mov pc, r2
  bx r2
  ;;  ldr pc, [r0, #4]  ;
  ENDP
  
  END
