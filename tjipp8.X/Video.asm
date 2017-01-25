    #include "p16f1705.inc"

    RADIX DEC           ; Use decimal numbers by default
    ERRORLEVEL -302     ; Kill messages about "Register not in bank 0"
    ERRORLEVEL -305     ; Kill messages about "Using default destination"

 GLOBAL Interrupt
 GLOBAL VideoInit
 
 EXTERN step
 EXTERN DTi
 EXTERN STi
    
;----------------
BANK1 UDATA     ; TRIS/PIE/OPTION_REG/ADCCONx
;----------------
 
;----------------
BANK0 UDATA     ; PORT/TMRx
;----------------

;----------------
BANK2 UDATA     ; LAT/CMx/DAC
;----------------
tvLine      res 1   ; ISR - counts 156..1 for tv lines
tvLine2     res 1   ; ISR - counts even/odd to call the tvLines twice
row         res 1   ; ISR - The current pixel row (y) 0..31
lines       res 1   ; ISR - For the loop of the 7*32 active lines
linecnt     res 1   ; ISR - Counting 7 lines for each pixel row (y)

     
;-----------------------
; Linear memory region 0x2000..0x29AF
; Each bank is 80 0x50 bytes. 31 banks @ 80 bytes = 2480 0x9B0
;
; Bank 0 0x2000   Bank 8 0x2280   Bank16 0x2500   Bank24 0x2780
; Bank 1 0x2050   Bank 9 0x22D0   Bank17 0x2550   Bank25 0x27D0
; Bank 2 0x20A0   Bank10 0x2320   Bank18 0x25A0   Bank26 0x2820
; Bank 3 0x20F0   Bank11 0x2370   Bank19 0x25F0   Bank27 0x2870
; Bank 4 0x2140   Bank12 0x23C0   Bank20 0x2640   Bank28 0x28C0
; Bank 5 0x2190   Bank13 0x2410   Bank21 0x2690   Bank29 0x2910
; Bank 6 0x21E0   Bank14 0x2460   Bank22 0x26E0   Bank30 0x2960..0x29AF
; Bank 7 0x2230   Bank15 0x24B0   Bank23 0x2730   
;-----------------------

#define videobuf  0x2100    ; 0x100 bytes for 64 columns, 32 lines 
                            ; of 1 bpp video buffer

                            
#define SYNCS   LATC,4
#define VIDEO   LATA,0

                            
;*******************************************************************************
; Macros
;*******************************************************************************        

; lSdddrSdddrSdddrSdddrSdddrSdddrSdddrSddd
; .^....^....^....^....^....^....^....^...

outFirstPixelM macro    ; 5 cycles (0.625 us)
    moviw   FSR0++      ; 1
    movwf   LATA        ; 1
    goto    $+1         ; 2
    nop                 ; 1
 endm

outNextPixelM macro     ; 5 cycles (0.625 us)
    lsrf    WREG        ; 1
    movwf   LATA        ; 1
    goto    $+1         ; 2
    nop                 ; 1
 endm

out8pixelsM macro       ; 40 cycles (5.000 us)
    outFirstPixelM      ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
    outNextPixelM       ; 5
 endm
 
out64pixels macro       ; 322 cycles (40.250 us)
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    out8pixelsM         ; 40
    nop                 ;  1 The last pixel need one extra cycle to be full
    bcf     LATA,0      ;  1 Output black for the rest of the line
 endm
        
            
     
     
PROG0 CODE    
 
;*******************************************************************************
; INTERRUPT
;*******************************************************************************
Interrupt:

; The next two instructions are used to eliminate jitter on the
; interrupt timing. When the interrupt occurs, it may have to wait
; an extra cycle becuase a two cycle instruction is in progress.
;
; We can tell the difference by looking at the least significant
; bit of TMR2. If the next instruction skips we use 2 cycles, if it
; doesn't skip then we use 3 cycles. The net result is that we add
; a cycle when needed so that we always get to 'dejittered' at the
; exact same time relative to the actual rollover of TMR2!
;

;    banksel TMR2
;    btfss   TMR2,0
;    bra     dejittered
;dejittered:
    
    banksel PIR1       
    bcf     PIR1,TMR2IF ; Clear TMR2IF -TMR2 to PR2 Match Interrupt Flag

    banksel LATC        ; This bank holds ISR variables and the GPIO LAT
                        ; and is being used for everything in the ISR from 
                        ; this point forwards

    bcf     SYNCS       ; 1 Pull sync low

;                             EVEN ODD  ROLL
;                           ----------------
    incf    tvLine2         ; 1    1    1 
    btfss   tvLine2,0       ; 1    2    1
    goto    J2              ; 2         2
    goto    $+1             ;      2
    goto    J3              ;      2
J2  decfsz  tvLine          ; 1         2
    goto    J3              ; 2
    movlw   D'312'/2        ;           1
    movwf   tvLine          ;           1
    goto    TvLineJumpTable ;           2
J3  movfw   tvLine          ; 1    1
    goto    TvLineJumpTable ; 2    2
;                            -------------
;                      Total 10   10   10

   
   
   
;
; This table computes the jump to the right type of horizontal line to output.
; The non-interlaced PAL standard is 312 lines but that value is larger that
; can fit in a 8 bit variable the current line number is divided by two before
; jumping to this function. So each entry handles two consecutive lines. (But
; are still called twice, with the same line number). The line counter is 
; counting backwards from 156 down to 1 due to speed reasons in the loop, this
; table is reversed and have a dummy first entry since 0 is not used.
TvLineJumpTable:
    brw
    nop                 ; Dummy to adjust for 0 not being in the loop values

    goto    HSYNCLINEUPDATE   ; 38 hsyncs
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    

    goto    VIDEOLINE   ;224 video
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    goto    VIDEOLINE   
    
    goto    HSYNCLINE   ; 46 hsyncs
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    goto    HSYNCLINE    
    
    goto    VSYNCLINE   ; 4 vsyncs
    goto    VSYNCLINE    

; This is a special version of the HSYNC that is called one per frame ~50Hz
; for decrementing the two Chip-8 timers as long as they are non-zero
HSYNCLINEUPDATE:        ; 16 cycles since sync was activated
    call    dly16c      ; 16 cycles
    bsf     SYNCS       ;  1 Set sync inactive
    movf    DTi,F
    btfss   STATUS,Z
    decf    DTi
    movf    STi,F
    btfss   STATUS,Z
    decf    STi
    bsf     step,0
    retfie
    
HSYNCLINE:              ; 16 cycles since sync was activated
    call    dly16c      ; 16 cycles
    bsf     SYNCS       ;  1 Set sync inactive
    retfie

    
VSYNCLINE:              ;  16 cycles since sync was activated
    call    dly238c     ; 238
    bsf     SYNCS       ;   1 Set sync inactive
    call    dly15c      ;  15
    bcf     SYNCS       ;   1 Pull sync low again
    call    dly238c     ; 238
    bsf     SYNCS       ;   1 Set sync inactive
    bsf     step,0

    retfie
    
    
VIDEOLINE:              ;  16 cycles since sync was activated
    call    dly16c      ;  16 cycles
    bsf     SYNCS       ;   1 Set sync inactive
; Calculate the address into the videobuffer to read from at this line
    ; TOTAL time is 10 cycles = 1.250 us
    ;                   Path1   Path2
    decfsz  linecnt     ;1      2
    goto    nonew       ;2
    incf    row         ;       1
    movlw   7           ;       1    
    movwf   linecnt     ;       1
    goto    setfsr      ;       2
nonew:                  
    call    dly4c  ;4
setfsr:
    movlw   HIGH(videobuf);1    1 
    movwf   FSR0H
    movfw   row         ;1      1
    movwf   FSR0L       ;1      1
    lslf    FSR0L       ;1      1   ; Multiply by 8   
    lslf    FSR0L       ;1      1   
    lslf    FSR0L       ;1      1   
    ;                   ==========
    ;                   13     13
    call    dly16c
    call    dly31c
    call    dly31c
    out64pixels         ; 322 cycles = 40.250 us
    retfie
    

dly7c:             
     goto   $+1           
dly5c:             
     nop                
dly4c:             
     return              
     
dly16c:            
     nop
dly15c:            
    movlw   0x03          
    movwf   WREG
    decfsz  WREG
    goto    $-1
    nop
    return               

dly31c:
    movlw   0x08          
    movwf   WREG
    decfsz  WREG
    goto    $-1
    goto    $+1
    return               
    
    
dly238c:
    goto    $+1
    goto    $+1
    goto    $+1
    goto    $+1
dly230c:
    movlw   0x4B
    movwf   WREG
    decfsz  WREG
    goto    $-1
    return               

dly469c:
    movlw   0x9A          
    movwf   WREG
    decfsz  WREG
    goto    $-1
    goto    $+1
    return               
     
dly1.500:               ; 0.125*12= 1.500 us
     goto $+1           ; 2 cycles
     goto $+1           ; 2 cycles
     goto $+1           ; 2 cycles
     goto $+1           ; 2 cycles
     return              ; 4 cycles (including call)

dly4.375:               ; 0.125*35= 4.375 us
    movlw 0x0A          ; 31 cycles
    movwf WREG
    decfsz WREG
    goto $-1
    return              ;4 cycles (including call)

dly5.750:               ; 0.125*46= 5.750 us
    movlw 0x0D          ; 43 cycles
    movwf WREG
    decfsz WREG
    goto $-1
    goto $+1            ;2 cycles
    return              ;4 cycles (including call)

dly6.000:               ; 0.125*48= 6.000 us
    movlw 0x0E          ; 43 cycles
    movwf WREG
    decfsz WREG
    goto $-1
    nop                 ;1 cycle
    return              ;4 cycles (including call)

    
VideoInit:
    banksel LATA
    movlw   D'312'/2
    movwf   tvLine
    clrf    tvLine2

    clrf    row
    movlw   8           ; Start at 8 instead of 7 because of pre-decrement
    movwf   linecnt     ; in the video line handler
    return
    
    
    END
