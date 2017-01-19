;-----------------------------------------------------------------------------
; Tjipp8 - Chip-8 interpreter for PIC16F1705 with PAL video output 
;-----------------------------------------------------------------------------
;
; The files for the full project are located at 
; github.com/SmallRoomLabs/Tjipp8
;
; Copyright (c) 2016 Mats Engstrom SmallRoomLabs
; Released under the MIT license
;
;

    #include "p16f1705.inc"

    RADIX DEC           ; Use decimal numbers by default
    ERRORLEVEL -302     ; Kill messages about "Register not in bank 0"
    ERRORLEVEL -305     ; Kill messages about "Using default destination"
    EXPAND              ; Expand macros to be steppable during debugging
 
    __CONFIG _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
    __CONFIG _CONFIG2, _WRT_OFF & _PPS1WAY_OFF & _ZCDDIS_ON & _PLLEN_ON & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON


;
;  A0 out Video/ICSPDAT         16F1705                     
;  A1 in  KeyIn1/ICSPCLK        +--v--+                       
;  A2 in  KeyIn2              + |1  14| -                       
;  A3 in  ICSPRES            A5 |2  13| RA0/ISPDAT              
;  A4 in  KeyIn3             A4 |3  12| RA1/ISPCLK              
;  A5 in  KeyIn4         RES/A3 |4  11| RA2                     
;  C0 out KeyOut1            C5 |5  10| C0                      
;  C1 out KeyOut2            C4 |6   9| C1                      
;  C2 out KeyOut3            C3 |7   8| C2                      
;  C3 out KeyOut4               +-----+                             
;  C4 out Sync                                                   
;  C5 out Sound                
; 
;  
#define KEYIN1  PORTA,1    
#define KEYIN2  PORTA,2    
#define KEYIN3  PORTA,4    
#define KEYIN4  PORTA,5    
#define KEYOUT1 LATC,0    
#define KEYOUT2 LATC,1    
#define KEYOUT3 LATC,2    
#define KEYOUT4 LATC,3    
    
#define SOUND   LATC,5
#define SYNCS   LATC,4
#define VIDEO   LATA,0

;*******************************************************************************
; Variables in shared memory
;*******************************************************************************
; CORE REGISTERS
;---------------
;    x00 INDF0      x04 FSR0L       x08 BSR
;    x01 INDF1      x05 FSR0H       x09 WREG
;    x02 PCL        x06 FSR1L       x0A PCLATH
;    x03 STATUS     x07 FSR1H       x0B INTCON

;-----------------------
    UDATA_SHR ; 16 bytes of shared/global memory area 
;-----------------------
palline     res 1
twocnt      res 1
lines       res 1   ; For the loop of the 7*32 active lines
linecnt     res 1   ; Counting 7 lines for each pixel row (y)
row         res 1   ; The current pixel low (y) 0..31
d1          res 1
d2          res 1
d3          res 1

;*******************************************************************************
; Variables in individual bank memories
;*******************************************************************************

;-----------------------
bank0 UDATA 0*0x80+0x20 ; Bank 0 PORT/TMRx
;-----------------------
aa          res 1       ;                                                   

;-----------------------
bank1 UDATA 1*0x80+0x20 ; Bank 1 TRIS/PIE/OPTION_REG/ADCCONx
;-----------------------
aaa         res 1       ; 

;-----------------------
bank2 UDATA 2*0x80+0x20 ; Bank 2 LAT/CMx/DAC
;-----------------------
aaaa        res 1       ; 

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

#define videobuf  0x2100    ; 0x100 bytes for 64 columns, 32 lines of 1 bpp video buffer
        
;-----------------------
; Constants
#define AAAAA       16  ;                                   

; Hardware-related constants
#define AAAAAA      PORTA,0

        
        
;*******************************************************************************
; Macros
;*******************************************************************************        

outpixel0 macro              ; 0.625 us
    moviw   FSR0++      ;1
    movwf   LATA        ;1

    nop                 ;3
    nop
    nop
 endm

outnextpixel macro              ; 0.625 us
    lsrf    WREG        ;1
    movwf   LATA        ;1

    nop                 ;3
    nop
    nop
 endm

; lSdddrSdddrSdddrSdddrSdddrSdddrSdddrSddd
; .^....^....^....^....^....^....^....^...
 
out8pixels macro           ; 8x5 cycles = 5.000 us  
    outpixel0           ;5
    outnextpixel        ;5
    outnextpixel        ;5
    outnextpixel        ;5
    outnextpixel        ;5
    outnextpixel        ;5
    outnextpixel        ;5
    outnextpixel        ;5
 endm
 
out64pixels macro       ; TOTAL 40.250 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    out8pixels          ; 5.000 us
    nop                 ; 0.125 us 
    bcf     LATA,0      ; 0.125 us Output black for the rest of the line
 endm

        
        
;*******************************************************************************
; Reset & Interrupt Vectors
;*******************************************************************************

RES_VECT CODE 0x0000        ; Processor reset vector
    goto    Start           ; Jump to beginning of program

ISR CODE 0x0004             ; Interrupt vector 
    goto Interrupt          ; Jump to interrupt code

;------------------------------------------------------------------------------
; Lookup tables here near the start of code to avoid page wrapping
;------------------------------------------------------------------------------

;****************************************************************************
; Convert a bit number (0..7) to a mask value
;   Input: WREG
;   Destroys:  WREG
;   Banksel:  
;****************************************************************************
BitToMask:
    brw
    dt  1,2,4,8,16,32,64,128
    
;
; This table computes the jump to the right type of horizontal line to output.
; The non-interlaced PAL standard is 312 lines but that value is larger that
; can fit in a 8 bit variable the current line number is divided by two before
; jumping to this function. So each entry handles two consecutive lines. (But
; are still called twice, with the same line number). The line counter is 
; counting backwards from 156 down to 1 due to speed reasons in the loop, this
; table is reversed and have a dummy first entry since 0 is not used.
LineJumpTable:
    brw
    nop                 ; Dummy to adjust for 0 not being in the loop values

    goto    HSYNCLINE   ; 38 hsyncs
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
    
    
;*******************************************************************************
; INTERRUPT CODE
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

    banksel TMR2
    btfss   TMR2,0
    bra     dejittered
dejittered:
    
    banksel PIR1       
    bcf     PIR1,TMR2IF ; Clear TMR2IF -TMR2 to PR2 Match Interrupt Flag

    banksel LATC        
    bcf     SYNCS       ; 1 Pull sync low

;                             EVEN ODD  ROLL
;                           ----------------
    incf    twocnt          ; 1    1    1 
    btfss   twocnt,0        ; 1    2    1
    goto    J2              ; 2         2
    goto    $+1             ;      2
    goto    J3              ;      2
J2  decfsz  palline         ; 1         2
    goto    J3              ; 2
    movlw   D'312'/2        ;           1
    movwf   palline         ;           1
    goto    LineJumpTable   ;           2
J3  movfw   palline         ; 1    1
    goto    LineJumpTable   ; 2    2
;                            -------------
;                      Total 10   10   10

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************
MAIN_PROG CODE
    
Start:
; Setup peripheral registers in the PIC

;             +--------- /WPUEN  Weak Pull-Up Enable
;             |+-------- INTEDG Interrupt Edge Select
;             ||+------- TMR0CS Timer0 Clock Source Select
;             |||+------ TMR0SE Timer0 Source Edge Select
;             ||||+----- PSA    Prescaler Assignment
;             |||||+---- PS2
;             ||||||+--- PS1
;             |||||||+-- PS0    Prescaler Rate Select
;             ||||||||    
    movlw   B'00000000'
    banksel OPTION_REG
    movwf   OPTION_REG      

;             +--------- SPLLEN Software PLL Enable bit
;             |+-------- IRCF3 Internal Oscillator Frequency Select
;             ||+------- IRCF2
;             |||+------ IRCF1
;             ||||+----- IRCF0
;             |||||+---- n/a
;             ||||||+--- SCS1 System Clock Select
;             |||||||+-- SCS0
;             ||||||||    
    movlw   B'11110000'     ; Internal 32MHz oscillator
    banksel OSCCON          
    movwf   OSCCON          ;1;
    
;    Setup Timer2 to generate interrupts at 64 us (15625 Hz) intervals
;
    
;             +--------- n/a
;             |+-------- T2OUTPS3 Timer2 Postcaler 
;             ||+------- T2OUTPS2
;             |||+------ T2OUTPS1
;             ||||+----- T2OUTPS0
;             |||||+---- TMR2ON   Timer2 Enable
;             ||||||+--- T2CKPS1  Timer2 Prescaler
;             |||||||+-- T2CKPS0
;             ||||||||    
    movlw   B'00011100' ; Pre/1 Post/4, Enable
    banksel T2CON
    movwf   T2CON

    movlw   0x7F        ; Timer2 Match value = 64uS
    banksel PR2
    movwf   PR2
    
    banksel PIR1
    bcf     PIR1,TMR2IF     ; Clear timer2 interupt flag TMR2IF
    clrf    PIR1
    
    banksel PIE1
    bsf PIE1,TMR2IE     ; Enable Timer2 interrupts

;             +--------- GIE    Global Interrupt Enable
;             |+-------- PEIE   Peripheral Interrupt Enable
;             ||+------- TMR0IE Timer0 Overflow Interrupt Enable
;             |||+------ INTE   INT External Interrupt Enable
;             ||||+----- IOCIE  Interrupt-on-Change Enable
;             |||||+---- TMR0IF Timer0 Overflow Interrupt Flag
;             ||||||+--- INTF   INT External Interrupt Flag
;             |||||||+-- IOCIF  Interrupt-on-Change Interrupt Flag
;             ||||||||    
    movlw   B'11000000'
    banksel INTCON
    movwf   INTCON          
    
    banksel ANSELA
    clrf    ANSELA          ; All GPIOs are digital
    clrf    ANSELC          ;
    
    banksel TRISC
    clrf    TRISC           ; Entire gpio C is output
    bcf     TRISA,0         ; Gpio A.0 is output

    
    movlw   D'312'/2
    movwf   palline
    clrf    twocnt

    clrf    row
    movlw   7
    movwf   linecnt
    
    
    banksel LATA
    movlw   HIGH(videobuf)
    movwf   FSR0H
    clrf    FSR0L 
    movlw   16
    movwf   d1
apa movlw   0x00    ;movlw   0x55
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movlw   0x00;   movlw   0xAA
    movwi   FSR0++
    movwi   FSR0++    
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    movwi   FSR0++
    decfsz  d1
    goto    apa

    movlw   HIGH(videobuf)
    movwf   FSR0H
    movlw   0x00
    movwf   FSR0L
    movlw   0x81
    movwi   FSR0++
    
    movlw   HIGH(videobuf)
    movwf   FSR0H
    movlw   0xFF
    movwf   FSR0L
    movlw   0xC3
    movwi   FSR0++

    movlw   HIGH(videobuf)
    movwf   FSR0H
    movlw   0x00
    movwf   FSR0L

    movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011100'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011100'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01110000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00001110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01110000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00001110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'01111100'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00111110'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000011'
	movwi	FSR0++
	movlw	B'11000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111110'
	movwi	FSR0++
	movlw	B'00000111'
	movwi	FSR0++
	movlw	B'11100000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11100000'
	movwi	FSR0++
	movlw	B'11111011'
	movwi	FSR0++
	movlw	B'11001111'
	movwi	FSR0++
	movlw	B'00000111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11000000'
	movwi	FSR0++
	movlw	B'11111101'
	movwi	FSR0++
	movlw	B'10111111'
	movwi	FSR0++
	movlw	B'00000011'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111110'
	movwi	FSR0++
	movlw	B'10111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'01111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11000011'
	movwi	FSR0++
	movlw	B'11000011'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'10000011'
	movwi	FSR0++
	movlw	B'11000001'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11100011'
	movwi	FSR0++
	movlw	B'11000111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11110011'
	movwi	FSR0++
	movlw	B'11001111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01000000'
	movwi	FSR0++
	movlw	B'01111111'
	movwi	FSR0++
	movlw	B'01111110'
	movwi	FSR0++
	movlw	B'00000010'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11100000'
	movwi	FSR0++
	movlw	B'01111110'
	movwi	FSR0++
	movlw	B'01111110'
	movwi	FSR0++
	movlw	B'00000111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'11111110'
	movwi	FSR0++
	movlw	B'11111101'
	movwi	FSR0++
	movlw	B'10111111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'11111011'
	movwi	FSR0++
	movlw	B'11011111'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'10111001'
	movwi	FSR0++
	movlw	B'10011101'
	movwi	FSR0++
	movlw	B'11111111'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'11111100'
	movwi	FSR0++
	movlw	B'10010000'
	movwi	FSR0++
	movlw	B'00001001'
	movwi	FSR0++
	movlw	B'00111110'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01110000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00001110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'01110000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00001110'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011100'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++
	movlw	B'00000001'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00011100'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'00111000'
	movwi	FSR0++
	movlw	B'00000000'
	movwi	FSR0++
	movlw	B'10000000'
	movwi	FSR0++

Main:
    bcf     SOUND
    call    Delay500ms
    bsf     SOUND
    call    Delay500ms
    goto    Main

    
    
    

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

    
ScanKeyMatrix:
    banksel LATC
    bcf     KEYOUT1
    banksel PORTA
    btfss   KEYIN1
    retlw   0
    btfss   KEYIN2
    retlw   1
    btfss   KEYIN3
    retlw   2
    btfss   KEYIN4
    retlw   3
   banksel LATC
   bsf     KEYOUT1

    banksel LATC
    bcf     KEYOUT2
    banksel PORTA
    btfss   KEYIN1
    retlw   4
    btfss   KEYIN2
    retlw   5
    btfss   KEYIN3
    retlw   6
    btfss   KEYIN4
    retlw   7
    banksel LATC
    bsf     KEYOUT2
    
    banksel LATC
    bcf     KEYOUT3
    banksel PORTA
    btfss   KEYIN1
    retlw   8
    btfss   KEYIN2
    retlw   9
    btfss   KEYIN3
    retlw   10
    btfss   KEYIN4
    retlw   11
    banksel LATC
    bsf     KEYOUT3

    banksel LATC
    bcf     KEYOUT4
    banksel PORTA
    btfss   KEYIN1
    retlw   12
    btfss   KEYIN2
    retlw   13
    btfss   KEYIN3
    retlw   14
    btfss   KEYIN4
    retlw   15
    banksel LATC
    bsf     KEYOUT4
    
    retlw 0xFF

;   Original  Chip-8    ScanCode     
;   ----------------------------
;   1 2 3 A   1 2 3 C   0 4 8 C 
;   4 5 6 B   4 5 6 D   1 5 9 D 
;   7 8 9 C   7 8 9 E   3 7 B F
;   * 0 # D   A 0 B F   2 6 A E
    

Loop:                       ; We should get here every 159744 cycles
    banksel PORTA
    movlw   0xFF-17
    movwf   FSR0L 
    clrf    INDF0
    incf    FSR0L
    movlw   0x0F
    movwf   INDF0

    movlw   0xFF-9
    movwf   FSR0L 
    clrf    INDF0
    incf    FSR0L
    call    ScanKeyMatrix
    movwf   INDF0

    movlw   0xFF-1
    movwf   FSR0L 
    clrf    INDF0
    incf    FSR0L
    movlw   0xF0
    movwf   INDF0
    
Delay500ms:    
    movlw	0x23
	movwf	d1
	movlw	0xB9
	movwf	d2
	movlw	0x09
	movwf	d3
Delay500ms_0
	decfsz	d1, f
	goto	$+2
	decfsz	d2, f
	goto	$+2
	decfsz	d3, f
	goto	Delay500ms_0

			;6 cycles
	goto	$+1
	goto	$+1
	goto	$+1
    return
    
;****************************************************************************
; Random - 16 bit random number generator
;   Input: <none>
;   Destroys: WREG, tmp
;   Banksel:  1
;****************************************************************************
Random:
;    banksel rndH
;    rrf     rndH,W      ;1; Wreg = Q12
;	xorwf	rndL,W      ;1; Wreg = xor(Q12,Q3)
;	movwf	tmp         ;1; tmp(bit3) = xor(Q12,Q3)
;	swapf	tmp         ;1; tmp(bit7) = xor(Q12,Q3)
;	rlf     rndH        ;1; Wreg = Q14
;	xorwf	rndH        ;1; Wreg = xor(Q15,Q14)
;	xorwf	tmp 		;1; tmp(bit7) = xor(Q15,Q14,Q12,Q3)
;	rlf     tmp,W		;1; cflag = xor(Q15,Q14,Q12,Q3)
;	rlf     rndL        ;1; move bit7 to new bit0 and then..
;	rlf     rndH        ;1; ..rotate RNG value to the left
    return
    
    
;****************************************************************************
; Bin8toAscii - Convert WREG into 3-digit number at dispbuf[0..2]
;   Input: WREG
;   Destroys: WREG, ones,tens
;   Banksel:  1
;****************************************************************************
Bin8toAscii:
;    banksel tens
;    clrf    tens            ;0;
;    decf    tens,F          ;0; Preset 'tens' to -1
;div10
;    movwf   ones            ;0;
;    incf    tens,F          ;0; bump 'tens', 0x00..0x25
;    movlw   6               ;   using "packed bcd" format
;    addwf   tens,W          ;0; bcd "digit carry"?
;    skpndc                  ;   no, skip, else
;    movwf   tens            ;0; fix 'tens'
;    movlw   10              ;   ones = ones - 10
;    subwf   ones,W          ;0; borrow?
;    bc      div10           ;   no, branch, else
;    movfw   ones            ;0;
;    movwf   dispbuf+2       ;c;
;    movfw   tens            ;0;
;    movwf   dispbuf+0       ;c;
;    swapf   dispbuf+0       ;c;
;    movwf   dispbuf+1       ;c;
;    movlw   0x0F        
;    andwf   dispbuf+1       ;c;
;    andwf   dispbuf+0       ;c;
     return
    END