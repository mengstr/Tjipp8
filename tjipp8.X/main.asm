;-----------------------------------------------------------------------------
; Tjipp8 - Chip-8 interpreter for PIC16F1705 with PAL video output 
;-----------------------------------------------------------------------------
;
; The files for the full project are located at 
; github.com/SmallRoomLabs/Tjipp8
;
; Copyright (c) 2016,2017 Mats Engstrom SmallRoomLabs
; Released under the MIT license
;
;

    #include "p16f1705.inc"

    RADIX DEC           ; Use decimal numbers by default
    ERRORLEVEL -302     ; Kill messages about "Register not in bank 0"
    ERRORLEVEL -305     ; Kill messages about "Using default destination"
    
    __CONFIG _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
    __CONFIG _CONFIG2, _WRT_OFF & _PPS1WAY_OFF & _ZCDDIS_ON & _PLLEN_ON & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON

    EXTERN  Interrupt       ; Video.asm
    EXTERN  VideoInit       ; Video.asm
    EXTERN  Chip8Code       ; Chip8Code.asm
    GLOBAL  DTi
    GLOBAL  STi
    GLOBAL  step
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
    
    
#define C8OFS   0x10+0x80
#define VF      Vx+0x0F         ; The VF (Flag register) is the last of the Vx


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
cnt         res 1   ; General counter variable
tmp         res 1   ; General temporary variable
rndH        res 1
rndL        res 1
instH       res 1   ; Current instruction High byte
instL       res 1   ; Current instruction Low byte
xPtr        res 1   ; The "x" part separated out of the current instruction
yPtr        res 1   ; The "y" part separated out of the current instruction
xVal        res 1   ; Value of the varible Vx as pointed to by xPtr
yVal        res 1   ; Value of the varible Vy as pointed to by yPtr
sprite1     res 1   ; Part of the shifted sprite for the left memory cell
sprite2     res 1   ; Part of the shifted sprite for the right memory cell
step        res 1
     
;*******************************************************************************
; Variables in individual bank memories
;*******************************************************************************


;----------------
BANK1 UDATA     ; TRIS/PIE/OPTION_REG/ADCCONx
;----------------
 
;----------------
BANK0 UDATA     ; PORT/TMRx
;----------------

;----------------
BANK2 UDATA     ; LAT/CMx/DAC
;----------------
Vx          res 16  ; Chip-8 Working registers V0..VF
Stack       res 32  ; Chip-8 Stack
I           res 2   ; Chip-8 memory Indexing register
PC          res 2   ; Chip-8 Program Counter
SP          res 1   ; Chip-8 Stack Pointer
DTi         res 1   ; Chip-8 Delay TImer
STi         res 1   ; Chip-8 Sound TImer

         
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
        
        
        
;*******************************************************************************
; Macros
;*******************************************************************************        



;***********************************tat*******************************************
; Reset & Interrupt Vectors
;*******************************************************************************

 
RES_VECT CODE 0x0000        ; Processor reset vector
    pagesel Start
    goto    Start           ; Jump to beginning of program
ISR_VECT CODE 0x0004        ; Interrupt vector 
    pagesel Interrupt
    goto    Interrupt       ; Jump to interrupt code
 

PROG0 CODE    

;****************************************************************************
; Convert a bit number (0..7) to a mask value
;   Input: WREG
;   Destroys:  WREG
;   Banksel:  
;****************************************************************************
BitToMask:
    brw
    dt  1,2,4,8,16,32,64,128

    
InstructionJumpTable:
    swapf   instH,W         ; Take the jump based on the high nybble of instH
    andlw   0x0F
    brw
    goto    OP_0            ; 0
    goto    OP_JP           ; 1
    goto    OP_CALL         ; 2
    goto    OP_SE_NN        ; 3
    goto    OP_SNE_NN       ; 4
    goto    OP_SE           ; 5
    goto    OP_LD_NN        ; 6
    goto    OP_ADD_NN       ; 7
    goto    OP_8_JumpTable  ; 8
    goto    OP_SNE          ; 9
    goto    OP_LD_I_NN      ; A
    goto    OP_JP_V0        ; B
    goto    OP_RND          ; C
    goto    OP_DRW          ; D
    goto    OP_E            ; E
    goto    OP_F            ; F

OP_8_JumpTable:
    movfw   instL       ; Take the jump based on lower nybble of instL
    andlw   0x0F
    brw
    goto    OP_LD       ; 0  LD VX, VY     
    goto    OP_OR       ; 1  OR VX, VY     
    goto    OP_AND      ; 2  AND VX, VY    
    goto    OP_XOR      ; 3  XOR VX, VY    
    goto    OP_ADD      ; 4  ADD VX, VY    
    goto    OP_SUB      ; 5  SUB VX, VY    
    goto    OP_SHR      ; 6  SHR VX        
    goto    OP_SUBN     ; 7  SUBN VX, VY   
    goto    OP_CLS      ; 8  Clear screen if bad opcode
    goto    OP_CLS      ; 9  Clear screen if bad opcode
    goto    OP_CLS      ; A  Clear screen if bad opcode
    goto    OP_CLS      ; B  Clear screen if bad opcode
    goto    OP_CLS      ; C  Clear screen if bad opcode
    goto    OP_CLS      ; D  Clear screen if bad opcode
    goto    OP_SHL      ; E  SHL VX  
    goto    OP_CLS      ; F  Clear screen if bad opcode
    

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************
MAIN_PROG
    
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

    movlw   0x55
    movwf   rndL
    movlw   0xAA
    movwf   rndH
    
    call    VideoInit
    
;    include "checkerboard.inc"
;    include "hadlogo.inc"

    bcf     SOUND

    banksel LATA
    call    Chip8Reset

MainLoop:
    banksel PC

    btfss   step,0
    goto    MainLoop
    bcf     step,0
    
    ; Load FSR with PC ofsetted for the Chip8Code-area
    movfw   PC+0        
    addlw   high(Chip8Code)
    addlw   0x80
    movwf   FSR0H
    movfw   PC+1
    movwf   FSR0L
    
    ; Get Chip-8 instruction from code area
    moviw   INDF0++     
    movwf   instH
    moviw   INDF0++
    movwf   instL

    ;Pre-parse / Pre-load the x and y parts of the instruction
    clrf    FSR0H

    movfw   instH       ; x is in low nybble of instH
    andlw   0x0F
    addlw   LOW(Vx)
    movwf   FSR0L
    movwf   xPtr        ; Store the pointer if needed later
    movfw   INDF0
    movwf   xVal        ; Store the data the pointer points to
    
    swapf   instL,W     ; y is in high nybble of instL
    andlw   0x0F
    addlw   LOW(Vx)
    movwf   FSR0L
    movwf   yPtr        ; Store the pointer if needed later
    movfw   INDF0
    movwf   yVal        ; Store the data the pointer points to
    
    ; Increment PC by 2 since all intructions are two bytes long
    incf    PC+1        
    incf    PC+1
    btfsc   STATUS,Z
    incf    PC+0
    
    ; Execute instruction
    call    InstructionJumpTable
    goto    MainLoop
    

    
    
Chip8Reset:
    banksel PC
    call   OP_CLS
    clrf    Vx+0
    clrf    Vx+1
    clrf    Vx+2
    clrf    Vx+3
    clrf    Vx+4
    clrf    Vx+5
    clrf    Vx+6
    clrf    Vx+7
    clrf    Vx+8
    clrf    Vx+9
    clrf    Vx+10
    clrf    Vx+11
    clrf    Vx+12
    clrf    Vx+13
    clrf    Vx+14
    clrf    Vx+15
    clrf    Stack+0
    clrf    Stack+1
    clrf    Stack+2
    clrf    Stack+3
    clrf    Stack+4
    clrf    Stack+5
    clrf    Stack+6
    clrf    Stack+7
    clrf    Stack+8
    clrf    Stack+9
    clrf    Stack+10
    clrf    Stack+11
    clrf    Stack+12
    clrf    Stack+13
    clrf    Stack+14
    clrf    Stack+15
    clrf    Stack+16
    clrf    Stack+17
    clrf    Stack+18
    clrf    Stack+19
    clrf    Stack+20
    clrf    Stack+21
    clrf    Stack+22
    clrf    Stack+23
    clrf    Stack+24
    clrf    Stack+25
    clrf    Stack+26
    clrf    Stack+27
    clrf    Stack+28
    clrf    Stack+29
    clrf    Stack+30
    clrf    Stack+31
    clrf    I+0
    clrf    I+1
    movlw   0x02
    movwf   PC+0
    clrf    PC+1
    clrf    SP
    clrf    DTi
    clrf    STi
    return
    
    
;   Original  Chip-8    ScanCode     
;   ----------------------------
;   1 2 3 A   1 2 3 C   0 4 8 C 
;   4 5 6 B   4 5 6 D   1 5 9 D 
;   7 8 9 C   7 8 9 E   3 7 B F
;   * 0 # D   A 0 B F   2 6 A E
ScanKeyMatrix:
    banksel LATC
    bcf     KEYOUT1
    bsf     KEYOUT2
    bsf     KEYOUT3
    bsf     KEYOUT4
    banksel PORTA
    btfss   KEYIN1
    retlw   1;
    btfss   KEYIN2
    retlw   4;
    btfss   KEYIN3
    retlw   10;
    btfss   KEYIN4
    retlw   7;
    banksel LATC
    bsf     KEYOUT1
    ;
    bcf     KEYOUT2
    banksel PORTA
    btfss   KEYIN1
    retlw   2;
    btfss   KEYIN2
    retlw   5;
    btfss   KEYIN3
    retlw   0;
    btfss   KEYIN4
    retlw   8;
    banksel LATC
    bsf     KEYOUT2
    ;
    bcf     KEYOUT3
    banksel PORTA
    btfss   KEYIN1
    retlw   3;
    btfss   KEYIN2
    retlw   6;
    btfss   KEYIN3
    retlw   11;
    btfss   KEYIN4
    retlw   9;
    banksel LATC
    bsf     KEYOUT3
    ;
    bcf     KEYOUT4
    banksel PORTA
    btfss   KEYIN1
    retlw   12;
    btfss   KEYIN2
    retlw   13;
    btfss   KEYIN3
    retlw   15;
    btfss   KEYIN4
    retlw   14;
    banksel LATC
    bsf     KEYOUT4
    
    retlw 0xFF

    
    
;****************************************************************************
; Random - 16 bit random number generator
;   Input: <none>
;   Destroys: WREG, tmp
;   Banksel:  
;****************************************************************************
Random:
    rrf     rndH,W      ;1; Wreg = Q12
	xorwf	rndL,W      ;1; Wreg = xor(Q12,Q3)
	movwf	tmp         ;1; tmp(bit3) = xor(Q12,Q3)
	swapf	tmp         ;1; tmp(bit7) = xor(Q12,Q3)
	rlf     rndH        ;1; Wreg = Q14
	xorwf	rndH        ;1; Wreg = xor(Q15,Q14)
	xorwf	tmp 		;1; tmp(bit7) = xor(Q15,Q14,Q12,Q3)
	rlf     tmp,W		;1; cflag = xor(Q15,Q14,Q12,Q3)
	rlf     rndL        ;1; move bit7 to new bit0 and then..
	rlf     rndH        ;1; ..rotate RNG value to the left
    movwf   rndL
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

; 7654 3211
; -0-- ----  1011 .... 00B.  SCU N        
; --00 ----  1100 .... 00C.  SCD N        
; ---- 0---  1110 0000 00E0  CLS           
; ---0 ----  1110 1110 00EE  RET           
; ---- -0--  1111 1011 00FB  SCR          
; ---- ----  1111 1100 00FC  SCL 
OP_0:        ; 0
    btfss   instL,6
    goto    OP_SCU
    btfsc   instL,5
    goto    OP_0a
    btfss   instL,4
    goto    OP_SCD
OP_0a
    btfss   instL,3
    goto    OP_CLS
    btfss   instL,4
    goto    OP_RET
    btfss   instL,2
    goto    OP_SCR
    goto    OP_SCL

    
; --0- ---- 1001 1101  9E  SKP VX        
; ---0 ---- 1010 0001  A1  SKNP VX     
OP_E:        ; E
    btfss   instL,5
    goto    OP_SKP
    goto    OP_SKNP

; 7654 3210  7654 3210    
;======================================
; ---- ----  0101 0101  55  LD [I], VX    
; ---- ----  0110 0101  65  LD VX, [I]    
;
; ---- ----  0010 1001  29  LD F, VX      
; ---- ----  0011 0000  30  LD HF, VX    
; ---- ----  0011 0011  33  LD B, VX      
;
; ---- ----  0000 0111  07  LD VX, DT     
; ---- ----  0000 1010  0A  LD VX, K      
;
; ---- ----  0001 0101  15  LD DT, VX     
;    
; ---- ----  0001 1000  18  LD ST, VX     
; ---- ----  0001 1110  1E  ADD I, VX  

OP_F:        ; F
    ;07/0A/15/18/1E/29/30/33/55/65
    btfss   instL,6
    goto    OP_f1       
    ;55/65
    btfss   instL,5     
    goto    OP_F_55
    goto    OP_F_65
OP_f1                   
    ;07/0A/15/18/1E/29/30/33
    btfss   instL,5
    goto    OP_f2       
    ;29/30/33
    btfss   instL,4     
    goto    OP_F_29
    ;30/33
    btfss   instL,2
    goto    OP_F_30
    goto    OP_F_33
OP_f2                   
    ;07/0A/15/18/1E    
    btfsc   instL,4
    goto    OP_f3
    ;07/0A
    btfss   instL,3
    goto    OP_F_07
    goto    OP_F_0A
OP_f3
    ;15/18/1E
    btfss   instL,3
    goto    OP_F_15
    btfss   instL,2
    goto    OP_F_18
    goto    OP_F_1E


    
;    
; 0 n n n - SYS addr
; 0 0 E 0 - CLS
; 0 0 E E - RET
; 1 n n n - JP addr
; 2 n n n - CALL addr
; 3 x k k - SE Vx, byte
; 4 x k k - SNE Vx, byte
; 5 x y 0 - SE Vx, Vy
; 6 x k k - LD Vx, byte
; 7 x k k - ADD Vx, byte
; 8 x y 0 - LD Vx, Vy
; 8 x y 1 - OR Vx, Vy
; 8 x y 2 - AND Vx, Vy
; 8 x y 3 - XOR Vx, Vy
; 8 x y 4 - ADD Vx, Vy
; 8 x y 5 - SUB Vx, Vy
; 8 x y 6 - SHR Vx {, Vy}
; 8 x y 7 - SUBN Vx, Vy
; 8 x y E - SHL Vx {, Vy}
; 9 x y 0 - SNE Vx, Vy
; A n n n - LD I, addr
; B n n n - JP V0, addr
; C x k k - RND Vx, byte
; D x y n - DRW Vx, Vy, nibble
; E x 9 E - SKP Vx
; E x A 1 - SKNP Vx
; F x 0 7 - LD Vx, DT
; F x 0 A - LD Vx, K
; F x 1 5 - LD DT, Vx
; F x 1 8 - LD ST, Vx
; F x 1 E - ADD I, Vx
; F x 2 9 - LD F, Vx
; F x 3 3 - LD B, Vx
; F x 5 5 - LD [I], Vx
; F x 6 5 - LD Vx, [I] 
; 
    
    
    
    
;    
; 00BN  SCU N
; ==========
; Scroll up N pixels (N/2 pixels in lores mode)
;    
OP_SCU:
    goto    OP_UNIMPLEMENTED
    return
    
    
;    
; 00CN  SCD N         
; ==========
; Scroll down N pixels (N/2 pixels in lores mode)
;    
OP_SCD:
    goto    OP_UNIMPLEMENTED
    return
    
  
;    
; 00E0 - CLS
; ==========
; Clear the display.
;    
OP_CLS:
    movlw   HIGH(videobuf)
    movwf   FSR0H
    clrf    FSR0L
    clrf    INDF0
    incfsz  FSR0L
    goto    $-2
    return
    
;    
; 00EE - RET
; ==========
; Return from a subroutine.
;
; The interpreter sets the program counter to the address at the top of 
; the stack, then subtracts 1 from the stack pointer.
;
OP_RET: 
    decf    SP          ; Point to last used entry in stack

    movlw   HIGH(Stack) ; Setup the index to access the entry 
    movwf   FSR0H
    movlw   LOW(Stack)
    addwf   SP,W
    addwf   SP,W
    movwf   FSR0L

    moviw   INDF0++     ; Load PC with address from stack
    movwf   PC+0        
    moviw   INDF0++
    movwf   PC+1        

    return
    
    
;
; 00FB  SCR           
; ==========
; Scroll right 2 pixels
;
OP_SCR:
    goto    OP_UNIMPLEMENTED
    return
    
    
;
; 00FC  SCL           
; ==========
; Scroll left 2 pixels
;
OP_SCL:
    goto    OP_UNIMPLEMENTED
    return
    
        
;
; 1nnn - JP addr
; ==============
; Jump to location nnn.
; 
; The interpreter sets the program counter to nnn.
;   
OP_JP:
    movfw   instL
    movwf   PC+1
    movfw   instH
    andlw   0x0F
    movwf   PC+0
    return
    

;    
; 2nnn - CALL addr
; ================
; Call subroutine at nnn.
;
; The interpreter increments the stack pointer, then puts the current PC 
; on the top of the stack. The PC is then set to nnn.
;    
OP_CALL:
    movlw   HIGH(Stack) ; Point to unused space in Stack
    movwf   FSR0H
    movlw   LOW(Stack)
    addwf   SP,W
    addwf   SP,W
    movwf   FSR0L

    movfw   PC+0        ; Store current PC to Stack
    movwi   INDF0++
    movfw   PC+1
    movwi   INDF0++
    
    incf    SP          ; Bump up stack pointer

    goto    OP_JP       ; Now we can just do a regular jump

;    
; 3xkk - SE Vx, byte
; ==================
; Skip next instruction if Vx = kk.
;
; The interpreter compares register Vx to kk, and if they are equal, 
; increments the program counter by 2.
; 
OP_SE_NN:    ; 3...
    movfw   instL       ; Move kk to Vy...
    movwf   yVal
    goto    OP_SE       ; ...and execute the OP_SE that compares Vx and Vy

    
    
; 
; 4xkk - SNE Vx, byte
; ===================
; Skip next instruction if Vx != kk.
;
; The interpreter compares register Vx to kk, and if they are not equal, 
; increments the program counter by 2.
; 
OP_SNE_NN: 
    movfw   instL       ; Move kk to Vy...
    movwf   yVal
    goto    OP_SNE      ; ...and execute the OP_SNE that compares Vx and Vy
    
    
; 
; 5xy0 - SE Vx, Vy
; ================
; Skip next instruction if Vx = Vy.
;
; The interpreter compares register Vx to register Vy, and if they are equal, 
; increments the program counter by 2.
; 
OP_SE:      
    movfw   yVal
    xorwf   xVal,W
    btfss   STATUS,Z    ; Test if equal
    return
    incf    PC+1
    incf    PC+1
    btfsc   STATUS,Z
    incf    PC+0
    return
    
    
; 
; 6xkk - LD Vx, byte
; ==================
; Set Vx = kk.
;
; The interpreter puts the value kk into register Vx.
; 
OP_LD_NN:
    movfw   xPtr
    movwf   FSR0L
    movfw   instL    ; Store kk part to register
    movwf   INDF0
    return
    
    
; 
; 7xkk - ADD Vx, byte
; ===================
; Set Vx = Vx + kk.
;
; Adds the value kk to the value of register Vx, then stores the result 
; in Vx.
; 
OP_ADD_NN:
    movfw   xPtr
    movwf   FSR0L
    movfw   instL    ; Add kk part to register
    addwf   INDF0
    return
    
    
; 
; 8xy0 - LD Vx, Vy
; ================
; Set Vx = Vy.
;
; Stores the value of register Vy in register Vx.
; 
OP_LD:     
    movfw   xPtr
    movwf   FSR0L
    movfw   yVal
    movwf   INDF0
    return
    
    
; 
; 8xy1 - OR Vx, Vy
; ================
; Set Vx = Vx OR Vy.
;
; Performs a bitwise OR on the values of Vx and Vy, then stores the result 
; in Vx. A bitwise OR compares the corrseponding bits from two values, and 
; if either bit is 1, then the same bit in the result is also 1. Otherwise,
; it is 0.
; 
OP_OR:     
    movfw   xPtr
    movwf   FSR0L
    movfw   xVal
    iorwf   yVal,W
    movwf   INDF0
    return
    
    
; 
; 8xy2 - AND Vx, Vy
; =================
; Set Vx = Vx AND Vy.
;
; Performs a bitwise AND on the values of Vx and Vy, then stores the result 
; in Vx. A bitwise AND compares the corrseponding bits from two values, and 
; if both bits are 1, then the same bit in the result is also 1. Otherwise, 
; it is 0.
; 
OP_AND:
    movfw   xPtr
    movwf   FSR0L
    movfw   xVal
    andwf   yVal,W
    movwf   INDF0
    return
    
    
; 
; 8xy3 - XOR Vx, Vy
; =================
; Set Vx = Vx XOR Vy.
;
; Performs a bitwise exclusive OR on the values of Vx and Vy, then stores 
; the result in Vx. An exclusive OR compares the corrseponding bits from 
; two values, and if the bits are not both the same, then the corresponding 
; bit in the result is set to 1. Otherwise, it is 0.
; 
OP_XOR:
    movfw   xPtr
    movwf   FSR0L
    movfw   xVal
    xorwf   yVal,W
    movwf   INDF0
   
    return
    
    
; 
; 8xy4 - ADD Vx, Vy
; =================
; Set Vx = Vx + Vy, set VF = carry.
;
; The values of Vx and Vy are added together. If the result is greater than 
; 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits
; of the result are kept, and stored in Vx.
; 
OP_ADD:    
    movfw   xPtr
    movwf   FSR0L
    clrf    VF
    movfw   xVal
    addwf   yVal,W
    movwf   INDF0
    btfsc   STATUS,C
    incf    VF
    return
    
    
; 
; 8xy5 - SUB Vx, Vy
; =================
; Set Vx = Vx - Vy, set VF = NOT borrow.
;
; If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from 
; Vx, and the results stored in Vx.
; 
OP_SUB:
    movfw   xPtr
    movwf   FSR0L
    clrf    VF
    movfw   yVal
    subwf   xVal,W
    movwf   INDF0
    btfsc   STATUS,C
    incf    VF
    return
    
    
; 
; 8xy6 - SHR Vx {, Vy}
; ====================
; Set Vx = Vx SHR 1.
;
; If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. 
; Then Vx is divided by 2.
; 
OP_SHR:
    movfw   xPtr
    movwf   FSR0L
    clrf    VF
    lsrf    xVal,W
    movwf   INDF0
    btfsc   STATUS,C
    incf    VF
    return
    
    
; 
; 8xy7 - SUBN Vx, Vy
; ==================
; Set Vx = Vy - Vx, set VF = NOT borrow.
;
; If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from 
; Vy, and the results stored in Vx.
; 
OP_SUBN:
    movfw   xPtr
    movwf   FSR0L
    clrf    VF
    movfw   xVal
    subwf   yVal,W
    movwf   INDF0
    btfsc   STATUS,C
    incf    VF
    return
    
    
; 
; 8xyE - SHL Vx {, Vy}
; ====================
; Set Vx = Vx SHL 1.
;
; If the most-significant bit of Vx is 1, then VF is set to 1, otherwise 
; to 0. Then Vx is multiplied by 2.
; 
OP_SHL: 
    movfw   xPtr
    movwf   FSR0L
    clrf    VF
    lslf    xVal,W
    movwf   INDF0
    btfsc   STATUS,C
    incf    VF
    return
    
    
; 
; 9xy0 - SNE Vx, Vy
; =================
; Skip next instruction if Vx != Vy.
;
; The values of Vx and Vy are compared, and if they are not equal, the 
; program counter is increased by 2.
; 
OP_SNE:      
    movfw   yVal
    xorwf   xVal,W
    btfsc   STATUS,Z    ; Test if not equal
    return
    incf    PC+1
    incf    PC+1
    btfsc   STATUS,Z
    incf    PC+0
    return
; 
; Annn - LD I, addr
; =================
; Set I = nnn.
;
; The value of register I is set to nnn.
;    
OP_LD_I_NN:
    movfw   instL
    movwf   I+1
    movfw   instH
    andlw   0x0F
    movwf   I+0
    return
    
    
; 
; Bnnn - JP V0, addr
; ==================
; Jump to location nnn + V0.
;
; The program counter is set to nnn plus the value of V0.
; 
OP_JP_V0:    ; B
    movfw   instL       ; Copy nnn to PC
    movwf   PC+1
    movfw   instH
    andlw   0x0F
    movwf   PC+0

    movfw   Vx+0        ; Add V0 to PC
    addwf   PC+1
    btfsc   STATUS,C
    incf    PC+0
    return
    
    
; 
; Cxkk - RND Vx, byte
; ===================
; Set Vx = random byte AND kk.
;
; The interpreter generates a random number from 0 to 255, which is then 
; ANDed with the value kk. The results are stored in Vx. See instruction 
; 8xy2 for more information on AND.
; 
OP_RND:  
    movfw   xPtr
    movwf   FSR0L
    call    Random
    andwf   instL,W
    movwf   INDF0
    return

    
; 
; Dxyn - DRW Vx, Vy, nibble
; =========================
; Display n-byte sprite starting at memory location I at (Vx, Vy), 
; set VF = collision.
;
; The interpreter reads n bytes from memory, starting at the address stored 
; in I. These bytes are then displayed as sprites on screen at coordinates 
; (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any 
; pixels to be erased, VF is set to 1, otherwise it is set to 0. If the 
; sprite is positioned so part of it is outside the coordinates of the 
; display, it wraps around to the opposite side of the screen. See 
; instruction 8xy3 for more information on XOR, and section 2.4, Display, 
; for more information on the Chip-8 screen and sprites.
; 
OP_DRW:
    clrf    VF          ; Clear collision bit
    movfw   instL       ; Copy "nibble" to cnt for looping
    andlw   0x0F
    movwf   cnt

    ; Video memory for the destination starts at Vx/8 +Vy*8
    ; set INDF0 to point there
    movlw   HIGH(videobuf)
    movwf   FSR0H
    lsrf    xVal,W      ; Calculate Vx/8
    lsrf    WREG,W
    lsrf    WREG,W
    movwf   FSR0L
    lslf    yVal,W      ; Calculate Xy*8
    lslf    WREG,W
    lslf    WREG,W
    addwf   FSR0L
    
    ; The I register points to the sprite data
    ; copy I to INDF1 
    movfw   I+0
    addlw   high(Chip8Code)
    addlw   0x80
    movwf   FSR1H
    movfw   I+1
    movwf   FSR1L
    
dr0 moviw   INDF1++     ; Get the sprite byte
    ; Reverse the bit order into the sprite1 variable
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    rlf     WREG 	 
    rrf     sprite1
    ; copy sprite1 into sprite2
    movfw   sprite1
    movwf   sprite2
    ; Shift sprite1 (x mod 8) steps to the left
    movfw   xVal
    andlw   0x07
    sublw   8
    brw
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    lslf    sprite1
    ; Shift sprite2 8-(x mod 8) steps to the right
    movfw   xVal
    andlw   0x07
    brw
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    lsrf    sprite2
    
    ; Now display (xored) both sprite(parts) on the screen, but first
    ; test if some pixels on screen are overlapping with sprites pixels
    movfw   INDF0
    andwf   sprite1,W
    btfss   STATUS,Z
    bsf     Vx,0
    movfw   INDF0
    xorwf   sprite1,W
    movwi   FSR0++

    movfw   INDF0
    andwf   sprite2,W
    btfss   STATUS,Z
    bsf     Vx,0
    movfw   INDF0
    xorwf   sprite2,W
    movwi   FSR0++

    movlw   6           ; Go down to next line
    addwf   FSR0L
    decfsz  cnt
    goto    dr0

    return

    
; 
; Ex9E - SKP Vx
; =============
; Skip next instruction if key with the value of Vx is pressed.
;
; Checks the keyboard, and if the key corresponding to the value of Vx is 
; currently in the down position, PC is increased by 2.
; 
OP_SKP:
    call    ScanKeyMatrix
    banksel PC
    xorwf   xVal
    btfss   STATUS,Z
    return
    incf    PC+1
    incf    PC+1
    btfsc   STATUS,Z
    incf    PC+0
    return
    

; 
; ExA1 - SKNP Vx
; ==============
; Skip next instruction if key with the value of Vx is not pressed.
;
; Checks the keyboard, and if the key corresponding to the value of Vx is 
; currently in the up position, PC is increased by 2.
; 
OP_SKNP:
    call    ScanKeyMatrix
    banksel PC
    xorwf   xVal
    btfsc   STATUS,Z
    return
    incf    PC+1
    incf    PC+1
    btfsc   STATUS,Z
    incf    PC+0
    return

    
; 
; Fx07 - LD Vx, DT
; ==============
; Set Vx = delay timer value.
;
; The value of DT is placed into Vx.
;
OP_F_07:  
    movfw   xPtr
    movwf   FSR0L
    movfw   DTi
    movwf   INDF0
    return
    
    
; 
; Fx0A - LD Vx, K
; ==============
; Wait for a key press, store the value of the key in Vx.
;
; All execution stops until a key is pressed, then the value of that 
; key is stored in Vx.
; 
OP_F_0A:  
    movfw   xPtr
    movwf   FSR0L
    call    ScanKeyMatrix
    banksel PC
    movwf   INDF0
    xorlw   0xFF
    btfsc   STATUS,Z
    goto    OP_F_0A
    return

    
; Fx15 - LD DT, Vx
; ==============
; Set delay timer = Vx.
;
; DT is set equal to the value of Vx.
; 
OP_F_15:    ; 
    movfw   xVal
    movwf   DTi
    return
    
    
; 
; Fx18 - LD ST, Vx
; ==============
; Set sound timer = Vx.
;
; ST is set equal to the value of Vx.
; 
OP_F_18:    ; 
    movfw   xVal
    movwf   STi
    return

     
; 
; Fx1E - ADD I, Vx
; Set I = I + Vx.
; ==============
; The values of I and Vx are added, and the results are stored in I.
; 
OP_F_1E:    ; 
    movfw   xVal
    addwf   I+1
    btfsc   STATUS,C
    incf    I+0
    return



; 
; Fx29 - LD F, Vx
; ==============
; Set I = location of sprite for digit Vx.
;
; The value of I is set to the location for the hexadecimal sprite 
; corresponding to the value of Vx. 
; 
OP_F_29:    ; 
    clrf    I+0     ; High part of I = 0
    lslf    xVal,W  ; Low part of I = xVal*5
    lslf    WREG,W
    addwf   xVal,W
    movwf   I+1
    return


; 
; 
OP_F_30:    ; 
    goto    OP_UNIMPLEMENTED
    return
    
    
; 
; Fx33 - LD B, Vx
; ==============
; Store BCD representation of Vx in memory locations I, I+1, and I+2.
;
; The interpreter takes the decimal value of Vx, and places the 
; hundreds digit in memory at location in I, the tens digit at location 
; I+1, and the ones digit at location I+2.
; 
OP_F_33:    ; 
    goto    OP_UNIMPLEMENTED
    return

    
    
; 
; Fx55 - LD [I], Vx
; ==============
; Store registers V0 through Vx in memory starting at location I.
;
; The interpreter copies the values of registers V0 through Vx into 
; memory, starting at the address in I.
; 
OP_F_55:    ; 
    goto    OP_UNIMPLEMENTED
    return

    
; 
; Fx65 - LD Vx, [I]
; ==============
; Read registers V0 through Vx from memory starting at location I.
;
; The interpreter reads values from memory starting at location I into 
; registers V0 through Vx.     
; 
OP_F_65:
    goto    OP_UNIMPLEMENTED
    return

OP_UNIMPLEMENTED:
    nop
    return
     
     END