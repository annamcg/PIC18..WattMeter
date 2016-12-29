;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WATT METER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; this program calculates a value of the DC power for a simple ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; external load ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Program outline

;  Initialize configuration registers and variables and include libraries and downloaded subroutines
;  Set ADC converter to start conversion from port E2
;  Convert 10 bit ADC output to a binary number corresponding to measured node voltage
;  Calculate load voltage
;  Multiply voltage by current to get power
;  Convert binary power to decimal
;  Display decimal value in milliWatts on LCD screen



;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        list  P=PIC18F4520, F=INHX32, C=160, N=0, ST=OFF, MM=OFF, R=DEC, X=ON
        #include <P18F4520.inc>
        __CONFIG  _CONFIG1H, _OSC_HS_1H  ;HS oscillator
        __CONFIG  _CONFIG2L, _PWRT_ON_2L & _BOREN_ON_2L & _BORV_2_2L  ;Reset
        __CONFIG  _CONFIG2H, _WDT_OFF_2H  ;Watchdog timer disabled
        __CONFIG  _CONFIG3H, _CCP2MX_PORTC_3H  ;CCP2 to RC1 (rather than to RB3)
        __CONFIG  _CONFIG4L, _LVP_OFF_4L & _XINST_OFF_4L  ;RB5 enabled for I/O
        errorlevel -314, -315          ;Ignore lfsr messages

;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         cblock  0x000                  ;Beginning of Access RAM
        TMR0LCOPY                      ;Copy of sixteen-bit Timer0 used by LoopTime
        TMR0HCOPY
        INTCONCOPY                     ;Copy of INTCON for LoopTime subroutine
        COUNT                          ;Counter available as local to subroutines
        ALIVECNT                       ;Counter for blinking "Alive" LED
        BYTE                           ;Eight-bit byte to be displayed
        BYTESTR:10                     ;Display string for binary version of BYTE

        TEMPSTR:6 

	NODE1_0
	NODE1_1
	f00
        f01
        f02
        f03
        f0
        f1
        f2
        f3
		
        endc

#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\MATHVARS.inc>


;;;;;;; Macro definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MOVLF   macro  literal,dest
        movlw  literal
        movwf  dest
        endm

POINT   macro  stringname
        MOVLF  high stringname, TBLPTRH
        MOVLF  low stringname, TBLPTRL
        endm

DISPLAY macro  register
        movff  register,BYTE
        call  ByteDisplay
        endm

DISPLAY2 macro  register
        movff  register,BYTE
        call  ByteDisplay2
        endm

;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        org  0x0000                    ;Reset vector
        nop 
        goto  Mainline

        org  0x0008                    ;High priority interrupt vector
        goto  $                        ;Trap

        org  0x0018                    ;Low priority interrupt vector
        goto  $           
			
;;;;;;;;;;;;;;;;;;;;;; Mainline		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Mainline

	rcall  Initial                 ;Initialize everything
L1
    bsf ADCON0,1
	  
	Continue_conversion
		btfsc ADCON0, 1
		bra Continue_conversion
	

	;;I_load_subroutine start


	movff ADRESH, AARGB0 
	movff ADRESL, AARGB1 
	MOVLF 0x13, BARGB0 				; multiply by 5000	
	MOVLF 0x88, BARGB1 			
	call  FXM1616U


	MOVLF  0x04,BARGB0             ; Put 1024 into the divisor.
        MOVLF  0x00,BARGB1                      
        call   FXD3216U      		   ; Now node 1 voltage multiplied by 1000 is stored in values AARGB0:AARGB3

	movff AARGB2, NODE1_0			;moving values of NODE1 into variables NODE1_0:NODE1_1
	movff AARGB3, NODE1_1


 ;      Since our supplement resistor value is set to 1 kOhm by design, we are not getting the current value at this stage, but are leaving 
 ;	it as voltage to deal with decimal point location at the end of the code

 ;	MOVLF  0x03,BARGB0             ; Put 1000 into the divisor
 ;      MOVLF  0xE8,BARGB1                      
 ;      call   FXD3216U  



	;; V_load


 		MOVLF  0x13,f0
		MOVLF  0x88,f1

		movlw  B'11111111'	;flips binary digits of measured node 1 value
		xorwf  NODE1_0, W 
		movwf	f02 
		movlw  B'11111111'	;flips binary digits of measured node 1 value
		xorwf  NODE1_1, W 
		movwf	f03 

		clrf WREG

			bcf STATUS,C
			movlw  B'00000001'
			addwf f03
			movlw  B'00000000'
			addwfc f02

		clrf WREG

;subtract V1 from battery

			bcf STATUS,C
			movf f1,W
			addwf f03
			movf f0,W
			addwfc f02

;Now load voltage is in variables f02:f03

;multiply voltage with current (since our divider resistor value is 1000 ohm, we did not manipulate the measured voltage value,...
; so current is higher by 1000.^3)

	movff NODE1_0, AARGB0 
	movff NODE1_1, AARGB1 
	movff f02, BARGB0 			
	movff f03, BARGB1 			
	call  FXM1616U

;divide result by 10.^6 to get power result in milliWatts

	MOVLF  0x0F,BARGB0             ; Put 10.^6 into the divisor.
        MOVLF  0x42,BARGB1    
	MOVLF  0x40,BARGB1                   
        call   FXD3224U 

;;;;;; DecDisplay subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This subroutine takes BYTE, converts it to an ASCII decimal value of BYTE and displays
; it.  This function does not set the positioning code.  For the temperature, the code
; is constant, so the position is initialized in the Initial subroutine.

	 movff  AARGB3,BYTE

         lfsr 0, TEMPSTR+2              ; Loads FSR0 register to BYTESTR + 2.

       
L44
          clrf  WREG                   ; Clear work register.
          movff BYTE, AARGB0           ; Move BYTE to AARGB0 to be divided.
          MOVLF D'10', BARGB0          ; Divide BYTE by 10.
          call  FXD0808U               ; Perform division.
          movf  REMB0, W               ; Move remainder to work register.
          
          iorlw 0x30                   ; Add offset to convert to an ASCII decimal number.
          movwf POSTDEC0               ; Load the ASCII value to the string and move to next string byte.
          
          movff AARGB0, BYTE           ; Move result to divisor to be divided again.	
          movf  FSR0L,W                ; Done?
          sublw low TEMPSTR
        ;UNTIL_ .Z.			
        bnz	L44
RL44
        
        lfsr 0, TEMPSTR                ; Set pointer to display temperature string: TEMPSTR.
        rcall   DisplayV               ; Call DisplayV to display temperature, a variable string.
        
bra L1
	

;;;;;;;;;;;;;;;;;;;;;; Initial subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Initial
	MOVLF  B'00011101',ADCON0
        MOVLF  B'10001110',ADCON1      ;Enable PORTA & PORTE digital I/O pins
	MOVLF  B'10111101',ADCON2
        MOVLF  B'11100001',TRISA       ;Set I/O for PORTA
        MOVLF  B'11011100',TRISB       ;Set I/O for PORTB
        MOVLF  B'11010000',TRISC       ;Set I/0 for PORTC
        MOVLF  B'00001111',TRISD       ;Set I/O for PORTD
        MOVLF  B'00000000',TRISE       ;Set I/O for PORTE
        MOVLF  B'10001000',T0CON       ;Set up Timer0 for a looptime of 10 ms
        MOVLF  B'00010000',PORTA       ;Turn off all four LEDs driven from PORTA


	
		
	

 	MOVLF 0xC0, TEMPSTR            ; Sets the position of TEMPSTR to the lower left hand corner.
      
        MOVLF A'm', TEMPSTR+3          ; Displays ‘m’ for ‘milli’ units.
	MOVLF A'W', TEMPSTR+4          ; Displays ‘W’ for ‘Watt’.
        MOVLF 0x00, TEMPSTR+5          ; Terminating byte for a string.


        rcall  InitLCD
        return


;;;;;;; InitLCD subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (InitLCD initializes LCD)
; Initialize the Optrex 8x2 character LCD.
; First wait for 0.1 second, to get past display's power-on reset time.

InitLCD
        MOVLF  10,COUNT                ;Wait 0.1 second
        ;REPEAT_
L2
          rcall  LoopTime              ;Call LoopTime 10 times
          decf  COUNT,F
        ;UNTIL_  .Z.
        bnz	L2
RL2

        bcf  PORTE,0                   ;RS=0 for command
        POINT  LCDstr                  ;Set up table pointer to initialization string
        tblrd*                         ;Get first byte from string into TABLAT
        ;REPEAT_
L3
          bsf  PORTE,1                 ;Drive E high
          movff  TABLAT,PORTD          ;Send upper nibble
          bcf  PORTE,1                 ;Drive E low so LCD will process input
          rcall  LoopTime              ;Wait ten milliseconds
          bsf  PORTE,1                 ;Drive E high
          swapf  TABLAT,W              ;Swap nibbles
          movwf  PORTD                 ;Send lower nibble
          bcf  PORTE,1                 ;Drive E low so LCD will process input
          rcall  LoopTime              ;Wait ten milliseconds
          tblrd+*                      ;Increment pointer and get next byte
          movf  TABLAT,F               ;Is it zero?
        ;UNTIL_  .Z.
        bnz	L3
RL3
        return
;;;;;;; T40 subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pause for 40 microseconds  or 40/0.4 = 100 clock cycles.
; Assumes 10/4 = 2.5 MHz internal clock rate.

T40
        movlw  100/3                   ;Each REPEAT loop takes 3 cycles
        movwf  COUNT
        ;REPEAT_
L4
          decf  COUNT,F
        ;UNTIL_  .Z.
        bnz	L4
RL4
        return


;;;;;;;;DisplayC subroutine;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; This subroutine is called with TBLPTR containing the address of a constant
; display string.  It sends the bytes of the string to the LCD.  The first
; byte sets the cursor position.  The remaining bytes are displayed, beginning
; at that position.
; This subroutine expects a normal one-byte cursor-positioning code, 0xhh, or
; an occasionally used two-byte cursor-positioning code of the form 0x00hh.

DisplayC
        bcf  PORTE,0                   ;Drive RS pin low for cursor-positioning code
        tblrd*                         ;Get byte from string into TABLAT
        movf  TABLAT,F                 ;Check for leading zero byte
        ;IF_  .Z.
        bnz	L5
          tblrd+*                      ;If zero, get next byte
        ;ENDIF_
L5
        ;REPEAT_
L6
          bsf  PORTE,1                 ;Drive E pin high
          movff  TABLAT,PORTD          ;Send upper nibble
          bcf  PORTE,1                 ;Drive E pin low so LCD will accept nibble
          bsf  PORTE,1                 ;Drive E pin high again
          swapf  TABLAT,W              ;Swap nibbles
          movwf  PORTD                 ;Write lower nibble
          bcf  PORTE,1                 ;Drive E pin low so LCD will process byte
          rcall  T40                   ;Wait 40 usec
          bsf  PORTE,0                 ;Drive RS pin high for displayable characters
          tblrd+*                      ;Increment pointer, then get next byte
          movf  TABLAT,F               ;Is it zero?
        ;UNTIL_  .Z.
        bnz	L6
RL6
        return


;;;;;;; DisplayV subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (no change needed)
;
; This subroutine is called with FSR0 containing the address of a variable
; display string.  It sends the bytes of the string to the LCD.  The first
; byte sets the cursor position.  The remaining bytes are displayed, beginning
; at that position.

DisplayV
		POINT  BYTE_1   
		rcall  DisplayC
        bcf  PORTE,0                   ;Drive RS pin low for cursor positioning code
        ;REPEAT_
L7
          bsf  PORTE,1                 ;Drive E pin high
          movff  INDF0,PORTD           ;Send upper nibble
          bcf  PORTE,1                 ;Drive E pin low so LCD will accept nibble
          bsf  PORTE,1                 ;Drive E pin high again
          swapf  INDF0,W               ;Swap nibbles
          movwf  PORTD                 ;Write lower nibble
          bcf  PORTE,1                 ;Drive E pin low so LCD will process byte
          rcall  T40                   ;Wait 40 usec
          bsf  PORTE,0                 ;Drive RS pin high for displayable characters
          movf  PREINC0,W              ;Increment pointer, then get next byte
        ;UNTIL_  .Z.                   ;Is it zero?
        bnz	L7
RL7
        return

;;;;;;; LoopTime subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This subroutine waits for Timer0 to complete its ten millisecond count
; sequence. It does so by waiting for sixteen-bit Timer0 to roll over. To obtain
; a period of precisely 10000/0.4 = 25000 clock periods, it needs to remove
; 65536-25000 or 40536 counts from the sixteen-bit count sequence.  The
; algorithm below first copies Timer0 to RAM, adds "Bignum" to the copy ,and
; then writes the result back to Timer0. It actually needs to add somewhat more
; counts to Timer0 than 40536.  The extra number of 12+2 counts added into
; "Bignum" makes the precise correction.

Bignum  equ     65536-25000+12+2

LoopTime
        ;REPEAT_
L9
        ;UNTIL_  INTCON,TMR0IF == 1    ;Wait until ten milliseconds are up
        btfss INTCON,TMR0IF
        bra	L9
RL9
        movff  INTCON,INTCONCOPY       ;Disable all interrupts to CPU
        bcf  INTCON,GIEH
        movff  TMR0L,TMR0LCOPY         ;Read 16-bit counter at this moment
        movff  TMR0H,TMR0HCOPY
        movlw  low  Bignum
        addwf  TMR0LCOPY,F
        movlw  high  Bignum
        addwfc  TMR0HCOPY,F
        movff  TMR0HCOPY,TMR0H
        movff  TMR0LCOPY,TMR0L         ;Write 16-bit counter at this moment
        movf  INTCONCOPY,W             ;Restore GIEH interrupt enable bit
        andlw  B'10000000'
        iorwf  INTCON,F
        bcf  INTCON,TMR0IF             ;Clear Timer0 flag
        return



;;;;;;; ByteDisplay subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
;
; Display whatever is in BYTE as a binary number.

ByteDisplay
        POINT  BYTE_1                  ;Display "BYTE="
        rcall  DisplayC
        lfsr  0,BYTESTR+8
        ;REPEAT_
L10
          clrf  WREG
          rrcf  BYTE,F                 ;Move bit into carry
          rlcf  WREG,F                 ;and from there into WREG
          iorlw  0x30                  ;Convert to ASCII
          movwf  POSTDEC0              ; and move to string
          movf  FSR0L,W                ;Done?
          sublw  low BYTESTR
        ;UNTIL_  .Z.
        bnz	L10
RL10

        lfsr  0,BYTESTR                ;Set pointer to display string
        MOVLF  0xc0,BYTESTR            ;Add cursor-positioning code
        clrf  BYTESTR+9                ;and end-of-string terminator
        rcall  DisplayV
        return

;;;;;;; 2nd ByteDisplay subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (need modification)
;
; Display whatever is in BYTE as a binary number.

ByteDisplay2
        ;POINT  BYTE_1                  ;Display "BYTE="
        ;rcall  DisplayC
        lfsr  0,BYTESTR+8
        ;REPEAT_
L102
          clrf  WREG
          rrcf  BYTE,F                 ;Move bit into carry
          rlcf  WREG,F                 ;and from there into WREG
          iorlw  0x30                  ;Convert to ASCII
          movwf  POSTDEC0              ; and move to string
          movf  FSR0L,W                ;Done?
          sublw  low BYTESTR
        ;UNTIL_  .Z.
        bnz	L102
RL102

        lfsr  0,BYTESTR                ;Set pointer to display string
        MOVLF  0x80,BYTESTR            ;Add cursor-positioning code
        clrf  BYTESTR+9                ;and end-of-string terminator
        rcall  DisplayV
        return


;;;;;;; Constant strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
LCDstr  db  0x33,0x32,0x28,0x01,0x0c,0x06,0x00  ;Initialization string for LCD
BYTE_1  db  "\x80POWER=   \x00"         ;Write "BYTE=" to first line of LCD
BarChars                               ;Bargraph user-defined characters
        db  0x00,0x48                  ;CGRAM-positioning code
        db  0x90,0x90,0x90,0x90,0x90,0x90,0x90,0x90  ;Column 1
        db  0x98,0x98,0x98,0x98,0x98,0x98,0x98,0x98  ;Columns 1,2
        db  0x9c,0x9c,0x9c,0x9c,0x9c,0x9c,0x9c,0x9c  ;Columns 1,2,3
        db  0x9e,0x9e,0x9e,0x9e,0x9e,0x9e,0x9e,0x9e  ;Columns 1,2,3,4
        db  0x9f,0x9f,0x9f,0x9f,0x9f,0x9f,0x9f,0x9f  ;Column 1,2,3,4,5
        db  0x00                       ;End-of-string terminator
	
		#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\FXD0808U.inc>
		#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\FXM1616U.inc>
		#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\FXD3216U.inc>
		#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\FXD3216U.inc>
		#include <O:\EE425 1BC FALL2016\Group 4\Final_Project\FXD3224U.inc>
		


        end


