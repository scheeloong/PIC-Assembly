;--------------------------------------------------------------------
; Practice 18: DEBUGGING on PIC18F4620 TWO 
	; Keys work when you press 
; Keypad:
;(UPDATED)
; 1: Black up
; 2. Black down
; 3. WHITE up
; 4. white down
; 5. 32 steps
; 6. 200 steps
; A. press once to on motor, press again to let go.  
; B. Flipper 

; Pin Outs
; A7-A6 -> Oscillator
; C7-C5, A5 -> Solenoid
; A4-A1 -> Stepper
; B7-B4-> Keypad, B3 -> Keypad enable
; B2, B1 -> Microswitch Interrupt 
; C4-C3 -> RTC
; C2-C1 -> Bidirectional motor, flipper
; A0, C0 -> both motors, reservoirs
; D7-D4 -> LCD, D3, D2 -> E, RS
; D1, D0 -> Bidirectional motor, Closing wheel
; E0 -> Microswtich close box 


; Input/Output , TRIS
; 76543210
; A -> 0000000
; B -> 1111111x
; C -> 000IC000
; D -> 00000000
; E -> xx0 

; Written By : Soon Chee Loong
; Accomplish: ??
;  
; 
;--------------------------------------------------------------------
; Need to Work On: 
; 10. Flipper Motor (work out delay timing) 
; 1. Timer, Real Time, use rtc clock, basically get current time, and get current time after operation and deduct to find actual time and display number on LCD. 
; 2. Number of dowels
; 3. Dowels pattern to display 
; 9. Microswitch Interrupt Service Routine at Port B7-B4, Change Keypad to Pins E..., it gets activated when pin b changes state  
	; B0, B1 B2-> Microswitch input with interrupts
; 7. Counting number of dowels using interrupts.
; 12. Change code to suit new stepper motor and box arrangments 
		; INITIAL : 1. box (won't drop cause lid opens), 2 box, 3 empty, 4 box (dowels drop), 5 wire close, 6 shut, 1. drop 

;		i)  Dowel Dropping
; 		ii) Move twice -> 1st box: shuts, 2nd box: dowel drop 
;		ii) move once -> 3rd box: (don't account for 3rd box) 
; 		iii) 
; 		iv) 
; 		v) 
; 		vi) 
;-----------
; DONE 
; 4. Changing Ports to actual ports 
	; C2, C1 => Bidirectional motor  DONE  	 (BROWN) 
	; D7-D4 => LCD, D3,D2 => E, RS   DONE     (ORANGE) 
	; B7-B4 => Keypad                DONE     (GREEN) 
	; B3 => Keypad Enable Bit        DONE	   (GREEN) 
	; C7-A5 => Solenoids             DONE           (RED) 
		; C7 = 
		; C6 = 
		; C5 = 
		; C4 = 
	; A0, C0 => both motors 		DONE			(BLUE) 
	; A4-A1 => Stepper Motor		DONE			(WHITE) 
; 5. Including Stepper Motor
; 6. Changing Code Flow to actual Flow 
; 8. Move Solenoids, First check that 2 motors start spinning 
; 11; BUG: Never goes to Ejector cause something wrong with eeprom or something. it always has BWBWBW regardless of input
		; Did not save w to pattern before pattern encoding
		; Did not decrement number of patterns. 

	list p=18f4620				; list directive to define processor
	#include <p18f4620.inc>		; processor specific variable definitions
;--------------------------------------------------------------------
;Configuration Bits
;--------------------------------------------------------------------
		CONFIG OSC=HS, FCMEN=OFF, IESO=OFF
		CONFIG PWRT = OFF, BOREN = SBORDIS, BORV = 3
		CONFIG WDT = OFF, WDTPS = 32768 ; 
		CONFIG MCLRE = ON, LPT1OSC = OFF, PBADEN = OFF, CCP2MX = PORTC
		CONFIG STVREN = ON, LVP = OFF, XINST = OFF
		CONFIG DEBUG = OFF
		CONFIG CP0 = OFF, CP1 = OFF, CP2 = OFF, CP3 = OFF
		CONFIG CPB = OFF, CPD = OFF
		CONFIG WRT0 = OFF, WRT1 = OFF, WRT2 = OFF, WRT3 = OFF
		CONFIG WRTB = OFF, WRTC = OFF, WRTD = OFF
		CONFIG EBTR0 = OFF, EBTR1 = OFF, EBTR2 = OFF, EBTR3 = OFF
		CONFIG EBTRB = OFF	 
;--------------------------------------------------------------------
;Declare unbanked variables (at 0x70 and on)
	cblock	0x70
		COUNTH			;used in delays
		COUNTM			;used in delays
		COUNTL			;used in delays
		Table_Counter
		lcd_tmp
		lcd_d1
		lcd_d2
		com
		dat
		num_patterns
		pattern
		num_dowels
		div_tmp		;used in dividing
		div_ans		;answer of the divide function
		num_white_dowels ; Keeps track of number of white dowels in reservoir
		num_black_dowels ; Keeps track of number of black dowels in reservoir 
		delay1s_1
		delay1s_2
		delay1s_3
		tempwsaveregister; DEBUG: save w for debugging
		; Interrupt variables
		W_TEMP
		STATUS_TEMP
		BSR_TEMP
		debugpattern
	endc	

;--------------------------------------------------------------------
;Declare constants for pin assignments (LCD on PORTD)
		#define	RS 	PORTD,2
		#define	E 	PORTD,3
	
         ORG       0x0000     ;RESET vector must always be at 0x00, when pic restarts, it gets here. 
        
		 goto      init       ;Just jump to the main code section.

		 org 	    0x08 ; high priority interrupt handler		
		 goto HighPriorityInterruptHandler
		 
			;		 org 0x18  ; will only be executed if NOT in compatibility mode
			;		 goto LowPriorityInterruptHandler 
;--------------------------------------------------------------------
; Delay: ~160us macro					
;--------------------------------------------------------------------

; This macro delays the LCD by 255 instructions on 20 MHZ clock. 

LCD_DELAY macro ; copy this whole macro till endm by just writing LCD_DELAY at bottom,
				; macro means inline code, it will just copy paste into whatever section. 
	movlw   0xFF	; 11111111
	movwf   lcd_d1   ; lcd_d1 has the value 
	decfsz  lcd_d1,f ; decrement lcd_d1 by 1, skip next line if 0. 
	goto    $-2
	endm
	
; To display strings in PIC18F4620
;--------------------------------------------------------------------
; Display macro							
;--------------------------------------------------------------------
Display macro	Message  ; call by typing Display "Command Line Argument", see example below, it uses look up tables
		; Declare local subfunctions
		local	loop_    ; local labels are part of the macro, cause word local, it is inline, so can't be non-local,
					     ; if it is non-local, the same loop_ will get copied many times resulting in 
					     ; runtime error.  
						 ; when have label/subfunction in macro, need local. 
		local 	end_
		movlw	LOW(Message) ; Message is command line argument, 
		movwf	TBLPTRL      ; moving value of low to a special register called TBLPTRL, Table Pointer Low
		movlw	HIGH(Message)
		movwf	TBLPTRH		; Table Pointer High
		movlw	UPPER(Message)
		movwf	TBLPTRU ; Table Pointer Upper
loop_
		tblrd	*+   ; read into TABLAT and increment, command to look at address, put into SPR TABLAT, increment and read it. 
		movf	TABLAT, W ; get data into working register 
		xorlw	B'00000000' ;check WORK reg to see if 0 is returned, 0 => end of string, xor : 00 && 11 => 0, 10 && 01 => 1; only returns 0 if all 0. 
		btfsc	STATUS,Z ; ; check last instruction (xor) to see if 0 was return, if it is 0, execute next line, if not, skip. 
		goto	end_
		call	WR_DATA ; write data into LCD 
		goto	loop_
end_
		endm

;--------------------------------------------------------------------
; Initialize PIC
;--------------------------------------------------------------------
init			

	movlw b'10000000'
	movwf INTCON ; enable unmasked interrupt ;
	movlw b'11110000' ;;
	movwf INTCON2 ; allow rising edge ;
	movlw b'11011000'; 
	movwf INTCON3 ; enable external interrupt and high priority them for int1 and int2 

			; TODO: ENABLE GLOBAL INTERRUPT 
			; TODO: ENABLE  "ENABLE FLAG" FOR B0 and B1, if flag bit is 1, then there is an interrupt. I will only jump to interrupt vector if interrupt is enabled
			; TODO: Enable interrupt on positive edge for both interrupts. 
			; TODO: Clear interrupt flag bits before operation begins. 
			; INT1P -> Interupt priority, 
			; INT1F -> Flag becomes 1 when hardware interrupt occurs
			; INT1D -> enables this interrupt to occur

; CHANGE THIS INTCON LINE AS YOU"RE USING INTERRUPTS!!!!!, build interrupt debouncing circuit, buy small resistors!!!!!
					;		 clrf	INTCON		; Disable all interrupts!! Pg95/390 of datasheet
         bsf	STATUS,0	; select bank 1, NOT SURE IF STILL APPLICABLE , a carry out occur if set C on STATUS to 1. 
		; Allow port E to be used for digital input 
		 movlw	0x0F ; 
		 movwf	ADCON1		; set portA to digital
		 movlw	0x0F		; configure comparators
		 movwf	CMCON		; for digital input

; Set Inputs/Outputs (TRISX) 
; A -> 0000000
; B -> 1111111x
; C -> 000IC000
; D -> 00000000
; E -> xx0 
         clrf	TRISA	; 00000000 -> A4-A1 => Stepper Motor 
         movlw	b'11111110'	; Set required keypad inputs
						;---------------------------
						;DEBUG
						;				clrf TRISB 
         movwf	TRISB ; 11111110 -> B7-B4 Keypad Inputs, B3 => Keypad Enable , B2, B1 -> microswtich interrupts 
	; ENABLE INTERRUPTS AT B1 and B2!!!
		 clrf	TRISC       ; 00000000 -> C7-C5,A5 Solenoids, C4,C3-> RTC,  A0,C0 => Both Motor, C2,C1 => Bidirectional motor 
		 clrf 	TRISD		; 00000000 -> D7-D4 LCD Data, D3,D2 E & RS for LCD , D1, D0 -> Bidirectional motor
		 movlw	b'111'		; Set required microswitch inputs, E0 -> Flipper microswtich
		 movwf	TRISE 		; microswtich input 
		; EEPROM
		 clrf	EEADRH		; point at address 00h for EEPROM (ranges from 00h to 33Fh) 
		 clrf	EEADR 		; EEADRH and EEADR holds the address of EEPROM being accessed

         bcf	STATUS,0   ; select bank 0, NOT SURE IF STILL NEEDED 
; Set Initial Output Voltage 
         clrf	PORTA ; Don't drive motor 
         clrf	PORTB ; Don't accept any initial input from KEYPAD
         clrf	PORTC ; Don't contract any solenoids, Don't move any motors 
         clrf	PORTD ; Don't write anything on LCD 
		 clrf   PORTE ; Don't accept any microswitch input 

		 call	InitLCD    ;Initialize the LCD, subroutine at bottom. 
		 goto	Standby    ; goto Standby 

;--------------------------------------------------------------------
; Look up table
;--------------------------------------------------------------------
Real_Time:
		dw		"HH:MM", 0			; temporary 
Welcome_Msg:
		dw		"Press any key", 0
Pattern_Select1:
		dw		"Press 1-6 to",0
Pattern_Select2:
		dw		"Select a pattern", 0	
Start_Operation:
		dw		"A) Start", 0
New_Pattern:
		dw		"B) Add pattern", 0
Completion_Msg1:
		dw		"Operation complete. A) Operation Time.", 0
Completion_Msg2:
		dw		"B) Pattern Info C) Dowel Count, D) Exit.", 0
Key_Continue:
		dw		"Press any key.",0
Light_Count:
		dw		"White Dowels: ",0
Dark_Count:
		dw		"Brown Dowels: ",0
Prev_Patterns:
		dw		"Prev Patterns:",0

; Start & Stop Message
Start_Reservoirs:
		dw 		"Res Activated", 0
Start_Stepper_Motor:
		dw 		"Step Motor Activated", 0
Start_Solenoids:
		dw 		"Sol Activated", 0
Start_Flipper_Forward:
		dw 		"Flip Forward Activated", 0

Start_Flipper_Backward:
		dw 		"Flip Backward Activated", 0

Stop_Reservoirs:
		dw 		"Res Terminated", 0
Stop_Flipper_Forward:
		dw 		"Flip Forward Terminated", 0
Stop_Flipper_Backward:
		dw 		"Flip Backward Terminated", 0

Close_Last:
		dw 		"Close Last Box", 0

Drop_Last:
		dw 		"Drop Last Box", 0

; Debug Message

Solenoid_White_Debug1:
		dw 		"Solenoid W T C", 0 ; Solenoid White Top Contract
Solenoid_White_Debug2:
		dw		"Solenoid W T E", 0 ; Top Extend
Solenoid_White_Debug3:
		dw 		"Solenoid W B C", 0 ; Bottom Contract
Solenoid_White_Debug4:
		dw		"Solenoid W B E", 0 ; Bottom Extend
Solenoid_Black_Debug1:
		dw 		"Solenoid B T C", 0 ; Solenoid Black Top Contract
Solenoid_Black_Debug2:
		dw		"Solenoid B T E", 0 
Solenoid_Black_Debug3:
		dw 		"Solenoid B B C", 0
Solenoid_Black_Debug4:
		dw		"Solenoid B B E", 0 

Stepper_Motor_Debug1:
		dw 		"Before Stepper Motor", 0
Stepper_Motor_Debug2:
		dw 		"After Stepper Motor", 0

Stepper_Motor_Debug3:
		dw 		"Stepper Motor 32", 0
Stepper_Motor_Debug4:
		dw 		"Stepper Motor 200", 0
Stepper_Motor_Debug11:
		dw 		"Stepper Motor 1", 0
Stepper_Motor_Debug12:
		dw 		"Stepper Motor 2", 0
Stepper_Motor_Debug13:
		dw 		"Stepper Motor 3", 0
Stepper_Motor_Debug14:
		dw 		"Stepper Motor 4", 0
Stepper_Motor_Debug15:
		dw 		"Stepper Motor 5", 0
Stepper_Motor_Debug16:
		dw 		"Stepper Motor 6", 0

LALA6:
		dw		"6", 0			; temporary 
LALA5:
		dw		"5", 0			; temporary 
LALA4:
		dw		"4", 0			; temporary 
LALA3:
		dw		"3", 0			; temporary 
LALA2:
		dw		"2", 0			; temporary 
LALA1:
		dw		"1", 0			; temporary 
LALAB:
		dw		"B", 0			; temporary 
LALAA:
		dw		"A", 0			; temporary 

BABA1:
		dw		"Interrupt 1 active", 0			; temporary 


BABA2:
		dw		"Interrupt 2 active", 0			; temporary 

BABA3:
		dw		"End Interrupt", 0			; temporary 

;--------------------------------------------------------------------
; Main 
;--------------------------------------------------------------------

; Main occurs in Operation

;--------------------------------------------------------------------
;Standby Mode
;--------------------------------------------------------------------
Standby:
		call		Clear_Display
		Display		Real_Time ; NOT DONE YET
		call		Switch_Lines
		Display		Welcome_Msg
		; Wait for Keypad to be pressed (Data Available -> B3)  
			;-----------------------------
			; DEBUG: 
;			movwf 		tempwsaveregister
;			movlw		B'00001000'
;			movwf		PORTB
;			movf		tempwsaveregister, W
		btfss		PORTB,3		;Wait until keypad button is pressed, btfss skips next line if 1(set) 
		bra			$-1
			;-----------------------------
			; DEBUG: 
;			movwf 		tempwsaveregister
;			movlw		B'00000000'
;			movwf		PORTB
;			movf		tempwsaveregister, W
		btfsc		PORTB,3		;Wait until keypad button is depressed, btfsc skips next line if 0 (clear)
		bra			$-1
		
;--------------------------------------------------------------------
; User Interface
;--------------------------------------------------------------------
		; Clear Number Patterns to 0 patterns initially  
		clrf		num_patterns ; stores number of patterns given. At least 1, max. infinity. 

; This subroutine gets called in beginning for pattern 1 and might get called again later
; for pattern2. It will store both patterns in EEPROM at address 01H and 02H in the end. 
; It may be repeated for a 3rd pattern too! num_patterns stores number of patterns given
Pattern_Menu:
		call		Clear_Display
		Display		Pattern_Select1
		call		Switch_Lines
		Display		Pattern_Select2
		; Wait till the keypad is pressed before reading
			;-----------------------------
			; DEBUG: 
;			movwf 		tempwsaveregister
;			movlw		B'00001000'
;			movwf		PORTB
;			movf		tempwsaveregister, W
		btfss		PORTB,3		;Wait until keypad button is pressed
		bra			$-1
			;-----------------------------
			; DEBUG: 
;			movwf 		tempwsaveregister
;			movlw		B'00000000'
;			movwf		PORTB
;			movf		tempwsaveregister, W
		btfsc		PORTB,3		;Wait until keypad button is released
		bra			$-1

		;make sure that the data entered is between 1 and 6 inclusive
		; Note: The keypad is released already, but the keypad holds the last key pressed, 
												; not sure if due to keypad or MM74C922N chip. 
		; skips if B7 == 1 or if B5 and B4 are both 1 => Keypad Input given was not 1-6 
		btfsc		PORTB,7
		goto		Pattern_Menu ; keep looping if B7 is not 0
		btfss		PORTB,4 ; B7 is 0, skip if B4 is 1
		goto		Store_Pattern
		btfss		PORTB,5 ; B7 is 0, skip if B5 is 1
		goto		Store_Pattern
		goto		Pattern_Menu
 
; Store the keypad pattern pressed onto pattern then to EEPROM 
Store_Pattern:
		;store data from keypad into memory for use during operation
		swapf		PORTB,w ; let w hold the last keypad pressed in xxxxbbbb (where bbbb is the code for last keypad pressed)
				; swapf swaps B7-B4 with B3-B0 and store result in w or f if f is stated. In this case, it stores in w. 
		andlw		0x0F 	; and w with 00001111 to not let first 4 bits which does not matter interfere, w = 0000bbbb  
		movwf		pattern ; let pattern register hold the current pattern 
		movlw		0x02	;takes keypad entry and coverts it into the pattern number
							; let w = 00000010  
		; Change coded pattern for 1-3 from keypad input to their actual binary values 
		cpfsgt		pattern ; compare f with W and skip if f > W , if f > W => f is more than 3 => skip
		incf		pattern ;f < 4 since did not skip, increment f by 1 => f += 1 (00000010+00000001 = 00000011) and store result in W
					; increment if input was 1-3 and not 4-6 to change the keypad encoder binary codes
					;  to actual numerical value and not keypad value 
		;write pattern to EEPROM
		incf		EEADR	;increment data memory address by one (initially at 0x00 at beginning of program) 
		movff		pattern, EEDATA ; movff => move from file to file, in this case, move from pattern to EEDATA
		call		Write_EEPROM ; write the first pattern at EEPROM
		
		;next two lines are just to check the encoder functionality
		call		Pattern_Encoder ; when returned, 'pattern' variable holds the pattern of the dowels based on input given 
									; 000111 => WWWBBB (W = white, B = Black) 
		; pattern holds actual pattern to dispense dowels 
;movff		pattern,PORTD	; move pattern to PORTC to output the patterns on the LEDs to see if right
							; Debugging just to see if correct pattern was given , have not checked!!!!! 
		incf		num_patterns		;increase the pattern count as first pattern was given. 

; Continue to get second pattern and maybe more if needed. 
Pattern_Menu2
		call		Clear_Display
		Display		Start_Operation ; Press A to start
		call		Switch_Lines
		Display		New_Pattern	    ; Press B to add a new pattern 
		
			;-----------------------------
			; DEBUG: 
;			movwf 		tempwsaveregister
;			movlw		B'00111000' ; A
;			movwf		PORTB
;			movf		tempwsaveregister, W


; New Debug Mode 
		btfss		PORTB,3		;Wait until data is available from the keypad
		bra			$-1
		
		swapf		PORTB,W		;Read keypad from portB <7:4> into W<3:0>
		andlw		0x0F ; and W with 00001111 

		movwf 		debugpattern ; debug pattern hows the pattern given. 

; NOw you have to determine which key it is 

		movf 	debugpattern, W 
		sublw 	0x07 
		bz		DebugKeyB
		goto AA

DebugKeyB:
		call		Clear_Display
		Display		LALAB ; NOT DONE YET
		bsf LATC, 2; move motor direction upwards
		call DelayFlipper
		bcf LATC, 2; stop it
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Stop_Flipper_Forward
		call 		DelayMessage
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Start_Flipper_Backward
		call 		DelayMessage
		bsf LATC, 1; move motor direction downwards
;		call DelayFlipper
		btfss		PORTE, 0		;Wait until microswitch is pressed 
		bra			$-1
		bcf LATC, 1; stop it
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Stop_Flipper_Backward
		call 		DelayMessage
		goto 		Pattern_Menu2	

AA: 
		movf 	debugpattern, W 
		sublw 	0x06 
		bz		DebugKey6
		goto BB

DebugKey6:
		call		Clear_Display
		Display		LALA6 ; NOT DONE YET
		call		Clear_Display
		Display		Stepper_Motor_Debug4
		call Stepper_Motor_200
		goto 		Pattern_Menu2

BB: 
		movf 	debugpattern, W 
		sublw 	0x05 
		bz		DebugKey5
		goto	 CC

DebugKey5
		call		Clear_Display
		Display		LALA5 ; NOT DONE YET
	
		call		Clear_Display
		Display		Stepper_Motor_Debug3
		call Stepper_Motor_32
		goto 		Pattern_Menu2

CC:
		movf 	debugpattern, W 
		sublw 	0x04 
		bz		DebugKey4
		goto 	DD

DebugKey4
		call		Clear_Display
		Display		LALA4 ; NOT DONE YET
		call		Clear_Display
		Display		Solenoid_White_Debug3	
		bsf     	LATC, 6
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_White_Debug4
		bcf   	    LATC, 6
		call 		DelaySolenoid
		goto 		Pattern_Menu2	

DD: 
		movf 	debugpattern, W 
		sublw 	0x03 
		bz		DebugKeyA
		goto EE


DebugKeyA:
		call		Clear_Display
		Display		LALAA ; NOT DONE YET
		bsf LATC, 0
		bsf LATC, 3
		btfsc		PORTB,3		;Wait until data is not available from the keypad
		bra			$-1
		btfss		PORTB,3		;Wait until data is available from the keypad
		bra			$-1
		bcf LATC, 0
		bcf LATC, 3
		btfsc		PORTB,3		;Wait until data is not available from the keypad
		bra			$-1
		goto 		Pattern_Menu2

EE: 

		movf 	debugpattern, W 
		sublw 	0x02 
		bz		DebugKey3
		goto FF


DebugKey3:
		call		Clear_Display
		Display		LALA3 ; NOT DONE YET 
		bsf    		LATC, 7
		call		Clear_Display
		Display		Solenoid_White_Debug1
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_White_Debug2
		bcf    		LATC, 7
		call 		DelaySolenoid
		goto 		Pattern_Menu2	


FF:
		movf 	debugpattern, W 
		sublw 	0x01
		bz		DebugKey2
		goto GG

DebugKey2:
		call		Clear_Display
		Display		LALA2 
		call		Clear_Display
		Display		Solenoid_Black_Debug3	
		bsf     	LATC, 4
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_Black_Debug4
		bcf     	LATC, 4
		call 		DelaySolenoid
		goto 		Pattern_Menu2

GG: 
		movf 	debugpattern, W 
		sublw 	0x00
		bz		DebugKey1
		goto 	Pattern_Menu2

DebugKey1:
		call		Clear_Display
		Display		LALA1 ; NOT DONE YET

		bsf     	LATC, 5
		call		Clear_Display
		Display		Solenoid_Black_Debug1
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_Black_Debug2
		bcf     	LATC, 5
		call 		DelaySolenoid
		goto 		Pattern_Menu2		

	


	
	


		;if not given B input (0111), go back to pattern menu
;		sublw		0x07  ; subtract 000111 with w and place result in w, so basically result = 7 - w; w = result; 
;		bz			Pattern_Menu ; bz = branch if zero (ZERO bit is 1 => last instruction resulted in 0), will repeat Pattern Menu
		;if A), start operation
;		sublw		0x04 ; The math used is 7- 3-  -4 = 0. ( if A it will minus 3 ) so when you minus 4, it will be A! 
;		bnz			Pattern_Menu2 ; loop again if button A was not pressed (means neither A nor B was pressed)

;--------------------------------------------------------------------
; Operation								
;--------------------------------------------------------------------

; Begin operation once user press A or when 2nd pattern is given. 
Operation:
; TODO:START TIMER
 
	; Begin spinning reservoirs AND LET IT SPIN TILL whenever

;----------------------


		; DEBUG
		;	call Stepper_Motor_Once_Debug

		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Start_Reservoirs
		call 		DelayMessage
;Spin_Reservoirs: 
		bsf LATC, 0
		bsf LATC, 3
;----------------------
; TODO: Delay while Counting Dowels using interrupts.
; 															goto InfiniteLoop; TEST: BOTH MOTORS SPIN FOREVER , it is definitely not watchdog, must be power supply. 
; Store number of patterns into EEDATA  
		movff		num_patterns,EEDATA
		incf		EEADR ; change EEPROM address
		call		Write_EEPROM ; write the number of patterns on EEPROM
		movf		num_patterns,w ; w holds number of patterns
		subwf		EEADR ; ; go to address of the last given pattern. 

; Dispense box into loading area

DisperserFirstBox:
		goto BoxPositioner ; dont move stepper motor for first box. 

Dispenser: ; Will come here 1st time right after above code, and right after 6 dowels are loaded onto box (Flipper) 
			; where it moves stepper motor once for next operation 
		movlw	0x00			; w is 00000000
		cpfsgt	num_patterns    ; compare num_patterns with W, if f>w, skip next line
		goto	Ejector		; will only be here with num_patterns == 0

								;		call		Clear_Display
								;		Display		Stepper_Motor_Debug1
		call Stepper_Motor_Once ; to move next box into place, at the same time closing the current box. 
								;	call		Clear_Display
								;	Display		Stepper_Motor_Debug2
		call Stepper_Motor_Once ; to move the 2nd box into place, need to move stepper motor twice. 		
;--------------------------------------------------------------------
; Box Positioner						
;--------------------------------------------------------------------
BoxPositioner:
; Read next pattern 
		call	Read_EEPROM	;read what the next pattern should be into w. 
		incf	EEADR		;increment address in the EEPROM
		movwf	pattern     ; need move from w to pattern before encoding final pattern 
		call	Pattern_Encoder	;convert pattern number to actual pattern sequence
		movlw	0x06		;set number of dowels to 6
		movwf	num_dowels ; num_dowels stores number of dowels 
		decf	num_patterns; decrement number of patterns left
Check_Box:
		movlw	0x00
		cpfsgt	num_dowels ; as long num_dowels is not 0, skip Ejector and jump to move box
		goto	Flipper ; Will only go to Dispenser once all 6 dowels are completed 
; Will be here if num_dowels not 0. 
;--------------------------------------------------------------------
; Dowel Sorter							
;--------------------------------------------------------------------
Dowel_Sorter:
		;check next dowel
		btfsc	pattern,0 ; will skip next line if the bit 0th bit of pattern is 0 => white
		bra		Dark_Dowel
; Move solenoids for White Dowels
Light_Dowel: 
		bsf    		LATC, 7
		call		Clear_Display
		Display		Solenoid_White_Debug1
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_White_Debug2
		bcf    		LATC, 7
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_White_Debug3	
		bsf     	LATC, 6
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_White_Debug4
		bcf   	    LATC, 6
		call 		DelaySolenoid
;										goto Light_Dowel ; TEST: Test that Solenoids are opening and closing within given time frame 
		
;ADDON: CHECK IF DOWEL IS IN BOX, REPEAT IF NOT	    ;btfss	PORTE,0		;check if dowel is in the box
													;bra		Light_Dowel	;repeat rod motion if dowel not in box
		decf	num_dowels	;decrease the dowel count
		rrncf	pattern		;shift the pattern over so the next dowel is in bit 0
		goto	Check_Box
; Move Soleniods for Dark Dowels
Dark_Dowel:
		bsf     	LATC, 5
		call		Clear_Display
		Display		Solenoid_Black_Debug1
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_Black_Debug2
		bcf     	LATC, 5
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_Black_Debug3	
		bsf     	LATC, 4
		call 		DelaySolenoid
		call		Clear_Display
		Display		Solenoid_Black_Debug4
		bcf     	LATC, 4
		call 		DelaySolenoid
;										goto Dark_Dowel ; ; TEST: Test that Solenoids are opening and closing within given time frame , need change to dark

;ADDON: CHECK IF DOWEL IS IN BOX, REPEAT IF NOT	    ;btfss	PORTE,0		;check if dowel is in the box
													;bra		Light_Dowel	;repeat rod motion if dowel not in box
		decf	num_dowels	;decrease the dowel count
		rrncf	pattern, 1		;shift the pattern over so the next dowel is in bit 0 and place result back into pattern 
		goto	Check_Box

;--------------------------------------------------------------------
; Flipper							
;--------------------------------------------------------------------
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Start_Flipper_Forward
		call 		DelayMessage
Flipper: 
	bsf LATC, 2; move motor direction upwards
	call DelayFlipper
	bcf LATC, 2; stop it
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Stop_Flipper_Forward
		call 		DelayMessage
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Start_Flipper_Backward
		call 		DelayMessage
	bsf LATC, 1; move motor direction downwards
	call DelayFlipper
	bcf LATC, 1; stop it
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Stop_Flipper_Backward
		call 		DelayMessage
	goto Dispenser

;--------------------------------------------------------------------
; Box Ejector							
;--------------------------------------------------------------------
; Will only come here once no more boxes needed to be loaded
; Currently, there is one box still at the dowel dispenser location. have to move it to drop it out. 
Ejector:
; Stop Reservoir
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Stop_Reservoirs
		call 		DelayMessage
bcf LATC, 3
bcf LATC, 0

		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Close_Last
		call 		DelayMessage
call Stepper_Motor_Once ; moves it to close box
		;------------------------------
		; TEMP: Message for debugging
		call		Clear_Display
		Display		Drop_Last
		call 		DelayMessage
call Stepper_Motor_Once ; moves it to shut it
call Stepper_Motor_Once ; move it to drop it  

;--------------------------------------------------------------------
; Completion Message with MENU				
;--------------------------------------------------------------------
Completion:
; TODO : Stop timer
		call		Clear_Display
		Display		Completion_Msg1
		call		Switch_Lines
		Display		Completion_Msg2
		call		Scroll_Menu
		swapf		PORTB,W		;Read keypad  from portB <7:4> into W<3:0>
		andlw		0x0F

; MENU: 
; D: End Operation
; A: Operation Time
; C: Dowel Count
; B: Pattern Info
		;go to correct menu option
		sublw		0x0F		;check if D is pressed
		bz			End_Operation
		sublw		0x0C		;check if A is pressed
		bz			Op_Time
		sublw		0x08		;check if C is pressed
		bz			Dowel_Count
		sublw		0x04		;check if B is pressed
		bz			Pattern_Info
		bra			Completion	;returns to completion menu if choice is not valid
		
;D
End_Operation:
		goto		Standby
;A
Op_Time:
		call		Clear_Display
		Display		Real_Time ; NOT DONE YET 
		call		Switch_Lines
		Display		Key_Continue
		btfss		PORTB,1			;Wait until keypad is pressed
		bra			$-1
		bra			End_Operation ; NEED CHANGE TO BRA COMPLETION
;-----------------
;B 
Pattern_Info:
		call		Clear_Display
		Display		Prev_Patterns ; NOT DONE YET!!!!! 
		call		Switch_Lines
		;read the pattern choices from EEPROM
		call		Read_EEPROM ; will read into W
		movwf		num_patterns ; will know number of patterns
		subwf		EEADR ;
		; Have not implemented Pattern Info  
		movlw		0x00
Disp_Pattern:
		call		Read_EEPROM ; read first pattern 
		addlw		0x30 ; W = 00110000, A PATTERN TO DISPLAY ON LCD
		movwf		PORTC	; TO TEST TO  DISPLAY ON LCD, NEED TO CHANGE
		call		WR_DATA
		incf		EEADR
		dcfsnz		num_patterns ; decrement and check if 0 , if it is, next line is not skipped 
		bra			End_Disp_Pattern ; skip as long still got number of patterns to display
		movlw		","
		call		WR_DATA ; TO TEST DISPLAY ON LCD, NEED TO CHANGE
		bra			Disp_Pattern

End_Disp_Pattern:
		btfss		PORTB,1			;Wait until keypad is pressed ; NOTE: IT WAITS TILL KEYPAD IS PRESSED. 
		bra			$-1
		bra			End_Operation
;-----------------
;C 
;TODO Dowel Count
Dowel_Count:
		; NOT DONE YET!!!!! 
		;read from the weight sensors the # of remaining dowels
		call		Clear_Display
		Display		Light_Count
		call		Switch_Lines
		Display		Dark_Count
		btfss		PORTB,1			;Wait until keypad is pressed
		bra			$-1
		bra			End_Operation

;--------------------------------------------------------------------
; Encoders
;--------------------------------------------------------------------
; This subroutine encodes which pattern was given, it branches to proper pattern when given. 
; To use it, pattern with incremented number 01 => 1 not 2 has to be used.  
Pattern_Encoder: ; PATTERN NEEDS TO HOLD PATTERN NUMBER, EXAMPLE, PATTERN 1 IS 000001 NOT 000000
		dcfsnz	pattern   ; decrement pattern and store result in W, skip if result is not 0 
		bra		Pattern_1
		dcfsnz	pattern
		bra		Pattern_2
		dcfsnz	pattern
		bra		Pattern_3
		dcfsnz	pattern
		bra		Pattern_4
		dcfsnz	pattern
		bra		Pattern_5

; These subroutines gets branched into by Pattern Encoder, when branched, the working register stores the proper pattern 
; 0 => white dowels 
; 1 => black dowels 
Pattern_6:
		movlw	b'010101' ; => dowels will come out as black, white, black, white, black, white. 
		bra		Exit_Encoder
Pattern_1:
		movlw	b'000000'
		bra		Exit_Encoder
Pattern_2:
		movlw	b'111111'
		bra		Exit_Encoder
Pattern_3:
		movlw	b'000111' ; => dowels come out from solenoid from right => black black, black, white, white, white -> on box will be white white white-> black black, black. 
		bra		Exit_Encoder
Pattern_4:
		movlw	b'001100'
		bra		Exit_Encoder
Pattern_5:
		movlw	b'110011'
		
; now, the pattern holds the actual pattern of the dowels that needs to be kept. 
Exit_Encoder:
		movwf	pattern
		return
;--------------------------------------------------------------------
; Accessing EEPROM
;--------------------------------------------------------------------
Write_EEPROM:
		;writes EEDATA to current address in EEPROM
		bcf			EECON1,EEPGD	;point to DATA memory
		bcf			EECON1,CFGS		;access EEPROM
		bsf			EECON1,WREN		;enable writes
		movlw		0x55			;writing sequence
		movwf		EECON2
		movlw		0xAA
		movwf		EECON2
		bsf			EECON1,WR		;set WR bit to begin write
		btfsc		EECON1,WR		;wait for write to finish
		bra			$-1
		bcf			EECON1,WREN		;disables writes on write complete set
		return

; Reads into W 
Read_EEPROM:
		;reads from currect address in EEPROM to working register
		bcf			EECON1,EEPGD ; clear EEPGD to go to EEPROM address, set to go to normal program memory. in this case, go to eeprom address
		bcf			EECON1,CFGS ; CFGS: set => configuration , clear => memory data
		bsf			EECON1,RD	; initialize EEPROM read 
		btfsc		EECON1,RD		;wait for read to finish, if bit RD of EECON1 is 0, skip next instruction
		bra			$-1
		movf		EEDATA,W ; move data read to W 
		return

;--------------------------------------------------------------------
; LCD control
;--------------------------------------------------------------------

; This function switches LCD to the next line
Switch_Lines:
		movlw	B'11000000'
		call	WR_INS
		return

; This function clears the Display
Clear_Display:
		movlw	B'00000001'
		call	WR_INS
		return

Scroll_Menu:
		movlw		b'00011000'		;Move to the left
		call		WR_INS
		call		Delay
		btfss		PORTB,1     ;Wait until data is available from the keypad
		bra			Scroll_Menu
		btfsc		PORTB,1
		bra			$-1
		return
;--------------------------------------------------------------------
; Stepper Motor Controls
;--------------------------------------------------------------------

; Model #1200 Pololu Stepper Motor: 1.8deg/step=> 200 steps per revolution => 200/6 => 33  or 34 steps. or 32 to simplify code. 
			; For counter clockwise: Blue -> Green -> Red -> Black 
			; Yellow and White connects to VSS 

; Model #57BYGH420 Wantai Stepper Motor: 1.8deg/step => 200 steps per revolution => 200/6 => 33 or 34 steps or 32 to simplify code. 

; Blue -> Green-> Red -> Black -> Clockwise
; Black -> Red -> Green -> Blue -> Counter clockwise

Stepper_Motor_4 ; Counter-clockwise
		bsf     LATA, 1
		call DelayStepperMotor ; Need delay to move motor
		bcf     LATA, 1
		bsf     LATA, 2
		call DelayStepperMotor
		bcf     LATA, 2
		bsf     LATA, 3
		call DelayStepperMotor
		bcf     LATA, 3
		bsf     LATA, 4
		call DelayStepperMotor
		bcf     LATA, 4
		return 

Stepper_Motor_4_Clockwise
		bsf     LATA, 4
		call DelayStepperMotor ; Need delay to move motor
		bcf     LATA, 4
		bsf     LATA, 3
		call DelayStepperMotor
		bcf     LATA, 3
		bsf     LATA, 2
		call DelayStepperMotor
		bcf     LATA, 2
		bsf     LATA, 1
		call DelayStepperMotor
		bcf     LATA, 1
		return 

Stepper_Motor_20: 
	call Stepper_Motor_4
	call Stepper_Motor_4
	call Stepper_Motor_4
	call Stepper_Motor_4
	call Stepper_Motor_4
	return 

Stepper_Motor_32: 
	call Stepper_Motor_20
	call Stepper_Motor_4
	call Stepper_Motor_4
	call Stepper_Motor_4
	return 

Stepper_Motor_40
	call Stepper_Motor_20
	call Stepper_Motor_20
	return 

Stepper_Motor_200
	call Stepper_Motor_40
	call Stepper_Motor_40
	call Stepper_Motor_40
	call Stepper_Motor_40
	call Stepper_Motor_40
	return 


; This subroutine moves the stepper motor for 60degrees (one actual complete movement) 
Stepper_Motor_Once
	call		Clear_Display
	Display		Stepper_Motor_Debug3
	call Stepper_Motor_32
										;call		Clear_Display
										;Display		Stepper_Motor_Debug4
										;call Stepper_Motor_200

	return 

Stepper_Motor_Once_Debug
	call		Clear_Display
	Display		Stepper_Motor_Debug3
	call Stepper_Motor_32
	call		Clear_Display
	Display		Stepper_Motor_Debug4
	call Stepper_Motor_200
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug11
	call Stepper_Motor_32
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug12
	call Stepper_Motor_32
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug13
	call Stepper_Motor_32
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug14
	call Stepper_Motor_32
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug15
	call Stepper_Motor_32
call DelayFlipper
	call		Clear_Display
	Display		Stepper_Motor_Debug16
	call Stepper_Motor_32
call DelayFlipper
	return 


;--------------------------------------------------------------------
; Divide Function (for # of dowels)
;--------------------------------------------------------------------
div_10:
		;divides a number by 10
		;input: number to be divided in working register
		;output: answer in working register
		;will need to modify division factor based on weight sensor properties
		movwf	div_tmp		;store number in temp register
		movlw	D'10'		;number to divide by
		clrf	div_ans		;reset div_ans
div_loop
		cpfsgt	div_tmp
		return
		subwf	div_tmp
		incf	div_ans
		bra		div_loop

;--------------------------------------------------------------------
; Menu_Delay (Exit if keypad is pressed)
;--------------------------------------------------------------------

; Keypad Delay to wait for input 
Delay
	local	Delay_Loop
      movlw 0x88 ; 10001000, 136
      movwf COUNTH
      movlw 0xBD ; 10111101, 189 
      movwf COUNTM
      movlw 0x04 ; 00000100 ; 4
      movwf COUNTL

Delay_Loop  ; loops till time out or if data is available
	  btfsc	 PORTB,1		;Check if data is available from the keypad
	  		return
      decfsz	COUNTH, f
      bra		Delay_Loop
      decfsz	COUNTM, f
      bra		Delay_Loop
      decfsz	COUNTL, f
      bra		Delay_Loop
      nop
      nop
			return

;---------------------------------
;DELAY 1 Second with 20MHz clock
; Actual delay = 1 seconds = 5000000 cycles
; Error = 0 %

Delay1s
			;4999993 cycles
	movlw	0x2C
	movwf	delay1s_1
	movlw	0xE7
	movwf	delay1s_2
	movlw	0x0B
	movwf	delay1s_3
Delay1s_help
	decfsz	delay1s_1, f
	goto	$+2
	decfsz	delay1s_2, f
	goto	$+2
	decfsz	delay1s_3, f
	goto	Delay1s_help
			;3 cycles
	nop
	nop
			;4 cycles (including call)
	return

Delay1sori
			;4999993 cycles
	movlw	0x2C
	movwf	delay1s_1
	movlw	0xE7
	movwf	delay1s_2
	movlw	0x0B
	movwf	delay1s_3
Delay1sori_help
	decfsz	delay1s_1, f
	goto	$+2
	decfsz	delay1s_2, f
	goto	$+2
	decfsz	delay1s_3, f
	goto	Delay1s_help
			;3 cycles
	goto	$+1
	nop
			;4 cycles (including call)
	return

Delay5s
	call Delay1s
	call Delay1s
	call Delay1s
	call Delay1s
	call Delay1s
	return

Delay25s
	call Delay5s
	call Delay5s
	call Delay5s
	call Delay5s
	call Delay5s
	return

Delay1000s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	call Delay25s
	return

Delay10000s 
	call Delay1000s
	call Delay1000s
	call Delay1000s 
	call Delay1000s
	call Delay1000s
	call Delay1000s 
	call Delay1000s
	call Delay1000s
	call Delay1000s 
	call Delay1000s
	return 

Delay100ks ; 
	call Delay10000s; 
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	call Delay10000s;
	return 

; Actual Delays

DelaySolenoid ; Delay for solenoids  Backup: Just 1 Delay10000
	call Delay10000s
	call Delay10000s
	return 

DelayStepperMotorNew; Delay for Pololu Stepper motor  
	call Delay1000s
	call Delay1000s
	return 

DelayStepperMotor ; Delay for Hurst Stepper Motor (Needs to be longer) 
	call Delay1000s
	call Delay1000s
	call Delay1000s
	call Delay1000s
	call Delay1000s
	return 

DelayFlipper ; Delay for Flipper Motor
	call Delay100ks
	return 

DelayMessage ; Delay for any short messages, especially during debugging to know what's going on 
	call Delay10000s
	call Delay10000s 
	return 


;--------------------------------------------------------------------
; LCD-related subroutines
;--------------------------------------------------------------------
InitLCD
	bcf STATUS,0
	bsf E     ;E default high
	
	;Wait for LCD POR to finish (~15ms)
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
	movlw	b'00110011'
	call	WR_INS
	movlw	b'00110010'
	call	WR_INS

	; 4 bits, 2 lines, 5x7 dots
	movlw	b'00101000'
	call	WR_INS

	; display on/off
	movlw	b'00001100'
	call	WR_INS
	
	; Entry mode
	movlw	b'00000110'
	call	WR_INS

	; Clear ram
	movlw	b'00000001'
		call	WR_INS
	return

    ;ClrLCD: Clear the LCD display
ClrLCD
	movlw	B'00000001'
	call	WR_INS
    return

;--------------------------------------------------------------------
    ; Write command to LCD - Input : W , output : -
;--------------------------------------------------------------------
; Write LCD specific instructions to LCD
WR_INS
	bcf		RS				;clear RS
	movwf	com				;W --> com
	andlw	0xF0			;mask 4 bits MSB w = X0
	movwf	PORTD			;Send 4 bits MSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	call	lcdLongDelay
	return

;--------------------------------------------------------------------
    ; Write data to LCD - Input : W , output : -
;--------------------------------------------------------------------
; Write words that needs to be output to LCD
WR_DATA
	bsf		RS				
	movwf	dat
	movf	dat,w
	andlw	0xF0		
	addlw	4
	movwf	PORTD		
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0		
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	return

lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return
;--------------------------------------------------------------------
; Interrupt Handling
;--------------------------------------------------------------------

; Interrupts
; Interrupt Flag bit
; Interrupt Enable bit 
; Global Interrupt (disable/enable) 
; reset vector is 0, when reset pic, it goes to 0. 

; high priority can interrupt low priority interrupt but not vice versa.  

 HighPriorityInterruptHandler: 
	; need declare temporary variables
MOVWF W_TEMP ; W_TEMP is in virtual bank
movf STATUS, w
mofw 
MOVFF STATUS, STATUS_TEMP ; STATUS_TEMP located anywhere
MOVFF BSR, BSR_TEMP ; BSR_TMEP located anywhere
;-------- USER ISR CODE -----------

btfss INTCON3, 0 ; check if interrupt 1 is set, if it is, skip next line
goto  CHECKTWO
; Interrupt 1 code
bcf INTCON3, 0

		call		Clear_Display
		Display		BABA1
		call 		DelaySolenoid


goto ENDINTERRUPT


CHECKTWO: 
btfss INTCON3, 1 ; if interrupt 2 is set, skip next line 
goto  ENDINTERRUPT
; Interrupt 2 code
bcf INTCON3, 1


		call		Clear_Display
		Display		BABA2
		call 		DelaySolenoid


; Check which flag bit got set
; Increase the count for number of dowels
; DEBUG: Display haha for 2 seconds with delay

; clear flag bit 
ENDINTERRUPT: 
	MOVFF BSR_TEMP, BSR ; Restore BSR
	MOVF W_TEMP, W ; Restore WREG
	MOVFF STATUS_TEMP, STATUS ; Restore STATUS
	; save bsr register?? pg 105
	; identify which flag generated the interrupt.. 
	; restore w register
	; restore status register

		call		Clear_Display
		Display		BABA3
		call 		DelaySolenoid
	retfie ; return interrupt 


I2Cstart
	banksel SSPCON2
	bsf SSPCON2, SEN   ; start it 
	banksel PIR1
Swait ; start wait
	btfss PIR1, SSPIF  ; wait for it to set == 1
	goto Swait
	bcf PIR1, SSPIF	; clear it
	return


; sending by putting code/address/data to SSPBUF register
; Note, address must be in w before calling 
i2cSend
	banksel SSPBUF	
	movwf SSPBUF
	banksel PIR1
cwait ; clear wait 
	btfss PIR1, SSPIF
	goto cwait
	bcf PIR1, SSPIF 
	return


i2cStop
	banksel SSPCON2
	bsf SSPCON2, PEN 
	banksel PIR1
Pwait
	btfss PIR1, SSPIF 
	goto Pwait
	bcf PIR1, SSPIF 
	return



;--------------------------------------------------------------------
; End
;--------------------------------------------------------------------
; For Debugging 
InfiniteLoop:
	goto InfiniteLoop ; 

; it never reaches end, it always goes to standby, though END signifies to MPLAB that this is the end of the code
	END



; I2C write code 
;I2Cwrite.asm
;
;i2C Master Mode Program
;for use in 24LC16B Serial EEPROM access by 16F877
;in I2C Master Mode Operation of SSP Module of 16F877 (with Fosc = 20 MHz ) ;
;1-byte data writing to 24LC16B
;
;
  list      p=16f877
INTCON EQU 0x0B STATUS EQU 0x03 ZERO EQU 0x00 PIR2 EQU 0x0D PIR1 EQU 0x0C SSPIF EQU 0x03
PIE1 EQU 0x8C
PIE2 EQU 0x8D
;for STATUS
;I2C Modules
;SSP Interrupt Flag (must be cleared for next I2C)
SSPCON
SSPEN
SSPCON2
PEN
SEN
SSPSTAT
SMP
SSPADD
baud100
baud400
TRISC
SSPBUF
;
;
org
goto START org 0x05
movlw 0x18 banksel TRISC movwf TRISC
;MSSP Module Setup movlw baud100
;0001 1000
  ;RC4 (SDA) and RC3(SCL) as inputs
  ;100KHz speed
  ;write baud rate
;1000 0000
;
banksel SSPADD movwf SSPADD
movlw 0x80 banksel SSPSTAT movwf SSPSTAT
EQU 0x14 EQU 0x05 EQU 0x91 EQU 0x02 EQU 0x00 EQU 0x94 EQU 0x07 EQU 0x93 EQU 0x31 EQU 0x0B EQU 0x87 EQU 0x13
  ;SSP Enable bit
;SMP=1 for 100KHz, SMP=0 for 400KHz (Choose 100KHZ)
;100KHZ standard speed
;400KHz fast speed  ($0B)
;line 1
0x0000
START
;i2c operation INITIALIZATION
;PORTC setup  - SDA and SCL both as inputs
            ;100KHZ (no slew rate control)
;selection with I2C mode
;
movlw 0x28 ;0010 1000 (SSPEN=1, SSPM3:0= 1000 ) master mode banksel SSPCON
movwf SSPCON
;=========================================================================== ;WRITING for a byte data
;Sequence
;1. Start event <S>
;2. Send Control Byte for EPEROM with Write Op <C>
; CONTROL BYTE STRUCTURE OF 24LC16B
; 1010A2A1A0R/W
; The first 4-bit code is establisihed for serial EEPROM by the I2C Bus Specification
; Next 3 bits are for Block Select (26LC16B has 8 blocks, with each block having 256 Bytes)
;    R/W = 1 for reading
;    R/W = 0 for writing
;3. Wait for ACK from EEPROM (Use SSPIF of PIR1)<K> ;4. Send the address in a block of EEPROM<A>
;5. Wait for ACK from EEPROM<K>
;6 Send a byte of data<D>
;7. Wait for ACK from EEPROM <K>
;8. STop EVENT<P>
Embedded Computing with PIC 16F877 – Assembly Language Approach. Charles Kim © 2006

Chapter 14. SSP module and I2C Bus for External EEPROM Access 407
;9. Give 24LC16 a few ms to write the data ;==================================================================== ; SEQUENCE IN WRITING
; PIC (master Side)>S>> >C>> >A>> >D>> >P>>
; 24LC16 (slave side) <<K< <<K< <<K< ;==================================================================== ;
;Flag Clear
banksel PIR1
bcf PIR1, SSPIF ;clear the SSP flag
;>S>> START
call i2cStart
;>C>> CONTROL movlw 0xA0
call i2cSend ;block code write to 24LC16 ;>A>> ADDRESS
movlw 0x00 ;address info
call i2cSend ;>D>> DATA
movlw 'm' ;1 byte data
call i2cSend ;>P>> STOP
call i2cStop
call delay10ms ;Give 24LC16B time to write the data
;your subroutines here
;your subroutines here
     END


