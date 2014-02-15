;--------------------------------------------------------------------
; Practice 20FinalReportCode: Final Report Code On Progress Four on PIC18F4620
; Written By : Soon Chee Loong

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
	endc	

; This macro delays the LCD by 255 instructions on 20 MHZ clock. 

LCD_DELAY macro ; copy this whole macro till endm by just writing LCD_DELAY at bottom,
				; macro means inline code, it will just copy paste into whatever section. 
	movlw   0xFF	; 11111111
	movwf   lcd_d1   ; lcd_d1 has the value 
	decfsz  lcd_d1,f ; decrement lcd_d1 by 1, skip next line if 0. 
	goto    $-2
	endm
	
;=====================================================================
; A)  LCD CODE 
;=====================================================================

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
; Look up table
;--------------------------------------------------------------------
Real_Time:
		dw		"17:04", 0			; temporary 
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
		dw		"White Dowels:  9",0
Dark_Count:
		dw		"Brown Dowels: 3 ",0
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

OPTIMEFINAL
		dw		"Operation : 90s", 0 


; The LCD code is called by using 
		call		Clear_Display
		Display		 STRING1;
		call		Switch_Lines
		Display		STRING2; 
; It will output both strings in the 2 line LCD display. 



;=====================================================================
; B) KEYPAD CODE 
;=====================================================================

	PORTB
;			movf		tempwsaveregister, W
		btfss		PORTB,3		;Wait until keypad button is pressed
		bra			$-1

; Keypad Code for MENU
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
		Display		OPTIMEFINAL ; NOT DONE YET 
		call		Switch_Lines
		Display		Key_Continue
		btfss		PORTB,3			;Wait until keypad is pressed
		bra			$-1
		bra			LASTMENU ; NEED CHANGE TO BRA COMPLETION
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
		btfss		PORTB,3			;Wait until keypad is pressed ; NOTE: IT WAITS TILL KEYPAD IS PRESSED. 
		bra			$-1
		bra			LASTMENU


; Keypad Code for patterns 1-6
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
 
;=====================================================================
; C) STORAGE INFORMATION
;=====================================================================

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

	; Begin spinning reservoirs AND LET IT SPIN TILL whenever
 
; Store number of patterns into EEDATA  
		movff		num_patterns,EEDATA
		incf		EEADR ; change EEPROM address
		call		Write_EEPROM ; write the number of patterns on EEPROM
		movf		num_patterns,w ; w holds number of patterns
		subwf		EEADR ; ; go to address of the last given pattern. 

; Dispense box into loading area
		
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
		btfss		PORTB,3		;Wait until keypad is pressed
		bra			$-1
		btfsc		PORTB,3		;Wait until keypad is released
		bra			$-1
		bra			LASTMENU

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
;		movlw	b'010101' ; => dowels will come out as black, white, black, white, black, white. 
		movlw	b'000000'
		bra		Exit_Encoder
Pattern_1:
		movlw	b'000000'
		bra		Exit_Encoder
Pattern_2:
;		movlw	b'111111'
		movlw	b'000000'
		bra		Exit_Encoder
Pattern_3:
;		movlw	b'000111' ; => dowels come out from solenoid from right => black black, black, white, white, white -> on box will be white white white-> black black, black. 
		movlw	b'000000'
		bra		Exit_Encoder
Pattern_4:
		movlw	b'000000'
;		movlw	b'001100'
		bra		Exit_Encoder
Pattern_5:
		movlw	b'000000'
;		movlw	b'110011'
		
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



; it never reaches end, it always goes to standby, though END signifies to MPLAB that this is the end of the code
	END