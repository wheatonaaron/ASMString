TITLE Project 6 - String Primitives and Macros     (Proj6_wheatoaa.asm)

INCLUDE Irvine32.inc
; ----------------------------
; Name: mDisplayString
; 
; Recieves a string and prints it.
; 
; Preconditions: String must be OFFSET before passing.
;
; Recieves:
; displayString = String to be displayed
;
; Postconditions: String is printed in console.
;
; Returns: None
; ----------------------------
mDisplayString MACRO	displayString
	PUSH	EDX
	MOV		EDX, displayString
	CALL	WriteString
	POP		EDX
ENDM

; ----------------------------
; Name: mGetString
; 
; Recieves a string from a user and stores that string in a buffer.
; 
; Preconditions: String and prompt to be used must be OFFSET properly. String length
; that can be processed must be defined.
;
; Recieves:
; prompt = Instructs user to enter a string
; buffer = buffer to store said string
; userString = The string the user provided
; userStringLen = The length of the string the user provided
;
; Postconditions: String and string length are stored.
;
; Returns: userString and userStringLen are returned for use later.
; ----------------------------
mGetString MACRO	prompt, buffer, userString, userStringLen
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX
	mDisplayString prompt ;Prompt user to enter value
	MOV		ECX, 63 ;Size of buffer
	MOV		EDX, buffer
	CALL	ReadString
	MOV		userString, EDX
	MOV		userStringLen, EAX
	POP		EDX
	POP		ECX
	POP		EAX
ENDM



.data
	; ----------------------------
	; Here our variables are defined, including prompts and instruction text, arrays for storing values, counters, booleans, buffers,
	; , as well as sizes of the arrays and flavor text.
	; ----------------------------
	programName		BYTE	"Low Level I/O Procedures",13,10,0
	myName			BYTE	"By: Aaron Wheaton",13,10,0
	intro1			BYTE	"This program will manipulate numbers that you, the user, provide to it.",13,10,0
	intro2			BYTE	"Please note these numbers have to fit within a 32 bit register.",13,10,0
	intro3			BYTE	"After providing said integers, the list of integers, their average, and sum will be displayed.",13,10,0
	userPrompt		BYTE	"Please enter a signed number: ",13,10,0
	errorMSG		BYTE	"Sorry, that number was invalid, please try again.",13,10,0
	valuesPrompt	BYTE	"The list of your values is: ",13,10,0
	sumMsg			BYTE	"The sum of these numbers is:",13,10,0
	AvgMsg			BYTE	"The truncated average of these numbers is:",13,10,0
	closingMsg		BYTE	"Thank you for using my program! - Aaron",13,10,0
	signBool		SDWORD	0
	arrayLocation	SDWORD	0
	userString		SDWORD	?
	buffer			BYTE	63 DUP(0)
	stringLen		SDWORD	?
	numArray		SDWORD	10 DUP(0)
	runningCount	SDWORD	0
	numSum			SDWORD	0
	numAvg			SDWORD	0
	stringHolder	BYTE	63 DUP(0)
	avgHolder		BYTE	63 DUP(0)
	writeNumber		SDWORD	1 DUP(?)
	byteCounter		SDWORD	1

.code
main PROC
; ----------------------------
; Name: main
; 
; Primary code block where sub procedures (and the assocaited macros) are called.
; 
; Preconditions: None, other than what is defined in the .data section
; 
;
; Recieves:
; All variables defined in .data, 10 signed integers from the user
;
; Postconditions: All procedures for string manipulation, as well as average/sum calculations are performed and displayed.
;
; Returns: The array of signed integers, the sum of those integers, and the truncated average of those integers.
; ----------------------------

	mDisplayString OFFSET programName ; Display all the intro text
	CALL		   CrLf
	mDisplayString OFFSET myName
	CALL		   CrLf
	mDisplayString OFFSET intro1
	CALL		   CrLf
	mDisplayString OFFSET intro2
	CALL		   CrLf
	mDisplayString OFFSET intro3
	CALL		   CrLf

	MOV		ECX, 10 ;Initialize Loop counter
	MOV		EAX, 0
	MOV		arrayLocation, 0
	_ReadValLoop:
		PUSH	arrayLocation ;Where in the array the string needs to be written
		PUSH	OFFSET errorMsg ;Error message for incorrect values
		PUSH	OFFSET numArray ;Array used to store all the values
		PUSH	OFFSET userPrompt ;Prompt to ask user for values
		PUSH	OFFSET buffer ;Buffer used to hold each input value
		PUSH	userString ;String the user inputs
		PUSH	stringLen ;Length of the string
		PUSH	runningCount ;"Running count" used for ASCII representation
		PUSH	signBool ;Boolean used to track if a number is negative or not
		CALL	ReadVal
		PUSH	EAX ;Push registers for string zeroing
		PUSH	ECX
		PUSH	EDI
		MOV		EAX, 0
		MOV		ECX, LENGTHOF stringHolder ;Zero out string holder
		MOV		EDI, OFFSET stringHolder
		REP		STOSB
		POP		EAX
		POP		ECX
		POP		EDI
		MOV		userString, 0 ;Zero out userstring
		ADD		arrayLocation, 4 ;Increment to next value in numArray 
		LOOP	_ReadValLoop

		mDIsplayString OFFSET valuesPrompt ;gives "here are your values" prompt to user
		MOV		ECX, 10 ;Initialize counter
		MOV		EAX, 0 ;Initialize EAX to be used for array position
	_WriteValLoop:
		MOV		signBool, 0 ;Zero out signbool (resets the boolean used to indicate # is positive or negative)
		MOV		EBX, numArray[EAX] ; Move the number to be written in EBX
		PUSH	EAX ;Push EAX, then use it to preserve the current number sum
		MOV		EAX, numSum
		ADD		numSum, EBX
		JO		_sumOverflow ; If adding the new number causes an overflow, jump to overflow handling section
		POP		EAX
		MOV		writeNumber, EBX ;Move number to be written to writeNumber if it passes checks
		ADD		writeNumber, 0 ;Add zero to see if negative flag is set to determine # is negative
		JS		_negVal
	_WriteValMain:
		PUSH	byteCounter ;Used to count how long the string is to work back to proper index
		PUSH	signBool ;see other signbool explanations
		PUSH	OFFSET writeNumber
		PUSH	OFFSET stringHolder ;Holds the string to be printed
		PUSH	OFFSET stringHolder + 62 ;Points to the end of the string, so they can be iterated backwards per ASCII formula
		CALL	WriteVal
		PUSH	EAX
		MOV		AL, ","
		CALL	WriteChar
		MOV		AL, " "
		CALL	WriteChar
		MOV		EAX, 0
		POP		EAX
		ADD		EAX, 4 ;Write comma and spacing, move to next value and loop
		LOOP	_WriteValLoop
		JMP		_avgCalc
	_negVal:
		MOV		signBool, 1 ;If value is negative, set sign bool to one, NEG flip writeNumber to positive (for ease)
		NEG		writeNumber
		JMP		_WriteValMain
	_sumOverflow:
		MOV		numSum, EAX ;If Overflow flag is triggered, restore previous value, and print error message
		mDisplayString OFFSET errorMsg
		POP		EAX

	_avgCalc:
		;Calculating average using numSum and dividing by 10
		MOV		EAX, numSum
		MOV		EBX, 10
		CDQ
		IDIV	EBX
		MOV		numAvg, EAX

	_afterWrite: ;Zero out holding string again
		CALL	CrLf
		MOV		EAX, 0
		MOV		ECX, LENGTHOF stringHolder
		MOV		EDI, OFFSET stringHolder
		REP		STOSB

	mDisplayString	OFFSET sumMsg ;gives sum prompt to user
	PUSH	byteCounter ;Very similar to write loop, same string holder etc, only now we're printing sum and avg
	PUSH	signBool
	PUSH	OFFSET numSum
	PUSH	OFFSET stringHolder
	PUSH	OFFSET stringHolder + 62
	CALL	WriteVal
	CALL	CrLf

	mDisplayString	OFFSET avgMSG ;gives trunacted average prompt to user
	PUSH	byteCounter
	PUSH	signBool
	PUSH	OFFSET numAvg
	PUSH	OFFSET avgHolder
	PUSH	OFFSET avgHolder + 62
	CALL	WriteVal
	CALL	CrLf

		CALL	farewell

main ENDP

; ----------------------------
; Name: ReadVal
; 
; Recieves a signed integer from a user as a string, then converts that string to the actual number
; using ASCII conventions.
; 
; Preconditions: Must be invoked in main, and have all the requesite variables pushed via the stack.
;
; Recieves:
; arrayLocation = Tells PROC where to write the number
; errorMSg = used to inform user of an invalid integer
; numArray = Used to store signed integers
; userPrompt = text used to prompt user for integer
; buffer = buffer used to store signed integer
; userString = String form of the integer the user provides
; stringLen = Length of string user provides
; runningCount = Used to store and combine the continual conversions of ASCII to real number
; signBool = Used to indicate a negative number
;
; Postconditions: Registers EAX, EBX, ECX, EDX, ESI, EDI are all used but are preserved via the stack.
;
; Returns: numArray holds 10 signed integers, converted from 10 user provided strings.
; ----------------------------
ReadVal			PROC
	PUSH		EBP ;Preserve stack
	MOV			EBP, ESP
	PUSH		EAX
	PUSH		EBX
	PUSH		ECX
	PUSH		EDX
	PUSH		EDI
	PUSH		ESI
	MOV			EDI, [EBP+32] ;numArray
	
	_getString:
		mGetString [EBP+28], [EBP+24], [EBP+20], [EBP+16] ;OFFSET userPrompt, OFFSET buffer, userString, stringLen
		MOV			EAX, [EBP+20] ;userString
		MOV			EBX, [EBP+16] ;stringLen

		CMP			EBX, 0 ;Checking for string length of zero (empty string)
		JE			_invalid

	_convert:
		MOV			ESI, EAX ;Move userstring to ESI
		MOV			ECX, EBX ;Utilize stringLen as ECX counter
		MOV			EAX, 0
		MOV			EDX, 0
		CLD ;Point LODSB forward


	_validate:
		MOV			EAX, 0
		LODSB		;Move ESI value to AX, increment ESI
		CMP			ECX, EBX ;If its the first character, jump to special handling
		JE			_firstCharCheck
	_validateMain:
		SUB			EAX, 48 ; Decrement to get to the "real" value, not ascii
		CMP			EAX, 0 ;Compare against acceptable range
		JL			_invalid
		CMP			EAX, 9
		JG			_invalid ;If it's greater than 9 or less than 0, not valid
		PUSH		EAX
		PUSH		EBX
		PUSH		ECX
		MOV			ECX, EAX ;Save EAX in ECX in case we need to restore it
		MOV			EAX, [EBP+12] ;Running count holder for ASCII conversion
		MOV			EBX, 10 ;Multiply by 10, check overflow flag
		MUL			EBX
		JO			_invalidprepop ;Needed this extra invalid to handle stack wonkiness if you didnt pop after 3 pushes above
		MOV			[EBP+12], EAX ;Move multiplied value back into running count
		POP			ECX
		POP			EBX
		POP			EAX
		JO			_invalid ;OF flag checked twice, on addition, and on multiplication
		ADD			[EBP+12], EAX ;Add to running count variable, also check overflow
		JO			_invalid
		_validsymbol:
			LOOP		_validate
			JMP			_loopEnd

	_firstCharCheck:
		CMP			EAX, 43 ;Check if first character is a plus, if so move on
		JE			_validsymbol
		CMP			EAX, 45 ;Check if first character is a minus, if it is, set signbool to 1 (used later)
		JE			_validneg
		SUB			EAX, 48 ; Decrement to get to the "real" value, not ascii
		CMP			EAX, 0 ;Compare against acceptable range
		JL			_invalid
		CMP			EAX, 9
		JG			_invalid ;If it's greater than 9 or less than 0, not valid
		ADD			[EBP+12], EAX
		JMP			_validsymbol

	_validneg:
		PUSH		EAX ;If first char is a minus, set signbool
		MOV			EAX, 1
		MOV			[EBP+8], EAX
		POP			EAX
		JMP			_validsymbol

	_invalid:
		MOV			EAX, 0 ;If its invalid, reset running count and warn user, then reset
		MOV			[EBP+12], EAX
		mDisplayString [EBP+36] ;error message
		JMP			_getString

	_invalidPrePop:
		POP			ECX ;If you get an overflow from multiplication, this properly restores the stack then sends up an error message
		POP			EBX
		POP			EAX
		MOV			EAX, 0
		MOV			[EBP+12], EAX
		mDisplayString [EBP+36] ;error message
		JMP			_getString

	_loopEnd:
		MOV			EBX, 1 
		CMP			[EBP+8], EBX
		JE			_negnum ;If signbool is one, go to negative case handle
	_loopEndMain:
		MOV			EAX, [EBP+12] ; Move running count to EAX
	_negNumMain:
		MOV			EBX, [EBP+40] ;Move array pointer using arrayLocation
		ADD			EDI, EBX
		MOV			[EDI], EAX ;Move number converted from ASCII to real into array
		POP			ESI
		POP			EDI
		POP			EDX
		POP			ECX
		POP			EBX
		POP			EAX
		POP			EBP
		RET			40

	_negnum:
		MOV			EAX, [EBP+12] ;If number is negative, we do all calcs with positive numbers then flip at the end here
		MOV			ECX, -1
		CDQ
		IMUL		ECX
		JMP			_negNumMain ;Negative number is then put into the array
ReadVal ENDP

; ----------------------------
; Name: WriteVal
; 
; Takes an input signed integer and writes it in the console as an ASCII string.
; 
; Preconditions: Must be invoked in main, and have all the requesite variables pushed via the stack.
;
; Recieves:
; byteCounter = Keeps track of exact byte location during STOSB process.
; signBool = Used to indicate a negative number
; writeNumber = The number to be written as an ASCII string
; stringHolder = Holds string to be printed
;
; Postconditions: Registers EAX, EBX, ECX, EDX, ESI, EDI are all used but are preserved via the stack.
;
; Returns: Converts the input signed integer into an ASCII string and prints it in the console.
; ----------------------------
WriteVal PROC
	PUSH		EBP
	MOV			EBP, ESP
	PUSH		EAX
	PUSH		EBX
	PUSH		ECX
	PUSH		EDX
	PUSH		EDI
	PUSH		ESI
	MOV			ESI, [EBP+16] ;writeNumber into ESI
	MOV			EAX, [ESI] ;Move number to be written into EAX
	MOV			EDI, [EBP+8] ;StringHolder to be written into  
	MOV			EBX, 10 ;EBX to be used for division conversion back to ASCII

	ADD			EAX, 0 ;Checking if number is negative
	JS			_flipSign
	JMP			_noFlip
	_flipSign:
		NEG			EAX
		PUSH		EBX
		MOV			EBX, 1 ;If number is negative, set sign bool and NEG flip to positive
		MOV			[EBP+20], EBX
		POP			EBX
	_noFlip:
		STD
		PUSH		EDI ;If number is positive, zero out stringHolder value
		MOV			EDX, 0
		MOV			[EDI], EDX
		MOV			ECX, 0
	_mainWriteLoop:
		CMP			EAX, 0
		JE			_writeEnd ;If there's nothing left to divide, jump to the end
		MOV			EDX, 0 ;Clear out remainder
		IDIV		EBX ;Divide number by 10 (we pushed 10 to EBX earlier)
		PUSH		EAX
		MOV			AL, DL ;Preserve EAX, then move DL to AL for STOSB
		ADD			AL, 48 ;Add 48 to convert to ASCII
		STOSB
		PUSH		EBX
		PUSH		EDX
		MOV			EBX, [EBP+24] ;Move byte counter value to EBX, increment it, and move it back out
		INC			EBX ;Increment byte counter
		MOV			[EBP+24], EBX
		POP			EDX
		POP			EBX
		POP			EAX
		CMP			EDX, 0
		JE			_writeEnd ;If remainder is zero, division is over
		CMP			EDX, 10
		JL			_mainWriteLoop ;If remainder exists but is less than 10 need to divide again
		JMP			_writeEnd

	_writeEnd:
		MOV			EBX, [EBP+20] ;Move sign bool into EBX
		CMP			EBX, 1 ;Check if sign bool is true for negative
		JE			_endNeg

		CMP			EAX, 10 ;Edge case for if EAX is 10 exactly
		JGE			_mainWriteLoop
		POP			EDI
		INC			EDI
	_nullTerm:
		CLD			;Write forward now
		ADD			AL, 00 ;Add null term at end of string
		STOSB
	_noAdd:
		MOV			EAX, [EBP+24] ;Edge case for not needing null term
		SUB			EDI, EAX
	
	_endMain:
		mDisplayString EDI ;Print the string using previous macro, then restore stack
		POP			ESI
		POP			EDI
		POP			EDX
		POP			ECX
		POP			EBX
		POP			EAX
		POP			EBP
		RET			8

	_endNeg:
		PUSH		EAX ;If its negative, write negative sign, then move to other side of string and null term
		MOV			AL, 45
		STOSB
		POP			EAX
		POP			EDI
		CLD
		INC			EDI
		MOV			AL, 00
		STOSB
		DEC			EDI
		MOV			EAX, [EBP+24]
		SUB			EDI, EAX
		JMP			_endMain
WriteVal ENDP
; ----------------------------
; Name: farewell
; 
; Simply prints a farewell message to the user.
; 
; Preconditions: Must be invoked in main.
; Recieves:
; closingMsg = Thanks the user and bids them farewell!
;
; Postconditions: Registers EAX, EBX, ECX, are modified via macro but preserved via the stack.
;
; Returns: Farewell message is returned, and program is closed.
; ----------------------------
farewell PROC
				mDisplayString	OFFSET closingMsg ;give closing message to user
				Invoke ExitProcess,0
farewell ENDP
END main
