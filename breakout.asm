#Display Resolution = 256 x 256
#Unit Width in pixels = 2
#Unit Height in pixels = 2
#Effective resolution for memory address purposes = 128 x 128
.data
#Status register bits
EXC_ENABLE_MASK:        .word   0x00000001

#Cause register bits
EXC_CODE_MASK:          .word   0x0000003c  #Exception code bits

EXC_CODE_INTERRUPT:     .word   0   #External interrupt
EXC_CODE_ADDR_LOAD:     .word   4   #Address error on load
EXC_CODE_ADDR_STORE:    .word   5   #Address error on store
EXC_CODE_IBUS:          .word   6   #Bus error instruction fetch
EXC_CODE_DBUS:          .word   7   #Bus error on load or store
EXC_CODE_SYSCALL:       .word   8   #System call
EXC_CODE_BREAKPOINT:    .word   9   #Break point
EXC_CODE_RESERVED:      .word   10  #Reserved instruction code
EXC_CODE_OVERFLOW:      .word   12  #Arithmetic overflow

#Status and cause register bits
EXC_INT_ALL_MASK:       .word   0x0000ff00  #Interrupt level enable bits

EXC_INT0_MASK:          .word   0x00000100  #Software
EXC_INT1_MASK:          .word   0x00000200  #Software
EXC_INT2_MASK:          .word   0x00000400  #Display
EXC_INT3_MASK:          .word   0x00000800  #Keyboard
EXC_INT4_MASK:          .word   0x00001000
EXC_INT5_MASK:          .word   0x00002000  #Timer
EXC_INT6_MASK:          .word   0x00004000
EXC_INT7_MASK:          .word   0x00008000

	Colors: .word   0x000000        #background color (black)
        	.word   0xffffff        #foreground color (white)
	ColorTable:				#Table for colors (provided in lecture)
		.word 0x000000 #black
		.word 0x0000ff #blue
		.word 0x00ff00 #green
		.word 0xff0000 #red
		.word 0x00ffff #blue + green
		.word 0xff00ff #blue + red
		.word 0xffff00 #green + red
		.word 0xffffff #white
	
	#Border Data table
	BorderTable:	#Table to store all the border lines information
			#First row = Left border (vertical line) data
			#Second row = Right border (vertical line) data
			#Third row = Top border (horizontal line) data
			#First column = X coordinate
			#Second column = Y coordinate
			#Third column = Line length
		.word 23, 9, 82
		.word 104, 9, 82
		.word 23, 9, 82
	
	#Table to store all data associated with the ball
	BallData: .word 60, 85, 60, 85, 3, 2, 1, 1, -1 	#First entry =  Current Top-Left X coordinate
							#Second entry = Current Top-Left Y coordinate
							#Third entry =  Old Top-Left X coordinate
							#Fourth entry = Old Top-Left Y coordinate
							#Fifth entry =  Ball size
							#Sixth entry =  X-axis speed
							#Seventh entry = X-axis direction 
							#Eight entry = Y-axis speed 
							#Nineth entry = Y-axis direction

	
	#Table to store all data associated with the paddle:
	PaddleData: .word 56, 88, 40, 40, 13, 2, 10, 1 	 #First entry =  Current Top-Left X coordinate
							 #Second entry = Current Top-Left Y coordinate
							 #Third entry =  Old Top-Left X coordinate
							 #Fourth entry = Old Top-Left Y coordinate
							 #Fifth entry =  Paddle Width
							 #Sixth entry =  Paddle Height
							 #Seventh entry = X-axis speed 
							 #Eight entry = X-axis direction 
	
	#Table to store the original ball position
	OriginalBallPosition:	.word	60, 85		#First entry = Top-left X coordinate
							#Second entry = Top-Left Y coordinate

	#Table to store the original paddle position
	OriginalPaddlePosition:	.word	56, 88		#First entry = Top-left X coordinate
							#Second entry = Top-Left Y coordinate
	#Each brick will be a 10x5 box.
	#Each brick is stuck next to each other 
	#X coordinate of a brick= word at (BricksLayoutTable + 0) + (Column index * Width of brick)
	#Y coordinate of a brick = word at (BricksLayoutTable + 4 bytes) + (Row index * Height of brick)
	#Color of row = Color of previous row - 1
	BricksLayout:			#Table to define layout for bricks
					#There will be 8 bricks per row
					#There will be 4 rows
					#Width of brick = 10
					#First column = Starting X coordinate
					#Second column = Startng Y coordinate
					#Third column = Color index
		.word 24, 10, 6

	#Default Direction
	BallYDirection:	.word	-1			#Default Ball Y direction (up)

	#Game music -> pitch duration - other declrations are below
	BrickDestroyedDuration:	.word	90		#Duration of note to play on brick destruction
	PaddleCollisionDuration:.word	90		#Duration of note to play on paddle collision
	BorderCollisionDuration:.word	90		#Duration of note to play on border collision
	GameLostDuration:	.word	300, 300, 300	#Duration of notes to play on game loss
	GameWonDuration:	.word	300, 400, 500, 400, 300		#Duration of notes to play on game won


	
	PressSpaceMsg:	.asciiz "PRESS SPACE"	#Message to instruct user to press space to start
	PressAMsg:	.asciiz	"<-A"		#Message to help tell user to press A to move to left
	PressDMsg:	.asciiz "D->"		#Message to help tell user to press D to move to right
	
	EmptyMsg:	.asciiz "           "	#Empty message to clear area occupied by Press space msg
	EmptyAAndDMsg:	.asciiz "   "		#Empty message to clear A and D instructions
	EmptyScoreMsg:  .asciiz "  "		#Empty string to erase score string
	EmptyLivesMsg:	.asciiz " "		#Empty string to erase lives string 
	
	GameWonMsg:	.asciiz	"YOU WON!"	#Message to show when player wins
	GameLostMsg:	.asciiz	"YOU LOST!"	#Message to show when player loses
	
	ScoreMsg:	.asciiz "  "		#Two characters + null character store integer score as string
	LivesMsg:	.asciiz " "		#One characters + null character store integer lives as string
	
	#Brick Grid specifications
	BricksGrid:		.byte 4, 8	#Number of Rows and Columns of brick table 
	#Brick dimensions
	BrickDimensions:	.byte 10, 5	#Width and height of a brick
	
	#Important color indices
	BlackIndex:	.byte 0			#Index of black in color table
	WhiteIndex:	.byte 7			#Index of white in color table
	
	#Game fields - Score and Lives
	Score:		.byte 0, 1			#Field to keep track of score count (0-32)
							#First entry = current score
							#Second entry = previous score (initialized with any random value to not match current score)
	Lives:		.byte 5, 6			#Field to keep track of lives (5-0). End game when this reaches 0
							#First entry = current lives
							#Second entry = prev. lives (initialized with any random value to not match current lives)
	#Ball Speed limits
	BallXMaxSpeed:	.byte	4			#Ball's max speed (X and Y)
	BallXMinSpeed:	.byte	2			#Ball's X min speed
	

	#Game music (everything except pitches)
	BrickDestroyedPitch:	.byte	96		#note to play on brick destruction
	PaddleCollisionPitch:	.byte	84		#note to play on paddle collision
	BorderCollisionPitch:	.byte	78		#note to play on border collision
	GameLostPitch:		.byte	46, 50, 54		#notes to play on game lost
	GameWonPitch:		.byte	64, 68, 72, 76, 80	#notes to play on game won
	
	Instrument:	.byte	118			#Use synth drum instrument
	Volume:		.byte	127			#Volume of note
	
	
	#Game flags
	GameActive:	.byte 0			#Flag to check if game is active
	PaddleFree:	.byte 0			#Flag to check if paddle is free to move
	
	DigitTable:				#Table to show characters on bitmap display (Values upto F obtained from lecture. Additional characters added to complete the game
						#Each entry in a row is a byte (8-bits) and there are 12 bytes
						#Each character is therefore a 8x12 character
						#I used PixilArt's online canvas to help make the entries (fisrt made A to see what I am working with)
        	.byte   ' ', 0,0,0,0,0,0,0,0,0,0,0,0
        	.byte   '0', 0x7e,0xff,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xff,0x7e
        	.byte   '1', 0x38,0x78,0xf8,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18
        	.byte   '2', 0x7e,0xff,0x83,0x06,0x0c,0x18,0x30,0x60,0xc0,0xc1,0xff,0x7e
        	.byte   '3', 0x7e,0xff,0x83,0x03,0x03,0x1e,0x1e,0x03,0x03,0x83,0xff,0x7e
       		.byte   '4', 0xc3,0xc3,0xc3,0xc3,0xc3,0xff,0x7f,0x03,0x03,0x03,0x03,0x03
        	.byte   '5', 0xff,0xff,0xc0,0xc0,0xc0,0xfe,0x7f,0x03,0x03,0x83,0xff,0x7f
        	.byte   '6', 0xc0,0xc0,0xc0,0xc0,0xc0,0xfe,0xfe,0xc3,0xc3,0xc3,0xff,0x7e
        	.byte   '7', 0x7e,0xff,0x03,0x06,0x06,0x0c,0x0c,0x18,0x18,0x30,0x30,0x60
        	.byte   '8', 0x7e,0xff,0xc3,0xc3,0xc3,0x7e,0x7e,0xc3,0xc3,0xc3,0xff,0x7e
        	.byte   '9', 0x7e,0xff,0xc3,0xc3,0xc3,0x7f,0x7f,0x03,0x03,0x03,0x03,0x03
        	.byte   '+', 0x00,0x00,0x00,0x18,0x18,0x7e,0x7e,0x18,0x18,0x00,0x00,0x00
        	.byte   '-', 0x00,0x00,0x00,0x00,0x00,0x7e,0x7e,0x00,0x00,0x00,0x00,0x00
        	.byte   '*', 0x00,0x00,0x00,0x66,0x3c,0x18,0x18,0x3c,0x66,0x00,0x00,0x00
        	.byte   '/', 0x00,0x00,0x18,0x18,0x00,0x7e,0x7e,0x00,0x18,0x18,0x00,0x00
        	.byte   '=', 0x00,0x00,0x00,0x00,0x7e,0x00,0x7e,0x00,0x00,0x00,0x00,0x00
        	.byte	'>', 0x00,0xc0,0x60,0x30,0x18,0x0c,0x06,0x0c,0x18,0x30,0x60,0xc0
        	.byte	'<', 0x00,0x03,0x06,0x0c,0x18,0x30,0x60,0x30,0x18,0x0c,0x06,0x03
        	.byte   '!', 0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x00,0x18,0x18
        	.byte   'A', 0x18,0x3c,0x66,0xc3,0xc3,0xc3,0xff,0xff,0xc3,0xc3,0xc3,0xc3
        	.byte   'B', 0xfc,0xfe,0xc3,0xc3,0xc3,0xfe,0xfe,0xc3,0xc3,0xc3,0xfe,0xfc
        	.byte   'C', 0x7e,0xff,0xc1,0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xc1,0xff,0x7e
        	.byte   'D', 0xfc,0xfe,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xfe,0xfc
        	.byte   'E', 0xff,0xff,0xc0,0xc0,0xc0,0xfe,0xfe,0xc0,0xc0,0xc0,0xff,0xff
        	.byte   'F', 0xff,0xff,0xc0,0xc0,0xc0,0xfe,0xfe,0xc0,0xc0,0xc0,0xc0,0xc0
        	.byte	'L', 0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xc0,0xff,0xff
        	.byte	'N', 0xc1,0xe1,0xa1,0xa1,0xb1,0x91,0x91,0x99,0x8d,0x85,0x87,0x83
        	.byte	'O', 0x7e,0xff,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xc3,0xff,0x7e	#Copied zero's
		.byte   'P', 0xff,0xc3,0xc3,0xc3,0xc3,0xc3,0xff,0xff,0xc0,0xc0,0xc0,0xc0
		.byte   'R', 0xfc,0xfe,0xc3,0xc3,0xc3,0xfe,0xfc,0xcc,0xc6,0xc3,0xc3,0xc3
		.byte   'S', 0x7e,0xff,0xc0,0xc0,0xc0,0x7e,0x7e,0x03,0x03,0x03,0xff,0x7e
		.byte	'T', 0xff,0xff,0xff,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18
		.byte	'U', 0x81,0x81,0x81,0x81,0x81,0x81,0x81,0x81,0x81,0xc3,0x66,0x3c
		.byte	'V', 0x81,0x81,0x81,0x81,0x81,0x81,0xc3,0xc3,0x42,0x66,0x3c,0x18
		.byte	'W', 0x81,0x81,0x81,0x81,0x81,0x99,0xbd,0xbd,0xa5,0xa5,0xa5,0xe7 
		.byte	'Y', 0x81,0x81,0xc3,0x42,0x66,0x24,0x3c,0x18,0x18,0x18,0x18,0x18
	
	BricksLeft: .space 32		#An array to store the 32 bricks. 0 means brick is safe. 1 means brick is destroyed.
.text

main:	#Main code
	#Enable interrupts in status register
	mfc0    $t0, $12

	#Disable all interrupt levels
	lw      $t1, EXC_INT_ALL_MASK
	not     $t1, $t1
	and     $t0, $t0, $t1
	
	#Enable console interrupt levels
	lw      $t1, EXC_INT3_MASK
	or      $t0, $t0, $t1
	#lw      $t1, EXC_INT4_MASK
	#or      $t0, $t0, $t1

	#Enable exceptions globally
	lw      $t1, EXC_ENABLE_MASK
	or      $t0, $t0, $t1

	mtc0    $t0, $12
	
	#Enable keyboard interrupts
	li      $t0, 0xffff0000     #Receiver control register
	li      $t1, 0x00000002     #Interrupt enable bit
	sw      $t1, ($t0)

	#Setup initial game state
	jal InitRand
	jal GenerateBallXDirection
	jal DrawBorder
	jal DrawBricks
	jal DrawBall
	jal DrawPaddle
	jal ShowPressSpaceMessage
	jal ShowLives
	jal ShowScore
	
	#Initialize all bricks as not destroyed = 0
	la $t0, BricksLeft
	#li $t1, 0		#Value to store = 0
	li $t2, 32		#Number of bricks
InitBricks:
   	sb $zero, 0($t0)	#Store 0
    	addiu $t0, $t0, 1	#Increase index of array (array address)
    	addiu $t2, $t2, -1	#Decrease count of brick left 
    	bne $t2, $zero, InitBricks	#Continue putting 0 if bricks are left
	
	#Set the game active flag
	li $t0, 1
	sb $t0, GameActive
	
	#Show press space message
	#sb $zero, PaddleFree
	jal ShowPressSpaceMessage
	
	#Wait for space press to start game
WaitForStart:
	lbu $s0, PaddleFree
    	beqz $s0, WaitForStart    #If paddle flag is not 1, then user has not pressed space. Continue waiting
    	#Duplicate check makes the wait loop work-------------------------------------------------------------
	lbu $s0, PaddleFree
    	beqz $s0, WaitForStart    #If paddle flag is not 1, then user has not pressed space. Continue waiting
    	
	#Space pressed, clear press space message and show instructions
	jal ClearPressSpaceMessage
	jal ShowInstructions
	#Start the game loop
	j GameLoop
	
	#Exit program
	#li $v0, 10
	#syscall		#Syscall 10 to terminate the program

#Main Game Loop
GameLoop:
	#Check if game is still active
	#If not, then user has lost all lives -> game has ended
	lbu $t0, GameActive
	lbu $s0, PaddleFree
	beqz $t0, GameEnd
	beqz $s0, GameLoop
	#Main game mechanics
	jal EraseBall			#Erase ball at old position
	jal MoveBall			#Update ball position
	jal CheckBorderCollision	#Check and handle border collisions
	jal CheckBrickCollision		#Check and handle brick collisions
	jal DrawBall			#Draw ball at new position
	jal DrawPaddle			#Update paddle display
	jal CheckPaddleCollision	#Check paddle collision
	
	#Check if player lost a life
	beq $v0, 1, PlayerLostLife
	
	#Update score card. Lives count update is handled in PlayerLostLife
	#jal ShowLives
	jal ShowScore
	
	#Check win condition (score = 32)
	lbu $t0, Score + 0
	li $t1, 32
	beq $t0, $t1, PlayerWon
	
	#Add delay for playable speed
	li $v0, 32			#Sleep syscall
	li $a0, 33			#33 milliseconds delay
	syscall
	
	j GameLoop			#Continue game loop

PlayerLostLife:				#Label to handle logic for losing a life
	#Decrement lives
	lbu $t0, Lives + 0
	subiu $t0, $t0, 1
	sb $t0, Lives + 0
	
	jal ShowLives	#Update lives display
	
	#Check if game over (lives = 0)
	lbu $t0, Lives + 0
	beqz $t0, PlayerLost
	
	#Player still has lives, reset for next round
	sb $zero, PaddleFree	#Reset paddle free flag
	
	jal ClearInstructions
	
	jal ResetBallParameters

	jal ResetPaddleParameters

	#Show press space message
	jal ShowPressSpaceMessage
	
	#Wait for space press to continue
WaitForContinue:
	lbu $s0, PaddleFree
	#nop
	#nop
	beqz $s0, WaitForContinue
	#nop
	#Duplicate check makes the wait loop work-------------------------------------------------------------
	lbu $s0, PaddleFree
    	beqz $s0, WaitForContinue    #If paddle flag is not 1, then user has not pressed space. Continue waiting
    	
    	
	#Space pressed, clear press space message and show instructions
	jal ClearPressSpaceMessage
	jal ShowInstructions
	jal GenerateBallXDirection
	#Resume game loop
	j GameLoop

PlayerLost:
	#Game over - no lives left
	jal ShowGameLostMessage
	jal PlayLosingMusic
	sb $zero, GameActive		#Set game as inactive
	j GameLoop

PlayerWon:
	#Player achieved winning score
	jal ShowGameWonMessage
	jal PlayWinningMusic
	sb $zero, GameActive		#Set game as inactive
	j GameLoop

GameEnd:
	#Game finished, wait or exit
	li $v0, 10
	syscall
	
#Paddle related functions (logic and drawing)------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Procedure: CheckPaddleCollision
#Check if ball collides with any paddle and handle collision

#Output: $v0 = 0 if ball is above paddle. $v0 = 1 if ball falls below paddle (player loses life)
CheckPaddleCollision:
	addiu $sp, $sp, -32
	sw $ra, 28($sp)
	sw $s0, 24($sp)
	sw $s1, 20($sp)
	sw $s2, 16($sp)
	sw $s3, 12($sp)
	sw $s4, 8($sp)
	sw $s5, 4($sp)
	sw $s6, 0($sp)
	
	la $s0, BallData
	lw $s1, 0($s0)		#Ball X
	lw $s2, 4($s0)		#Ball Y
	lw $s3, 16($s0)		#Ball size
	
	la $s4, PaddleData
	lw $s5, 0($s4)		#Paddle X
	lw $s6, 4($s4)		#Paddle Y
	lw $t0, 16($s4)		#Paddle width
	lw $t1, 20($s4)		#Paddle height
	
	#Check if ball has fallen below paddle (life lost)
	addu $t2, $s6, $t1	#Paddle bottom Y = Paddle Y + Paddle height
	addu $t3, $s2, $s3	#Ball bottom Y = Ball Y + Ball size
	
	bgeu $t3, $t2, BallFellBelow	#If Ball bottom > Paddle bottom, life lost
	
	#Collision detection
	#Y-axis overlap
	#addu $t4, $s2, $s3			#Ball bottom = Ball Y + Ball size
	bltu $t3, $s6, NoPaddleCollision	#If Ball bottom < Paddle top, no collision
	#bgtu $s2, $t2, NoPaddleCollision	#If Ball top > Paddle bottom, no collision
	
	#X-axis overlap
	addu $t5, $s1, $s3			#Ball right = Ball X + Ball size
	bltu $t5, $s5, NoPaddleCollision	#If Ball right < Paddle left, no collision
	
	addu $t6, $s5, $t0		#Paddle right = Paddle X + Paddle width
	bgtu $s1, $t6, NoPaddleCollision	#If Ball left > Paddle right, no collision
	
	#If reached here, collision detected 
	jal PlayPaddleCollisionMusic
	#Reverse Ball Y-direction
	lw $t7, 32($s0)		#Load current Y direction
	neg $t7, $t7		#Reverse Y direction
	sw $t7, 32($s0)		#Store new Y direction
	
	#Calculate new X speed based on collision position
	addu $t8, $s5, $t0	#Paddle right = Paddle X + Paddle width
	srl $t0, $t0, 1		#Paddle width / 2
	addu $t8, $s5, $t0	#Paddle center X = Paddle X + (Paddle width / 2)
	
	addu $t9, $s1, $s3	#Ball right = Ball X + Ball size
	srl $t3, $s3, 1		#Ball size / 2  
	addu $t9, $s1, $t3	#Ball center X = Ball X + (Ball size / 2)
	
	subu $t7, $t9, $t8       #Offset = Ball Center X - Paddle Center X
	
	#Use linear interpolation to find the new X speed
	#Linear interpolation formula: y = y1 + ((x - x1) * (y2 - y1)) / (x2 - x1))
	#y = New X speed
	#y1 = Ball min X speed
	#y2 = Ball max X speed
	#x = offset (between ball and paddle center) = x - x1
	#x1 = start of input range = start of paddle from either side = 0
	#x2 = end of input range = paddle width / 2
	#Absolute valus of offset will be used because direction is handled else where
	#New X speed = Ball min X speed + (|Offset| * (max X speed - min X speed)) / (Paddle width / 2)
	#This gives speed range [2, 4] based on distance from center
	
	#Load speed limits from memory
	lbu $t2, BallXMinSpeed + 0	#$t2 = min X speed
	lbu $t3, BallXMaxSpeed + 0	#$t3 = max X speed
	subu $t6, $t3, $t2	#$t6 = speed range = max X speed - min X speed
	
	#Get absolute value of offset
	bltz $t7, NegativeOffset
	move $t4, $t7		#$t4 = |Offset|
	j ContinueSpeedCalculation
	
NegativeOffset:
	neg $t4, $t7		#$t4 = - Offset = |Offset|
	
ContinueSpeedCalculation:

	mul $t5, $t4, $t6		#$t5 = |Offset| * speed range
	div $t5, $t0			#(|Offset| * speed range) / (Paddle width / 2)
	mflo $t5			#$t5 = (|Offset| * speed range) / (Paddle width / 2)
	
	lbu $t0, BallXMinSpeed + 0	#$t0 = min X speed
	addu $t0, $t0, $t5		#$t = min X speed + (|Offset| * speed range) / (Paddle width / 2)
	sw $t0, 20($s0)			#Store speed
	
	#Detect what side of the paddle was hit
	lw $t0, 16($s4)		#Paddle width
	srl $t0, $t0, 1		#Paddle width / 2
	addu $t0, $s5, $t0	#Paddle X + (Paddle width / 2)
	
	bgtu $s1, $t0, HitPaddleRightSide	#If Ball left > (Paddle left + Paddle width / 2), collision on right side
	#Else, collision on left side
HitPaddleLeftSide:
	li $t7, -1		#Set X direction to left
	sw $t7, 24($s0)		#Store X direction
	j CollisionSuccess
	
HitPaddleRightSide:
	li $t7, 1		#Set X direction to right
	sw $t7, 24($s0)		#Store X direction
	j CollisionSuccess
	
CollisionSuccess:
	li $v0, 0		#Return 0 for successful collision
	j RestoreAndReturn
	
BallFellBelow:
	li $v0, 1		#Return 1 for life lost
	j RestoreAndReturn
	
NoPaddleCollision:
	li $v0, 0		#Return 0 (no collision, and no life lost yet)
	
RestoreAndReturn:
	#Restore registers
	lw $s6, 0($sp)
	lw $s5, 4($sp)
	lw $s4, 8($sp)
	lw $s3, 12($sp)
	lw $s2, 16($sp)
	lw $s1, 20($sp)
	lw $s0, 24($sp)
	lw $ra, 28($sp)
	addiu $sp, $sp, 32
	
	jr $ra			#Return to caller

#Procedure: DrawPaddle
#Draw the game paddle using the game paddle data table
#It is white in color

#No input/ouput registers
DrawPaddle:
	addiu $sp, $sp, -24	#Make space on stack for 6 words
	sw $ra, 20($sp)		#Store $ra on stack
	sw $s0, 16($sp)		#Store $s0 on stack
	sw $s1, 12($sp)		#Store $s1 on stack
	
	la $t0, PaddleData	#$t0 = address of Paddle data table
	lw $a0, 0($t0)		#$a0 = Paddle Top-Left X
	#lw $a1, 4($t0)		#$a1 = Paddle Top-Left Y
	
	lw $s0, 8($t0)		#$s0 = Paddle Old Top Left X
	beq $a0, $s0, EndPaddleLoop	#No need to redraw since paddle hasn't moved
	
	#Else, erase the old paddle first
	jal ErasePaddle
	
	#Reload paddle data after function call
	la $t0, PaddleData	#$t0 = address of Paddle data table
	lw $a0, 0($t0)		#$a0 = Paddle Top-Left X
	lw $a1, 4($t0)		#$a1 = Paddle Top-Left Y
	
	lbu $a2, WhiteIndex	#$a2 = white color index
	
	lw $s0, 16($t0)		#$s0 = Paddle width
	lw $s1, 20($t0)		#$s1 = Paddle height
	
	#Store the initial coordinates and color on stack
	sw $a0, 8($sp)		#Store X coordinate
	sw $a1, 4($sp)		#Store current Y coordinate on stack
	sw $a2, 0($sp)		#Store color
	
PaddleLoop:
	sw $a1, 4($sp)		#Store current Y coordinate on stack
	move $a3, $s0		#$a3 = Paddle width (length of horizontal line)
	
	jal HorzLine		#Draw horizontal line
	
	lw $a0, 8($sp)		#Restore X coordinate
	lw $a1, 4($sp)		#Restore Y coordinate
	lw $a2, 0($sp)		#Restore color
	
	addiu $a1, $a1, 1	#Increment Y coordinate
	subi $s1, $s1, 1	#Decrease remaining paddle height
	bgtz $s1, PaddleLoop	#Continue if height remaining > 0

CopyNewPaddleCoordinatesToOldPaddleCoordinates:
	la $t0, PaddleData	#$t0 = address of Paddle data table
	lw $a0, 0($t0)		#$a0 = Paddle Top-Left X
	lw $a1, 4($t0)		#$a1 = Paddle Top-Left Y
	sw $a0, 8($t0)		#Old X = New X
	sw $a1, 12($t0)		#Old Y = New Y

EndPaddleLoop:
	lw $ra, 20($sp)		#Restore $ra
	lw $s0, 16($sp)		#Restore $s0
	lw $s1, 12($sp)		#Restore $s1
	addiu $sp, $sp, 24	#Readjust stack pointer
	
	jr $ra			#Return to caller

#Procedure: ErasePaddle
#Erase the game paddle using the game paddle data table
#Draw a black paddle over the old paddle

#No input/ouput registers
ErasePaddle:
	addiu $sp, $sp, -24	#Make space on stack for 6 words
	sw $ra, 20($sp)		#Store $ra on stack
	sw $s0, 16($sp)		#Store $s0 on stack
	sw $s1, 12($sp)		#Store $s1 on stack
	
	la $t0, PaddleData	#$t0 = address of Paddle data table
	
	lw $a0, 8($t0)		#$a0 = Paddle Old Top-Left X
	lw $a1, 12($t0)		#$a1 = Paddle Old Top-Left Y
	
	lw $s0, 16($t0)		#$s0 = Paddle width
	lw $s1, 20($t0)		#$s1 = Paddle height
	
	lbu $a2, BlackIndex	#$a2 = black color index
	
	#Store the initial coordinates and color on stack
	sw $a0, 8($sp)		#Store X coordinate
	sw $a1, 4($sp)		#Store current Y coordinate on stack
	sw $a2, 0($sp)		#Store color
	
ErasePaddleLoop:
	sw $a1, 4($sp)		#Store current Y coordinate on stack
	move $a3, $s0		#$a3 = Paddle width
	
	jal HorzLine		#Draw horizontal line
	
	lw $a0, 8($sp)		#Restore X coordinate
	lw $a1, 4($sp)		#Restore Y coordinate
	lw $a2, 0($sp)		#Restore color
	
	addiu $a1, $a1, 1	#Increment Y coordinate
	subi $s1, $s1, 1	#Decrease remaining paddle height
	bne $s1, $0, ErasePaddleLoop	#Continue if height remaining > 0
	
EndErasePaddleLoop:
	lw $ra, 20($sp)		#Restore $ra
	lw $s0, 16($sp)		#Restore $s0
	lw $s1, 12($sp)		#Restore $s1
	addiu $sp, $sp, 24	#Readjust stack pointer
	
	jr $ra			#Return to caller

	
#Procedure: ResetPaddleParameters
#Reset paddle position

#No input/ouput registers	
ResetPaddleParameters:

	lw $t0, OriginalPaddlePosition + 0
	sw $t0, PaddleData + 0			#Restore Paddle (Top-Left) X

	lw $t0, OriginalPaddlePosition + 4
	sw $t0, PaddleData + 4			#Restore Paddle (top-Left) Y
	
	jr $ra
#Ball related functions (logic and drawing)------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

	
#Procedure: DrawBall
#Draw the ball at its current position

#No input/ouput registers
DrawBall:
	subi $sp, $sp, 4
	sw $ra, 0($sp)		#Store $ra on stack
	
	la $t0, BallData
	lw $a0, 0($t0)		#Current X coordinate
	lw $a1, 4($t0)		#Current Y coordinate
	lbu $a2, WhiteIndex	#White color
	lw $a3, 16($t0)		#Ball size
	
	jal DrawBox
	
	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra			#Go back to caller

#Procedure: EraseBall
#Erase the ball at its old position by drawing a black box

#No input/ouput registers
EraseBall:
	subi $sp, $sp, 4
	sw $ra, 0($sp)		#Store $ra on stack
	
	la $t0, BallData
	lw $a0, 8($t0)		#Old X coordinate
	lw $a1, 12($t0)		#Old Y coordinate
	lbu $a2, BlackIndex	#Black color index
	lw $a3, 16($t0)		#Ball size
	
	jal DrawBox		#Earase the ball by drawing a black ball over it
	
	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra			#Go back to caller

#Procedure: MoveBall
#Update ball position based on velocity and direction

#No input/ouput registers
MoveBall:
	la $t0, BallData	#$t0 = address of ball data table
	
	#Store current position as old position
	lw $t1, 0($t0)		#Current X
	lw $t2, 4($t0)		#Current Y
	sw $t1, 8($t0)		#Store as old X
	sw $t2, 12($t0)		#Store as old Y
	
	#Update X position
	lw $t3, 20($t0)		#X speed
	lw $t4, 24($t0)		#X direction (-1 or 1)
	mulu $t5, $t3, $t4	#$t5 = Speed * direction
	addu $t1, $t1, $t5	#New X = old X + (speed * direction)
	sw $t1, 0($t0)		#Store new X
	
	#Update Y position
	lw $t3, 28($t0)		#Y speed (add this to BallData)
	lw $t4, 32($t0)		#Y direction (add this to BallData)
	mulu $t5, $t3, $t4	#$t5 = Speed * direction
	addu $t2, $t2, $t5	#New Y = old Y + (speed * direction)
	sw $t2, 4($t0)		#Store new Y
	
	jr $ra

#Procedure: ResetBallParameters

#Reset ball position, X speed, and Y direction

#No input/ouput registers
ResetBallParameters:
	lw $t0, OriginalBallPosition + 0
	sw $t0, BallData + 0			#Restore Ball current X

	lw $t0, OriginalBallPosition + 4
	sw $t0, BallData + 4			#Restore Ball current Y
	
	lbu $t0, BallXMinSpeed + 0
	sw  $t0, BallData + 20			#Restore Ball Min X speed
	
	lw $t0, BallYDirection + 0
	sw  $t0, BallData + 32			#Restore Ball Y direction

	jr $ra					#Return to caller

#Brick related functions (logic and drawing)------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Procedure: CheckBrickCollision
#Check collision with all bricks and handle it - ONLY ONE COLLISION PER FRAME
#Optimized with Spatial Hashing

#No input/ouput registers
CheckBrickCollision:
    	subi $sp, $sp, 32
    	sw $ra, 28($sp)
    	sw $s0, 24($sp)   #For Row index
    	sw $s1, 20($sp)   #For Column index
    	sw $s2, 16($sp)   #For Brick index
    	sw $s3, 12($sp)   #For BallData address
    	sw $s4, 8($sp)    #For BricksLayout address
    	sw $s5, 4($sp)    #For Brick X position
    	sw $s6, 0($sp)    #For Brick Y position

    	la $s3, BallData
    	la $s4, BricksLayout

    	#Ball position
    	lw $t0, 0($s3)     #Current Ball Top-Left X = Ball X
    	lw $t1, 4($s3)     #Current Ball Top-Left Y = Ball Y

    	#Brick layout origin
    	lw $t2, 0($s4)     #Grid Top-Left X = Grid X
    	lw $t3, 4($s4)     #Grid Top-Right Y = Grid Y

    
    	sub $t4, $t0, $t2	#$t4 = Ball X - Grid X
    	bltz $t4, DoneBrickCollision  #If $t4 < 0, ball is out of bounds. Not possible but safety

    
    	sub $t5, $t1, $t3	#$t5 = Compute Ball Y - Grid Y
    	bltz $t5, DoneBrickCollision #If $t5 < 0, ball is out of bounds. Not possible but safety

	#Get the column index of the column the ball is in
	#The bricks are laid together, and are 10 units wide
	#Column coordinates: 24-34, 34-44, etc.
    	#Column index = (Ball X - Grid X) / Brick Width
    	#Assume ball's (top-left) X coordinate = 42
    	#(Grid X is fixed = 24. Brick Width is fixed = 10)
    	#(42 - 24) / 10 = 18 / 10: Quotient = 1
    	#LO register will contain the quotient = 1 = column index 
    	lbu $t6, BrickDimensions + 0	#$t6 = Brick Width
    	div $t4, $t6			#LO register = column index
    	mflo $s1              		#$s1 = column index
	
	#Row index calculation is similar to column index
	#The bricks in a column are laid together and are 5 units in height
	#Row coordiantes: 10-15, 15-20, 20-25
    	#Row index = (Ball Y - Grid Y) / Brick Height
    	#Assume ball's (top-left) Y coordinate = 21
    	#(Grid Y is fixed = 10. Brick Height is fixed = 5)
    	#(21 - 10) / 5 = 11 / 5: Quotient = 2
    	#LO register will contain the quotient = 2 = row index 
    	lbu $t6, BrickDimensions + 1	#$t6 = Brick Width
    	div $t5, $t6			#LO = row index
   	mflo $s0              		#$s0 = row index

    	#Bounds check
    	#Row index can't be < 0 and > 4 for collision
    	#Column index can't be < 0 and > 8 for collision
    	blt $s0, $zero, DoneBrickCollision
    	bge $s0, 4, DoneBrickCollision
    	blt $s1, $zero, DoneBrickCollision
    	bge $s1, 8, DoneBrickCollision

    	#Compute brick index for the 1D BricksLeft array: index = row index * 8 + column index
    	li $t6, 8
    	mul $s2, $s0, $t6     	#row * 8
    	addu $s2, $s2, $s1     	#$s2 = row * 8 + column

    	#Check if brick is destroyed
    	la $t7, BricksLeft	#$t7 = base address of BricksLeft
    	addu $t7, $t7, $s2	#$t7 = address of brick collided with
    	lbu $t8, 0($t7)		#Retrieve contents at address (0 or 1)
    	bnez $t8, DoneBrickCollision   #Already destroyed if 1. Skip
	
    	#Get the selected brick's position
    	move $a0, $s0		#$a0 = row index
    	move $a1, $s1		#$a1 = column index
    	jal CalculateBrickPosition           #$v0 = Brick X, $v1 = Brick Y

   	move $s5, $v0         	#$s5 = Brick X
    	move $s6, $v1         	#$s6 = Brick Y

    	#Check if the selected brick actually collided, and if yes, what side. 
    	move $a0, $s5		#$a0 = Birck X
    	move $a1, $s6		#$a1 = Brick Y
    	move $a2, $s2		#$a2 = Brick index (in BricksLeft array)
    	jal CheckSingleBrickCollision

    	bne $v0, 1, DoneBrickCollision       #No collision
    	#Else, there is a brick collsion
    	jal PlayBrickDestroyedMusic
	#Update score
	lbu $a0, Score + 0
	addiu $a0, $a0, 1	#Increment score
	sb  $a0, Score + 0

HandleBrickCollisionAndExit:	#Collision Handling
    	#Mark brick as destroyed
    	#la $t0, BricksLeft
    	#addu $t0, $t0, $s2
    	li $t1, 1
    	sb $t1, BricksLeft($s2)

    	#Erase the brick with black box 
    	move $a0, $s5         #$a0 = Birck X
    	move $a1, $s6         #$a1 = Brick Y
    	lbu $a2, BlackIndex
	jal DrawBrick

    	#Determine the side of brick collision
   	move $a0, $s5         #$a0 = Birck X
    	move $a1, $s6         #$a1 = Brick Ys
    	jal DetermineBrickCollisionSide      #$v0 = side (1, 2, 3, or 4)

    	#Reverse direction as needed
    	beq $v0, 1, CollisionTop		#$v0 = 1 = collided with top surface of brick
    	beq $v0, 2, CollisionBottom		#$v0 = 2 = collided with bottom surface of brick
    	beq $v0, 3, CollisionLeft		#$v0 = 3 = collided with left surface of brick
    	beq $v0, 4, CollisionRight		#$v0 = 4 = collided with right surface of brick
    	j DoneBrickCollision			#Safety. Never triggered


CollisionTop:			#Handling logic for when ball hits top of brick
    	lw $t0, 32($s3)			#Get the 9th entry from Ball data table. 9th entry = Y-axis direction
    	blez $t0, DoneBrickCollision  	#If the ball is moving up, don't reverse
    	neg $t0, $t0       		#Only reverse if moving down
    	sw $t0, 32($s3)			#Update direction
    	j DoneBrickCollision

CollisionBottom:		#Handling logic for when ball hits bottom of brick
    	lw $t0, 32($s3)			#Get the 9th entry from Ball data table. 9th entry = Y-axis direction
    	bgez $t0, DoneBrickCollision  	#If the ball is moving down, don't reverse
    	neg $t0, $t0       		#Only reverse if the ball is moving up
    	sw $t0, 32($s3)			#Update direction
    	j DoneBrickCollision

#If the ball collides with either side of a brick, the X direction of the ball is reversed
CollisionLeft:			#Handling logic for when ball hits left of brick
	lw $t0, 24($s3)			#Get the 7th entry from Ball data table. 7th entry = X-axis direction
	#blez $0, DoneBrickCollision  	#If the ball is moving left, don't reverse
	neg $t0, $t0       		#Only reverse if the ball is moving right
    	sw $t0, 24($s3)			#Update direction
    	j DoneBrickCollision		
CollisionRight:			#Handling logic for when ball hits right of brick
    	lw $t0, 24($s3)			#Get the 7th entry from Ball data table. 7th entry = X-axis direction
    	#bgez $0, DoneBrickCollision  	#If the ball is moving right, don't reverse
    	neg $t0, $t0			#Reverse direction
    	sw $t0, 24($s3)			#Update direction
    	j DoneBrickCollision	


DoneBrickCollision:		#Restore all $s registers, stack pointer, and $ra
    	lw $ra, 28($sp)
    	lw $s0, 24($sp)
    	lw $s1, 20($sp)
    	lw $s2, 16($sp)
    	lw $s3, 12($sp)
    	lw $s4, 8($sp)
    	lw $s5, 4($sp)
    	lw $s6, 0($sp)
    	addiu $sp, $sp, 32
    	jr $ra




#Procedure: CheckSingleBrickCollision
#Check if ball collides with a single brick
#Input: $a0 = Brick X
#Input: $a1 = Brick Y
#Input: $a2 = Brick index
#Output: $v0 = 1 if collision, 0 if no collision
CheckSingleBrickCollision:
	#Get ball position and size
	la $a3, BallData	#$a3 = ball data table
	lw $t0, 0($a3)		#Ball X
	lw $t1, 4($a3)		#Ball Y
	lw $t2, 16($a3)		#Ball size
	
	#Get brick dimensions
	la $a3, BrickDimensions
	lbu $t4, 0($a3)		#Brick width
	lbu $t5, 1($a3)		#Brick height
	
	#Check X overlap: 
	
	addu $t6, $a0, $t4			#Brick right = Brick X + Brick width
	bge $t0, $t6, NoCollisionDetected	#Ball left >= Brick right (No collision)
	
	addu $t6, $t0, $t2			#Ball right = Ball X + Ball Size
	ble $t6, $a0, NoCollisionDetected	#Ball right <= brick left (No collision)
	
	
	addu $t6, $a1, $t5			#Brick bottom edge
	bge $t1, $t6, NoCollisionDetected	#Ball top >= brick bottom (No collision)
	
	addu $t6, $t1, $t2			#Ball bottom edge
	ble $t6, $a1, NoCollisionDetected	#Ball bottom <= Brick top (No collision)
	
	#Collision detected
	li $v0, 1
	jr $ra
	
NoCollisionDetected:
	li $v0, 0
	jr $ra



#Procedure: DetermineBrickCollisionSide

#Input: $a0 = brick X 
#Input: $a1 = brick Y
#Output: $v0 = collision side (1=top, 2=bottom, 3=left, 4=right)
DetermineBrickCollisionSide:
	#Get ball position and size
	la $a2, BallData
	lw $t0, 0($a2)		#Ball Top-Left X 
	lw $t1, 4($a2)		#Ball Top-Left Y
	lw $t2, 16($a2)		#Ball size
	
	#Get brick dimensions  
	la $a3, BrickDimensions
	lbu $t3, 0($a3)		#Brick Width
	lbu $t4, 1($a3)		#Brick Height
	
	#Calculate overlap in X direction
	#X overlap = min(Ball right, Brick right) - max(Ball left, Brick left)
	
	addu $t5, $t0, $t2	#Ball right = Ball X + size
	addu $t6, $a0, $t3	#Brick right = Brick X + width
	
	#Find minimum right edge
	blt $t5, $t6, UseBallRight
	move $t5, $t6		#Use brick right as minimum
UseBallRight:
	#Find maximum left edge and calculate X overlap
	bgt $t0, $a0, UseBallLeft
	sub $t5, $t5, $a0	#X overlap = Min right - Brick left
	j CalculateYOverlap
UseBallLeft:
	sub $t5, $t5, $t0	#X overlap = Min right - Ball left
	
CalculateYOverlap:
	#Calculate overlap in Y direction
	#Y overlap = min(Ball bottom, Brick bottom) - max(Ball top, Brick top)
	addu $t6, $t1, $t2	#Ball bottom = Ball Y + size
	addu $t7, $a1, $t4	#Brick bottom = Brick Y + height
	
	#Find minimum bottom edge
	blt $t6, $t7, UseBallBottom
	move $t6, $t7		#Use brick bottom as minimum
UseBallBottom:
	#Find maximum top edge and calculate Y overlap
	bgt $t1, $a1, UseBallTop
	sub $t6, $t6, $a1	#Y overlap = Min bottom - Brick top
	j CompareOverlaps
UseBallTop:
	sub $t6, $t6, $t1	#Y overlap = Min bottom - Ball top
	
CompareOverlaps:
	#The smaller overlap indicates the collision side
	#If X overlap < Y overlap, collision is on left or right side  
	#If Y overlap < X overlap, collision is on top or bottom side
	blt $t5, $t6, HorizontalCollision
	
VerticalCollision:
	#Compare ball center Y to brick center Y to determine top vs bottom
	srl $t7, $t2, 1		#Ball size/2
	addu $t7, $t1, $t7	#Ball center Y
	srl $t8, $t4, 1		#Brick height/2  
	addu $t8, $a1, $t8	#Brick center Y
	#If Ball center Y < Brick center Y, ball hit the top side
	blt $t7, $t8, HitTopSide
	#Else, it hit the bottom side
	li $v0, 2		#Bottom side
	jr $ra
	
HorizontalCollision:
	#Compare ball center X to brick center X to determine left vs right
	srl $t7, $t2, 1		#Ball size/2
	addu $t7, $t0, $t7	#Ball center X
	srl $t8, $t3, 1		#Brick width/2
	addu $t8, $a0, $t8	#Brick center X  
	#If Ball center X < Brick center X, ball hit the left side
	blt $t7, $t8, HitLeftSide
	#Else, the ball hit the right side
	li $v0, 4		#Right side
	jr $ra

HitTopSide:
	li $v0, 1
	jr $ra
	
HitLeftSide:
	li $v0, 3
	jr $ra
	
#Procedure: DrawBricks

#Uses BricksLayout table and DrawBrick routine to draw all the bricks

#No input/ouput registers
DrawBricks:

DrawBricksSetup:
	subi $sp, $sp, 32	#Make space on stack for registers
	#Store $ra and $s registers on stack
	sw $ra, 28($sp)		
	sw $s0, 24($sp)
	sw $s1, 20($sp)
	sw $s2, 16($sp)
	sw $s3, 12($sp)
	sw $s4, 8($sp)
	sw $s5, 4($sp)
	sw $s6, 0($sp)
	
	la $s0, BricksLayout		#$s0 = Base address of BricksLayout table
	
	la $t0, BricksGrid		#$t0 = address of bricks grid
	lbu $s1, 1($t0)			#$s1 = Number of bricks (boxes) in a row = columns
	lbu $s2, 0($t0)			#$s2 = Number of brick rows
	
	li $s3, 0			#$s3 = Column index
	li $s4, 0			#$s4 = Row index
	la $t0, BrickDimensions		#$t0 = address of brick dimensions
	lbu $s5, 0($t0)		#$s5 = Brick Width
	lbu $s6, 1($t0)		#$s6 = Brick height
	
#Draw all rows
DrawBricksRows:

#Draw all columns
DrawBricksColumns:
	#Draw brick requires:
	#X coordinate in $a0
	#Y coordinate in $a1
	#Color in $a2
	
	move $a0, $s4	#$a0 = Row index
	move $a1, $s3	#$a0 = Column index
	
	jal CalculateBrickPosition
	
	move $a0, $v0	#Brick X
	move $a1, $v1	#Brick Y
	
	#Color index = word at (BricksLayoutTable + 8 bytes) - Row index
	lw $a2, 8($s0)	#$a2 = word at (BricksLayoutTable + 8 bytes)
	subu $a2, $a2, $s4	#$a2 = Color index = word at (BricksLayoutTable + 8 bytes) - Row index
	
	jal DrawBrick
	
	addiu $s3, $s3, 1		#$s3 = $s3 + 1
	blt $s3, $s1, DrawBricksColumns	#Continue drawing columns of a row if column index < number of columns in a row
DoneDrawingBricksColumns:
	li $s3, 0		#Reset column index
	
	addiu $s4, $s4, 1		#$s4 = $s4 + 1. Move to next row index
	blt $s4, $s2, DrawBricksRows	#Continue drawing rows if row index < number of brick rows
	
DoneDrawingBricksRows:
	#Restore $ra and $s registers
	lw $ra, 28($sp)		
	lw $s0, 24($sp)
	lw $s1, 20($sp)
	lw $s2, 16($sp)
	lw $s3, 12($sp)
	lw $s4, 8($sp)
	lw $s5, 4($sp)
	lw $s6, 0($sp)
	
	addiu $sp, $sp, 32	#Restore stack pointer
	
	jr $ra

#Procedure: DrawBrick

#Draw a game brick at the specified X, Y coordinate with the specfied color
#The width and height of the brick are stored in memory

#Input: $a0 = x coordinate 
#Input: $a1 = y coordinate 
#Input: $a2 = color number 
#No output registers
DrawBrick:
	addiu $sp, $sp, -24	#Make space on stack for 6 words
	sw $ra, 20($sp)		#Store $ra on stack
	sw $s0, 16($sp)		#Store $s0 on stack
	sw $s1, 12($sp)		#Store $s1 on stack
	sw $a0, 8($sp)		#Store $a0 (X coordinate) on stack
	sw $a2, 0($sp)		#Store $a2 (color number) on stack
	la $t0, BrickDimensions		#$t0 = address of brick dimensions
	lbu $s0, 0($t0)		#$s0 = Brick Width
	lbu $s1, 1($t0)		#$s1 = Brick height
	
BrickLoop:

	sw $a1, 4($sp)		#Store $a1 (Y coordinate) on stack
	move $a3, $s0		#$a3 = Brick width
	
	jal HorzLine

	lw $a0, 8($sp)		#Restore $a0 (X coordinate)
	lw $a1, 4($sp)		#Restore $a1 (Y coordinate)
	lw $a2, 0($sp)		#Restore $a2 (color number)
	
	
	addiu $a1, $a1, 1	#Increment Y coordinate
	subi $s1, $s1, 1	#Decrease remaining brick height
	bne $s1, $0, BrickLoop	#Continue drawing brick if brick heigh left is not 0
	
	lw $ra, 20($sp)		#Restore $ra
	lw $s0, 16($sp)		#Restore $s0
	lw $s1, 12($sp)		#Restore $s1
	addiu $sp, $sp, 24	#Readjust stack pointer
	
	jr $ra			#Return to caller

#Procedure: CalculateBrickPosition
#Calculate the X,Y position of a brick based on row and column
#Input: $a0 = row index
#Input: $a1 = column index
#Output: $v0 = X coordinate,
#Output: $v1 = Y coordinate
CalculateBrickPosition:
	la $t0, BricksLayout
	lw $t1, 0($t0)		#Base X coordinate
	lw $t2, 4($t0)		#Base Y coordinate
	
	#Calculate X: base x + (column * brick width)
	la $t0, BrickDimensions
	lbu $t3, 0($t0)		#Brick width
	mult $a1, $t3		#Column * Brick width
	mflo $t3
	addu $v0, $t1, $t3	#$v0 = base x + (column * Brick width)
	
	#Calculate Y: base y + (row * brick height)
	lbu $t3, 1($t0)		#Brick height
	mult $a0, $t3		#Row * Brick height
	mflo $t3
	addu $v1, $t2, $t3	#$v1 = base y + (row * Brick height)
	jr $ra

#Border related functions (logic and drawing)-----------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Procedure: DrawBorder
#Draw borders (box) for the game screen using Horizontal Line and Vertical Line drawing procedures

#No input/ouput registers
DrawBorder:
	
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	
	#Draw left vertical line
	la $t0, BorderTable
	lw $a0, 0($t0)	#$a0 = X coordinate
	lw $a1, 4($t0)	#$a1 = Y coordinate
	lbu $a2, WhiteIndex	#$a2 = color index of white
	lw $a3, 8($t0)	#$a3 = Length of line
	jal VertLine

	#Draw right vertical line
	la $t0, BorderTable
	lw $a0, 12($t0)	#$a0 = X coordinate
	lw $a1, 16($t0)	#$a1 = Y coordinate
	lbu $a2, WhiteIndex	#$a2 = color index of white
	lw $a3, 20($t0)	#$a3 = Length of line
	jal VertLine
	
	#Draw top horizontal line
	la $t0, BorderTable
	lw $a0, 24($t0)	#$a0 = X coordinate
	lw $a1, 28($t0)	#$a1 = Y coordinate
	lbu $a2, WhiteIndex	#$a2 = color index of white
	lw $a3, 32($t0)	#$a3 = Length of line
	jal HorzLine
	
	lw $ra, 0($sp)
	addiu $sp, $sp, 4
	jr $ra

#Procedure: CheckBorderCollision
#Check if ball collides with any border and handle collision

#No input/ouput registers
CheckBorderCollision:
	subi $sp, $sp, 4
	sw $ra, 0($sp)		#Store $ra
	
	la $t0, BallData	#$t0 = address of ball data table
	lw $t1, 0($t0)		#$t1 = Current X
	lw $t2, 4($t0)		#$t2 = Current Y
	lw $t3, 16($t0)		#$t3 = Ball size
	
	la $t5, BorderTable	#$t5 = border data table address
	
	#Check left border collision
	lbu $t4, 0($t5)
	ble $t1, $t4, CollideLeftBorder
	
	#Check right border collision
	addu $t6, $t1, $t3	#Ball right edge
	lw $t4, 12($t5)
	bge $t6, $t4, CollideRightBorder
	
	#Check top border collision (Y <= 9)
	lw $t4, 28($t5)
	ble $t2, $t4, CollideTopBorder
	
	j NoBorderCollision

CollideLeftBorder:
	#Reverse X direction
	lw $t4, 24($t0)		#Current X direction
	neg $t4, $t4		#Reverse it
	sw $t4, 24($t0)		#Store new X direction
	
	#Move ball away from border
	lw $t4, 0($t5)
	addiu $t4, $t4, 1		
	sw $t4, 0($t0)		#Set Current X to left border X + 1
	jal PlayBorderCollisionMusic
	j NoBorderCollision

CollideRightBorder:
	#Reverse X direction
	lw $t4, 24($t0)		#Current X direction
	neg $t4, $t4		#Reverse it
	sw $t4, 24($t0)		#Store new direction
	
	#Move ball away from border
	lw $t4, 12($t5)		#$t4 = right border X	
	sub $t4, $t4, $t3	#right border X - ball size
	subiu $t4, $t4, 1	#right border X - ball size - 1
	sw $t4, 0($t0)		#Set Current X to right border X - ball size - 1
	jal PlayBorderCollisionMusic
	j NoBorderCollision

CollideTopBorder:
	#Reverse Y direction
	lw $t4, 32($t0)		#Current Y direction
	neg $t4, $t4		#Reverse it
	sw $t4, 32($t0)		#Store new direction
	
	#Move ball away from border
	lw $t4, 28($t5)		#top border Y
	addiu $t4, $t4, 1	#Add to Y coordinate		
	sw $t4, 4($t0)		#Set Y to 10
	jal PlayBorderCollisionMusic
	j NoBorderCollision


NoBorderCollision:
	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra			#Return to caller


#Basic drawing related functions---------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#Procedure: CalcAddress

#Calculate address of X, Y corrdinate
#Bitmap display  = 256x256. Unit Width = 2. Unit Height = 2. Effective resolution = 128x128
#Input: $a0 = X coordinate (0-127)
#Input: $a1 = Y coordinate (0-127)
#Output: $v0 = memory address

CalcAddress:
	#$v0 = base + $a0*4 + $a1*32*4
	li $v0, 0x10040000	#0x10040000 = base address of heap, which is being used for the bitmap display
	sll $a0, $a0, 2		#$a0 = $a0 * 4
	addu $v0, $v0, $a0	#$v0 = $v0 + $a0*4

	sll $a1, $a1, 9		#$a1 = $a1 * 512 = $a1 * 128 * 4
	add  $v0, $v0, $a1	#$v0 = base + $a0*4 + $a1*128*4
	
	jr $ra			#Return to caller


#Procedure: GetColor
#Get color code from ColorTable (logic shared in lecture)
#Input: $a2 = color number (0-7)
#Output: $v1 = actual number to write to the display
GetColor:
	la $t0, ColorTable	#Load color table base address 
	sll $a2, $a2, 2		#$a2 = $a2 * 4 (index * 4 offset required to traverse a word table)
	addu $a2, $t0, $a2	#Address of color = base + offset
	lw $v1, 0($a2)		#Get color from memory
	
	jr $ra			#Return to caller

#Procedure: DrawDot

#Draw a dot at the specified coordinate of the specified color (logic shared in lecture)
#Input: $a0 = x coordinate (0-127)
#Input: $a1 = y coordinate (0-127)
#Input: $a2 = color number

#No ouput registers
DrawDot:
	addiu $sp, $sp, -8	#Make space on stack for 2 words
	sw $ra, 4($sp)		#Store $ra on stack
	sw $a2, 0($sp)		#Store color number on stack
	
	jal CalcAddress		#Jump and link to calculate address
				#$v0 has address of dot
	
	lw $a2, 0($sp)		#Restore $a2. $a2 = color number
	sw $v0, 0($sp)		#Store address of dot on stack
	
	jal GetColor		#Jump and link to procedure to get color from color table
				#$v1 has color
	
	lw $v0, 0($sp)		#Restore $v0
	sw $v1, 0($v0)		#Make dot on display
	
	lw $ra, 4($sp)		#Restore $ra
	
	addiu $sp, $sp, 8	#Readjust $sp
	
	jr $ra			#Return to caller
	
#Procedure: HorzLine

#Draw a horizontal line on the bitmap display (logic shared in lecture)
#Input: $a0 = x coordinate (0-31)
#Input: $a1 = y coordinate (0-31)
#Input: $a2 = color number (0-7)
#Input: $a3 = length of the line (1-32)

#No ouput registers
HorzLine:
#create stack frame / save $ra
	addiu $sp, $sp, -20	#Make space on stack for 5 words
	sw $ra, 16($sp)		#Store $ra on stack
	sw $a1, 12($sp)		#Store $a1 (y coordinate) on stack
	sw $a2, 8($sp)		#Store $a2 (color number) on stack
HorzLoop:
	sw $a0, 4($sp)		#Store $a0 (X coordinate) on stack
	sw $a3, 0($sp)		#Store $a3 (length of line) on stack
	
	jal DrawDot		#Jump and link to draw a dot at X, Y corrdinate
	
	lw $a0, 4($sp)		#Restore $a0 (X coordinate)
	lw $a1, 12($sp)		#Restore $a1 (y coordinate)
	lw $a2, 8($sp)		#Restore $s2 (color number)
	lw $a3, 0($sp)		#Restore $a3 (length of line)
	
	addiu $a0, $a0, 1	#Increase X coordinate
	addiu $a3, $a3, -1	#Decrement number of pixels left to complete the line
	
	bne $a3, $0, HorzLoop	#Continue drawing line if pixels are left
	
	lw $ra, 16($sp)		#Restore $ra
	addiu $sp, $sp, 20	#Readjust stack pointer
	
	jr $ra			#Return to caller


#Procedure: VertLine

#Draw a vertical line on the bitmap display (logic shared in lecture)
#Input: $a0 = x coordinate 
#Input: $a1 = y coordinate 
#Input: $a2 = color number 
#Input: $a3 = length of the line (1-128)

#No ouput registers
VertLine:
	addiu $sp, $sp, -20	#Make space on stack for 5 words
	sw $ra, 16($sp)		#Store $ra on stack
	sw $a0, 12($sp)		#Store $a0 (X coordinate) on stack
	sw $a2, 8($sp)		#Store $a2 (color number) on stack
VertLoop:
	sw $a1, 4($sp)		#Store $a1 (Y coordinate) on stack
	sw $a3, 0($sp)		#Store $a3 (length of line) on stack
	
	jal DrawDot		#Jump and link to draw a dot at X, Y corrdinate
	
	lw $a0, 12($sp)		#Restore $a0 (X coordinate)
	lw $a1, 4($sp)		#Restore $a1 (y coordinate)
	lw $a2, 8($sp)		#Restore $s2 (color number)
	lw $a3, 0($sp)		#Restore $a3 (length of line)
	
	addiu $a1, $a1, 1	#Increase Y coordinate
	addiu $a3, $a3, -1	#Decrement number of pixels left to complete the line
	
	bne $a3, $0, VertLoop	#Continue drawing line if pixels are left
	
	lw $ra, 16($sp)		#Restore $ra
	addiu $sp, $sp, 20	#Readjust stack pointer
	
	jr $ra			#Return to caller

#Procedure: DrawBox 
#Input: $a0 = x coordinate 
#Input: $a1 = y coordinate 
#Input: $a2 = color number 
#Input: $a3 = size of the box (1-128)

#No ouput registers
DrawBox:
	addiu $sp, $sp, -24	#Make space on stack for 6 words
	sw $ra, 20($sp)		#Store $ra on stack
	sw $s0, 16($sp)		#Store $s0 on stack
	move $s0, $a3		#$s0 = $a3 (box size)
BoxLoop:
	sw $a0, 12($sp)		#Store $a0 (X coordinate) on stack
	sw $a1, 8($sp)		#Store $a1 (Y coordinate) on stack
	sw $a2, 4($sp)		#Store $a2 (color number) on stack
	sw $a3, 0($sp)		#Store $a3 (size of box) on stack
	
	jal HorzLine		#Draw a horizontal line
		
	
	lw $a0, 12($sp)		#Restore $a0 (X coordinate)
	lw $a1, 8($sp)		#Restore $a1 (Y coordinate)
	lw $a2, 4($sp)		#Restore $a2 (color number)
	lw $a3, 0($sp)		#Restore $a3 (size of box)
	
	
	addiu $a1, $a1, 1	#Increment Y coordinate
	
	addiu $s0, $s0, -1	#Decrement counter by 1
	bne $s0, $0, BoxLoop	#Continue drawing box if box size left is not 0
	
	lw $ra, 20($sp)		#Restore $ra
	lw $s0, 16($sp)		#Restore $s0
	addiu $sp, $sp, 24	#Readjust stack pointer
	
	jr $ra			#Return to caller

#All Text related functions------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#OutText: display ascii characters on the bit mapped display (Modified to work with a display resolution of 128x128)
#Input: $a0 = horizontal pixel co-ordinate (0-128)
#Input: $a1 = vertical pixel co-ordinate (0-128)
#Input: $a2 = pointer to asciiz text (to be displayed)
OutText:
        addiu   $sp, $sp, -24
        sw      $ra, 20($sp)

        li      $t8, 1          #line number in the digit array (1-12)
_text1:
        la      $t9, 0x10040000 #get the memory start address
        sll     $t0, $a0, 2     #assumes mars was configured as 128 x 128
        addu    $t9, $t9, $t0   #and 1 pixel width, 1 pixel height
        sll     $t0, $a1, 9    #(a0 * 4) + (a1 * 4 * 128)
        addu    $t9, $t9, $t0   #t9 = memory address for this pixel

        move    $t2, $a2        #t2 = pointer to the text string
_text2:
        lb      $t0, 0($t2)     #character to be displayed
        addiu   $t2, $t2, 1     #last character is a null
        beq     $t0, $zero, _text9

        la      $t3, DigitTable #find the character in the table
_text3:
        lb      $t4, 0($t3)     #get an entry from the table
        beq     $t4, $t0, _text4
        beq     $t4, $zero, _text4
        addiu   $t3, $t3, 13    #go to the next entry in the table
        j       _text3
_text4:
        addu    $t3, $t3, $t8   #t8 is the line number
        lb      $t4, 0($t3)     #bit map to be displayed

        sw      $zero, 0($t9)   #first pixel is black
        addiu   $t9, $t9, 4

        li      $t5, 8          #8 bits to go out
_text5:
        la      $t7, Colors
        lw      $t7, 0($t7)     #assume black
        andi    $t6, $t4, 0x80  #mask out the bit (0=black, 1=white)
        beq     $t6, $zero, _text6
        la      $t7, Colors     #else it is white
        lw      $t7, 4($t7)
_text6:
        sw      $t7, 0($t9)     #write the pixel color
        addiu   $t9, $t9, 4     #go to the next memory position
        sll     $t4, $t4, 1     #and line number
        addiu   $t5, $t5, -1    #and decrement down (8,7,...0)
        bne     $t5, $zero, _text5

        sw      $zero, 0($t9)   #last pixel is black
        addiu   $t9, $t9, 4
        j       _text2          #go get another character

_text9:
        addiu   $a1, $a1, 1     #advance to the next line
        addiu   $t8, $t8, 1     #increment the digit array offset (1-12)
        bne     $t8, 13, _text1

        lw      $ra, 20($sp)
        addiu   $sp, $sp, 24
        jr      $ra
        

#Procedure: ShowPressSpaceMessage
#Shows the Press space message

#No input/ouput registers
ShowPressSpaceMessage:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack
	
	li $a0, 8
	li $a1, 106
	la $a2, PressSpaceMsg
	jal OutText		#Show the Press space message
	
	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack	
	jr $ra
	
#Procedure: ClearPressSpaceMessage
#Clear the Press space message

#No input/ouput registers
ClearPressSpaceMessage:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack

	li $a0, 8
	li $a1, 106
	la $a2, EmptyMsg
	jal OutText		#Clear the press space message

	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack	
	jr $ra
	
#Procedure: ShowInstructions
#Show game control instructions

#No input/ouput registers
ShowInstructions:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack

	li $a0, 0
	li $a1, 106
	la $a2, PressAMsg	
	jal OutText		#Show <-A
	
	li $a0, 98
	li $a1, 106
	la $a2, PressDMsg	
	jal OutText		#Show D->

	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra
	
#Procedure: ClearInstructions
#Clear game control instructions

#No input/ouput registers
ClearInstructions:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack

	li $a0, 0
	li $a1, 106
	la $a2, EmptyAAndDMsg	
	jal OutText		#Show <-A
	
	li $a0, 98
	li $a1, 106
	la $a2, EmptyAAndDMsg	
	jal OutText		#Show D->

	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra

#Procedure: ShowGameWonMessage
#Show game won message

#No input/ouput registers
ShowGameWonMessage:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack

	li $a0, 24
	li $a1, 52
	la $a2, GameWonMsg
	jal OutText

	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra
#Procedure: ShowGameLostMessage
#Show game lost message

#No input/ouput registers
ShowGameLostMessage:

	subi $sp, $sp, 4	
	sw $ra, 0($sp)		#Store $ra on stack

	li $a0, 21
	li $a1, 52
	la $a2, GameLostMsg
	jal OutText

	lw $ra, 0($sp)		#Restore $ra
	addiu $sp, $sp, 4	#Restore stack
	jr $ra
	
	
#Procedure: ShowScore
#Display and update the score. Only redraw if old score does not match current score

#No input/ouput registers
ShowScore:
	subi $sp, $sp, 4
	sw $ra, 0($sp)		#Store $ra
	
	lbu $t0, Score + 0	#Current score
	lbu $t1, Score + 1	#Previous score
	beq $t0, $t1, DoneDisplayingScore
	
	sb $t0, Score + 1		#update previous score
	
	li $t1, 10
	div $t0, $t1		#Divide $t0/10
	mflo $t0		#Ten's place digit
	addiu $t0, $t0, 0x30	#Character representation of digit
	mfhi $t1		#Unit place digit
	addiu $t1, $t1, 0x30	#Character representation of digit
	
	sb $t0, ScoreMsg + 0	#Store ten's place character 
	sb $t1, ScoreMsg + 1	#Store unit's place character
	
	li $a0, 107
	li $a1, 10
	la $a2, EmptyScoreMsg
	jal OutText		#Clear score message
	
	li $a0, 107
	li $a1, 10
	la $a2, ScoreMsg
	jal OutText		#Show score message
	
DoneDisplayingScore:		#Restore $ra and stack
		
	lw $ra, 0($sp)		
	addiu $sp, $sp, 4
	jr $ra

#Procedure: ShowLives
#Display and update life count. Only redraw if old life count does not match current life count

#No input/ouput registers
ShowLives:
	subi $sp, $sp, 4
	sw $ra, 0($sp)		#Store $ra
	
	lbu $t0, Lives + 0	#Current life count
	lbu $t1, Lives + 1	#Previous life count
	beq $t0, $t1, DoneDisplayingLives
	
	sb $t0, Lives + 1		#update previous life count
	
	addiu $t0, $t0, 0x30	#Character representation of digit
	
	sb $t0, LivesMsg + 0	#Store character
	
	li $a0, 7
	li $a1, 10
	la $a2, EmptyLivesMsg
	jal OutText		#Clear lives count
	
	li $a0, 7
	li $a1, 10
	la $a2, LivesMsg
	jal OutText		#Show lives left
	
DoneDisplayingLives:		#Restore $ra and stack
		
	lw $ra, 0($sp)		
	addiu $sp, $sp, 4
	jr $ra
	
#Random number related functions----------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Procedure: InitRand

#Initialize seed for random number generator
#We are going to use only one random generator (ID: 0)

#No input/ouput registers
InitRand:
	
	li $v0, 30	#Syscall 30 is used to get system time
	syscall		#Get system time
	
	#$a0 contains the least significant word of the system time
	
	move $a1, $a0	#Move the the least significant word of system time into $a1
	li $a0, 0
			
	li $v0, 40	#Syscall 40 is used to set the seed
	syscall		#Set the seed for the random number generator

	jr $ra			#Go back to the caller
	
#Procedure: GenerateBallXDirection

#Generate a random X-axis direction for the ball and set it in memory

#No input/ouput registers
GenerateBallXDirection:
	li $a0, 0
	li $a1, 2
	li $v0, 42
	syscall		#Generated number -> either 0 or 1

	li $t0, 2
	mul $t0, $t0, $a0	#$t0 = 2 * (0 or 1)
	
	subi $t0, $t0, 1	#2 * (0 or 1) - 1
				#0 - 1 = -1
				#2 - 1 = 1
	#$t0 holds the ball's X axis direction
	sw $t0, BallData + 24	#Update Ball X direction
	
	jr $ra
	
#Audio related functions------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Procedure: PlayBorderCollisionMusic

#Music to play when ball collides with the border

#No input/output registers
PlayBorderCollisionMusic:
	lbu $a0, BorderCollisionPitch
	lw $a1, BorderCollisionDuration
	lbu $a2, Instrument
	lbu $a3, Volume
	
	li $v0, 31
	syscall		#Start playing the note and return (need not wait for whold duration)
	
	jr $ra		#Return

#Procedure: PlayBrickDestroyedMusic

#Music to play when ball destroys a brick

#No input/output registers
PlayBrickDestroyedMusic:
	lbu $a0, BrickDestroyedPitch
	lw $a1, BrickDestroyedDuration
	lbu $a2, Instrument
	lbu $a3, Volume
	
	li $v0, 31
	syscall		#Start playing the note and return (need not wait for whold duration)
	
	jr $ra		#Return

#Procedure: PlayPaddleCollisionMusic

#Music to play when ball collides with the paddle

#No input/output registers
PlayPaddleCollisionMusic:
	lbu $a0, PaddleCollisionPitch
	lw $a1, PaddleCollisionDuration
	lbu $a2, Instrument
	lbu $a3, Volume
	
	li $v0, 31
	syscall		#Start playing the note and return (need not wait for whold duration)
	
	jr $ra		#Return

#Procedure: PlayLosingMusic

#Music to play on losing the game

#No input/output registers
PlayLosingMusic:
	li $t0, 0	#Index for notes and duration array
	la $t1, GameLostPitch
	la $t2, GameLostDuration
	
	lbu $a2, Instrument
	lbu $a3, Volume
	
PlayLosingMusicLoop:
	addu $a0, $t1, $t0	#pitch address + index
	lbu $a0, 0($a0)		#Load the pitch from memory
	
	sll $t3, $t0, 2		#$t3 = index * 4
	addu $a1, $t2, $t3	#duration + index * 4
	lw $a1, 0($a1)		#Load the duration
	
	li $v0, 33
	syscall			#Play note for duration and then return
	
	addiu $t0, $t0, 1	#Increment index
	bltu  $t0, 3, PlayLosingMusicLoop	#There are only 3 notes to play
	  
	jr $ra		#Return
			

#Procedure: PlayWinningMusic

#Music to play on winning the game

#No input/output registers
PlayWinningMusic:
	li $t0, 0	#Index for notes and duration array
	la $t1, GameWonPitch
	la $t2, GameWonDuration
	
	lbu $a2, Instrument
	lbu $a3, Volume
	
PlayWinningMusicLoop:
	addu $a0, $t1, $t0	#pitch address + index
	lbu $a0, 0($a0)		#Load the pitch from memory
	
	sll $t3, $t0, 2		#$t3 = index * 4
	addu $a1, $t2, $t3	#duration + index * 4
	lw $a1, 0($a1)		#Load the duration
	
	li $v0, 33
	syscall			#Play note for duration and then return
	
	addiu $t0, $t0, 1	#Increment index
	bltu  $t0, 5, PlayWinningMusicLoop	#There are only 5 notes to play
	  
	jr $ra		#Return
########################################################################
#Description:
#    Example SPIM exception handler
#    Derived from the default exception handler in the SPIM S20
#    distribution.
#
#History:
#    Dec 2009    J Bacon
	
########################################################################
#Exception handling code.  This must go first!
	
.kdata
	__start_msg_:   .asciiz "  Exception "
	__end_msg_:     .asciiz " occurred and ignored\n"
	
#Messages for each of the 5-bit exception codes
	__exc0_msg:     .asciiz "  [Interrupt] "
	__exc1_msg:     .asciiz "  [TLB]"
	__exc2_msg:     .asciiz "  [TLB]"
	__exc3_msg:     .asciiz "  [TLB]"
	__exc4_msg:     .asciiz "  [Address error in inst/data fetch] "
	__exc5_msg:     .asciiz "  [Address error in store] "
	__exc6_msg:     .asciiz "  [Bad instruction address] "
	__exc7_msg:     .asciiz "  [Bad data address] "
	__exc8_msg:     .asciiz "  [Error in syscall] "
	__exc9_msg:     .asciiz "  [Breakpoint] "
	__exc10_msg:    .asciiz "  [Reserved instruction] "
	__exc11_msg:    .asciiz ""
	__exc12_msg:    .asciiz "  [Arithmetic overflow] "
	__exc13_msg:    .asciiz "  [Trap] "
	__exc14_msg:    .asciiz ""
	__exc15_msg:    .asciiz "  [Floating point] "
	__exc16_msg:    .asciiz ""
	__exc17_msg:    .asciiz ""
	__exc18_msg:    .asciiz "  [Coproc 2]"
	__exc19_msg:    .asciiz ""
	__exc20_msg:    .asciiz ""
	__exc21_msg:    .asciiz ""
	__exc22_msg:    .asciiz "  [MDMX]"
	__exc23_msg:    .asciiz "  [Watch]"
	__exc24_msg:    .asciiz "  [Machine check]"
	__exc25_msg:    .asciiz ""
	__exc26_msg:    .asciiz ""
	__exc27_msg:    .asciiz ""
	__exc28_msg:    .asciiz ""
	__exc29_msg:    .asciiz ""
	__exc30_msg:    .asciiz "  [Cache]"
	__exc31_msg:    .asciiz ""
	
	__level_msg:    .asciiz "Interrupt mask: "
	
	
#########################################################################
#Lookup table of exception messages
	__exc_msg_table:
		.word   __exc0_msg, __exc1_msg, __exc2_msg, __exc3_msg, __exc4_msg
		.word   __exc5_msg, __exc6_msg, __exc7_msg, __exc8_msg, __exc9_msg
		.word   __exc10_msg, __exc11_msg, __exc12_msg, __exc13_msg, __exc14_msg
		.word   __exc15_msg, __exc16_msg, __exc17_msg, __exc18_msg, __exc19_msg
		.word   __exc20_msg, __exc21_msg, __exc22_msg, __exc23_msg, __exc24_msg
		.word   __exc25_msg, __exc26_msg, __exc27_msg, __exc28_msg, __exc29_msg
		.word   __exc30_msg, __exc31_msg
	
#Variables for save/restore of registers used in the handler
	save_v0:    .word   0
	save_a0:    .word   0
	save_at:    .word   0
	save_a1:    .word   0
	save_a2:    .word   0
	save_a3:    .word   0
	save_t0:    .word   0
	save_t1:    .word   0
	save_t2:    .word   0
	save_t3:    .word   0	


#########################################################################
	#This is the exception handler code that the processor runs when
	#an exception occurs. It only prints some information about the
	#exception, but can serve as a model of how to write a handler.
	#
	#Because this code is part of the kernel, it can use $k0 and $k1 without
	#saving and restoring their values.  By convention, they are treated
	#as temporary registers for kernel use.
	#
	#On the MIPS-1 (R2000), the exception handler must be at 0x80000080
	#This address is loaded into the program counter whenever an exception
	#occurs.  For the MIPS32, the address is 0x80000180.
	#Select the appropriate one for the mode in which SPIM is compiled.
	
.ktext  0x80000180
	
	#Save ALL registers modified in this handler, except $k0 and $k1
	#This includes $t* since the user code does not explicitly
	#call this handler.  $sp cannot be trusted, so saving them to
	#the stack is not an option.  This routine is not reentrant (can't
	#be called again while it is running), so we can save registers
	#to static variables.
	sw      $v0, save_v0
	sw      $a0, save_a0
	sw	$a1, save_a1
	sw	$a2, save_a2
	sw	$a3, save_a3
	sw      $t0, save_t0
	sw      $t1, save_t1
	sw	$t2, save_t2
	sw 	$t3, save_t3
	#$at is the temporary register reserved for the assembler.
	#It may be modified by pseudo-instructions in this handler.
	#Since an interrupt could have occurred during a pseudo
	#instruction in user code, $at must be restored to ensure
	#that that pseudo instruction completes correctly.
	.set    noat
	sw      $at, save_at
	.set    at
	#Determine cause of the exception
	mfc0    $k0, $13        #Get cause register from coprocessor 0
	srl     $a0, $k0, 2     #Extract exception code field (bits 2-6)
	andi    $a0, $a0, 0x1f
	
	#Check for program counter issues (exception 6)
	bne     $a0, 6, ok_pc
	nop
	
	mfc0    $a0, $14        #EPC holds PC at moment exception occurred
	andi    $a0, $a0, 0x3   #Is EPC word-aligned (multiple of 4)?
	beqz    $a0, ok_pc
	nop
	
	#Bail out if PC is unaligned
	#Normally you don't want to do syscalls in an exception handler,
	#but this is MARS and not a real computer
	li      $v0, 4
	la      $a0, __exc3_msg
	syscall
	li      $v0, 10
	syscall
	
ok_pc:
	mfc0    $k0, $13
	srl     $a0, $k0, 2     #Extract exception code from $k0 again
	andi    $a0, $a0, 0x1f
	bnez    $a0, non_interrupt  #Code 0 means exception was an interrupt
	nop

	#External interrupt handler
	#Don't skip instruction at EPC since it has not executed.
	#Interrupts occur BEFORE the instruction at PC executes.
	#Other exceptions occur during the execution of the instruction,
	#hence for those increment the return address to avoid
	#re-executing the instruction that caused the exception.
	
	#Keyboard input handling logic - Developed using material from M10
	#Check if there is a character available

	
	li      $k1, 0xffff0000       #$k1 = address of Receiver Control register
	lw      $a0, 0($k1)           #Load receiver control value
	andi    $a0, $a0, 0x0001      #Check ready bit
	beqz    $a0, nochar           #If not ready, jump to nochar
	
	#If control reached here, then there is a character to read
	#We read character
	li  $k1, 0xffff0004       	#$k1 = address of Receiver Data register
	lbu $v0, 0($k1)           	#Load the character and put it in $t1
	
	#Skip processing if game is active
	lbu $k0, GameActive		#$k0 = Game active flag
	beqz $k0, EndKeyboardInterruptCheck	#If game is inactive, consume the character and ignore

	lbu $k0, PaddleFree		#$k0 = paddle free flag
	beqz $k0, CheckSpaceToFree	#If paddle not free, check for space key
	j CheckMovementKeys		#Paddle is already free, check movement keys

CheckSpaceToFree:
	li $t1, 0x20			#ASCII for space
	bne $v0, $t1, EndKeyboardInterruptCheck	#If not space, ignore input
	
	#Free the paddle
	li $t0, 1
	sb $t0, PaddleFree		#Set paddle as free
	j EndKeyboardInterruptCheck

CheckMovementKeys:
	lbu $k0, PaddleFree		#$k0 = paddle free flag
	beqz $k0, EndKeyboardInterruptCheck
	#Check movement keys (only processed if paddle is free)
	li $t1, 0x61			#ASCII for 'a'
	beq $v0, $t1, MovePaddleLeft	#If 'a', move left
	
	li $t1, 0x64			#ASCII for 'd'  
	beq $v0, $t1, MovePaddleRight	#If 'd', move right
	
	j EndKeyboardInterruptCheck	#Ignore other keys

MovePaddleLeft:
	la $t0, PaddleData		#$t0 = Paddle Data table address
	lw $t1, 0($t0)			#Current X coordinate
	lw $t2, 24($t0)			#Speed
	
	#Check left boundary (assume border starts at X=1)
	lw $t3, BorderTable + 0		#Left boundary X
	subu $a0, $t1, $t2		#Calculate new X position
	bleu $a0, $t3, MoveToLeftBorder	#Move to left border if new X <= left boundary
	
	sw $a0, 0($t0)			#Update X coordinate
	j EndKeyboardInterruptCheck

MoveToLeftBorder:
	addiu $t3, $t3, 2		#Move three pixels away from border (factoring a 1 pixel border width)
	sw $t3, 0($t0)			#Update X coordinate to stay within bounds
	j EndKeyboardInterruptCheck

MovePaddleRight:
	la $t0, PaddleData		#$t0 = Paddle Data table address
	lw $t1, 0($t0)			#Current X coordinate  
	lw $t2, 24($t0)			#Speed
	lw $t3, 16($t0)			#Paddle width
	
	lw $a1, BorderTable + 12	#Right boundary X
	addu $a0, $t1, $t2		#Calculate new X position
	addu $a2, $a0, $t3		#Calculate right edge of paddle
	bgeu $a2, $a1, MoveToRightBorder	#Move to right border if paddle would cross it
	
	sw $a0, 0($t0)			#Update X coordinate
	j EndKeyboardInterruptCheck

MoveToRightBorder:
	subu $t3, $a1, $t3		#Calculate max allowed X position (Border X - Paddle width)
	subiu $t3, $t3, 1		#Move one pixel away from border
	sw $t3, 0($t0)			#Update X coordinate to stay within bounds

EndKeyboardInterruptCheck:
	j return


nochar:
	#not a character
	#Print interrupt level
	#Normally you don't want to do syscalls in an exception handler,
	#but this is MARS and not a real computer
	li      $v0, 4          #print_str
	la      $a0, __level_msg
	syscall
	
	li      $v0, 1          #print_int
	mfc0    $k0, $13        #Cause register
	srl     $a0, $k0, 11    #Right-justify interrupt level bits
	syscall
	
	li      $v0, 11         #print_char
	li      $a0, 10         #Line feed
	syscall
		
	j       return

non_interrupt:
	#Print information about exception.
	#Normally you don't want to do syscalls in an exception handler,
	#but this is MARS and not a real computer
	li      $v0, 4          #print_str
	la      $a0, __start_msg_
	syscall
	
	li      $v0, 1          #print_int
	mfc0    $k0, $13        #Extract exception code again
	srl     $a0, $k0, 2
	andi    $a0, $a0, 0x1f
	syscall
	
	#Print message corresponding to exception code
	#Exception code is already shifted 2 bits from the far right
	#of the cause register, so it conveniently extracts out as
	#a multiple of 4, which is perfect for an array of 4-byte
	#string addresses.
	#Normally you don't want to do syscalls in an exception handler,
	#but this is MARS and not a real computer
	li      $v0, 4          #print_str
	mfc0    $k0, $13        #Extract exception code without shifting
	andi    $a0, $k0, 0x7c
	lw      $a0, __exc_msg_table($a0)
	nop
	syscall
	
	li      $v0, 4          #print_str
	la      $a0, __end_msg_
	syscall
	
	#Return from (non-interrupt) exception. Skip offending instruction
	#at EPC to avoid infinite loop.
	mfc0    $k0, $14
	addiu   $k0, $k0, 4
	mtc0    $k0, $14

return:
	#Restore registers and reset processor state
	lw      $v0, save_v0
	lw      $a0, save_a0
	lw      $a1, save_a1
	lw      $a2, save_a2
	lw      $a3, save_a3
	lw      $t0, save_t0
	lw      $t1, save_t1
	lw      $t2, save_t2
	lw      $t3, save_t3	
	.set    noat	#Prevent assembler from modifying $at
	lw      $at, save_at
	.set    at

	mtc0    $zero, $13      #Clear Cause register
	
	#Re-enable interrupts, which were automatically disabled
	#when the exception occurred, using read-modify-write cycle.
	mfc0    $k0, $12        #Read status register
	andi    $k0, 0xfffd     #Clear exception level bit
	ori     $k0, 0x0001     #Set interrupt enable bit
	mtc0    $k0, $12        #Write back
		
	#Return from exception on MIPS32:
	eret
	
	
#########################################################################
#Standard startup code.  Invoke the routine "main" with arguments:
#main(argc, argv, envp)
	
	.text
.globl __start
__start:
	lw      $a0, 0($sp)     #argc = *$sp
	addiu   $a1, $sp, 4     #argv = $sp + 4
	addiu   $a2, $sp, 8     #envp = $sp + 8
	sll     $v0, $a0, 2     #envp += size of argv array
	addu    $a2, $a2, $v0
	jal     main
	nop
	
	li      $v0, 10         #exit
	syscall
	
.globl __eoth
__eoth:
