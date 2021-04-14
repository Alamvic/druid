Class {
	#name : #DruidCogit,
	#superclass : #Cogit,
	#category : #'Druid-Tests'
}

{ #category : #configuration }
DruidCogit class >> initializeBytecodeTableForSistaV1 [
	
	"Do nothing for now"
]

{ #category : #configuration }
DruidCogit class >> initializePrimitiveTable [
	
	"Do nothing for now"
]

{ #category : #'simulation only' }
DruidCogit >> bytecodeFixupClass [
	<doNotGenerate>
	^CogBytecodeFixup
]

{ #category : #garbage }
DruidCogit >> entryOffset: anInteger [ 
	
	"Nothing"
]

{ #category : #garbage }
DruidCogit >> forCogit: aDruidCogit methodZone: aCogMethodZone [ 
	
	"nothing?"
]

{ #category : #generated }
DruidCogit >> genPrimitiveAdd [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self TstR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 := self JumpNonZero: 0
]

{ #category : #garbage }
DruidCogit >> minCallAddress: anInteger [ 
	
	"Nothing"
]

{ #category : #accessing }
DruidCogit >> objectRepresentationClass [

	^ self
]
