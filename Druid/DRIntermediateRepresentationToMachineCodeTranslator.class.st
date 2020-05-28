Class {
	#name : #DRIntermediateRepresentationToMachineCodeTranslator,
	#superclass : #Object,
	#instVars : [
		'instructions',
		'memory',
		'cogit'
	],
	#pools : [
		'CogRTLOpcodes'
	],
	#category : #'Druid-Instructions'
}

{ #category : #operations }
DRIntermediateRepresentationToMachineCodeTranslator class >> translate: aCollection withCompiler: cogit [
	
	^ self new 
		cogit: cogit;
		translate: aCollection;
		yourself
]

{ #category : #accessing }
DRIntermediateRepresentationToMachineCodeTranslator >> cogit: aSimpleStackBasedCogit [ 
	cogit := aSimpleStackBasedCogit
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> gen: opcode operand: operandOne [

	^ cogit gen: opcode operand: operandOne 
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> gen: opcode operand: operandOne operand: operandTwo [
	
	^ cogit gen: opcode operand: operandOne operand: operandTwo

]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> gen: opcode operand: operandOne operand: operandTwo operand: operandThree [
	
	^ cogit gen: opcode operand: operandOne operand: operandTwo operand: operandThree

]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genAdd: anInteger toRegister: aRegisterName [ 

	instructions add: (self gen: AddCwR operand: anInteger operand: (self translateRegister: aRegisterName)) 
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genMoveConstant: anInteger toRegister: aRegisterName [ 

	instructions add: (self gen: MoveCwR operand: anInteger operand: (self translateRegister: aRegisterName)) 
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genMoveIndirectRegister: aRegisterName toRegister: anotherRegisterName [ 

	instructions add:
			(self
				gen: MoveMwrR
				operand: 0
				operand: (self translateRegister: aRegisterName)
				operand: (self translateRegister: anotherRegisterName))
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genMoveRegister: aRegisterName toRegister: anotherRegisterName [
	instructions
		add:
			(self
				gen: MoveRR
				operand: (self translateRegister: aRegisterName)
				operand: (self translateRegister: anotherRegisterName))
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genPushConstant: anInteger [ 
	
	instructions add: (self gen: PushCw operand: anInteger)
]

{ #category : #'machine code instructions' }
DRIntermediateRepresentationToMachineCodeTranslator >> genPushRegister: aName [ 
	
	instructions add: (self gen: PushR operand: (self translateRegister: aName))
]

{ #category : #operations }
DRIntermediateRepresentationToMachineCodeTranslator >> generate [
	
	| codeSize allocatedAddress lastAddress |

	codeSize := cogit generateInstructionsAt: cogit methodZone freeStart.
	allocatedAddress := cogit methodZone allocate: codeSize.
	lastAddress := cogit outputInstructionsAt: allocatedAddress.
	
	^ lastAddress
]

{ #category : #operations }
DRIntermediateRepresentationToMachineCodeTranslator >> initialize [

	super initialize.
	instructions := OrderedCollection new.
	cogit := self

]

{ #category : #accessing }
DRIntermediateRepresentationToMachineCodeTranslator >> instructions [
	^ instructions
]

{ #category : #mocking }
DRIntermediateRepresentationToMachineCodeTranslator >> objectMemory [
	^ self
]

{ #category : #operations }
DRIntermediateRepresentationToMachineCodeTranslator >> translate: aCollection [ 

	cogit 
		allocateOpcodes: aCollection size * 2 
		bytecodes: 0 
		ifFail: [ self halt ].
		
	cogit zeroOpcodeIndex.
	cogit methodLabel address: cogit methodZone freeStart.
	
	aCollection do: [ :anIRInstruction | 
		anIRInstruction accept: self ]
]

{ #category : #utils }
DRIntermediateRepresentationToMachineCodeTranslator >> translateRegister: aString [ 
	
	aString = #BasePointer ifTrue: [ ^ 5 ].
	
	^ cogit backend generalPurposeRegisterMap at: aString
]

{ #category : #visiting }
DRIntermediateRepresentationToMachineCodeTranslator >> visitAdd: aDRAdd [

	aDRAdd leftOperandRegister isConstant
		ifTrue: [ 
			self
				genMoveConstant: aDRAdd leftOperandRegister value
				toRegister: aDRAdd result name ]
		ifFalse: [
			self 
				genMoveRegister: aDRAdd leftOperandRegister name 
				toRegister: aDRAdd result name ].
		
	aDRAdd rightOperandRegister isConstant
		ifTrue: [ 
			^ self
				genAdd: aDRAdd rightOperandRegister value
				toRegister: aDRAdd result name ]
		ifFalse: [ self halt ].
]

{ #category : #visiting }
DRIntermediateRepresentationToMachineCodeTranslator >> visitLoad: aDRLoad [
	aDRLoad valueRegister isIndirection
		ifFalse: [ self halt ].
	aDRLoad valueRegister isRegister
		ifFalse: [ self halt ].

	self
		genMoveIndirectRegister: aDRLoad valueRegister value name
		toRegister: aDRLoad result name
]

{ #category : #visiting }
DRIntermediateRepresentationToMachineCodeTranslator >> visitPush: aPush [
	aPush value isConstant
		ifTrue: [ ^ self genPushConstant: aPush value value ].
	
	^ self genPushRegister: aPush value name

]
