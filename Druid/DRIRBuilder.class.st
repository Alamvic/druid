Class {
	#name : #DRIRBuilder,
	#superclass : #Object,
	#instVars : [
		'instructions',
		'nextLogicalRegisterIndex',
		'registerMapping',
		'isa'
	],
	#category : #'Druid-Instructions'
}

{ #category : #building }
DRIRBuilder >> add: aValue and: anotherValue [

	^ self appendInstruction: (DRAdd new
		leftOperand: aValue;
		rightOperand: anotherValue;
		result: self newLogicalRegister;
		yourself)
]

{ #category : #accessing }
DRIRBuilder >> appendInstruction: anInstruction [ 
	
	instructions add: anInstruction.
	^ anInstruction
]

{ #category : #'register allocation' }
DRIRBuilder >> assignPhysicalRegisterToLogical: aCollectionOfLogicals [

	| mappings aBinding |

	mappings := Dictionary new.

	aCollectionOfLogicals do: [ :aLogicalRegister | 
		
		aBinding := registerMapping associations
			detect: [ :anAssociation | anAssociation value = aLogicalRegister ]
			ifNone: [ registerMapping associations
												detect: [ :anAssociation | anAssociation value isNil ]
												ifNone: [ "We have to Spill "1 halt ] ].
			
		aBinding value: aLogicalRegister.
		mappings at: aLogicalRegister put: aBinding key].

	^ mappings
]

{ #category : #'register allocation' }
DRIRBuilder >> assignPhysicalRegisters [
	
	| reversedInstructions newInstructions |
	self initializeFreeRegistersForIsa: isa.
	
	reversedInstructions := instructions reverse.
	newInstructions := OrderedCollection new.
	
	reversedInstructions do: [ :anInstruction | 
		self assignPhysicalRegistersTo: anInstruction into: newInstructions ].
	
	instructions := newInstructions
]

{ #category : #'register allocation' }
DRIRBuilder >> assignPhysicalRegistersTo: anInstruction into: newInstructions [ 

	| mappings |
	mappings := self assignPhysicalRegisterToLogical: anInstruction logicalRegisters.
	anInstruction updateLogicalRegisters: mappings.
	
	newInstructions addFirst: anInstruction
]

{ #category : #initialization }
DRIRBuilder >> initialize [

	super initialize.
	nextLogicalRegisterIndex := 1.
	instructions := OrderedCollection new.
	isa := #'X64'
]

{ #category : #'register allocation' }
DRIRBuilder >> initializeFreeRegisters: aCollectionOfNames [

	registerMapping := (aCollectionOfNames
		collect: [ :aName | (DRPhysicalRegister named: aName) -> nil ]) asDictionary
		
]

{ #category : #'register allocation' }
DRIRBuilder >> initializeFreeRegistersForIsa: anIsa [
	
	"We cannot use the R8 in SysV and R11 in Windows!!"
	anIsa = #X64
		ifTrue: [ self initializeFreeRegisters: #(rax rbx rcx rdx rsi rdi r12 r9 r10) ]
		ifFalse: [ self initializeFreeRegisters: #(eax ebx ecx edx esi edi) ]
]

{ #category : #accessing }
DRIRBuilder >> instructions [
	^ instructions
]

{ #category : #accessing }
DRIRBuilder >> isa [
	^ isa
]

{ #category : #accessing }
DRIRBuilder >> isa: anObject [
	isa := anObject
]

{ #category : #building }
DRIRBuilder >> loadAddress: aValue [

	^ self
		appendInstruction:
			(DRLoad
				value: (DRIndirection value: aValue)
				result: self newLogicalRegister)
]

{ #category : #building }
DRIRBuilder >> newConstant: aValue [

	^ DRConstant value: aValue
]

{ #category : #'logical registers' }
DRIRBuilder >> newLogicalRegister [

	| aNewRegister |
	
	aNewRegister := DRLogicalRegister named: 'l' , nextLogicalRegisterIndex printString.
		
	nextLogicalRegisterIndex := nextLogicalRegisterIndex + 1.
	
	^ aNewRegister 
]

{ #category : #building }
DRIRBuilder >> push: aValue [
	
	^ self appendInstruction: (DRPush value: aValue).
]
