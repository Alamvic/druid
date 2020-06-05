Class {
	#name : #DRIRBuilder,
	#superclass : #Object,
	#instVars : [
		'instructions',
		'nextLogicalRegisterIndex',
		'registerMapping',
		'isa',
		'currentBasicBlock',
		'basicBlocks',
		'currentNode'
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
	
	anInstruction astNode: currentNode.
	currentBasicBlock appendInstruction: anInstruction.
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
	
	reversedInstructions := self instructions reverse.
	newInstructions := OrderedCollection new.
	
	reversedInstructions do: [ :anInstruction | 
		self assignPhysicalRegistersTo: anInstruction into: newInstructions ].
	
	^ newInstructions
]

{ #category : #'register allocation' }
DRIRBuilder >> assignPhysicalRegistersTo: anInstruction into: newInstructions [ 

	| mappings |
	mappings := self assignPhysicalRegisterToLogical: anInstruction logicalRegisters.
	anInstruction updateLogicalRegisters: mappings.
	
	newInstructions addFirst: anInstruction
]

{ #category : #building }
DRIRBuilder >> bitAnd: aValue with: anotherValue [ 

	^ self appendInstruction: (DRBitAnd new
		leftOperand: aValue;
		rightOperand: anotherValue;
		result: self newLogicalRegister;
		yourself)	
]

{ #category : #building }
DRIRBuilder >> bitShift: aValue magnitude: anotherValue [

	^ self
		appendInstruction:
			(DRBitShiftLeft new
				leftOperand: aValue;
				rightOperand: anotherValue;
				result: self newLogicalRegister;
				yourself)
]

{ #category : #building }
DRIRBuilder >> bitShiftRight: aValue magnitude: anotherValue [

	^ self
		appendInstruction:
			(DRBitShiftRight new
				leftOperand: aValue;
				rightOperand: anotherValue;
				result: self newLogicalRegister;
				yourself)
]

{ #category : #building }
DRIRBuilder >> break [
	"Append an instruction that breaks, for debugging"
	^ self appendInstruction: DRBreak new
]

{ #category : #building }
DRIRBuilder >> buildNewBlockWith: aBlockClosure [ 
	
	| newBasicBlock |
	newBasicBlock := self pushNewBasicBlock.
	aBlockClosure value.
	^ newBasicBlock
]

{ #category : #building }
DRIRBuilder >> compare: aValue with: anotherValue [ 

	^ self
		appendInstruction:
			(DRCompare new
				leftOperand: aValue;
				rightOperand: anotherValue;
				result: self newLogicalRegister;
				yourself)
]

{ #category : #building }
DRIRBuilder >> conditionalBranch [
	
	^ self appendInstruction: DRConditionalBranch new
]

{ #category : #debugging }
DRIRBuilder >> currentNode: anASTNode [

	"We store the current AST node.
	We will bind all instructions generated to the current node, for debugging purposes"
	
	currentNode := anASTNode
]

{ #category : #building }
DRIRBuilder >> greaterOrEquals: aValue than: anotherValue [ 

	^ self
		appendInstruction:
			(DRCompare new
				leftOperand: aValue;
				rightOperand: anotherValue;
				result: self newLogicalRegister;
				yourself)
]

{ #category : #initialization }
DRIRBuilder >> initialize [

	super initialize.
	nextLogicalRegisterIndex := 1.
	isa := #'X64'.
	
	"Start with a basic block"
	basicBlocks := OrderedCollection new.
	self pushNewBasicBlock.
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

	^ basicBlocks gather: [ :e | e instructions ]
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
DRIRBuilder >> jump [

	^ self appendInstruction: DRJump new
]

{ #category : #building }
DRIRBuilder >> jumpFalse [

	^ self appendInstruction: DRJumpFalse new
]

{ #category : #building }
DRIRBuilder >> label [
	
	^ self appendInstruction: DRLabel new
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
DRIRBuilder >> newPhiFunctionWithValues: aCollection [ 
	
	^ DRPhi new
		values: aCollection;
		result: self newLogicalRegister;
		yourself
]

{ #category : #building }
DRIRBuilder >> pop [

	^ self appendInstruction: (DRPop new result: self newLogicalRegister)
]

{ #category : #building }
DRIRBuilder >> push: aValue [
	
	^ self appendInstruction: (DRPush value: aValue).
]

{ #category : #building }
DRIRBuilder >> pushNewBasicBlock [
	
	currentBasicBlock := DRBasicBlock new.
	basicBlocks add: currentBasicBlock.
	^ currentBasicBlock
]

{ #category : #building }
DRIRBuilder >> send: aSelector to: aReceiver arguments: aCollection [ 
	
	^ self appendInstruction:
			(DRSend new
				selector: aSelector;
				receiver: aReceiver;
				arguments: aCollection;
				result: self newLogicalRegister)
]

{ #category : #building }
DRIRBuilder >> storeValue: origin into: destination [

	^ self
		appendInstruction:
			(DRStore new
				origin: origin;
				destination: destination;
				result: self newLogicalRegister)
]

{ #category : #building }
DRIRBuilder >> substract: aValue and: anotherValue [

	^ self
		appendInstruction:
			(DRSubstract new
				leftOperand: aValue;
				rightOperand: anotherValue;
				result: self newLogicalRegister;
				yourself)
]
