Class {
	#name : #DRInstruction,
	#superclass : #Object,
	#instVars : [
		'result',
		'node'
	],
	#category : #'Druid-Instructions'
}

{ #category : #'as yet unclassified' }
DRInstruction >> appendOn: anInstructionBuilder [

	self operands do: [ :e | e appendOn: anInstructionBuilder ].
	anInstructionBuilder appendInstruction: self.
]

{ #category : #accessing }
DRInstruction >> astNode: anASTNode [

	node := anASTNode
]

{ #category : #testing }
DRInstruction >> isConstantInteger [
	
	^ false
]

{ #category : #accessing }
DRInstruction >> logicalRegisters [
	
	^ self registers reject: [ :e | e isPhysical ]
]

{ #category : #printing }
DRInstruction >> mnemonic [

	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
DRInstruction >> operands [
		
	self subclassResponsibility
]

{ #category : #printing }
DRInstruction >> printOn: aStream [

	aStream
		nextPutAll: self mnemonic;
		space.
	
	self operands
		do: [ :operand |
			aStream nextPutAll: operand result asString.
			aStream space ].
	
	self result 
		ifNotNil: [ :aResult | 
			aStream
			 	nextPutAll: ' -> ';
				nextPutAll: aResult printString ].
]

{ #category : #printing }
DRInstruction >> result [ 

	^ result
]

{ #category : #printing }
DRInstruction >> result: aLogicalRegister [

	result := aLogicalRegister
]
