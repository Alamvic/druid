Class {
	#name : #DRInstruction,
	#superclass : #Object,
	#category : #'Druid-instructions'
}

{ #category : #'as yet unclassified' }
DRInstruction >> appendOn: anInstructionBuilder [

	self operands do: [ :e | e appendOn: anInstructionBuilder ].
	anInstructionBuilder appendInstruction: self.
]

{ #category : #testing }
DRInstruction >> isConstantInteger [
	
	^ false
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
		
	aStream nextPutAll: self result
]
