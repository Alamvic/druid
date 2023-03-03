Class {
	#name : #DRArithmeticInstruction,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #types }
DRArithmeticInstruction >> type [

	^ self typeWithAlreadySeen: Set new
]

{ #category : #type }
DRArithmeticInstruction >> typeWithAlreadySeen: aCollection [ 
	
	(aCollection includes: self)
		ifTrue: [ ^ DRUndefinedType new ].
	
	aCollection add: self.
	self operands do: [ :e | | operandType |
		operandType := e typeWithAlreadySeen: aCollection.
		"If nil it means it's a recursive type, we continue with the next operand"
		operandType isUndefined ifFalse: [ ^ operandType ] ].
	
	self error: 'Cannot compute instruction type'
]
