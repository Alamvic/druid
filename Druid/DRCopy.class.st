Class {
	#name : #DRCopy,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRCopy >> acceptVisitor: aVisitor [
	
	^ aVisitor visitCopy: self
]

{ #category : #converting }
DRCopy >> asJitCompileTimeExpression [

	^ result name
]

{ #category : #testing }
DRCopy >> isConstanteable [

	^ self operand isConstanteable
]

{ #category : #printing }
DRCopy >> isCopy [ 

	^true 
]

{ #category : #testing }
DRCopy >> isJITCompileTimeExpression [

	^ operands first isConstanteable
]

{ #category : #testing }
DRCopy >> isJITCompileTimeExpressionIgnoring: aCollection [

	(aCollection includes: self operand) ifTrue: [ ^ true ].

	^ self operand isJITCompileTimeExpressionIgnoring: aCollection
]

{ #category : #SCCP }
DRCopy >> isMandatoryInstruction [ 

	self result isPhysicalRegister ifTrue: [ ^ true ].
	self result isStagedRegister ifTrue: [ ^ true ].
	^ false
]

{ #category : #testing }
DRCopy >> isSetReturnValue [
	
	^ self result isPhysicalRegister and:  [ self result name = 'ReceiverResultReg' ]
]

{ #category : #testing }
DRCopy >> jitCompileTimeExpression [

	^ operands first jitCompileTimeExpression
]

{ #category : #printing }
DRCopy >> opcode [
	
	^ ':='
]

{ #category : #accessing }
DRCopy >> operand [

	^ operands unique
]

{ #category : #printing }
DRCopy >> prettyPrint [

	^ (self isStaged
		   ifTrue: [ '[STAGED]' ]
		   ifFalse: [ '' ]) , result prettyPrint , ' := '
	  , operands first result prettyPrint
]

{ #category : #SCCP }
DRCopy >> sccpLatticeValueFor: sccp [

	^ sccp latticeValue: operands first
]

{ #category : #'constant folding' }
DRCopy >> simpleConstantFold [
	
	^ self operand1 simpleConstantFold
]

{ #category : #types }
DRCopy >> type [
	
	^ self operands first type
]

{ #category : #type }
DRCopy >> typeWithAlreadySeen: aCollection [ 
	
	(aCollection includes: self)
		ifTrue: [ ^ DRUndefinedType new ].
	
	aCollection add: self.
	^ operands first typeWithAlreadySeen: aCollection
]
