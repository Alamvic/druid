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

	^ self operand1 asJitCompileTimeExpression
]

{ #category : #printing }
DRCopy >> isCopy [ 

	^true 
]

{ #category : #testing }
DRCopy >> isJITCompileTimeExpression [

	^ operands first isJITCompileTimeExpression
]

{ #category : #testing }
DRCopy >> isJITCompileTimeExpressionIgnoring: aCollection [

	(aCollection includes: self operand) ifTrue: [ ^ true ].

	^ self operand isJITCompileTimeExpressionIgnoring: aCollection
]

{ #category : #SCCP }
DRCopy >> isMandatoryInstruction [ 

	self result isPhysicalRegister ifTrue: [ ^ true ].
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
	
	^ result name, ' := ', operands first name asString
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
		ifTrue: [ ^ nil ].
	
	aCollection add: self.
	^ operands first typeWithAlreadySeen: aCollection
]
