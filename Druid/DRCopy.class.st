Class {
	#name : #DRCopy,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRCopy >> acceptVisitor: aVisitor [
	
	^ aVisitor visitCopy: self
]

{ #category : #printing }
DRCopy >> isCopy [ 

	^true 
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

{ #category : #printing }
DRCopy >> opcode [
	
	^ ':='
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
