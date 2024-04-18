Class {
	#name : #DRLoadStackValue,
	#superclass : #DRStackInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadStackValue >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadStackValue: self
]

{ #category : #testing }
DRLoadStackValue >> isLoadStackValue [

	^ true
]

{ #category : #printing }
DRLoadStackValue >> opcode [
	
	^ 'Load-stack-value'
]

{ #category : #SCCP }
DRLoadStackValue >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : #'as yet unclassified' }
DRLoadStackValue >> shiftStackAccessBy: anInteger [ 

	self assert: operands first isConstant.
	operands := OrderedCollection with: (operands first value + anInteger) asDRValue.
	super shiftStackAccessBy: anInteger
]

{ #category : #testing }
DRLoadStackValue >> stackDepth [
	
	"I don't change the stack depth, so just return the stack depth of my stack dependant"
	stackDependency ifEmpty: [ ^ 0 ].
	^ stackDependency anyOne stackDepth
]
