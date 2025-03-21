Class {
	#name : 'DRLoadStackValue',
	#superclass : 'DRStackInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadStackValue >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadStackValue: self
]

{ #category : 'visiting' }
DRLoadStackValue >> definingStackInstruction [

	^ self stackSlotDefiningAtDepth: operands first value ifNone: [ nil ]
]

{ #category : 'testing' }
DRLoadStackValue >> isLoadStackValue [

	^ true
]

{ #category : 'printing' }
DRLoadStackValue >> opcode [
	
	^ 'Load-stack-value'
]

{ #category : 'SCCP' }
DRLoadStackValue >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : 'as yet unclassified' }
DRLoadStackValue >> shiftStackAccessBy: anInteger [ 

	self assert: operands first isConstant.
	operands := OrderedCollection with: (operands first value + anInteger) asDRValue.
	super shiftStackAccessBy: anInteger
]

{ #category : 'accessing' }
DRLoadStackValue >> stackDelta [
	
	^ 0
]

{ #category : 'testing' }
DRLoadStackValue >> stackDepth [
	
	"I don't change the stack depth, so just return the stack depth of my stack dependant"
	stackDependencies ifEmpty: [ ^ 0 ].
	^ stackDependencies anyOne stackDepth
]
