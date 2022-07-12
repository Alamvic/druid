Class {
	#name : #DRArithmeticInstruction,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #types }
DRArithmeticInstruction >> type [

	"Add is only valid among operands of the same type"

	(self operands collect: [ :e | e type ] as: Set) size = 1 ifFalse: [ 
		self error: 'incompatible types' ].
	
	^ self operands anyOne type
]
