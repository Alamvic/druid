Class {
	#name : #DRFrameInfoCleaner,
	#superclass : #DROptimisation,
	#category : #'Druid-Tests'
}

{ #category : #accessing }
DRFrameInfoCleaner >> applyTo: cfg [

	cfg instructionsDo: [ :i |
		i isLoadArgument ifTrue: [ i hasUsers ifFalse: [ i removeFromCFG ] ].
		i isFrameReturn ifTrue: [
			i replaceUsesBy: i operand1.
			i removeFromCFG ] ]
]
