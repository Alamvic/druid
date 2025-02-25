Class {
	#name : 'DRFrameInfoCleaner',
	#superclass : 'DROptimisation',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRFrameInfoCleaner >> applyTo: cfg [

	cfg instructionsDo: [ :i |
		i isLoadArgument ifTrue: [ i hasUsers ifFalse: [ i removeFromCFG ] ].
		i isFrameReturn ifTrue: [ i replaceBy: i asCopy ] ]
]
