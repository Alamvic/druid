Class {
	#name : 'DRCleanInlinedClosures',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRCleanInlinedClosures >> applyTo: cfg [

	cfg instructionsDo: [ :i | i isDRBlockClosure ifTrue: [ i hasUsers ifFalse: [ i removeFromCFG ] ] ]
]
