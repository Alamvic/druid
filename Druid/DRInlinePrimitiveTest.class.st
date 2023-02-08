Class {
	#name : #DRInlinePrimitiveTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRInlinePrimitiveTest >> testInlinedSend [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveDoubleDeferredInline.

	cfg applyOptimisation: optimisation.

	self assert: cfg messageSends isEmpty.
	self assert: (cfg instructions anySatisfy: [ :i | i isMultiply ]). "Inlined operation"
	self assert: (cfg instructions count: [ :i | i isLoad ]) equals: 2.
	self assert: (cfg instructions count: [ :i | i isStore ]) equals: 2
]

{ #category : #tests }
DRInlinePrimitiveTest >> testNotInlinedSend [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveDoubleDeferredInline.

	self assert: cfg messageSends size equals: 1.
	self assert: (cfg instructions count: [ :i | i isLoad ]) equals: 1.
	self assert: (cfg instructions count: [ :i | i isStore ]) equals: 1
]
