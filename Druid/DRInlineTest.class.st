Class {
	#name : #DRInlineTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRInlineTest >> setUp [

	super setUp.
	self configureInterpreter.
	optimisation := DRInline new
]

{ #category : #tests }
DRInlineTest >> testInlineSend [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveDoubleDeferredInline.

	cfg applyOptimisation: optimisation.

	self assert: cfg messageSends isEmpty.
	self assert: (cfg instructions anySatisfy: [ :i | i isMultiply ]) "Inlined operation"
]

{ #category : #tests }
DRInlineTest >> testNotInlinedSend [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveDoubleDeferredInline.

	self assert: cfg messageSends size equals: 1
]
