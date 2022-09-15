Class {
	#name : #DRInlineTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRInlineTest >> setUp [

	super setUp.
	self configureInterpreter.
	compilerCompiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new.
	optimisation := DRInline new
]

{ #category : #tests }
DRInlineTest >> testInlinePreservePrimitiveFailCode [

	| cfg send failureCodeUser loadFailureCode |
	cfg := self generateDruidIRFor: #primitiveFailDeferredInline.

	send := cfg messageSends first.
	loadFailureCode := send nextInstruction.
	failureCodeUser := loadFailureCode users anyOne.
	
	"primitive failure code access"
	self assert: loadFailureCode isLoad.
	self assert: failureCodeUser isPhiFunction.

	cfg applyOptimisation: optimisation.

	self flag: #TODO

]

{ #category : #tests }
DRInlineTest >> testInlinePreservePrimitiveFailCodeManyLevels [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAssertDeferredInline.

	cfg applyOptimisation: optimisation.
	cfg applyOptimisation: optimisation.

	self assert: cfg messageSends isEmpty.
	"TODO: better assert"
	self
		assert: (cfg instructions count: [ :i | i isLoad ])
		equals: (cfg instructions count: [ :i | i isStore ])
]

{ #category : #tests }
DRInlineTest >> testInlinedSend [

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
