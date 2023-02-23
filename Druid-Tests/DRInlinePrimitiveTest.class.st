Class {
	#name : #DRInlinePrimitiveTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRInlinePrimitiveTest >> setUp [

	super setUp.
	self configureInterpreter.
	compilerCompiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new.
	optimisation := DRInline new
]

{ #category : #tests }
DRInlinePrimitiveTest >> testInlinePreservePrimitiveFailCode [

	| cfg send failureCodeUser loadFailureCode storeFailureCode |
	cfg := self generateDruidIRFor: #primitiveFailDeferredInline.

	send := cfg messageSends first.
	storeFailureCode := send previousInstruction.
	loadFailureCode := send nextInstruction.
	
	failureCodeUser := loadFailureCode users anyOne.
	
	"primitive failure code access"
	self assert: storeFailureCode isStore.
	self assert: loadFailureCode isLoad.
	self assert: failureCodeUser isPhiFunction.

	cfg applyOptimisation: optimisation.

	self assert: (cfg instructions count: [ :i | i isLoad]) equals: 2.
	self assert: (cfg instructions count: [ :i | i isStore]) equals: 2.
]

{ #category : #tests }
DRInlinePrimitiveTest >> testInlinePreservePrimitiveFailCodeManyLevels [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAssertDeferredInline.

	cfg applyOptimisation: optimisation.
	cfg applyOptimisation: optimisation.

	self assert: cfg messageSends isEmpty.
	
	self
		assert: (cfg instructions count: [ :i | i isLoad ])
		equals: (cfg instructions count: [ :i | i isStore ])
]

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
