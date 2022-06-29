Class {
	#name : #DRCustomisedCompilerTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCustomisedCompilerTest >> compile: selector [

	(self newCompiler: selector) compile
]

{ #category : #tests }
DRCustomisedCompilerTest >> generateDruidIRFor: selector [

	| method compiler cfg |
	method := DruidTestInterpreter lookupSelector: selector.
	compiler := self newCompiler: selector.
	compiler customiseFor: method.
	cfg := compiler generateDruidIRFor: method.
	compiler optimize: cfg.

	^ cfg
]

{ #category : #tests }
DRCustomisedCompilerTest >> newCompiler: selector [

	^ DRPrimitiveCompilerCompiler new
		interpreter: self interpreter;
		primitiveName: selector;
		primitiveCompilerName: #gen_ , selector;
		compilerClass: DruidTestRTLCompiler;
		yourself
]

{ #category : #tests }
DRCustomisedCompilerTest >> testAddsPreambleToGeneratedCode [

	| genMethod preambleCode |
	self compile: #primitiveAdd.

	genMethod := DruidTestRTLCompiler lookupSelector: #gen_primitiveAdd.
	preambleCode := genMethod ast body statements first formattedCode.

	self
		assert: preambleCode
		equals:
		'self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ]'
]

{ #category : #tests }
DRCustomisedCompilerTest >> testRemoveIRChecks [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAdd.

	self assert: cfg allConditionalJumps size equals: 2.
	self
		assert: cfg allConditionalJumps first operand1 name
		equals: 'Arg0Reg'.
	self assert: cfg allConditionalJumps second operand1 isAdd
]
