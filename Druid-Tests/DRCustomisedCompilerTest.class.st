Class {
	#name : #DRCustomisedCompilerTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'compiler'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCustomisedCompilerTest >> compile: selector [

	self configureCompiler: selector.
	compiler compile
]

{ #category : #tests }
DRCustomisedCompilerTest >> configureCompiler: selector [

	compiler
		primitiveName: selector;
		primitiveCompilerName: #gen_ , selector
]

{ #category : #tests }
DRCustomisedCompilerTest >> generateDruidIRFor: selector [

	| method cfg |
	method := DruidTestInterpreter lookupSelector: selector.
	compiler customiseFor: method.
	cfg := compiler generateDruidIRFor: method.
	compiler optimize: cfg.

	^ cfg
]

{ #category : #tests }
DRCustomisedCompilerTest >> newBytecodeCompiler [

	^ self newCompiler: DRBytecodeCompilerCompiler
]

{ #category : #tests }
DRCustomisedCompilerTest >> newCompiler: compilerClass [

	^ compilerClass new
		interpreter: self interpreter;
		compilerClass: DruidTestRTLCompiler;
		yourself
]

{ #category : #tests }
DRCustomisedCompilerTest >> newPrimitiveCompiler [

	^ self newCompiler: DRPrimitiveCompilerCompiler
]

{ #category : #tests }
DRCustomisedCompilerTest >> setUp [

	super setUp.
	compiler := self newPrimitiveCompiler
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
DRCustomisedCompilerTest >> testIsMappedShouldAnnotateBytecode [

	| cfg selector |
	selector := #extUnconditionalJump.
	compiler := self newBytecodeCompiler.
	compiler
		sourceName: selector;
		targetName: #gen_ , selector.
	cfg := self generateDruidIRFor: selector.

	self assert: cfg hasAnnotatedBytecode
]

{ #category : #tests }
DRCustomisedCompilerTest >> testRemoveIRChecks [

	| cfg selector |
	selector := #primitiveAdd.
	self configureCompiler: selector.
	cfg := self generateDruidIRFor: selector.

	self assert: cfg allConditionalJumps size equals: 2.
	self
		assert: cfg allConditionalJumps first operand1 name
		equals: 'Arg0Reg'.
	self assert: cfg allConditionalJumps second operand1 isAdd
]
