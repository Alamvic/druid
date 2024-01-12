Class {
	#name : #DRCustomisedCompilerTest,
	#superclass : #DRDruidTest,
	#instVars : [
		'compiler'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCustomisedCompilerTest >> configureCompiler: selector [

	compiler
		sourceName: selector;
		targetName: #gen_ , selector
]

{ #category : #tests }
DRCustomisedCompilerTest >> druidCompile: selector [

	self configureCompiler: selector.
	compiler compile.
	^ compiler targetMethod 
]

{ #category : #tests }
DRCustomisedCompilerTest >> generateDruidIRFor: selector [

	| method cfg |
	self configureCompiler: selector.
	
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
		configureForCompilerClass: DruidTestRTLCompiler;
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
	genMethod := self druidCompile: #primitiveAdd.
	preambleCode := genMethod ast body statements first formattedCode.

	self
		assert: preambleCode
		equals:
		'self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ]'
]

{ #category : #tests }
DRCustomisedCompilerTest >> testIsMappedShouldAnnotateBytecode [

	| cfg |
	compiler := self newBytecodeCompiler.
	cfg := self generateDruidIRFor: #extUnconditionalJump.

	self assert: cfg hasAnnotatedBytecode
]

{ #category : #tests }
DRCustomisedCompilerTest >> testIsMappedWithSendShouldNotAnnotateBytecode [

	| cfg |
	compiler := self newBytecodeCompiler.
	cfg := self generateDruidIRFor: #bytecodePrimAdd.

	self deny: cfg hasAnnotatedBytecode
]

{ #category : #tests }
DRCustomisedCompilerTest >> testRemoveIRChecks [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAdd.

	self assert: cfg allConditionalJumps size equals: 2.
	self
		assert: cfg allConditionalJumps first operand1 name
		equals: 'R2'.
	self assert: cfg allConditionalJumps second operand1 isAdd
]
