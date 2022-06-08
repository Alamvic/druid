Class {
	#name : #DRSpecialisedCompilerTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSpecialisedCompilerTest >> compile: selector [

	(self newCompiler: selector) compile
]

{ #category : #tests }
DRSpecialisedCompilerTest >> generateDruidIRFor: selector [

	| method compiler cfg |
	method := DruidTestInterpreter lookupSelector: selector.
	compiler := self newCompiler: selector.
	cfg := compiler generateDruidIRFor: method.
	compiler optimize: cfg.

	^ cfg
]

{ #category : #tests }
DRSpecialisedCompilerTest >> newCompiler: selector [

	^ DRSpecialisedPrimitiveCompilerCompiler new
		interpreter: self interpreter;
		primitiveName: selector;
		primitiveCompilerName: #gen_ , selector;
		compilerClass: DruidTestRTLCompiler;
		yourself
]

{ #category : #tests }
DRSpecialisedCompilerTest >> testSpecialisedCompilerAddsPreambleToGeneratedCode [

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
DRSpecialisedCompilerTest >> testSpecialisedCompilerRemoveIRChecks [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAdd.

	self assert: cfg allConditionalJumps size equals: 2.
	self
		assert: cfg allConditionalJumps first condition operand1 name
		equals: 'Arg0Reg'.
	self assert: cfg allConditionalJumps second condition isAdd
]
