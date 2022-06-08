Class {
	#name : #DRSpecialisedCompilerTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSpecialisedCompilerTest >> compile: selector [

	compilerCompiler
		primitiveName: selector;
		primitiveCompilerName: #gen_ , selector;
		compilerClass: DruidTestRTLCompiler;
		compile
]

{ #category : #initialization }
DRSpecialisedCompilerTest >> setUp [

	super setUp.
	compilerCompiler := DRSpecialisedPrimitiveCompilerCompiler new
		                    interpreter: DruidTestInterpreter new;
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
