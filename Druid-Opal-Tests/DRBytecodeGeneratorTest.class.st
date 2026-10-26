Class {
	#name : 'DRBytecodeGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator',
		'typeSystem',
		'counter'
	],
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method [

	self assert: compiledMethod equalsResult: method arguments: {  }
]

{ #category : 'asserting' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method arguments: arguments [

	| result expectedResult receiver |
	receiver := compiledMethod methodClass new.

	receiver initialize.
	result := compiledMethod valueWithReceiver: receiver arguments: arguments.

	receiver initialize.
	expectedResult := method valueWithReceiver: receiver arguments: arguments.

	expectedResult isClosure ifTrue: [
		^ self assertClosure: result equals: expectedResult ].

	(expectedResult isCollection and: [
		 expectedResult isNotEmpty and: [ expectedResult anyOne isClosure ] ])
		ifTrue: [
			self assert: result size equals: expectedResult size.
			expectedResult doWithIndex: [ :elem :i |
				self assertClosure: (result at: i) equals: elem ].
			^ self ].

	self assert: result equals: expectedResult
]

{ #category : 'asserting' }
DRBytecodeGeneratorTest >> assertClosure: result equals: expected [

	| blockResult blockExpected |

	self assert: result isClosure.
	self assert: expected isClosure.	
	self assert: result argumentCount equals: expected argumentCount.
	
	"Try some value?"
	blockResult := result valueWithArguments: (Array ofSize: result argumentCount).
	blockExpected := result valueWithArguments: (Array ofSize: result argumentCount).
	self assert: blockResult equals: blockExpected
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass [

	| compiledMethod expectedMethod |
	expectedMethod := aClass >> aSelector.
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass checkByteCode: byteCode [

	| compiledMethod expectedMethod |
	self setUp.
		
	expectedMethod := aClass >> aSelector.
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: (self checkByteCode: byteCode in: compiledMethod ir).
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationNoInlineFor: aSelector fromClass: aClass arguments: args [

	| compiledMethod expectedMethod |
	compilerCompiler configureForCompilerClassWithoutInline: nil.

	expectedMethod := aClass >> aSelector.

	compiledMethod := self compileSelector: aSelector fromClass: aClass.

	self
		assert: compiledMethod
		equalsResult: expectedMethod
		arguments: args
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> checkByteCode: byteCode in: compiledMethodIR [
	"check if the provided bytecode exist in the compiledMethod IR"
	
	^ compiledMethodIR startSequence sequence anySatisfy: [ :each | each printString includesAll: byteCode]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> compileSelector: selector fromClass: aClass [

	| method interpreter cfg |
	method := aClass >> selector.

	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.
	method methodClass: aClass.
	^ method
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> generateDruidIRFor: method [

	| cfg |
	cfg := compilerCompiler generateDruidIRFor: method.
	compilerCompiler optimize: cfg.	
	^ cfg
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> generateDruidIRForSelector: selector [

	^ self generateDruidIRFor: (DROpalExamples lookupSelector: selector)
]

{ #category : 'helpers' }
DRBytecodeGeneratorTest >> generateMethodForSelector: aSelector [

	| cfg method |
	cfg := self generateDruidIRForSelector: aSelector.
	method := self generateMethodFromCFG: cfg withSelector: aSelector.
	^ method
]

{ #category : 'utilities' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector [

	^ DROpalCompiler 
		  generateMethodFromCFG: aDRControlFlowGraph
		  withSelector: selector
		  numArgs: compilerCompiler irGenerator numberOfArguments
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> incrementCounter [

	counter := counter + 1
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> setUp [
	super setUp.
	
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler configureForCompilerClass: nil.
	
	bytecodeGenerator := DRTreegraphBytecodeGenerator new.
	
	counter := 0
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> tearDown [

	| aPackage |
	aPackage := PackageOrganizer default packageNamed: 'DrOpal-Extensions' ifAbsent: nil.
	aPackage ifNotNil: [ aPackage removeFromSystem ].
	
	super tearDown.
]
