Class {
	#name : 'DRCustomizedBytecodedGeneratorTest',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'running' }
DRCustomizedBytecodedGeneratorTest >> setUp [
	super setUp.
	
	self skip "return to this when doing inlining"
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationAllSatisfy [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #allSatisfy:.
	method := Array lookupSelector: selector.
	
	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.
	
	result := method valueWithReceiver: { 1. 2 } arguments: { [ :e | e even ] }.
	self assert: result equals: false.

	result := method valueWithReceiver: { 2. 4 } arguments: { [ :e | e even ] }.
	self assert: result equals: true.

"
	{ Array } do: [ :e |
		e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
		e addSelectorSilently: selector withMethod: method ]
	"
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationAnySatisfy [

	| method result cfg selector |
	
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #anySatisfy:.
	method := Array lookupSelector: selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.
	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2 } arguments: { [ :e | e even ] }.
	self assert: result equals: true.
	
	result := method valueWithReceiver: { 1 . 3 } arguments: { [ :e | e even ] }.
	self assert: result equals: false.

"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationDifference [

	| method result cfg selector |
	
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #difference:.
	method := Set >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2. 3 } asSet arguments: { { 3 . 4 . 5 } asSet }.

	self assertCollection: result includesAll: { 1. 2 }.


"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationDo [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #do:.
	method := SequenceableCollection >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := OrderedCollection new.
	method valueWithReceiver: { 1. 2. 3 } arguments: { [ :e | result add: e ] }.

	self assertCollection: result includesAll: { 1. 2. 3 }.


"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationSelect [

	| method result cfg selector |
	compilerCompiler irGenerator typeSystem:
		(typeSystem := DRCustomisationTypeSystem new).
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #select:.
	method := Collection >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method
		          valueWithReceiver: { 1. 2. 3 } asOrderedCollection
		          arguments: { [ :e | e even ] }.

	self assertCollection: result includesAll: { 2 }.
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodCustomisationSlowCount [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #slowCount.
	method := Array lookupSelector: selector.
	
	compilerCompiler interpreter: Array new.

	cfg := self generateDruidIRFor: method.

	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.
	
	result := method valueWithReceiver: { 1. 2 } arguments: {}.
	self assert: result equals: 2.

	result := method valueWithReceiver: { 2. 4. 6 } arguments: {}.
	self assert: result equals: 3.
]

{ #category : 'tests' }
DRCustomizedBytecodedGeneratorTest >> testMethodWithTypeCheck [

	| method result |
	
	compilerCompiler configureForCompilerClass: nil.
	method := self generateMethodForSelector: #methodCollectionSize:.

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4
]
