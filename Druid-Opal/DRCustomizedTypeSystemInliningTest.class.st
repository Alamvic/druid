Class {
	#name : 'DRCustomizedTypeSystemInliningTest',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'running' }
DRCustomizedTypeSystemInliningTest >> setUp [
	super setUp.
		
	compilerCompiler := DRMethodCompilerCompiler new.
	typeSystem := DRCustomisationTypeSystem new.
	
	compilerCompiler configureForCompilerClass: nil.
	compilerCompiler irGenerator typeSystem: typeSystem.
	
	bytecodeGenerator := DRBytecodeGenerator new.
	
]

{ #category : 'tests' }
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationAllSatisfy [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationAnySatisfy [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationDifference [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationDo [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationSelect [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodCustomisationSlowCount [

	| method result cfg selector |
	
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
DRCustomizedTypeSystemInliningTest >> testMethodRecompilationWithProfile [

	| method result cfg selector methodProfiler previousMethodSize |
	
	self skip. "This is a proof of concept, does not metter"
	
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	typeSystem := DRProfileBasedTypeSystem reset.
	compilerCompiler configureForCompilerClass: nil.
	selector := #methodCollectionSize:.
	method := self generateMethodForSelector: selector.

	previousMethodSize := (DrOpalExamples >> selector) size.

	self assert: method size > previousMethodSize. "Bigger than original"
	previousMethodSize := method size.

	"First execution"
	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	"Profiled type"
	self assert: typeSystem profileTable size equals: 1.
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast arguments first)
		hasSameElements: { (DRClassType for: OrderedCollection) }.

	"Recompile"
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size > previousMethodSize. "Bigger because of new guard"
	previousMethodSize := method size.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4.

	"New profiled type"
	self assert: typeSystem profileTable size equals: 2. "Old and new methods"
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast temporaries first)
		hasSameElements: { (DRClassType for: Array2D) }.


	"Recompile again"
	compilerCompiler := DRMethodCompilerCompiler new.
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size equals: 144. "bigger then before"

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4.

	"Last profiled type"
	self assert: typeSystem profileTable size equals: 3. "New type from Array2D inline"
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast temporaries second)
		hasSameElements: { (DRClassType for: Array) }
]
