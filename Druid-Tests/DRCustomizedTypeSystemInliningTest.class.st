Class {
	#name : 'DRCustomizedTypeSystemInliningTest',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
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
