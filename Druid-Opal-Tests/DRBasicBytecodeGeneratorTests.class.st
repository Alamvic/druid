Class {
	#name : 'DRBasicBytecodeGeneratorTests',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test001_emptyMethodReturnsSelf [

	| newMethod receiver result |
	newMethod := self compileSelector: #empty fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result identicalTo: receiver
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test002_returnLiteralInteger [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnLiteralInteger fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 1
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test003_returnArgument [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnArgument: fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver arguments: {17}.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test004_returnUninitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnUninitializedTemp fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: nil
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test005_returnInitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInitializedTemp fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test006_returnTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnTempAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test007_returnNestedTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedTempAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test008_returnInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariable fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	receiver dummyVariable: 178.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 178
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test009_setInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #setInstanceVariable fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: receiver dummyVariable equals: 179
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test010_returnInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariableAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test011_returnNestedInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedInstanceVariableAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17.
	self assert: receiver fillMask equals: 17.
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test012_sendMessage [

	| newMethod |
	newMethod := self compileSelector: #sendMessage fromClass: DROpalExamples.
	
	newMethod valueWithReceiver: self.
	
	self assert: counter equals: 1
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test013_returnIfTrueIfFalseValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueIfFalseValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test014_returnIfTrueValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: nil
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test015_returnIfTrueNonLocalReturn [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueNonLocalReturn: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test016_storeIfTrueIfFalseValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrueIfFalseValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test017_storeIfTrueValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrueValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: nil
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test018_storeInstVarIfTrueIfFalseValue [

	| newMethod instance |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeInstVarIfTrueIfFalseValue: fromClass: DROpalExamples.

	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { true }.
	self assert: instance dummyVariable equals: 5.
	
	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { false }.
	self assert: instance dummyVariable equals: 42.
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test019_storeInstVarIfTrueValue [

	| newMethod instance |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeInstVarIfTrueValue: fromClass: DROpalExamples.

	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { true }.
	self assert: instance dummyVariable equals: 5.

	newMethod valueWithReceiver: instance arguments: { false }.
	self assert: instance dummyVariable equals: nil.
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test020_storeIfTrue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test021_storeWhileTrue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeWhileTrue fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self) equals: 5
]

{ #category : 'tests' }
DRBasicBytecodeGeneratorTests >> test022_storeToDo [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeToDo fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self) equals: 5
]
