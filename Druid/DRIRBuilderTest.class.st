Class {
	#name : #DRIRBuilderTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRIRBuilderTest >> testIRBuilderStartsEmpty [

	self assert: DRIRBuilder new instructions isEmpty
]

{ #category : #tests }
DRIRBuilderTest >> testLoadAddressAppendsCreatedInstruction [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: 17.
	self assert: (builder instructions includes: loadInstruction)
]

{ #category : #tests }
DRIRBuilderTest >> testLoadAddressAppendsInstruction [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: 17.
	self assert: builder instructions size equals: 1
]

{ #category : #tests }
DRIRBuilderTest >> testNewConstantDoesNotAppendInstruction [

	| builder |
	builder := DRIRBuilder new.
	builder newConstant: 17.
	self assert: builder instructions isEmpty
]

{ #category : #tests }
DRIRBuilderTest >> testPushAppendsCreatedInstruction [

	| builder pushInstruction |
	builder := DRIRBuilder new.
	pushInstruction := builder push: 17.
	self assert: (builder instructions includes: pushInstruction)
]

{ #category : #tests }
DRIRBuilderTest >> testPushAppendsnstruction [

	| builder pushInstruction |
	builder := DRIRBuilder new.
	pushInstruction := builder push: 17.
	self assert: builder instructions size equals: 1
]
