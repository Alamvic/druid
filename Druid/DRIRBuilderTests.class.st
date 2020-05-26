Class {
	#name : #DRIRBuilderTests,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRIRBuilderTests >> testIRBuilderStartsEmpty [

	self assert: DRIRBuilder new instructions isEmpty
]

{ #category : #tests }
DRIRBuilderTests >> testLoadAddressAppendsCreatedInstruction [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: 17.
	self assert: (builder instructions includes: loadInstruction)
]

{ #category : #tests }
DRIRBuilderTests >> testLoadAddressAppendsInstruction [

	| builder loadInstruction |
	builder := DRIRBuilder new.
	loadInstruction := builder loadAddress: 17.
	self assert: builder instructions size equals: 1
]

{ #category : #tests }
DRIRBuilderTests >> testNewConstantDoesNotAppendInstruction [

	| builder |
	builder := DRIRBuilder new.
	builder newConstant: 17.
	self assert: builder instructions isEmpty
]

{ #category : #tests }
DRIRBuilderTests >> testPushAppendsCreatedInstruction [

	| builder pushInstruction |
	builder := DRIRBuilder new.
	pushInstruction := builder push: 17.
	self assert: (builder instructions includes: pushInstruction)
]

{ #category : #tests }
DRIRBuilderTests >> testPushAppendsnstruction [

	| builder pushInstruction |
	builder := DRIRBuilder new.
	pushInstruction := builder push: 17.
	self assert: builder instructions size equals: 1
]
