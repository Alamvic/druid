Class {
	#name : #DRVMStateTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRVMStateTest >> testMergeVMStateToBiggerVMStateFillsHoles [

	| state mergeState irBuilder |
	irBuilder := DRIRGenerator new.
	
	state := irBuilder newVMState.
	state push: 1 asDRValue.

	mergeState := irBuilder newVMState.
	mergeState push: (DRPhiFunction operands: {2 asDRValue}).
	mergeState push: (DRPhiFunction operands: {2 asDRValue}).
	state addTo: mergeState.
	
	self assert: mergeState operandStack first operands size equals: 2.
	self assert: mergeState operandStack second operands size equals: 2
]

{ #category : #tests }
DRVMStateTest >> testMergeVMStateToEmptyVMState [

	| state mergeState irBuilder |
	irBuilder := DRIRGenerator new.
	
	state := irBuilder newVMState.
	state push: 1 asDRValue.


	mergeState := irBuilder newVMState.
	state addTo: mergeState.
	
	self assert: mergeState operandStack size equals: 1
]

{ #category : #tests }
DRVMStateTest >> testMergeVMStateToNonEmptyVMStateFillsState [

	| state mergeState irBuilder |
	irBuilder := DRIRGenerator new.
	
	state := irBuilder newVMState.
	state push: 1 asDRValue.

	mergeState := irBuilder newVMState.
	state addTo: mergeState.
	state addTo: mergeState.
	
	self assert: mergeState operandStack first operands size equals: 2
]
