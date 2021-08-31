Class {
	#name : #DRSCCPInstructionEvaluationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPInstructionEvaluationTest >> testEvaluateNonConstantConditionalJump [

	| sccp instruction value |
	sccp := DRSCCP new.
	instruction := DRConditionalJump operands: { 
		DRTest operands: { 
			1 asDRValue.
			2 asDRValue }
			result: (DRTemporaryRegister id: 0) }.
	
	value := instruction sccpEvaluateFor: sccp.
	self assert: value equals: sccp bottom
]
