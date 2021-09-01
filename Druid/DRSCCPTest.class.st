Class {
	#name : #DRSCCPTest,
	#superclass : #TestCase,
	#instVars : [
		'optimisation',
		'instructionFactory',
		'leftOperand1',
		'leftOperand2',
		'rightOperand1',
		'rightOperand2'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRSCCPTest >> setUp [

	super setUp.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRSCCP new.
	
	"Set the operands of the add instructions"
	leftOperand1 := 3.
	leftOperand2 := 4.
	
	rightOperand1 := 5.
	rightOperand2 := 6.
]
