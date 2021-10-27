Class {
	#name : #DRSCCPTest,
	#superclass : #DROptimisationTest,
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

{ #category : #accessing }
DRSCCPTest >> optimisation [

	^ optimisation
]

{ #category : #accessing }
DRSCCPTest >> optimisation: anObject [

	optimisation := anObject
]

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

{ #category : #helpers }
DRSCCPTest >> setUpCFGWithConditional: jump [

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1.
	
	jump condition isInstruction ifTrue: [
		b1 addInstruction: jump condition.
	].
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	jump falseBranch: b3.
	b2 jumpTo: b4. 
	b3 jumpTo: b4. 
		
	cfg b0: b0.
	cfg b1: b1. 
	cfg b2: b2. 
	cfg b3: b3. 
	cfg b4: b4. 
	cfg conditionnalJump: jump. 

	^ cfg
]
