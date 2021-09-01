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

{ #category : #tests }
DRSCCPTest >> setUpCFG: n [

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	
	cfg initialBasicBlock: b0.
	
	n = 1 ifTrue: [ 
		b0 endInstruction: DRNoop new.
		cfg initialBasicBlock: b0. 
		cfg b0: b0. ].
	
	n = 2 ifTrue: [ 
		b1 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		
		cfg b0: b0. 
		cfg b1: b1. ].

	n = 3 ifTrue: [ 
		b2 endInstruction: DRNoop new.
		b0 jumpTo: b1.
		b1 jumpTo: b2. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. ].
	
	n = 4 ifTrue: [ 
		b3 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		b1 jumpTo: b2. 
		b2 jumpTo: b3. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. 
		cfg b3: b3. ].

	^ cfg
]

{ #category : #helpers }
DRSCCPTest >> setUpCFGWithConditional: jump [

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4. 
	
	cfg initialBasicBlock: b0.
	
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
