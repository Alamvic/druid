"
The true SCCP (with conditionnal jumps) 
"
Class {
	#name : #DRSCCP,
	#superclass : #Object,
	#instVars : [
		'latticeValues',
		'CFGWorkList',
		'SSAWorkList',
		'edges',
		'instancecfg'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #evaluating }
DRSCCP >> addJumpToCFGWorklist: aDRJump [ 
	
	^ edges detect: [ :e | 
		e parent = (self getBlock: aDRJump from: instancecfg)
			and: [ e child = aDRJump target ] ]
]

{ #category : #operations }
DRSCCP >> applyPropagation [

	instancecfg blocksDo: [ :b | 
		b instructionsDo: [ :i | 
			| lattice |
			lattice := self value: i.
			({ 
				 self top.
				 self bottom } includes: lattice) ifFalse: [ 
				b replaceInstruction: i by: (DRCopy
						 operands: { (DRConstantValue value: lattice) }
						 result: i result) ] ] ]
]

{ #category : #operations }
DRSCCP >> applyTo: cfg [

	self initializeSCCPWithCFG: cfg.
	[ CFGWorkList isEmpty and: SSAWorkList isEmpty ] whileFalse: [ 
		self stepCFGWorklist.
		self stepSSAWorklist ].
	self applyPropagation
]

{ #category : #'accessing - constants' }
DRSCCP >> bottom [

	^ 'Bot'
]

{ #category : #operations }
DRSCCP >> buildEdges: cfg [

	| list workList currentBlock edge|
	
	list := OrderedCollection new. 
	workList := OrderedCollection new. 
	workList add: cfg initialBasicBlock. 
	
	[workList isEmpty] whileFalse: [ 
		currentBlock := workList removeFirst. 
		currentBlock successors do: [ :s | 
			edge := DREdge new. 
			edge parent: currentBlock. 
			edge child: s. 
			list add: edge. 
			workList add: s]].
	
	^ list
	
]

{ #category : #operations }
DRSCCP >> dependenciesOf: anInstruction [

	^ instancecfg instructions select: [ :instruction | 
		  instruction operands includes: anInstruction ]
]

{ #category : #operations }
DRSCCP >> dependentInstructionsOf: aValue inList: aList [

	^ aList select: [ :anInstruction | anInstruction operands includes: aValue ]
]

{ #category : #operations }
DRSCCP >> evaluateAllPhisInBlock: anEdge [

	^ nil
]

{ #category : #operations }
DRSCCP >> evaluateAssign: anInstruction [

	| previousValue newValue |
	
	previousValue := self value: anInstruction. 
	self setValueOf: anInstruction at: (newValue := anInstruction sccpEvaluateFor: self).
	
	previousValue = newValue ifFalse: [
		SSAWorkList addAll: (self dependenciesOf: anInstruction) ]
]

{ #category : #operations }
DRSCCP >> evaluateConditionnal: aBlock [

	| c  block|
	aBlock instructionsDo: [ :s |
		 s operands do: [ :d |
			(self value: d ~= self bottom) ifTrue: [
				(self value: d ~= self value: s) ifTrue:[
				  self setValueOf: d at: (self value: s). 
					(self value: d = self bottom) ifTrue: [edges do: [:e | e parent = aBlock ifTrue: [ CFGWorkList add: e]]]
					ifFalse: [
						c := aBlock instruction last.
						c condition ifTrue: [
							block := c trueBranch. ]
						ifFalse: [block := c falseBranch]]]]]]. 
]

{ #category : #evaluation }
DRSCCP >> evaluateInstruction: anInstruction [

	self evaluateAssign: anInstruction
"Add the successor of the block to the worklist"
						"CFGWorkList addAll:
							(currentEdge child successors collect: [ :successor | 
								 edges detect: [ :succesorEdge | 
									 succesorEdge parent = currentEdge child and: [ 
										 succesorEdge child = successor ] ] ])"
]

{ #category : #operations }
DRSCCP >> evaluatePhi: anInstruction [ 

	^ nil
]

{ #category : #operations }
DRSCCP >> executedEdgesEntering: aBlock [

	^ edges select: [ :e | e child = aBlock and: [e isExecuted]]. 
	
]

{ #category : #operations }
DRSCCP >> getBlock: anInstruction from: aCFG [

	"Returns a block from a control flow graph that contains the instruction"
	
	aCFG blocksDo: [ :b | b instructionsDo: [ :i | i = anInstruction ifTrue: [ ^ b ] ] ].
	^ nil. 
]

{ #category : #operations }
DRSCCP >> initialize [

	super initialize.
	latticeValues := Dictionary new.
]

{ #category : #operations }
DRSCCP >> initializeLatticeValues: cfg [

	cfg blocksDo: [ :b | 
		b instructionsDo: [ :i |
			
			latticeValues at: i put: self top.
			i operands do: [ :o | 
				o isConstant ifTrue: [ self setValueOf: o at: o value ] ] ] ]
]

{ #category : #operations }
DRSCCP >> initializeSCCPWithCFG: cfg [

	instancecfg := cfg.
	SSAWorkList := OrderedCollection new.
	edges := OrderedCollection newFrom: (self buildEdges: cfg). "This operation marks the edges as unexecuted."
	CFGWorkList := OrderedCollection newFrom:
		               (edges select: [ :edge | 
			                edge parent = cfg initialBasicBlock ]). "Edges leaving the initial block."

	self initializeLatticeValues: cfg
]

{ #category : #operations }
DRSCCP >> interpretOverLatticeValue: operation [ 

	operation isPhiFunction ifTrue: [ self interpretePhiFunction: operation. ^ self. ]. 
	operation operands: (operation operands collect: [ :op | self interpreteValue: op ]). 
	"Add constant folding here"

	
]

{ #category : #operations }
DRSCCP >> interpretePhiFunction: operation [

	| operandsList |
	operation operand1 operand1 value = operation operand2 operand1 value 
		ifTrue: [ 
			operandsList := OrderedCollection new.
			operandsList add: operation operand1 operand1.
			operation operands: operandsList.
			latticeValues at: operation put: operation operand1. ] 
]

{ #category : #operations }
DRSCCP >> interpreteValue: op [

	|lv|
	
	lv := latticeValues at: op ifAbsent: [ 1halt. op ]. 
	({self bottom. self top} includes: lv) ifTrue: [  1halt. ^ op ]. 
	^ lv. 
]

{ #category : #operations }
DRSCCP >> setValueOf: aRegister at: aValue [
	"Return the lattice value for the register"
	latticeValues at: aRegister put: aValue. 
]

{ #category : #operations }
DRSCCP >> stepCFGWorklist [

	| currentEdge edgeList |
	CFGWorkList isEmpty ifTrue: [ ^ self ].

	currentEdge := CFGWorkList removeFirst.
	currentEdge isExecuted ifFalse: [ 
		self evaluateAllPhisInBlock: currentEdge.
		edgeList := self executedEdgesEntering: currentEdge child ].

	edgeList isEmpty ifTrue: [ 
		currentEdge child instructionsDo: [ :instruction | 
			self evaluateInstruction: instruction ] ]
]

{ #category : #operations }
DRSCCP >> stepSSAWorklist [

	| currentInstruction workingBlock |
	SSAWorkList isEmpty ifTrue: [ ^ self ].

	currentInstruction := SSAWorkList removeFirst.
	workingBlock := self getBlock: currentInstruction from: instancecfg.
	(self executedEdgesEntering: workingBlock) isEmpty ifTrue: [ 
		currentInstruction isPhiFunction
			ifTrue: [ self evaluatePhi: currentInstruction ]
			ifFalse: [ 
				workingBlock lastInstruction isConditionalJump
					ifFalse: [ 
					workingBlock instructionsDo: [ :i | self evaluateAssign: i ] ]
					ifTrue: [ self evaluateConditionnal: workingBlock ] ] ]
]

{ #category : #'accessing - constants' }
DRSCCP >> top [
	^ 'Top'
]

{ #category : #operations }
DRSCCP >> value: aRegister [

	"Return the lattice value for the register"
	^ latticeValues at: aRegister
]
