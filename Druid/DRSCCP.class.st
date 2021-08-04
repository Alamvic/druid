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
		  e parent = (self getBlock: aDRJump) and: [ 
			  e child = aDRJump target ] ]
]

{ #category : #operations }
DRSCCP >> applyPropagation [

	instancecfg instructionsDo: [ :i | 
			| lattice |
			lattice := self value: i.
			({ 
				 self top.
				 self bottom } includes: lattice) ifFalse: [ 
				i block replaceInstruction: i by: (DRCopy
						 operands: { (DRConstantValue value: lattice) }
						 result: i result) ].  ]. 
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
	
	| ret |
	anInstruction isConditionalJump ifTrue: [
		self value = self top ifTrue: [ 
			ret := OrderedCollection new. 
			ret addAll: anInstruction trueBranch instructions.
			ret addAll: anInstruction falseBranch instructions. 
			^ ret ]
		ifFalse: [ 
			"We assume self value = self bottom" 
			anInstruction condition value ifTrue: [ 
				 ret := OrderedCollection new. 
				 ret addAll: anInstruction trueBranch instructions.
				^ ret. ]
			ifFalse: [ 
				ret := OrderedCollection new. 
				ret addAll: anInstruction falseBranch instructions.
				^ ret. ] ] ]. 
	^ instancecfg instructions select: [ :instruction | 
		  instruction operands includes: anInstruction ]
]

{ #category : #operations }
DRSCCP >> dependentInstructionsOf: aValue inList: aList [

	^ aList select: [ :anInstruction | anInstruction operands includes: aValue ]
]

{ #category : #operations }
DRSCCP >> evaluateAllPhisInBlock: anEdge [

	anEdge child instructionsDo: [ :i | i isPhiFunction ifTrue: [ self evaluateOperands: i ] ]. 
	"self halt. "
	"anEdge child instructionsDo: [ :i | i isPhiFunction ifFalse: [ self evaluateResult: i ] ]." 
]

{ #category : #operations }
DRSCCP >> evaluateAssign: anInstruction [

	| previousValue newValue |
	

	previousValue := self value: anInstruction. 
	self setValueOf: anInstruction at: (newValue := anInstruction sccpEvaluateFor: self).
	
	previousValue = newValue ifFalse: [
		SSAWorkList addAll: (self dependenciesOf: anInstruction) ]. 
]

{ #category : #operations }
DRSCCP >> evaluateConditionnal: aBlock [

	| c  block|
	self halt. 
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
DRSCCP >> evaluateOperands: aPhi [

	"(self value: aPhi ~= self bottom) ifTrue: [ 
		aPhi operands do: [ :o |
			 ] ]"
	aPhi operand1 operand1 value =  aPhi operand1 value 
		ifTrue: [ 
			self setValueOf: aPhi at: aPhi operand1. 
			self halt.].
	"self halt. "
]

{ #category : #operations }
DRSCCP >> evaluatePhi: anInstruction [ 
	
	self evaluateOperands: anInstruction. 
	self evaluateResult: anInstruction. 
]

{ #category : #operations }
DRSCCP >> evaluateResult: aPhi [

	| value |
	((self value: aPhi) ~= self bottom) ifTrue: [ 
		 value := self interpretePhiFunction: aPhi.
		 (value ~= (self value: aPhi)) ifTrue: [ 
			 self setValueOf: aPhi at: value. 
			 SSAWorkList addAll: (self dependenciesOf: aPhi) ]].
]

{ #category : #operations }
DRSCCP >> executedEdgesEntering: aBlock [

	^ edges select: [ :e | e child = aBlock and: [e isExecuted]]. 
	
]

{ #category : #operations }
DRSCCP >> getBlock: anInstruction [

	"Returns a block from a control flow graph that contains the instruction"
	
	^ anInstruction block
]

{ #category : #operations }
DRSCCP >> initialize [

	super initialize.
	latticeValues := OrderedDictionary new.
]

{ #category : #operations }
DRSCCP >> initializeLatticeValues: cfg [

	cfg instructionsDo: [ :i |
			latticeValues at: i put: self top.
			 i operands do: [ :o |
				o isConstant ifTrue: [ self setValueOf: o at: o value ] ] ]. 
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

	"| leftEdge rightEdge |
	
	edges do: [ :e | ((e parent = (self getBlock: operation operand1)) and:  (e child = (self getBlock: operation))) ifTrue: [ leftEdge := e ] ]. 
	edges do: [ :e | ((e parent = (self getBlock: operation operand2)) and: (e child = (self getBlock: operation))) ifTrue: [ rightEdge := e ] ]. 
	self halt. 
	(leftEdge isExecuted and: rightEdge isExecuted) ifFalse: [ 
		leftEdge isExecuted ifFalse: [ ^ self value: operation operand2 ].
		rightEdge isExecuted ifFalse: [ ^ self value: operation operand1 ]]."
	
	
	operation operand1 operand1 value = operation operand2 operand1 value 
		ifTrue: [ 
			^ operation operand1 operand1 value ]
		ifFalse: [ 
			^ self value: operation ]
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
	workingBlock := self getBlock: currentInstruction.
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
