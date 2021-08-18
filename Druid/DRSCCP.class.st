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
			i isJump ifFalse: [ 
			lattice := self value: i.
			({ 
				 self top.
				 self bottom } includes: lattice) ifFalse: [ 
				i block replaceInstruction: i by: (DRCopy
						 operands: { (DRConstantValue value: lattice) }
						 result: i result) ]. ] ]. 
]

{ #category : #operations }
DRSCCP >> applyTo: cfg [


	self initializeSCCPWithCFG: cfg.
	[ CFGWorkList isEmpty and: SSAWorkList isEmpty ] whileFalse: [ 
		self stepCFGWorklist.
		self stepSSAWorklist].

	self applyPropagation. 
	edges select: [ :e |  e isExecuted]. 
	CFGWorkList. 
	SSAWorkList. 
	latticeValues. 
	"self halt. "
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
DRSCCP >> evaluateAllPhisInBlock: anEdge [

	anEdge child instructionsDo: [ :i | 
		i isPhiFunction ifTrue: [ self evaluatePhi: i ] ]
]

{ #category : #operations }
DRSCCP >> evaluateAssign: anInstruction [

	| previousValue newValue |
	
	anInstruction isJump ifTrue: [ ^ self ].
	anInstruction operands do: [ :o | self setValueOf: o at: (o sccpEvaluateFor: self) ].
	
	previousValue := self value: anInstruction. 
	self setValueOf: anInstruction at: (newValue := anInstruction sccpEvaluateFor: self).
	
	previousValue = newValue ifFalse: [
		SSAWorkList addAll: (self dependenciesOf: anInstruction) ]
]

{ #category : #operations }
DRSCCP >> evaluateConditionnal: aConditionalJump [
	
	CFGWorkList. 
	 (aConditionalJump sccpEvaluateFor: self) ~= (self bottom) ifTrue: [ aConditionalJump targets do: [ :t |
			CFGWorkList add: (edges select: [:e | (e child = t) and: (e parent = (self getBlock: aConditionalJump))]) first]]
		ifFalse: [
			aConditionalJump condition value ifTrue: [ 
				CFGWorkList add: (edges select: [:e | (e child = aConditionalJump trueBranch) and: (e parent = (self getBlock: aConditionalJump))]) first] 
			ifFalse: [ 
				CFGWorkList add: (edges select: [:e | (e child = aConditionalJump falseBranch) and: (e parent = (self getBlock: aConditionalJump))]) first]
			]
		
]

{ #category : #operations }
DRSCCP >> evaluateOperands: aPhi [

	| edge |

	((self value: aPhi) ~= (self bottom)) ifTrue: [ 
		aPhi operands do: [ :r |
			 edge := edges select: [ :e | ((e parent) = (self getBlock: r)) and: ((e child) = (self getBlock: aPhi))]. 
			 edge := edge first.
			 edge isExecuted ifTrue: [
				edge child instructionsDo: [:i |
					i result = r ifTrue: [
						self setValueOf: r at: i ]]]]].
	
	
	
	"aPhi operand1 operand1 value =  aPhi operand1 value 
		ifTrue: [ 
			self setValueOf: aPhi at: aPhi operand1. 
			self halt.].
	self halt. "
]

{ #category : #operations }
DRSCCP >> evaluatePhi: anInstruction [ 
	|operandValues|
	
	operandValues := self evaluateOperands: anInstruction. 
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

	cfg instructionsDo: [ :i | latticeValues at: i put: self top ]
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

	| leftEdge rightEdge |
	
	edges do: [ :e | ((e parent = (self getBlock: operation operand1)) and:  (e child = (self getBlock: operation))) ifTrue: [ leftEdge := e ] ]. 
	edges do: [ :e | ((e parent = (self getBlock: operation operand2)) and: (e child = (self getBlock: operation))) ifTrue: [ rightEdge := e ] ].
	
	(leftEdge isExecuted xor: rightEdge isExecuted) ifTrue: [ 
		leftEdge isExecuted ifFalse: [ ^ self value: operation operand2 ].
		rightEdge isExecuted ifFalse: [ ^ self value: operation operand1 ]].
	
	operation operand1 operand1 value = operation operand2 operand1 value 
		ifTrue: [ 
			^ operation operand1 operand1 value ]
		ifFalse: [ 
			^ self value: operation ]
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
		currentEdge mark. 
		self evaluateAllPhisInBlock: currentEdge.
		edgeList := self executedEdgesEntering: currentEdge child.
		edgeList size = 1 ifTrue: [ 
			currentEdge child instructionsDo: [ :instruction | 
				instruction isConditionalJump
					ifFalse: [ 
						currentEdge child instructions last isConditionalJump ifFalse: [ 
						self evaluateAssign: instruction. 
						CFGWorkList addAll: (edges select: [ :e | e parent = currentEdge child ])
					]]
					ifTrue: [ self evaluateConditionnal: instruction ] ] ] ] .

	
]

{ #category : #operations }
DRSCCP >> stepSSAWorklist [

	| currentInstruction workingBlock |
	SSAWorkList isEmpty ifTrue: [ ^ self ].

	currentInstruction := SSAWorkList removeFirst.
	workingBlock := self getBlock: currentInstruction.
	(self executedEdgesEntering: workingBlock) isEmpty ifFalse: [ 
		currentInstruction isPhiFunction
			ifTrue: [ self evaluatePhi: currentInstruction ]
			ifFalse: [ 
					workingBlock instructionsDo: [ :i | self evaluateAssign: i ]]]
]

{ #category : #'accessing - constants' }
DRSCCP >> top [
	^ 'Top'
]

{ #category : #operations }
DRSCCP >> value: aValue [

	"Return the lattice value for the register"
	aValue isConstant ifTrue: [^ aValue value ].
	^ latticeValues at: aValue
]
