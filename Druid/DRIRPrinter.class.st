Class {
	#name : 'DRIRPrinter',
	#superclass : 'Object',
	#instVars : [
		'cfg'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRIRPrinter class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph.
]

{ #category : 'converting' }
DRIRPrinter >> asString [

	^ '[[b' , cfg initialBasicBlock id asString , ']]' , String cr , String cr
	  , (cfg blocks flatCollect: [ :b | self blockAsString: b ] as: String)
]

{ #category : 'as yet unclassified' }
DRIRPrinter >> blockAsString: aDRBasicBlock [

	| res |
	res := self blockHeaderFor: aDRBasicBlock.

	res := res , (aDRBasicBlock instructions flatCollect: [ :i |
		        String tab , (self instructionAsString: i) , String cr ]).

	^ res
]

{ #category : 'converting' }
DRIRPrinter >> blockHeaderFor: aDRBasicBlock [

	| res |
	res := 'b' , aDRBasicBlock id asString , ':'.

	aDRBasicBlock predecessors size > 1 ifTrue: [
		res := res , ('[' , (', ' join:
			         (aDRBasicBlock predecessors collect: [ :b |
				          'b' , b id asString ])) , ']') ].

	res := res , String cr.
	^ res
]

{ #category : 'initialization' }
DRIRPrinter >> initializeFor: aDRControlFlowGraph [

	cfg := aDRControlFlowGraph.
]

{ #category : 'as yet unclassified' }
DRIRPrinter >> instructionAsString: aDRInstruction [
	
	^ aDRInstruction acceptVisitor: self.
]

{ #category : 'printing' }
DRIRPrinter >> printInstr: aDRValue withOperands: prettyOperands [

	^ aDRValue result prettyPrint , ' := ' , aDRValue opcode , '('
	  , prettyOperands , ')'
]

{ #category : 'printing' }
DRIRPrinter >> printRegularOperands: aDRValue [

	^ ', ' join:
		  (aDRValue operands collect: [ :op | op result prettyPrint ])
]

{ #category : 'visiting' }
DRIRPrinter >> visitAddition: aDRAdd [

	^ self
		  printInstr: aDRAdd
		  withOperands: (self printRegularOperands: aDRAdd)
]

{ #category : 'visiting' }
DRIRPrinter >> visitBranchIfCondition: aDRBranchIfCondition [

	^ self
		  printInstr: aDRBranchIfCondition
		  withOperands: aDRBranchIfCondition condition opcode , ', '
			  , (self printRegularOperands: aDRBranchIfCondition) , '; '
			  , (', ' join:
					   (aDRBranchIfCondition targets collect: [ :b |
						    'b' , b id asString ]))
]

{ #category : 'visiting' }
DRIRPrinter >> visitCopy: aDRCopy [

	^ self
		  printInstr: aDRCopy
		  withOperands: (self printRegularOperands: aDRCopy)
]

{ #category : 'visiting' }
DRIRPrinter >> visitGetConditionCode: aDRGetConditionCode [

	^ self
		  printInstr: aDRGetConditionCode
		  withOperands: (self printRegularOperands: aDRGetConditionCode)
]

{ #category : 'visiting' }
DRIRPrinter >> visitJump: aDRJump [

	^ self
		  printInstr: aDRJump
		  withOperands: 'b' , aDRJump target id asString
]

{ #category : 'visiting' }
DRIRPrinter >> visitLoadArgument: aDRLoadArgument [ 
	
	^ self
		  printInstr: aDRLoadArgument
		  withOperands: (self printRegularOperands: aDRLoadArgument).
]

{ #category : 'visiting' }
DRIRPrinter >> visitLoadReceiver: aDRLoadReceiver [ 
	
	^ self
		  printInstr: aDRLoadReceiver
		  withOperands: (self printRegularOperands: aDRLoadReceiver)
]

{ #category : 'visiting' }
DRIRPrinter >> visitMultiply: aDRMultiply [

	^ self
		  printInstr: aDRMultiply
		  withOperands: (self printRegularOperands: aDRMultiply)
]

{ #category : 'visiting' }
DRIRPrinter >> visitNoop: aDRNoop [

	^ self printInstr: aDRNoop withOperands: ''
]

{ #category : 'visiting' }
DRIRPrinter >> visitPhiFunction: aDRPhiFunction [

	^ self
		  printInstr: aDRPhiFunction
		  withOperands: (self printRegularOperands: aDRPhiFunction)
]

{ #category : 'visiting' }
DRIRPrinter >> visitReturn: aDRReturn [

	^ self
		  printInstr: aDRReturn
		  withOperands: (self printRegularOperands: aDRReturn)
]

{ #category : 'visiting' }
DRIRPrinter >> visitSubstract: aDRSubtract [ 
		^ self
		  printInstr: aDRSubtract
		  withOperands: (self printRegularOperands: aDRSubtract).
]
