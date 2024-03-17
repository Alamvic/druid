Class {
	#name : #DRArgumentCountCheckCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#instVars : [
		'argumentCount'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRArgumentCountCheckCustomisation class >> pragmaName [

	^ 'argumentCountCheck'
]

{ #category : #factory }
DRArgumentCountCheckCustomisation class >> withArgumentCount: anInteger [

	^ self new
		  argumentCount: anInteger;
		  yourself
]

{ #category : #'cog jit support' }
DRArgumentCountCheckCustomisation >> argumentCount: anInteger [

	argumentCount := anInteger
]

{ #category : #api }
DRArgumentCountCheckCustomisation >> generateCustomisedPreamble: aDRGeneratorMethodBuilder [ 

	self flag: #TODO. "Improve the costumisation model"
	
	"Nothing"
]

{ #category : #api }
DRArgumentCountCheckCustomisation >> optimize: aDRPrimitiveControlFlowGraph [

	"self methodNumArgs = 0 ifTrue: [ ... ]."

	| checkBlock |
	checkBlock := aDRPrimitiveControlFlowGraph newBasicBlockWith: [ :b |
		              | condition |
		              condition := b
			                           equals: DRInterpreterReference argumentCount
			                           to: argumentCount asDRValue.
		              b
			              jumpIf: condition
			              to: aDRPrimitiveControlFlowGraph firstBasicBlock
			              ifFalseTo: aDRPrimitiveControlFlowGraph failureExitBlock ].
	aDRPrimitiveControlFlowGraph initialBasicBlock jumpTo: checkBlock
]
