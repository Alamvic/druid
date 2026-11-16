Class {
	#name : 'DRCleanTemporaryInstructions',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRCleanTemporaryInstructions >> applyTo: cfg [

	| escapingVars |
	
	"If we use name in scope instead of TemporaryVariable, we can merge these two"
	self cleanArgumentVariables: cfg. 
	self cleanTemporaryVariables: cfg.
	
	
]

{ #category : 'accessing' }
DRCleanTemporaryInstructions >> cleanArgumentVariables: aDRMethodControlFlowGraph [ 
	| args |

	args := (aDRMethodControlFlowGraph blockClosures flatCollect: [ :each | each operands ]) 
							select: [ : each | each scope = aDRMethodControlFlowGraph scope and: [ each isLoadArgument ]].

	args do: [ :each | aDRMethodControlFlowGraph scope addArgumentIntoCopied: each ].
	
]

{ #category : 'accessing' }
DRCleanTemporaryInstructions >> cleanEscapingVars: temps in: aDRScope [ 

	temps do: [ :each | aDRScope moveTempIntoEscaping: each].
]

{ #category : 'accessing' }
DRCleanTemporaryInstructions >> cleanTemporaryVariables: aDRMethodControlFlowGraph [ 
	
	| blockClosures blockOperands tempVectorVars copiedVars storesAfter|
	
	blockClosures := aDRMethodControlFlowGraph blockClosures.
	
	blockOperands := (blockClosures flatCollect: [ :each | each operands ]) 
							select: [ : each | each scope = aDRMethodControlFlowGraph scope].
	
	tempVectorVars := (blockOperands select: [ :each | each isStoreTemporaryVariable ] 
		thenCollect: [ :each | each variableName]) asSet.
		
	copiedVars := (blockOperands select: [ :each | each isLoadTemporaryVariable ] 
		thenCollect: [ :each | each variableName]) asSet.	
			
			
	storesAfter :=  aDRMethodControlFlowGraph allTemporaryInstructions 
		select: [ :each | each isStoreTemporaryVariable].
		
	blockClosures do: [ :blockClosure |
		storesAfter do: [ :store |
			
			((copiedVars includes: store variableName) and: [ blockClosure isPostDominatedBy: store ])
				ifTrue: [ tempVectorVars add: store variableName ]
		]
	].	
	
	copiedVars := copiedVars difference: tempVectorVars.
	
	tempVectorVars do: [ :each | aDRMethodControlFlowGraph scope moveTempIntoTempVector: each ].
	
	copiedVars do: [ :each | aDRMethodControlFlowGraph scope moveTempIntoCopied: each ].
	
	

	
]
