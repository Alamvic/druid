Class {
	#name : #DRDeadEdgeSplitting,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRDeadEdgeSplitting >> canBeAppliedWithGeneratedPathsIn: aDRControlFlowGraph [ 

	^ aDRControlFlowGraph deadPaths isNotEmpty 
]

{ #category : #accessing }
DRDeadEdgeSplitting >> doApplyWithGeneratedPaths: aCFG [

	"Find first (longest) dead path in CFG and split guilty edge"

	| edge |
	aCFG deadPaths ifEmpty: [ ^ self ].
	
	edge := (aCFG deadPaths associations detectMax: [ :v | v value size ]) key.
	[ edge destination predecessors size = 1 ] whileTrue: [ 
		edge := edge parent.
		edge ifNil: [ ^ self ] ].

	edge split

]
