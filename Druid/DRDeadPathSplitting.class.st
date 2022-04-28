Class {
	#name : #DRDeadPathSplitting,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadPathSplitting >> doApply: aCFG [

	| edge |
	aCFG generatePaths.
	self assert: aCFG deadPaths isNotEmpty.
	edge := aCFG deadPaths keys first.
	[ edge destination predecessors size = 1 ] whileTrue: [ 
		edge := edge parent.
		edge ifNil: [ ^ self ] ].

	('Splitting: ' , edge destination id asString , ' -> ' , edge source id asString) traceCr.

	edge destination splitTo: edge source.
	aCFG validate.
	aCFG generatePaths

	"	(aCFG deadPaths keys 
		collect: [ :edge | edge source -> edge destination ] as: Set)
		do: [ :association |
			association value splitTo: association key ]
	"
]
