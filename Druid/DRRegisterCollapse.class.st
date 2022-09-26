Class {
	#name : #DRRegisterCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #accessing }
DRRegisterCollapse >> applyTo: cfg [

	cfg instructionsDo: [ :i | 
		i result isNoResult ifFalse: [ 
			i users size > 1 ifTrue: [ self error: 'Same result used in many instructions' ].
			i basicBlock removeInstruction: i ] ]
]
