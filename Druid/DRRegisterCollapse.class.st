Class {
	#name : #DRRegisterCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #accessing }
DRRegisterCollapse >> applyTo: cfg [

	cfg instructionsDo: [ :i | 
		i isMandatoryInstruction ifFalse: [ 
			i basicBlock removeInstruction: i ] ]
]
