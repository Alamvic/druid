Class {
	#name : #DRAnnotateBytecodeInstructionCoalescing,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #accessing }
DRAnnotateBytecodeInstructionCoalescing >> applyTo: cfg [

	"Moves all annotateBytecode: to the beginning of the cfg."
	
	self flag: #TODO. 
	"Make sense having more than one?"
	"We should move it to the dominator critical path?"

	cfg instructionsDo: [ :i |
		i isAnnotateBytecode ifTrue: [
			i basicBlock = cfg firstBasicBlock ifFalse: [
				i basicBlock removeInstruction: i.
				cfg firstBasicBlock addInstructionFirst: i ] ] ]
]
