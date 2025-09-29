Class {
	#name : 'DRCopyPropagation',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRCopyPropagation >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions do: [ :i |
		(i isCopy and: [
			 i result isPhysicalRegister not and: [ i operand1 isInstruction ] ])
			ifTrue: [ self propagateCopyFrom: i ] ]
]

{ #category : 'testing' }
DRCopyPropagation >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph instructions anySatisfy: [ :i | i isCopy ]
]

{ #category : 'operations' }
DRCopyPropagation >> propagateCopyFrom: i [

	i users copy do: [ :user | user replaceDependency: i by: i operands first ]
]
