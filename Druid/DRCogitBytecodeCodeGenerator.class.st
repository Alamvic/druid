Class {
	#name : #DRCogitBytecodeCodeGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitBytecodeCodeGenerator >> visitCall: aDRCall [

	super visitCall: aDRCall.
	aDRCall isMapped ifTrue: [
		self assert: aDRCall controlFlowGraph hasAnnotatedBytecode ]
]
