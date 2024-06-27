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

{ #category : #visiting }
DRCogitBytecodeCodeGenerator >> visitFlushStack: aDRFlushStack [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssFlushStackExceptTop:
			 arguments: { (RBLiteralValueNode value: aDRFlushStack operand1 value) })
]
