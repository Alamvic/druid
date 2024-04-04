Class {
	#name : #DRCogitSimpleStackPrimitiveCodeGenerator,
	#superclass : #DRCogitPrimitiveCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitSimpleStackPrimitiveCodeGenerator >> visitLoadArgument: aDRLoadArgument [
	"cogit genLoadArgAtDepth: 0 into: Arg0Reg."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genLoadArg:into:
			 arguments: {
					 (RBLiteralValueNode value: aDRLoadArgument argNum).
					 (RBVariableNode named: aDRLoadArgument result name) })
]
