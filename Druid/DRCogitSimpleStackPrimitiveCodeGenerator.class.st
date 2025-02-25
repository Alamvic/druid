Class {
	#name : 'DRCogitSimpleStackPrimitiveCodeGenerator',
	#superclass : 'DRCogitPrimitiveCodeGenerator',
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'visiting' }
DRCogitSimpleStackPrimitiveCodeGenerator >> visitLoadArgument: aDRLoadArgument [
	"cogit genLoadArg: i spilled: n into: Reg."

	self generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genLoadArg:spilled:into:
			 arguments: {
					 (RBLiteralValueNode value: aDRLoadArgument argNum).
					 (RBLiteralValueNode value: aDRLoadArgument controlFlowGraph numberOfSpillSlots ).
					 (RBVariableNode named: aDRLoadArgument result name) })
]
