Class {
	#name : #DRJITCompileTimeExpressionGenerator,
	#superclass : #Object,
	#category : #Druid
}

{ #category : #building }
DRJITCompileTimeExpressionGenerator >> binaryMessage: aSymbol receiverInstruction: aDRAdd argumentInstruction: aDRConstantValue [

	| receiver argument |
	receiver := aDRAdd acceptVisitor: self.
	argument := aDRConstantValue acceptVisitor: self.

	^ RBMessageNode
		  receiver: receiver
		  selector: aSymbol
		  arguments: { argument }
]

{ #category : #parsing }
DRJITCompileTimeExpressionGenerator >> parse: aString [

	^ RBParser parseExpression: aString
]

{ #category : #building }
DRJITCompileTimeExpressionGenerator >> simpleMessage: aSymbol receiverInstruction: aDRInstruction [

	| receiver |
	receiver := aDRInstruction acceptVisitor: self.

	^ RBMessageNode receiver: receiver selector: aSymbol arguments: {  }
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visit: anInstruction [
	
	^ anInstruction acceptVisitor: self
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitAddition: aDRAdd [ 
	
	^ self 
		binaryMessage: #+
		receiverInstruction: aDRAdd operand1
		argumentInstruction: aDRAdd operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitBitAnd: aDRBitAnd [

	^ self
		  binaryMessage: #bitAnd:
		  receiverInstruction: aDRBitAnd operand1
		  argumentInstruction: aDRBitAnd operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitBranchIfCondition: aDRBranchIfCondition [

	^ aDRBranchIfCondition condition acceptVisitor: self withBranch: aDRBranchIfCondition
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitConstant: aDRConstantValue [ 
	
	^ RBLiteralValueNode value: aDRConstantValue value
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitCopy: aDRCopy [ 
	
	^ aDRCopy operand1 acceptVisitor: self	
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranchIfCondition [

	aDRBranchIfCondition operands last value = true ifTrue: [
		^ aDRBranchIfCondition operands first acceptVisitor: self ].

	^ self
		  binaryMessage: #=
		  receiverInstruction: aDRBranchIfCondition operand1
		  argumentInstruction: aDRBranchIfCondition operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitGetConditionCode: aDRGetConditionCode [ 
	self shouldBeImplemented.
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitGreaterThan: aDRGreaterThanComparison inBranch: aDRBranchIfCondition [

	^ self
		  binaryMessage: #>
		  receiverInstruction: aDRBranchIfCondition operand1
		  argumentInstruction: aDRBranchIfCondition operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitInterpreterExpression: aDRInterpreterExpression [

	^ RBVariableNode named: aDRInterpreterExpression name
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitInterpreterReference: aDRInterpreterReference [ 

	^ self parse: aDRInterpreterReference name
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitJITCompileTimeExpression: aDRJitCompileTimeExpression [ 
	
	^ self parse: aDRJitCompileTimeExpression expression
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitLeftShift: aDRLeftShift [

	^ self
		  binaryMessage: #'<<'
		  receiverInstruction: aDRLeftShift operand1
		  argumentInstruction: aDRLeftShift operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitLoad: aDRLoad [

	self assert: aDRLoad isJITCompileTimeExpression.
	"Just visit the operand, should be a variable"
	^ aDRLoad operand1 acceptVisitor: self
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitNegate: aDRNegated [ 

	^ self simpleMessage: #negated receiverInstruction: aDRNegated operand1
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitPhiFunction: aDRPhiFunction [ 

	^ RBVariableNode named: aDRPhiFunction asJitCompileTimeExpression expression
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitRightShift: aDRRightShift [

	^ self
		  binaryMessage: #'>>'
		  receiverInstruction: aDRRightShift operand1
		  argumentInstruction: aDRRightShift operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitStore: aDRStore [

	| variable value |
	variable := RBVariableNode named: aDRStore address name.
	value := aDRStore operand1 acceptVisitor: self.

	^ RBAssignmentNode variable: variable value: value
]
