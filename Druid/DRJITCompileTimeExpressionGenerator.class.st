Class {
	#name : #DRJITCompileTimeExpressionGenerator,
	#superclass : #Object,
	#category : #Druid
}

{ #category : #building }
DRJITCompileTimeExpressionGenerator >> binaryMessage: aSymbol receiverInstruction: operand1 argumentInstruction: operand2 [

	| receiver argument |
	receiver := self operandNodeFor: operand1.
	argument := self operandNodeFor: operand2.

	^ RBMessageNode
		  receiver: receiver
		  selector: aSymbol
		  arguments: { argument }
]

{ #category : #accessing }
DRJITCompileTimeExpressionGenerator >> operandNodeFor: aDRInstruction [

	aDRInstruction isConstanteable ifFalse: [
		^ RBVariableNode named: 't' , aDRInstruction result id asString ].
	^ aDRInstruction acceptVisitor: self
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
	
	^ self operandNodeFor: aDRCopy operand1
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
	
	^ aDRGetConditionCode condition acceptVisitor: self withBranch: aDRGetConditionCode
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

	^ RBVariableNode named: aDRInterpreterReference name
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitJITCompileTimeExpression: aDRJitCompileTimeExpression [ 
	
	^ RBParser parseExpression: aDRJitCompileTimeExpression expression
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
DRJITCompileTimeExpressionGenerator >> visitMultiply: aDRMultiply [

	^ self
		  binaryMessage: #*
		  receiverInstruction: aDRMultiply operand1
		  argumentInstruction: aDRMultiply operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitNegate: aDRNegated [ 

	^ self simpleMessage: #negated receiverInstruction: aDRNegated operand1
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitObjectReferenceValue: aDRConstantValue [ 
	
	^ RBVariableNode named: aDRConstantValue value expression
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitPhiFunction: aDRPhiFunction [ 

	^ RBVariableNode named: 't', aDRPhiFunction result id asString
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

	^ RBAssignmentNode
		  variable: (RBVariableNode named: aDRStore operand2 name)
		  value: (aDRStore operand1 acceptVisitor: self)
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitSubstract: aDRSubtract [

	^ self
		  binaryMessage: #-
		  receiverInstruction: aDRSubtract operand1
		  argumentInstruction: aDRSubtract operand2
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitTestMask: aDRTestMask inBranch: aDRGetConditionCode [

	^ self
		  binaryMessage: #anyMask:
		  receiverInstruction: aDRGetConditionCode operand1
		  argumentInstruction: aDRGetConditionCode operand2
]
