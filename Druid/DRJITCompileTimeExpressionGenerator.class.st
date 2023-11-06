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
		  binaryMessage: #&
		  receiverInstruction: aDRBitAnd operand1
		  argumentInstruction: aDRBitAnd operand2
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
DRJITCompileTimeExpressionGenerator >> visitInterpreterExpression: aDRInterpreterExpression [

	^ RBLiteralValueNode value: aDRInterpreterExpression name
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
