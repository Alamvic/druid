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
DRJITCompileTimeExpressionGenerator >> visitConstant: aDRConstantValue [ 
	
	^ RBLiteralValueNode value: aDRConstantValue value
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitCopy: aDRCopy [ 
	
	^ aDRCopy operand1 acceptVisitor: self	
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitJITCompileTimeExpression: aDRJitCompileTimeExpression [ 
	
	^ RBParser parseExpression: aDRJitCompileTimeExpression expression
]

{ #category : #visiting }
DRJITCompileTimeExpressionGenerator >> visitPhiFunction: aDRPhiFunction [ 
	
	^ aDRPhiFunction operand1 acceptVisitor: self	
]
