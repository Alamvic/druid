Class {
	#name : #DRCogitStackToRegisterPrimitiveCodeGenerator,
	#superclass : #DRCogitPrimitiveCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #'code generation' }
DRCogitStackToRegisterPrimitiveCodeGenerator >> spRegVariable [

	^ RBVariableNode named: 'SPReg'
]

{ #category : #visiting }
DRCogitStackToRegisterPrimitiveCodeGenerator >> visitLoadArgument: aDRLoadArgument [
	"Just copy the Arg[X]Reg to the result"

	| sourceRegister destinationRegister |
	sourceRegister := 'Arg' , aDRLoadArgument argNum asString , 'Reg'.
	destinationRegister := aDRLoadArgument result name.

	self copyRegister: sourceRegister to: destinationRegister
]

{ #category : #visiting }
DRCogitStackToRegisterPrimitiveCodeGenerator >> visitPush: aDRPush [
	"If the backend has not LinkReg
		- then the return address is on TOS, we must push the value after it.
		- otherwise, just push.
	This check is staged, the info is at JIT time"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: (RBMessageNode
					  receiver:
					  (RBMessageNode
						   receiver: RBVariableNode selfNode
						   selector: #backend)
					  selector: #hasLinkRegister)
			 selector: #ifTrue:ifFalse:
			 arguments: {
					 (RBBlockNode body:
						  (RBSequenceNode statements: { (self buildPushFrom: aDRPush) })).

					 (RBBlockNode body: (RBSequenceNode statements: {
								   (RBMessageNode
									    receiver: RBVariableNode selfNode
									    selector: #MoveMw:r:R:
									    arguments: {
											    (RBLiteralValueNode value: 0).
											    self spRegVariable.
											    self tempRegVariable }).

								   (RBMessageNode
									    receiver: RBVariableNode selfNode
									    selector: #MoveR:Mw:r:
									    arguments: {
											    (aDRPush operand1 rtlPushArgumentExpressions: self)
												    first.
											    (RBLiteralValueNode value: 0).
											    self spRegVariable }).

								   (RBMessageNode
									    receiver: RBVariableNode selfNode
									    selector: 'PushR:'
									    arguments: { self tempRegVariable }) })) })
]
