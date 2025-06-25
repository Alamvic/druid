Class {
	#name : 'DRInline',
	#superclass : 'DRControlFlowOptimisation',
	#instVars : [
		'inlineGenerator'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRInline >> doApply: cfg [

	cfg messageSends copy do: [ :messageSend | 
		self inline: messageSend ]
]

{ #category : 'inlining' }
DRInline >> generateInlineIRForMethod: classAndMethodToInline of: aMessageSend [

	| lastFrame methodClass methodToInline |
	"TODO: delegate generation to type?"
	methodClass := classAndMethodToInline key.
	(methodClass isKindOf: DRUndefinedType) ifTrue: [ "If we don't know the receiver's type, copy the message send"
		(DRMethod methodNode: aMessageSend originAST methodNode) asDRValue
			pushFrameIn: inlineGenerator
			receiver: aMessageSend receiver.
		^ inlineGenerator resolveMessageSend: aMessageSend originAST receiver: aMessageSend receiver arguments: aMessageSend arguments ].

	methodToInline := classAndMethodToInline value.
	lastFrame := inlineGenerator
		             interpretCode: (DRMethod methodNode: methodToInline)
		             receiver: aMessageSend operands first
		             arguments: aMessageSend operands allButFirst.

	inlineGenerator finishCodeInterpretation: lastFrame.

	^ lastFrame returnedValue
]

{ #category : 'inlining' }
DRInline >> generateInlineIRForMethods: methodsToInline of: aMessageSend [

	| returnValues lastBlock |

	returnValues := OrderedCollection new.
	methodsToInline allButLastDo: [ :classAndMethod |
		| jump |
		jump := inlineGenerator currentBasicBlock endInstruction:
			        (DRBranchIfCondition operands: {
					         (DRTypeOf typeClass: classAndMethod key).
					         aMessageSend receiver.
					         classAndMethod key asDRValue }).
		jump newTrueBranch: inlineGenerator newBasicBlock.
		returnValues add: (self generateInlineIRForMethod: classAndMethod of: aMessageSend).
		jump newFalseBranch: inlineGenerator newBasicBlock ].

	returnValues add: (self generateInlineIRForMethod: methodsToInline last of: aMessageSend).

	returnValues size = 1 ifTrue: [ ^ returnValues first ].

	lastBlock := inlineGenerator newBasicBlock.

	returnValues do: [ :returnValue |
		returnValue basicBlock jumpTo: lastBlock ].
	^ lastBlock addInstruction:
		  (aMessageSend instructionFactory phiWithVariables: returnValues)
]

{ #category : 'inlining' }
DRInline >> inline: messageSend [

	| methodsToInline inlinedMethodReturnValue innerDependencies |

	(self isIgnoredSelector: messageSend) 
		ifTrue: [ ^ self ].

	inlineGenerator := messageSend inlineGenerator.

	methodsToInline := self methodsToInlineOf: messageSend.
	methodsToInline ifEmpty: [ ^ self ].

	"Keep the the dependecies from inner blocks to remove the message as user"
	innerDependencies := messageSend innerDependencies.

	"Break message send basic block"
	messageSend basicBlock breakBy: messageSend.
	messageSend basicBlock predecessor jumpTo: inlineGenerator newFirstBasicBlock.

	"Generate inlining methods IR"
	inlineGenerator numberOfArguments:
		messageSend operands size - 1.
	inlinedMethodReturnValue := self generateInlineIRForMethods: methodsToInline of: messageSend.

	"Replace usages by inlines result"
	inlineGenerator currentBasicBlock jumpTo: messageSend basicBlock successor.
	messageSend replaceUsesBy: inlinedMethodReturnValue.
	(innerDependencies copyWithoutAll: messageSend innerDependencies) do: [ :i | i removeUser: messageSend ].

	"Remove message send block"
	messageSend basicBlock removeFromCFGAndDisconnectSuccessors.

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: inlineGenerator ir.
	DRDeadCodeElimination new applyTo: inlineGenerator ir.
	inlineGenerator ir validate
]

{ #category : 'inlining' }
DRInline >> isIgnoredSelector: messageSend [

	^ { #class. #instVarAt: . #and: . #or} includes: messageSend selector
]

{ #category : 'inlining' }
DRInline >> methodsToInlineOf: messageSend [
	"Returns a collection of associations: 
	    DRType (or #default) -> method AST
	"

	messageSend methodNode ifNotNil: [ "If a method node was set during interpretation then the message send is statically binding to them"
		^ { (#default -> messageSend methodNode) } ].

	^ inlineGenerator typeSystem methodsForMessage: messageSend
]
