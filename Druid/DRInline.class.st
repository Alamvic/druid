Class {
	#name : 'DRInline',
	#superclass : 'DRControlFlowOptimisation',
	#instVars : [
		'inlineGenerator'
	],
	#classInstVars : [
		'enabled'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRInline class >> enabled [

	^ enabled
]

{ #category : 'accessing' }
DRInline class >> enabled: aBoolean [ 

	enabled := aBoolean 
]

{ #category : 'system startup' }
DRInline class >> initialize [ 

	enabled := true
]

{ #category : 'accessing' }
DRInline >> doApply: cfg [

	self class enabled ifFalse: [ ^ self ].
	
	self inlineDomainSends: cfg.
	self inlineControlFlowSends: cfg

]

{ #category : 'inlining' }
DRInline >> generateInlineIRForMethod: classAndMethodToInline of: aMessageSend [

	| lastFrame methodType methodNode |
	methodType := classAndMethodToInline key.

	"Base on type inference, get the inlined or current method"
	methodNode := methodType isUndefined
		              ifTrue: [ aMessageSend originAST methodNode ]
		              ifFalse: [ classAndMethodToInline value ].

	"Push new frame"
	(DRMethod methodNode: methodNode)
		pushFrameIn: inlineGenerator
		receiver: aMessageSend receiver.

	"If we don't know the receiver's type, copy the message send"
	methodType isUndefined ifTrue: [
			^ inlineGenerator
				  resolveMessageSend: aMessageSend originAST
				  receiver: aMessageSend receiver
				  arguments: aMessageSend arguments ].


	"Else, interpret the inlined code"
	lastFrame := inlineGenerator
		             interpretInlinedCode: (DRMethod methodNode: methodNode)
		             receiver: aMessageSend receiver
		             arguments: aMessageSend arguments.
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

	"Last case of the guard (if any)"
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

	| methodsToInline inlinedMethodReturnValue |
	inlineGenerator := messageSend inlineGenerator.

	methodsToInline := self methodsToInlineOf: messageSend.
	methodsToInline ifEmpty: [ ^ self ].

	"Break message send basic block"
	messageSend basicBlock breakBy: messageSend.
	messageSend basicBlock predecessor jumpTo:
		inlineGenerator newFirstBasicBlock.

	"Generate inlining methods IR"
	inlineGenerator numberOfArguments: messageSend operands size - 1.
	inlinedMethodReturnValue := self
		                            generateInlineIRForMethods:
		                            methodsToInline
		                            of: messageSend.

	"Replace usages by inlines result"
	inlineGenerator currentBasicBlock jumpTo:
		messageSend basicBlock successor.
	messageSend replaceUsesBy: inlinedMethodReturnValue.

	"Remove message send block"
	messageSend basicBlock removeFromCFGAndDisconnectSuccessors.

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: inlineGenerator ir.
	DRDeadCodeElimination new applyTo: inlineGenerator ir.
	inlineGenerator ir validate
]

{ #category : 'as yet unclassified' }
DRInline >> inlineControlFlowSends: cfg [

	| sends |
	sends := cfg messageSends select: [ :send | send hasMissingInlines ].
	sends := sends select: [ :send | self isControlFlowMessage: send ].
	sends do: [ :messageSend | self inline: messageSend ]
]

{ #category : 'as yet unclassified' }
DRInline >> inlineDomainSends: cfg [

	| sends |
	sends := cfg messageSends select: [ :send | send hasMissingInlines ].
	sends := sends reject: [ :send | self isControlFlowMessage: send ].
	sends sort: #blockOperandCount descending.
	(sends takeFirst: 3) do: [ :messageSend | self inline: messageSend ]
]

{ #category : 'testing' }
DRInline >> isControlFlowMessage: aDRMessageSend [

	^ #( ifTrue: ifFalse: whileTrue: whileFalse: value: ) anySatisfy: [
		  :sel | aDRMessageSend selector includesSubstring: sel ]
]

{ #category : 'inlining' }
DRInline >> methodsToInlineOf: messageSend [
	"Returns a collection of associations: 
	    DRType (or #default) -> method AST
	"

	messageSend methodNode ifNotNil: [ "If a method node was set during interpretation then the message send is statically binding to them"
		^ { (#default -> messageSend methodNode) } ].

	^ messageSend inliningMethods
]
