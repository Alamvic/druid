Class {
	#name : #DRPragmaBasedTypeSystem,
	#superclass : #DRTypeSystem,
	#instVars : [
		'slang'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #'as yet unclassified' }
DRPragmaBasedTypeSystem >> extractTypeOfNode: aRBNode from: msgAST [

	aRBNode isLiteralNode ifTrue: [ ^ { aRBNode value class name } ].

	^ (msgAST methodNode allPragmasNamed: #var:type:)
		                     select: [ :pragma |
		                     pragma arguments first value = msgAST receiver name ]
		                     thenCollect: [ :pragma |
		                     pragma arguments last value ].
]

{ #category : #API }
DRPragmaBasedTypeSystem >> receiverTypes: aDRMessageSend [

	| msgAST receiverTypeNames |
	msgAST := aDRMessageSend originAST.
	receiverTypeNames := self extractTypeOfNode: msgAST receiver from: msgAST.

	"If there are not pragma, try to guess from IR"
	receiverTypeNames ifEmpty: [ ^ { aDRMessageSend receiverType } ].

	"Unknown should always be the last type, as it's used as a fallback when inlining"
	receiverTypeNames detect: [ :type | type = #Unknown ] ifFound: [
		receiverTypeNames remove: #Unknown.
		receiverTypeNames add: #Unknown ].

	^ receiverTypeNames collect: [ :typeName |
		  typeName = #Unknown
			  ifTrue: [ DRUndefinedType new ]
			  ifFalse: [
			  DRClassType for: (self class environment at: typeName) ] ]
]

{ #category : #accessing }
DRPragmaBasedTypeSystem >> slangTyper [
	
	slang ifNil: [ | codeGenerator |
		codeGenerator := MLVMCCodeGenerator new.
		codeGenerator vmMaker: VMMaker new.
		codeGenerator vmMaker vmmakerConfiguration: VMMakerConfiguration.
		codeGenerator vmClass: CoInterpreter.
		codeGenerator addClass: StackInterpreter.
		codeGenerator addClass: CoInterpreter.
		slang := codeGenerator.
	].
	^ slang
]

{ #category : #private }
DRPragmaBasedTypeSystem >> typeOf: aNode [

	^ aNode acceptVisitor: self
]

{ #category : #visiting }
DRPragmaBasedTypeSystem >> visitInstanceVariableNode: aRBVariableNode [ 
	
	| slangType isPointer |
	slangType := self slangTyper typeOfVariable: aRBVariableNode name.
	isPointer := self slangTyper isPointerCType: slangType.
	
	isPointer ifTrue: [ ^ DRPointerType new ].
	self halt: 'Unimplemented'
]
