Class {
	#name : #DRPragmaBasedTypeSystem,
	#superclass : #DRTypeSystem,
	#category : #'Druid-IR-Types'
}

{ #category : #inference }
DRPragmaBasedTypeSystem >> receiverTypes: aDRMessageSend [

	| msgAST receiverTypeNames |
	msgAST := aDRMessageSend originAST.
	receiverTypeNames := (msgAST methodNode allPragmasNamed: #var:type:)
		                     select: [ :pragma |
		                     pragma arguments first value = msgAST receiver name ]
		                     thenCollect: [ :pragma |
		                     pragma arguments last value ].

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
