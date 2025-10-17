Class {
	#name : 'DRSlangTypeSystem',
	#superclass : 'DRPragmaBasedTypeSystem',
	#category : 'Druid-Cogit-TypeSystem',
	#package : 'Druid-Cogit',
	#tag : 'TypeSystem'
}

{ #category : 'accessing' }
DRSlangTypeSystem >> slangTyper [
	
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

{ #category : 'private' }
DRSlangTypeSystem >> typeOf: aNode [

	^ aNode acceptVisitor: self
]

{ #category : 'visiting' }
DRSlangTypeSystem >> visitInstanceVariableNode: aRBVariableNode [ 
	
	| slangType isPointer |
	slangType := self slangTyper typeOfVariable: aRBVariableNode name.
	isPointer := self slangTyper isPointerCType: slangType.
	
	isPointer ifTrue: [ ^ DRPointerType new ].
	self halt: 'Unimplemented'
]
