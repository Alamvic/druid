Class {
	#name : 'DRLoadArgument',
	#superclass : 'DRTemporaryVariableInstruction',
	#instVars : [
		'argName'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadArgument >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoadArgument: self
]

{ #category : 'accessing' }
DRLoadArgument >> argName [

	^ argName
]

{ #category : 'accessing' }
DRLoadArgument >> argName: selector [

	argName := selector
]

{ #category : 'accessing' }
DRLoadArgument >> argNum [

	^ self operand1 value
]

{ #category : 'accessing' }
DRLoadArgument >> argumentName [

	^ 'arg' , self argNum asString
]

{ #category : 'visiting' }
DRLoadArgument >> isLoadArgument [

	^ true
]

{ #category : 'visiting' }
DRLoadArgument >> isMandatoryInstruction [

	^ true
]

{ #category : 'printing' }
DRLoadArgument >> opcode [
	
	^ 'LoadArg'
]

{ #category : 'SCCP' }
DRLoadArgument >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'accessing' }
DRLoadArgument >> typeWithAlreadySeen: _ [
	
	^ classType ifNil: [DRUndefinedType new]
]

{ #category : 'accessing' }
DRLoadArgument >> variableName [

	^ self argName
]
