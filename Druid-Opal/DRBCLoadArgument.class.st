Class {
	#name : 'DRBCLoadArgument',
	#superclass : 'DRBCTemporaryVariableInstruction',
	#instVars : [
		'argName'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBCLoadArgument >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoadArgument: self
]

{ #category : 'accessing' }
DRBCLoadArgument >> argName [

	^ argName
]

{ #category : 'accessing' }
DRBCLoadArgument >> argName: selector [

	argName := selector
]

{ #category : 'accessing' }
DRBCLoadArgument >> argNum [

	^ variableName
]

{ #category : 'accessing' }
DRBCLoadArgument >> argumentName [

	^ 'arg' , self argNum asString
]

{ #category : 'visiting' }
DRBCLoadArgument >> isLoadArgument [

	^ true
]

{ #category : 'visiting' }
DRBCLoadArgument >> isMandatoryInstruction [

	^ true
]

{ #category : 'printing' }
DRBCLoadArgument >> opcode [
	
	^ 'LoadArg'
]

{ #category : 'SCCP' }
DRBCLoadArgument >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'accessing' }
DRBCLoadArgument >> typeWithAlreadySeen: aCollection [
	
	^ classType ifNil: [DRUndefinedType new]
]

{ #category : 'accessing' }
DRBCLoadArgument >> variableName [

	^ self argName
]
