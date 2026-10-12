Class {
	#name : 'DRCGLoadArgument',
	#superclass : 'DRCGTemporaryVariableInstruction',
	#instVars : [
		'argName'
	],
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRCGLoadArgument >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoadArgument: self
]

{ #category : 'accessing' }
DRCGLoadArgument >> argName [

	^ argName
]

{ #category : 'accessing' }
DRCGLoadArgument >> argName: selector [

	argName := selector
]

{ #category : 'accessing' }
DRCGLoadArgument >> argNum [

	^ self operand1 value
]

{ #category : 'accessing' }
DRCGLoadArgument >> argumentName [

	^ 'arg' , self argNum asString
]

{ #category : 'visiting' }
DRCGLoadArgument >> isLoadArgument [

	^ true
]

{ #category : 'visiting' }
DRCGLoadArgument >> isMandatoryInstruction [

	^ true
]

{ #category : 'printing' }
DRCGLoadArgument >> opcode [
	
	^ 'LoadArg'
]

{ #category : 'SCCP' }
DRCGLoadArgument >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'accessing' }
DRCGLoadArgument >> typeWithAlreadySeen: aCollection [
	
	^ classType ifNil: [DRUndefinedType new]
]

{ #category : 'accessing' }
DRCGLoadArgument >> variableName [

	^ self argName
]
