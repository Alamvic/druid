Class {
	#name : 'DRLoadArgument',
	#superclass : 'DRInstruction',
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

	^ argName ifNil: [ 'arg' , self argNum asString ]
]

{ #category : 'accessing' }
DRLoadArgument >> argName: selector [

	argName := selector
]

{ #category : 'accessing' }
DRLoadArgument >> argNum [

	^ self operand1 value
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
DRLoadArgument >> type [
	
	^ classType ifNil: [DRUndefinedType new]
]
