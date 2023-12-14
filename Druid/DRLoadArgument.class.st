Class {
	#name : #DRLoadArgument,
	#superclass : #DRInstruction,
	#instVars : [
		'size',
		'argNum',
		'argName'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadArgument >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoadArgument: self
]

{ #category : #accessing }
DRLoadArgument >> argName [

	^ argName ifNil: [ 'arg' , argNum asString ]
]

{ #category : #accessing }
DRLoadArgument >> argName: selector [

	argName := selector
]

{ #category : #accessing }
DRLoadArgument >> argNum [

	^ argNum
]

{ #category : #accessing }
DRLoadArgument >> classType: aType [
	
	classType := aType
]

{ #category : #visiting }
DRLoadArgument >> isLoadArgument [

	^ true
]

{ #category : #visiting }
DRLoadArgument >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRLoadArgument >> opcode [
	
	^ 'LoadArgument', argNum asString
]

{ #category : #accessing }
DRLoadArgument >> operands: operandList [

	argNum := operandList first value.
	super operands: operandList allButFirst
]

{ #category : #SCCP }
DRLoadArgument >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #accessing }
DRLoadArgument >> type [

	^ DRSignedIntegerType size: 8 "Word size"
]
