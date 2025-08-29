Class {
	#name : 'DRArrayNode',
	#superclass : 'DRInstruction',
	#instVars : [
		'argName'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRArrayNode >> acceptVisitor: aVisitor [
	
	^ aVisitor visitArrayNode: self
]

{ #category : 'visiting' }
DRArrayNode >> opcode [
	
	^ 'Array'
]

{ #category : 'visiting' }
DRArrayNode >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]
