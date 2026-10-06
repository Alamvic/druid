Class {
	#name : 'DRReturn',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRReturn >> acceptVisitor: aVisitor [
	
	^ aVisitor visitReturn: self
]

{ #category : 'initialization' }
DRReturn >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : 'visiting' }
DRReturn >> isMandatoryInstruction [

	^ true
]

{ #category : 'testing' }
DRReturn >> isReturn [

	^ true
]

{ #category : 'printing' }
DRReturn >> opcode [
	
	^ 'Return'
]

{ #category : 'SCCP' }
DRReturn >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : 'accessing' }
DRReturn >> scope [
	^ scope
]

{ #category : 'accessing' }
DRReturn >> scope: aDRScope [ 

	scope:= aDRScope 
]

{ #category : 'testing' }
DRReturn >> supportConstantOperand [

	^ false
]
