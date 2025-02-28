Class {
	#name : 'DRClosureCreation',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRClosureCreation >> acceptVisitor: aVisitor [

	^ aVisitor visitClosureCreation: self
]

{ #category : 'testing' }
DRClosureCreation >> isClosureCreation [

	^ true
]

{ #category : 'testing' }
DRClosureCreation >> isMandatoryInstruction [

	^ true
]

{ #category : 'testing' }
DRClosureCreation >> needs2ACConversion [

	^ false
]

{ #category : 'SCCP' }
DRClosureCreation >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
