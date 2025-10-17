Class {
	#name : 'DRAnnotateBytecode',
	#superclass : 'DRInstruction',
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRAnnotateBytecode >> acceptVisitor: aDRVisitor [ 
	
	^ aDRVisitor visitAnnnotateBytecode: self
]

{ #category : 'testing' }
DRAnnotateBytecode >> isAnnotateBytecode [

	^ true
]

{ #category : 'visiting' }
DRAnnotateBytecode >> isMandatoryInstruction [

	^ true
]

{ #category : 'SCCP' }
DRAnnotateBytecode >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom
]
