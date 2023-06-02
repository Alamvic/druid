Class {
	#name : #DRAssignation,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRAssignation >> acceptVisitor: aVisitor [
	
	^ aVisitor visitAssignation: self
]

{ #category : #visiting }
DRAssignation >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRAssignation >> opcode [

	^ 'ASSIGN'
]

{ #category : #SCCP }
DRAssignation >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]
