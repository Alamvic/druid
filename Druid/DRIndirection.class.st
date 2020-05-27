Class {
	#name : #DRIndirection,
	#superclass : #DRValue,
	#instVars : [
		'valueInstruction',
		'value'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRIndirection class >> value: aValue [

	^ self new
		value: aValue;
		yourself
]

{ #category : #testing }
DRIndirection >> isConstant [
	
	^ value isConstant
]

{ #category : #testing }
DRIndirection >> isPhysical [
	
	^ value isPhysical
]

{ #category : #accessing }
DRIndirection >> printOn: aStream [

	^ aStream nextPutAll: '(', self value asString, ')'
]

{ #category : #accessing }
DRIndirection >> registers [
	
	^ value registers
]

{ #category : #accessing }
DRIndirection >> result [

	^ self
]

{ #category : #'register allocation' }
DRIndirection >> updateLogicalRegisters: mappings [

	value isConstant
		ifFalse: [ value isPhysical
				ifFalse: [ value := mappings at: value ] ].
			
	^ self
]

{ #category : #accessing }
DRIndirection >> value [
	^ value
]

{ #category : #accessing }
DRIndirection >> value: anInstruction [
	valueInstruction := anInstruction.
	value := valueInstruction result
]
