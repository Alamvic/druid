Class {
	#name : #DRLoad,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-instructions'
}

{ #category : #'instance-creation' }
DRLoad class >> basePointerRegister [
	
	^ self value: #BP
]

{ #category : #testing }
DRLoad >> isConstantInteger [
	
	^ value isInteger
]
