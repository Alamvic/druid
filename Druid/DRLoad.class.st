Class {
	#name : #DRLoad,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-instructions'
}

{ #category : #'instance-creation' }
DRLoad class >> basePointerRegister [
	
	^ self value: DRPhisicalRegister basePointer
]

{ #category : #testing }
DRLoad >> isConstantInteger [
	
	^ value isInteger
]

{ #category : #printing }
DRLoad >> mnemonic [
	
	^ 'LOAD'
]

{ #category : #accessing }
DRLoad >> result [
	
	^ 't1'
]
