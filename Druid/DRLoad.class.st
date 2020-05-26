Class {
	#name : #DRLoad,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #'instance-creation' }
DRLoad class >> basePointerRegister [
	
	^ self value: DRPhisicalRegister basePointer
]

{ #category : #printing }
DRLoad >> mnemonic [
	
	^ 'LOAD'
]

{ #category : #accessing }
DRLoad >> result [
	
	^ 't1'
]
