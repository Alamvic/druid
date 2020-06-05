Class {
	#name : #DRPop,
	#superclass : #DRInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #printing }
DRPop >> mnemonic [
	
	^ 'POP'
]

{ #category : #'as yet unclassified' }
DRPop >> operands [
	
	^ {result}
]

{ #category : #accessing }
DRPop >> registers [
	
	^ result registers
]
