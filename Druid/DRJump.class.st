Class {
	#name : #DRJump,
	#superclass : #DRInstruction,
	#instVars : [
		'target'
	],
	#category : #'Druid-Instructions'
}

{ #category : #printing }
DRJump >> mnemonic [ 

	^ 'JMP'
]

{ #category : #'as yet unclassified' }
DRJump >> operands [
		
	^ {}
]

{ #category : #accessing }
DRJump >> target: aDRLabel [ 
	target := aDRLabel
]
