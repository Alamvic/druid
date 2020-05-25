Class {
	#name : #DRPush,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-instructions'
}

{ #category : #printing }
DRPush >> mnemonic [
	
	^ 'PUSH'
]

{ #category : #accessing }
DRPush >> result [
	"no result"
	^ ''
]
