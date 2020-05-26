Class {
	#name : #DRPush,
	#superclass : #DROneArgumentInstruction,
	#category : #'Druid-Instructions'
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
