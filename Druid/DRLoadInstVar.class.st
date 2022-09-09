Class {
	#name : #DRLoadInstVar,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #printing }
DRLoadInstVar >> opcode [
	
	^ 'LoadInstVar'
]

{ #category : #types }
DRLoadInstVar >> type [

	^ DRUndefinedType new
]
