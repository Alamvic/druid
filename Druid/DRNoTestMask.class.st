Class {
	#name : 'DRNoTestMask',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRNoTestMask >> isNoTestMask [
	
	^ true
]

{ #category : 'testing' }
DRNoTestMask >> opcode [
	
	^ 'NoTest'
]
