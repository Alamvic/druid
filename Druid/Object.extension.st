Extension { #name : #Object }

{ #category : #'*Druid' }
Object >> asDRInterpreterValue [
	
	^ DRInterpreterValue value: self
]

{ #category : #'*Druid' }
Object >> asDRValue [
	
	^ DRConstantValue value: self
]
