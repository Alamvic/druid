Extension { #name : #Object }

{ #category : #'*Druid' }
Object >> asDRInterpreterValue [
	
	^ DRInterpreterValue value: self
]

{ #category : #'*Druid' }
Object >> asDRValue [
	
	^ DRConstantValue value: self
]

{ #category : #'*Druid' }
Object >> isJITCompileTimeExpression [
	
	^ true
]

{ #category : #'*Druid' }
Object >> sccpEquals: anInteger [

	^ self = anInteger
]
