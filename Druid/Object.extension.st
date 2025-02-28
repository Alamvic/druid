Extension { #name : 'Object' }

{ #category : '*Druid' }
Object >> asDRInterpreterValue [
	
	^ DRInterpreterValue value: self
]

{ #category : '*Druid' }
Object >> asDRValue [
	
	^ DRConstantValue value: self
]

{ #category : '*Druid' }
Object >> isBooleanValue [

	^ false
]

{ #category : '*Druid' }
Object >> isStaged [
	
	^ true
]

{ #category : '*Druid' }
Object >> sccpEquals: anInteger [

	^ self = anInteger
]

{ #category : '*Druid' }
Object >> sccpLessThanOrEquals: anInteger [

	^ self <= anInteger
]
