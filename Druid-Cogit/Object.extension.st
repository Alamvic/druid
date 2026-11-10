Extension { #name : 'Object' }

{ #category : '*Druid-Cogit' }
Object >> asDRInterpreterValue [

	self flag: #cogitSpecific.
	^ DRInterpreterValue value: self
]
