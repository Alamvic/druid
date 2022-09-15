Extension { #name : #Collection }

{ #category : #'*Druid' }
Collection >> unique [
	
	self assert: self size = 1.
	^ self anyOne 
]
