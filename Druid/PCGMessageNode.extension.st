Extension { #name : #PCGMessageNode }

{ #category : #'*Druid' }
PCGMessageNode classSide >> receiver: receiver selector: selector argument: argument [
	^ self new
		receiver: receiver;
		selector: selector;
		arguments: (OrderedCollection with: argument);
		yourself
]
