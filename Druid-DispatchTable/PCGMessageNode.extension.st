Extension { #name : #PCGMessageNode }

{ #category : #'*Druid-DispatchTable' }
PCGMessageNode >> arguments: anObject [

	arguments := anObject
]

{ #category : #'*Druid-DispatchTable' }
PCGMessageNode class >> receiver: receiver selector: selector argument: argument [
	^ self new
		receiver: receiver;
		selector: selector;
		arguments: (OrderedCollection with: argument);
		yourself
]
