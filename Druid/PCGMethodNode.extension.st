Extension { #name : #PCGMethodNode }

{ #category : #'*Druid' }
PCGMethodNode >> returnNode [
	"Answer the receiver's <PCGReturnNode>"

	^ self statements detect: #isReturn
]
