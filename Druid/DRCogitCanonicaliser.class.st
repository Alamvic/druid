Class {
	#name : #DRCogitCanonicaliser,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitCanonicaliser >> applyTo: cfg [

	cfg instructions copy do: [ :e | e acceptVisitor: self ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitAddition: aDRAdd [

	(aDRAdd operands anySatisfy: [ :op | op = 0 asDRValue ]) ifTrue: [ 
		^ aDRAdd replaceBy: (DRCopy
				   operands: (aDRAdd operands select: [ :e | e ~= 0 asDRValue ])
				   result: aDRAdd result) ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitAnd: aDRBitAnd [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCopy: aDRCopy [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLeftShift: aDRLeftShift [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoad: aDRLoad [ 
	
	| address addressOperands |
	address := aDRLoad operand1.
	address isAdd ifFalse: [ ^ self ].
	
	addressOperands := address operands.
	(addressOperands first isLoadFramePointer
		and: [ addressOperands second isJITCompileTimeExpression: 'FrameReceiverOffset' ])
			ifTrue: [ ^ aDRLoad replaceBy: (DRLoadReceiver result: aDRLoad result) ].
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadReceiver: aDRLoadReceiver [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitNoop: aDRNoop [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPush: aDRPush [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitStore: aDRStore [ 
	
	"Nothing"
]
