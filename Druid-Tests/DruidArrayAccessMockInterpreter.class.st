Class {
	#name : #DruidArrayAccessMockInterpreter,
	#superclass : #Object,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #testing }
DruidArrayAccessMockInterpreter class >> isDruidTestInterpreter [
	
	^ true
]

{ #category : #accessing }
DruidArrayAccessMockInterpreter >> from: from to: to do: aBlock [

	| nextValue |
	nextValue := from.
	[nextValue <= to]
		whileTrue:
			[aBlock value: nextValue.
			nextValue := nextValue + 1]
]

{ #category : #accessing }
DruidArrayAccessMockInterpreter >> methodIteratingArrayUsingAt: anArray [

	<var: #anArray type: #Array>
	
	| r |
	
	r := 0.
	self from: 1 to: anArray size do: [ :i  | r := r + (anArray at: i) ].
	
	^ r
]

{ #category : #accessing }
DruidArrayAccessMockInterpreter >> methodWithArrayUsingAt: anArray [

	<var: #anArray type: #Array>
	
	^ anArray at: 1

]

{ #category : #accessing }
DruidArrayAccessMockInterpreter >> methodWithUsingMultipleReturns [
	
	^ self multipleReturns: false
]

{ #category : #accessing }
DruidArrayAccessMockInterpreter >> multipleReturns: aValue [ 

	aValue ifTrue: [ ^ 43 ].
	
	^ false	
]
