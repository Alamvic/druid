Class {
	#name : #DruidStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'returnValue',
		'temporaryFrame'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidStackFrame >> initialize [

	super initialize.
	temporaryFrame := Dictionary new
]

{ #category : #accessing }
DruidStackFrame >> receiver [
	^ receiver
]

{ #category : #accessing }
DruidStackFrame >> receiver: aCogVMSimulatorLSB [ 
	receiver := aCogVMSimulatorLSB
]

{ #category : #accessing }
DruidStackFrame >> returnValue [
	
	^ returnValue ifNil: [ {DRNullValue new . nil } ]
]

{ #category : #accessing }
DruidStackFrame >> returnValue: anInteger [ 
	self haltIf: [ anInteger isKindOf: DRBasicBlock ].
	returnValue := anInteger
]

{ #category : #accessing }
DruidStackFrame >> temporaryAt: aString [
	
	^ temporaryFrame at: aString
]

{ #category : #accessing }
DruidStackFrame >> temporaryAt: aString put: aValue [ 
	
	"Store an intermediate interpretation value in a temporary variable"
	^ temporaryFrame at: aString put: aValue
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame [

	^ temporaryFrame
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame: aCollection [ 
	temporaryFrame := aCollection
]
