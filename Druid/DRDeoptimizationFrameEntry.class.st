Class {
	#name : #DRDeoptimizationFrameEntry,
	#superclass : #Object,
	#instVars : [
		'compiledMethod',
		'savedPC',
		'arguments',
		'temporaries',
		'stack',
		'receiver'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #accessing }
DRDeoptimizationFrameEntry >> arguments [
	^ arguments
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> arguments: aCollection [ 
	
	arguments := aCollection 
	
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> compiledMethod [
	^ compiledMethod
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> compiledMethod: aCompiledMethod [ 
	
	compiledMethod := aCompiledMethod
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> receiver [
	^ receiver
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> receiver: aDRInterpreterValue [ 
	receiver := aDRInterpreterValue
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> savedPC [
	^ savedPC
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> savedPC: anInteger [ 
	
	savedPC := anInteger 

]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> stack [
	^ stack
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> stack: aCollection [ 
	
	stack := aCollection 
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> temporaries [
	^ temporaries
]

{ #category : #accessing }
DRDeoptimizationFrameEntry >> temporaries: aCollection [ 
	
	temporaries := aCollection 

]

{ #category : #updating }
DRDeoptimizationFrameEntry >> updateInformationFrom: aDRBytecodeGenerator [ 
	
	receiver updateInformationFrom: aDRBytecodeGenerator.
	temporaries do: [ :e | e updateInformationFrom: aDRBytecodeGenerator ].
	arguments do: [ :e | e updateInformationFrom: aDRBytecodeGenerator ].
	stack do: [ :e | e updateInformationFrom: aDRBytecodeGenerator ].
]
