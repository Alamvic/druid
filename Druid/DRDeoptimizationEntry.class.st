Class {
	#name : #DRDeoptimizationEntry,
	#superclass : #Object,
	#instVars : [
		'irInstruction',
		'frames',
		'pc'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #adding }
DRDeoptimizationEntry >> addFrameForMethod: aCompiledMethod savedPC: savedPC receiver: aReceiver arguments: arguments temporaries: temporaries stack: stackElements [ 

	frames add: 	(DRDeoptimizationFrameEntry new
				compiledMethod: aCompiledMethod;
				savedPC: savedPC;
				receiver: (aReceiver acceptVisitor: self);
				arguments: (arguments collect: [:e | e acceptVisitor: self ]);
				temporaries: (temporaries collect: [:e | e acceptVisitor: self ]);
				stack: (stackElements collect: [:e | e acceptVisitor: self ]);
				yourself)
]

{ #category : #deoptimization }
DRDeoptimizationEntry >> deoptimizeContext: originalContext [ 
	
	^ frames reverse inject: originalContext sender into: [ :sender :frame | |newContext|
		newContext := Context 
			sender: sender 
			receiver: (frame receiver readFromContext: originalContext)  
			method: frame compiledMethod 
			arguments: (frame arguments collect: [ :e | e readFromContext: originalContext ]) . 
		
		newContext pc: frame savedPC.
		
		frame temporaries do: [ :e | 1halt ].
		frame stack do: [ :e | 1halt ].
		newContext
		 ]
]

{ #category : #adding }
DRDeoptimizationEntry >> initialize [ 

	super initialize.
	frames := OrderedCollection new
]

{ #category : #accessing }
DRDeoptimizationEntry >> irInstruction: anIRSend [ 
	
	irInstruction := anIRSend 
]

{ #category : #accessing }
DRDeoptimizationEntry >> pc [
	^ pc
]

{ #category : #updating }
DRDeoptimizationEntry >> updatePCFrom: aDRBytecodeGenerator [ 

	pc := aDRBytecodeGenerator pcForInstruction: irInstruction
]

{ #category : #visiting }
DRDeoptimizationEntry >> visitCopy: aDRCopy [ 
	
	^ DRFromConstantValue new value: aDRCopy operand value
]

{ #category : #visiting }
DRDeoptimizationEntry >> visitInterpreterValue: aDRInterpreterValue [ 
	
	^ DRFromReceiver new
]

{ #category : #visiting }
DRDeoptimizationEntry >> visitLoadArgument: aDRLoadArgument [ 
	
	^ DRFromArgument new index: aDRLoadArgument argNum
]

{ #category : #visiting }
DRDeoptimizationEntry >> visitStoreTempVar: aDRStoreTemporaryVariable [ 
	
	^ DRFromTemporary new name: aDRStoreTemporaryVariable variableName
]

{ #category : #visiting }
DRDeoptimizationEntry >> visitTemporaryVariable: aDRLoadTemporaryVariable [ 
	
	^ DRFromTemporary new name: aDRLoadTemporaryVariable variableName
]
