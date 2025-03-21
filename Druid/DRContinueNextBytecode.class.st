Class {
	#name : 'DRContinueNextBytecode',
	#superclass : 'DRInstruction',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRContinueNextBytecode >> acceptVisitor: aDRCogitCanonicaliser [
	
	aDRCogitCanonicaliser visitContinueNextBytecode: self
]

{ #category : 'visiting' }
DRContinueNextBytecode >> isMandatoryInstruction [

	^ true
]

{ #category : 'printing' }
DRContinueNextBytecode >> opcode [
	
	^ 'Next Bytecode'
]

{ #category : 'SCCP' }
DRContinueNextBytecode >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
