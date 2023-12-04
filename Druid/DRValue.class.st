Class {
	#name : #DRValue,
	#superclass : #Object,
	#category : #'Druid-IR'
}

{ #category : #sccp }
DRValue class >> tryParseOperandForSCCP: aDRValue [

	^ aDRValue
]

{ #category : #visiting }
DRValue >> acceptVisitor: aVisitor [

	^ self subclassResponsibility
]

{ #category : #building }
DRValue >> addToBlock: aDRBasicBlock [ 
	
	aDRBasicBlock addInstruction: self
]

{ #category : #dependencies }
DRValue >> addToDependenciesList: alreadyComputedDependencies [
	
	(alreadyComputedDependencies includes: self)
		ifTrue: [ ^ alreadyComputedDependencies ].
	
	alreadyComputedDependencies add: self.
	self dependencies do: [ :e |
		e addToDependenciesList: alreadyComputedDependencies ].

]

{ #category : #adding }
DRValue >> addToUsersList: alreadyComputedUsers [

	(alreadyComputedUsers includes: self) ifTrue: [
		^ alreadyComputedUsers ].

	alreadyComputedUsers add: self.
	self users do: [ :e | e addToUsersList: alreadyComputedUsers ]
]

{ #category : #'users-definitions' }
DRValue >> addUser: anInstruction [
	
	"Nothing"
]

{ #category : #convering }
DRValue >> asDRValue [
	
	^ self
]

{ #category : #accessing }
DRValue >> asDependencyList [

	"Return myself as a list of dependencies, independently of MY dependencies"	
	^ { self }
]

{ #category : #memory }
DRValue >> asMemoryAddress [

	^ DRAbsoluteMemoryAddress new
		  expression: self;
		  yourself
]

{ #category : #copying }
DRValue >> copyToVariableIn: aDRIRGenerator inNode: aNode [
	
	^ aDRIRGenerator
		addInstructionFrom: aNode
		instructionKind: DRCopy
		operands: { self }.
]

{ #category : #accessing }
DRValue >> dependencies [
	
	^ self operands flatCollect: [ :op | op asDependencyList ]
]

{ #category : #testing }
DRValue >> hasDependency: anInstruction [

	^ false
]

{ #category : #testing }
DRValue >> hasDependencyRecursively: anInstruction [

	^ self hasDependencyRecursively: anInstruction visited: {  }
]

{ #category : #testing }
DRValue >> hasDependencyRecursively: anInstruction visited: alreadyVisited [

	(alreadyVisited includes: self) ifTrue: [ ^ false ].
	^ (self hasDependency: anInstruction) or: [ 
		  self operands anySatisfy: [ :o | 
			  o hasDependencyRecursively: anInstruction visited: { self } , alreadyVisited ] ]
]

{ #category : #accessing }
DRValue >> hasId: id [

	^ false
]

{ #category : #testing }
DRValue >> hasUser: anInstruction [
	
	"For values non-instructions we can say this always holds"
	^ true
]

{ #category : #testing }
DRValue >> isAdd [
	
	^ false
]

{ #category : #testing }
DRValue >> isAnnotateBytecode [

	^ false
]

{ #category : #testing }
DRValue >> isComparison [

	^ false
]

{ #category : #testing }
DRValue >> isComplexCondition [

	^ false
]

{ #category : #testing }
DRValue >> isConstant [
	
	^ false
]

{ #category : #testing }
DRValue >> isConstraintConstant [

	^ self isConstant 
]

{ #category : #testing }
DRValue >> isCopy [ 
	
	^ false
]

{ #category : #testing }
DRValue >> isDRBlockClosure [
	
	^ false
]

{ #category : #testing }
DRValue >> isFrameReturn [

	^ false
]

{ #category : #testing }
DRValue >> isGlobal [

	^ self isNullValue or: [ self isPhysicalRegister or: [ self isConstant ] ]
]

{ #category : #testing }
DRValue >> isInstruction [

	^ false
]

{ #category : #testing }
DRValue >> isInvariantFrom: loop [

	"A value is loop invariant with respect to a given loop if it can be moved outside the body of the loop without affecting the semantics of the program."

	self subclassResponsibility
]

{ #category : #testing }
DRValue >> isJITCompileTimeExpression [
	
	^ false
]

{ #category : #testing }
DRValue >> isJITCompileTimeExpression: aString [ 
	
	^ false
]

{ #category : #accessing }
DRValue >> isJump [ 

	^ false
]

{ #category : #testing }
DRValue >> isLoad [
	
	^ false
]

{ #category : #testing }
DRValue >> isLoadArgument [

	^ false
]

{ #category : #testing }
DRValue >> isLoadFramePointer [
	
	^ false
]

{ #category : #visiting }
DRValue >> isMandatoryInstruction [

	^ false
]

{ #category : #testing }
DRValue >> isMemoryAddress [
	
	^ false
]

{ #category : #testing }
DRValue >> isNoResult [
	
	^ false
]

{ #category : #accessing }
DRValue >> isNoop [ 
	
	^ false
]

{ #category : #visiting }
DRValue >> isNullValue [

	^ false
]

{ #category : #testing }
DRValue >> isObjectReferenceValue [

	^ false
]

{ #category : #testing }
DRValue >> isPhiFunction [

	^ false
]

{ #category : #testing }
DRValue >> isPhysicalRegister [

	^ false
]

{ #category : #testing }
DRValue >> isPush [

	^ false
]

{ #category : #testing }
DRValue >> isRegister [

	^ false
]

{ #category : #testing }
DRValue >> isReturn [

	^ false
]

{ #category : #testing }
DRValue >> isSetConditionCode [
	
	^ false
]

{ #category : #testing }
DRValue >> isStackInstruction [

	^ false
]

{ #category : #visiting }
DRValue >> isStore [

	^ false 
]

{ #category : #testing }
DRValue >> isStoreTemporaryVariable [

	^ false
]

{ #category : #testing }
DRValue >> isSubtract [
	
	^ false
]

{ #category : #testing }
DRValue >> isValue [

	^ true
]

{ #category : #testing }
DRValue >> isVirtualRegister [
	
	^ false
]

{ #category : #joining }
DRValue >> joinNonNull: anotherValue onIRGenerator: aBuilder from: originBlocks [

	^ (DRPhiFunction joinedVariables: { anotherValue. self })
		result: aBuilder allocateTemporaryRegister;
		addToBlock: aBuilder currentBasicBlock;
		origin: thisContext stack
]

{ #category : #joining }
DRValue >> joinValue: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	self == anotherValue ifTrue: [ ^ self ].
	^ anotherValue joinNonNull: self onIRGenerator: anIRGenerator from: originBlocks
]

{ #category : #accessing }
DRValue >> label [
	
	^ self name
]

{ #category : #interpretation }
DRValue >> lookupClass [
	
	^ DRSignedIntegerType
]

{ #category : #accessing }
DRValue >> operands [
	
	^ #()
]

{ #category : #printing }
DRValue >> prettyPrint [

	^ self value asString
]

{ #category : #printing }
DRValue >> printResult [

	^ self value asString
]

{ #category : #'users-definitions' }
DRValue >> removeUser: anInstruction [
	
	"Nothing"
]

{ #category : #'users-definitions' }
DRValue >> replaceUsesBy: anotherInstruction [
	
	"Nothing"
]

{ #category : #accessing }
DRValue >> result [

	^ self
]

{ #category : #'cogit-generation' }
DRValue >> rtlOperandQualifier [

	self subclassResponsibility
]

{ #category : #'cogit-generation' }
DRValue >> rtlOperandQualifierForInstruction: instruction [

	^ self rtlOperandQualifier
]

{ #category : #cogit }
DRValue >> rtlPushArgumentExpressions: generator [
	
	^ { RBVariableNode named: self name }
]

{ #category : #cogit }
DRValue >> rtlPushOperandQualifierForInstruction: anInstruction [
	
	^ 'Constant:'
]

{ #category : #'constant folding' }
DRValue >> simpleConstantFold [
	
	^ self
]

{ #category : #transforming }
DRValue >> simplify [
	
	^ self
]

{ #category : #accessing }
DRValue >> targets [

	^ #()
]

{ #category : #types }
DRValue >> typeWithAlreadySeen: aCollection [ 
	
	^ self type
]

{ #category : #accessing }
DRValue >> users [
	
	^ #()
]

{ #category : #dependencies }
DRValue >> withAllDependencies [
	
	| dependencies |
	dependencies := OrderedCollection new.
	self addToDependenciesList: dependencies.
	^ dependencies
]

{ #category : #accessing }
DRValue >> withAllUsers [

	| allUsers |
	allUsers := OrderedCollection new.
	self addToUsersList: allUsers.
	^ allUsers
]
