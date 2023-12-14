Class {
	#name : #DRCogitStackToRegisterPrimitiveCodeGenerator,
	#superclass : #DRCogitPrimitiveCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitStackToRegisterPrimitiveCodeGenerator >> visitLoadArgument: aDRLoadArgument [
	"Just copy the Arg[X]Reg to the result"

	| sourceRegister destinationRegister |
	sourceRegister := 'Arg' , aDRLoadArgument argNum asString , 'Reg'.
	destinationRegister := aDRLoadArgument result name.

	self copyRegister: sourceRegister to: destinationRegister
]
