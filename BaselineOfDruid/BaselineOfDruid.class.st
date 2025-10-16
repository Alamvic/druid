Class {
	#name : 'BaselineOfDruid',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDruid',
	#package : 'BaselineOfDruid'
}

{ #category : 'baselines' }
BaselineOfDruid >> baseline: spec [
  <baseline>
  spec
    for: #(common)
    do: [
		spec
			baseline: 'PharoVM'
			with: [ spec
				className: 'BaselineOfVMMaker';
				repository: 'github://pharo-project/opensmalltalk-vm:druid' ].

		spec
			baseline: 'AIGraphAlgorithms'
			with: [ spec
				className: 'BaselineOfAIGraphAlgorithms';
				repository: 'github://pharo-ai/graph-algorithms:master' ].
		
		spec
			baseline: 'MachineArithmetic'
			with: [ spec
				className: 'BaselineOfMachineArithmetic';
				repository: 'github://shingarov/MachineArithmetic:pure-z3';
				loads: #(Z3Proper) ].

		spec
			package: 'Druid' with: [ spec requires: #('AIGraphAlgorithms' 'MachineArithmetic') ];
			package: 'Druid-Cogit' with: [ spec requires: #('Druid' 'PharoVM') ];
			package: 'Druid-Opal' with: [ spec requires: 'Druid' ];
			package: 'Druid-Tests' with: [ spec requires: #('Druid' 'Druid-Cogit' 'Druid-Opal') ];
			package: 'Druid-UI' with: [ spec requires: #('Druid' 'Druid-Tests') ].

		spec
			group: 'Core' with: #('Druid');
			group: 'Cogit' with: #('Druid-Cogit');
			group: 'Opal' with: #('Druid-Opal');
			group: 'Full' with: #('Core' 'Cogit' 'Opal');
			group: 'Tests' with: #('Druid-Tests');
			group: 'UI' with: #('Druid-UI');
			group: 'default' with: #('Full' 'Tests' 'UI') ]
]

{ #category : 'doits' }
BaselineOfDruid >> changeRepositoryType [
	| repos newRepo |
	(Smalltalk hasClassNamed: #MCGitRepository) ifFalse: [ ^ self ].
	
	repos := MCRepositoryGroup default repositories
		select: [ :r | (r description includesSubstring: 'LibGit:master') or: [ 
			r description includesSubstring: 'LibGit/master' ] ].
	repos ifEmpty: [  ^ self ].
	
	'github-cache/theseion' asFileReference deleteAll.
	newRepo := (Smalltalk at: #MCGitRepository) new
		repoUrl: 'git@github.com:theseion/LibGit';
		ensureDirectory;
		yourself.
		
	MCRepositoryGroup allInstances
		select: [ :group | group repositories includesAnyOf: repos ]
		thenDo: [ :group | 
			repos do: [ :repo | group removeRepository: repo ].
			group addRepository: newRepo ]
]

{ #category : 'doits' }
BaselineOfDruid >> postLoadActions [
	self changeRepositoryType
]
