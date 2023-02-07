Class {
	#name : #BaselineOfDruid,
	#superclass : #BaselineOf,
	#category : #BaselineOfDruid
}

{ #category : #baselines }
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
			package: 'Druid' with: [ spec requires: 'PharoVM' ];
			package: 'Druid-Tests' with: [ spec requires: 'Druid' ];
			package: 'Druid-UI' with: [ spec requires: #('Druid' 'Druid-Tests') ].

		spec
			group: 'Core' with: #('Druid' );
			group: 'DruidTests' with: #('Core' 'Druid-Tests');
			group: 'UI' with: #('Druid-UI');
			group: 'default' with: #('Druid' 'DruidTests' 'Druid-UI') ]
]
