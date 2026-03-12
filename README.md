# Druid: The magical compiler infraestructure

Druid implements an optimizing compiler infraestructures to develop compilers in Pharo.
The core includes an Intermediate Representation in SSA form and Optimizations that manipulate it.
The goal of these project is to reuse the IR and optimization implementations between different compilers made in Pharo.

You can inspect the IR, visualize the Control Flog Graph and interact with the Instructions.

<img width="1511" height="587" alt="image" src="https://github.com/user-attachments/assets/8d08d3bd-8351-4ad4-92d3-86fd3be1e7c3" />


### Implementations

Currently, Druid is used in two compiler implementations:

- **Druid - Cogit**: a _metacompiler_ to generate the Baseline JIT Compiler of the Pharo VM from its Interpreter. [Read more here](https://github.com/Alamvic/druid/wiki/Metacompilation-of-JIT-compilers)

- **Druid - Opal**: an optimizing bytecode compiler for Pharo. [Read more here](https://github.com/Alamvic/druid/wiki/Optimizing-Bytecode-Compiler)


## Getting started

You can install the _core_ by running
```st
Metacello new
	repository: 'github://Alamvic/druid:main';
	baseline: 'Druid';
	load: 'Core' 
```

To contribute to any compiler implementation, check [the installation page](https://github.com/Alamvic/druid/wiki/Installation).


## Research

Druid is part of research papers and thesis

- Are Abstract-interpreter Baseline JITs Worth it? – CGO 2026 - https://inria.hal.science/hal-05407834 
- DRUID - Metacompilation of Baseline JIT Compilers – PhD Thesis - https://hal.science/tel-05492093
- PiNodes in the Druid Meta-Compiler - IWST 2025 - https://inria.hal.science/hal-05262681/document
- Meta-compilation of Baseline JIT Compilers with Druid – Programming 2025 – https://arxiv.org/abs/2502.20543 
- Ordering Optimisations in Meta-Compilation of Primitive Methods - FAST 2022 - https://openreview.net/pdf?id=jYsMG5sjQy


## How to contribute?

For questions and bug reports, please [open an issue](https://github.com/Alamvic/druid/issues).

If you already have something implemented, we would love to receive a [Pull Request](https://github.com/Alamvic/druid/pulls).

You can contact us in the [Pharo Discord](https://discord.gg/QewZMZa).
