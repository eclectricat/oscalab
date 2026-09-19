# OSCALAB: Scala DSP playground

## Goal

- playground for DSP experimentation
- exploring ways of interacting with electronic sound/music generation via 'Code' interface
  - full control of sound engine and sequencing
  - 100% synthesis based, self contained 'sandbox'


## Features

- flexible extensible sound engine, minimal and easy to understand
- every module implements 'SignalGenerator' interface, modules can be wired arbitrarily
- implementations of e.g virtual analog modules, fm, additive synth
- easy to integrate microtonal or alternative tuning
- flexible sequencing of sound triggers and all sound parameters
- interfaces with system audio (and midi), stream multi-channel audio to daw
- scala implementation (fast, expressive, interactive)
  - REPL interactive 'live' coding interface
- simple UI elements for Waveform visualisation, and Parameter input (sliders)

## Setup

Easiest setup is with Intellij IDEA, but it can also be used with CLI Scala Repl, or just as a scala library

In IDEA:

- Run "Scala REPL" (compiles/runs the 'framework' section of the repo)
- Switch to live/*.sc script files
  - select pieces of code, and run them with "Send to Scala REPL" (right-click, or Ctrl-Shift-X)
  - full syntax highlighting and static code checks, autocompletion

<video src="docs/ShortDemo.mp4" controls></video>

## Status

Work in progress, subject to change, majority of modules in experimental/POC state.

