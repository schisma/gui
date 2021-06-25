# Schisma GUI

Not a fan of composing strictly via plaintext? This web-based GUI is for you.

# Installation

If you don't already have PureScript/Spago installed, please follow the steps
outlined [here](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md).

If you don't already have it, install `parcel`:

```sh
npm install -g parcel-bundler@1.12.3
```

Once those are installed, run the following:

```sh
npm install
```

You'll also need to make sure [Schisma](https://gitlab.com/schisma/schisma) is
installed and available on your `$PATH`.

You can then launch the app with the following:

```sh
npm run launch
```

The app will be running at [http://localhost:3000](http://localhost:3000).

# Getting Started

The primary entry point to working with Schisma is through a
["Project File"](https://gitlab.com/schisma/schisma#project-file). You can
type a relative or absolute path to a `project.json` file here to load the
project. If the file doesn't exist and the parent directory is writeable, a new
project file will be created instead.

If you'd like to see a demo, Schisma comes with an
[example project](https://gitlab.com/schisma/schisma/-/blob/master/examples/Commemoration/project.json).
You can type in the relative (or absolute) path to this `project.json` and
click "Load". You should see the Tracker and the Instruments populate with
data. Try clicking on "Synth Controls" and then click on a track within the
Tracker. The controls for the corresponding synth should then show up.

## Key Bindings

* `F5` - Play from the current line (if a line in the Tracker is selected) or
  play from the beginning (if no line is selected)
* `F6` - Stop playback or MIDI sandbox
* `F7` - Play in MIDI sandbox
* `Alt + m` - Mute selected track
* `Alt + s` - Solo selected track
* `Escape` - Deselect the current tracker cell input and enable MIDI keyboard
  (note that the MIDI Sandbox must be turned on and the instrument
  corresponding to the selected track must have its MIDI channel > 0)

## MIDI Interop

Want to compose with a MIDI device? Plug it in and then run:

```sh
npm run list-midi-inputs
```

This will show a table with the available MIDI devices. Choose the port
corresponding to your device and then launch the app:

```sh
MIDI_INPUT_PORT=1 npm run launch
```

If you don't have a MIDI device, there is a MIDI keyboard built into the GUI.

```
Key => Note
  a => C
  w => C#
  s => D
  e => D#
  d => E
  f => F
  u => F#
  j => G
  i => G#
  k => A
  o => A#
  l => B
  ; => C

Key => Function
  z => Lower keyboard by one octave
  x => Raise keyboard by one octave
```

# Launching a Development Server

If you're looking to develop the GUI further, you'll want to run the following
in order to run a development server with auto-reload functionality:

```sh
spago build
npm run build:css
npm run hot-reload:client
npm run hot-reload:server
```
