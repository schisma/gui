class MidiKeyboard {
  constructor(element, callback) {
    this.keyMap = new Map([
      [65, 12], // C
      [87, 13], // C#
      [83, 14], // D
      [69, 15], // D#
      [68, 16], // E
      [70, 17], // F
      [85, 18], // F#
      [74, 19], // G
      [73, 20], // G#
      [75, 21], // A
      [79, 22], // A#
      [76, 23], // B
      [59, 24]  // C
    ]);

    this.pressedKeys = [];
    this.octave = 4;

    element.addEventListener('keydown', (event) => {
      if (event.which == 88) {
        event.preventDefault();
        this.octave += 1;
      } else if (event.which == 90) {
        event.preventDefault();
        this.octave -= 1;
      } else {
        this.triggerNote(event.which, 144, callback);
      }
    });

    element.addEventListener('keyup', (event) => {
      this.triggerNote(event.which, 128, callback);
    });
  }

  determineMidiNoteNumber(key) {
    if (this.keyMap.has(key)) {
      return this.keyMap.get(key) + (this.octave * 12);
    } else {
      return 0;
    }
  }

  triggerNote(key, command, callback) {
    const noteNumber = this.determineMidiNoteNumber(key);

    if (noteNumber < 21 || noteNumber > 108) {
      return;
    }

    if (command == 144 && !this.pressedKeys.includes(noteNumber)) {
      callback({
        command: command,
        noteNumber: noteNumber,
        value: 127
      })();
      this.pressedKeys.push(noteNumber);
      return;
    }

    if (command == 128 && this.pressedKeys.includes(noteNumber)) {
      callback({
        command: command,
        noteNumber: noteNumber,
        value: 0
      })();
      this.pressedKeys.splice(this.pressedKeys.indexOf(noteNumber), 1);
      return;
    }
  }
}

exports._midiKeyboard = function(element, callback) {
  return new MidiKeyboard(element, callback);
}
