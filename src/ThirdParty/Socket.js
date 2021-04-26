class Socket {
  constructor(port) {
    this.socket = new WebSocket(`ws://127.0.0.1:${port}`);
    this.socketReady = false;

    this.socket.onopen = (event) => {
      this.socketReady = true;
    };
  }

  sendMidiChannel(channel) {
    if (this.socketReady) {
      const formattedMessage = {
        type: 'midi channel',
        contents: {
          channel: channel
        }
      }

      this.socket.send(JSON.stringify(formattedMessage));
    }
  }

  sendMidiMessage(midiMessage) {
    if (this.socketReady) {
      const formattedMessage = {
        type: 'midi message',
        contents: {
          data: Array.from(midiMessage.data),
          timestamp: midiMessage.timestamp
        }
      }

      this.socket.send(JSON.stringify(formattedMessage));
    }
  }
}

exports._createSocket = function(port) {
  return new Socket(port);
}

exports._sendMidiChannel = function(socket, channel) {
  socket.sendMidiChannel(channel);
}

exports._sendMidiMessage = function(socket, midiMessage) {
  socket.sendMidiMessage(midiMessage);
}
