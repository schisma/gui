var createError = require('http-errors');
var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var logger = require('morgan');

var WebSocket = require('ws');
var midi = require('midi');

var indexRouter = require('./routes/index');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(logger('dev'));
app.use(express.json({ limit: '100mb' }));
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', indexRouter);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  next(createError(404));
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});


var wsPort = 8888;
var wss = new WebSocket.Server({ port: wsPort });

let midiChannel = 0;

var midiOutput = new midi.Output();
midiOutput.openVirtualPort('Schisma Output');

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(value) {
    const message = JSON.parse(value);
    console.log(message);

    switch (message.type) {
      case 'midi message':
        midiOutput.sendMessage(message.contents.data);
        break;
      case 'midi channel':
        midiChannel = message.contents.channel;
        break;
    }
  });
});


if (process.env.MIDI_INPUT_PORT) {
  const input = new midi.Input();
  const midiInputPort = parseInt(process.env.MIDI_INPUT_PORT, 10);
  const inputName = input.getPortName(midiInputPort);

  input.on('message', (deltaTime, message) => {
    const channel = message[0] + midiChannel - 1
    const midiMessage = [channel, message[1], message[2]];
    console.log(midiMessage);
    midiOutput.sendMessage(midiMessage);
  });

  input.openPort(midiInputPort);
  console.log(`Forwarding MIDI input from ${inputName} to Schisma...`)
}


module.exports = app;
