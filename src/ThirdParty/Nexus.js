const NexusUI = require('nexusui');
const { debounce } = require('utilities');

exports._dial = function(id, options, onChangeCallback) {
  const settings = Object.assign({}, options, {
    interaction: 'radial',
    mode: 'relative'
  });

  const dial = new NexusUI.Dial(id, settings);

  dial.on('change', debounce(function(value) {
    onChangeCallback(value)();
  }, 100));

  return dial;
}

exports._dialWithNumber = function(id, numberId, options, onChangeCallback) {
  const dial = this._dial(id, options, onChangeCallback);
  const number = new NexusUI.Number(numberId);

  number.link(dial);

  return dial;
}

exports._radio = function(id, options, onChangeCallback) {
  const radio = new NexusUI.RadioButton(id, options);

  radio.on('change', function(value) {
    onChangeCallback(value)();
  });

  return radio;
}

exports._updateDialValue = function(dial, value) {
  dial.value = value;
  return dial;
}

exports._toggle = function(id, options, onChangeCallback) {
  const toggle = new NexusUI.Toggle(id, options);

  toggle.on('change', function(state) {
    onChangeCallback(state)();
  });

  return toggle;
}

exports._updateToggleState = function(toggle, state) {
  toggle.state = state;
  return toggle;
}
