const Papa = require('papaparse');

exports.unparse = function(rows) {
  return Papa.unparse(rows);
}
