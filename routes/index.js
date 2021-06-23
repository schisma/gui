const { exec, spawn } = require('child_process');
const fs = require('fs-extra');
const path = require('path');

const csv = require('csv-parser');
const find = require('find-process');
const fkill = require('fkill');
const untildify = require('untildify');

const express = require('express');
const router = express.Router();

const schismaBin = 'schisma';

router.get('/', function(req, res, next) {
  res.render('index', {});
});

router.get('/instruments', function(req, res, next) {
  const file = path.resolve(untildify(req.query.file));

  // TODO: Check for existence of file

  const contents = JSON.parse(fs.readFileSync(file, 'utf8'));
  res.json(contents.instruments);
});

router.put('/instruments', function(req, res, next) {
  const instrumentsFile = path.resolve(untildify(req.body.file));

  // TODO: Check for existence of file

  const instruments = JSON.parse(req.body.contents);
  const output = JSON.stringify({ instruments: instruments }, null, 2);

  fs.writeFile(instrumentsFile, output, function(error) {
    if (error) {
      res.sendStatus(400);
    } else {
      res.sendStatus(200);
    }
  });
});

router.put('/play', function(req, res, next) {
  fkill('csound', { silent: true });

  const projectFile = path.resolve(untildify(req.body.projectFile));

  const schisma = spawn(schismaBin, [
    'tracker',
    'play',
    '-p', projectFile,
    '-s', req.body.startLine,
    '-e', req.body.endLine,
  ], {
    detached: true,
    stdio: 'ignore'
  });

  schisma.unref();


  let iterations = 10;

  const findProcess = function() {
    find('name', 'csound', true).then(function (list) {
      if (list.length == 0) {
        if (iterations <= 1) {
          res.sendStatus(500);
        } else {
          iterations = iterations - 1;
          setTimeout(findProcess, 300);
        }
      } else {
        res.sendStatus(202);
      }
    });
  }

  findProcess();
});

router.get('/project', function(req, res, next) {
  const file = path.resolve(untildify(req.query.file));
  const dir = path.dirname(file);

  if (!fs.existsSync(file)) {
    try {
      fs.accessSync(dir, fs.constants.W_OK);

      const example = path.resolve(__dirname, '../data/projects/example');
      fs.copySync(example, dir);

      const projectFile = path.resolve(dir, './project.json');
      fs.moveSync(projectFile, file);
    } catch (error) {
      file = undefined;
    }
  }

  const contents = JSON.parse(fs.readFileSync(file, 'utf8'));

  contents.compositionFile = path.resolve(dir, contents.compositionFile);
  contents.instrumentsFile = path.resolve(dir, contents.instrumentsFile);
  contents.trackerFile = path.resolve(dir, contents.trackerFile);

  res.json(contents);
});

router.put('/stop', function(req, res, next) {
  fkill('csound', { silent: true });

  res.sendStatus(204);
});

router.get('/synth-list', function(req, res, next) {
  const command = `${schismaBin} synth list`;
  const options = {};

  exec(command, options, function(error, stdout, stderr) {
    const synths = JSON.parse(stdout).map(function(synth) {
      synth.parameters = synth.parameters.map(function(parameter) {
        parameter.value = parameter.defaultValue;
        return parameter;
      });

      return synth;
    });

    res.json(synths);
  })
});

router.get('/tracker', function(req, res, next) {
  const results = [];
  const csvOptions = {
    headers: false,
    mapValues: ({ header, index, value }) => value.trim()
  };
  const file = path.resolve(untildify(req.query.file));

  // TODO: Check for existence of file

  fs.createReadStream(file)
    .pipe(csv(csvOptions))
    .on('data', (data) => results.push(Object.values(data)))
    .on('end', () => {
      res.json(results);
    });
});

router.put('/tracker', function(req, res, next) {
  const trackerFile = path.resolve(untildify(req.body.file));

  // TODO: Check for existence of file

  fs.writeFile(trackerFile, req.body.contents, function(error) {
    if (error) {
      res.sendStatus(400);
    } else {
      res.sendStatus(200);
    }
  });
});


module.exports = router;
