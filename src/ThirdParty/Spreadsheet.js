const Handsontable = require('handsontable').default;

class Spreadsheet {
  constructor(spreadsheetElement, rowSeparator, callbacks) {
    this.rowSeparator = rowSeparator;
    this.callbacks = callbacks;

    this.spreadsheet = this._createSpreadsheet(
      spreadsheetElement, [0]
    );
    this.isEditing = false;

    this.exportPlugin = this.spreadsheet.getPlugin('exportFile');

    this.renumberRows(0);
  }

  addSeparator(row) {
    const cols = this.spreadsheet.countCols();
    for (let i = 0; i <= cols - 1; i++) {
      this.spreadsheet.setDataAtCell(row, i, this.rowSeparator, 'ignore')
    }
  }

  _afterBeginEditing() {
    this.isEditing = false
  }

  _afterChange(changes, source) {
    if (source === 'loadData') {
      return;
    }

    if (source != 'ignore') {
      const cell = changes[0];
      const col = cell[1];
      const rowIndex = parseInt(cell[0], 10);
      const text = cell[3];

      if (col == '1' && text == '--') {
        this.addSeparator(rowIndex);
      }

      this.renumberRows(rowIndex);

      this.callbacks.afterChange(this)();
    }

    if (source == 'edit') {
      this.isEditing = false;
    }
  }

  _afterCreateCol(colIndex, amount, source) {
    this.callbacks.afterCreateCol(this)(colIndex)();
  }

  _afterCreateRow(rowIndex, amount, source) {
    if (source == 'ContextMenu.rowAbove') {
      this.renumberRows(rowIndex - 1);
      this.callbacks.afterCreateRow(this)();
    } else if (source == 'ContextMenu.rowBelow') {
      this.renumberRows(rowIndex);
      this.callbacks.afterCreateRow(this)();
    }
  }

  _afterRemoveCol(columnIndex, amount, removedColumns, source) {
    if (source == 'ContextMenu.removeColumn') {
      this.callbacks.afterRemoveCol(this)(removedColumns)();
    }
  }

  _afterRemoveRow(rowIndex, amount, removedRows, source) {
    if (source == 'ContextMenu.removeRow') {
      this.renumberRows(rowIndex);
      this.callbacks.afterRemoveRow(this)();
    }
  }

  _afterSelection(row, column, row2, column2) {
    const selectedColumns = this.spreadsheet.getSelected();
    const columns = selectedColumns.flatMap(function(selection) {
      let startRow, startCol, endRow, endCol;
      [startRow, startCol, endRow, endCol] = selection;

      const sorted = [startCol, endCol].sort((a, b) => a - b);
      [startCol, endCol] = sorted;

      return Array.from({
        length: endCol - startCol + 1
      }, (_, i) => i + startCol);
    });

    const uniqueColumns = [...new Set(columns)].sort((a, b) => a - b);

    this.callbacks.onSelection(this)(uniqueColumns)();
  }

  blur() {
    this.spreadsheet.deselectCell();
    this.spreadsheet.unlisten();
    this.callbacks.onBlur(this)();
  }

  _createSpreadsheet(spreadsheetElement, readOnlyColumnIndices) {
    const spreadsheet = Handsontable(spreadsheetElement, {
      licenseKey: 'non-commercial-and-evaluation',
      data: [],
      colHeaders: [],
      cells: function (row, col) {
        const cellProperties = {};

        if (readOnlyColumnIndices.includes(col)) {
          cellProperties.readOnly = true;
        }

        return cellProperties;
      },
      contextMenu: true,
      hiddenColumns: {
        indicators: true
      },
      minSpareRows: 1,
      height: '47vh',
      width: '100%',
      afterBeginEditing: () => {
        this._afterBeginEditing();
      },
      afterChange: (changes, source) => {
        this._afterChange(changes, source);
      },
      afterCreateCol: (colIndex, amount, source) => {
        // NOTE: setTimeout is needed to allow Handsontable to "settle".
        setTimeout(
          () => this._afterCreateCol(colIndex, amount, source),
          1
        );
      },
      afterCreateRow: (rowIndex, amount, source) => {
        // NOTE: setTimeout is needed to allow Handsontable to "settle".
        setTimeout(
          () => this._afterCreateRow(rowIndex, amount, source),
          1
        );
      },
      afterRemoveCol: (columnIndex, amount, removedColumns, source) => {
        // NOTE: setTimeout is needed to allow Handsontable to "settle".
        setTimeout(
          () => this._afterRemoveCol(columnIndex, amount, removedColumns, source),
          1
        );
      },
      afterRemoveRow: (rowIndex, amount, removedRows, source) => {
        // NOTE: setTimeout is needed to allow Handsontable to "settle".
        setTimeout(
          () => this._afterRemoveRow(rowIndex, amount, removedRows, source),
          1
        );
      },
      afterSelection: (row, column, row2, column2) => {
        this._afterSelection(row, column, row2, column2);
      }
    });

    Handsontable.hooks.add('afterDocumentKeyDown', (event) => {
      this._onKeyDown(event);
    });

    return spreadsheet;
  }

  exportAsCsv() {
    return this.exportPlugin.exportAsString('csv', {
      bom: false,
      columnDelimiter: ',',
      columnHeaders: false,
      exportHiddenColumns: false,
      exportHiddenRows: false,
      rowDelimiter: "\n",
      rowHeaders: false
    });
  }

  _lastRowNumber(lastRowIndex) {
    const rowNumbers =
      this.spreadsheet.getData(0, 0, lastRowIndex - 1, 0).reverse().flat();
    const finalNumber = parseInt(rowNumbers.find(function(number) {
      return Number.isInteger(parseInt(number, 10));
    }), 10);

    if (finalNumber === 0) {
      return 0;
    } else {
      return finalNumber || -1;
    }
  }

  _onKeyDown(event) {
    if (event.code == 'F5') {
      event.preventDefault();

      const rowNumbers = this.startAndEndRows();
      this.callbacks.onPlay(this)();
    }

    if (event.code == 'F6') {
      event.preventDefault();
      this.callbacks.onStop(this)();
    }

    if (event.code == 'F7') {
      event.preventDefault();
      this.callbacks.onPlayOnlyMidi(this)();
    }

    if (event.key == 'Escape') {
      if (!this.isEditing) {
        this.blur();
      }

      if (this.isEditing) {
        this.isEditing = false;
      }
    }

    if (event.altKey == true) {
      event.preventDefault();

      switch (event.key.toLowerCase()) {
        case 'm':
          this.callbacks.onMute(this)();
          break;
        case 's':
          this.callbacks.onSolo(this)();
          break;
      }
    }
  }

  renumberRows(startIndex) {
    let rowNumber = 0;

    if (startIndex > 0) {
      rowNumber = this._lastRowNumber(startIndex) + 1;
    }

    const endIndex = this.spreadsheet.countRows() - 2;
    const cells = [];
    for (let i = startIndex; i <= endIndex; i++) {
      const data = this.spreadsheet.getDataAtCell(i, 1);
      if (data != this.rowSeparator) {
        cells.push([i, 0, rowNumber]);
        rowNumber = rowNumber + 1;
      }
    }

    this.spreadsheet.setDataAtCell(cells, 'ignore');
  }

  startAndEndRows() {
    const selected = this.spreadsheet.getSelected();
    let start = 0;
    if (selected) {
      start = parseInt(this.spreadsheet.getDataAtCell(selected[0][0], 0), 10);
    }

    const end = this._lastRowNumber(this.spreadsheet.countRows() - 1);

    return { start: start, end: end };
  }

  // TODO:
  updateSpreadsheetContextMenu(contextMenuItems) {
    this.spreadsheet.updateSettings({
      contextMenu: {
        items: Handsontable.plugins.ContextMenu.DEFAULT_ITEMS.concat(
          ['---------']
        ).concat(
          contextMenuItems
        )
      }
    });

    return this;
  }

  updateSpreadsheetData(rows) {
    this.spreadsheet.loadData(rows);

    return this;
  }

  updateSpreadsheetHeaders(headers) {
    this.spreadsheet.updateSettings({ colHeaders: headers });

    return this;
  }
}

exports.exportAsCsv = function(spreadsheet) {
  return spreadsheet.exportAsCsv().replace(/\n.*$/, '');
}

exports._rowNumbers = function(spreadsheet) {
  return spreadsheet.startAndEndRows();
}

exports._spreadsheet = function(id, callbacks) {
  return new Spreadsheet(
    document.getElementById(id),
    '----------',
    callbacks
  );
}

exports._updateSpreadsheetContextMenu = function(spreadsheet, contextMenuItems) {
  return spreadsheet.updateSpreadsheetContextMenu(contextMenuItems);
}

exports._updateSpreadsheetHeaders = function(spreadsheet, headers) {
  return spreadsheet.updateSpreadsheetHeaders(headers);
}

exports._updateSpreadsheetData = function(spreadsheet, rows) {
  return spreadsheet.updateSpreadsheetData(rows);
}
