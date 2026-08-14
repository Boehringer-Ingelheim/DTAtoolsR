(function () {
  'use strict';

  // Track which groups have been permanently expanded via show-more button
  var permanentlyExpandedGroups = new Set();

  // Get the table we're working with
  var messagesTable = document.getElementById('messages-table');
  if (!messagesTable) {
    return;
  }

  var tbody = messagesTable.querySelector('tbody');
  var thead = messagesTable.querySelector('thead');
  var inspectOverlay = document.getElementById('inspect-overlay');
  var inspectOverlayContent = document.getElementById('inspect-overlay-content');
  var inspectOverlayBackdrop = document.getElementById('inspect-overlay-backdrop');
  var inspectOverlayClose = document.getElementById('inspect-overlay-close');

  // ============================================================================
  // 1. Show More / Expand capped groups
  // ============================================================================
  function setupShowMoreButtons() {
    var showMoreButtons = document.querySelectorAll('.show-more-btn');
    showMoreButtons.forEach(function (btn) {
      btn.addEventListener('click', function (e) {
        e.stopPropagation();
        var group = btn.dataset.group;
        if (group) {
          permanentlyExpandedGroups.add(group);
          var extraRows = tbody.querySelectorAll('tr.msg-row-extra[data-group="' + group + '"]');
          extraRows.forEach(function (row) {
            row.style.display = '';
          });
          var moreRow = tbody.querySelector('tr.msg-more-row[data-group="' + group + '"]');
          if (moreRow) {
            moreRow.hidden = true;
          }
        }
      });
    });
  }

  // ============================================================================
  // 2. Sorting
  // ============================================================================
  function expandAllCappedRows() {
    var extraRows = tbody.querySelectorAll('tr.msg-row-extra[style*="display:none"]');
    extraRows.forEach(function (row) {
      row.style.display = '';
    });
    var moreRows = tbody.querySelectorAll('tr.msg-more-row');
    moreRows.forEach(function (row) {
      row.hidden = true;
    });
  }

  function getAllMsgRows() {
    return Array.prototype.slice.call(tbody.querySelectorAll('tr.msg-row'));
  }

  function sortTable(columnIndex, ascending) {
    var rows = getAllMsgRows();

    // Check if column contains all numeric values
    var isNumeric = rows.every(function (row) {
      var cell = row.cells[columnIndex];
      var text = cell ? cell.textContent.trim() : '';
      return text === '' || !isNaN(Number(text));
    });

    rows.sort(function (rowA, rowB) {
      var cellA = rowA.cells[columnIndex];
      var cellB = rowB.cells[columnIndex];
      var textA = cellA ? cellA.textContent.trim() : '';
      var textB = cellB ? cellB.textContent.trim() : '';

      // Empty values always sort last
      if (textA === '' && textB === '') return 0;
      if (textA === '') return 1;
      if (textB === '') return -1;

      var result = 0;
      if (isNumeric) {
        var numA = Number(textA);
        var numB = Number(textB);
        result = numA - numB;
      } else {
        result = textA.toLowerCase().localeCompare(textB.toLowerCase());
      }

      return ascending ? result : -result;
    });

    // Re-append rows in sorted order
    rows.forEach(function (row) {
      tbody.appendChild(row);
    });
  }

  function setupSorting() {
    var sortHeaders = thead.querySelectorAll('th.sortable');
    sortHeaders.forEach(function (th) {
      th.addEventListener('click', function () {
        // Expand all capped rows first
        expandAllCappedRows();

        // Determine column index
        var columnIndex = Array.prototype.indexOf.call(th.parentNode.children, th);

        // Determine sort direction
        var ascending = !th.classList.contains('sort-asc');
        if (th.classList.contains('sort-desc')) {
          ascending = true;
        }

        // Remove sort classes from all headers
        sortHeaders.forEach(function (header) {
          header.classList.remove('sort-asc', 'sort-desc');
        });

        // Add the appropriate class to this header
        if (ascending) {
          th.classList.add('sort-asc');
        } else {
          th.classList.add('sort-desc');
        }

        // Sort the table
        sortTable(columnIndex, ascending);
      });
    });
  }

  // ============================================================================
  // 3. Filtering
  // ============================================================================
  var activeFilters = {
    text: '',
    columns: {}
  };

  var filterControls = document.querySelectorAll('.report-filter-text, .report-filter-select');
  var hasActiveFilter = false;

  function matchesAllFilters(row) {
    // Text filter (case-insensitive substring match on full row text)
    if (activeFilters.text) {
      if (row.textContent.toLowerCase().indexOf(activeFilters.text.toLowerCase()) === -1) {
        return false;
      }
    }

    // Column filters (exact match on data-<column> attribute)
    for (var column in activeFilters.columns) {
      if (activeFilters.columns.hasOwnProperty(column)) {
        var filterValue = activeFilters.columns[column];
        if (filterValue) {
          var rowValue = row.dataset[column];
          if (rowValue !== filterValue) {
            return false;
          }
        }
      }
    }

    return true;
  }

  function applyFilters() {
    var rows = tbody.querySelectorAll('tr.msg-row');
    var anyFilterActive =
      activeFilters.text !== '' ||
      Object.keys(activeFilters.columns).some(function (col) {
        return activeFilters.columns[col] !== '';
      });

    rows.forEach(function (row) {
      var matches = matchesAllFilters(row);
      row.style.display = matches ? '' : 'none';
    });

    // If no filter is active, restore capping state for non-permanently-expanded groups
    if (!anyFilterActive) {
      var extraRows = tbody.querySelectorAll('tr.msg-row-extra');
      extraRows.forEach(function (row) {
        var group = row.dataset.group;
        if (!permanentlyExpandedGroups.has(group)) {
          row.style.display = 'none';
        }
      });
    }

    // "More" rows: hidden while any filter is active (matches are shown
    // filter-flattened); a group the user has already permanently expanded
    // never gets its button back, even once filters clear -- its rows are
    // already all visible, so showing the button again would be a dead
    // control that looks like there's more data. Otherwise a capped group
    // still hiding rows gets its button restored.
    var moreRows = tbody.querySelectorAll('tr.msg-more-row');
    moreRows.forEach(function (moreRow) {
      var group = moreRow.dataset.group;
      if (anyFilterActive || permanentlyExpandedGroups.has(group)) {
        moreRow.style.display = 'none';
        return;
      }
      moreRow.style.display = '';
    });
  }

  function setupFiltering() {
    filterControls.forEach(function (control) {
      if (control.classList.contains('report-filter-text')) {
        control.addEventListener('input', function () {
          activeFilters.text = this.value;
          applyFilters();
        });
      } else if (control.classList.contains('report-filter-select')) {
        var column = control.dataset.filterColumn;
        activeFilters.columns[column] = '';
        control.addEventListener('change', function () {
          activeFilters.columns[column] = this.value;
          applyFilters();
        });
      }
    });
  }

  // ============================================================================
  // 4. Inspect Overlay
  // ============================================================================
  function setupInspectOverlay() {
    // Delegate click on tbody for msg-row clicks
    if (tbody) {
      tbody.addEventListener('click', function (e) {
        var row = e.target.closest('tr.msg-row');
        if (row) {
          var id = row.dataset.id;
          if (id) {
            var panel = document.getElementById('inspect-panel-' + id);
            if (panel) {
              inspectOverlayContent.innerHTML = panel.innerHTML;
              inspectOverlay.removeAttribute('hidden');
            }
          }
        }
      });
    }

    // Close on backdrop click
    if (inspectOverlayBackdrop) {
      inspectOverlayBackdrop.addEventListener('click', function () {
        inspectOverlay.setAttribute('hidden', '');
      });
    }

    // Close on close button
    if (inspectOverlayClose) {
      inspectOverlayClose.addEventListener('click', function () {
        inspectOverlay.setAttribute('hidden', '');
      });
    }

    // Close on Escape key
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') {
        if (!inspectOverlay.hasAttribute('hidden')) {
          inspectOverlay.setAttribute('hidden', '');
        }
      }
    });
  }

  // ============================================================================
  // Initialize everything
  // ============================================================================
  setupShowMoreButtons();
  setupSorting();
  setupFiltering();
  setupInspectOverlay();
})();
