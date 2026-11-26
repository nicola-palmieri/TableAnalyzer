/* One delegated controller also serves dynamically regenerated colour widgets. */
(function () {
  'use strict';
  const selector = '.color-dropdown';
  let restoreInput = null, restoreUntil = 0;
  function close(widget, restore) {
    widget.querySelector('.color-dropdown-grid').classList.remove('open');
    const button = widget.querySelector('.color-dropdown-button');
    button.setAttribute('aria-expanded', 'false');
    if (restore) {
      restoreInput = widget.dataset.inputId;
      restoreUntil = Date.now() + 1500;
      button.focus({preventScroll: true});
    }
  }
  function open(widget) {
    document.querySelectorAll(selector).forEach(other => { if (other !== widget) close(other, false); });
    widget.querySelector('.color-dropdown-grid').classList.add('open');
    widget.querySelector('.color-dropdown-button').setAttribute('aria-expanded', 'true');
    (widget.querySelector('[aria-checked="true"]') || widget.querySelector('.color-cell')).focus();
  }
  function name(widget) {
    const label = widget.parentElement.querySelector('label');
    if (!label) return 'plot';
    const copy = label.cloneNode(true);
    copy.querySelectorAll('.app-help-icon').forEach(e => e.remove());
    return copy.textContent.trim();
  }
  function describe(widget) {
    const title = name(widget);
    widget.querySelector('.color-dropdown-button').setAttribute('aria-label', 'Choose colour for ' + title + '; current ' + widget.dataset.selected);
    widget.querySelector('.color-dropdown-grid').setAttribute('aria-label', 'Colour for ' + title);
  }
  function prepare() {
    document.querySelectorAll(selector).forEach(widget => {
      if (widget.dataset.colorReady || !window.Shiny) return;
      widget.dataset.colorReady = 'true';
      describe(widget);
      Shiny.setInputValue(widget.dataset.inputId, widget.dataset.selected, {priority: 'event'});
      // A selected colour may regenerate this UI. Preserve its keyboard position.
      if (widget.dataset.inputId === restoreInput && Date.now() < restoreUntil &&
          document.activeElement === document.body) {
        widget.querySelector('.color-dropdown-button').focus({preventScroll: true});
      }
    });
  }
  document.addEventListener('click', event => {
    const widget = event.target.closest(selector);
    if (!widget) return;
    const trigger = event.target.closest('.color-dropdown-button');
    if (trigger) {
      if (trigger.getAttribute('aria-expanded') === 'true') close(widget, true);
      else open(widget);
      return;
    }
    const cell = event.target.closest('.color-cell');
    if (!cell) return;
    widget.dataset.selected = cell.dataset.color;
    widget.querySelector('.color-dropdown-swatch').style.backgroundColor = cell.dataset.color;
    widget.querySelectorAll('.color-cell').forEach(e => e.setAttribute('aria-checked', String(e === cell)));
    describe(widget);
    Shiny.setInputValue(widget.dataset.inputId, cell.dataset.color, {priority: 'event'});
    close(widget, true);
  });
  document.addEventListener('pointerdown', event => {
    document.querySelectorAll(selector).forEach(widget => {
      if (!widget.contains(event.target)) close(widget, false);
    });
  });
  document.addEventListener('keydown', event => {
    const widget = event.target.closest(selector);
    if (!widget) return;
    if (event.key === 'Escape') { event.preventDefault(); close(widget, true); return; }
    // Let the browser choose the next tab stop before hiding the focused cell.
    if (event.key === 'Tab') { setTimeout(() => close(widget, false), 0); return; }
    if (event.target.matches('.color-dropdown-button')) {
      if (event.key === 'ArrowDown' || event.key === 'ArrowUp') { event.preventDefault(); open(widget); }
      return;
    }
    const cells = [...widget.querySelectorAll('.color-cell')];
    const index = cells.indexOf(event.target);
    if (index < 0) return;
    const columns = Number(widget.dataset.columns);
    let next;
    if (event.key === 'ArrowRight') next = (index + 1) % cells.length;
    if (event.key === 'ArrowLeft') next = (index + cells.length - 1) % cells.length;
    if (event.key === 'ArrowDown') next = (index + columns) % cells.length;
    if (event.key === 'ArrowUp') next = (index + cells.length - columns) % cells.length;
    if (event.key === 'Home') next = 0;
    if (event.key === 'End') next = cells.length - 1;
    if (next !== undefined) { event.preventDefault(); cells[next].focus(); }
  });
  document.addEventListener('focusin', event => {
    if (!event.target.closest(selector)) restoreInput = null;
    document.querySelectorAll(selector).forEach(widget => {
      if (!widget.contains(event.target)) close(widget, false);
    });
  });
  function start() {
    prepare();
    new MutationObserver(records => {
      if (records.some(record => record.addedNodes.length)) prepare();
    }).observe(document.body, {childList: true, subtree: true});
    if (window.jQuery) jQuery(document).on('shiny:connected', prepare);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
}());
