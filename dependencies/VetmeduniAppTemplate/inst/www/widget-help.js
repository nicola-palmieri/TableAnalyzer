// Generic accessible contextual help; applications supply their own scientific copy.
(function () {
  'use strict';
  const shared = {
    width: 'Set subplot width in logical screen pixels. Export preserves the preview proportions at 300 dpi.',
    height: 'Set subplot height in logical screen pixels. Export preserves the preview proportions at 300 dpi.',
    base_size: 'Set the base plot font size in points. Axis labels, facet headings and legend text scale together.',
    show_legend: 'Show or hide the plot legend without changing the data or analysis.',
    legend_position: 'Place the legend below, above, to the left or to the right of the figure.',
    download: 'Save the current figure as a PNG at 300 dpi, preserving the preview layout and styling.'
  };
  const api = window.VetmeduniAppHelp = window.VetmeduniAppHelp || {};
  const resolvers = api.resolvers = api.resolvers || [];
  api.register = function (resolver) {
    if (typeof resolver !== 'function') throw new TypeError('Help resolver must be a function.');
    resolvers.push(resolver);
    if (document.body) prepare();
  };
  function textFor(id, label, sharedReference) {
    for (let i = resolvers.length - 1; i >= 0; --i) {
      const text = resolvers[i](id, label, sharedReference);
      // An app can explicitly omit help for an obvious control.
      if (text === false || text) return text;
    }
    if (/order_|order of levels/i.test(id + ' ' + label)) {
      const reference = /first\s*=\s*reference/i.test(label);
      return 'Arrange all levels in the order you want them displayed. Keep every level selected: this control orders levels and does not exclude samples.' +
        (reference ? ' The first level is the reference for comparisons.' : ' This control does not choose a statistical reference.') +
        (sharedReference ? ' This order is shared with another grouping control.' : '');
    }
    return shared[id.split('-').pop()] || null;
  }
  let tooltip, active;
  function hide() {
    if (active) active.removeAttribute('aria-describedby');
    active = null;
    if (tooltip) tooltip.hidden = true;
  }
  function show(button) {
    clearTimeout(closeTimer);
    hide();
    if (!tooltip) {
      tooltip = document.createElement('div');
      tooltip.id = 'app-widget-tooltip';
      tooltip.className = 'app-widget-tooltip';
      tooltip.setAttribute('role', 'tooltip');
      document.body.appendChild(tooltip);
      tooltip.addEventListener('mouseenter', () => clearTimeout(closeTimer));
      tooltip.addEventListener('mouseleave', hide);
    }
    active = button;
    tooltip.textContent = button.dataset.help;
    tooltip.hidden = false;
    button.setAttribute('aria-describedby', tooltip.id);
    const r = button.getBoundingClientRect();
    const t = tooltip.getBoundingClientRect();
    tooltip.style.left = Math.max(8, Math.min(r.left, innerWidth - t.width - 8)) + 'px';
    tooltip.style.top = Math.max(8, r.bottom + t.height + 8 > innerHeight ? r.top - t.height - 8 : r.bottom + 8) + 'px';
  }
  let closeTimer;
  function icon(text, label) {
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'app-help-icon';
    button.setAttribute('aria-label', 'Help: ' + label);
    button.dataset.help = text;
    button.textContent = '?';
    bind(button);
    return button;
  }
  function bind(button) {
    button.dataset.helpBound = 'true';
    button.addEventListener('mouseenter', () => { clearTimeout(closeTimer); show(button); });
    button.addEventListener('mouseleave', () => { closeTimer = setTimeout(hide, 180); });
    button.addEventListener('focus', () => show(button));
    button.addEventListener('blur', hide);
    button.addEventListener('click', event => {
      event.preventDefault(); event.stopPropagation();
      button.focus({preventScroll: true});
      show(button);
    });
  }
  function prepare() {
    // Label icons stay outside checkbox/radio labels, so clicking help cannot toggle a value.
    document.querySelectorAll('.app-polish label.control-label, .app-polish .checkbox > label, .ta-sidebar label, .dataTables_filter label, .dataTables_length label').forEach(label => {
      if (label.dataset.helpReady || label.matches('.radio-inline, .radio label, .btn, .btn-file, .input-group-btn')) return;
      const container = label.closest('.shiny-input-container') || label.parentElement;
      const input = container.querySelector('input[id], select[id], .color-dropdown-button[id]');
      // Selectize rewrites label targets after binding, including dynamic Block controls.
      const id = (label.htmlFor || (input && input.id) || container.id || '').replace(/-selectized$/, '');
      const title = label.textContent.trim();
      const explicit = label.closest('[data-help]') || container.closest('[data-help]');
      const text = explicit ? explicit.dataset.help : textFor(id, title, container.dataset.orderSharedReference === 'true');
      if (!text) return;
      label.dataset.helpReady = 'true';
      const button = icon(text, title);
      if (label.closest('.checkbox')) label.after(button);
      else label.appendChild(button);
    });
    document.querySelectorAll('.app-help-icon[data-help]').forEach(button => {
      if (!button.dataset.helpBound) bind(button);
    });
    if (active && !active.isConnected) hide();
  }
  document.addEventListener('keydown', event => { if (event.key === 'Escape') hide(); });
  document.addEventListener('pointerdown', event => {
    if (!event.target.closest('.app-help-icon, .app-widget-tooltip')) hide();
  });
  function repositionOrHide() {
    if (active && document.activeElement === active) show(active);
    else hide();
  }
  document.addEventListener('scroll', repositionOrHide, true);
  window.addEventListener('resize', repositionOrHide);
  function start() {
    prepare();
    let scheduled = false;
    new MutationObserver(records => {
      if (scheduled || !records.some(record => record.addedNodes.length || record.removedNodes.length)) return;
      scheduled = true;
      requestAnimationFrame(() => { scheduled = false; prepare(); });
    }).observe(document.body, {childList: true, subtree: true});
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
}());
