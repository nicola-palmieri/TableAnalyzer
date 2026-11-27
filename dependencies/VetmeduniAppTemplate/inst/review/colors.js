// Development-only browser audit. This file is not loaded by either application.
(function () {
  function rgb(value) {
    const parts = value.match(/[\d.]+/g).map(Number);
    return [parts[0], parts[1], parts[2], parts.length > 3 ? parts[3] : 1];
  }
  function blend(top, bottom) {
    return top.slice(0, 3).map((v, i) => v * top[3] + bottom[i] * (1 - top[3])).concat(1);
  }
  function luminance(color) {
    const a = color.slice(0, 3).map(v => {
      v /= 255;
      return v <= .04045 ? v / 12.92 : ((v + .055) / 1.055) ** 2.4;
    });
    return a[0] * .2126 + a[1] * .7152 + a[2] * .0722;
  }
  function contrast(foreground, background) {
    const a = luminance(foreground), b = luminance(background);
    return (Math.max(a, b) + .05) / (Math.min(a, b) + .05);
  }
  function background(element) {
    const parents = [];
    for (let p = element; p; p = p.parentElement) parents.unshift(p);
    return parents.reduce((color, p) => blend(rgb(getComputedStyle(p).backgroundColor), color), [255, 255, 255, 1]);
  }
  function visible(element) {
    return element.checkVisibility() && element.getBoundingClientRect().width > 0;
  }
  function snapshot() {
    const failures = [];
    const selectors = ['.navbar .nav-link', '.navbar .app-wordmark-accent', '.home-hero p',
      '.home-wordmark', '.home-card-top h3', '.home-analysis-card p', '.home-optional',
      '.ta-sidebar label', '.ta-sidebar h6', '.ta-sidebar small', '.ta-sidebar-subtitle',
      '.ta-sidebar button', '.selectize-input', '.selectize-dropdown .option',
      '.alert', '.text-muted', '.help-block', '.ta-type-hint', '.upload-filename',
      'table th', 'table td', '.dataTables_info', '.page-link', '.user-guide-content p'];
    let checked = 0;
    for (const selector of selectors) {
      const elements = [...document.querySelectorAll(selector)].filter(visible).slice(0, 20);
      for (const element of elements) {
        if (element.closest('.shiny-plot-output,.color-dropdown,[disabled],.disabled,[aria-disabled=true]')) continue;
        if (!element.textContent.trim()) continue;
        const style = getComputedStyle(element), bg = background(element);
        const fg = blend(rgb(style.color), bg);
        const large = parseFloat(style.fontSize) >= 24 ||
          (parseFloat(style.fontSize) >= 18.66 && parseInt(style.fontWeight, 10) >= 700);
        const minimum = large ? 3 : 4.5, ratio = contrast(fg, bg);
        checked++;
        if (ratio + .001 < minimum) failures.push({selector, text: element.textContent.trim().slice(0, 90),
          color: style.color, background: bg.slice(0, 3), contrast: ratio, minimum});
      }
    }
    const nav = document.querySelector('.navbar'), logo = document.querySelector('.navbar-institution');
    const n = nav.getBoundingClientRect(), l = logo.getBoundingClientRect();
    const style = selector => {
      const e = document.querySelector(selector);
      if (!e) return null;
      const s = getComputedStyle(e);
      return {color: s.color, background: s.backgroundColor};
    };
    return {checked, failures, overflow: document.documentElement.scrollWidth > innerWidth,
      logoInside: l.width > 0 && l.top >= n.top && l.bottom <= n.bottom,
      navbar: style('.navbar'), workspace: style('body'), wordmark: style('.navbar .app-wordmark-accent'),
      panel: style('.tab-pane.active .sidebar'), metrics: style('.app-metric-strip'),
      banner: style('.home-hero'), button: style('.home-start')};
  }
  function feedback() {
    const group = document.createElement('section');
    const classes = ['alert alert-success', 'alert alert-warning', 'alert alert-danger', 'alert alert-info',
      'alert alert-info app-state-running', 'shiny-notification', 'shiny-notification shiny-notification-message',
      'shiny-notification shiny-notification-warning', 'shiny-notification shiny-notification-error'];
    document.body.append(group);
    const values = classes.map(name => {
      const node = document.createElement('div'); node.className = name; node.textContent = 'Feedback';
      group.append(node);
      const s = getComputedStyle(node);
      return {name, color: s.color, background: s.backgroundColor, contrast: contrast(rgb(s.color), rgb(s.backgroundColor))};
    });
    group.remove();
    return values;
  }
  window.vetmeduniColorAudit = {snapshot, feedback};
}());
