// Generic brand navigation and tab availability preserve the current Shiny session.
(function () {
  'use strict';
  function prepareBrand() {
    document.querySelectorAll('.navbar-brand').forEach(brand => {
      const config = brand.querySelector('[data-home-input]');
      const name = config ? config.dataset.brandLabel : brand.textContent.trim();
      brand.setAttribute('aria-label', name + ' - Home');
      brand.style.cursor = 'pointer';
      if (brand.tagName !== 'A' && brand.tagName !== 'BUTTON') {
        brand.setAttribute('role', 'link');
        brand.tabIndex = 0;
      }
    });
  }
  document.addEventListener('click', function (event) {
    if (event.target.closest('.nav-link.disabled, .nav-link[aria-disabled="true"], .navbar-nav li.disabled > a')) {
      event.preventDefault(); event.stopImmediatePropagation(); return;
    }
    const brand = event.target.closest('.navbar-brand');
    if (!brand) return;
    event.preventDefault();
    const config = brand.querySelector('[data-home-input]');
    const input = config ? config.dataset.homeInput : 'go_home';
    if (window.Shiny && input) Shiny.setInputValue(input, Date.now(), {priority: 'event'});
  }, true);
  document.addEventListener('keydown', function (event) {
    const brand = event.target.closest('.navbar-brand');
    if (!brand || brand.tagName === 'A' || brand.tagName === 'BUTTON') return;
    if (event.key === 'Enter' || event.key === ' ') {event.preventDefault(); brand.click();}
  });
  let registered = false;
  function register() {
    if (registered || !window.Shiny) return;
    registered = true;
    Shiny.addCustomMessageHandler('toggleTabState', function (message) {
      document.querySelectorAll('.navbar [data-value]').forEach(tab => {
        if (tab.dataset.value !== message.tab) return;
        const disabled = !!message.disable;
        tab.classList.toggle('disabled', disabled);
        tab.setAttribute('aria-disabled', String(disabled));
        if (disabled) tab.setAttribute('tabindex', '-1');
        else tab.removeAttribute('tabindex');
        if (tab.parentElement.tagName === 'LI') tab.parentElement.classList.toggle('disabled', disabled);
      });
    });
  }
  document.addEventListener('shiny:connected', register);
  function start() {prepareBrand(); register();}
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
}());
