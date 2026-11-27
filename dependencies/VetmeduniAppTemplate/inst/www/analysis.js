(function () {
  function register() {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler('analysisBusy', function (message) {
      const button = document.getElementById(message.id);
      if (!button) return;
      button.disabled = message.busy;
      button.setAttribute('aria-busy', String(message.busy));
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register);
  else register();
})();
