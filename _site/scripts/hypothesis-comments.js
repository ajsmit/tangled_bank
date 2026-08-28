(() => {
  const enableHypothesis = () => {
    document.body.classList.add("hypothesis-enabled");

    const embedUrl = "https://hypothes.is/embed.js";
    if (document.querySelector(`script[src="${embedUrl}"]`)) return;

    const script = document.createElement("script");
    script.src = embedUrl;
    script.async = true;
    document.head.appendChild(script);
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", enableHypothesis, { once: true });
  } else {
    enableHypothesis();
  }
})();
