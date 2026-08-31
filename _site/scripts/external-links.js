(() => {
  const siteHosts = new Set([
    window.location.hostname,
    "tangledbank.netlify.app",
    "www.tangledbank.netlify.app"
  ]);

  const setLinkTarget = (link) => {
    const href = link.getAttribute("href");
    if (!href || href.startsWith("javascript:")) return;

    let url;
    try {
      url = new URL(href, window.location.href);
    } catch (_error) {
      return;
    }

    const external =
      (url.protocol === "http:" || url.protocol === "https:") &&
      !siteHosts.has(url.hostname);

    if (external) {
      link.setAttribute("target", "_blank");
      const rel = new Set(
        (link.getAttribute("rel") || "").split(/\s+/).filter(Boolean)
      );
      rel.add("noopener");
      link.setAttribute("rel", [...rel].join(" "));
    } else {
      link.removeAttribute("target");
    }
  };

  const updateLinks = (root = document) => {
    root.querySelectorAll?.("a[href]").forEach(setLinkTarget);
  };

  const initialise = () => {
    updateLinks();
    document.addEventListener(
      "click",
      (event) => {
        const link = event.target.closest?.("a[href]");
        if (link) setLinkTarget(link);
      },
      { capture: true }
    );
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialise, { once: true });
  } else {
    initialise();
  }
})();
