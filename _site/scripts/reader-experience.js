(() => {
  const ready = (callback) => {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", callback, { once: true });
    } else {
      callback();
    }
  };

  const buildReadingProgress = () => {
    const toc = document.querySelector("#TOC");
    const article = document.querySelector("main.content");
    if (!toc || !article || toc.querySelector(".tb-reading-progress")) return;

    const track = document.createElement("div");
    track.className = "tb-reading-progress";
    track.setAttribute("aria-hidden", "true");
    track.innerHTML = '<span class="tb-reading-progress-bar"></span>';
    toc.prepend(track);

    let framePending = false;
    const update = () => {
      const start = article.getBoundingClientRect().top + window.scrollY;
      const distance = Math.max(article.offsetHeight - window.innerHeight, 1);
      const progress = Math.min(100, Math.max(0, ((window.scrollY - start) / distance) * 100));
      document.documentElement.style.setProperty("--tb-reading-progress", `${progress}%`);
      framePending = false;
    };
    const requestUpdate = () => {
      if (!framePending) {
        framePending = true;
        window.requestAnimationFrame(update);
      }
    };

    update();
    window.addEventListener("scroll", requestUpdate, { passive: true });
    window.addEventListener("resize", requestUpdate, { passive: true });
  };

  const cloneList = (source) => {
    if (!source) return null;
    const clone = source.cloneNode(true);
    clone.querySelectorAll("button, input, form, .sidebar-tools-main").forEach((element) => element.remove());
    clone.removeAttribute("id");
    clone.querySelectorAll("[id]").forEach((element) => element.removeAttribute("id"));
    return clone;
  };

  const addGroup = (panel, title, source) => {
    const list = cloneList(source);
    if (!list || !list.querySelector("a")) return;

    const heading = document.createElement("p");
    heading.className = "tb-mobile-contents-heading";
    heading.textContent = title;
    panel.append(heading, list);
  };

  const buildMobileContents = () => {
    const article = document.querySelector("main.content");
    const titleBlock = document.querySelector("#title-block-header");
    if (!article || article.querySelector(".tb-mobile-contents")) return;

    const courseSource = document.querySelector("#quarto-sidebar .sidebar-menu-container");
    const pageSource = document.querySelector("#TOC > ul");
    if (!courseSource && !pageSource) return;

    const details = document.createElement("details");
    details.className = "tb-mobile-contents";
    details.innerHTML =
      '<summary>Chapter contents</summary><div class="tb-mobile-contents-panel"></div>';

    const panel = details.querySelector(".tb-mobile-contents-panel");
    addGroup(panel, "On this page", pageSource);
    addGroup(panel, "Course", courseSource);

    if (!panel.querySelector("a")) return;
    const courseHero = article.querySelector(".tb-course-hero");
    if (courseHero) {
      courseHero.insertAdjacentElement("afterend", details);
      return;
    }

    const openingParagraph = Array.from(article.querySelectorAll("p")).find(
      (paragraph) =>
        !paragraph.closest("#title-block-header, .course-page-nav, .tb-mobile-contents")
    );
    if (openingParagraph) {
      const openingBlock = openingParagraph.closest(".callout") || openingParagraph;
      openingBlock.insertAdjacentElement("afterend", details);
    } else if (titleBlock) {
      titleBlock.insertAdjacentElement("afterend", details);
    } else {
      article.prepend(details);
    }
  };

  const siteHosts = new Set([
    window.location.hostname,
    "tangledbank.netlify.app",
    "www.tangledbank.netlify.app"
  ]);

  const enforceLinkTarget = (link) => {
    const href = link.getAttribute("href");
    if (!href || href.startsWith("javascript:")) return null;

    let url;
    try {
      url = new URL(href, window.location.href);
    } catch {
      return null;
    }

    const isExternal =
      (url.protocol === "http:" || url.protocol === "https:") &&
      !siteHosts.has(url.hostname);

    if (isExternal) {
      link.setAttribute("target", "_blank");
      const rel = new Set((link.getAttribute("rel") || "").split(/\s+/).filter(Boolean));
      rel.add("noopener");
      link.setAttribute("rel", [...rel].join(" "));
      return { isExternal, url };
    }

    if (link.getAttribute("target") === "_blank") {
      link.removeAttribute("target");
    }
    return { isExternal, url };
  };

  const enforceLinkTargets = () => {
    document.querySelectorAll("a[href]").forEach(enforceLinkTarget);

    // Search results and other Quarto components may add links after load.
    // Classify the clicked link immediately before the browser follows it.
    document.addEventListener(
      "click",
      (event) => {
        const link = event.target.closest?.("a[href]");
        if (!link) return;

        const classification = enforceLinkTarget(link);
        const isCardLink = link.matches(
          ".tb-card, .tb-action, .tb-course-path-step a, .tb-course-fact a"
        );
        const isPlainPrimaryClick =
          event.button === 0 &&
          !event.altKey &&
          !event.ctrlKey &&
          !event.metaKey &&
          !event.shiftKey;

        if (
          classification &&
          !classification.isExternal &&
          isCardLink &&
          isPlainPrimaryClick &&
          !link.hasAttribute("download")
        ) {
          event.preventDefault();
          window.location.assign(classification.url.href);
        }
      },
      { capture: true }
    );
  };

  ready(() => {
    buildReadingProgress();
    buildMobileContents();
    enforceLinkTargets();
  });
})();
