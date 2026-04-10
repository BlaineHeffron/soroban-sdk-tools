/* ── nav highlight ── */
const currentPage = document.body.dataset.page;
for (const link of document.querySelectorAll(".site-nav a")) {
  const href = link.getAttribute("href");
  if (!href) continue;
  if (
    (currentPage === "home" && href === "index.html") ||
    (currentPage === "migration" && href === "migration.html") ||
    (currentPage === "examples" && href === "examples.html")
  ) {
    link.classList.add("is-active");
  }
}

/* ── copy install command ── */
(function initCopyButtons() {
  for (const button of document.querySelectorAll("[data-copy-target]")) {
    button.addEventListener("click", async () => {
      const target = document.getElementById(button.dataset.copyTarget);
      if (!target) return;

      const text = target.innerText.trim();
      const originalLabel = button.textContent;

      try {
        await navigator.clipboard.writeText(text);
        button.textContent = "Copied";
        button.classList.add("is-copied");
      } catch {
        button.textContent = "Failed";
      }

      window.setTimeout(() => {
        button.textContent = originalLabel;
        button.classList.remove("is-copied");
      }, 1400);
    });
  }
})();

/* ── reveal on scroll ── */
(function initReveals() {
  const reveals = document.querySelectorAll(".reveal");
  if (!reveals.length) return;
  if (!("IntersectionObserver" in window)) {
    for (const element of reveals) element.classList.add("is-visible");
    return;
  }

  const observer = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        if (!entry.isIntersecting) continue;
        entry.target.classList.add("is-visible");
        observer.unobserve(entry.target);
      }
    },
    { threshold: 0.18, rootMargin: "0px 0px -6% 0px" }
  );

  for (const element of reveals) observer.observe(element);
})();

/* ── hero parallax ── */
(function initHeroParallax() {
  const panel = document.querySelector("[data-parallax]");
  if (!panel || window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;

  let ticking = false;

  function update() {
    const rect = panel.getBoundingClientRect();
    const viewport = window.innerHeight || 1;
    const offset = ((rect.top + rect.height / 2) / viewport - 0.5) * -18;
    panel.style.transform = `translateY(${offset.toFixed(2)}px)`;
    ticking = false;
  }

  function requestTick() {
    if (ticking) return;
    ticking = true;
    requestAnimationFrame(update);
  }

  update();
  window.addEventListener("scroll", requestTick, { passive: true });
  window.addEventListener("resize", requestTick);
})();

/* ── starfield ── */
(function initStarfield() {
  const canvas = document.getElementById("starfield");
  if (!canvas) return;
  const ctx = canvas.getContext("2d");
  let w, h, stars;
  const STAR_COUNT = 120;
  const DPR = Math.min(window.devicePixelRatio || 1, 2);

  function resize() {
    w = window.innerWidth;
    h = window.innerHeight;
    canvas.width = w * DPR;
    canvas.height = h * DPR;
    canvas.style.width = w + "px";
    canvas.style.height = h + "px";
    ctx.setTransform(DPR, 0, 0, DPR, 0, 0);
  }

  function seed() {
    stars = [];
    for (let i = 0; i < STAR_COUNT; i++) {
      stars.push({
        x: Math.random() * w,
        y: Math.random() * h,
        r: Math.random() * 1.2 + 0.3,
        a: Math.random() * 0.5 + 0.15,
        speed: Math.random() * 0.0008 + 0.0003,
        phase: Math.random() * Math.PI * 2,
      });
    }
  }

  function draw(t) {
    ctx.clearRect(0, 0, w, h);
    for (const s of stars) {
      const flicker = 0.5 + 0.5 * Math.sin(t * s.speed * 1000 + s.phase);
      const alpha = s.a * (0.6 + 0.4 * flicker);
      ctx.beginPath();
      ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2);
      ctx.fillStyle = `rgba(180, 210, 240, ${alpha})`;
      ctx.fill();
    }
    requestAnimationFrame(draw);
  }

  resize();
  seed();
  requestAnimationFrame(draw);

  let resizeTimer;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => { resize(); seed(); }, 200);
  });
})();
