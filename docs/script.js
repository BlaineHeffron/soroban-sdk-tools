/* ── nav highlight ── */
const currentPage = document.body.dataset.page;
for (const link of document.querySelectorAll(".site-nav a")) {
  const href = link.getAttribute("href");
  if (!href) continue;
  if (
    (currentPage === "home" && href === "index.html") ||
    (currentPage === "migration" && href === "migration.html") ||
    (currentPage === "examples" && href === "examples.html") ||
    (currentPage === "case-study" && href === "case-study.html")
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

/* ── rust syntax highlighting ── */
(function initRustSyntaxHighlighting() {
  const blocks = document.querySelectorAll("pre code.language-rust");
  if (!blocks.length) return;

  const KEYWORDS = new Set([
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum",
    "extern", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move",
    "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait",
    "type", "unsafe", "use", "where", "while",
  ]);
  const TYPES = new Set([
    "Address", "Bytes", "BytesN", "Env", "Error", "Option", "PersistentMap", "Result",
    "String", "Symbol", "TemporaryMap", "Vec", "bool", "i128", "i16", "i32", "i64", "i8",
    "u128", "u16", "u32", "u64", "u8", "usize",
  ]);

  const tokenPattern =
    /\/\/[^\n]*|\/\*[\s\S]*?\*\/|#\[[^\]\n]*\]|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])'|\b(?:true|false)\b|\b\d[\d_]*(?:\.\d[\d_]*)?(?:[iu](?:8|16|32|64|128|size)|f(?:32|64))?\b|'[A-Za-z_]\w*|\b[A-Za-z_]\w*!|\b[A-Za-z_]\w*\b/g;

  function escapeHtml(value) {
    return value
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;");
  }

  function classify(token) {
    if (token.startsWith("//") || token.startsWith("/*")) return "tok-comment";
    if (token.startsWith("#[")) return "tok-attribute";
    if (token.startsWith("\"") || /^'(?:\\.|[^'\\])'$/.test(token)) return "tok-string";
    if (/^'(?:[A-Za-z_]\w*)$/.test(token)) return "tok-lifetime";
    if (/^(?:true|false)$/.test(token)) return "tok-bool";
    if (/^\d/.test(token)) return "tok-number";
    if (token.endsWith("!")) return "tok-macro";
    if (KEYWORDS.has(token)) return "tok-keyword";
    if (TYPES.has(token)) return "tok-type";
    return "";
  }

  function highlightRust(source) {
    tokenPattern.lastIndex = 0;
    let result = "";
    let last = 0;
    let match;

    while ((match = tokenPattern.exec(source)) !== null) {
      const token = match[0];
      const index = match.index;

      result += escapeHtml(source.slice(last, index));
      const tokenClass = classify(token);
      const safeToken = escapeHtml(token);
      result += tokenClass ? `<span class="${tokenClass}">${safeToken}</span>` : safeToken;
      last = index + token.length;
    }

    result += escapeHtml(source.slice(last));
    return result;
  }

  for (const block of blocks) {
    const source = block.textContent ?? "";
    block.innerHTML = highlightRust(source);
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

/* ── case study page ── */
(function initCaseStudy() {
  if (document.body.dataset.page !== "case-study") return;

  const summaryTarget = document.getElementById("case-study-summary");
  const measurementTarget = document.getElementById("measurement-table");
  const interactionTarget = document.getElementById("interaction-table");
  const evidenceTarget = document.getElementById("evidence-table");
  const deploymentTarget = document.getElementById("deployment-table");
  const measuredAtTarget = document.getElementById("measured-at");
  const networkTarget = document.getElementById("measured-network");
  const conclusionTarget = document.getElementById("case-study-conclusion");

  function variantLabel(variant) {
    if (variant === "auto") return "auto_shorten=true";
    if (variant === "symbolic") return "auto_shorten=true, symbolic=true";
    return "manual Soroban SDK";
  }

  function formatPercent(base, value) {
    const delta = ((base - value) / base) * 100;
    return `${Math.round(delta)}%`;
  }

  function expertNetworkPath(network) {
    return network === "mainnet" ? "public" : "testnet";
  }

  function deploymentUrl(contractId, network) {
    const labNetwork = network === "mainnet" ? "mainnet" : "testnet";
    return `https://lab.stellar.org/r/${labNetwork}/contract/${contractId}`;
  }

  function renderTable(target, headers, rows) {
    const thead = `<thead><tr>${headers.map((header) => `<th>${header}</th>`).join("")}</tr></thead>`;
    const tbody = `<tbody>${rows
      .map((row) => `<tr>${row.map((cell) => `<td>${cell}</td>`).join("")}</tr>`)
      .join("")}</tbody>`;
    target.innerHTML = `<table class="measure-table">${thead}${tbody}</table>`;
  }

  function contractUrl(contractId, network) {
    return `https://stellar.expert/explorer/${expertNetworkPath(network)}/contract/${contractId}`;
  }

  function storageUrl(contractId, network) {
    return `https://stellar.expert/explorer/${expertNetworkPath(network)}/contract/${contractId}/storage`;
  }

  function shortHash(hash) {
    return `${hash.slice(0, 8)}...${hash.slice(-6)}`;
  }

  fetch("data/case-study-results.json")
    .then((response) => {
      if (!response.ok) throw new Error("Unable to load case study data");
      return response.json();
    })
    .then((data) => {
      const measuredAt = new Date(data.measured_at);
      measuredAtTarget.textContent = measuredAt.toLocaleString("en-US", {
        dateStyle: "long",
        timeStyle: "short",
        timeZone: "America/New_York",
      });
      networkTarget.textContent = data.network;

      const measurements = data.measurements.slice().sort((a, b) => {
        return (
          a.family.localeCompare(b.family) ||
          a.key.localeCompare(b.key) ||
          ["auto", "symbolic", "manual"].indexOf(a.variant) -
            ["auto", "symbolic", "manual"].indexOf(b.variant)
        );
      });

      const keyLookup = new Map(
        measurements.map((item) => [`${item.family}:${item.variant}:${item.key}`, item.bytes])
      );
      const deploymentLookup = new Map(
        data.deployments.map((item) => [`${item.family}:${item.variant}`, item.contract_id])
      );

      const measurementCitation = (family, variant, key) => {
        const contractId = deploymentLookup.get(`${family}:${variant}`);
        const match = data.measurements.find(
          (item) => item.family === family && item.variant === variant && item.key === key
        );
        if (!contractId || !match) return "";
        return [
          `<a class="cite-link" href="${contractUrl(contractId, data.network)}" rel="noopener" title="Contract history shows ${key} measurement result">[log]</a>`,
          `<a class="cite-link" href="${storageUrl(contractId, data.network)}" rel="noopener" title="Contract storage shows the ledger entry">[entry]</a>`,
        ].join("");
      };

      const autoAllowance = keyLookup.get("token-lite:auto:allowance(owner,spender)");
      const symbolicAllowance = keyLookup.get("token-lite:symbolic:allowance(owner,spender)");
      const manualAllowance = keyLookup.get("token-lite:manual:allowance(owner,spender)");
      const manualBalance = keyLookup.get("token-lite:manual:balance(address)");
      const autoBalance = keyLookup.get("token-lite:auto:balance(address)");
      const symbolicBalance = keyLookup.get("token-lite:symbolic:balance(address)");
      const manualAccount = keyLookup.get("account:manual:owner");
      const autoAccount = keyLookup.get("account:auto:owner");

      summaryTarget.innerHTML = [
        {
          label: "Two-address allowance key",
          value: `${autoAllowance} bytes`,
          note: `${formatPercent(manualAllowance, autoAllowance)} smaller than manual (${manualAllowance} bytes)`,
          citation: measurementCitation("token-lite", "auto", "allowance(owner,spender)"),
        },
        {
          label: "Single account balance key",
          value: `${autoBalance} bytes`,
          note: `${formatPercent(manualBalance, autoBalance)} smaller than manual (${manualBalance} bytes)`,
          citation: measurementCitation("token-lite", "auto", "balance(address)"),
        },
        {
          label: "Account metadata item key",
          value: `${autoAccount} bytes`,
          note: `${formatPercent(manualAccount, autoAccount)} smaller than manual (${manualAccount} bytes)`,
          citation: measurementCitation("account", "auto", "owner"),
        },
      ]
        .map(
          (item) => `
            <article class="stat-panel">
              <p class="card-kicker">${item.label} ${item.citation ?? ""}</p>
              <h3>${item.value}</h3>
              <p>${item.note}</p>
            </article>
          `
        )
        .join("");

      renderTable(
        measurementTarget,
        [
          "Contract",
          "Key",
          "Durability",
          "manual SDK bytes",
          "auto_shorten=true bytes",
          "auto reduction",
          "auto_shorten=true, symbolic=true bytes",
          "symbolic reduction",
        ],
        [
          [
            "increment",
            "counter",
            "instance",
            `16 ${measurementCitation("increment", "manual", "counter")}`,
            `12 ${measurementCitation("increment", "auto", "counter")}`,
            formatPercent(16, 12),
            `12 ${measurementCitation("increment", "symbolic", "counter")}`,
            formatPercent(16, 12),
          ],
          [
            "account",
            "owner",
            "instance",
            `28 ${measurementCitation("account", "manual", "owner")}`,
            `12 ${measurementCitation("account", "auto", "owner")}`,
            formatPercent(28, 12),
            `12 ${measurementCitation("account", "symbolic", "owner")}`,
            formatPercent(28, 12),
          ],
          [
            "account",
            "guardian",
            "instance",
            `28 ${measurementCitation("account", "manual", "guardian")}`,
            `12 ${measurementCitation("account", "auto", "guardian")}`,
            formatPercent(28, 12),
            `12 ${measurementCitation("account", "symbolic", "guardian")}`,
            formatPercent(28, 12),
          ],
          [
            "account",
            "nonce",
            "instance",
            `28 ${measurementCitation("account", "manual", "nonce")}`,
            `12 ${measurementCitation("account", "auto", "nonce")}`,
            formatPercent(28, 12),
            `12 ${measurementCitation("account", "symbolic", "nonce")}`,
            formatPercent(28, 12),
          ],
          [
            "token-lite",
            "admin",
            "instance",
            `28 ${measurementCitation("token-lite", "manual", "admin")}`,
            `12 ${measurementCitation("token-lite", "auto", "admin")}`,
            formatPercent(28, 12),
            `12 ${measurementCitation("token-lite", "symbolic", "admin")}`,
            formatPercent(28, 12),
          ],
          [
            "token-lite",
            "balance(address)",
            "persistent",
            `${manualBalance} ${measurementCitation("token-lite", "manual", "balance(address)")}`,
            `${autoBalance} ${measurementCitation("token-lite", "auto", "balance(address)")}`,
            formatPercent(manualBalance, autoBalance),
            `${symbolicBalance} ${measurementCitation("token-lite", "symbolic", "balance(address)")}`,
            formatPercent(manualBalance, symbolicBalance),
          ],
          [
            "token-lite",
            "allowance(owner,spender)",
            "persistent",
            `${manualAllowance} ${measurementCitation("token-lite", "manual", "allowance(owner,spender)")}`,
            `${autoAllowance} ${measurementCitation("token-lite", "auto", "allowance(owner,spender)")}`,
            formatPercent(manualAllowance, autoAllowance),
            `${symbolicAllowance} ${measurementCitation("token-lite", "symbolic", "allowance(owner,spender)")}`,
            formatPercent(manualAllowance, symbolicAllowance),
          ],
        ]
      );

      renderTable(
        interactionTarget,
        [
          "Contract",
          "Interaction",
          "manual SDK bytes",
          "auto_shorten=true bytes",
          "auto reduction",
          "auto_shorten=true, symbolic=true bytes",
          "symbolic reduction",
        ],
        data.interaction_totals.map((interaction) => {
          const bytesByVariant = Object.fromEntries(
            interaction.variants.map((variant) => [variant.variant, variant.bytes])
          );
          const manualBytes = bytesByVariant.manual;
          const autoBytes = bytesByVariant.auto;
          const symbolicBytes = bytesByVariant.symbolic;

          return [
            interaction.family,
            interaction.interaction,
            `${manualBytes}`,
            `${autoBytes}`,
            formatPercent(manualBytes, autoBytes),
            `${symbolicBytes}`,
            formatPercent(manualBytes, symbolicBytes),
          ];
        })
      );

      renderTable(
        evidenceTarget,
        ["Contract", "Variant", "Key", "Bytes", "History", "Storage", "Tx"],
        data.measurements
          .slice()
          .sort((a, b) => {
            return (
              a.family.localeCompare(b.family) ||
              a.key.localeCompare(b.key) ||
              ["auto", "symbolic", "manual"].indexOf(a.variant) -
                ["auto", "symbolic", "manual"].indexOf(b.variant)
            );
          })
          .map((item) => {
            const contractId = deploymentLookup.get(`${item.family}:${item.variant}`);
            return [
              item.family,
              variantLabel(item.variant),
              item.key,
              `${item.bytes}`,
              `<a class="cite-link" href="${contractUrl(contractId, data.network)}" rel="noopener">log</a>`,
              `<a class="cite-link" href="${storageUrl(contractId, data.network)}" rel="noopener">entry</a>`,
              item.citation_tx_hash
                ? `<a class="cite-link" href="https://stellar.expert/explorer/${expertNetworkPath(data.network)}/tx/${item.citation_tx_hash}" rel="noopener">${shortHash(item.citation_tx_hash)}</a>`
                : "",
            ];
          })
      );

      const deployments = data.deployments.slice().sort((a, b) => {
        return (
          a.family.localeCompare(b.family) ||
          ["auto", "symbolic", "manual"].indexOf(a.variant) -
            ["auto", "symbolic", "manual"].indexOf(b.variant)
        );
      });

      renderTable(
        deploymentTarget,
        ["Contract", "Variant", "Package", "Contract ID"],
        deployments.map((deployment) => [
          deployment.family,
          variantLabel(deployment.variant),
          `<code>${deployment.package}</code>`,
          `<a href="${deploymentUrl(deployment.contract_id, data.network)}" rel="noopener"><code>${deployment.contract_id}</code></a>`,
        ])
      );

      conclusionTarget.innerHTML = `
        <p><strong>Result.</strong> For contracts that lean on account-shaped keys, <code>auto_shorten=true</code> came out ahead in this ${data.network} run. It tied the symbolic route on simple instance items, then pulled ahead once the key included one address or two.</p>
        <p><strong>What changed.</strong> Symbolic mode kept readable prefixes, which helped a little but not enough. <code>balance(address)</code> landed at ${symbolicBalance} bytes versus ${manualBalance} for manual storage, while <code>allowance(owner,spender)</code> landed at ${symbolicAllowance} versus ${manualAllowance}. The hashed auto route flattened those same keys to ${autoBalance} and ${autoAllowance} bytes.</p>
        <p><strong>Takeaway.</strong> If the goal is the smallest practical key footprint, use the fully automatic route. If you want readable prefixes and can live with the extra bytes, symbolic mode is still a reasonable middle ground. The manual path is mostly useful here as the baseline.</p>
      `;
    })
    .catch((error) => {
      const message = `<p>Case study data failed to load.</p><p><code>${error.message}</code></p>`;
      if (summaryTarget) summaryTarget.innerHTML = message;
      if (measurementTarget) measurementTarget.innerHTML = message;
    });
})();
