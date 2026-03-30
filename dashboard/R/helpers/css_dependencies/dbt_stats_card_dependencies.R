

# Router-safe UKHSA card + tabs assets (inline CSS+JS)
# - No tab offsets (removes the unexpected gap)
# - Active tab merges into panel via border-bottom removal + -1px margin overlap
# - No underline/bold on inactive/hover; tabs use govuk-body typographic feel
# - Slightly reduced top padding on the card
# - Height locking preserved so the card never shrinks when switching tabs


# Router-safe UKHSA card + tabs assets (inline CSS+JS)
# Tweaks in this version:
# - Active tab overlap -> -3px (virtually no visual gap to panel)
# - Panel top padding -> 4px
# - Inactive tab hover background -> #e0dedb (slightly darker)


# UKHSA card + tabs assets — roomy buttons & small panel gap
# Keeps collision-safe ARIA selection; adjustable spacing via CSS variables.


# UKHSA card + tabs assets — roomier buttons, merge with panel, square corners
ukhsa_card_tabs_assets <- function(
  accent_underline   = FALSE,          # existing option (kept)
  accent_colour      = "#CF102D",      # existing option (kept)
  button_label_mode  = c("wrap", "ellipsis"),  # NEW: global label behaviour
  mobile_stack       = FALSE           # NEW: stack justified buttons on narrow screens
) {
  button_label_mode <- match.arg(button_label_mode)

  # --- Build the common card/tabs CSS you already had ---
  css_card_tabs <- paste0("
  /* ================================
     UKHSA card + tabs (scoped)
     ================================ */

  .lm-card-ukhsa {
    --card-border: #b1b4b6;   /* neutral border (panels) */
    --card-text:   #0b0c0c;   /* body text */
    --card-grey:   #f3f2f1;   /* card background */
    --card-grey-2: #eeece9;   /* hover baseline */
    --card-white:  #ffffff;

    /* Spacing knobs (no gap to panel) */
    --tab-pad-y: 8px;         /* vertical padding on tabs */
    --tab-pad-x: 14px;        /* horizontal padding on tabs */
    --tab-gap:    6px;        /* gap between tabs */

    background: var(--card-grey);
    border: none;
    border-radius: 0;                 /* square corners */
    padding: 16px 16px 16px 16px;
    width: 100%;
    box-sizing: border-box;
  }

  .lm-card-ukhsa .govuk-heading-m,
  .lm-card-ukhsa .govuk-heading-l { margin-top: 0; color: var(--card-text); }
  .lm-card-ukhsa .govuk-body,
  .lm-card-ukhsa .govuk-body-s,
  .lm-card-ukhsa .govuk-hint {
    color: #505a5f;
    font-size: 14px;
    line-height: 1.45;
    font-weight: 400;
    margin-top: 0.25rem;
  }

  .lm-card-ukhsa .ukhsa-tabs { margin-top: 10px; }

  .lm-card-ukhsa .ukhsa-tabs__list {
    display: flex;
    flex-wrap: wrap;
    gap: var(--tab-gap);
    padding: 0;
    margin: 0;
    align-items: flex-start;
  }

  .lm-card-ukhsa .ukhsa-tabs__tab {
    display: inline-block;
    cursor: pointer;
    text-decoration: none;
    background-color: var(--card-grey);
    color: var(--card-text);
    border: none;
    box-sizing: border-box;
    margin: 0;
    font-size: 14px;
    line-height: 1.4;
    font-weight: 400;
    padding: var(--tab-pad-y) var(--tab-pad-x);
    border-radius: 0;
  }

  .lm-card-ukhsa .ukhsa-tabs__tab:hover {
    background-color: #e0dedb;
    color: var(--card-text);
  }

  .lm-card-ukhsa .ukhsa-tabs__tab:focus,
  .lm-card-ukhsa .ukhsa-tabs__tab:focus-visible {
    outline: none;
    box-shadow: none;
  }

  .lm-card-ukhsa .ukhsa-tabs__tab[aria-selected='true'] {
    text-decoration: none;
    background-color: var(--card-white);
    color: var(--card-text);
    border: 1px solid var(--card-border);
    border-bottom-color: transparent;
    margin-bottom: -1px;
    z-index: 1;", if (isTRUE(accent_underline)) paste0(" box-shadow: inset 0 -1px 0 ", accent_colour, ";"), "
  }

  .lm-card-ukhsa .ukhsa-tabs__panel {
    border: 1px solid var(--card-border);
    background: var(--card-white);
    padding: 8px 16px 16px;
    margin-top: 0;
    min-height: var(--panel-min-h, 0px);
    box-sizing: border-box;
    border-radius: 0;
  }
  .lm-card-ukhsa .ukhsa-tabs__panel.is-hidden { display: none; }

  .lm-card-ukhsa .ukhsa-tabs__panel[data-panel='chart'] {
    border: 1px solid var(--card-border);
    padding: 10px;
    background: var(--card-white);
  }

  @media (max-width: 640px) {
    .lm-card-ukhsa { padding: 18px 12px 12px 12px; }
    .lm-card-ukhsa .ukhsa-tabs__list { gap: 4px; }
    .lm-card-ukhsa .ukhsa-tabs__tab  { padding: 7px 12px; }
    .lm-card-ukhsa .ukhsa-tabs__panel { padding: 8px 14px 14px; }
  }
  
.code-block {
        max-height: 300px;
        overflow: auto;             /* both axes if needed */
        border: 1px solid #e5e5e5;
        border-radius: 6px;
        background: #f8f9fa;
        padding: 8px 10px;
      }
      .code-block pre {
        margin: 0;
        white-space: pre;            /* no wrapping */
        overflow-x: auto;            /* horizontal scrollbar for long lines */
        font-family: SFMono-Regular, Consolas, 'Liberation Mono', Menlo, monospace;
        font-size: 12px;
        line-height: 1.4;
      }
")

  # --- NEW: GLOBAL radioGroupButtons equal-height CSS ---
  css_buttons_equal_height <- "
  /* ===========================================================
     GLOBAL: Equal-height radioGroupButtons (shinyWidgets)
     Works for justified sets and both DOM patterns:
     - .btn-group.btn-group-justified > .btn
     - .btn-group.btn-group-justified > .btn-group > .btn
     =========================================================== */

  .btn-group.btn-group-justified {
    display: flex !important;
    flex-wrap: nowrap;          /* set TRUE via media query if you want to stack */
    align-items: stretch;       /* children equal the tallest height */
    width: 100%;
  }

  .btn-group.btn-group-justified > .btn-group {
    flex: 1 1 0;
    display: flex;              /* allows the inner .btn to stretch */
  }

  .btn-group.btn-group-justified > .btn,
  .btn-group.btn-group-justified > .btn-group > .btn {
    flex: 1 1 auto;
    display: flex;
    align-items: center;
    justify-content: center;
    line-height: 1.2;
    padding: 8px 12px;
    height: auto;               /* flex height wins */
  }
  "

  # --- Switchable label behaviour (wrap vs ellipsis) ---
  css_buttons_label <- switch(
    button_label_mode,
    "wrap" = "
      .btn-group.btn-group-justified > .btn,
      .btn-group.btn-group-justified > .btn-group > .btn {
        white-space: normal;    /* multi-line labels */
      }
    ",
    "ellipsis" = "
      .btn-group.btn-group-justified > .btn,
      .btn-group.btn-group-justified > .btn-group > .btn {
        white-space: nowrap;    /* single line with truncation */
        overflow: hidden;
        text-overflow: ellipsis;
      }
    "
  )

  # --- Optional: mobile stacking for justified groups ---
  css_buttons_mobile <- if (isTRUE(mobile_stack)) "
    @media (max-width: 640px) {
      .btn-group.btn-group-justified { flex-wrap: wrap; }
      .btn-group.btn-group-justified > .btn-group,
      .btn-group.btn-group-justified > .btn { flex: 1 1 100%; }
    }
  " else ""

  # --- Existing JS (unchanged) ---
  js <- "
  (function () {
    function recalcHeights(container) {
      const visible = container.querySelector('.ukhsa-tabs__panel:not(.is-hidden)');
      const h = visible ? visible.offsetHeight : 0;
      const prev = container.__ukhsaMinH || 0;
      const next = Math.max(prev, h);
      container.__ukhsaMinH = next;
      container.style.setProperty('--panel-min-h', next + 'px');
    }

    function activate(container, targetId, triggerBtn) {
      const tabs   = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));
      const panels = Array.from(container.querySelectorAll('.ukhsa-tabs__panel'));

      tabs.forEach(t => {
        t.setAttribute('aria-selected', 'false');
        t.setAttribute('tabindex', '-1');
      });
      panels.forEach(p => p.classList.add('is-hidden'));

      const nextTab   = tabs.find(t => t.getAttribute('data-target') === targetId) || triggerBtn;
      const nextPanel = panels.find(p => p.id === targetId);

      if (nextTab) {
        nextTab.setAttribute('aria-selected', 'true');
        nextTab.setAttribute('tabindex', '0');
        if (!triggerBtn || triggerBtn === nextTab) {
          nextTab.focus({ preventScroll: true });
        }
      }

      if (nextPanel) {
        nextPanel.classList.remove('is-hidden');
        try { if (window.HTMLWidgets && HTMLWidgets.staticRender) { HTMLWidgets.staticRender(); } } catch(e) {}
        try { if (window.Shiny && Shiny.bindAll) { Shiny.bindAll(nextPanel); } } catch(e) {}
        try { window.dispatchEvent(new Event('resize')); } catch(e) {}
      }
      requestAnimationFrame(() => setTimeout(() => recalcHeights(container), 30));
    }

    function bind(container) {
      if (!container || container.__ukhsaBound) return;
      container.__ukhsaBound = true;

      const tabList = container.querySelector('.ukhsa-tabs__list');
      const tabs    = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));
      const panels  = Array.from(container.querySelectorAll('.ukhsa-tabs__panel'));

      if (tabList) tabList.setAttribute('role', 'tablist');

      tabs.forEach((t, i) => {
        t.setAttribute('role', 'tab');
        const preSelected = t.classList.contains('is-selected') || t.getAttribute('aria-selected') === 'true';
        t.setAttribute('aria-selected', preSelected ? 'true' : 'false');
        t.setAttribute('tabindex', preSelected ? '0' : '-1');
        t.classList.remove('is-selected');

        const target = t.getAttribute('data-target');
        if (target) t.setAttribute('aria-controls', target);
        if (!t.id) t.id = (container.id || 'ukhsa-tabs') + '-tab-' + i;
      });

      panels.forEach(p => {
        p.setAttribute('role', 'tabpanel');
        const labelledBy = tabs.find(t => t.getAttribute('data-target') === p.id);
        if (labelledBy) p.setAttribute('aria-labelledby', labelledBy.id);
      });

      container.addEventListener('click', function (e) {
        const btn = e.target.closest('.ukhsa-tabs__tab[data-target]');
        if (!btn || !container.contains(btn)) return;
        e.preventDefault();
        e.stopPropagation();
        activate(container, btn.getAttribute('data-target'), btn);
      }, true);

      container.addEventListener('keydown', function (e) {
        const current = container.querySelector('.ukhsa-tabs__tab[aria-selected=\"true\"]');
        if (!current) return;

        const list = Array.from(container.querySelectorAll('.ukhsa-tabs__tab'));
        const idx  = list.indexOf(current);
        let nextIdx = idx;

        switch (e.key) {
          case 'ArrowRight': nextIdx = (idx + 1) % list.length; break;
          case 'ArrowLeft':  nextIdx = (idx - 1 + list.length) % list.length; break;
          case 'Home':       nextIdx = 0; break;
          case 'End':        nextIdx = list.length - 1; break;
          default: return;
        }
        e.preventDefault();
        const nextTab = list[nextIdx];
        if (nextTab) activate(container, nextTab.getAttribute('data-target'), nextTab);
      }, false);

      requestAnimationFrame(() => setTimeout(() => recalcHeights(container), 30));
    }

    function bindAll(){ document.querySelectorAll('.lm-card-ukhsa .ukhsa-tabs').forEach(bind); }

    document.addEventListener('DOMContentLoaded', bindAll);
    document.addEventListener('shiny:connected', bindAll);
    document.addEventListener('shiny:recalculated', () => {
      bindAll();
      document.querySelectorAll('.lm-card-ukhsa .ukhsa-tabs').forEach(c => {
        requestAnimationFrame(() => setTimeout(() => recalcHeights(c), 30));
      });
    });
    document.addEventListener('shiny:value', bindAll, true);

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('ukhsa-tabs-init', bindAll);
    }
  })();
  "
  
# -----------------------
  # JS fix for pickerInput dropdown auto-close
  # -----------------------
  js_dropdown_fix <- "
    // Keep dropdownButton open when interacting with its content
    $(document).on('click', '.dropdown-menu', function(e) {
      e.stopPropagation();
    });

    // Prevent pickerInput (bootstrap-select) from bubbling clicks that close parent
    $(document).on('click', '.bootstrap-select', function(e) {
      e.stopPropagation();
    });
  "

  # Compose final CSS (card/tabs + global buttons + behaviour)
  css <- paste0(
    css_card_tabs,
    "\n\n/* === GLOBAL: radioGroupButtons equal-height === */\n",
    css_buttons_equal_height,
    "\n", css_buttons_label,
    "\n", css_buttons_mobile, "\n"
  )

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$script(htmltools::HTML(js))
  )
}