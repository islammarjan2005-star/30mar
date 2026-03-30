# Side Nav Asset ----
govuk_toc_assets <- function(
  NAVBAR_HEIGHT_PX     = 149,   # header height at top of page
  MIN_OFFSET_PX        = 12,    # smallest top offset after shrink
  DRAWER_BREAKPOINT_PX = 1500   # viewport width threshold for drawer mode
) {
  stopifnot(is.numeric(NAVBAR_HEIGHT_PX), NAVBAR_HEIGHT_PX > 0)
  stopifnot(is.numeric(MIN_OFFSET_PX), MIN_OFFSET_PX > 0, MIN_OFFSET_PX <= NAVBAR_HEIGHT_PX)
  stopifnot(is.numeric(DRAWER_BREAKPOINT_PX), DRAWER_BREAKPOINT_PX > 0)

  # -----------------------
  # CSS
  # -----------------------
  css <- sprintf("
  html { scroll-behavior: smooth; }

  /* Global custom properties for FAB & drawer geometry */
  :root {
    --fab-size: 44px;
    --fab-radius: calc(var(--fab-size) / 2);  /* 22px for size 44 */
    --drawer-pad-left: 12px;                  /* keep in sync with drawer padding-left */
    --drawer-pad-top: 12px;                   /* keep in sync with drawer padding-top  */
  }

  /* GOV.UK container stays centred */
  .govuk-width-container {
    position: relative;
    max-width: 960px; /* GOV.UK default */
    margin: 0 auto;
  }

  /* Base overlay sidebar (default state: expanded) — blend into background */
  .sidebar {
    position: fixed;
    top: calc(var(--current-offset, %1$dpx) + %2$dpx);
    left: max(12px, calc((100%% - 960px) / 2 - 240px));
    width: 220px;
    max-height: calc(100vh - var(--current-offset, %1$dpx) - %2$dpx * 2);
    overflow: auto;

    /* blend (no box-shadow/border) */
    background: transparent;
    box-shadow: none;
    border: none;
    padding-right: 0.5rem;

    display: flex; flex-direction: column;
    z-index: 1000; /* draw over content */
  }

  /* Drawer mode — elevation, padding, and rounded top-right corner */
  .sidebar.sidebar--drawer {
    left: 0; /* off-canvas from page edge */
    width: min(85vw, 360px);
    height: calc(100vh - calc(var(--current-offset, %1$dpx) + %2$dpx));
    max-height: none;
    transform: translateX(-100%%);   /* hidden by default in drawer mode */
    transition: transform .25s ease; /* controlled via JS for first-load */
    will-change: transform;

    /* elevate only in drawer mode */
    background: #fff;
    box-shadow: 0 0 0 1px rgba(0,0,0,.08), 0 12px 24px rgba(0,0,0,.24);

    /* comfortable spacing inside drawer (controls FAB geometry too) */
    padding-top: var(--drawer-pad-top);
    padding-left: var(--drawer-pad-left);
    padding-right: 12px;
    padding-bottom: 12px;

    /* Rounded top-right = FAB radius + left padding */
    border-top-right-radius: calc(var(--fab-radius) + var(--drawer-pad-left));
  }
  .sidebar.sidebar--drawer.sidebar--open { transform: translateX(0); }

  /* NEW: disable transitions for initial closed state on small screens */
  .sidebar.sidebar--drawer.sidebar--no-transition { transition: none !important; }

  /* Scrim over the page (no click-to-close) */
  .sidebar__scrim {
    position: fixed; inset: 0;
    background: rgba(0,0,0,.35);
    z-index: 999;
    opacity: 0; pointer-events: none; /* keep non-interactive to avoid accidental closes */
    transition: opacity .2s ease;
  }
  .sidebar__scrim--visible { opacity: 1; pointer-events: auto; }

  /* -------- FABs --------
     Two FABs per sidebar:
     - .sidebar__fab--panel: inside the sidebar (rides the transform smoothly)
     - .sidebar__fab--window: left-edge 'tab' (shown when drawer is closed)
  */

  /* Shared look — minimal grey palette */
  .sidebar__fab {
    width: var(--fab-size);
    min-height: var(--fab-size);
    display: inline-flex; align-items: center; justify-content: center;
    background: #f3f2f1;              /* light grey */
    color: #0b0c0c;                    /* dark grey icon */
    border: 1px solid rgba(0,0,0,.15); /* subtle outline */
    box-shadow: 0 2px 6px rgba(0,0,0,.08);
    cursor: pointer;
  }
  .sidebar__fab:hover  { background: #e6e6e6; }
  .sidebar__fab:active { background: #dbdbdb; }
  .sidebar__fab:focus-visible {
    outline: 3px solid #ffbf47; outline-offset: 2px;  /* GOV.UK focus colour */
  }
  .sidebar__fab svg {
    width: 22px; height: 22px; display: block; fill: currentColor;
  }

  /* Panel FAB (inside panel) — circular, positioned by padding geometry
     Centre distance from right edge = FAB radius + left padding.
     Right CSS offset = desired-centre - radius = (r + padLeft) - r = padLeft
  */
  .sidebar__fab--panel {
    position: absolute;
    top: var(--drawer-pad-top);
    right: var(--drawer-pad-left);
    border-radius: 9999px;   /* circular inside the panel */
    z-index: 10;              /* above nav inside the panel */
  }

  /* Window FAB (left-edge 'tab'): flat left, rounded right (hamburger only)
     Vertical alignment matches the panel FAB (adds drawer top padding).
  */
  .sidebar__fab--window {
    position: fixed;
    left: 0; /* pinned to viewport left edge */
    top: calc(var(--current-offset, %1$dpx) + %2$dpx + var(--drawer-pad-top));
    z-index: 1002; /* above sidebar + scrim */
    transition: top .1s linear;

    /* Tab silhouette: flat left, rounded right */
    border-radius: 0 var(--fab-radius) var(--fab-radius) 0;

    padding: 6px 10px;
    min-width: var(--fab-size);
  }

  /* Keep ONS nav flush */
  .sidebar .ons-section-nav { margin: 0; }

  /* Avoid heading hidden under header+offset when scrolling */
  section[id] { scroll-margin-top: calc(var(--current-offset, %1$dpx) + %2$dpx); }

  /* Sidebar footer pinned */
  .sidebar__footer {
    margin-top: auto; position: sticky; bottom: 0;
    background: #f3f2f1; border-top: 1px solid rgba(0,0,0,0.08); padding: 8px;
  }

  /* ---- ACTIVE look (ONS-like) ---- */
  .ons-section-nav__item--active > .ons-section-nav__link,
  .ons-section-nav__item--active > .nav-link {
    color: #7D0A1B !important;
    font-weight: 700 !important;
    background-color: rgba(243, 242, 241, 0.12) !important;
    border-left: 6px solid #B50E27 !important;
    padding-left: 12px !important;
    text-decoration: none !important;
  }

  /* Non-active links */
  .ons-section-nav__item > .ons-section-nav__link,
  .ons-section-nav__item > .nav-link {
    display: block; padding: 6px 8px; text-decoration: none;
    transition: background-color .2s ease, color .2s ease;
  }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link { color: #CF102D !important; }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link:visited,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link:visited { color: #CF102D !important; }
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .ons-section-nav__link:hover,
  .sidebar .ons-section-nav__item:not(.ons-section-nav__item--active) > .nav-link:hover {
    color: #7D0A1B !important; text-decoration: underline solid #7D0A1B 2px;
  }
  .sidebar .nav-link.active {
    color: inherit !important; background: transparent !important; border-left: none !important;
    text-transform: none !important; font-weight: inherit !important;
  }

  /* Back to top styling */
  .govuk-back-to-top { display: inline-flex; align-items: center; gap: 6px; font-weight: bold; text-decoration: none; color: #1d70b8; }
  .govuk-back-to-top:hover, .govuk-back-to-top:focus { color: #003078; text-decoration: underline; }
  .govuk-back-to-top__icon { width: 25px; height: 25px; display: inline-block; }
  ",
  NAVBAR_HEIGHT_PX, MIN_OFFSET_PX)

  # -----------------------
  # JavaScript (first-load no-animation close; FAB only closes; keep open on link click)
  # -----------------------
  js <- paste0(
"(function(){
  const NAVBAR_HEIGHT = ", NAVBAR_HEIGHT_PX, ";
  const MIN_OFFSET    = ", MIN_OFFSET_PX, ";
  const DRAW_BP       = ", DRAWER_BREAKPOINT_PX, ";
  const SHRINK_RANGE  = NAVBAR_HEIGHT - MIN_OFFSET;

  // Match CSS transition
  const DRAWER_TRANSITION_MS = 250;
  const FAB_SWAP_DELAY_MS    = 60;

  function computeOffset(){
    var y = window.pageYOffset || document.documentElement.scrollTop || 0;
    var shrink = Math.min(y, SHRINK_RANGE);
    return NAVBAR_HEIGHT - shrink;
  }
  var lastApplied = -1;
  function applyOffsetOnce(){
    var o = computeOffset();
    if (Math.abs(o - lastApplied) >= 1){
      document.documentElement.style.setProperty('--current-offset', Math.round(o) + 'px');
      lastApplied = o;
    }
  }

  // Router-aware visibility check
  function isTreeHidden(el){
    for (let n = el; n; n = n.parentElement){
      const s = window.getComputedStyle(n);
      if (s.display === 'none' || s.visibility === 'hidden' || n.hidden) return true;
    }
    return false;
  }

  function getRouteAndFragment(){
    var h = window.location.hash || '';
    if (!h.startsWith('#!/')) return { route:'', fragment:'' };
    var raw = h.slice(3);
    var parts = raw.split('#');
    return { route: parts[0]||'', fragment: parts[1]||'' };
  }
  function setRouteFragment(route, fragment){
    var newHash = '#!/' + (route||'') + (fragment ? ('#'+fragment) : '');
    history.replaceState(null, document.title, newHash);
  }

  function scrollToSection(id){
    var target = document.getElementById(id);
    if (!target) return;
    var currentOffset = computeOffset();
    var top = target.getBoundingClientRect().top + window.pageYOffset - currentOffset - 8;
    var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    window.scrollTo({ top: top, behavior: reduce ? 'auto' : 'smooth' });
  }

  function updateActive(sidebar, sections, links) {
    if (!sections.length || !links.length) return;
    const offset = computeOffset();
    const viewportTop = offset + 8;
    let bestIdx = -1, bestDist = Infinity;
    for (let i = 0; i < sections.length; i++) {
      const rect = sections[i].getBoundingClientRect();
      const dist = Math.abs(rect.top - viewportTop);
      const isCandidate = (rect.bottom > viewportTop) && (rect.top <= window.innerHeight * 0.6);
      if (isCandidate && dist < bestDist) { bestDist = dist; bestIdx = i; }
    }
    if (bestIdx === -1) {
      for (let i = 0; i < sections.length; i++) {
        const rect = sections[i].getBoundingClientRect();
        const dist = Math.abs(rect.top - viewportTop);
        if (dist < bestDist) { bestDist = dist; bestIdx = i; }
      }
    }
    const items = Array.from(sidebar.querySelectorAll('.ons-section-nav__item'));
    items.forEach(li => li.classList.remove('ons-section-nav__item--active'));
    links.forEach(a => a.removeAttribute('aria-current'));
    if (bestIdx !== -1) {
      const id = sections[bestIdx].id;
      const match = sidebar.querySelector('.nav-link[data-section=\"' + id + '\"]');
      if (match) {
        const li = match.closest('.ons-section-nav__item');
        if (li) li.classList.add('ons-section-nav__item--active');
        match.setAttribute('aria-current', 'location');
      }
    }
  }

  function ensureBackToTop(sidebar, links, sections){
    if (sidebar.querySelector('.sidebar__footer')) return;
    var footer = document.createElement('div');
    footer.className = 'sidebar__footer';
    footer.innerHTML = [
      '<a href=\"#\" class=\"govuk-link govuk-back-to-top\" aria-label=\"Back to top\">',
      '  <svg class=\"govuk-back-to-top__icon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">',
      '    <path d=\"M12 5l-6 6h4v8h4v-8h4z\" fill=\"currentColor\"/>',
      '  </svg>',
      '  <span>Back to top</span>',
      '</a>'
    ].join('');
    sidebar.appendChild(footer);
    var link = footer.querySelector('.govuk-back-to-top');
    link.addEventListener('click', function(ev){
      ev.preventDefault();
      var rf = getRouteAndFragment();
      setRouteFragment(rf.route, '');
      links.forEach(a => a.removeAttribute('aria-current'));
      Array.from(sidebar.querySelectorAll('.ons-section-nav__item'))
           .forEach(li => li.classList.remove('ons-section-nav__item--active'));
      var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
      window.scrollTo({ top: 0, behavior: reduce ? 'auto' : 'smooth' });
      requestAnimationFrame(function(){ applyOffsetOnce(); updateActive(sidebar, sections, links); });
    });
  }

  function uniqueId(){ return 'sidebar-' + Math.random().toString(36).slice(2); }
  function ensureScrim(){ let s = document.querySelector('.sidebar__scrim'); if (!s){ s = document.createElement('div'); s.className = 'sidebar__scrim'; document.body.appendChild(s); } return s; }

  // Create two FABs per sidebar: inside panel & fixed left-edge tab (hamburger only)
  function ensureFabs(sidebar){
    // Panel FAB (inside)
    let panelFab = sidebar.querySelector('.sidebar__fab--panel');
    if (!panelFab){
      panelFab = document.createElement('button');
      panelFab.type = 'button';
      panelFab.className = 'sidebar__fab sidebar__fab--panel';
      panelFab.setAttribute('aria-controls', sidebar.id);
      panelFab.setAttribute('aria-label', 'Toggle sections');
      panelFab.innerHTML = '<svg viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">'
                         + '  <rect x=\"3\" y=\"6\"  width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '  <rect x=\"3\" y=\"11\" width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '  <rect x=\"3\" y=\"16\" width=\"18\" height=\"2\" rx=\"1\"></rect>'
                         + '</svg>';
      sidebar.appendChild(panelFab);
    }

    // Window FAB (fixed tab; hamburger only)
    let windowFab = document.querySelector('.sidebar__fab--window[data-for=\"' + sidebar.id + '\"]');
    if (!windowFab){
      windowFab = document.createElement('button');
      windowFab.type = 'button';
      windowFab.className = 'sidebar__fab sidebar__fab--window';
      windowFab.setAttribute('data-for', sidebar.id);
      windowFab.setAttribute('aria-controls', sidebar.id);
      windowFab.setAttribute('aria-label', 'Toggle sections');
      windowFab.innerHTML =
        '<svg viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\">' +
        '  <rect x=\"3\" y=\"6\"  width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '  <rect x=\"3\" y=\"11\" width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '  <rect x=\"3\" y=\"16\" width=\"18\" height=\"2\" rx=\"1\"></rect>' +
        '</svg>';
      document.body.appendChild(windowFab);
    }
    return { panelFab, windowFab };
  }

  // Show/hide correct FAB; IMPORTANT: no FABs in overlay mode
  function updateFabState(sidebar, fabs){
    const { panelFab, windowFab } = fabs;
    const visible = !isTreeHidden(sidebar);
    if (!visible){ panelFab.style.display = 'none'; windowFab.style.display = 'none'; return; }

    const animClosing = sidebar.classList.contains('sidebar--anim-closing');
    const inDrawer    = sidebar.classList.contains('sidebar--drawer');
    const isOpen      = sidebar.classList.contains('sidebar--open');

    if (animClosing){
      // During close animation keep panel FAB visible, window FAB hidden
      panelFab.style.display  = 'inline-flex';
      windowFab.style.display = 'none';
      return;
    }

    if (!inDrawer) {
      // Overlay: NO FABs
      panelFab.style.display  = 'none';
      windowFab.style.display = 'none';
    } else if (isOpen) {
      // Drawer open: show panel FAB (inside)
      panelFab.style.display  = 'inline-flex';
      windowFab.style.display = 'none';
    } else {
      // Drawer closed: show window FAB (left-edge tab)
      panelFab.style.display  = 'none';
      windowFab.style.display = 'inline-flex';
    }
  }

  // --- Drawer/Overlay helpers ---
  let closeSwapTimer = null;
  function clearCloseTimer(){ if (closeSwapTimer){ clearTimeout(closeSwapTimer); closeSwapTimer = null; } }

  function openDrawer(sidebar, scrim, fabs){
    sidebar.classList.add('sidebar--drawer');
    sidebar.classList.remove('sidebar--anim-closing'); // ensure flag cleared
    sidebar.classList.add('sidebar--open');
    scrim.classList.add('sidebar__scrim--visible');
    clearCloseTimer();
    updateFabState(sidebar, fabs);
    const firstLink = sidebar.querySelector('.nav-link, .ons-section-nav__link, a, button');
    if (firstLink) firstLink.focus();
  }

  function closeDrawer(sidebar, scrim, fabs){
    // Stay in drawer; begin close animation
    sidebar.classList.add('sidebar--anim-closing'); // keep panel FAB visible while sliding out
    sidebar.classList.remove('sidebar--open');
    scrim.classList.remove('sidebar__scrim--visible');
    clearCloseTimer();

    // Keep panel FAB during transition
    updateFabState(sidebar, fabs);

    const onEnd = function(ev){
      if (ev.propertyName !== 'transform') return;
      sidebar.removeEventListener('transitionend', onEnd);
      clearCloseTimer();
      sidebar.classList.remove('sidebar--anim-closing');
      updateFabState(sidebar, fabs); // now show window FAB
    };
    sidebar.addEventListener('transitionend', onEnd);

    // Safety swap if transitionend is missed
    closeSwapTimer = setTimeout(function(){
      sidebar.classList.remove('sidebar--anim-closing');
      updateFabState(sidebar, fabs);
    }, DRAWER_TRANSITION_MS + FAB_SWAP_DELAY_MS);
  }

  // Breakpoint check
  function shouldUseDrawer(){ return window.innerWidth < DRAW_BP; }

  // First-load entry to drawer CLOSED with NO TRANSITION
  function enterDrawerClosedNoAnim(sidebar, scrim, fabs){
    sidebar.classList.add('sidebar--drawer', 'sidebar--no-transition');
    sidebar.classList.remove('sidebar--open', 'sidebar--anim-closing');
    scrim.classList.remove('sidebar__scrim--visible');
    updateFabState(sidebar, fabs);
    // Force reflow, then remove the no-transition flag
    void sidebar.offsetHeight;
    requestAnimationFrame(function(){ sidebar.classList.remove('sidebar--no-transition'); });
  }

  // Enforce the right mode by breakpoint
  // INITIAL LOAD: if we enter drawer mode, start CLOSED (no animation)
  // SUBSEQUENT RESIZE: overlay->drawer transitions OPEN the drawer
  function enforceMode(sidebar, scrim, fabs, isInitial){
    if (!sidebar) return;
    const visible = !isTreeHidden(sidebar);
    if (!visible){ updateFabState(sidebar, fabs); return; }

    if (shouldUseDrawer()){
      if (!sidebar.classList.contains('sidebar--drawer')){
        // Entering drawer mode from overlay or first load
        if (isInitial || !sidebar.__seenOverlay){
          enterDrawerClosedNoAnim(sidebar, scrim, fabs);  // <-- no animation
        } else {
          openDrawer(sidebar, scrim, fabs);               // <-- open on later transition
        }
      } else {
        // Already in drawer: respect current open/closed state
        updateFabState(sidebar, fabs);
      }
    } else {
      // Force overlay (non-drawer) when width >= breakpoint
      if (sidebar.classList.contains('sidebar--drawer')){
        sidebar.classList.remove('sidebar--drawer', 'sidebar--open', 'sidebar--anim-closing', 'sidebar--no-transition');
        scrim.classList.remove('sidebar__scrim--visible');
      }
      // Mark that we've rendered overlay at least once
      sidebar.__seenOverlay = true;
      updateFabState(sidebar, fabs);
    }
  }

  function bindSidebar(sidebar){
    if (!sidebar) return;
    if (!sidebar.id) sidebar.id = uniqueId();
    if (sidebar.__tocBound) return;
    sidebar.__tocBound = true;
    sidebar.__seenOverlay = false;  // track if overlay has been visible this session

    const links = Array.from(sidebar.querySelectorAll('.nav-link[data-section]'));
    if (!links.length) { sidebar.__tocBound = false; setTimeout(function(){ bindSidebar(sidebar); }, 50); return; }

    const scrim = ensureScrim();
    const fabs  = ensureFabs(sidebar);
    const { panelFab, windowFab } = fabs;

    // Toggle only active in drawer mode (small widths)
    function toggleDrawer(){
      if (!shouldUseDrawer()){
        // Overlay enforced by breakpoint: ignore FAB clicks
        return;
      }
      if (!sidebar.classList.contains('sidebar--drawer')) {
        // From overlay on small width: first click ends in drawer-closed (guard)
        sidebar.classList.add('sidebar--drawer', 'sidebar--no-transition');
        sidebar.classList.add('sidebar--anim-closing'); // harmless when no-transition is set
        sidebar.classList.remove('sidebar--open');
        scrim.classList.remove('sidebar__scrim--visible');
        updateFabState(sidebar, fabs);
        void sidebar.offsetHeight;
        requestAnimationFrame(function(){ sidebar.classList.remove('sidebar--no-transition'); });
        return;
      }

      const isOpen = sidebar.classList.contains('sidebar--open');
      isOpen ? closeDrawer(sidebar, scrim, fabs) : openDrawer(sidebar, scrim, fabs);
    }

    function onFabClick(ev){ ev.preventDefault(); if (isTreeHidden(sidebar)) return; toggleDrawer(); }
    panelFab.addEventListener('click', onFabClick);
    windowFab.addEventListener('click', onFabClick);

    // Clicking nav links SHOULD NOT close the drawer.
    // Keep drawer open and let smooth scroll proceed.
    sidebar.addEventListener('click', function(ev){
      const a = ev.target.closest('a[data-section]');
      if (!a) return;
      ev.preventDefault();
      const section = a.getAttribute('data-section');
      const rf = getRouteAndFragment();
      setRouteFragment(rf.route, section);
      scrollToSection(section);
      // DO NOT close the drawer here.
    });

    // Sections list for scroll-spy
    let sections = links.map(a => document.getElementById(a.getAttribute('data-section'))).filter(Boolean);
    if (!sections.length){ sidebar.__tocBound = false; setTimeout(function(){ bindSidebar(sidebar); }, 50); return; }

    ensureBackToTop(sidebar, links, sections);

    // Initial fragment sync
    const rf = getRouteAndFragment();
    if (rf.fragment){
      const initial = sidebar.querySelector('.nav-link[data-section=\"' + rf.fragment + '\"]');
      if (initial){
        links.forEach(l => {
          l.classList.remove('active');
          l.removeAttribute('aria-current');
          const pli = l.closest('.ons-section-nav__item');
          if (pli) pli.classList.remove('ons-section-nav__item--active');
        });
        initial.setAttribute('aria-current', 'location');
        const li = initial.closest('.ons-section-nav__item');
        if (li) li.classList.add('ons-section-nav__item--active');
        setTimeout(function(){ scrollToSection(rf.fragment); }, 0);
      }
    }

    // Scroll spy, initial mode & FAB state
    let ticking = false;
    function onScrollSpy(){ if (!ticking){ ticking = true; requestAnimationFrame(function(){ applyOffsetOnce(); updateActive(sidebar, sections, links); ticking = false; }); } }
    applyOffsetOnce(); updateActive(sidebar, sections, links);
    enforceMode(sidebar, scrim, fabs, /* isInitial: */ true);   // enforce mode by breakpoint on load

    window.addEventListener('scroll', onScrollSpy, { passive: true });
    window.addEventListener('resize', function(){ applyOffsetOnce(); enforceMode(sidebar, scrim, fabs, /* isInitial: */ false); }, { passive: true });

    // Ensure FAB updates at the end of transitions
    sidebar.addEventListener('transitionend', function(ev){
      if (ev.propertyName === 'transform') updateFabState(sidebar, fabs);
    });
  }

  // Bind all sidebars
  function bindAll(){ Array.from(document.querySelectorAll('.sidebar')).forEach(bindSidebar); }

  // Router-aware re-evaluation (hash routers + DOM visibility) -> re-enforce mode
  function refreshAll(){
    Array.from(document.querySelectorAll('.sidebar')).forEach(sb => {
      if (!sb.__tocBound) return;
      const scrim = ensureScrim();
      const panelFab = sb.querySelector('.sidebar__fab--panel');
      const windowFab = document.querySelector('.sidebar__fab--window[data-for=\"' + sb.id + '\"]');
      if (panelFab && windowFab){
        const fabs = { panelFab, windowFab };
        enforceMode(sb, scrim, fabs, /* isInitial: */ false);
      }
    });
  }

  // MutationObserver watches for display/class/hidden changes (router)
  let moQueued = false;
  const mo = new MutationObserver(function(){
    if (!moQueued){
      moQueued = true;
      requestAnimationFrame(function(){ moQueued = false; refreshAll(); });
    }
  });

  document.addEventListener('DOMContentLoaded', function(){
    applyOffsetOnce();
    bindAll();
    // Do not force refreshAll immediately (avoids accidental auto-open on initial small width).
    mo.observe(document.body, { subtree: true, attributes: true, attributeFilter: ['style','class','hidden'] });
  });
  window.addEventListener('hashchange', function(){ setTimeout(refreshAll, 0); }, { passive: true });
})();"
  )

  # -----------------------
  # Scoped override to remove GOV.UK focus halo inside sidebar
  # -----------------------
  local_focus_override_css <- "
  .sidebar a.nav-link:focus,
  .sidebar a.nav-link:active,
  .sidebar a.nav-link:focus-visible,
  .sidebar .ons-section-nav__link:focus,
  .sidebar .ons-section-nav__link:active,
  .sidebar .ons-section-nav__link:focus-visible,
  .sidebar .govuk-link:focus,
  .sidebar .govuk-link:active,
  .sidebar .govuk-link:focus-visible,
  .sidebar a:focus,
  .sidebar a:active,
  .sidebar a:focus-visible {
    background-color: transparent !important;
    color: inherit !important;
    box-shadow: none !important; /* removes GOV.UK focus halo */
    outline: none !important;
    text-decoration: none !important;
  }
  "

  htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$style(htmltools::HTML(local_focus_override_css)),
    htmltools::tags$script(htmltools::HTML(js))
  )
}

