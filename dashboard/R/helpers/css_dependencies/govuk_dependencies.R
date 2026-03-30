
# ------------------------------------------------------------
# GOV.UK Helpers for Shiny Apps
# ------------------------------------------------------------

#' Add GOV.UK Frontend CSS/JS dependencies and initialise components
#'
#' Injects the GOV.UK Frontend styles and JavaScript into your Shiny app,
#' and calls `GOVUKFrontend.initAll()` once the DOM is ready. Supports GOV.UK
#' or DBT design variants.
#'
#' @param style Character. Which style set to use: `"GOVUK"` or `"DBT"`.
#'   Defaults to `"GOVUK"`.
#' @param css_govuk Character. Filename for GOV.UK CSS (in `www/`).
#' @param css_dbt Character. Filename for DBT CSS (in `www/`).
#' @param js Character. Filename for GOV.UK JS (in `www/`).
#'
#' @return A `tagList()` containing `<link>` and `<script>` tags for inclusion
#'   in the UI.
#'
#' @examples
#' # In your UI:
#' fluidPage(
#'   govuk_dependencies(style = "DBT"),
#'   h1("My GOV.UK styled app")
#' )
#'
#' @export


# Minimal local-only loader: GOV.UK + ONS (no CDN, no addResourcePath, no parameters)
govuk_dependencies <- function(
  style       = c("GOVUK", "DBT"),
  css_govuk   = "govuk-frontend-6.0.0-beta.0.min.css",
  css_dbt     = "DBT-frontend-6.0.0-beta.0.min.css",
  js_govuk    = "govuk-frontend-6.0.0-beta.0.min.js",
  include_ons = TRUE,   # set FALSE if you want to disable ONS
  disable_ons_js = TRUE,
  enforce_card_tabs_css = TRUE
) {
  style <- match.arg(style)
  css_file <- if (style == "DBT") css_dbt else css_govuk

  tagList(

    # -------------------------------
    # ONS Design System assets FIRST
    # -------------------------------
    if (isTRUE(include_ons)) tagList(
      # ONS CSS (kept)
      tags$link(rel = "stylesheet", href = "ons/css/main.css"),

      # ONS JS (OPTIONAL): only include if you need its behaviours on this page
      if (!isTRUE(disable_ons_js)) tagList(
        # Base URL must be defined BEFORE main.js (ONS chunks resolve relative to this)
        tags$script(HTML("var ONS_assets_base_URL = 'ons/';")),
        tags$script(src = "ons/scripts/main.js", defer = NA)
      )
    ),

    # --------------------------------
    # GOV.UK Frontend assets LAST
    # --------------------------------
    tags$link(rel = "stylesheet", href = css_file),
    tags$script(src = js_govuk, defer = NA),

    # Re-initialise GOV.UK after Shiny DOM updates (loaded from /www)
    tags$script(src = "govuk-shiny-init.js"),

    # --------------------------------
    # Targeted footer override: suppress the crest pseudo-element
    # --------------------------------
    tags$style(HTML("
      /* Remove the CSS-injected crest in the footer */
      .govuk-footer__copyright-logo::before,
      .govuk-footer__copyright-logo:before {
        content: none !important;
        display: none !important;
        background: none !important;          /* covers background-image and background: currentcolor */
        -webkit-mask-image: none !important;  /* covers masking path used in @supports */
        mask-image: none !important;
      }
    ")),

    # --------------------------------
    # Scoped safety net for tab layout
    # --------------------------------
    if (isTRUE(enforce_card_tabs_css)) tags$style(HTML("
      /* Scoped to your GOV.UK/UKHSA-style card wrapper */
      .lm-card-ukhsa .govuk-tabs__list {
        display: flex;
        flex-wrap: wrap;
        border-bottom: 1px solid #b1b4b6;
        margin-bottom: 12px;
      }
      .lm-card-ukhsa .govuk-tabs__tab {
        display: inline-block;
        padding: 8px 12px;
      }
      .lm-card-ukhsa .govuk-tabs__panel {
        padding: 0; /* let inner content choose its spacing */
      }
      .lm-card-ukhsa .govuk-tabs__list-item--selected .govuk-tabs__tab {
        border-bottom: 3px solid #0b0c0c; /* matches GOV.UK */
      }
    "))
  )
}








#' Apply GOV.UK rebrand classes and meta tags
#'
#' Adds classes to `<html>` and `<body>` for GOV.UK rebranded styling,
#' and sets the theme colour meta tag.
#'
#' @return A `tagList()` with `<meta>` and `<script>` tags.
#'
#' @examples
#' fluidPage(
#'   govuk_apply_rebrand(),
#'   h1("Rebranded GOV.UK app")
#' )
#'
#' @export
govuk_apply_rebrand <- function() {
  tagList(
    tags$meta(name = "theme-color", content = "#1d70b8"),
    tags$script(HTML("
      (function () {
        document.documentElement.classList.add('govuk-template', 'govuk-template--rebranded');
        document.addEventListener('DOMContentLoaded', function () {
          document.body.classList.add('govuk-template__body');
        });
      })();
    "))
  )
}





DBT_widget_styling <- function() {
  tags$head(
    tags$style(HTML("
      /* --------------------------------------------- */
      /* DBT HEADING COLOURS                           */
      /* --------------------------------------------- */
      .mb-0.text-inherit { color: #CF102D !important; }
      .mb-0, .text-inherit { color: #CF102D !important; }
      h1.mb-0.text-inherit, h2.mb-0.text-inherit, h3.mb-0.text-inherit,
      h4.mb-0.text-inherit, h5.mb-0.text-inherit, h6.mb-0.text-inherit {
        color: #CF102D !important;
      }

      /* --------------------------------------------- */
      /* SLIDER STYLING                                */
      /* --------------------------------------------- */
      .irs--shiny .irs-bar { background-color: #FF0000; border-color: #FF0000; height: 8px; }
      .irs--shiny .irs-handle > i:first-child { background-color: #FF0000; border-color: #FF0000; }
      .irs--shiny .irs-handle:hover > i:first-child,
      .irs--shiny .irs-handle:focus > i:first-child {
        background-color: #CC0000; border-color: #CC0000;
      }
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
        background-color: #FF0000; color: #FFFFFF; border-color: #FF0000;
      }

      /* --------------------------------------------- */
      /* STAT CARD TITLES TWO-LINE LOCK                */
      /* --------------------------------------------- */
      .govuk-summary-card .govuk-heading-s {
        line-height: 1.25;
        min-height: calc(1.25em * 2);
        max-height: calc(1.25em * 2);
        overflow: hidden;
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
      }

      /* --------------------------------------------- */
      /* TOOLTIP (HOVER + CLICK HYBRID)                */
      /* --------------------------------------------- */

      /* Base tooltip (hidden) */
      .govuk-info-wrap .govuk-info-tooltip {
        opacity: 0;
        transform: translate(-8px, 28px);
        transition: opacity .12s ease-out, transform .12s ease-out;
        pointer-events: none;
        z-index: 1000;
      }

      /* Shown when hover delay fires */
      .govuk-info-wrap.hover-open .govuk-info-tooltip {
        opacity: 1;
        transform: translate(-8px, 24px);
      }

      /* Shown AND interactive when clicked */
      .govuk-info-wrap.open .govuk-info-tooltip {
        opacity: 1;
        transform: translate(-8px, 24px);
        pointer-events: auto;
      }

      /* Focus ring on info button */
      .govuk-info-btn:focus {
        outline: 3px solid #fd0;
        outline-offset: 2px;
        box-shadow: 0 0 0 2px #0b0c0c;
      }
    ")),

    tags$script(HTML("
      // Hover delay (ms)
      const HOVER_DELAY = 500;
      let hoverTimer = null;

      document.addEventListener('mouseover', function(e) {
        const wrap = e.target.closest('.govuk-info-wrap');
        if (!wrap) return;

        // If already open via click, ignore hover
        if (wrap.classList.contains('open')) return;

        // Start delayed hover open
        hoverTimer = setTimeout(() => {
          wrap.classList.add('hover-open');
        }, HOVER_DELAY);
      });

      document.addEventListener('mouseout', function(e) {
        const wrap = e.target.closest('.govuk-info-wrap');
        if (!wrap) return;

        clearTimeout(hoverTimer);

        // If not click-open, close hover-open state
        if (!wrap.classList.contains('open')) {
          wrap.classList.remove('hover-open');
        }
      });

      // CLICK HANDLING
      document.addEventListener('click', function(e) {
        const btn = e.target.closest('.govuk-info-btn');

        // Clicking the info button
        if (btn) {
          const wrap = btn.closest('.govuk-info-wrap');

          // Cancel hover state
          clearTimeout(hoverTimer);
          wrap.classList.remove('hover-open');

          // Permanently open (interactive)
          wrap.classList.add('open');
          return;
        }

        // Click outside closes all
        document.querySelectorAll('.govuk-info-wrap.open')
                .forEach(el => el.classList.remove('open'));
      });
    "))
  )
}


#' Add GOV.UK head assets (favicons, manifest)
#'
#' Injects favicon links, Apple touch icons, and manifest references into the
#' document head. Adjust paths to match your `www/assets/` structure.
#'
#' @return A `tagList()` with `<link>` tags for favicons and manifest.
#'
#' @examples
#' fluidPage(
#'   govuk_head_assets(),
#'   h1("App with GOV.UK favicons")
#' )
#'
#' @export
govuk_head_assets <- function() {
  tagList(
    tags$link(rel = "manifest", href = "assets/manifest.json"),
    tags$link(rel = "icon", type = "image/png",
              href = "assets/images/favicon-32x32.png", sizes = "32x32"),
    tags$link(rel = "icon", type = "image/png",
              href = "assets/images/favicon-16x16.png", sizes = "16x16"),
    tags$link(rel = "apple-touch-icon", href = "assets/images/apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "assets/rebrand/favicon.svg")
  )
}
