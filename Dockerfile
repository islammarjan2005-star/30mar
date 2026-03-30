FROM 165562107270.dkr.ecr.eu-west-2.amazonaws.com/jupyterhub-visualisation-base:rv4

# Work inside /dashboard
WORKDIR /dashboard

# Install CRAN packages (kept early for caching)
RUN Rscript -e "options(warn = 2); \
  install.packages(c( \
    'shiny', \
    'shinyBS', \
    'bslib', \
    'dplyr', \
    'stringr', \
    'lubridate', \
    'ggplot2', \
    'plotly', \
    'DBI', \
    'RPostgres', \
    'shinyGovstyle', \
    'shinyWidgets', \
    'rlang', \
    'shiny.router', \
    'bs4Dash', \
    'pool', \
    'DT', \
    'reactable', \
    'remotes', \
    'maps' \
  ), clean = TRUE)"

# Copy in the app (assuming you run docker build from the repo root)
COPY dashboard/ ./

# Now that files exist in the image, install the vendored package
RUN Rscript -e "options(warn = 2); \
  remotes::install_local('vendor/dbtplotr-master', upgrade = 'never', dependencies = TRUE)"

EXPOSE 8888
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=8888)"]

USER dw-user