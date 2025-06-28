FROM rocker/shiny:latest

# Install system dependencies (if needed)
RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
&& rm -rf /var/lib/apt/lists/*
  
  # Install R packages
  RUN R -e "install.packages(c('shiny', 'tidyverse', 'ggplot2'), repos='https://cloud.r-project.org/')"

# Copy app files to the image
COPY . /srv/shiny-server/
  
  # Set the working directory
  WORKDIR /srv/shiny-server/
  
  # Set permissions
  RUN chown -R shiny:shiny /srv/shiny-server

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
