FROM rocker/r-base:4.0.0

RUN apt-get update -y && apt-get install -y \
  pandoc \
  texlive-latex-base \
  texlive-fonts-recommended \
  texlive-fonts-extra \
  texlive-latex-extra
RUN R -e "install.packages(c('rmarkdown', 'tinytex', 'docopt'))" 
WORKDIR app

COPY forms forms
COPY sections sections
COPY R R
COPY render.R render.R

ENTRYPOINT ["./render.R"]
CMD ["-h"]
