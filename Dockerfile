# tar in en baseimage ifrån dockerhub. Denna innehåller plumber vid installation.
FROM rstudio/plumber

# skapar en mapp i dockercontainern som sätts till startpunkt (du kan kalla den för vad du vill)
WORKDIR /api-caller/

RUN R -e "install.packages(c('glmnet','glmnetUtils' ,'quantreg'))"

# kopierar appen från lokalt till ditt nya WD
COPY /app/ .

# startar en R session - run_plumber.
ENTRYPOINT ["Rscript" , "run_plumber.R" ]