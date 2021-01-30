# ESIF data cleaning

<!-- badges: start -->
<!-- badges: end -->

## Co 

Sada skriptů na transformaci geografických údajů o projektech EU fondů (ESIF) v ČR z obodbí 2014-2020. Cíl je rozpadnout každý projekt na obecní úroveň podle informací patrných buď ze zadaných geografických údajů, nebo z textových polí a informací o projektu a příjemci.

## Jak s tím pracovat

- `dev.Rmd` a [online](esif-data-cleaning.netlify.app/dev.html): základní informace k architektuře (anglicky)
- `how-to-repeat.Rmd` a [online](esif-data-cleaning.netlify.app/how-to-repeat.html): kompletní návod na zopakování postupu s novými daty

## Co je kde

### Konfigurace

- celková konfigurace: `config.yml`
- konfigurace webového výstupu: `_site.yml`
- konfigurace Netlify: `.netlify/`

### Build skripty, viz [návod](how-to-repeat.html)

- `build-and-render.R`: celkový build v R
- `build-and-render.sh`: totéž v Bashi

### R a Rmarkdown skripty

- 0N_*.R: skripty, které načítají vstupní data 
- NN_*.Rmd
- ostatní `*.Rmd` další dokumentace a nástroje, které se též

### Výstup

- Arrow dataset v `data-output`
- Excel (chunked) v `data-export`
- dokumentace exportu v `codebook.Rmd` a [online](codebook.html)
- web na <https://esif-data-cleaning.netlify.app>
