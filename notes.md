# Notes and bits - storage

## EC NUTS codes

```r
tf <- tempfile()
download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx", tf)
readxl::read_excel(tf, sheet = 6) %>% View()
filter(str_detect(`NUTS 3 CODE`, "CZ"))
```
