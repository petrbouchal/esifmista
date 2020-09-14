#!/usr/bin/env zsh

ulimit -s 32768

if 
  Rscript -e "rmarkdown::render_site()"
  Rscript -e "rmarkdown::render('codebook.Rmd', output_format=c('word_document', 'pdf_document'))";
then
  echo "Deploying"
  netlify deploy --dir="docs" --prod;
else
  echo "Build failed";
fi
