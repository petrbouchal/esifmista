#!/usr/bin/env zsh

ulimit -s 32768

if
  Rscript -e "rmarkdown::render_site()";
then
  echo "Deploying"
  netlify deploy --dir="docs" --prod;
else
  echo "Build failed";
fi
