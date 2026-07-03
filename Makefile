readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF8")'

transparent-logo:
	cd man/figures && chmod +x ./trans.sh && ./trans.sh

