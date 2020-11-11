all:
	R -e 'devtools::document()' && R -e 'devtools::install()'
