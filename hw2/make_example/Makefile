# This Makefile is used to document the workflow necessary to generate the full paper.html document

# Format:
#
# target1: dep1 dep2 ...
# 	cmd1
#   cmd2
#   ...
#
# target2: dep3 dep4 ...
# 	cmd1
#   cmd2
#   ...



paper.html: paper.Rmd tz/tz.png sim/sim.png sim/sim_tab_results.Rdata
	Rscript -e "library(rmarkdown);render('paper.Rmd')"

tz/tz.png: tz/plot_tz.R world/tz_world_mp.shp
	cd tz;\
	Rscript plot_tz.R

world/tz_world_mp.shp: tz_world_mp.zip
	unzip -o tz_world_mp.zip
	touch world/tz_world_mp.shp

tz_world_mp.zip:
	echo "Downloading TZ shapefile..."
	curl -O http://efele.net/maps/tz/world/tz_world_mp.zip

sim/sim_results.Rdata sim/sim_tab_results.Rdata: sim/run_sim.R
	cd sim;\
	Rscript run_sim.R

sim/sim.png: sim/plot_sim.R sim/sim_results.Rdata sim/sim_tab_results.Rdata
	cd sim;\
	Rscript plot_sim.R

.PHONY: clean
clean:
	rm -f sim/*.png
	rm -f sim/*.Rdata
	rm -f tz/tz.png
	rm -rf world/
	rm -f tz_world_mp.zip
	rm -f *.html
