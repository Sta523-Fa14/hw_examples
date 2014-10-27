make_example
================

To build this document it is necessary to do the follow:

1. Prepare timezone data and results
    1. Download shapefiles (`tz_world_mp.zip`) from http://efele.net/maps/tz/world/
    2. Unzip the shapefiles
    3. Change directory to `tz`
    4. Run `plot_tz.R`

2. Run $\pi$ simulation
    1. Change directory to `sim`
    2. Run `run_sim.R`
    3. Run `plot_sim.R`

3. Render/knit the paper