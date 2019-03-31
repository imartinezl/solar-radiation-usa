# National Solar Radiation Data Base Visualization

### Summary
This repository contains the code for the [DataViz Battle for the month of March 2019](https://www.reddit.com/r/dataisbeautiful/comments/axknia/battle_dataviz_battle_for_the_month_of_march_2019/)

The interactive visualization is available [here](https://imartinezl.shinyapps.io/solar-radiation-usa/), and it was built using R and the packages Shiny, ggplot2 and leaflet.

Every month, the subreddit [r/dataisbeautiful/](https://www.reddit.com/r/dataisbeautiful/) challenges people across the globe to work with a new dataset. These challenges range in difficulty, filesize, and analysis required. What I love about this competition is the freedom it gives the participants to visualize the dataset. As a matter of fact, this is the only rule you need to follow:

> Use the dataset. Work with the data, perform the analysis, and generate a visual. It is entirely up to each participant to decide how to present the visual.

### DataViz Thinking Process

At first, I did an initial exploration of the dataset, looking for any interesting patterns over different years.

Unfortunately, my desire to find relevant correlations over time vanished when I noticed that the "*...TMY data set was composed of 12 typical meteorological months that are concatenated without modification to form a single year...*", which meant that the **year** feature was not important at all, and as a result, it had to be ignored and it was not possible to extract patterns over time.

So, once I discarted the *"evolution-over-time"* plot, I just focused on tyring to give a good understanding of the data through a clear visualization.

### Stations Map

TMY3 stations over all United States were included as circle-markers in an interactive map built with the library Leaflet for R. When an station is hovered, information such as name, location, state and class is pop-up. Moreover, when an station is clicked, the server loads the corresponding *.csv* data file about that station.

![](docs/map_screenshot.png)

### Solar Radiation Data

The solar radiation data is visualized on three different polar graphs, which are described below.
The user can select the variable of interest from the input selector.

|     | Plot  | Polar Axis  | Radial Axis  | Color Axis  |
|---|---|:---:|:---:|:---:|
| 1 | Value along the year | Hour | Value | Day |
| 2 | Maximum value at each day | Day | Max Value | - |
| 3 | Value on an specific day | Hour | Value | - |

![](docs/plot_screenshot.png)


### Made with
- [R](https://www.r-project.org/) - Programming Language / 3.5.2
- [RStudio](https://www.rstudio.com/) - IDE for R / 1.1.463 
- [shiny](https://shiny.rstudio.com/) - Interactive web apps with R / 1.2.0
- [ggplot2](https://ggplot2.tidyverse.org/) - Library for graphics / 3.1.0
- [leaflet](https://rstudio.github.io/leaflet/) - Library for interactive maps / 2.0.2
- [dplyr](https://dplyr.tidyverse.org/) - A grammar of data manipulation / 0.7.8 
- [lubridate](https://lubridate.tidyverse.org/) - Library for date-times / 1.7.4
- [hrbrthemes](https://hrbrmstr.github.io/hrbrthemes/) - Typography-centric themes for ggplot2 / 0.6.0
