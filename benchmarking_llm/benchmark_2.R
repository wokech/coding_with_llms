# Benchmark 2: Use the package usmap to produce a plot

# Persona:  You are a GIS data analyst working on spatial mapping at a newly 
# funded startup. Context: You have been tasked with plotting county populations 
# in the USA. Action: Write an R script to load the required packages (usmap) and 
# datasets (county population data from the usmaps package), as well as plot the 
# map using the economist theme. Tone: Professional. 
# Output Format: Provide an R script.

# If not listed below then it means that it failed to run!!!

# NOTE: Only Perplexity worked! Issue may be that search is always on with Perplexity??!!

# Perplexity

# Load required packages
library(usmap)
library(ggplot2)
library(scales)

# Load county population data (2022 estimates from usmap)
data(countypop)  # countypop has columns: fips, abbr, county, pop_2022

# Plot the US county population map using usmap and ggplot2
p <- plot_usmap(
  regions = "counties",
  data = countypop,
  values = "pop_2022",
  color = "white",
  linewidth = 0.1
) +
  scale_fill_gradient(
    trans = "log",
    low = "white",
    high = "#0072B2",
    labels = label_number(big.mark = ","),
    name = "Population (2022)"
  ) +
  labs(
    title = "County Population in the United States (2022)",
    caption = "Data: US Census Bureau via usmap"
  ) +
  theme_economist() +  # economist theme from ggplot2/thematic packages
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_colorbar(barwidth = unit(10, "cm"))
  )

# Print the plot
print(p)



