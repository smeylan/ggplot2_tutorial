library(ggplot2)
#if this fails, try install.packages("ggplot2")
# also make sure you have colorRamps, reshape, and grid

#create the directories that we will save figures into later
dir.create('results', showWarnings=F)
dir.create('figures', showWarnings=F)

# source('testPlot2.R') -- this requires an additional script not available in the repo

# Generate some data that we will use throughout the tutorial. 
# y ~ x, then some random covariates, and labels for each point
# Generalize w.r.t n b/c we need more points later

genData = function(n){
    x = runif(n, 0, 33)
    y = (3*x +rnorm(n, sd=25))
    z = as.factor(sample(c(1:5), size=n, replace=T)) # 5 random categories
    a = as.factor(sample(c(1:2), size=n, replace=T)) # 2 random categories
    label = sapply(1:n, function(x){paste0(sample(c('a','b','c','d','e','f','g',
        'h','i','j','k'), 5), collapse='')}) # 5 character string for every point
    # anonymous functions. Same as a list comprehension in Python, [fun(x) for x in collection]
    df = data.frame(x,y,z,a, label)
    return(df)
}

df = genData(100)
df[1:10,]

# Basic scatterplot with the built-in graphics library
options(repr.plot.width = 4, repr.plot.height = 4)
plot(y ~ x, df)

# ggplot equivalent of the above
ggplot(df) + geom_point(aes(x,y))

# Now we specify a different color for each group. Built-in graphics:
plot(y ~ x, df)
points(y ~ x, subset(df, a == 1), col='brown2', pch=20)
points(y ~ x, subset(df, a == 2), col='cyan4', pch=20)

# Equivalent plot in ggplot:
ggplot(df) + geom_point(aes(x,y,color=a))

sf = data.frame(country=c('Nigeria','UK'), y2001=c(1,2), y2002=c(2,3), y2003=c(3,1))
sf

library('reshape')
lf = melt(sf, id = c("country"))
lf

sf2 = data.frame(country=c('Nigeria','UK'), x2001=c(8,9), x2002=c(7,11), x2003=c(6,10), y2001=c(1,2), y2002=c(2,3), y2003=c(3,1))
sf2

# A "molten" dataset: variable identity is treated as a variable.
# Every row is an observation of a single variable
m2 = melt(sf2, id = c("country"))
m2$year = as.numeric(gsub('[[:alpha:]]','',m2$variable))
m2$var = as.factor(sapply(m2$variable, function(x){substr(x,1,1)}))
m2$variable = NULL
m2

aggregate(value ~ var + country, m2, mean)

# From the long-form we can cast any short form we want
lf2 = cast(m2, country+year~var)
lf2

ggplot(lf2) + geom_point(aes(x,y, colour=country, shape=as.factor(year)))

ggplot(df) #blank: collection of layers; in this case empty!

ggplot(df) + geom_point(aes(x,y,color=a)) # aesthetic specified in the geom layer

ggplot(df, aes(x,y,color=a)) + geom_point() # equivalent!

ggplot(df) + geom_point(aes(x,y)) #x and y are obligatory to define the position of the geom

ggplot(df) + geom_point(aes(x,y), shape=4) # shape is arbitrary; does not depend on data

ggplot(df) + geom_point(aes(x,y,colour=z), shape=4) # colour is linked to the data

ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) #colour and 

# locally weighted-regression (LOESS smoothing)
ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + geom_smooth(aes(x,y))

# Linear model
ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + geom_smooth(aes(x,y), method='glm')

# specifying the color of the linear regression: stats also have aesthetic mappings
ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + geom_smooth(aes(x,y), 
method='glm', colour='red', se=F)

ggplot(subset(df, x > 0 & y > 0) ) + geom_point(aes(x,y)) + coord_trans(x = "log2", y = "log2")

ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)
) + coord_cartesian(ylim=c(-40,140), xlim=c(-10,40))

#facet_wrap: consecutive values of a variable
ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + facet_wrap(~z)

#facet_grid: combinations of two or more variables
ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + facet_grid(a~z)

ggplot(df) + geom_point(aes(x,y,colour=z,shape=z)) + theme_bw()

# histogram of x values, grouped by a
ggplot(df) + geom_histogram(aes(x, fill=a), alpha=.5, bins=6) + facet_grid(a ~ .) + theme_bw()

# density plot (smoothed histogram) of x values, grouped by a
ggplot(df) + geom_density(aes(x, fill=a), alpha=.5) + theme_bw()

# dotplot of x values (like this histogram)
ggplot(df) + geom_dotplot(aes(x, fill=a), alpha=.5) + facet_grid(a ~ .) + theme_bw()

# boxplot of y values, grouped by a
ggplot(df) + geom_boxplot(aes(y=y,x=a, fill=a), ) + theme_bw()

# violin plot (boxplot with relative density) of y values, grouped by a
ggplot(df) + geom_violin(aes(y=y,x=a, fill=a), alpha=.5) + theme_bw()

# violin plot of y values, grouped by a, faceted by z
ggplot(df) + geom_violin(aes(y=y,x=a, fill=a), alpha=.5) + facet_wrap(~z) + theme_bw()

# labeled location of x,y values, colors from z
ggplot(df) + geom_text(aes(x,y,colour=z,label=label))

# barplots are somewhat complicated. Need to compute stats first 
df_means = aggregate(y ~ z + a, df, mean)
df_means

# barplot of the means
ggplot(df_means) + geom_bar(aes(x = z , y=y, fill=a),position='dodge',stat = "identity") + theme_bw()

# compute standard error of the mean (and other statistics)
sem = function(x) sd(x)/sqrt(length(x))
df_agg <- do.call(data.frame, aggregate(. ~ z + a, df, function(x) c(mean = mean(x), sd = sd(x), sem = sem(x))))
df_agg$y_high = df_agg$y.mean + df_agg$y.sem
df_agg$y_low = df_agg$y.mean - df_agg$y.sem
df_agg

# barplot with error bars
ggplot(df_agg) + geom_bar(aes(x = z , y=y.mean, fill=a),position='dodge',stat = "identity"
) + geom_errorbar(aes(x=z, ymax=y_high, ymin=y_low, fill=a), width=.2, position=position_dodge(.9)) + theme_bw()

# changing the color of the barplot
ggplot(df_agg) + geom_bar(aes(x = z , y=y.mean, fill=a),position='dodge',stat = "identity"
) + scale_fill_manual(values=c("#E69F00", "#56B4E9")) + geom_errorbar(aes(x=z, ymax=y_high, 
ymin=y_low, fill=a), width=.2, position=position_dodge(.9)) + theme_bw()

# generate more data: we want to use some methods for visualizing large amounts of data
df2 = genData(10000)

#2d bins: how many points do we have in every bin?
ggplot(df2) + geom_bin2d(aes(y=y,x=x), alpha=.5) + theme_bw()

# add a reference line for 0
ggplot(df2) + geom_bin2d(aes(y=y,x=x), alpha=.5) + theme_bw() + geom_hline(yintercept=0)

# isomorph plot
ggplot(df2) + geom_density2d(aes(y=y,x=x), alpha=.5) + theme_bw()

# isomorph plot for two groups
ggplot(df2) + geom_density2d(aes(y=y,x=x, colour=a), alpha=.5) + theme_bw()

# tiled density plot
ggplot(df2) + stat_density2d(geom="tile", aes(x=x,y=y,fill = ..density..), contour = FALSE) + theme_bw()

# change the color ramp for the density plot to something a little more exciting
library('colorRamps') # color is its own subject
ggplot(df2) + stat_density2d(geom="tile", aes(x=x,y=y,fill = ..density..), contour = FALSE)  + scale_fill_gradientn(colours=matlab.like(200)) + theme_bw()

# saving a pdf
p1 = ggplot(df2) + stat_density2d(geom="tile", aes(x=x,y=y,fill = ..density..), contour = FALSE)  + scale_fill_gradientn(colours=matlab.like(200)) + theme_bw()
p2 = ggplot(df_agg) + geom_bar(aes(x = z , y=y.mean, fill=a),position='dodge',stat = "identity"
) + scale_fill_manual(values=c("#E69F00", "#56B4E9")) + geom_errorbar(aes(x=z, ymax=y_high, 
ymin=y_low, fill=a), width=.2, position=position_dodge(.9)) + theme_bw()

pdf('figures/heatmap.pdf', width=8, height=8)
print(p1)
dev.off()

#saving a png. Note geom='raster' rather than tile for writing to a file
png('figures/heatmap.png', width=1000, height=1000)
print(p2)
dev.off()

#multiplotting
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

quartz(width=10, height=5)
multiplot(p1, p2, cols=2)

#time versioning plots
outputDir = paste0('results/', gsub('[ :]','\\_',Sys.time()), '/')
dir.create(outputDir, showWarnings=F)
print(paste('Created new outputDir:', outputDir))

pdf(paste0(outputDir, 'heatmap.pdf'), width=8, height=8)
print(p1)
dev.off()

png(paste0(outputDir, 'barplot.png'), width=1000, height=1000)
print(p2)
dev.off()

#getting a range from a plot to use in another plot
ggplot_build(p1)$panel$ranges[[1]]$y.range

#source('testPlot1.R') #Not included in the repositoy
#source('testPlot2.R') #Not included in the repositoy
